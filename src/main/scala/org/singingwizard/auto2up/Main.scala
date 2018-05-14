package org.singingwizard.auto2up

import org.rogach.scallop._
import org.apache.pdfbox.pdmodel._
import java.io.File
import resource._
import org.apache.pdfbox.pdmodel.common.PDRectangle
import org.apache.pdfbox.multipdf.LayerUtility
import org.apache.pdfbox.util.Matrix
import org.apache.pdfbox.pdmodel.graphics.image.JPEGFactory
import org.apache.pdfbox.pdmodel.graphics.image.LosslessFactory
import org.apache.pdfbox.rendering.PDFRenderer
import org.apache.pdfbox.rendering.ImageType
import scala.collection.JavaConversions._
import javax.imageio.ImageWriter
import javax.imageio.ImageIO
import org.rogach.scallop.exceptions.ScallopException
import java.io.IOException
import java.util.zip.GZIPInputStream
import java.io.FileInputStream
import java.util.zip.ZipException
import java.io.PipedOutputStream
import java.io.PipedInputStream

class Conf(args: Array[String]) extends ScallopConf(args) {
  version("auto2up 0.1")
  banner("""Usage: auto2up ... input
           |auto2up 2-ups PDFs in a slightly smart way.
           |The input can be PS or Gzip'd PS if GhostScript's ps2pdf is available in the PATH.
           |Options:
           |""".stripMargin)
  footer("\n72 points is 1 inch. 1/8 inch is 9 points.\n")
  mainOptions = Seq(outputFilename)
  val inputFilename = trailArg[String]("input", descr = "Input filename (really required, BUG)", required = false)
  val outputFilename = opt[String]("output", descr = "Output filename",
    default = inputFilename.get.map(outputFilenameForFilename))

  val drawBox = opt[Boolean]("draw-box", 'b', descr = "Draw boxes around placed pages", default = Some(false))

  val inkThreshold = opt[Float]("ink-threshold", 't',
    descr = "The gray level to consider 'ink' on the page. (1.0 is white, 0.0 is full black)",
    default = Some(0.95f))
  val dpi = opt[Float]("dpi", 'd',
    descr = "Rendering resolution used for analysis in DPI.",
    default = Some(60f))
  val inkMargin = opt[Float]("ink-margin", 'n',
    descr = "Margin to leave around ink when cropping in points.",
    default = Some(3f))

  val shortEdgeMargin = opt[Float]("short-edge-margin", 's',
    descr = "Margin on short paper edge in points.",
    default = Some(9f))
  val longEdgeMargin = opt[Float]("long-edge-margin", 'l',
    descr = "Margin on long paper edge in points.",
    default = Some(9f * 4))
  val interMargin = opt[Float]("inter-margin", 'i',
    descr = "Gap between pages placed on one sheet in points.",
    default = Some(9f / 2))

  val recenter = toggle("recenter",
    descrYes = "Recenter all pages by cropping each page based on it's own ink box.",
    descrNo = "Crop all pages in the exact same way based on the combined ink box.",
    default = Some(false))

  val verbose = tally("verbose", descr = "Increase verbosity")
  val quiet = tally("quiet", descr = "Decrease verbosity")

  def verbosity = verbose() - quiet()

  val help = opt[Boolean]("help", descr = "Show this help", default = Some(false))
  verify()

  /** Remove the extension from a filename and replace it with "-2up.pdf".
    */
  def outputFilenameForFilename(fn: String): String = {
    val nameStart = fn.lastIndexOf(File.separator) max 0
    def negNone(i: Int): Option[Int] = if (i < 0) None else Some(i)
    val extStart = negNone(fn.lastIndexOf(".pdf")).orElse(negNone(fn.lastIndexOf(".ps"))).orElse(negNone(fn.lastIndexOf(".")))
    val sansExt = extStart.map(fn.substring(0, _)).getOrElse(fn)
    sansExt + "-2up.pdf"
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    try {
      val conf = new Conf(args)
      if (!conf.help() && conf.inputFilename.isDefined) {
        main(conf)
      } else {
        conf.printHelp()
      }
    } catch {
      case ScallopException(msg) ⇒
        println(msg)
    }
  }
  def main(conf: Conf): Unit = {
    implicit val _conf = conf

    // Extract margin config for later use
    val shortEdgeMargin = conf.shortEdgeMargin()
    val longEdgeMargin = conf.longEdgeMargin()
    val interMargin = conf.interMargin()

    // The output page size
    val pageSize = new PDRectangle(PDRectangle.LETTER.getHeight, PDRectangle.LETTER.getWidth)

    // The amount of the page we will actually use.
    val usedSize = new PDRectangle(pageSize.getWidth - shortEdgeMargin * 2 - interMargin, pageSize.getHeight - longEdgeMargin * 2)

    // Load/open PDFs
    val inputFile = new File(conf.inputFilename())
    val outputFile = new File(conf.outputFilename())

    if (!(inputFile.isFile() && inputFile.canRead()))
      throw new IOException(s"Input file not readable: $inputFile")
    if (outputFile.exists() && (!outputFile.canWrite() || !outputFile.isFile()))
      throw new IOException(s"Output file not writable (check permissions and what is there now): $outputFile")
    if (outputFile.exists())
      trace(-1, s"$outputFile already exists. Overwriting.")

    trace(0, s"Input file: $inputFile")
    for {
      in ← managed(loadInput(inputFile))
      out ← managed(new PDDocument())
    } {
      // Setup helper classes
      val layerUtility = new LayerUtility(out)
      implicit val renderer = new PDFRenderer(in)

      // Get the ink boxes of each page
      val inkBoxes = for (pageNo ← 0 until in.getNumberOfPages) yield findInkBox(pageNo)
      // Compute the largest SIZE the can contain any of the boxes.
      // If we are not recentering compute the union of all the boxes instead.
      val inkBoxSize = inkBoxes.map(r ⇒ if (conf.recenter()) r.createRetranslatedRectangle() else r).reduce(unionRect)

      // Compute the scale needed to fit the input pages perfectly in the width and height
      val scaleForWidth = usedSize.getWidth / (inkBoxSize.getWidth * 2)
      val scaleForHeight = usedSize.getHeight / inkBoxSize.getHeight
      // Set scale to the minimum of the two so everything will fit
      val scale = scaleForHeight min scaleForWidth

      trace(0, s"Scale: ${(scale * 100).formatted("%.1f")}")

      // Compute the margins needed to horizontally center everything properly for pages of size inkBoxSize
      val shortEdgeCenteringMargin = (pageSize.getWidth - inkBoxSize.getWidth * 2 * scale) / 3
      val interCenteringMargin = (pageSize.getWidth - inkBoxSize.getWidth * 2 * scale) / 3

      // Iterate over pairs of pages
      for (pageNo ← 0 until in.getNumberOfPages by 2) {
        // Create and setup the output page
        val page = new PDPage()
        page.setMediaBox(pageSize)
        out.addPage(page)

        // Compute the actual ink box for this pair of pages based on the pair itself
        val pairInkBox = if (pageNo + 1 < in.getNumberOfPages)
          unionRect(inkBoxes(pageNo), inkBoxes(pageNo + 1))
        else
          inkBoxes(pageNo)

        val inkBox = if (conf.recenter()) pairInkBox else inkBoxSize

        // Vertically centering margin
        val longEdgeCenteringMargin = (pageSize.getHeight - inkBox.getHeight * scale) / 2

        // Setup the drawing context for this page
        for (contentStream ← managed(new PDPageContentStream(out, page, PDPageContentStream.AppendMode.APPEND, false))) {
          // Iterate over the two input pages
          for (subPage ← 0 to 1 if pageNo + subPage < in.getNumberOfPages) {
            val n = pageNo + subPage

            // Import the page as a Form XObject
            val embPage = layerUtility.importPageAsForm(in, n)

            contentStream.saveGraphicsState()

            // Compute the transform for this page
            val pageTransform = new Matrix()
            // Translate to the corner of the page space
            pageTransform.translate(shortEdgeCenteringMargin, longEdgeCenteringMargin)
            // Translate over by the inter-margin and the scaled page width based on which sub-page we are rendering.
            pageTransform.translate(subPage * (interCenteringMargin + inkBoxSize.getWidth * scale), 0)
            // Scale it
            pageTransform.scale(scale, scale)
            // Translate so the corner of the ink on the page will be at 0,0
            pageTransform.translate(-inkBox.getLowerLeftX, -inkBox.getLowerLeftY)

            contentStream.transform(pageTransform)

            // Draw clipped page
            contentStream.saveGraphicsState()
            markRectangle(contentStream, inkBox)
            contentStream.clip()
            // Draw the input page to the output
            contentStream.drawForm(embPage)
            contentStream.restoreGraphicsState()

            // Draw the box if requested. On top of page without clipping.
            if (conf.drawBox()) {
              markRectangle(contentStream, inkBox)
              contentStream.closeAndStroke()
            }

            contentStream.restoreGraphicsState()
          }
        }
      }

      trace(0, s"Output file: $outputFile")
      out.save(outputFile)
    }
  }

  val degrees: Float = (Math.PI / 180).toFloat

  /** Compute the union of two axis-aligned rectangle as a new AA rectangle.
    */
  def unionRect(a: PDRectangle, b: PDRectangle): PDRectangle = {
    val r = new PDRectangle

    r.setLowerLeftX(a.getLowerLeftX min b.getLowerLeftX)
    r.setLowerLeftY(a.getLowerLeftY min b.getLowerLeftY)
    r.setUpperRightX(a.getUpperRightX max b.getUpperRightX)
    r.setUpperRightY(a.getUpperRightY max b.getUpperRightY)

    r
  }

  /** Open inputFile as a PDF converting from PS if needed.
    */
  def loadInput(inputFile: File) = {
    try {
      PDDocument.load(inputFile)
    } catch {
      case _: IOException ⇒ {
        // Assume the PDF parse failed
        val psStream = try {
          new GZIPInputStream(new FileInputStream(inputFile))
        } catch {
          case _: ZipException ⇒ {
            new FileInputStream(inputFile)
          }
        }
        import scala.sys.process._
        val pipeOutput = new PipedOutputStream()
        val pipeInput = new PipedInputStream(pipeOutput)
        val proc = "ps2pdf - -" #< psStream #> pipeOutput run ()
        PDDocument.load(pipeInput)
      }
    }
  }

  /** Compute the AABB of the non-white areas on the page.
    */
  def findInkBox(pageNo: Int)(implicit conf: Conf, renderer: PDFRenderer) = {
    val dpi = conf.dpi()
    val marginOfError = (dpi / 72 * conf.inkMargin()) max 1
    val threshold = 255 * conf.inkThreshold()

    val img = renderer.renderImageWithDPI(pageNo, dpi, ImageType.GRAY)
    //ImageIO.write(img, "PNG", new File("tmp.png"))
    //val pdimg = LosslessFactory.createFromImage(out, img)
    val rast = img.getData()
    val pixelBuf = new Array[Int](rast.getNumBands)

    var maxX = Float.MinValue
    var maxY = Float.MinValue
    var minX = Float.MaxValue
    var minY = Float.MaxValue

    for {
      x ← 0 until rast.getWidth
      y ← 0 until rast.getHeight
      v = rast.getPixel(x, y, pixelBuf)(0)
      if v < threshold
    } {
      //println(x,y, v)
      maxX = maxX max x
      maxY = maxY max y
      minX = minX min x
      minY = minY min y
    }

    maxX = (maxX + marginOfError) min rast.getWidth
    maxY = (maxY + marginOfError) min rast.getHeight
    minX = (minX - marginOfError) max 0
    minY = (minY - marginOfError) max 0

    val scale = 72 / dpi
    val r = new PDRectangle
    // Y is flipped between raster and PDF
    r.setLowerLeftX(minX * scale)
    r.setLowerLeftY((rast.getHeight - maxY) * scale)
    r.setUpperRightX(maxX * scale)
    r.setUpperRightY((rast.getHeight - minY) * scale)
    r
  }

  /** Outline a rectangle to the content stream using moveTo/lineTo.
    */
  def markRectangle(contentStream: PDPageContentStream, box: PDRectangle) = {
    contentStream.moveTo(box.getLowerLeftX, box.getLowerLeftY)
    contentStream.lineTo(box.getLowerLeftX, box.getUpperRightY)
    contentStream.lineTo(box.getUpperRightX, box.getUpperRightY)
    contentStream.lineTo(box.getUpperRightX, box.getLowerLeftY)
    contentStream.lineTo(box.getLowerLeftX, box.getLowerLeftY)
  }

  def trace(level: Int, msg: ⇒ String)(implicit conf: Conf) {
    if (level <= conf.verbosity)
      println(msg)
  }
}
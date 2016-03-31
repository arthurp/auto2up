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
    default = inputFilename.get.map(i => i.stripSuffix(".pdf") + "-2up.pdf"))

  val drawBox = opt[Boolean]("draw-box", 'b', descr = "Draw boxes around placed pages", default = Some(false))

  val inkThreshold = opt[Float]("ink-threshold", 't',
    descr = "The gray level to consider 'ink' on the page. (1.0 is white, 0.0 is full black)",
    default = Some(0.95f))
  val dpi = opt[Float]("dpi", 'd',
    descr = "Rendering resolution used for analysis in DPI.",
    default = Some(100f))
  val inkMargin = opt[Float]("ink-margin", 'n',
    descr = "Margin to leave around ink when cropping in points.",
    default = Some(2f))

  val shortEdgeMargin = opt[Float]("short-edge-margin", 's',
    descr = "Margin on short paper edge in points.",
    default = Some(9f))
  val longEdgeMargin = opt[Float]("long-edge-margin", 'l',
    descr = "Margin on long paper edge in points.",
    default = Some(9f * 4))
  val interMargin = opt[Float]("inter-margin", 'i',
    descr = "Gap between pages placed on one sheet in points.",
    default = Some(9f / 2))

  val help = opt[Boolean]("help", descr = "Show this help", default = Some(false))
  verify()
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
      case ScallopException(msg) =>
        println(msg)
    }
  }
  def main(conf: Conf): Unit = {
    val inputFile = new File(conf.inputFilename())
    val outputFile = new File(conf.outputFilename())
    for {
      in <- managed(loadInput(inputFile))
      out <- managed(new PDDocument())
    } {
      val layerUtility = new LayerUtility(out)

      val renderer = new PDFRenderer(in)

      def findInkBox(pageNo: Int) = {
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
          x <- 0 until rast.getWidth
          y <- 0 until rast.getHeight
          val v = rast.getPixel(x, y, pixelBuf)(0)
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

      val inkBoxes = for (pageNo <- 0 until in.getNumberOfPages) yield findInkBox(pageNo)
      val inkBoxSize = inkBoxes.map(_.createRetranslatedRectangle()).reduce(unionRect)

      val shortEdgeMargin = conf.shortEdgeMargin().toFloat // 72f / 8
      val longEdgeMargin = conf.longEdgeMargin().toFloat // 72f / 2
      val interMargin = conf.interMargin().toFloat // 72f / 16

      val pageSize = new PDRectangle(PDRectangle.LETTER.getHeight, PDRectangle.LETTER.getWidth)

      val usedWidth = pageSize.getWidth - shortEdgeMargin * 2 - interMargin
      val usedHeight = pageSize.getHeight - longEdgeMargin * 2

      val scaleForWidth = usedWidth / (inkBoxSize.getWidth * 2)
      val scaleForHeight = usedHeight / inkBoxSize.getHeight
      val scale = scaleForHeight min scaleForWidth

      val shortEdgeCenteringMargin = (pageSize.getWidth - inkBoxSize.getWidth * 2 * scale) / 3
      val interCenteringMargin = (pageSize.getWidth - inkBoxSize.getWidth * 2 * scale) / 3

      for (pageNo <- 0 until in.getNumberOfPages by 2) {
        val page = new PDPage()
        page.setMediaBox(pageSize)
        //page.setRotation(90)
        out.addPage(page)

        val inkBox = if (pageNo + 1 < in.getNumberOfPages)
          unionRect(inkBoxes(pageNo), inkBoxes(pageNo + 1))
        else
          inkBoxes(pageNo)

        val longEdgeCenteringMargin = (pageSize.getHeight - inkBox.getHeight * scale) / 2

        def markInkBox(contentStream: PDPageContentStream) = {
          contentStream.moveTo(inkBox.getLowerLeftX, inkBox.getLowerLeftY)
          contentStream.lineTo(inkBox.getLowerLeftX, inkBox.getUpperRightY)
          contentStream.lineTo(inkBox.getUpperRightX, inkBox.getUpperRightY)
          contentStream.lineTo(inkBox.getUpperRightX, inkBox.getLowerLeftY)
          contentStream.lineTo(inkBox.getLowerLeftX, inkBox.getLowerLeftY)
        }

        for (subPage <- 0 to 1 if pageNo + subPage < in.getNumberOfPages) {
          val n = pageNo + subPage
          val embPage = layerUtility.importPageAsForm(in, n)
          //val inkBox = inkBoxes(n)

          for (contentStream <- managed(new PDPageContentStream(out, page, PDPageContentStream.AppendMode.APPEND, false))) {
            contentStream.saveGraphicsState();

            val pageTransform = new Matrix()
            pageTransform.translate(shortEdgeCenteringMargin, longEdgeCenteringMargin)
            pageTransform.translate(subPage * interCenteringMargin, 0)
            pageTransform.scale(scale, scale)
            pageTransform.translate(-inkBox.getLowerLeftX, -inkBox.getLowerLeftY)
            pageTransform.translate(subPage * inkBoxSize.getWidth, 0)

            contentStream.transform(pageTransform)

            if (conf.drawBox()) {
              markInkBox(contentStream)
              contentStream.closeAndStroke()
            }

            markInkBox(contentStream)
            contentStream.clip()

            contentStream.drawForm(embPage)
            contentStream.restoreGraphicsState()
          }
        }
      }

      out.save(outputFile)
    }
  }

  val degrees: Float = (Math.PI / 180).toFloat

  def unionRect(a: PDRectangle, b: PDRectangle): PDRectangle = {
    val r = new PDRectangle

    r.setLowerLeftX(a.getLowerLeftX min b.getLowerLeftX)
    r.setLowerLeftY(a.getLowerLeftY min b.getLowerLeftY)
    r.setUpperRightX(a.getUpperRightX max b.getUpperRightX)
    r.setUpperRightY(a.getUpperRightY max b.getUpperRightY)

    r
  }

  def loadInput(inputFile: File) = {
    if (!(inputFile.isFile() && inputFile.canRead()))
      throw new IOException("Input file not readable")

    try {
      PDDocument.load(inputFile)
    } catch {
      case _: IOException => {
        // Assume the PDF parse failed
        val psStream = try {
          new GZIPInputStream(new FileInputStream(inputFile))
        } catch {
          case _: ZipException => {
            new FileInputStream(inputFile)
          }
        }
        import scala.sys.process._
        val pipeOutput = new PipedOutputStream()
        val pipeInput = new PipedInputStream(pipeOutput)
        val proc = "ps2pdf - -" #< psStream #> pipeOutput run()
        PDDocument.load(pipeInput)
      }
    }
  }
}
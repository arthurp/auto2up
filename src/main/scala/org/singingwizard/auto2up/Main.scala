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

object Main {
  def main(args: Array[String]): Unit = {
    object Conf extends ScallopConf(args) {
      val inputFilename = trailArg[String]("input", descr = "Input filename", required = true)
      val outputFilename = opt[String]("output", descr = "Output filename",
        default = inputFilename.get.map(i => i.stripSuffix(".pdf") + "-2up.pdf"))
      val drawBox = opt[Boolean]("drawBox", default = Some(false))
      verify()
    }
    val inputFile = new File(Conf.inputFilename())
    val outputFile = new File(Conf.outputFilename())
    for {
      in <- managed(PDDocument.load(inputFile))
      out <- managed(new PDDocument())
    } {
      val layerUtility = new LayerUtility(out)

      val renderer = new PDFRenderer(in)

      def findInkBox(pageNo: Int) = {
        val dpi = 30f
        val marginOfError = (dpi / 72 * 2) max 1
        val threshold = 255 - 50

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
      println(inkBoxSize)

      val shortEdgeMargin = 72f / 8
      val longEdgeMargin = 72f / 8
      val interMargin = 72f / 16

      val pageSize = new PDRectangle(PDRectangle.LETTER.getHeight, PDRectangle.LETTER.getWidth)
      println(pageSize)

      val usedWidth = pageSize.getWidth - shortEdgeMargin * 2 - interMargin
      val usedHeight = pageSize.getHeight - longEdgeMargin * 2
      println(usedWidth, usedHeight)

      val scaleForWidth = usedWidth / (inkBoxSize.getWidth * 2)
      val scaleForHeight = usedHeight / inkBoxSize.getHeight
      val scale = scaleForHeight min scaleForWidth
      println(scaleForWidth, scaleForHeight, scale)

      val shortEdgeCenteringMargin = (pageSize.getWidth - inkBoxSize.getWidth * 2 * scale) / 3
      val interCenteringMargin = (pageSize.getWidth - inkBoxSize.getWidth * 2 * scale) / 3

      for (pageNo <- 0 until in.getNumberOfPages by 2) {
        val page = new PDPage()
        page.setMediaBox(pageSize)
        //page.setRotation(90)
        out.addPage(page)

        val inkBoxPairSize = if (pageNo + 1 < in.getNumberOfPages)
          unionRect(inkBoxes(pageNo).createRetranslatedRectangle(), inkBoxes(pageNo + 1).createRetranslatedRectangle())
        else
          inkBoxes(pageNo).createRetranslatedRectangle()

        val longEdgeCenteringMargin = (pageSize.getHeight - inkBoxPairSize.getHeight * scale) / 2

        for (subPage <- 0 to 1 if pageNo + subPage < in.getNumberOfPages) {
          val n = pageNo + subPage
          val embPage = layerUtility.importPageAsForm(in, n)
          val inkBox = inkBoxes(n)
          embPage.setBBox(inkBox)

          for (contentStream <- managed(new PDPageContentStream(out, page, PDPageContentStream.AppendMode.APPEND, false))) {
            contentStream.saveGraphicsState();

            val pageTransform = new Matrix()
            //tr.rotate(90 * degrees)
            pageTransform.translate(shortEdgeCenteringMargin, longEdgeCenteringMargin)
            pageTransform.translate(subPage * interCenteringMargin, 0)
            pageTransform.scale(scale, scale)
            pageTransform.translate(-inkBox.getLowerLeftX, -inkBox.getLowerLeftY)
            pageTransform.translate(subPage * inkBoxSize.getWidth, 0)

            println(pageTransform)
            contentStream.transform(pageTransform)

            if (Conf.drawBox()) {
              contentStream.moveTo(inkBox.getLowerLeftX, inkBox.getLowerLeftY)
              contentStream.lineTo(inkBox.getLowerLeftX, inkBox.getUpperRightY)
              contentStream.lineTo(inkBox.getUpperRightX, inkBox.getUpperRightY)
              contentStream.lineTo(inkBox.getUpperRightX, inkBox.getLowerLeftY)
              contentStream.lineTo(inkBox.getLowerLeftX, inkBox.getLowerLeftY)
              contentStream.closeAndStroke()
            }
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
}
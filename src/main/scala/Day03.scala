import scala.io.Source

object Day03 extends App {

  val input = Source.fromFile("day03.input").mkString
  val lines = input.split("\n")

  val Extractor = """\s+(\d+)\s+(\d+)\s+(\d+)""".r

  val extractedLines = lines.map { line =>
    val Extractor(a, b, c) = line
    List(a.toInt, b.toInt, c.toInt)
  }

  def isValidTriangle(sides: List[Int]): Boolean = {
    sides.sorted.reverse match {
      case max :: rest => rest.sum > max
    }
  }
  val validTriangles = extractedLines.filter(isValidTriangle)

  println(s"Valid triangles: ${validTriangles.length}")

  val validMatrixTriangles = extractedLines.grouped(3).collect { case matrix =>
    val transposed = matrix.toList.transpose
    transposed.count(isValidTriangle)
  }

  println(s"Valid matrix triangles: ${validMatrixTriangles.sum}")
}

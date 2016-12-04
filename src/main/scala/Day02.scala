import Day02.Direction.{Down, Up}

import scala.io.Source

object Day02 extends App {

  val input = Source.fromFile("day02.input").mkString
  val lines = input.split("\n")

  sealed trait Direction
  object Direction {
    case object Left extends Direction
    case object Right extends Direction
    case object Up extends Direction
    case object Down extends Direction

    def apply(c: Char): Direction = c match {
      case 'L' => Left
      case 'R' => Right
      case 'U' => Up
      case 'D' => Down
    }
  }
  case class Coordinate(x: Int, y: Int)

  def coordinateToNumber(coordinate: Coordinate): Int = coordinate match {
    case Coordinate(-1, 1) => 1
    case Coordinate(0, 1) => 2
    case Coordinate(1, 1) => 3
    case Coordinate(-1, 0) => 4
    case Coordinate(0, 0) => 5
    case Coordinate(1, 0) => 6
    case Coordinate(-1, -1) => 7
    case Coordinate(0, -1) => 8
    case Coordinate(1, -1) => 9
  }

  def nextCoordinate(coordinate: Coordinate, direction: Direction): Coordinate = direction match {
    case Direction.Left => coordinate.copy(x = math.max(coordinate.x - 1, -1))
    case Direction.Right => coordinate.copy(x = math.min(coordinate.x + 1, 1))
    case Up => coordinate.copy(y = math.min(coordinate.y + 1, 1))
    case Down => coordinate.copy(y = math.max(coordinate.y - 1, -1))
  }


  val numbers = lines.foldLeft((Coordinate(0,0), Seq.empty[Int])) { case ((c, n), line) =>
    val directions = line.map(Direction(_))
    val nc = directions.foldLeft(c) { case (cc, d) => nextCoordinate(cc, d)}
    (nc, n :+ coordinateToNumber(nc))
  }

  println(s"Code: ${numbers._2.mkString}")


  def coordinateToNumberBigBad(coordinate: Coordinate): Char = coordinate match {
    case Coordinate(0, 2) => '1'
    case Coordinate(-1, 1) => '2'
    case Coordinate(0, 1) => '3'
    case Coordinate(1, 1) => '4'
    case Coordinate(-2, 0) => '5'
    case Coordinate(-1, 0) => '6'
    case Coordinate(0, 0) => '7'
    case Coordinate(1, 0) => '8'
    case Coordinate(2, 0) => '9'
    case Coordinate(-1, -1) => 'A'
    case Coordinate(0, -1) => 'B'
    case Coordinate(1, -1) => 'C'
    case Coordinate(0, -2) => 'D'
  }

  def nextCoordinateBigPad(coordinate: Coordinate, direction: Direction): Coordinate = {
    val newCoordinate = direction match {
      case Direction.Left => coordinate.copy(x = coordinate.x - 1)
      case Direction.Right => coordinate.copy(x = coordinate.x + 1)
      case Up => coordinate.copy(y = coordinate.y + 1)
      case Down => coordinate.copy(y = coordinate.y - 1)
    }
    if (math.abs(newCoordinate.x) + math.abs(newCoordinate.y) > 2) {
      coordinate
    } else {
      newCoordinate
    }
  }

  val numbersBigPad = lines.foldLeft((Coordinate(-2 ,0), Seq.empty[Char])) { case ((c, n), line) =>
    val directions = line.map(Direction(_))
    val nc = directions.foldLeft(c) { case (cc, d) => nextCoordinateBigPad(cc, d)}
    (nc, n :+ coordinateToNumberBigBad(nc))
  }

  println(s"Code Big Pad: ${numbersBigPad._2.mkString}")

}

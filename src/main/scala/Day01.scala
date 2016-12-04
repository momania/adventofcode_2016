import Day01.Facing.{East, North, South, West}

import scala.annotation.tailrec
import scala.io.Source

object Day01 extends App {

  sealed trait Facing {
    def turn(direction: Direction): Facing
  }

  object Facing {

    case object North extends Facing {
      override def turn(direction: Direction) = direction match {
        case Direction.Left => West
        case Direction.Right => East
      }
    }

    case object East extends Facing {
      override def turn(direction: Direction) = direction match {
        case Direction.Left => North
        case Direction.Right => South
      }
    }

    case object South extends Facing {
      override def turn(direction: Direction) = direction match {
        case Direction.Left => East
        case Direction.Right => West
      }
    }

    case object West extends Facing {
      override def turn(direction: Direction) = direction match {
        case Direction.Left => South
        case Direction.Right => North
      }
    }

  }

  sealed trait Direction

  object Direction {
    case object Left extends Direction
    case object Right extends Direction

    def apply(d: String): Direction = d match {
      case "L" => Left
      case "R" => Right
    }
  }

  case class Coordinate(x: Int, y: Int) {
    def blocksAway: Int = math.abs(x) + math.abs(y)
  }
  case class Position(coordinate: Coordinate, facing: Facing)
  case class Instruction(direction: Direction, steps: Int)

  val InstructionParser = """(.)(\d+)""".r

  def parseInstruction(i: String): Instruction = i match {
    case InstructionParser(d, s) => Instruction(Direction(d), s.toInt)
  }

  def movePosition(currentPosition: Position, instruction: Instruction): Position = {
    val facing = currentPosition.facing.turn(instruction.direction)
    val newCoordinate = walkCoordinates(currentPosition.coordinate, facing, instruction.steps)
    Position(newCoordinate, facing)
  }

  def walkCoordinates(current: Coordinate, facing: Facing, steps: Int): Coordinate = {
    facing match {
      case North => current.copy(y = current.y + steps)
      case East => current.copy(x = current.x + steps)
      case South => current.copy(y = current.y - steps)
      case West => current.copy(x = current.x - steps)
    }
  }

  val input = Source.fromFile("day01.input").mkString
  val instructions = input.split(",").map(_.trim).map(parseInstruction).toList

  val start = Position(Coordinate(0, 0), North)
  val end = instructions.foldLeft(start) { case (p, i) => movePosition(p, i) }
  println(s"End position: $end")
  println(s"Blocks away from start: ${end.coordinate.blocksAway}")

  @tailrec def findFirstDouble(instructions: List[Instruction], position: Position, coordinates: List[Coordinate]): Coordinate = {
    instructions match {
      case Nil => sys.error("wah?")
      case instruction :: restInstructions =>
        val facing = position.facing.turn(instruction.direction)

        @tailrec def walk(stepsLeft: Int, cc: Coordinate,  cs: List[Coordinate]): Either[List[Coordinate], Coordinate] = {
          val coordinate = walkCoordinates(cc, facing, 1)
          if (cs.contains(coordinate)) {
            Right(coordinate)
          } else {
            val ncs = coordinate :: cs
            if (stepsLeft > 1) {
              walk(stepsLeft - 1, coordinate, ncs)
            } else {
              Left(ncs)
            }
          }
        }
        walk(instruction.steps, position.coordinate, coordinates) match {
          case Right(c) => c
          case Left(cc) =>
            findFirstDouble(restInstructions, movePosition(position, instruction), cc)
        }

    }
  }

  val firstCross = findFirstDouble(instructions, start, Nil)
  println(s"First crossing point: $firstCross")
  println(s"Location: ${firstCross.blocksAway}")
}

import scala.io.Source

object Day04 extends App {

  val input = Source.fromFile("day04.input").mkString
  val lines = input.split("\n")

  val Extractor = """(\d+)\[(.*)\]""".r


  case class CharacterCount(character: Char, count: Int)
  case class RoomInput(id: Int, checksum: String, encryptedName: String)
  def calculateChecksum(input: String): String = {
    val characterCounts = input.groupBy(identity(_)).map{ case (c, s) => CharacterCount(c, s.length)}.toList
    val sorted = characterCounts.sortWith{ case (ccl, ccr) =>
      ccl.count > ccr.count || (ccl.count == ccr.count  && ccl.character < ccr.character)
    }
    sorted.take(5).map(_.character).mkString
  }

  val validRooms = lines.flatMap { line =>
    line.split("-").toList.reverse match {
      case Extractor(id, checksum) :: rest =>
        if (checksum == calculateChecksum(rest.flatten.mkString)) {
          Some(RoomInput(id.toInt, checksum, rest.reverse.mkString("-")))
        } else {
          None
        }
    }
  }

  println(s"Nr of valid rooms: ${validRooms.length}")
  println(s"Sum of ids: ${validRooms.map(_.id).sum}")

  // a == 97
  def decodeRoomName(name: String, id: Int): String = {
    name.collect {
      case '-' => ' '
      case char => ((((char + id) - 97) % 26) + 97).toChar
    }
  }

  val storageRoom = validRooms.find { r =>
    decodeRoomName(r.encryptedName, r.id) == "northpole object storage"
  }

  for (r <- storageRoom) {
    println(s"Storage room ID: ${r.id}")
  }

}

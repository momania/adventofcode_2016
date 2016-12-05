import java.security.MessageDigest

import scala.annotation.tailrec

object Day05 extends App {

  val input = "ojvtpuvg"

  val digest = MessageDigest.getInstance("MD5")

  def generateHash(input: String, index: Int): String = {
    digest.reset()
    digest.update((input + index).getBytes)
    digest.digest().map(0xFF & _).map { "%02x".format(_) }.foldLeft("") { _ + _ }
  }


  @tailrec def findSecretCharacters(toFind: Int, startIndex: Int, found: Seq[Char]): Seq[Char] = {
    println(s"Searching from index $startIndex")
    val range = startIndex to 999999999
    val nextIndex = range.find { index => generateHash(input, index).startsWith("00000") }
    nextIndex match {
      case Some(i) =>
        val foundSoFar = found :+ generateHash(input, i).drop(5).head
        if (foundSoFar.size >= toFind) {
          foundSoFar
        } else {
          findSecretCharacters(toFind, i + 1, foundSoFar)
        }
      case None => found
    }
  }

  @tailrec def findSecretCharactersWithIndex(toFind: Int, startIndex: Int, found: Map[Int, Char]): Map[Int, Char] = {
    println(s"Searching from index $startIndex")
    val range = startIndex to 999999999
    val nextIndex = range.find { index => generateHash(input, index).startsWith("00000") }
    nextIndex match {
      case Some(i) =>
        val drop = generateHash(input, i).drop(5)
        val idx = drop.head.asDigit
        if (idx < 8 && idx >= 0 && !found.contains(idx)) {
          val char = drop.drop(1).head
          val foundSoFar = found + (idx -> char)
          if (foundSoFar.size >= toFind) {
            foundSoFar
          } else {
            findSecretCharactersWithIndex(toFind, i + 1, foundSoFar)
          }
        } else {
          findSecretCharactersWithIndex(toFind, i + 1, found)
        }
      case None => found
    }
  }

  val passwordChars = findSecretCharacters(8, 0, Seq.empty)
  println(s"Password:: ${new String(passwordChars.toArray)}")

//  val indexedPasswordChars = findSecretCharactersWithIndex(8, 0, Map.empty)
//  println(s"Password:: ${new String(indexedPasswordChars.toSeq.sortBy(_._1).map(_._2).mkString)}")
}

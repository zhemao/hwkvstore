package kvstore

import Chisel._

object TestUtils {
  def messToWords(mess: String, wordbytes: Int, offset: Int = 0): Array[BigInt] = {
    val padding = "".padTo(offset, 0.toChar)
    val paddedMess = padding + mess
    val numWords = (paddedMess.length - 1) / wordbytes + 1
    val messWords = new Array[BigInt](numWords)
    var word = BigInt(0)

    for (i <- 0 until paddedMess.length / wordbytes) {
      word = BigInt(0)
      for (j <- 0 until wordbytes) {
        val byte = BigInt(paddedMess(i * wordbytes + j))
        word |= byte << (8 * j)
      }
      messWords(i) = word
    }

    if (paddedMess.length % wordbytes > 0) {
      word = BigInt(0)
      val lastChunk = (messWords.length - 1) * wordbytes
      for (j <- 0 until paddedMess.length % wordbytes) {
        val byte = BigInt(paddedMess(lastChunk + j))
        word |= byte << (8 * j)
      }
      messWords(messWords.length - 1) = word
    }

    messWords
  }
  def computeHash(table: Array[Int], key: String, bytes: Int): BigInt = {
    var result = BigInt(0)
    for (j <- 0 until bytes) {
      var h = table((key(0) + j) % 256)
      for (i <- 1 until key.length)
        h = table(h ^ key(i))
      result |= (h << (8 * j))
    }
    result
  }
}

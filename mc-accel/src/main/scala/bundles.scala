package McAccel

import Chisel._

class KeyInfo(val KeyLenSize: Int, val TagSize: Int) extends Bundle {
  val len = UInt(width = KeyLenSize)
  val tag = UInt(width = TagSize)

  override def clone =
    (new KeyInfo(KeyLenSize, TagSize)).asInstanceOf[this.type]
}

class HashInfo(val HashSize: Int, val KeyLenSize: Int, val TagSize: Int)
    extends Bundle {
  val hash1 = UInt(width = HashSize)
  val hash2 = UInt(width = HashSize)
  val len = UInt(width = KeyLenSize)
  val tag = UInt(width = TagSize)

  override def clone =
    (new HashInfo(HashSize, KeyLenSize, TagSize)).asInstanceOf[this.type]
}

class HashSelection(val HashSize: Int, val TagSize: Int) extends Bundle {
  val tag = UInt(width = TagSize)
  val hash = UInt(width = HashSize)
  val found = Bool()

  override def clone =
    (new HashSelection(HashSize, TagSize)).asInstanceOf[this.type]
}

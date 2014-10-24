package McAccel

import Chisel._

class MessageInfo(val LenSize: Int, val TagSize: Int,
    val dir: IODirection = OUTPUT) extends Bundle {
  val len = UInt(dir, LenSize)
  val tag = UInt(dir, TagSize)

  override def clone =
    (new MessageInfo(LenSize, TagSize, dir)).asInstanceOf[this.type]
}

class HashInfo(val HashSize: Int, val KeyLenSize: Int, val TagSize: Int,
    val dir: IODirection = OUTPUT) extends Bundle {
  val hash1 = UInt(width = HashSize)
  val hash2 = UInt(width = HashSize)
  val len = UInt(width = KeyLenSize)
  val tag = UInt(width = TagSize)

  override def clone =
    (new HashInfo(HashSize, KeyLenSize, TagSize, dir)).asInstanceOf[this.type]
}

class HashSelection(val HashSize: Int, val TagSize: Int,
    val dir: IODirection = OUTPUT) extends Bundle {
  val tag = UInt(dir, TagSize)
  val hash = UInt(dir, HashSize)
  val found = Bool()

  override def clone =
    (new HashSelection(HashSize, TagSize, dir)).asInstanceOf[this.type]
}

class AddrLenPair(val AddrSize: Int, val dir: IODirection = OUTPUT)
    extends Bundle {
  val addr = UInt(dir, AddrSize)
  val len  = UInt(dir, AddrSize)

  override def clone =
    (new AddrLenPair(AddrSize, dir)).asInstanceOf[this.type]
}

package kvstore

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

class AddrLenPair(val AddrSize: Int, val LenSize: Int,
    val dir: IODirection = OUTPUT) extends Bundle {
  val addr = UInt(dir, AddrSize)
  val len  = UInt(dir, LenSize)

  override def clone =
    (new AddrLenPair(AddrSize, LenSize, dir)).asInstanceOf[this.type]
}

class MemCommand(val ReadAddrSize: Int, val WriteAddrSize: Int,
    val ActionSize: Int, val dir: IODirection = OUTPUT) extends Bundle {
  val action = Bits(dir, ActionSize)
  val readstart = UInt(dir, ReadAddrSize)
  val writestart = UInt(dir, WriteAddrSize)
  val len = UInt(dir, ReadAddrSize)

  override def clone =
    (new MemCommand(ReadAddrSize, WriteAddrSize, ActionSize, dir))
      .asInstanceOf[this.type]
}

class CopyRequest(val HashSize: Int, val LenSize: Int,
    val dir: IODirection = OUTPUT) extends Bundle {
  val hash = UInt(dir, HashSize)
  val len = UInt(dir, LenSize)

  override def clone =
    (new CopyRequest(HashSize, LenSize, dir)).asInstanceOf[this.type]
}

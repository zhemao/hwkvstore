package McAccel

import Chisel._

class HashInfo(val HashSize: Int, val KeyLenSize: Int) extends Bundle {
  val hash1 = UInt(width = HashSize)
  val hash2 = UInt(width = HashSize)
  val len = UInt(width = KeyLenSize)

  override def clone =
    (new HashInfo(HashSize, KeyLenSize)).asInstanceOf[this.type]
}

class HashSelection(val HashSize: Int) extends Bundle {
  val hash = UInt(width = HashSize)
  val found = Bool()

  override def clone =
    (new HashSelection(HashSize)).asInstanceOf[this.type]
}

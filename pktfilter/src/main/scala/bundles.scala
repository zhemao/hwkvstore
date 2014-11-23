package pktfilter

import Chisel._

class StreamIO[+T <: Data](gen: T) extends Bundle {
  val ready = Bool(INPUT)
  val valid = Bool(OUTPUT)
  val data = gen.clone.asOutput
  val last  = Bool(OUTPUT)
  override def clone = new StreamIO(gen).asInstanceOf[this.type]
}

object Stream {
  def apply[T <: Data](gen: T): StreamIO[T] = new StreamIO(gen)
}

class RoutingInfo extends Bundle {
  val srcAddr = UInt(width = 32)
  val dstAddr = UInt(width = 32)
  val srcPort = UInt(width = 16)
  val dstPort = UInt(width = 16)
}

object RoutingInfo {
  def apply(srcAddr: UInt, srcPort: UInt,
      dstAddr: UInt, dstPort: UInt): RoutingInfo = {
    val info = new RoutingInfo
    info.srcAddr := srcAddr
    info.srcPort := srcPort
    info.dstAddr := dstAddr
    info.dstPort := dstPort
    info
  }
}

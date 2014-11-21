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

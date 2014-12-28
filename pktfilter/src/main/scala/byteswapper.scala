package pktfilter

import Chisel._

class ByteSwapper(BitWidth: Int) extends Module {
  val NBytes = BitWidth / 8
  val io = new Bundle {
    val wordin = Decoupled(UInt(width = BitWidth)).flip
    val wordout = Decoupled(UInt(width = BitWidth))
  }

  val flipped = Cat((0 until NBytes).map(
    i => io.wordin.bits((i + 1) * 8 - 1, i * 8)
  ))

  io.wordout.ready <> io.wordin.ready
  io.wordout.valid <> io.wordin.valid
  io.wordout.bits := flipped
}

object ByteSwapper {
  def apply(wordin: DecoupledIO[UInt], w: Int): DecoupledIO[UInt] = {
    val swapper = Module(new ByteSwapper(w))
    swapper.io.wordin <> wordin
    swapper.io.wordout
  }
}

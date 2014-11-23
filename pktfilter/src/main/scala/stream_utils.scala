package pktfilter

import Chisel._

class StreamSplit[+T <: Data](gen: T) extends Module {
  val io = new Bundle {
    val in = Stream(gen).flip
    val out_a = Stream(gen)
    val out_b = Stream(gen)
    val sel = Bool(INPUT)
  }

  io.out_a.data := io.in.data
  io.out_b.data := io.in.data
  io.out_a.valid := io.in.valid && !io.sel
  io.out_b.valid := io.in.valid && io.sel
  io.out_a.last := io.in.last && !io.sel
  io.out_b.last := io.in.last && io.sel
  io.in.ready := Mux(io.sel, io.out_b.ready, io.out_a.ready)
}

object StreamSplit {
  def apply[T <: Data](gen: T): StreamSplit[T] = {
    Module(new StreamSplit(gen))
  }
}

class StreamMux[+T <: Data](gen: T) extends Module {
  val io = new Bundle {
    val in_a = Stream(gen).flip
    val in_b = Stream(gen).flip
    val out = Stream(gen)
    val sel = Bool(INPUT)
  }

  io.out.data := Mux(io.sel, io.in_b.data, io.in_a.data)
  io.out.last := Mux(io.sel, io.in_b.last, io.in_a.last)
  io.out.valid := Mux(io.sel, io.in_b.valid, io.in_a.valid)
  io.in_a.ready := io.out.ready && !io.sel
  io.in_b.ready := io.out.ready && io.sel
}

object StreamMux {
  def apply[T <: Data](gen: T): StreamMux[T] = {
    Module(new StreamMux(gen))
  }
}

class StreamWriter[+T <: Data](gen: T, CountSize: Int) extends Module {
  val io = new Bundle {
    val stream = Stream(gen).flip
    val writeData = gen.clone.asOutput
    val writeEn = Bool(OUTPUT)

    val enable = Bool(INPUT)
    val ignore = Bool(INPUT)

    val count = UInt(OUTPUT, CountSize)
  }

  val count = Reg(init = UInt(0, CountSize))
  val writeData = Reg(gen)
  val writeEn = Reg(init = Bool(false))

  when (io.stream.valid && io.enable) {
    writeEn := !io.ignore
    writeData := io.stream.data
    when (io.stream.last) {
      count := UInt(0)
    } .otherwise {
      count := count + UInt(1)
    }
  } .otherwise {
    writeEn := Bool(false)
  }

  io.count := count
  io.stream.ready := io.enable
  io.writeData := writeData
  io.writeEn := writeEn
}

object StreamWriter {
  def apply[T <: Data](gen: T, CountSize: Int): StreamWriter[T] = {
    Module(new StreamWriter(gen, CountSize))
  }
}

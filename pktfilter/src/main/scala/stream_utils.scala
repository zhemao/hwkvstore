package pktfilter

import Chisel._

class StreamArbiter[T <: Data](gen: T, n: Int) extends Module {
  val io = new Bundle {
    val ins = Vec.fill(n) { Stream(gen) }.flip
    val out = Stream(gen)
  }

  val sel = Reg(init = UInt(0, log2Up(n)))
  val running = Reg(init = Bool(false))

  io.out.data := io.ins(sel).data
  io.out.last := io.ins(sel).last
  io.out.valid := io.ins(sel).valid && running

  for (i <- 0 until n)
    io.ins(i).ready := io.out.ready && sel === UInt(i) && running

  when (!running) {
    when (io.ins(sel).valid) {
      running := Bool(true)
    } .otherwise {
      sel := sel + UInt(1)
    }
  } .elsewhen (io.out.valid && io.out.last && io.out.ready) {
    running := Bool(false)
    sel := sel + UInt(1)
  }
}

object StreamArbiter {
  def apply[T <: Data](vec: Vec[StreamIO[T]]): StreamIO[T] = {
    val arbiter = Module(new StreamArbiter(vec(0).gen, vec.length))
    arbiter.io.ins <> vec
    arbiter.io.out
  }

  def apply[T <: Data](streams: Iterable[StreamIO[T]]): StreamIO[T] = {
    apply(Vec(streams))
  }

  def apply[T <: Data](streams: StreamIO[T]*): StreamIO[T] = {
    apply(streams)
  }
}

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

class StreamWriter[+T <: Data](gen: T, CountSize: Int) extends Module {
  val io = new Bundle {
    val stream = Stream(gen).flip
    val writeData = gen.clone.asOutput
    val writeEn = Bool(OUTPUT)

    val enable = Bool(INPUT)
    val count = UInt(OUTPUT, CountSize)
    val finished = Bool(OUTPUT)
  }

  val count = Reg(init = UInt(0, CountSize))
  val writeData = Reg(gen)
  val writeEn = Reg(init = Bool(false))
  val finished = Reg(init = Bool(true))

  when (io.stream.valid && io.enable) {
    writeEn := Bool(true)
    writeData := io.stream.data
    when (finished) {
      count := UInt(1)
      finished := Bool(false)
    } .otherwise {
      count := count + UInt(1)
      when (io.stream.last) {
        finished := Bool(true)
      }
    }
  } .otherwise {
    writeEn := Bool(false)
  }

  io.count := count
  io.stream.ready := io.enable
  io.writeData := writeData
  io.writeEn := writeEn
  io.finished := finished
}

object StreamWriter {
  def apply[T <: Data](gen: T, CountSize: Int): StreamWriter[T] = {
    Module(new StreamWriter(gen, CountSize))
  }
}

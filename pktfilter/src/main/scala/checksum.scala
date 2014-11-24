package pktfilter

import Chisel._

class ChecksumCompute(LenSize: Int) extends Module {
  val io = new Bundle {
    val data = Decoupled(UInt(width = 8)).flip
    val len = Decoupled(UInt(width = LenSize)).flip
    val result = Decoupled(UInt(width = 16))
  }

  val highbyte = Reg(UInt(width = 8))
  val cursum = Reg(UInt(width = 32))
  val length = Reg(UInt(width = LenSize))

  val (s_idle :: s_higher :: s_lower :: s_fold :: s_invert :: s_finish :: Nil) = Enum(Bits(), 6)
  val state = Reg(init = s_idle)

  io.data.ready := (state === s_higher || state === s_lower)
  io.len.ready := (state === s_idle)
  io.result.valid := (state === s_finish)
  io.result.bits := cursum(15, 0)

  switch (state) {
    is (s_idle) {
      when (io.len.valid) {
        length := io.len.bits
        state := s_higher
      }
    }
    is (s_higher) {
      when (io.data.valid) {
        highbyte := io.data.bits
        length := length - UInt(1)

        when (length === UInt(1)) {
          cursum := cursum + Cat(io.data.bits, UInt(0, 8))
          state := s_fold
        } .otherwise {
          state := s_lower
        }
      }
    }
    is (s_lower) {
      when (io.data.valid) {
        cursum := cursum + Cat(highbyte, io.data.bits)
        length := length - UInt(1)

        when (length === UInt(1)) {
          state := s_fold
        } .otherwise {
          state := s_higher
        }
      }
    }
    is (s_fold) {
      cursum := cursum(31, 16) + cursum(15, 0)
      state := s_invert
    }
    is (s_invert) {
      cursum := ~cursum
      state := s_finish
    }
    is (s_finish) {
      when (io.result.ready) {
        state := s_idle
      }
    }
  }
}

class ChecksumComputeTest(c: ChecksumCompute) extends Tester(c) {
  val words = Array.fill(50) { rnd.nextInt & 0xffff }
  var checksum = words.foldLeft(0) { _ + _ }

  checksum = (checksum >> 16) + (checksum & 0xffff)
  checksum = ~checksum & 0xffff

  expect(c.io.len.ready, 1)
  poke(c.io.len.bits, 2 * words.length)
  poke(c.io.len.valid, 1)
  step(1)
  poke(c.io.len.valid, 0)

  poke(c.io.data.valid, 1)

  for (w <- words) {
    expect(c.io.data.ready, 1)
    poke(c.io.data.bits, (w >> 8) & 0xff)
    step(1)
    poke(c.io.data.bits, w & 0xff)
    step(1)
  }
  poke(c.io.data.valid, 0)

  poke(c.io.result.ready, 1)
  step(2)
  expect(c.io.result.valid, 1)
  expect(c.io.result.bits, checksum)
  step(1)
  poke(c.io.result.ready, 0)
  expect(c.io.len.ready, 1)
}

object ChecksumComputeMain {
  def main(args: Array[String]) {
    chiselMain.run(args, () => new ChecksumCompute(8),
      (c: ChecksumCompute) => new ChecksumComputeTest(c))
  }
}

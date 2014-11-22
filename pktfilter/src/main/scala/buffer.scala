package pktfilter

import Chisel._

class PacketBuffer(BufferSize: Int) extends Module {
  val AddrSize = log2Up(BufferSize)
  val io = new Bundle {
    val readData = Stream(UInt(width = 8))
    val stream = Decoupled(UInt(width = AddrSize)).flip
    val skip = Decoupled(UInt(width = AddrSize)).flip

    val writeData = UInt(INPUT, 8)
    val writeEn   = Bool(INPUT)

    val empty = Bool(OUTPUT)
    val full = Bool(OUTPUT)
  }

  val mem = Mem(UInt(width = 8), BufferSize)
  val writeHead   = Reg(init = UInt(0, AddrSize))
  val readHead    = Reg(init = UInt(0, AddrSize))
  val readCount   = Reg(init = UInt(0, AddrSize))

  val (r_wait :: r_send :: r_fetch :: r_skip :: Nil) = Enum(Bits(), 4)
  val state = Reg(init = r_wait)

  val readValid = (readHead != writeHead)
  val readData = Reg(UInt(width = 8))
  val readLast = Reg(Bool())

  val skipValid = (writeHead - readHead) >= readCount

  switch (state) {
    is (r_wait) {
      when (io.skip.valid) {
        readCount := io.skip.bits
        state := r_skip
      } .elsewhen (io.stream.valid) {
        readCount := io.stream.bits
        state := r_fetch
      }
    }
    is (r_send) {
      when (io.readData.ready) {
        when (readCount === UInt(0)) {
          state := r_wait
        } .otherwise {
          readHead := readHead + UInt(1)
          state := r_fetch
        }
      }
    }
    is (r_fetch) {
      when (readValid) {
        readData := mem(readHead)
        readCount := readCount - UInt(1)
        readLast := readCount === UInt(1)
        state := r_send
      }
    }
    is (r_skip) {
      when (skipValid) {
        readHead := readHead + readCount
        state := r_wait
      }
    }
  }

  io.readData.valid := (state === r_send)
  io.readData.data := readData
  io.readData.last := readLast

  val ctrlReady = (state === r_wait)
  io.stream.ready := ctrlReady
  io.skip.ready   := ctrlReady

  val writeEn = Reg(next = io.writeEn)
  val writeData = Reg(next = io.writeData)

  when (writeEn) {
    mem(writeHead) := writeData
    writeHead := writeHead + UInt(1)
  }

  io.empty := (writeHead === readHead)
  io.full := (writeHead - UInt(1) === readHead)
}

class PacketBufferTest(c: PacketBuffer) extends Tester(c) {
  val packet = "askdfj;adfjsklnasndasfjadsfjkdsa"
  val midLen = 7

  poke(c.io.writeEn, 1)
  for (i <- 0 until midLen) {
    poke(c.io.writeData, packet(i))
    step(1)
  }
  poke(c.io.writeEn, 0)

  expect(c.io.stream.ready, 1)
  poke(c.io.stream.valid, 1)
  poke(c.io.stream.bits, packet.size)
  step(1)
  poke(c.io.stream.valid, 0)

  poke(c.io.readData.ready, 1)
  step(1)

  for (i <- 0 until midLen) {
    expect(c.io.readData.valid, 1)
    expect(c.io.readData.data, packet(i))
    expect(c.io.readData.last, 0)
    step(2)
  }

  poke(c.io.readData.ready, 0)

  expect(c.io.empty, 1)

  poke(c.io.writeEn, 1)
  for (i <- midLen until packet.size) {
    poke(c.io.writeData, packet(i))
    step(1)
  }
  poke(c.io.writeEn, 0)

  expect(c.io.empty, 0)
  poke(c.io.readData.ready, 1)

  for (i <- midLen until packet.size - 1) {
    expect(c.io.readData.valid, 1)
    expect(c.io.readData.data, packet(i))
    expect(c.io.readData.last, 0)
    step(2)
  }

  expect(c.io.readData.valid, 1)
  expect(c.io.readData.data, packet(packet.size - 1))
  expect(c.io.readData.last, 1)
  step(1)
  expect(c.io.skip.ready, 1)
}

object PacketBufferMain {
  def main(args: Array[String]) {
    chiselMain.run(args, () => new PacketBuffer(255),
      (c: PacketBuffer) => new PacketBufferTest(c))
  }
}

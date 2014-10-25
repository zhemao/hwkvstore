package McAccel

import Chisel._
import Chisel.AdvTester._
import McAccel.Constants._
import McAccel.TestUtils._

class MemoryHandler(WordSize: Int, ValAddrSize: Int, KeyAddrSize: Int)
      extends Module with CoreParameters with MemoryOpConstants {
  val io = new Bundle {
    val mem = new HellaCacheIO
    val keyData = Decoupled(UInt(width = 8))

    val cacheWriteAddr = UInt(OUTPUT, ValAddrSize)
    val cacheWriteData = UInt(OUTPUT, 8)
    val cacheWriteEn = Bool(OUTPUT)

    val allKeyAddr = UInt(OUTPUT, KeyAddrSize)
    val allKeyData = UInt(OUTPUT, WordSize)
    val allKeyWrite = Bool(OUTPUT)

    val cmd = Decoupled(new MemCommand(coreDataBits, ValAddrSize, 2)).flip
  }

  val BytesPerWord = coreDataBits / 8
  val ByteShift = log2Up(BytesPerWord)

  val (s_idle :: s_stream_key_wait :: s_stream_key_out ::
    s_req_data :: s_proc_resp :: s_finish :: Nil) = Enum(Bits(), 6)
  val state = Reg(init = s_idle)
  val ret_state = Reg(init = s_idle)

  val len = Reg(UInt(width = ValAddrSize))
  val bytesread = Reg(UInt(width = ValAddrSize))
  val readaddr = Reg(UInt(width = coreDataBits))
  val writeaddr = Reg(UInt(width = ValAddrSize))

  val keyDataValid = Reg(init = Bool(false))
  val keyDataBits = Reg(UInt(width = 8))

  val word = Reg(Bits(width = coreDataBits))

  io.mem.req.valid := (state === s_req_data)
  io.mem.req.bits.addr := readaddr
  io.mem.req.bits.kill := Bool(false)
  io.mem.req.bits.typ := MT_D
  io.mem.req.bits.cmd := M_XRD
  io.mem.req.bits.phys := Bool(true)

  io.keyData.valid := (state === s_stream_key_out)
  io.keyData.bits  := word(7, 0)

  io.cmd.ready := (state === s_idle)

  switch (state) {
    is (s_idle) {
      when (io.cmd.valid) {
        readaddr := io.cmd.bits.readstart
        writeaddr := io.cmd.bits.writestart
        len := io.cmd.bits.len
        bytesread := UInt(0)

        switch (io.cmd.bits.action) {
          is (StreamKeyAction) {
            state := s_stream_key_wait
          }
        }
      }
    }
    is (s_stream_key_wait) {
      when (io.keyData.ready) {
        state := s_req_data
        ret_state := s_stream_key_out
      }
    }
    is (s_stream_key_out) {
      bytesread := bytesread + UInt(1)
      word := Cat(UInt(0, 8), word(coreDataBits - 1, 8))
      when (bytesread === len - UInt(1)) {
        state := s_finish
      } .elsewhen (bytesread(ByteShift - 1, 0) === UInt(BytesPerWord - 1)) {
        ret_state := s_stream_key_out
        state := s_req_data
      }
    }
    is (s_req_data) {
      when (io.mem.req.ready) {
        readaddr := readaddr + UInt(BytesPerWord)
        state := s_proc_resp
      }
    }
    is (s_proc_resp) {
      when (io.mem.resp.valid) {
        word := io.mem.resp.bits.data
        state := ret_state
      }
    }
    is (s_finish) {
      state := s_idle
    }
  }
}

class MemoryHandlerTest(c: MemoryHandler) extends AdvTester(c) {
  val MemReq_OHandler  = new DecoupledSink(
    c.io.mem.req,
    (sckt: HellaCacheReq) => TestMemReq.extract(this, sckt))
  val MemResp_IHandler = new ValidSource(
    c.io.mem.resp,
    (sckt: HellaCacheResp, in: TestMemResp) => in.inject(this, sckt))

  val randomMemory = new RandomMemory(64, 64*64*2,
    MemReq_OHandler.outputs, MemResp_IHandler.inputs)

  val key = "asdkfjqwekjfasdkfj"
  val keyWords = messToWords(key, 8)

  randomMemory.store_data(0, keyWords)

  wire_poke(c.io.cmd.valid, 1)
  wire_poke(c.io.cmd.bits.action, 0)
  wire_poke(c.io.cmd.bits.readstart, 0)
  wire_poke(c.io.cmd.bits.len, key.length)
  step(1)
  wire_poke(c.io.cmd.valid, 0)
  wire_poke(c.io.keyData.ready, 1)

  var addr = 0

  until (peek(c.io.cmd.ready) == 1, 450) {
    if (peek(c.io.keyData.valid) == 1) {
      expect(c.io.keyData.bits, key(addr))
      addr += 1
    }
    randomMemory.process()
  }
}

object MemoryHandlerMain {
  def main(args: Array[String]) {
    chiselMainTest(args, () => Module(new MemoryHandler(64, 16, 5))) {
      c => new MemoryHandlerTest(c)
    }
  }
}

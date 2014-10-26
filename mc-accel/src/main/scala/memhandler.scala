package McAccel

import Chisel._
import Chisel.AdvTester._
import McAccel.Constants._
import McAccel.TestUtils._

class MemoryHandler(val WordSize: Int, ValAddrSize: Int, KeyAddrSize: Int)
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

    val cmd = Decoupled(
      new MemCommand(coreDataBits, ValAddrSize, ActionSize)).flip
  }

  val BytesPerInputWord = coreDataBits / 8
  val InputByteShift = log2Up(BytesPerInputWord)
  val BytesPerOutputWord = WordSize / 8
  val OutputByteShift = log2Up(BytesPerOutputWord)
  val OutputWordsPerInputWord = coreDataBits / WordSize

  val (s_idle :: s_stream_key_wait :: s_shift_bytes :: s_shift_words ::
    s_req_data :: s_proc_resp :: s_finish :: Nil) = Enum(Bits(), 7)
  val state = Reg(init = s_idle)
  val outputMask = Reg(Bits(width = 3))
  val shiftByWord = Reg(Bool())

  val len = Reg(UInt(width = ValAddrSize))
  val bytesread = Reg(UInt(width = ValAddrSize))
  val readaddr = Reg(UInt(width = coreDataBits))
  val writeaddr = Reg(UInt(width = ValAddrSize))

  val keyDataValid = Reg(init = Bool(false))
  val keyDataBits = Reg(UInt(width = 8))

  val word = Reg(Bits(width = coreDataBits))
  val byteOff = bytesread(InputByteShift - 1, 0)
  val wordsread = bytesread(ValAddrSize - 1, OutputByteShift)
  val wordlen = len(ValAddrSize - 1, OutputByteShift)

  io.mem.req.valid := (state === s_req_data)
  io.mem.req.bits.addr := readaddr
  io.mem.req.bits.kill := Bool(false)
  io.mem.req.bits.typ := MT_D
  io.mem.req.bits.cmd := M_XRD
  io.mem.req.bits.phys := Bool(true)

  val shifting = (state === s_shift_bytes) || (state === s_shift_words)

  io.keyData.valid := shifting && outputMask(0).toBool
  io.keyData.bits  := word(7, 0)

  io.allKeyAddr := writeaddr(KeyAddrSize + OutputByteShift - 1, OutputByteShift)
  io.allKeyData := word(WordSize - 1, 0)
  io.allKeyWrite := shifting && outputMask(1).toBool

  io.cacheWriteData := word(7, 0)
  io.cacheWriteAddr := writeaddr
  io.cacheWriteEn := shifting && outputMask(2).toBool

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
            shiftByWord := Bool(false)
            outputMask := Bits("b001")
          }
          is (CopyKeyAction) {
            state := s_req_data
            shiftByWord := Bool(true)
            outputMask := Bits("b010")
          }
          is (CopyValueAction) {
            state := s_req_data
            shiftByWord := Bool(false)
            outputMask := Bits("b100")
          }
        }
      }
    }
    is (s_stream_key_wait) {
      when (io.keyData.ready) {
        state := s_req_data
      }
    }
    is (s_shift_bytes) {
      bytesread := bytesread + UInt(1)
      writeaddr := writeaddr + UInt(1)
      word := Cat(UInt(0, 8), word(coreDataBits - 1, 8))
      when (bytesread === len - UInt(1)) {
        state := s_finish
      } .elsewhen (byteOff === UInt(BytesPerInputWord - 1)) {
        state := s_req_data
      }
    }
    is (s_shift_words) {
      bytesread := bytesread + UInt(BytesPerOutputWord)
      if (WordSize == coreDataBits) {
        when (wordsread === wordlen - UInt(1)) {
          state := s_finish
        } .otherwise {
          state := s_req_data
          writeaddr := writeaddr + UInt(BytesPerOutputWord)
        }
      } else {
        word := Cat(UInt(0, WordSize), word(coreDataBits - 1, WordSize))
        val wordOff = bytesread(InputByteShift - 1, OutputByteShift)
        when (wordsread === wordlen - UInt(1)) {
          state := s_finish
        } .elsewhen (wordOff === UInt(OutputWordsPerInputWord - 1)) {
          state := s_req_data
          writeaddr := writeaddr + UInt(BytesPerOutputWord)
        }
      }
    }
    is (s_req_data) {
      when (io.mem.req.ready) {
        readaddr := readaddr + UInt(BytesPerInputWord)
        state := s_proc_resp
      }
    }
    is (s_proc_resp) {
      when (io.mem.resp.valid) {
        word := io.mem.resp.bits.data
        when (shiftByWord) {
          state := s_shift_words
        } .otherwise {
          state := s_shift_bytes
        }
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

  val memory = new RandomMemory(64, 64*64*2,
    MemReq_OHandler.outputs, MemResp_IHandler.inputs)

  val key = "asdkfjqwekjfasdkfj"
  val keyInWords = messToWords(key, 8)
  val keyOutWords = messToWords(key, c.WordSize / 8)

  memory.store_data(0, keyInWords)

  wire_poke(c.io.cmd.valid, 1)
  wire_poke(c.io.cmd.bits.action, 0)
  wire_poke(c.io.cmd.bits.readstart, 0)
  wire_poke(c.io.cmd.bits.writestart, 0)
  wire_poke(c.io.cmd.bits.len, key.length)
  takestep()
  wire_poke(c.io.cmd.valid, 0)
  wire_poke(c.io.keyData.ready, 1)

  var addr = 0

  until (peek(c.io.cmd.ready) == 1, 450) {
    expect(c.io.cacheWriteEn, 0)
    expect(c.io.allKeyWrite, 0)
    if (peek(c.io.keyData.valid) == 1) {
      isTrace = true
      expect(c.io.keyData.bits, key(addr))
      addr += 1
      isTrace = false
    }
    memory.process()
  }

  wire_poke(c.io.keyData.ready, 0)

  wire_poke(c.io.cmd.valid, 1)
  wire_poke(c.io.cmd.bits.action, 1)
  takestep()
  wire_poke(c.io.cmd.valid, 0)

  memory.store_data(0, keyInWords)

  until (peek(c.io.cmd.ready) == 1, 450) {
    expect(c.io.keyData.valid, 0)
    expect(c.io.cacheWriteEn, 0)
    if (peek(c.io.allKeyWrite) == 1) {
      isTrace = true
      addr = peek(c.io.allKeyAddr).toInt
      expect(c.io.allKeyData, keyOutWords(addr))
      isTrace = false
    }
    memory.process()
  }

  wire_poke(c.io.cmd.valid, 1)
  wire_poke(c.io.cmd.bits.action, 2)
  takestep()
  wire_poke(c.io.cmd.valid, 0)

  until (peek(c.io.cmd.ready) == 1, 450) {
    expect(c.io.keyData.valid, 0)
    expect(c.io.allKeyWrite, 0)
    if (peek(c.io.cacheWriteEn) == 1) {
      isTrace = true
      addr = peek(c.io.cacheWriteAddr).toInt
      expect(c.io.cacheWriteData, key(addr))
      isTrace = false
    }
    memory.process()
  }
}

object MemoryHandlerMain {
  def main(args: Array[String]) {
    chiselMainTest(args, () => Module(new MemoryHandler(64, 16, 5))) {
      c => new MemoryHandlerTest(c)
    }
  }
}

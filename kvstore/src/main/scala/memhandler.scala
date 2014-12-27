package kvstore

import Chisel._
import Chisel.AdvTester._
import kvstore.Constants._
import kvstore.TestUtils._

class MemoryHandler(ValWordSize: Int, ValLenSize: Int, KeyAddrSize: Int)
      extends Module with CoreParameters with MemoryOpConstants {
  val ValWordBytes = ValWordSize / 8
  val ValWordShift = log2Up(ValWordSize) - 3
  val ValAddrSize = ValLenSize - ValWordShift
  val io = new Bundle {
    val mem = new HellaCacheIO
    val keyData = Decoupled(UInt(width = 8))

    val cacheWriteAddr = UInt(OUTPUT, ValAddrSize)
    val cacheWriteData = UInt(OUTPUT, ValWordSize)
    val cacheWriteEn = Bool(OUTPUT)

    val cmd = Decoupled(
      new MemCommand(coreDataBits, ValAddrSize, ActionSize)).flip
  }

  val BytesPerInputWord = coreDataBits / 8
  val InputByteShift = log2Up(BytesPerInputWord)

  val (s_idle :: s_stream_key_wait :: s_shift_bytes :: s_shift_words ::
    s_req_data :: s_proc_resp :: s_finish :: s_init_shift ::
    Nil) = Enum(Bits(), 8)
  val state = Reg(init = s_idle)
  val outputMask = Reg(Bits(width = 2))

  val len = Reg(UInt(width = ValLenSize))
  val bytesread = Reg(UInt(width = ValLenSize))
  val readaddr = Reg(UInt(width = coreDataBits))
  val writeaddr = Reg(UInt(width = ValAddrSize))
  val initshift = Reg(UInt(width = InputByteShift + 3))
  val initByteOff = initshift(InputByteShift + 2, 3)

  val word = Reg(Bits(width = coreDataBits))
  val savedByte = Reg(Bits(width = 8))
  val byteOff = Reg(UInt(width = InputByteShift))
  val brlower = bytesread(InputByteShift - 1, 0)
  val splitWord = (byteOff === UInt(0) && brlower === UInt(1))

  io.mem.req.valid := (state === s_req_data)
  io.mem.req.bits.addr := readaddr
  io.mem.req.bits.kill := Bool(false)
  io.mem.req.bits.typ := MT_D
  io.mem.req.bits.cmd := M_XRD
  io.mem.req.bits.phys := Bool(params[Boolean]("physaddr"))

  val shifting = (state === s_shift_bytes || state === s_shift_words)

  io.keyData.valid := shifting && outputMask(0).toBool
  io.keyData.bits  := word(7, 0)

  val byword = Reg(Bool())

  if (ValWordSize == 8)
    io.cacheWriteData := word(7, 0)
  else
    io.cacheWriteData := Mux(splitWord, Cat(word(7, 0), savedByte), word(15, 0))
  io.cacheWriteAddr := writeaddr
  io.cacheWriteEn := shifting && outputMask(1).toBool

  io.cmd.ready := (state === s_idle)

  switch (state) {
    is (s_idle) {
      when (io.cmd.valid) {
        readaddr := Cat(
          io.cmd.bits.readstart(coreDataBits - 1, InputByteShift),
          UInt(0, InputByteShift))
        initshift := Cat(
          io.cmd.bits.readstart(InputByteShift - 1, 0),
          UInt(0, 3))
        writeaddr := io.cmd.bits.writestart
        len := io.cmd.bits.len
        bytesread := UInt(0)

        switch (io.cmd.bits.action) {
          is (StreamKeyAction) {
            state := s_stream_key_wait
            byword := Bool(false)
            outputMask := Bits("b01")
          }
          is (CopyValueAction) {
            state := s_req_data
            byword := Bool(ValWordShift == 1)
            outputMask := Bits("b10")
          }
        }
      }
    }
    is (s_stream_key_wait) {
      when (io.keyData.ready) {
        state := s_req_data
      }
    }
    is (s_shift_words) {
      writeaddr := writeaddr + UInt(1)

      when (splitWord) {
        word := Cat(UInt(0, 8), word(coreDataBits - 1, 8))
        bytesread := bytesread + UInt(1)
        byteOff := byteOff + UInt(1)
      } .elsewhen (bytesread === len - UInt(3)) {
        word := Cat(UInt(0, coreDataBits - 8), word(23, 16))
        bytesread := bytesread + UInt(ValWordBytes)
        byteOff := byteOff + UInt(ValWordBytes)
      } .elsewhen (byteOff === UInt(BytesPerInputWord - 3)) {
        word := Cat(UInt(0, ValWordSize), word(coreDataBits - 1, ValWordSize))
        savedByte := word(23, 16)
        bytesread := bytesread + UInt(ValWordBytes + 1)
        byteOff := byteOff + UInt(ValWordBytes + 1)
      } .otherwise {
        word := Cat(UInt(0, ValWordSize), word(coreDataBits - 1, ValWordSize))
        bytesread := bytesread + UInt(ValWordBytes)
        byteOff := byteOff + UInt(ValWordBytes)
      }

      when (bytesread === len - UInt(3)) {
        // stay in the same state
      } .elsewhen (bytesread === len - UInt(2) || bytesread === len - UInt(1)) {
        state := s_finish
      } .elsewhen (byteOff === UInt(BytesPerInputWord - 2)) {
        state := s_req_data
      } .elsewhen (byteOff === UInt(BytesPerInputWord - 3)) {
        state := s_req_data
      }
    }
    is (s_shift_bytes) {
      bytesread := bytesread + UInt(1)
      writeaddr := writeaddr + UInt(1)
      byteOff := byteOff + UInt(1)
      word := Cat(UInt(0, 8), word(coreDataBits - 1, 8))
      when (bytesread === len - UInt(1)) {
        state := s_finish
      } .elsewhen (byteOff === UInt(BytesPerInputWord - 1)) {
        state := s_req_data
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
        when (initshift === UInt(0)) {
          byteOff := UInt(0)
          when (byword) {
            state := s_shift_words
          } .otherwise {
            state := s_shift_bytes
          }
        } .elsewhen (byword && initByteOff === UInt(BytesPerInputWord - 1)) {
          state := s_req_data
          bytesread := bytesread + UInt(1)
          when (len === UInt(1)) {
            word := Cat(UInt(0, coreDataBits - 8),
              io.mem.resp.bits.data(coreDataBits - 1, coreDataBits - 8))
            state := s_shift_words
          } .otherwise {
            savedByte := io.mem.resp.bits.data(
              coreDataBits - 1, coreDataBits - 8)
            initshift := UInt(0)
          }
        } .otherwise {
          state := s_init_shift
        }
      }
    }
    is (s_init_shift) {
      word := word >> initshift
      byteOff := initByteOff
      initshift := UInt(0)
      when (byword) {
        state := s_shift_words
      } .otherwise {
        state := s_shift_bytes
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
  val keyWords = messToWords(key, 8, 7)
  val cacheWords = messToWords(key, c.ValWordBytes)

  memory.store_data(0, keyWords)

  wire_poke(c.io.cmd.valid, 1)
  wire_poke(c.io.cmd.bits.action, 0)
  wire_poke(c.io.cmd.bits.readstart, 7)
  wire_poke(c.io.cmd.bits.writestart, 0)
  wire_poke(c.io.cmd.bits.len, key.length)
  takestep()
  wire_poke(c.io.cmd.valid, 0)
  wire_poke(c.io.keyData.ready, 1)

  var addr = 0

  until (peek(c.io.cmd.ready) == 1, 450) {
    expect(c.io.cacheWriteEn, 0)
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

  addr = 0

  until (peek(c.io.cmd.ready) == 1, 450) {
    expect(c.io.keyData.valid, 0)
    if (peek(c.io.cacheWriteEn) == 1) {
      isTrace = true
      expect(c.io.cacheWriteAddr, addr)
      expect(c.io.cacheWriteData, cacheWords(addr))
      isTrace = false
      addr += 1
    }
    memory.process()
  }
}

object MemoryHandlerMain {
  def main(args: Array[String]) {
    chiselMain.run(args, () => new MemoryHandler(16, 6, 5),
      (c: MemoryHandler) => new MemoryHandlerTest(c))
  }
}

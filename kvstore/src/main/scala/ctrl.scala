package kvstore

import Chisel._
import kvstore.Constants._

class CtrlModule(WordSize: Int, ValAddrSize: Int, KeyLenSize: Int,
    HashSize: Int, TagSize: Int) extends Module with CoreParameters {
  val ByteShift = log2Up(WordSize) - 3
  val KeyAddrSize = KeyLenSize - ByteShift
  val io = new Bundle {
    val rocc = new RoCCInterface

    val cacheWriteAddr = UInt(OUTPUT, ValAddrSize)
    val cacheWriteData = UInt(OUTPUT, 8)
    val cacheWriteEn = Bool(OUTPUT)

    val addrLenAddr = UInt(OUTPUT, HashSize)
    val addrLenWriteData = new AddrLenPair(ValAddrSize, OUTPUT)
    val addrLenWriteEn = Vec.fill(2) { Bool(OUTPUT) }
    val addrLenReadData = new AddrLenPair(ValAddrSize, INPUT)
    val addrLenReadEn = Bool(OUTPUT)

    val keyLenAddr = UInt(OUTPUT, HashSize)
    val keyLenData = UInt(OUTPUT, KeyLenSize)
    val keyLenWrite = Bool(OUTPUT)

    val lock = Bool(OUTPUT)
    val halted = Bool(INPUT)
    val writemode = Bool(OUTPUT)
    val findAvailable = Bool(OUTPUT)

    val keyInfo = Decoupled(new MessageInfo(ValAddrSize, TagSize))
    val keyData = Decoupled(UInt(width = 8))
    val hashSel = Decoupled(new HashSelection(HashSize, TagSize)).flip
    val copyReq = Decoupled(new CopyRequest(HashSize, KeyLenSize))
  }

  val writemode = Reg(init = Bool(false))
  io.writemode := writemode

  val wantmode = Reg(Bool())
  val result = Reg(init = Bits(0, 64))

  val findAvailable = Reg(Bool())
  io.findAvailable := findAvailable

  val (s_wait :: s_switch ::
    s_send_info :: s_stream_key :: s_gethash ::
    s_reskey_setlen :: s_reskey_start_copy :: s_reskey_end_copy ::
    s_waitaddrlen :: s_getaddrlen ::
    s_stream_value :: s_stream_value_finish ::
    s_delkey_setlen :: s_finish :: Nil) = Enum(UInt(), 14)

  val state = Reg(init = s_wait)
  val found_state = Reg(init = s_wait)

  val action = Reg(Bits(width = ActionSize))
  val len = Reg(UInt(width = coreDataBits))
  val keytag = Reg(UInt(width = TagSize))
  val readstart = Reg(UInt(width = coreDataBits))
  val writestart = Reg(UInt(width = ValAddrSize))
  val memCmdValid = (state === s_stream_key || state === s_stream_value)

  val memhandler = Module(
    new MemoryHandler(ValAddrSize, KeyAddrSize))
  memhandler.io.keyData <> io.keyData
  memhandler.io.mem     <> io.rocc.mem
  memhandler.io.cacheWriteAddr <> io.cacheWriteAddr
  memhandler.io.cacheWriteData <> io.cacheWriteData
  memhandler.io.cacheWriteEn   <> io.cacheWriteEn
  memhandler.io.cmd.valid := memCmdValid
  memhandler.io.cmd.bits.len := len
  memhandler.io.cmd.bits.readstart := readstart
  memhandler.io.cmd.bits.writestart := writestart
  memhandler.io.cmd.bits.action := action

  val hash = Reg(UInt(width = HashSize))
  val addrLenData = Reg(new AddrLenPair(ValAddrSize))
  val setLen = Reg(init = Bits(width = 3))

  io.addrLenReadEn := (state === s_waitaddrlen)
  io.addrLenAddr := hash
  io.addrLenWriteData := addrLenData
  io.addrLenWriteEn   := setLen(2, 1).toBools
  io.keyLenAddr  := hash
  io.keyLenData  := len(KeyLenSize - 1, 0)
  io.keyLenWrite := setLen(0).toBool

  io.lock := (state === s_switch)

  val respData = Reg(Bits(width = coreDataBits))
  val respDest = Reg(Bits(width = 5))
  io.rocc.resp.bits.data := respData
  io.rocc.resp.bits.rd   := respDest

  io.keyInfo.valid := (state === s_send_info)
  io.keyInfo.bits.tag := keytag
  io.keyInfo.bits.len := len(KeyLenSize - 1, 0)

  io.copyReq.bits.hash := hash
  io.copyReq.bits.len := len(KeyLenSize - 1, 0)
  io.copyReq.valid := (state === s_reskey_start_copy)

  io.hashSel.ready := (state === s_gethash)

  val AddrLookupDelay = 2
  val delayCount = Reg(UInt(width = log2Up(AddrLookupDelay)))

  switch (state) {
    is (s_wait) {
      setLen := Bits("b000")
      when (io.rocc.cmd.valid) {
        respDest := io.rocc.cmd.bits.inst.rd

        switch (io.rocc.cmd.bits.inst.funct) {
          is (SwitchModeInst) {
            findAvailable := Bool(false)
            wantmode := io.rocc.cmd.bits.inst.rs1(0).toBool
            state := s_switch
          }
          is (DelKeyInst) {
            action := StreamKeyAction
            readstart := io.rocc.cmd.bits.rs1
            writestart := UInt(0)
            len := io.rocc.cmd.bits.rs2
            state := s_send_info
            found_state := s_delkey_setlen
            findAvailable := Bool(false)
          }
          is (ReserveKeyInst) {
            action := StreamKeyAction
            readstart := io.rocc.cmd.bits.rs1
            writestart := UInt(0)
            len := io.rocc.cmd.bits.rs2(KeyLenSize - 1, 0)
            keytag := io.rocc.cmd.bits.rs2(KeyLenSize + TagSize - 1, KeyLenSize)
            state := s_send_info
            found_state := s_reskey_setlen
            findAvailable := Bool(true)
          }
          is (AssocAddrInst) {
            hash := io.rocc.cmd.bits.rs1
            addrLenData.addr := io.rocc.cmd.bits.rs2
            setLen := Bits("b010")
          }
          is (AssocLenInst) {
            hash := io.rocc.cmd.bits.rs1
            addrLenData.len := io.rocc.cmd.bits.rs2
            setLen := Bits("b100")
          }
          is (WriteValInst) {
            action := CopyValueAction
            hash := io.rocc.cmd.bits.rs1(HashSize - 1, 0)
            readstart := io.rocc.cmd.bits.rs2
            state := s_waitaddrlen
            delayCount := UInt(AddrLookupDelay - 1)
          }
        }
      }
    }
    is (s_switch) {
      when (io.halted) {
        writemode := wantmode
        state := s_wait
      }
    }
    is (s_send_info) {
      when (io.keyInfo.ready) {
        state := s_stream_key
      }
    }
    is (s_stream_key) {
      when (memhandler.io.cmd.ready) {
        state := s_gethash
      }
    }
    is (s_gethash) {
      when (io.hashSel.valid) {
        when (io.hashSel.bits.found) {
          hash := io.hashSel.bits.hash
          state := found_state
        } .otherwise {
          respData := Bits(HashNotFound)
          state := s_finish
        }
      }
    }
    is (s_delkey_setlen) {
      addrLenData.addr := UInt(0)
      addrLenData.len  := UInt(0)
      len := UInt(0)
      setLen := Bits("b111")
      respData := hash.toBits
      state := s_finish
    }
    is (s_reskey_setlen) {
      addrLenData.addr := UInt(0)
      addrLenData.len  := UInt(0)
      setLen := Bits("b001")
      respData := hash.toBits
      state := s_reskey_start_copy
    }
    is (s_reskey_start_copy) {
      setLen := Bool(false)
      when (io.copyReq.ready) {
        state := s_reskey_end_copy
      }
    }
    is (s_reskey_end_copy) {
      when (io.copyReq.ready) {
        state := s_finish
      }
    }
    is (s_finish) {
      setLen := Bits("b000")
      when (io.rocc.resp.ready) {
        state := s_wait
      }
    }
    is (s_waitaddrlen) {
      when (delayCount === UInt(0)) {
        state := s_getaddrlen
      } .otherwise {
        delayCount := delayCount - UInt(1)
      }
    }
    is (s_getaddrlen) {
      writestart := io.addrLenReadData.addr
      len := io.addrLenReadData.len
      state := s_stream_value
    }
    is (s_stream_value) {
      when (memhandler.io.cmd.ready) {
        state := s_stream_value_finish
      }
    }
    is (s_stream_value_finish) {
      when (memhandler.io.cmd.ready) {
        state := s_wait
      }
    }
  }

  io.rocc.cmd.ready := (state === s_wait)
  io.rocc.resp.valid := (state === s_finish)
}

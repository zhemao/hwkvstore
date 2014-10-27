package McAccel

import Chisel._
import McAccel.Constants._

class CtrlModule(WordSize: Int, ValAddrSize: Int, KeyLenSize: Int,
    HashSize: Int, TagSize: Int) extends Module with CoreParameters {
  val ByteShift = log2Up(WordSize) - 3
  val KeyAddrSize = KeyLenSize - ByteShift
  val io = new Bundle {
    val rocc = new RoCCInterface

    val cacheWriteAddr = UInt(OUTPUT, ValAddrSize)
    val cacheWriteData = UInt(OUTPUT, 8)
    val cacheWriteEn = Bool(OUTPUT)

    val addrLenWriteAddr = UInt(OUTPUT, HashSize)
    val addrLenWriteData = new AddrLenPair(ValAddrSize, OUTPUT)
    val addrLenWriteEn = Bool(OUTPUT)

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
    s_send_info :: s_stream_data :: s_gethash ::
    s_reskey_setlen :: s_reskey_start_copy :: s_reskey_end_copy ::
    s_delkey_setlen :: s_finish :: Nil) = Enum(UInt(), 10)

  val state = Reg(init = s_wait)
  val found_state = Reg(init = s_wait)

  val action = Reg(Bits(width = ActionSize))
  val len = Reg(UInt(width = coreDataBits))
  val keytag = Reg(UInt(width = TagSize))
  val readstart = Reg(UInt(width = coreDataBits))
  val writestart = Reg(UInt(width = ValAddrSize))
  val memCmdValid = (state === s_stream_data)

  val memhandler = Module(
    new MemoryHandler(WordSize, ValAddrSize, KeyAddrSize))
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
  val setLen = Reg(init = Bool(false))

  io.addrLenWriteAddr := hash
  io.addrLenWriteData := addrLenData
  io.addrLenWriteEn   := setLen
  io.keyLenAddr  := hash
  io.keyLenData  := len(KeyLenSize - 1, 0)
  io.keyLenWrite := setLen

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

  switch (state) {
    is (s_wait) {
      when (io.rocc.cmd.valid) {
        respDest := io.rocc.cmd.bits.inst.rd

        switch (io.rocc.cmd.bits.inst.funct) {
          is (SwitchModeInst) {
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
            len := io.rocc.cmd.bits.rs2
            state := s_send_info
            found_state := s_reskey_setlen
            findAvailable := Bool(true)
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
        state := s_stream_data
      }
    }
    is (s_stream_data) {
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
      setLen := Bool(true)
      respData := hash.toBits
      state := s_finish
    }
    is (s_reskey_setlen) {
      addrLenData.addr := UInt(0)
      addrLenData.len  := UInt(0)
      setLen := Bool(true)
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
      setLen := Bool(false)
      when (io.rocc.resp.ready) {
        state := s_wait
      }
    }
  }

  io.rocc.cmd.ready := (state === s_wait)
  io.rocc.resp.valid := (state === s_finish)
}

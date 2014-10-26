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

    val allKeyAddr = UInt(OUTPUT, KeyAddrSize)
    val allKeyData = UInt(OUTPUT, WordSize)
    val allKeyWrite = Bool(OUTPUT)

    val keyLenAddr = UInt(OUTPUT, HashSize)
    val keyLenData = UInt(OUTPUT, KeyLenSize)
    val keyLenWrite = Bool(OUTPUT)

    val lock = Bool(OUTPUT)
    val halted = Bool(INPUT)
    val writemode = Bool(OUTPUT)

    val keyInfo = Decoupled(new MessageInfo(ValAddrSize, TagSize))
    val keyData = Decoupled(UInt(width = 8))
    val hashSel = Decoupled(new HashSelection(HashSize, TagSize)).flip
  }

  val writemode = Reg(init = Bool(false))
  io.writemode := writemode

  val wantmode = Reg(Bool())
  val result = Reg(init = Bits(0, 64))

  val (s_wait :: s_switch ::
    s_delkey_send_info :: s_delkey_stream_data ::
    s_delkey_gethash :: s_delkey_setlen ::
    s_finish :: Nil) = Enum(UInt(), 7)

  val state = Reg(init = s_wait)

  val action = Reg(Bits(width = ActionSize))
  val len = Reg(UInt(width = coreDataBits))
  val keytag = Reg(UInt(width = TagSize))
  val readstart = Reg(UInt(width = coreDataBits))
  val writestart = Reg(UInt(width = ValAddrSize))
  val memCmdValid = (state === s_delkey_stream_data)

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
  val keyLenData = Reg(UInt(width = KeyLenSize))
  val setLen = (state === s_delkey_setlen)

  io.addrLenWriteAddr := hash
  io.addrLenWriteData := addrLenData
  io.addrLenWriteEn   := setLen
  io.keyLenAddr  := hash
  io.keyLenData  := keyLenData
  io.keyLenWrite := setLen

  io.lock := (state === s_switch)

  val respData = Reg(Bits(width = coreDataBits))
  val respDest = Reg(Bits(width = 5))
  io.rocc.resp.bits.data := respData
  io.rocc.resp.bits.rd   := respDest

  io.keyInfo.valid := (state === s_delkey_send_info)
  io.keyInfo.bits.tag := keytag
  io.keyInfo.bits.len := len(KeyLenSize - 1, 0)

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
            keytag := UInt(0)
            state := s_delkey_send_info
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
    is (s_delkey_send_info) {
      when (io.keyInfo.ready) {
        state := s_delkey_stream_data
      }
    }
    is (s_delkey_stream_data) {
      when (memhandler.io.cmd.ready) {
        state := s_delkey_gethash
      }
    }
    is (s_delkey_gethash) {
      when (io.hashSel.valid) {
        when (io.hashSel.bits.found) {
          addrLenData.addr := UInt(0)
          addrLenData.len  := UInt(0)
          keyLenData := UInt(0)
          hash := io.hashSel.bits.hash
          state := s_delkey_setlen
        } .otherwise {
          respData := Bits(HashNotFound)
          state := s_finish
        }
      }
    }
    is (s_delkey_setlen) {
      respData := hash.toBits
      state := s_finish
    }
    is (s_finish) {
      when (io.rocc.resp.ready) {
        state := s_wait
      }
    }
  }

  io.rocc.cmd.ready := (state === s_wait)
  io.rocc.resp.valid := (state === s_finish)
}

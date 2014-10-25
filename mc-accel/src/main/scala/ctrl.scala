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
    val hashSel = Decoupled(new HashSelection(HashSize, TagSize))
  }

  val lock = Reg(init = Bool(false))
  val writemode = Reg(init = Bool(false))
  io.lock := lock
  io.writemode := writemode

  val wantmode = Reg(Bool())
  val result = Reg(init = Bits(0, 64))

  val (s_wait :: s_switch :: s_delkey_send_info :: s_delkey_stream_data :: s_finish :: Nil) = Enum(UInt(), 5)

  val state = Reg(init = s_wait)

  val len = Reg(UInt(width = KeyLenSize))
  val keytag = Reg(UInt(width = TagSize))

  switch (state) {
    is (s_wait) {
      when (io.rocc.cmd.valid) {
        switch (io.rocc.cmd.bits.inst.funct) {
          is (SwitchModeInst) {
            lock := Bool(true)
            wantmode := io.rocc.cmd.bits.inst.rs1(0).toBool
            state := s_switch
          }
          is (DelKeyInst) {
            len := io.rocc.cmd.bits.rs1(KeyLenSize - 1, 0)
            keytag := UInt(0)
          }
        }
      }
    }
    is (s_switch) {
      when (io.halted) {
        writemode := wantmode
        state := s_finish
      }
    }
    is (s_delkey_send_info) {
      when (io.keyInfo.ready) {
        state := s_delkey_stream_data
      }
    }
    is (s_delkey_stream_data) {
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

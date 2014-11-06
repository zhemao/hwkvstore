package McAccel

import Chisel._

class KeyCopier(HashSize: Int, WordSize: Int, KeySize: Int) extends Module {
  val BytesPerWord = WordSize / 8
  val KeyLenSize = log2Up(KeySize)
  val WordShift = log2Up(WordSize) - 3
  val KeyAddrSize = KeyLenSize - WordShift
  val ReadDelay = 3

  val io = new Bundle {
    val curKeyAddr = UInt(OUTPUT, KeyAddrSize)
    val curKeyData = UInt(INPUT, WordSize)

    val allKeyAddr = UInt(OUTPUT, HashSize + KeyAddrSize)
    val allKeyData = UInt(OUTPUT, WordSize)
    val allKeyWrite = Bool(OUTPUT)

    val copyReq = Decoupled(new CopyRequest(HashSize, KeyLenSize)).flip
    val selCopy = Bool(OUTPUT)
  }

  val s_wait :: s_copy :: Nil = Enum(Bits(), 2)
  val state = Reg(init = s_wait)

  val index = Reg(UInt(width = KeyAddrSize))
  val hash = Reg(UInt(width = HashSize))
  val write = (state === s_copy)
  val len = Reg(UInt(width = KeyLenSize))
  val nextlen = Mux(len(KeyLenSize - 1, WordShift) === UInt(0),
    UInt(0), len - UInt(BytesPerWord))

  val delayedWrite = ShiftRegister(write, ReadDelay)
  val delayedIndex = ShiftRegister(index, ReadDelay)

  io.curKeyAddr := index
  io.allKeyAddr := Cat(hash, delayedIndex)
  io.allKeyWrite := delayedWrite
  io.allKeyData := io.curKeyData
  io.selCopy := (state === s_copy)
  io.copyReq.ready := (state === s_wait)

  switch (state) {
    is (s_wait) {
      when (io.copyReq.valid) {
        index := UInt(0)
        hash := io.copyReq.bits.hash
        len := io.copyReq.bits.len
        state := s_copy
      }
    }
    is (s_copy) {
      when (nextlen === UInt(0)) {
        state := s_wait
      } .otherwise {
        index := index + UInt(1)
        len := nextlen
      }
    }
  }
}

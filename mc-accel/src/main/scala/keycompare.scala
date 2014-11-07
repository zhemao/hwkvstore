package McAccel

import Chisel._
import McAccel.TestUtils._
import McAccel.Constants.MaxFanIn

class KeyCompare(HashSize: Int, WordSize: Int, KeySize: Int, TagSize: Int)
    extends Module {
  val KeyLenSize = log2Up(KeySize)
  val WordShift = log2Up(WordSize) - 3
  val KeyAddrSize = KeyLenSize - WordShift

  val io = new Bundle {
    val curKeyAddr = UInt(OUTPUT, KeyAddrSize)
    val curKeyData = UInt(INPUT, WordSize)
    val allKeyAddr = UInt(OUTPUT, HashSize + KeyAddrSize)
    val allKeyData = UInt(INPUT, WordSize)
    val lenAddr = UInt(OUTPUT, HashSize)
    val lenData = UInt(INPUT, KeyLenSize)
    val hashIn = Decoupled(new HashInfo(HashSize, KeyLenSize, TagSize)).flip
    val hashOut = Decoupled(new HashSelection(HashSize, TagSize))
    val findAvailable = Bool(INPUT)
  }

  val DecodeDelay = (HashSize - 1) / log2Up(MaxFanIn) + 1
  val AllReadDelay = 2 + DecodeDelay
  val curKeyData = ShiftRegister(io.curKeyData, DecodeDelay)

  val index = Reg(UInt(width = KeyAddrSize))
  val delayedIndex = ShiftRegister(index, AllReadDelay)

  val curInfo = Reg(new HashInfo(HashSize, KeyLenSize, TagSize))
  val checkFirst = Reg(Bool())
  val curHash = Mux(checkFirst, curInfo.hash1, curInfo.hash2)

  // curKeyAddr and allKeyAddr will be delayed by one cycle outside
  // lenAddr should not be delayed
  io.curKeyAddr := index
  io.allKeyAddr := Cat(curHash, index)
  io.lenAddr := curHash

  val wordLen = curInfo.len(KeyLenSize - 1, WordShift)
  val reachedEnd = if (WordShift == 0) {
    delayedIndex === wordLen - UInt(1)
  } else {
    val byteOff = curInfo.len(WordShift - 1, 0)
    Mux(byteOff === UInt(0),
      delayedIndex === wordLen - UInt(1),
      delayedIndex === wordLen)
  }

  val (s_wait :: s_delay_len :: s_check_len ::
    s_delay_data :: s_check_data :: s_handoff :: Nil) = Enum(UInt(), 6)
  val state = Reg(init = s_wait)

  val hashFound = Reg(Bool())
  val delayCount = Reg(UInt(width = log2Up(AllReadDelay)))

  io.hashOut.bits.hash := curHash
  io.hashOut.bits.found := hashFound
  io.hashOut.bits.tag := curInfo.tag
  io.hashOut.valid := (state === s_handoff)
  io.hashIn.ready := (state === s_wait)

  switch (state) {
    is (s_wait) {
      checkFirst := Bool(true)
      hashFound := Bool(false)
      when (io.hashIn.valid) {
        curInfo := io.hashIn.bits
        state := s_delay_len
      }
    }
    is (s_delay_len) {
      state := s_check_len
    }
    is (s_check_len) {
      when (io.findAvailable && io.lenData === UInt(0)) {
        hashFound := Bool(true)
        state := s_handoff
      } .elsewhen (io.lenData === curInfo.len) {
        index := UInt(0)
        delayCount := UInt(AllReadDelay - 1)
        state := s_delay_data
      } .elsewhen (checkFirst) {
        checkFirst := Bool(false)
        state := s_delay_len
      } .otherwise {
        state := s_handoff
      }
    }
    is (s_delay_data) {
      when (delayCount === UInt(0)) {
        state := s_check_data
      } .otherwise {
        delayCount := delayCount - UInt(1)
        index := index + UInt(1)
      }
    }
    is (s_check_data) {
      when (curKeyData != io.allKeyData) {
        when (checkFirst) {
          checkFirst := Bool(false)
          state := s_delay_len
        } .otherwise {
          state := s_handoff
        }
      } .elsewhen (reachedEnd) {
        hashFound := Bool(true)
        state := s_handoff
      } .otherwise {
        index := index + UInt(1)
      }
    }
    is (s_handoff) {
      when (io.hashOut.ready) {
        state := s_wait
      }
    }
  }
}

class KeyCompareSetup(
    val WordSize: Int, val MaxKeySize: Int, val NumKeys: Int, TagSize: Int)
      extends Module {
  val KeyLenSize = log2Up(MaxKeySize)
  val WordShift = log2Up(WordSize) - 3
  val KeyAddrSize = KeyLenSize - WordShift
  val HashSize = log2Up(NumKeys)
  val WordBytes = WordSize / 8
  val CurKeyWords = MaxKeySize / WordBytes
  val AllKeyWords = CurKeyWords * NumKeys

  val io = new Bundle {
    val curKeyAddr = UInt(INPUT, KeyAddrSize)
    val curKeyData = UInt(INPUT, WordSize)
    val curKeyWrite = Bool(INPUT)
    val allKeyAddr = UInt(INPUT, KeyAddrSize + HashSize)
    val allKeyData = UInt(INPUT, WordSize)
    val allKeyWrite = Bool(INPUT)
    val lenAddr = UInt(INPUT, HashSize)
    val lenData = UInt(INPUT, KeyLenSize)
    val lenWrite = Bool(INPUT)
    val hash1 = UInt(INPUT, HashSize)
    val hash2 = UInt(INPUT, HashSize)
    val len = UInt(INPUT, KeyLenSize)
    val intag = UInt(INPUT, TagSize)
    val hashsel = UInt(OUTPUT, HashSize)
    val start = Bool(INPUT)
    val finish = Bool(INPUT)
    val ready = Bool(OUTPUT)
    val done = Bool(OUTPUT)
    val found = Bool(OUTPUT)
    val outtag = UInt(OUTPUT, TagSize)
    val findAvailable = Bool(INPUT)
  }

  val curKeyMem = Module(new UnbankedMem(WordSize, CurKeyWords))
  val allKeyMem = Module(new BankedMem(WordSize, CurKeyWords, NumKeys))
  val lenMem = Mem(UInt(width = KeyLenSize), NumKeys)

  val keycomp = Module(new KeyCompare(HashSize, WordSize, MaxKeySize, TagSize))
  keycomp.io.hashIn.bits.hash1 := io.hash1
  keycomp.io.hashIn.bits.hash2 := io.hash2
  keycomp.io.hashIn.bits.len := io.len
  keycomp.io.hashIn.bits.tag := io.intag
  keycomp.io.hashIn.valid := io.start
  keycomp.io.hashOut.ready := io.finish
  keycomp.io.findAvailable := io.findAvailable
  io.ready := keycomp.io.hashIn.ready
  io.done := keycomp.io.hashOut.valid
  io.hashsel := keycomp.io.hashOut.bits.hash
  io.found := keycomp.io.hashOut.bits.found
  io.outtag := keycomp.io.hashOut.bits.tag

  keycomp.io.curKeyData <> curKeyMem.io.readData
  keycomp.io.curKeyAddr <> curKeyMem.io.readAddr
  keycomp.io.allKeyData <> allKeyMem.io.readData
  keycomp.io.allKeyAddr <> allKeyMem.io.readAddr

  val lenReadAddr = Reg(next = keycomp.io.lenAddr)
  keycomp.io.lenData := lenMem(lenReadAddr)

  curKeyMem.io.writeAddr := io.curKeyAddr
  curKeyMem.io.writeData := io.curKeyData
  curKeyMem.io.writeEn   := io.curKeyWrite

  allKeyMem.io.writeAddr := io.allKeyAddr
  allKeyMem.io.writeData := io.allKeyData
  allKeyMem.io.writeEn   := io.allKeyWrite
  allKeyMem.io.readEn    := Bool(true)

  when (io.lenWrite) {
    lenMem(io.lenAddr) := io.lenData
  }
}

class KeyCompareTest(c: KeyCompareSetup) extends Tester(c) {
  val key1 = "abcdefghijklmnopqrstuvwxyz"
  val key2 = "abcdefghijklmnopqrstuvwxzy"
  val key3 = "0123456789"

  val WordBytes = c.WordSize / 8

  def writeCurKey(key: String) {
    val keyWords = messToWords(key, WordBytes)
    poke(c.io.curKeyWrite, 1)
    for (i <- 0 until keyWords.length) {
      poke(c.io.curKeyAddr, i)
      poke(c.io.curKeyData, keyWords(i))
      step(1)
    }
    poke(c.io.curKeyWrite, 0)
  }

  def writeKeyData(hash: Int, key: String) {
    val keyWords = messToWords(key, WordBytes)
    val start = hash * c.MaxKeySize / WordBytes
    poke(c.io.lenWrite, 1)
    poke(c.io.lenAddr, hash)
    poke(c.io.lenData, key.length)
    step(1)
    poke(c.io.lenWrite, 0)

    poke(c.io.allKeyWrite, 1)
    for (i <- 0 until keyWords.length) {
      poke(c.io.allKeyAddr, start + i)
      poke(c.io.allKeyData, keyWords(i))
      step(1)
    }
    poke(c.io.allKeyWrite, 0)
  }

  def waitForDone {
    isTrace = false
    while (peek(c.io.done) == 0)
      step(1)
    isTrace = true
  }

  def checkHashes(hash1: Int, hash2: Int, expectation: Int) {
    expect(c.io.ready, 1)

    poke(c.io.hash1, hash1)
    poke(c.io.hash2, hash2)
    poke(c.io.start, 1)
    step(1)
    poke(c.io.start, 0)

    waitForDone

    expectation match {
      case 0 => {
        expect(c.io.found, 1)
        expect(c.io.hashsel, hash1)
      }
      case 1 => {
        expect(c.io.found, 1)
        expect(c.io.hashsel, hash2)
      }
      case 2 => {
        expect(c.io.found, 0)
      }
    }

    poke(c.io.finish, 1)
    step(1)
    poke(c.io.finish, 0)
  }

  writeCurKey(key1)
  writeKeyData(0, key1)
  writeKeyData(1, key2)
  writeKeyData(2, key3)

  poke(c.io.len, key1.length)

  // check that we find the answer when hash1 is 0
  checkHashes(0, 1, 0)

  // check that we find the answer when hash2 is 0
  checkHashes(1, 0, 1)

  // check that we find no answer when neither of the hashes is 0
  checkHashes(1, 2, 2)

  poke(c.io.findAvailable, 1)
  checkHashes(1, 3, 1)
  checkHashes(2, 3, 1)
}

object KeyCompareMain {
  def main(args: Array[String]) {
    chiselMainTest(args, () => Module(new KeyCompareSetup(32, 256, 8, 4))) {
      c => new KeyCompareTest(c)
    }
  }
}

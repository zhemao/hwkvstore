package McAccel

import Chisel._
import McAccel.TestUtils._

class KeyCompare(KeyAddrSize: Int, HashSize: Int, WordSize: Int) extends Module {
  val WordShift = log2Up(WordSize) - 3
  val KeyLenSize = KeyAddrSize + WordShift

  val io = new Bundle {
    val curKeyAddr = UInt(OUTPUT, KeyAddrSize)
    val curKeyData = UInt(INPUT, WordSize)
    val allKeyAddr = UInt(OUTPUT, HashSize + KeyAddrSize)
    val allKeyData = UInt(INPUT, WordSize)
    val hashIn = new DecoupledIO(new HashInfo(HashSize, KeyLenSize)).flip
    val hashOut = new DecoupledIO(new HashSelection(HashSize))
  }

  val index = Reg(UInt(width = KeyAddrSize))
  val curInfo = Reg(new HashInfo(HashSize, KeyLenSize))
  val checkFirst = Reg(Bool())
  val curHash = Mux(checkFirst, curInfo.hash1, curInfo.hash2)

  // the addresses will be delayed by one going out
  // first data is word is at index 1, since index 0 is for length
  io.curKeyAddr := index - UInt(1)
  io.allKeyAddr := Cat(curHash, index)

  val wordLen = curInfo.len(KeyLenSize - 1, WordShift)
  val byteOff = curInfo.len(WordShift - 1, 0)
  val reachedEnd = Mux(byteOff === UInt(0),
    index === wordLen + UInt(1), index === wordLen + UInt(2))

  val (s_wait :: s_delay_len :: s_check_len ::
    s_delay_data :: s_check_data :: s_handoff :: Nil) = Enum(UInt(), 6)
  val state = Reg(init = s_wait)

  val hashFound = Reg(Bool())

  io.hashOut.bits.hash := curHash
  io.hashOut.bits.found := hashFound
  io.hashOut.valid := (state === s_handoff)
  io.hashIn.ready := (state === s_wait)

  switch (state) {
    is (s_wait) {
      when (io.hashIn.valid) {
        curInfo := io.hashIn.bits
        checkFirst := Bool(true)
        index := UInt(0)
        state := s_delay_len
        hashFound := Bool(false)
      }
    }
    is (s_delay_len) {
      state := s_check_len
    }
    is (s_check_len) {
      when (io.allKeyData(KeyAddrSize - 1, 0) === curInfo.len) {
        state := s_delay_data
        index := UInt(1)
      } .elsewhen (checkFirst) {
        checkFirst := Bool(false)
        state := s_delay_len
      } .otherwise {
        state := s_handoff
      }
    }
    is (s_delay_data) {
      state := s_check_data
    }
    is (s_check_data) {
      when (io.curKeyData != io.allKeyData) {
        when (checkFirst) {
          checkFirst := Bool(false)
          state := s_delay_len
          index := UInt(0)
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

class KeyCompareSetup(val MaxKeySize: Int, val NumKeys: Int, val WordSize: Int)
    extends Module {
  val KeyAddrSize = log2Up(MaxKeySize)
  val HashSize = log2Up(NumKeys)
  val io = new Bundle {
    val curKeyAddr = UInt(INPUT, KeyAddrSize)
    val curKeyData = UInt(INPUT, WordSize)
    val curKeyWrite = Bool(INPUT)
    val allKeyAddr = UInt(INPUT, KeyAddrSize + HashSize)
    val allKeyData = UInt(INPUT, WordSize)
    val allKeyWrite = Bool(INPUT)
    val hash1 = UInt(INPUT, HashSize)
    val hash2 = UInt(INPUT, HashSize)
    val len = UInt(INPUT, KeyAddrSize)
    val hashsel = UInt(OUTPUT, HashSize)
    val start = Bool(INPUT)
    val finish = Bool(INPUT)
    val ready = Bool(OUTPUT)
    val done = Bool(OUTPUT)
    val found = Bool(OUTPUT)
  }

  val curKeyMem = Mem(UInt(width = WordSize), MaxKeySize)
  val allKeyMem = Mem(UInt(width = WordSize), NumKeys * MaxKeySize)

  val keycomp = Module(new KeyCompare(KeyAddrSize, HashSize, WordSize))
  keycomp.io.hashIn.bits.hash1 := io.hash1
  keycomp.io.hashIn.bits.hash2 := io.hash2
  keycomp.io.hashIn.bits.len := io.len
  keycomp.io.hashIn.valid := io.start
  keycomp.io.hashOut.ready := io.finish
  io.ready := keycomp.io.hashIn.ready
  io.done := keycomp.io.hashOut.valid
  io.hashsel := keycomp.io.hashOut.bits.hash
  io.found := keycomp.io.hashOut.bits.found

  val curKeyAddr = Reg(next = keycomp.io.curKeyAddr)
  keycomp.io.curKeyData := curKeyMem(curKeyAddr)

  val allKeyAddr = Reg(next = keycomp.io.allKeyAddr)
  keycomp.io.allKeyData := allKeyMem(allKeyAddr)

  when (io.curKeyWrite) {
    curKeyMem(io.curKeyAddr) := io.curKeyData
  }

  when (io.allKeyWrite) {
    allKeyMem(io.allKeyAddr) := io.allKeyData
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

  def writeKeyData(start: Int, key: String) {
    val keyWords = messToWords(key, WordBytes)
    poke(c.io.allKeyWrite, 1)
    poke(c.io.allKeyAddr, start)
    poke(c.io.allKeyData, key.length)
    step(1)
    for (i <- 0 until keyWords.length) {
      poke(c.io.allKeyAddr, start + i + 1)
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
  writeKeyData(c.MaxKeySize, key2)
  writeKeyData(2 * c.MaxKeySize, key3)

  poke(c.io.len, key1.length)

  // check that we find the answer when hash1 is 0
  checkHashes(0, 1, 0)

  // check that we find the answer when hash2 is 0
  checkHashes(1, 0, 1)

  // check that we find no answer when neither of the hashes is 0
  checkHashes(1, 2, 2)
}

object KeyCompareMain {
  def main(args: Array[String]) {
    chiselMainTest(args, () => Module(new KeyCompareSetup(32, 4, 64))) {
      c => new KeyCompareTest(c)
    }
  }
}

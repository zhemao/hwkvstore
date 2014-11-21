package kvstore

import Chisel._
import kvstore.TestUtils._
import kvstore.Constants._

class PearsonHasher(HashBytes: Int, KeyLenSize: Int) extends Module {
  val io = new Bundle {
    val keyData = new ValidIO(UInt(width = 8)).flip
    val keyLen = UInt(INPUT, KeyLenSize)
    val romAddr = Vec.fill(HashBytes) { UInt(OUTPUT, 8) }
    val romData = Vec.fill(HashBytes) { UInt(INPUT, 8) }
    val result = new ValidIO(UInt(width = 8 * HashBytes))
    val restart = Bool(INPUT)
  }

  val h = Vec.fill(HashBytes) { Reg(UInt(width = 8)) }
  val index = Reg(init = UInt(0, KeyLenSize))

  val keyByte = io.keyData.bits
  for (j <- 0 until HashBytes) {
    io.romAddr(j) := Mux(index === UInt(0),
      keyByte + UInt(j), h(j) ^ keyByte)
  }

  when (io.restart) {
    for (i <- 0 until HashBytes)
      h(i) := UInt(0)
    index := UInt(0)
  } .elsewhen (index != io.keyLen && io.keyData.valid) {
    h := io.romData
    index := index + UInt(1)
  }

  io.result.bits := Cat(h.toSeq.reverse)
  io.result.valid := index === io.keyLen
}

class PearsonHasherSetup(val HashBytes: Int, WordSize: Int, MemSize: Int)
    extends Module {
  val KeyAddrSize = log2Up(MemSize)
  val WordBytes = WordSize / 8
  val KeyByteAddrSize = log2Up(WordBytes) + KeyAddrSize
  val io = new Bundle {
    val keyAddr = UInt(INPUT, KeyAddrSize)
    val keyData = UInt(INPUT, WordSize)
    val keyWrite = Bool(INPUT)
    val keyLen = UInt(INPUT, KeyByteAddrSize)
    val result = UInt(OUTPUT, 8 * HashBytes)
    val start = Bool(INPUT)
    val finished = Bool(OUTPUT)
  }

  val romValues = pearsonRomValues1

  val rom = Vec(romValues.map { x => UInt(x, 8) })
  val keyMem = Mem(UInt(width = WordSize), MemSize, true)
  val index = Reg(init = UInt(0, KeyAddrSize))

  when (io.keyWrite) {
    keyMem(io.keyAddr) := io.keyData
  }

  val s_wait :: s_start :: s_hash :: Nil = Enum(UInt(), 3)
  val state = Reg(init = s_wait)

  val hasher = Module(new PearsonHasher(HashBytes, KeyAddrSize))
  hasher.io.keyLen := io.keyLen
  io.result := hasher.io.result.bits
  io.finished := hasher.io.result.valid
  hasher.io.restart := (state === s_start)
  hasher.io.keyData.bits := keyMem(index)
  hasher.io.keyData.valid := (state === s_hash)
  for (i <- 0 until HashBytes)
    hasher.io.romData(i) := rom(hasher.io.romAddr(i))

  switch (state) {
    is (s_wait) {
      when (io.start) {
        index := UInt(0)
        state := s_start
      }
    }
    is (s_start) {
      state := s_hash
    }
    is (s_hash) {
      when (index === io.keyLen) {
        state := s_wait
      } .otherwise {
        index := index + UInt(1)
      }
    }
  }
}

class PearsonHasherTest(c: PearsonHasherSetup) extends Tester(c) {
  val key = "asdfklj;kadgjaskn23kgnas"
  val keyWords = messToWords(key, c.WordBytes)

  val hash = computeHash(c.romValues, key, c.HashBytes)
  printf("Expect %s -> %x\n", key, hash)

  poke(c.io.keyWrite, 1)
  for (i <- 0 until keyWords.length) {
    poke(c.io.keyAddr, i)
    poke(c.io.keyData, keyWords(i))
    step(1)
  }
  poke(c.io.keyWrite, 0)
  poke(c.io.keyLen, key.length)

  poke(c.io.start, 1)
  step(1)
  poke(c.io.start, 0)
  step(key.length + 2)
  expect(c.io.finished, 1)
  expect(c.io.result, hash)
}

object PearsonHasherMain {
  def main(args: Array[String]) {
    chiselMainTest(args, () => Module(new PearsonHasherSetup(2, 8, 256))) {
      c => new PearsonHasherTest(c)
    }
  }
}

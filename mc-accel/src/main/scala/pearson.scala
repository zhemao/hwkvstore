package McAccel

import Chisel._
import McAccel.TestUtils._

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

  val romValues = Array(
    98,  6, 85,150, 36, 23,112,164,135,207,169,  5, 26, 64,165,219,
    61, 20, 68, 89,130, 63, 52,102, 24,229,132,245, 80,216,195,115,
    90,168,156,203,177,120,  2,190,188,  7,100,185,174,243,162, 10,
    237, 18,253,225,  8,208,172,244,255,126,101, 79,145,235,228,121,
    123,251, 67,250,161,  0,107, 97,241,111,181, 82,249, 33, 69, 55,
    59,153, 29,  9,213,167, 84, 93, 30, 46, 94, 75,151,114, 73,222,
    197, 96,210, 45, 16,227,248,202, 51,152,252,125, 81,206,215,186,
    39,158,178,187,131,136,  1, 49, 50, 17,141, 91, 47,129, 60, 99,
    154, 35, 86,171,105, 34, 38,200,147, 58, 77,118,173,246, 76,254,
    133,232,196,144,198,124, 53,  4,108, 74,223,234,134,230,157,139,
    189,205,199,128,176, 19,211,236,127,192,231, 70,233, 88,146, 44,
    183,201, 22, 83, 13,214,116,109,159, 32, 95,226,140,220, 57, 12,
    221, 31,209,182,143, 92,149,184,148, 62,113, 65, 37, 27,106,166,
    3, 14,204, 72, 21, 41, 56, 66, 28,193, 40,217, 25, 54,179,117,
    238, 87,240,155,180,170,242,212,191,163, 78,218,137,194,175,110,
    43,119,224, 71,122,142, 42,160,104, 48,247,103, 15, 11,138,239
  )

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
  def computeHash(table: Array[Int], key: String): BigInt = {
    var result = BigInt(0)
    for (j <- 0 until c.HashBytes) {
      var h = table((key(0) + j) % 256)
      for (i <- 1 until key.length)
        h = table(h ^ key(i))
      result |= (h << (8 * j))
    }
    result
  }

  val key = "asdfklj;kadgjaskn23kgnas"
  val keyWords = messToWords(key, c.WordBytes)

  val hash = computeHash(c.romValues, key)
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

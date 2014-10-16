package McAccel

import Chisel._

class PearsonHasher(HashBytes: Int, MessAddrSize: Int) extends Module {
  val io = new Bundle {
    val messAddr = UInt(OUTPUT, MessAddrSize)
    val messData = UInt(INPUT, 8)
    val messLen = UInt(INPUT, MessAddrSize)
    val romAddr = Vec.fill(HashBytes) { UInt(OUTPUT, 8) }
    val romData = Vec.fill(HashBytes) { UInt(INPUT, 8) }
    val result = UInt(OUTPUT, 8 * HashBytes)
    val start = Bool(INPUT)
    val finished = Bool(OUTPUT)
  }

  val h = Vec.fill(HashBytes) { Reg(UInt(width = 8)) }

  val index = Reg(init = UInt(0, MessAddrSize))
  io.messAddr := index

  val s_wait :: s_hash :: Nil = Enum(UInt(), 2)
  val state = Reg(init = s_wait)

  for (j <- 0 until HashBytes) {
    io.romAddr(j) := Mux(index === UInt(0),
      io.messData + UInt(j),
      h(j) ^ io.messData)
  }

  switch (state) {
    is (s_wait) {
      when (io.start) {
        for (i <- 0 until HashBytes) {
          h(i) := UInt(0)
        }
        state := s_hash
      }
    }
    is (s_hash) {
      when (index === io.messLen) {
        state := s_wait
      } .otherwise {
        h := io.romData
        index := index + UInt(1)
      }
    }
  }

  io.result := Cat(h.toSeq.reverse)
  io.finished := (state === s_wait)
}

class PearsonHasherSetup(val HashBytes: Int, MemSize: Int) extends Module {
  val MessAddrSize = log2Up(MemSize)
  val io = new Bundle {
    val messAddr = UInt(INPUT, MessAddrSize)
    val messData = UInt(INPUT, 8)
    val messWrite = Bool(INPUT)
    val messLen = UInt(INPUT, MessAddrSize)
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

  val messMem = Mem(UInt(width = 8), MemSize, true)

  when (io.messWrite) {
    messMem(io.messAddr) := io.messData
  }

  val hasher = Module(new PearsonHasher(HashBytes, MessAddrSize))
  hasher.io.messLen  <> io.messLen
  hasher.io.result   <> io.result
  hasher.io.start    <> io.start
  hasher.io.finished <> io.finished

  hasher.io.messData := messMem(hasher.io.messAddr)
  for (i <- 0 until HashBytes)
    hasher.io.romData(i) := rom(hasher.io.romAddr(i))
}

class PearsonHasherTest(c: PearsonHasherSetup) extends Tester(c) {
  var hexpand = BigInt(0)
  val mess = "asdfklj;kadgjaskn23kgnas"

  def computeHash(table: Array[Int], mess: String): BigInt = {
    var result = BigInt(0)
    for (j <- 0 until c.HashBytes) {
      var h = table((mess(0) + j) % 256)
      for (i <- 1 until mess.length)
        h = table(h ^ mess(i))
      result |= (h << (8 * j))
    }
    result
  }

  val hash = computeHash(c.romValues, mess)
  printf("Expect %s -> %x\n", mess, hash)

  poke(c.io.messWrite, 1)
  for (i <- 0 until mess.length) {
    poke(c.io.messAddr, i)
    poke(c.io.messData, mess(i))
    step(1)
  }
  poke(c.io.messWrite, 0)
  poke(c.io.messLen, mess.length)

  poke(c.io.start, 1)
  step(1)
  poke(c.io.start, 0)
  step(mess.length + 2)
  expect(c.io.finished, 1)
  expect(c.io.result, hash)
}

object PearsonHasherMain {
  def main(args: Array[String]) {
    chiselMainTest(args, () => Module(new PearsonHasherSetup(2, 256))) {
      c => new PearsonHasherTest(c)
    }
  }
}

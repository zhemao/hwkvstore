package McAccel

import Chisel._
import McAccel.TestUtils._
import McAccel.Constants._

class HasherWriter(HashSize: Int, WordSize: Int, KeySize: Int, TagSize: Int)
    extends Module {
  val WordBytes = WordSize / 8
  val ByteShift = log2Up(WordSize) - 3
  val KeyLenSize = log2Up(KeySize)
  val KeyAddrSize = KeyLenSize - ByteShift
  val HashBytes = (HashSize - 1) / 8 + 1

  val io = new Bundle {
    val keyWriteAddr = UInt(OUTPUT, KeyAddrSize)
    val keyWriteData = UInt(OUTPUT, WordSize)
    val keyWrite = Bool(OUTPUT)

    val keyData = Decoupled(UInt(width = 8)).flip
    val keyInfo = Decoupled(new MessageInfo(KeyLenSize, TagSize)).flip
    val hashOut = Decoupled(new HashInfo(HashSize, KeyLenSize, TagSize))

    val lock = Bool(INPUT)
    val halted = Bool(OUTPUT)
  }

  val keyLen = Reg(UInt(width = KeyLenSize))
  val keyTag = Reg(UInt(width = TagSize))

  val s_wait :: s_read :: s_finish :: Nil = Enum(UInt(), 3)
  val state = Reg(init = s_wait)
  val index = Reg(init = UInt(0, KeyLenSize))
  val restart = Reg(init = Bool(false))
  val hashInputValid = io.keyData.valid && state === s_read
  val byteOff = if (ByteShift == 0) {
    UInt(0, 1)
  } else {
    index(ByteShift - 1, 0)
  }
  val inputShift = Cat(byteOff, UInt(0, 3))
  val keyWrite = Reg(init = Bool(false))
  val keyWriteData = Reg(init = UInt(0, WordSize))
  val keyWriteAddr = Reg(init = UInt(0, KeyAddrSize))
  io.keyWrite := keyWrite
  io.keyWriteData := keyWriteData
  io.keyWriteAddr := keyWriteAddr

  switch (state) {
    is (s_wait) {
      restart := Bool(false)
      when (io.keyInfo.valid && !io.lock) {
        keyLen := io.keyInfo.bits.len
        keyTag := io.keyInfo.bits.tag
        index := UInt(0)
        state := s_read
      }
    }
    is (s_read) {
      keyWrite := Bool(false)
      when (io.keyData.valid) {
        when (byteOff === UInt(0)) {
          keyWriteData := io.keyData.bits
        } .otherwise {
          keyWriteData := keyWriteData | io.keyData.bits << inputShift
        }
        when (index === keyLen - UInt(1)) {
          keyWrite := Bool(true)
          keyWriteAddr := index(KeyLenSize - 1, ByteShift)
          state := s_finish
        } .elsewhen (byteOff === UInt(WordBytes - 1)) {
          keyWrite := Bool(true)
          keyWriteAddr := index(KeyLenSize - 1, ByteShift)
        }
        index := index + UInt(1)
      }
    }
    is (s_finish) {
      keyWrite := Bool(false)
      when (io.hashOut.ready) {
        restart := Bool(true)
        state := s_wait
      }
    }
  }

  val rom1 = Vec(pearsonRomValues1.map { x => UInt(x, 8) })
  val rom2 = Vec(pearsonRomValues2.map { x => UInt(x, 8) })
  val roms = Array(rom1, rom2)
  val results = Array(io.hashOut.bits.hash1, io.hashOut.bits.hash2)

  for (hashind <- 0 until 2) {
    val rom = roms(hashind)
    val hasher = Module(new PearsonHasher(HashBytes, KeyAddrSize))
    hasher.io.keyLen := keyLen
    hasher.io.keyData.valid := hashInputValid
    hasher.io.keyData.bits := io.keyData.bits
    for (i <- 0 until HashBytes)
      hasher.io.romData(i) := rom(hasher.io.romAddr(i))
    results(hashind) := hasher.io.result.bits
    hasher.io.restart := restart
  }

  io.hashOut.valid := (state === s_finish)
  io.hashOut.bits.len := keyLen
  io.hashOut.bits.tag := keyTag
  io.keyInfo.ready := (state === s_wait && !io.lock)
  io.keyData.ready := (state === s_read)
  io.halted := (state === s_wait && io.lock)
}

class HasherWriterSetup(val HashSize: Int, val WordSize: Int,
    KeySize: Int, TagSize: Int) extends Module {
  val NumWords = KeySize / WordSize
  val ByteShift = log2Up(WordSize) - 3
  val KeyLenSize = log2Up(KeySize)
  val KeyAddrSize = KeyLenSize - ByteShift

  val io = new Bundle {
    val keyData = Decoupled(UInt(width = 8)).flip
    val keyInfo = Decoupled(new MessageInfo(KeyLenSize, TagSize)).flip
    val hashOut = Decoupled(new HashInfo(HashSize, KeyLenSize, TagSize))

    val keyReadAddr = UInt(INPUT, KeyAddrSize)
    val keyReadData = UInt(OUTPUT, WordSize)
  }

  val mem = Mem(UInt(width = WordSize), NumWords)

  val hw = Module(new HasherWriter(HashSize, WordSize, KeySize, TagSize))
  hw.io.keyData <> io.keyData
  hw.io.keyInfo <> io.keyInfo
  hw.io.hashOut <> io.hashOut

  when (hw.io.keyWrite) {
    mem(hw.io.keyWriteAddr) := hw.io.keyWriteData
  }

  io.keyReadData := mem(io.keyReadAddr)
}

class HasherWriterTest(c: HasherWriterSetup) extends Tester(c) {
  val WordBytes = c.WordSize / 8
  val HashBytes = (c.HashSize - 1) / 8 + 1
  val key = "asdfklj;kadgjaskn23kgnas"
  val keyWords = messToWords(key, WordBytes)

  expect(c.io.keyInfo.ready, 1)
  poke(c.io.keyInfo.valid, 1)
  poke(c.io.keyInfo.bits.len, key.length)
  poke(c.io.keyInfo.bits.tag, 2)
  step(1)
  poke(c.io.keyInfo.valid, 0)
  step(1)

  poke(c.io.keyData.valid, 1)
  poke(c.io.hashOut.ready, 1)

  for (byte <- key) {
    expect(c.io.keyData.ready, 1)
    poke(c.io.keyData.bits, byte)
    step(1)
  }

  val hash1 = computeHash(pearsonRomValues1, key, HashBytes) % (1 << c.HashSize)
  val hash2 = computeHash(pearsonRomValues2, key, HashBytes) % (1 << c.HashSize)

  expect(c.io.hashOut.valid, 1)
  expect(c.io.hashOut.bits.hash1, hash1)
  expect(c.io.hashOut.bits.hash2, hash2)
  expect(c.io.hashOut.bits.len, key.length)
  expect(c.io.hashOut.bits.tag, 2)
  expect(c.io.keyData.ready, 0)

  poke(c.io.hashOut.ready, 0)
  poke(c.io.keyData.valid, 0)

  for (i <- 0 until keyWords.length) {
    poke(c.io.keyReadAddr, i)
    step(1)
    expect(c.io.keyReadData, keyWords(i))
  }
}

object HasherWriterMain {
  def main(args: Array[String]) {
    chiselMainTest(args, () => Module(new HasherWriterSetup(10, 8, 256, 4))) {
      c => new HasherWriterTest(c)
    }
  }
}

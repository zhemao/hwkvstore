package McAccel

import Chisel._
import McAccel.TestUtils._
import McAccel.Constants._

class LookupPipeline(val WordSize: Int, val KeySize: Int, val NumKeys: Int)
    extends Module {
  val WordBytes = WordSize / 8
  val CurKeyWords = KeySize / WordBytes
  val AllKeyWords = CurKeyWords * NumKeys
  val HashSize = log2Up(NumKeys)

  val io = new Bundle {
    val keyIncoming = new DecoupledIO(UInt(width = 8)).flip
    val hashOut = new DecoupledIO(new HashSelection(HashSize))
    val allKeyAddr = UInt(INPUT, log2Up(AllKeyWords))
    val allKeyData = UInt(INPUT, WordSize)
    val allKeyWrite = Bool(INPUT)
  }

  val hasherwriter = Module(new HasherWriter(HashSize, WordSize, KeySize))
  hasherwriter.io.keyIncoming <> io.keyIncoming

  val keycompare = Module(new KeyCompare(HashSize, WordSize, KeySize))
  keycompare.io.hashIn <> hasherwriter.io.hashOut
  keycompare.io.hashOut <> io.hashOut

  val swapped = Reg(init = Bool(false))
  val kcCurAddr = Cat(!swapped, keycompare.io.curKeyAddr)
  val hwWriteAddr = Cat(swapped, hasherwriter.io.keyWriteAddr)

  val allReadAddr = Reg(next = keycompare.io.allKeyAddr)
  val curReadAddr = Reg(next = kcCurAddr)
  val curWriteAddr = Reg(next = hwWriteAddr)
  val curWriteData = Reg(next = hasherwriter.io.keyWriteData)
  val curWrite = Reg(next = hasherwriter.io.keyWrite)

  val curKeyMem = Mem(UInt(width = WordSize), 2 * CurKeyWords)
  val allKeyMem = Mem(UInt(width = WordSize), AllKeyWords)

  when (curWrite) {
    curKeyMem(curWriteAddr) := curWriteData
  }

  when (io.allKeyWrite) {
    allKeyMem(io.allKeyAddr) := io.allKeyData
  }

  keycompare.io.curKeyData := curKeyMem(curReadAddr)
  keycompare.io.allKeyData := allKeyMem(allReadAddr)

  when (hasherwriter.io.hashOut.valid && keycompare.io.hashIn.ready) {
    swapped := !swapped
  }
}

class LookupPipelineTest(c: LookupPipeline) extends Tester(c) {
  val WordBytes = c.WordSize / 8
  val HashBytes = (c.HashSize - 1) / 8 + 1
  val KeyWords = c.KeySize / WordBytes

  def writeKeyData(start: BigInt, key: String) {
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
    step(1)
  }

  def streamCurKey(key: String) {
    while (peek(c.io.keyIncoming.ready) == 0) {
      step(1)
    }
    poke(c.io.keyIncoming.valid, 1)
    poke(c.io.keyIncoming.bits, key.length)
    step(1)
    for (ch <- key) {
      poke(c.io.keyIncoming.bits, ch)
      step(1)
    }
    poke(c.io.keyIncoming.valid, 0)
    step(1)
  }

  val key1 = "abcdefghijklmnopqrstuvwxyz"
  val key2 = "abcdefghijklmnopqrstuvwxzy"
  val key3 = "0123456789"

  val hash1 = computeHash(pearsonRomValues1, key1, HashBytes) % c.NumKeys
  val hash2 = computeHash(pearsonRomValues1, key2, HashBytes) % c.NumKeys
  val hash3 = computeHash(pearsonRomValues2, key3, HashBytes) % c.NumKeys

  printf("hashes: %d %d %d\n", hash1, hash2, hash3)

  writeKeyData(hash1 * KeyWords, key1)
  writeKeyData(hash2 * KeyWords, key2)
  writeKeyData(hash3 * KeyWords, key3)

  // stream in key1
  streamCurKey(key1)
  expect(c.io.hashOut.valid, 0)
  // stream in key 2 and check result of key 1 lookup
  streamCurKey(key2)
  expect(c.io.hashOut.valid, 1)
  expect(c.io.hashOut.bits.found, 1)
  expect(c.io.hashOut.bits.hash, hash1)
  poke(c.io.hashOut.ready, 1)
  step(1)
  poke(c.io.hashOut.ready, 0)
  // stream in key 3 and check result of key 2 lookup
  streamCurKey(key3)
  expect(c.io.hashOut.valid, 1)
  expect(c.io.hashOut.bits.found, 1)
  expect(c.io.hashOut.bits.hash, hash2)
  poke(c.io.hashOut.ready, 1)
  step(1)
  poke(c.io.hashOut.ready, 0)
  step(1)
  // wait for key 3 result to arrive
  expect(c.io.hashOut.valid, 0)
  while (peek(c.io.hashOut.valid) == 0)
    step(1)
  expect(c.io.hashOut.valid, 1)
  expect(c.io.hashOut.bits.found, 1)
  expect(c.io.hashOut.bits.hash, hash3)
}

object LookupPipelineMain {
  def main(args: Array[String]) {
    chiselMainTest(args, () => Module(new LookupPipeline(32, 256, 16))) {
      c => new LookupPipelineTest(c)
    }
  }
}

package McAccel

import Chisel._
import McAccel.TestUtils._
import McAccel.Constants._

class LookupPipeline(
    val WordSize: Int, val KeySize: Int, val NumKeys: Int,
    ValCacheSize: Int, TagSize: Int)
      extends Module {
  val WordBytes = WordSize / 8
  val CurKeyWords = KeySize / WordBytes
  val AllKeyWords = CurKeyWords * NumKeys
  val HashSize = log2Up(NumKeys)
  val KeyLenSize = log2Up(KeySize)
  val ValAddrSize = log2Up(ValCacheSize)

  val io = new Bundle {
    val keyInfo = Decoupled(new MessageInfo(KeyLenSize, TagSize)).flip
    val keyData = Decoupled(UInt(width = 8)).flip

    val resultInfo = Decoupled(new MessageInfo(ValAddrSize, TagSize))
    val resultData = Decoupled(UInt(width = 8))

    val cacheWriteAddr = UInt(INPUT, ValAddrSize)
    val cacheWriteData = UInt(INPUT, 8)
    val cacheWriteEn = Bool(INPUT)

    val addrLenWriteAddr = UInt(INPUT, HashSize)
    val addrLenWriteData = new AddrLenPair(ValAddrSize, INPUT)
    val addrLenWriteEn = Bool(INPUT)

    val allKeyAddr = UInt(INPUT, log2Up(AllKeyWords))
    val allKeyData = UInt(INPUT, WordSize)
    val allKeyWrite = Bool(INPUT)
  }

  val hasherwriter = Module(
    new HasherWriter(HashSize, WordSize, KeySize, TagSize))
  hasherwriter.io.keyData <> io.keyData
  hasherwriter.io.keyInfo <> io.keyInfo

  val keycompare = Module(
    new KeyCompare(HashSize, WordSize, KeySize, TagSize))
  keycompare.io.hashIn <> hasherwriter.io.hashOut

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

  val valcache = Module(new ValueCache(NumKeys, ValCacheSize, TagSize))
  valcache.io.hashIn           <> keycompare.io.hashOut
  valcache.io.resultInfo       <> io.resultInfo
  valcache.io.resultData       <> io.resultData
  valcache.io.cacheWriteAddr   <> io.cacheWriteAddr
  valcache.io.cacheWriteData   <> io.cacheWriteData
  valcache.io.cacheWriteEn     <> io.cacheWriteEn
  valcache.io.addrLenWriteAddr <> io.addrLenWriteAddr
  valcache.io.addrLenWriteData <> io.addrLenWriteData
  valcache.io.addrLenWriteEn   <> io.addrLenWriteEn
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

  def writeValue(hash: BigInt, start: Int, value: String) {
    poke(c.io.addrLenWriteAddr, hash)
    poke(c.io.addrLenWriteData.addr, start)
    poke(c.io.addrLenWriteData.len, value.length)
    poke(c.io.addrLenWriteEn, 1)
    step(1)
    poke(c.io.addrLenWriteEn, 0)

    poke(c.io.cacheWriteEn, 1)
    for (i <- 0 until value.length) {
      poke(c.io.cacheWriteAddr, start + i)
      poke(c.io.cacheWriteData, value(i))
      step(1)
    }
    poke(c.io.cacheWriteEn, 0)
  }

  def streamCurKey(key: String, tag: Int) {
    isTrace = false
    println(s"Waiting for keyInfo ready on ${tag}")
    while (peek(c.io.keyInfo.ready) == 0)
      step(1)
    poke(c.io.keyInfo.valid, 1)
    poke(c.io.keyInfo.bits.len, key.length)
    poke(c.io.keyInfo.bits.tag, tag)
    step(1)
    poke(c.io.keyInfo.valid, 0)
    step(1)
    println(s"Waiting for keyData ready on ${tag}")
    while (peek(c.io.keyData.ready) == 0)
      step(1)
    poke(c.io.keyData.valid, 1)
    for (ch <- key) {
      poke(c.io.keyData.bits, ch)
      step(1)
    }
    poke(c.io.keyData.valid, 0)
    step(1)
    isTrace = true
  }

  def checkResult(value: String, tag: Int) {
    isTrace = false
    println(s"Waiting for ${tag} resultInfo ready")
    while (peek(c.io.resultInfo.valid) == 0)
      step(1)
    isTrace = true

    expect(c.io.resultInfo.bits.len, value.length)
    expect(c.io.resultInfo.bits.tag, tag)

    poke(c.io.resultInfo.ready, 1)
    step(1)
    poke(c.io.resultInfo.ready, 0)
    poke(c.io.resultInfo.ready, 0)
    poke(c.io.resultData.ready, 1)

    for (ch <- value) {
      expect(c.io.resultData.valid, 1)
      expect(c.io.resultData.bits, ch)
      step(1)
    }

    poke(c.io.resultData.ready, 0)
  }

  val key1 = "abcdefghijklmnopqrstuvwxyz"
  val key2 = "abcdefghijklmnopqrstuvwxzy"
  val key3 = "0123456789"
  val key4 = "abcd"

  val value1 = "askdfj;j23jfasdkfjdasdfjkajsdfj"
  val value2 = "aknqqnn34jasdkfjk"
  val value3 = "2934inbvkdswfjkdfj"
  val value4 = ""

  val hash1 = computeHash(pearsonRomValues1, key1, HashBytes) % c.NumKeys
  val hash2 = computeHash(pearsonRomValues1, key2, HashBytes) % c.NumKeys
  val hash3 = computeHash(pearsonRomValues2, key3, HashBytes) % c.NumKeys

  printf("hashes: %d %d %d\n", hash1, hash2, hash3)

  writeKeyData(hash1 * KeyWords, key1)
  writeKeyData(hash2 * KeyWords, key2)
  writeKeyData(hash3 * KeyWords, key3)

  writeValue(hash1, 0, value1)
  writeValue(hash2, value1.length, value2)
  writeValue(hash3, value1.length + value2.length, value3)

  // stream in the first three keys to fill up the pipeline
  streamCurKey(key1, 1)
  streamCurKey(key2, 2)
  streamCurKey(key3, 3)

  // check the first result and stream in the last key
  checkResult(value1, 1)
  streamCurKey(key4, 4)

  // check the last three keys
  checkResult(value2, 2)
  checkResult(value3, 3)
  checkResult(value4, 4)
}

object LookupPipelineMain {
  def main(args: Array[String]) {
    chiselMainTest(args,
      () => Module(new LookupPipeline(32, 256, 16, 256, 4))) {
      c => new LookupPipelineTest(c)
    }
  }
}

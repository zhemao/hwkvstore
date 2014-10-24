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
    val lock = Bool(INPUT)
    val halted = Bool(OUTPUT)
    val writemode = Bool(INPUT)

    val readKeyInfo = Decoupled(new MessageInfo(KeyLenSize, TagSize)).flip
    val readKeyData = Decoupled(UInt(width = 8)).flip

    val writeKeyInfo = Decoupled(new MessageInfo(KeyLenSize, TagSize)).flip
    val writeKeyData = Decoupled(UInt(width = 8)).flip

    val hashSel = Decoupled(new HashSelection(HashSize, TagSize))

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

    val keyLenAddr = UInt(INPUT, HashSize)
    val keyLenData = UInt(INPUT, KeyLenSize)
    val keyLenWrite = Bool(INPUT)
  }

  val hasherwriter = Module(
    new HasherWriter(HashSize, WordSize, KeySize, TagSize))
  hasherwriter.io.lock    <> io.lock
  hasherwriter.io.keyData.bits := Mux(io.writemode,
    io.writeKeyData.bits, io.readKeyData.bits)
  hasherwriter.io.keyData.valid := Mux(io.writemode,
    io.writeKeyData.valid, io.readKeyData.valid)
  hasherwriter.io.keyInfo.bits := Mux(io.writemode,
    io.writeKeyInfo.bits, io.readKeyInfo.bits)
  hasherwriter.io.keyInfo.valid := Mux(io.writemode,
    io.writeKeyInfo.valid, io.readKeyInfo.valid)

  io.readKeyInfo.ready := hasherwriter.io.keyInfo.ready && !io.writemode
  io.writeKeyInfo.ready := hasherwriter.io.keyInfo.ready && io.writemode
  io.readKeyData.ready := hasherwriter.io.keyData.ready && !io.writemode
  io.writeKeyData.ready := hasherwriter.io.keyData.ready && io.writemode

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
  val lenMem = Mem(UInt(width = KeyLenSize), NumKeys)

  when (curWrite) {
    curKeyMem(curWriteAddr) := curWriteData
  }

  when (io.allKeyWrite) {
    allKeyMem(io.allKeyAddr) := io.allKeyData
  }

  when (io.keyLenWrite) {
    lenMem(io.keyLenAddr) := io.keyLenData
  }

  keycompare.io.curKeyData := curKeyMem(curReadAddr)
  keycompare.io.allKeyData := allKeyMem(allReadAddr)
  keycompare.io.lenData := lenMem(keycompare.io.lenAddr)

  when (hasherwriter.io.hashOut.valid && keycompare.io.hashIn.ready) {
    swapped := !swapped
  }

  val valcache = Module(new ValueCache(NumKeys, ValCacheSize, TagSize))
  valcache.io.resultInfo       <> io.resultInfo
  valcache.io.resultData       <> io.resultData
  valcache.io.cacheWriteAddr   <> io.cacheWriteAddr
  valcache.io.cacheWriteData   <> io.cacheWriteData
  valcache.io.cacheWriteEn     <> io.cacheWriteEn
  valcache.io.addrLenWriteAddr <> io.addrLenWriteAddr
  valcache.io.addrLenWriteData <> io.addrLenWriteData
  valcache.io.addrLenWriteEn   <> io.addrLenWriteEn

  keycompare.io.hashOut.ready := Mux(io.writemode,
    io.hashSel.ready, valcache.io.hashIn.ready)
  valcache.io.hashIn.bits := keycompare.io.hashOut.bits
  valcache.io.hashIn.valid := keycompare.io.hashOut.valid && !io.writemode
  io.hashSel.bits := keycompare.io.hashOut.bits
  io.hashSel.valid := keycompare.io.hashOut.valid && io.writemode

  io.halted := hasherwriter.io.halted &&
    keycompare.io.hashIn.ready &&
    valcache.io.hashIn.ready
}

class LookupPipelineTest(c: LookupPipeline) extends Tester(c) {
  val WordBytes = c.WordSize / 8
  val HashBytes = (c.HashSize - 1) / 8 + 1

  def writeKeyData(hash: BigInt, key: String) {
    val keyWords = messToWords(key, WordBytes)
    val start = hash * c.KeySize / WordBytes
    poke(c.io.keyLenWrite, 1)
    poke(c.io.keyLenAddr, hash)
    poke(c.io.keyLenData, key.length)
    step(1)
    poke(c.io.keyLenWrite, 0)

    poke(c.io.allKeyWrite, 1)
    for (i <- 0 until keyWords.length) {
      poke(c.io.allKeyAddr, start + i)
      poke(c.io.allKeyData, keyWords(i))
      step(1)
    }
    poke(c.io.allKeyWrite, 0)
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
    println(s"Waiting for readKeyInfo ready on ${tag}")
    while (peek(c.io.readKeyInfo.ready) == 0)
      step(1)
    poke(c.io.readKeyInfo.valid, 1)
    poke(c.io.readKeyInfo.bits.len, key.length)
    poke(c.io.readKeyInfo.bits.tag, tag)
    step(1)
    poke(c.io.readKeyInfo.valid, 0)
    step(1)
    println(s"Waiting for readKeyData ready on ${tag}")
    while (peek(c.io.readKeyData.ready) == 0)
      step(1)
    poke(c.io.readKeyData.valid, 1)
    for (ch <- key) {
      poke(c.io.readKeyData.bits, ch)
      step(1)
    }
    poke(c.io.readKeyData.valid, 0)
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

  writeKeyData(hash1, key1)
  writeKeyData(hash2, key2)
  writeKeyData(hash3, key3)

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
  poke(c.io.lock, 1)

  // check the last three keys
  checkResult(value2, 2)
  checkResult(value3, 3)
  checkResult(value4, 4)
  expect(c.io.halted, 1)
}

object LookupPipelineMain {
  def main(args: Array[String]) {
    chiselMainTest(args,
      () => Module(new LookupPipeline(32, 256, 16, 256, 4))) {
      c => new LookupPipelineTest(c)
    }
  }
}

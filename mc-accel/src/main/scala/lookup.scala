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
    val findAvailable = Bool(INPUT)

    val readKeyInfo = Decoupled(new MessageInfo(KeyLenSize, TagSize)).flip
    val readKeyData = Decoupled(UInt(width = 8)).flip

    val writeKeyInfo = Decoupled(new MessageInfo(KeyLenSize, TagSize)).flip
    val writeKeyData = Decoupled(UInt(width = 8)).flip

    val hashSel = Decoupled(new HashSelection(HashSize, TagSize))
    val copyReq = Decoupled(new CopyRequest(HashSize, KeyLenSize)).flip

    val resultInfo = Decoupled(new MessageInfo(ValAddrSize, TagSize))
    val resultData = Decoupled(UInt(width = 8))

    val cacheWriteAddr = UInt(INPUT, ValAddrSize)
    val cacheWriteData = UInt(INPUT, 8)
    val cacheWriteEn = Bool(INPUT)

    val addrLenAddr = UInt(INPUT, HashSize)
    val addrLenWriteData = new AddrLenPair(ValAddrSize, INPUT)
    val addrLenWriteEn = Vec.fill(2) { Bool(INPUT) }
    val addrLenReadData = new AddrLenPair(ValAddrSize, OUTPUT)

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
  keycompare.io.findAvailable := io.findAvailable

  val keycopy = Module(new KeyCopier(HashSize, WordSize, KeySize))
  keycopy.io.copyReq <> io.copyReq

  val curKeyMem = Mem(UInt(width = WordSize), 2 * CurKeyWords, true)
  val allKeyMem = Mem(UInt(width = WordSize), AllKeyWords, true)
  val lenMem = Mem(UInt(width = KeyLenSize), NumKeys, true)

  val swapped = Reg(init = Bool(false))
  val curReadAddrRaw = Mux(keycopy.io.selCopy,
    keycopy.io.curKeyAddr, keycompare.io.curKeyAddr)
  val curReadAddrExt = Cat(!swapped, curReadAddrRaw)
  val hwWriteAddr = Cat(swapped, hasherwriter.io.keyWriteAddr)

  val allReadAddr = Reg(next = keycompare.io.allKeyAddr)
  val curReadAddr = Reg(next = curReadAddrExt)
  val curReadData = curKeyMem(curReadAddr)
  val curWriteAddr = Reg(next = hwWriteAddr)
  val curWriteData = Reg(next = hasherwriter.io.keyWriteData)
  val curWrite = Reg(next = hasherwriter.io.keyWrite)


  when (curWrite) {
    curKeyMem(curWriteAddr) := curWriteData
  }

  val allWriteAddr = Reg(next = keycopy.io.allKeyAddr)
  val allWriteEn = Reg(next = keycopy.io.allKeyWrite)
  val allWriteData = Reg(next = keycopy.io.allKeyData)

  when (allWriteEn) {
    allKeyMem(allWriteAddr) := allWriteData
  }

  when (io.keyLenWrite) {
    lenMem(io.keyLenAddr) := io.keyLenData
  }

  keycompare.io.curKeyData := curReadData
  keycompare.io.allKeyData := allKeyMem(allReadAddr)
  keycompare.io.lenData := lenMem(keycompare.io.lenAddr)

  keycopy.io.curKeyData := curReadData

  when (hasherwriter.io.hashOut.valid && keycompare.io.hashIn.ready) {
    swapped := !swapped
  }

  val valcache = Module(new ValueCache(NumKeys, ValCacheSize, TagSize))
  valcache.io.resultInfo       <> io.resultInfo
  valcache.io.resultData       <> io.resultData
  valcache.io.cacheWriteAddr   <> io.cacheWriteAddr
  valcache.io.cacheWriteData   <> io.cacheWriteData
  valcache.io.cacheWriteEn     <> io.cacheWriteEn
  valcache.io.addrLenAddr      <> io.addrLenAddr
  valcache.io.addrLenWriteData <> io.addrLenWriteData
  valcache.io.addrLenWriteEn   <> io.addrLenWriteEn
  valcache.io.addrLenReadData  <> io.addrLenReadData

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

  def writeValue(hash: BigInt, start: Int, value: String) {
    isTrace = false
    poke(c.io.addrLenAddr, hash)
    poke(c.io.addrLenWriteData.addr, start)
    poke(c.io.addrLenWriteData.len, value.length)
    poke(c.io.addrLenWriteEn, Array[BigInt](1, 1))
    step(1)
    poke(c.io.addrLenWriteEn, Array[BigInt](0, 0))

    poke(c.io.cacheWriteEn, 1)
    for (i <- 0 until value.length) {
      poke(c.io.cacheWriteAddr, start + i)
      poke(c.io.cacheWriteData, value(i))
      step(1)
    }
    poke(c.io.cacheWriteEn, 0)
    isTrace = true
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

  def writeKey(key: String, tag: Int) = {
    isTrace = false
    println(s"Waiting for writeKeyInfo ready on ${tag}")
    while (peek(c.io.writeKeyInfo.ready) == 0)
      step(1)
    poke(c.io.writeKeyInfo.valid, 1)
    poke(c.io.writeKeyInfo.bits.len, key.length)
    poke(c.io.writeKeyInfo.bits.tag, tag)
    step(1)
    poke(c.io.writeKeyInfo.valid, 0)
    step(1)
    println(s"Waiting for writeKeyData ready on ${tag}")
    while (peek(c.io.writeKeyData.ready) == 0)
      step(1)
    poke(c.io.writeKeyData.valid, 1)
    for (ch <- key) {
      poke(c.io.writeKeyData.bits, ch)
      step(1)
    }
    poke(c.io.writeKeyData.valid, 0)
    step(1)

    poke(c.io.hashSel.ready, 1)
    println(s"Waiting for hashSel valid on ${tag}")
    while (peek(c.io.hashSel.valid) == 0) {
      step(1)
    }
    step(1)
    poke(c.io.hashSel.ready, 0)
    expect(c.io.hashSel.bits.found, 1)
    expect(c.io.hashSel.bits.tag, tag)
    val hash = peek(c.io.hashSel.bits.hash)
    expect(c.io.copyReq.ready, 1)
    poke(c.io.copyReq.valid, 1)
    poke(c.io.copyReq.bits.hash, hash)
    poke(c.io.copyReq.bits.len, key.length)
    step(1)
    poke(c.io.copyReq.valid, 0)

    println(s"Waiting for copyReq finish on ${tag}")
    while (peek(c.io.copyReq.ready) == 0) {
      step(1)
    }

    poke(c.io.keyLenAddr, hash)
    poke(c.io.keyLenData, key.length)
    poke(c.io.keyLenWrite, 1)
    step(1)
    poke(c.io.keyLenWrite, 0)

    isTrace = true

    hash
  }

  val key1 = "abcdefghijklmnopqrstuvwxyz"
  val key2 = "abcdefghijklmnopqrstuvwxzy"
  val key3 = "0123456789"
  val key4 = "abcd"

  val value1 = "askdfj;j23jfasdkfjdasdfjkajsdfj"
  val value2 = "aknqqnn34jasdkfjk"
  val value3 = "2934inbvkdswfjkdfj"
  val value4 = ""

  poke(c.io.writemode, 1)
  poke(c.io.findAvailable, 1)

  val hash1 = writeKey(key1, 1)
  val hash2 = writeKey(key2, 2)
  val hash3 = writeKey(key3, 3)

  printf("hashes: %d %d %d\n", hash1, hash2, hash3)

  writeValue(hash1, 0, value1)
  writeValue(hash2, value1.length, value2)
  writeValue(hash3, value1.length + value2.length, value3)

  poke(c.io.writemode, 0)
  poke(c.io.findAvailable, 0)

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
      () => Module(new LookupPipeline(32, 256, 32, 1024, 4))) {
      c => new LookupPipelineTest(c)
    }
  }
}

package kvstore

import Chisel._
import Chisel.AdvTester._
import kvstore.TestUtils._
import kvstore.Constants._

class KeyValueStore extends Module {
  val WordSize = params[Int]("wordsize")
  val KeySize = params[Int]("keysize")
  val NumKeys = params[Int]("numkeys")
  val ValCacheSize = params[Int]("valcachesize")
  val TagSize = params[Int]("tagsize")

  val WordBytes = WordSize / 8
  val CurKeyWords = KeySize / WordBytes
  val AllKeyWords = CurKeyWords * NumKeys
  val HashSize = log2Up(NumKeys)
  val KeyLenSize = log2Up(KeySize)
  val ValAddrSize = log2Up(ValCacheSize)
  val KeyAddrSize = log2Up(AllKeyWords)

  val io = new Bundle {
    val keyInfo = Decoupled(new MessageInfo(KeyLenSize, TagSize)).flip
    val keyData = Decoupled(UInt(width = 8)).flip
    val resultInfo = Decoupled(new MessageInfo(ValAddrSize, TagSize))
    val resultData = Decoupled(UInt(width = 8))
    val writeready = Bool(OUTPUT)
    val readready  = Bool(OUTPUT)
    val rocc = new RoCCInterface
  }

  val lookup = Module(new LookupPipeline(
    WordSize, KeySize, NumKeys, ValCacheSize, TagSize))
  lookup.io.readKeyInfo <> Queue(io.keyInfo, 2)
  lookup.io.readKeyData <> Queue(io.keyData, 2)
  io.resultInfo <> Queue(lookup.io.resultInfo, 2)
  io.resultData <> Queue(lookup.io.resultData, 2)

  val ctrl = Module(new CtrlModule(
    WordSize, ValAddrSize, KeyLenSize, HashSize, TagSize))
  ctrl.io.rocc    <> io.rocc
  ctrl.io.keyInfo <> lookup.io.writeKeyInfo
  ctrl.io.keyData <> lookup.io.writeKeyData
  ctrl.io.hashSel <> lookup.io.hashSel
  ctrl.io.copyReq <> lookup.io.copyReq
  ctrl.io.cacheWriteAddr <> lookup.io.cacheWriteAddr
  ctrl.io.cacheWriteData <> lookup.io.cacheWriteData
  ctrl.io.cacheWriteEn   <> lookup.io.cacheWriteEn
  ctrl.io.keyLenAddr  <> lookup.io.keyLenAddr
  ctrl.io.keyLenData  <> lookup.io.keyLenData
  ctrl.io.keyLenWrite <> lookup.io.keyLenWrite
  ctrl.io.addrLenAddr      <> lookup.io.addrLenAddr
  ctrl.io.addrLenWriteData <> lookup.io.addrLenWriteData
  ctrl.io.addrLenWriteEn   <> lookup.io.addrLenWriteEn
  ctrl.io.addrLenReadData  <> lookup.io.addrLenReadData
  ctrl.io.addrLenReadEn    <> lookup.io.addrLenReadEn
  ctrl.io.lock      <> lookup.io.lock
  ctrl.io.halted    <> lookup.io.halted
  ctrl.io.writemode <> lookup.io.writemode
  ctrl.io.findAvailable <> lookup.io.findAvailable
  ctrl.io.resetCounts   <> lookup.io.resetCounts

  io.writeready := ctrl.io.writemode  && !lookup.io.halted
  io.readready  := !ctrl.io.writemode && !lookup.io.halted
}

class KeyValueStoreTest(c: KeyValueStore) extends AdvTester(c) {
  private def isBusy = peek(c.io.rocc.cmd.ready) == 0

  val Cmd_IHandler = new DecoupledSource(c.io.rocc.cmd,
    (sckt: RoCCCommand, in: TestCmd) => in.inject(this, sckt))
  val Resp_OHandler = new DecoupledSink(c.io.rocc.resp,
    (sckt: RoCCResponse) => TestResp.extract(this, sckt))
  val MemReq_OHandler = new DecoupledSink(c.io.rocc.mem.req,
    (sckt: HellaCacheReq) => TestMemReq.extract(this, sckt))
  val MemResp_IHandler = new ValidSource(c.io.rocc.mem.resp, 
    (sckt: HellaCacheResp, in: TestMemResp) => in.inject(this, sckt))


  val memory = new RandomMemory(64, 64*64*2, 
    MemReq_OHandler.outputs, MemResp_IHandler.inputs)
  val key = "asdkfjqwekjfasdkfj"
  val value = "qpwoinfasw9qr09q23jasknvasdfjksjqefksajfk"
  val keyWords = messToWords(key, 8, 4)
  val valueWords = messToWords(value, 8, 2)
  var accelAddr = 0

  memory.store_data(0, keyWords)
  memory.store_data(256, valueWords)

  expect(c.io.writeready, 1)

  println("Testing delete key")
  val delKey = TestInst(1, 0, 1, 2, true, true, true)
  Cmd_IHandler.inputs.enqueue(TestCmd(delKey, 4, key.length))

  until (Cmd_IHandler.isIdle && !isBusy, 450) {
    memory.process()
  }

  assert( Resp_OHandler.outputs.size == 1,
    s"Expected 1 output in queue, instead there are ${Resp_OHandler.outputs.size}" )
  var resp = Resp_OHandler.outputs.dequeue()
  assert( resp.data == HashNotFound,
    s"Expected HashNotFound, instead got ${resp.data}" )

  def setKeyValue(key: String, value: String) {
    val memData = messToWords(key + value, 8)
    memory.store_data(0, memData)

    until (Cmd_IHandler.isIdle && !isBusy, 450) {
      memory.process()
    }

    val HashBytes = (c.HashSize - 1) / 8 + 1
    val hash = computeHash(pearsonRomValues1, key, HashBytes) % c.NumKeys

    println("Reserving key")
    val resKey = TestInst(2, 0, 1, 2, true, true, true)
    Cmd_IHandler.inputs.enqueue(TestCmd(resKey, 0, key.length))

    until (Cmd_IHandler.isIdle && !isBusy, 450) {
      memory.process()
    }

    assert(!Resp_OHandler.outputs.isEmpty, "No response found")
    resp = Resp_OHandler.outputs.dequeue()
    assert(resp.data == hash, s"Expected ${hash} instead got ${resp.data}")

    println("Writing value")
    val assocAddr = TestInst(3, 0, 1, 2, false, true, true)
    val assocLen  = TestInst(4, 0, 1, 2, false, true, true)
    val writeVal  = TestInst(5, 0, 1, 2, false, true, true)

    Cmd_IHandler.inputs.enqueue(TestCmd(assocAddr, hash, accelAddr))
    Cmd_IHandler.inputs.enqueue(TestCmd(assocLen,  hash, value.length))
    Cmd_IHandler.inputs.enqueue(TestCmd(writeVal,  hash, key.length))

    accelAddr += value.length

    until (Cmd_IHandler.isIdle && !isBusy, 450) {
      memory.process()
    }
  }

  def setReadMode() {
    val readMode = TestInst(0, 0, 0, 0, false, false, false)
    Cmd_IHandler.inputs.enqueue(TestCmd(readMode))
    until (Cmd_IHandler.isIdle && !isBusy, 450) {}
    expect(c.io.readready, 1)
  }

  def setWriteMode() {
    val writeMode = TestInst(0, 0, 1, 0, false, false, false)
    Cmd_IHandler.inputs.enqueue(TestCmd(writeMode))
    until (Cmd_IHandler.isIdle && !isBusy, 10) {}
    expect(c.io.writeready, 1)
  }

  setKeyValue(key, value)

  def streamCurKey(key: String, tag: Int) {
    until (peek(c.io.keyInfo.ready) == 1, 450) {}

    wire_poke(c.io.keyInfo.valid, 1)
    wire_poke(c.io.keyInfo.bits.len, key.length)
    wire_poke(c.io.keyInfo.bits.tag, tag)
    takestep()
    wire_poke(c.io.keyInfo.valid, 0)
    takestep()

    until (peek(c.io.keyData.ready) == 1, 450) {}
    wire_poke(c.io.keyData.valid, 1)

    for (ch <- key) {
      wire_poke(c.io.keyData.bits, ch)
      takestep()
    }

    wire_poke(c.io.keyData.valid, 0)
    takestep()
  }

  def checkResult(value: String, tag: Int) {
    until (peek(c.io.resultInfo.valid) == 1, 450) {}

    expect(c.io.resultInfo.bits.len, value.length)
    expect(c.io.resultInfo.bits.tag, tag)

    wire_poke(c.io.resultInfo.ready, 1)
    takestep()
    wire_poke(c.io.resultInfo.ready, 0)
    wire_poke(c.io.resultInfo.ready, 0)
    wire_poke(c.io.resultData.ready, 1)

    for (ch <- value) {
      until(peek(c.io.resultData.valid) == 1, 10) {}
      expect(c.io.resultData.bits, ch)
      takestep()
    }

    wire_poke(c.io.resultData.ready, 0)
  }

  setReadMode()

  println("Streaming in key")
  streamCurKey(key, 0)
  println("Reading out value")
  checkResult(value, 0)

  println("Checking switch to write mode")
  setWriteMode()

  println("Checking resetCounts")
  val resetCounts = TestInst(6, 0, 0, 0, false, false, false)
  Cmd_IHandler.inputs.enqueue(TestCmd(resetCounts))
  until (Cmd_IHandler.isIdle && !isBusy, 10) {}
  takesteps(2) {}

  println("Checking short key")
  setKeyValue("k", "value")
  setReadMode()
  streamCurKey("k", 1)
  checkResult("value", 1)
}

object KeyValueStoreMain {
  def main(args: Array[String]) {
    chiselMain.run(args, () => (new KeyValueStore),
      (c: KeyValueStore) => new KeyValueStoreTest(c))
  }
}

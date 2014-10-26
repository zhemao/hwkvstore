package McAccel

import Chisel._
import Chisel.AdvTester._
import McAccel.TestUtils._
import McAccel.Constants._

class KeyValueStore (
    WordSize: Int, KeySize: Int, NumKeys: Int,
    ValCacheSize: Int, TagSize: Int) extends Module {
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

  val lookup = Module(
    new LookupPipeline(WordSize, KeySize, NumKeys, ValCacheSize, TagSize))
  lookup.io.readKeyInfo <> io.keyInfo
  lookup.io.readKeyData <> io.keyData
  lookup.io.resultInfo  <> io.resultInfo
  lookup.io.resultData  <> io.resultData

  val ctrl = Module(
    new CtrlModule(WordSize, ValAddrSize, KeyLenSize, HashSize, TagSize))
  ctrl.io.rocc    <> io.rocc
  ctrl.io.keyInfo <> lookup.io.writeKeyInfo
  ctrl.io.keyData <> lookup.io.writeKeyData
  ctrl.io.hashSel <> lookup.io.hashSel
  ctrl.io.cacheWriteAddr <> lookup.io.cacheWriteAddr
  ctrl.io.cacheWriteData <> lookup.io.cacheWriteData
  ctrl.io.cacheWriteEn   <> lookup.io.cacheWriteEn
  ctrl.io.keyLenAddr  <> lookup.io.keyLenAddr
  ctrl.io.keyLenData  <> lookup.io.keyLenData
  ctrl.io.keyLenWrite <> lookup.io.keyLenWrite
  ctrl.io.addrLenWriteAddr <> lookup.io.addrLenWriteAddr
  ctrl.io.addrLenWriteData <> lookup.io.addrLenWriteData
  ctrl.io.addrLenWriteEn   <> lookup.io.addrLenWriteEn
  ctrl.io.lock      <> lookup.io.lock
  ctrl.io.halted    <> lookup.io.halted
  ctrl.io.writemode <> lookup.io.writemode

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
  val keyWords = messToWords(key, 8)
  val valueWords = messToWords(value, 8)

  memory.store_data(0, keyWords)
  memory.store_data(256, valueWords)

  val writeMode = TestInst(0, 0, 1, 0, false, false, false)
  Cmd_IHandler.inputs.enqueue(TestCmd(writeMode))

  until (Cmd_IHandler.isIdle && !isBusy, 10) {
    Resp_OHandler.process()
  }
  expect(c.io.writeready, 1)

  val delKey = TestInst(1, 0, 1, 2, true, true, true)
  Cmd_IHandler.inputs.enqueue(TestCmd(delKey, 0, key.length))

  until (Cmd_IHandler.isIdle && !isBusy, 450) {
    peek(c.io.rocc.cmd.ready)
    Resp_OHandler.process()
    memory.process()
  }

  assert( !Resp_OHandler.outputs.isEmpty, "No response found" )
  val resp = Resp_OHandler.outputs.dequeue()
  assert( resp.data == HashNotFound,
    s"Expected HashNotFound, instead got ${resp.data}" )
}

object KeyValueStoreMain {
  def main(args: Array[String]) {
    chiselMainTest(args,
      () => Module(new KeyValueStore(32, 256, 16, 1024, 4))) {
        c => new KeyValueStoreTest(c)
    }
  }
}

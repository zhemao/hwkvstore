package pktfilter

import Chisel._
import Chisel.AdvTester._
import kvstore._
import kvstore.TestUtils._
import scala.collection.mutable.ArrayBuffer

class PacketTestSetup extends Module {
  val io = new Bundle {
    val rocc = new RoCCInterface
    val temac_rx = Stream(UInt(width = 8)).flip
    val core_rx  = Stream(UInt(width = 8))
    val temac_tx = Stream(UInt(width = 8))
    val core_tx  = Stream(UInt(width = 8)).flip
    val readready = Bool(OUTPUT)
  }

  val filter = Module(new PacketFilter)
  val kvstore = Module(new KeyValueStore)

  kvstore.io.keyInfo <> filter.io.keyInfo
  kvstore.io.keyData <> filter.io.keyData
  kvstore.io.resultInfo <> filter.io.resultInfo
  kvstore.io.resultData <> filter.io.resultData
  kvstore.io.readready <> filter.io.readready

  io.rocc <> kvstore.io.rocc
  io.temac_rx <> filter.io.temac_rx
  io.temac_tx <> filter.io.temac_tx
  io.core_rx  <> filter.io.core_rx
  io.core_tx  <> filter.io.core_tx

  io.readready := kvstore.io.readready
}

class PacketTest(c: PacketTestSetup) extends AdvTester(c) {
  var pktFile = getClass.getResourceAsStream("/mc-udp-pkt.raw")
  var byte = pktFile.read()

  val response = new ArrayBuffer[Int]()

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
  val key = "this key"
  val value = "this is the value"
  val keyWords = messToWords(key, 8)
  val valueWords = messToWords(value, 8)

  memory.store_data(0, keyWords)
  memory.store_data(256, valueWords)

  println("Reserving key")
  val resKey = TestInst(2, 0, 1, 2, true, true, true)
  Cmd_IHandler.inputs.enqueue(TestCmd(resKey, 0, key.length))

  until (Cmd_IHandler.isIdle && !isBusy, 450) {
    memory.process()
  }

  assert(!Resp_OHandler.outputs.isEmpty, "No response found")
  val resp = Resp_OHandler.outputs.dequeue()
  val hash = resp.data

  val assocAddr = TestInst(3, 0, 1, 2, false, true, true)
  val assocLen  = TestInst(4, 0, 1, 2, false, true, true)
  val writeVal  = TestInst(5, 0, 1, 2, false, true, true)
  val readMode = TestInst(0, 0, 0, 0, false, false, false)

  Cmd_IHandler.inputs.enqueue(TestCmd(assocAddr, hash, 0))
  Cmd_IHandler.inputs.enqueue(TestCmd(assocLen,  hash, value.length))
  Cmd_IHandler.inputs.enqueue(TestCmd(writeVal,  hash, 256))
  Cmd_IHandler.inputs.enqueue(TestCmd(readMode))

  until (Cmd_IHandler.isIdle && !isBusy, 450) {
    memory.process()
  }

  expect(c.io.readready, 1)
  wire_poke(c.io.temac_rx.valid, 1)

  while (byte != -1) {
    val nextbyte = pktFile.read()
    wire_poke(c.io.temac_rx.data, byte)
    if (nextbyte == -1)
      wire_poke(c.io.temac_rx.last, 1)
    until(peek(c.io.temac_rx.ready) == 1, 100) {}
    takestep()
    byte = nextbyte
  }

  wire_poke(c.io.temac_rx.last, 0)
  wire_poke(c.io.temac_rx.valid, 0)

  pktFile.close()

  var finished = false

  wire_poke(c.io.temac_tx.ready, 1)

  while (!finished) {
    until(peek(c.io.temac_tx.valid) == 1, 450) {}
    response += peek(c.io.temac_tx.data).intValue
    if (peek(c.io.temac_tx.last) == 1)
      finished = true
    takestep()
  }

  wire_poke(c.io.temac_tx.ready, 0)
  assert(response.size > 0, "Didn't get a response")

  until(peek(c.io.temac_rx.ready) == 1, 450) {}

  pktFile = getClass.getResourceAsStream("/arp-broadcast.raw")
  val pktBytes = new Array[Byte](42)
  pktFile.read(pktBytes)

  println("Sending ARP packet")

  wire_poke(c.io.temac_rx.valid, 1)

  for (i <- 0 until pktBytes.length) {
    val w = pktBytes(i).intValue & 0xff
    isTrace = true
    wire_poke(c.io.temac_rx.data, w)
    if (i == pktBytes.length - 1)
      wire_poke(c.io.temac_rx.last, 1)
    isTrace = false
    until(peek(c.io.temac_rx.ready) == 1, 100) {}
    takestep()
  }

  wire_poke(c.io.temac_rx.valid, 0)
  wire_poke(c.io.core_rx.ready, 1)

  println("Receiving ARP packet")

  for (i <- 0 until pktBytes.length) {
    until(peek(c.io.core_rx.valid) == 1, 100) {}
    val w = pktBytes(i).intValue & 0xff
    isTrace = true
    expect(c.io.core_rx.data, w)
    isTrace = false
    takestep()
  }

  wire_poke(c.io.core_rx.ready, 0)
}

object PacketTestMain {
  def main(args: Array[String]) {
    chiselMain.run(args, () => new PacketTestSetup(),
      (c: PacketTestSetup) => new PacketTest(c))
  }
}

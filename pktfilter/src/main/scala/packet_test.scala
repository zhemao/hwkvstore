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

  def roundup(num: Int) = if (num % 2 == 0) num else num + 1

  var accelAddr = 0

  def setKey(key: String, value: String) {
    val memData = messToWords(key + value, 8)

    memory.store_data(0, memData)

    val writeMode = TestInst(0, 0, 1, 0, false, false, false)
    val resKey = TestInst(2, 0, 1, 2, true, true, true)
    Cmd_IHandler.inputs.enqueue(TestCmd(writeMode))
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

    Cmd_IHandler.inputs.enqueue(TestCmd(assocAddr, hash, accelAddr))
    Cmd_IHandler.inputs.enqueue(TestCmd(assocLen,  hash, value.length))
    Cmd_IHandler.inputs.enqueue(TestCmd(writeVal,  hash, key.length))
    Cmd_IHandler.inputs.enqueue(TestCmd(readMode))
    accelAddr += roundup(value.length)

    until (Cmd_IHandler.isIdle && !isBusy, 450) {
      memory.process()
    }
    expect(c.io.readready, 1)
  }

  def sendPacket(packet: Array[Byte]) {
    wire_poke(c.io.temac_rx.last, 0)
    wire_poke(c.io.temac_rx.valid, 1)

    for (i <- 0 until packet.length) {
      val w = packet(i).intValue & 0xff
      wire_poke(c.io.temac_rx.data, w)
      if (i == packet.length - 1)
        wire_poke(c.io.temac_rx.last, 1)
      until(peek(c.io.temac_rx.ready) == 1, 200) {}
      takestep()
    }
    wire_poke(c.io.temac_rx.valid, 0)
  }

  def recvPacket(stream: StreamIO[UInt]): Array[Byte] = {
    val response = new ArrayBuffer[Byte]()
    var finished = false

    until(peek(stream.valid) == 1, 1000) {}
    wire_poke(stream.ready, 1)

    while (!finished) {
      val success = until(peek(stream.valid) == 1, 200) {}
      if (!success) {
        finished = true
      } else {
        response += peek(stream.data).byteValue
        if (peek(stream.last) == 1)
          finished = true
        takestep()
      }
    }

    wire_poke(stream.ready, 0)

    response.toArray
  }

  def packetsMatch(packet1: Array[Byte], packet2: Array[Byte]): Boolean = {
    if (packet1.length != packet2.length)
      false
    else {
      var ind = 0
      var matching = true
      while (ind < packet1.length && matching) {
        matching = (packet1(ind) == packet2(ind))
        ind += 1
      }
      matching
    }
  }

  setKey("this key", "this is the value")

  var pktFile = getClass.getResourceAsStream("/mc-udp-pkt.raw")
  val regularMcPacket = new Array[Byte](82)
  pktFile.read(regularMcPacket)
  pktFile.close()

  println("Send and receive regular MC packet")
  sendPacket(regularMcPacket)
  var response = recvPacket(c.io.temac_tx)

  assert(response.size > 0, "Didn't get a response for first MC packet")
  var respValue = new String(response.slice(78, response.length))
  assert(respValue == "this is the value", "response incorrect")
  dumpPacket(response)

  pktFile = getClass.getResourceAsStream("/arp-broadcast.raw")
  val arpPacket = new Array[Byte](42)
  pktFile.read(arpPacket)
  pktFile.close()

  println("Send and receive ARP packet")
  sendPacket(arpPacket)
  response = recvPacket(c.io.core_rx)
  assert(packetsMatch(response, arpPacket), "ARP packet does not match")

  setKey("0", "a")

  pktFile = getClass.getResourceAsStream("/mc-short-packet.raw")
  val shortMcPacket = new Array[Byte](75)
  pktFile.read(shortMcPacket)
  pktFile.close()

  println("Send and receive short MC packet")
  sendPacket(shortMcPacket)
  response = recvPacket(c.io.temac_tx)
  assert(response.size > 0, "Didn't get a response for second MC packet")
  respValue = new String(response.slice(78, response.length))
  assert(respValue == "a", "response incorrect")
  dumpPacket(response)

  println("Send and receive ARP again")
  sendPacket(arpPacket)
  response = recvPacket(c.io.core_rx)
  assert(packetsMatch(response, arpPacket), "ARP packet does not match")

  println("Changed short MC value and resend packet")
  setKey("0", "bc")
  sendPacket(shortMcPacket)
  response = recvPacket(c.io.temac_tx)
  assert(response.size > 0, "Didn't get a response for second MC packet")
  respValue = new String(response.slice(78, response.length))
  assert(respValue == "bc", "response incorrect")
  dumpPacket(response)

  val longReplyPacket = new Array[Byte](82)
  pktFile = getClass.getResourceAsStream("/mc-long-reply.raw")
  pktFile.read(longReplyPacket)
  pktFile.close()

  println("Send packet and receive long reply")
  val longValue = "199.813198495".padTo(199, '0')
  setKey("8.207044", longValue)
  sendPacket(longReplyPacket)
  response = recvPacket(c.io.temac_tx)
  assert(response.size > 0, "Didn't get a response for long reply MC packet")
  respValue = new String(response.slice(78, response.length))
  assert(respValue == longValue, "long response incorrect")
}

object PacketTestMain {
  def main(args: Array[String]) {
    chiselMain.run(args, () => new PacketTestSetup(),
      (c: PacketTestSetup) => new PacketTest(c))
  }
}

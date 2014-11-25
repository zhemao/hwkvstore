package pktfilter

import Chisel._
import kvstore.MessageInfo
import pktfilter.Constants._

class PacketFilter extends Module {
  val KeyLenSize = params[Int]("keylensize")
  val ValLenSize = params[Int]("vallensize")
  val TagSize = params[Int]("tagsize")
  val BufferSize = params[Int]("bufsize")
  val AddrSize = log2Up(BufferSize)

  val io = new Bundle {
    val temac_rx = Stream(UInt(width = 8)).flip
    val core_rx  = Stream(UInt(width = 8))
    val temac_tx = Stream(UInt(width = 8))
    val core_tx  = Stream(UInt(width = 8)).flip
    val keyInfo = Decoupled(new MessageInfo(KeyLenSize, TagSize))
    val keyData = Decoupled(UInt(width = 8))
    val resultInfo = Decoupled(new MessageInfo(ValLenSize, TagSize)).flip
    val resultData = Decoupled(UInt(width = 8)).flip
  }

  val curTag = Reg(init = UInt(0, TagSize))
  val getReqLens = Mem(UInt(width = KeyLenSize), 1 << TagSize)
  val getReqRoutes = Mem(new RoutingInfo, 1 << TagSize)

  val pktLen = Reg(init = UInt(0, 16))
  val keyLen = Reg(init = UInt(0, 16))
  val headerLen = Reg(init = UInt(0, 8))
  val lenOffset = Reg(UInt(width = 16))
  //val protOffset = Reg(UInt(width = 16))

  val ignore = Reg(init = Bool(false))

  val (m_idle :: m_ihl :: m_tlh :: m_tll :: m_prot ::
    m_start_stream :: m_finish :: m_start_skip ::
    m_srcaddr_start :: m_srcaddr :: m_dstaddr :: m_ip_end ::
    m_srcport_h :: m_srcport_l :: m_dstport_h :: m_dstport_l :: m_udp_end ::
    m_magic :: m_opcode :: m_keylen_h :: m_keylen_l :: rest) = Enum(Bits(), 29)
  val (m_xtralen :: m_key_info :: m_key_start :: m_pkt_defer ::
    m_keydata_send :: m_keydata_read :: m_keydata_last ::
    m_finish_defer :: Nil) = rest
  val m_state = Reg(init = m_idle)

  val mainWriter = StreamWriter(UInt(width = 8), 16)
  mainWriter.io.stream <> io.temac_rx
  mainWriter.io.ignore := ignore

  val pktCount = mainWriter.io.count
  val writeData = mainWriter.io.writeData
  val writeEn = mainWriter.io.writeEn

  val mainBuffer = Module(new PacketBuffer(BufferSize))
  mainBuffer.io.writeData := writeData
  mainBuffer.io.writeEn := writeEn
  mainBuffer.io.stream.valid := (m_state === m_start_stream || m_state === m_pkt_defer)
  mainBuffer.io.stream.bits := pktLen
  mainBuffer.io.skip.valid := (m_state === m_start_skip)
  mainBuffer.io.skip.bits := pktLen

  val rx_ready = (m_state != m_start_stream) && (m_state != m_start_skip) &&
                 (m_state != m_key_info) && (m_state != m_pkt_defer) &&
                 (m_state != m_keydata_send) && (m_state != m_keydata_last) &&
                 (m_state != m_finish_defer) &&
                 !mainBuffer.io.full
  mainWriter.io.enable := rx_ready

  val deferWriter = StreamWriter(UInt(width = 8), 16)
  deferWriter.io.ignore := Bool(false)

  val deferBuffer = Module(new PacketBuffer(BufferSize))
  deferBuffer.io.writeData := deferWriter.io.writeData
  deferBuffer.io.writeEn := deferWriter.io.writeEn
  deferWriter.io.enable := !deferBuffer.io.full

  val sendDefer = Reg(init = Bool(false))

  val streamSplit = StreamSplit(UInt(width = 8))
  streamSplit.io.in <> mainBuffer.io.readData
  streamSplit.io.out_b <> deferWriter.io.stream
  streamSplit.io.sel := sendDefer

  io.core_rx <> StreamArbiter(streamSplit.io.out_a, deferBuffer.io.readData)

  io.keyInfo.bits.len := keyLen(KeyLenSize - 1, 0)
  io.keyInfo.bits.tag := curTag
  io.keyInfo.valid := (m_state === m_key_info)

  io.keyData.bits := writeData
  io.keyData.valid := (m_state === m_keydata_send) || (m_state === m_keydata_last)

  val ipv6 = Reg(Bool())
  val srcAddr = Reg(UInt(width = 32))
  val dstAddr = Reg(UInt(width = 32))
  val srcPort = Reg(UInt(width = 16))
  val dstPort = Reg(UInt(width = 16))
  val curRoute = RoutingInfo(srcAddr, srcPort, dstAddr, dstPort)

  switch (m_state) {
    is (m_idle) {
      when (io.temac_rx.valid && !mainBuffer.io.full) {
        m_state := m_ihl
      }
    }
    is (m_ihl) {
      val version = writeData(7, 4)
      when (version === UInt(4)) {
        // IPv4
        headerLen := Cat(writeData(3, 0), UInt(0, 2))
        lenOffset := UInt(IPv4LengthOffset + 1)
        //protOffset := UInt(IPv4ProtocolOffset + 1)
        ipv6 := Bool(false)
        m_state := m_tlh
      } .elsewhen (version === UInt(6)) {
        // IPv6
        headerLen := UInt(40)
        lenOffset := UInt(IPv6LengthOffset + 1)
        //protOffset := UInt(IPv6ProtocolOffset + 1)
        ipv6 := Bool(true)
        m_state := m_tlh
      } .otherwise {
        // if the version makes no sense
        // ignore the rest of the packet
        // and skip the byte we have already read
        m_state := m_start_skip
        pktLen := UInt(2)
        ignore := Bool(true)
      }
    }
    is (m_tlh) {
      when (pktCount === lenOffset) {
        pktLen := Cat(writeData, UInt(0, 8))
        m_state := m_tll
      }
    }
    is (m_tll) {
      when (writeEn) {
        pktLen := Cat(pktLen(15, 8), writeData)
        when (ipv6) {
          m_state := m_start_stream
        } .otherwise {
          m_state := m_prot
        }
      }
    }
    is (m_prot) {
      when (pktCount === UInt(IPv4ProtocolOffset + 1)) {
        when (writeData === UInt(UdpProtocol)) {
          m_state := m_srcaddr_start
        } .otherwise {
          m_state := m_start_stream
        }
      }
    }
    is (m_srcaddr_start) {
      when (pktCount === UInt(IPv4SrcAddrOffset)) {
        srcAddr := UInt(0)
        m_state := m_srcaddr
      }
    }
    is (m_srcaddr) {
      when (writeEn) {
        srcAddr := Cat(srcAddr(23, 0), writeData)
        when (pktCount === UInt(IPv4DstAddrOffset)) {
          dstAddr := UInt(0)
          m_state := m_dstaddr
        }
      }
    }
    is (m_dstaddr) {
      when (writeEn) {
        dstAddr := Cat(dstAddr(23, 0), writeData)
        when (pktCount === headerLen) {
          headerLen := headerLen + UInt(8)
          m_state := m_srcport_h
        } .elsewhen (pktCount === UInt(IPv4OptionsOffset)) {
          m_state := m_ip_end
        }
      }
    }
    is (m_ip_end) {
      when (pktCount === headerLen) {
        headerLen := headerLen + UInt(8)
        m_state := m_srcport_h
      }
    }
    is (m_srcport_h) {
      when (writeEn) {
        srcPort := Cat(writeData, UInt(0, 8))
        m_state := m_srcport_l
      }
    }
    is (m_srcport_l) {
      when (writeEn) {
        srcPort := Cat(srcPort(15, 8), writeData)
        m_state := m_dstport_h
      }
    }
    is (m_dstport_h) {
      when (writeEn) {
        dstPort := Cat(writeData, UInt(0, 8))
        m_state := m_dstport_l
      }
    }
    is (m_dstport_l) {
      when (writeEn) {
        dstPort := Cat(dstPort(15, 8), writeData)
        m_state := m_udp_end
      }
    }
    is (m_udp_end) {
      when (pktCount === headerLen) {
        headerLen := headerLen + UInt(24)
        m_state := m_magic
      }
    }
    is (m_magic) {
      when (writeEn) {
        when (writeData === UInt(MCMagic)) {
          m_state := m_opcode
        } .otherwise {
          m_state := m_start_stream
        }
      }
    }
    is (m_opcode) {
      when (writeEn) {
        when (writeData === UInt(GetOpcode)) {
          m_state := m_keylen_h
        } .otherwise {
          m_state := m_start_stream
        }
      }
    }
    is (m_keylen_h) {
      when (writeEn) {
        keyLen := Cat(writeData, UInt(0, 8))
        m_state := m_keylen_l
      }
    }
    is (m_keylen_l) {
      when (writeEn) {
        keyLen := Cat(keyLen(15, 8), writeData)
        m_state := m_xtralen
      }
    }
    is (m_xtralen) {
      when (writeEn) {
        getReqRoutes(curTag) := curRoute
        headerLen := headerLen + writeData
        m_state := m_key_info
      }
    }
    is (m_key_info) {
      when (io.keyInfo.ready) {
        getReqLens(curTag) := pktLen
        curTag := curTag + UInt(1)
        m_state := m_key_start
      }
    }
    is (m_key_start) {
      when (pktCount === headerLen) {
        sendDefer := Bool(true)
        m_state := m_pkt_defer
      }
    }
    is (m_pkt_defer) {
      when (mainBuffer.io.stream.ready) {
        m_state := m_keydata_send
      }
    }
    is (m_keydata_send) {
      when (io.keyData.ready) {
        m_state := m_keydata_read
      }
    }
    is (m_keydata_read) {
      when (io.temac_rx.valid) {
        when (io.temac_rx.last) {
          m_state := m_keydata_last
        } .otherwise {
          m_state := m_keydata_send
        }
      }
    }
    is (m_keydata_last) {
      when (io.keyData.ready) {
        m_state := m_finish_defer
      }
    }
    is (m_finish_defer) {
      when (mainBuffer.io.stream.ready) {
        sendDefer := Bool(false)
        m_state := m_idle
      }
    }
    is (m_start_stream) {
      when (mainBuffer.io.stream.ready) {
        m_state := m_finish
      }
    }
    is (m_start_skip) {
      when (mainBuffer.io.skip.ready) {
        m_state := m_finish
      }
    }
    is (m_finish) {
      when (io.temac_rx.valid && io.temac_rx.last) {
        ignore := Bool(false)
        m_state := m_idle
      }
    }
  }

  val (d_idle :: d_check :: d_stream :: d_skip :: d_resp :: Nil) = Enum(Bits(), 5)
  val d_state = Reg(init = d_idle)
  val resTag = Reg(UInt(width = TagSize))
  val resLen = Reg(UInt(width = KeyLenSize))
  val reqPktRoute = Reg(new RoutingInfo)
  val reqPktLen = Reg(UInt(width = AddrSize))

  deferBuffer.io.stream.bits := reqPktLen
  deferBuffer.io.stream.valid := (d_state === d_stream)
  deferBuffer.io.skip.bits := reqPktLen
  deferBuffer.io.skip.valid := (d_state === d_skip)

  io.resultInfo.ready := (d_state === d_idle)

  val ResponderCacheSize = params[Int]("respcachesize")
  val responder = Module(new Responder(AddrSize, ResponderCacheSize))
  responder.io.resultData <> io.resultData
  responder.io.resLen := resLen
  responder.io.pktRoute := reqPktRoute
  responder.io.start := (d_state === d_resp)

  io.temac_tx <> StreamArbiter(io.core_tx, responder.io.temac_tx)

  switch (d_state) {
    is (d_idle) {
      when (io.resultInfo.valid) {
        resLen := io.resultInfo.bits.len
        resTag := io.resultInfo.bits.tag
        d_state := d_check
      }
    }
    is (d_check) {
      reqPktLen := getReqLens(resTag)
      reqPktRoute := getReqRoutes(resTag)
      when (resLen === UInt(0)) {
        d_state := d_stream
      } .otherwise {
        d_state := d_skip
      }
    }
    is (d_stream) {
      when (deferBuffer.io.stream.ready) {
        d_state := d_idle
      }
    }
    is (d_skip) {
      when (deferBuffer.io.skip.ready) {
        d_state := d_resp
      }
    }
    is (d_resp) {
      when (responder.io.ready) {
        d_state := d_idle
      }
    }
  }
}

class PacketFilterTest(c: PacketFilter) extends Tester(c) {
  def waitUntil(signal: Bool, timeout: Int) {
    isTrace = false
    var ticks = 0
    while (ticks < timeout && peek(signal) != 1) {
      step(1)
      ticks += 1
    }
    isTrace = true
    if (ticks == timeout)
      println(s"Error: timed out waiting for ${signal.name}")
  }

  def sendPacket(stream: StreamIO[UInt], packet: Array[Byte], key: String = null) {
    var keyind = 0

    poke(stream.valid, 1)
    if (key != null) {
      poke(c.io.keyInfo.ready, 1)
      poke(c.io.keyData.ready, 1)
    }
    for (i <- 0 until packet.size) {
      val byte = packet(i)
      val word = if (byte < 0) (256 + byte.intValue) else byte.intValue
      poke(stream.data, word)
      if (i == packet.size - 1)
        poke(stream.last, 1)
      else
        poke(stream.last, 0)

      waitUntil(stream.ready, 10)
      step(1)

      var cycles = 10
      while (cycles > 0 && peek(stream.ready) != 1) {
        if (key != null) {
          if (peek(c.io.keyInfo.valid) == 1) {
            expect(c.io.keyInfo.bits.len, key.size)
          }
          if (peek(c.io.keyData.valid) == 1) {
            expect(c.io.keyData.bits, key(keyind))
            keyind += 1
          }
        }
        cycles -= 1
        step(1)
      }
    }
    poke(stream.valid, 0)
    if (key != null)
      assert(keyind == key.length, "Error: didn't finish reading key data")
    poke(c.io.keyData.ready, 0)
    poke(c.io.keyInfo.ready, 0)
  }

  def temacSendPacket(packet: Array[Byte], key: String = null) {
    sendPacket(c.io.temac_rx, packet, key)
  }

  def coreSendPacket(packet: Array[Byte]) {
    sendPacket(c.io.core_tx, packet)
  }

  def recvPacket(stream: StreamIO[UInt], packet: Array[Byte]) {
    poke(stream.ready, 1)
    for (i <- 0 until packet.size) {
      waitUntil(stream.valid, 10)

      val byte = packet(i)
      val word = if (byte < 0) (256 + byte.intValue) else byte.intValue
      expect(stream.data, word)
      if (i == packet.size - 1)
        expect(stream.last, 1)
      else
        expect(stream.last, 0)
      step(1)
    }
    poke(stream.ready, 0)
  }

  def coreRecvPacket(packet: Array[Byte]) {
    recvPacket(c.io.core_rx, packet)
  }

  def temacRecvPacket(packet: Array[Byte]) {
    recvPacket(c.io.temac_tx, packet)
  }

  def sendResult(result: String, tag: Int) {
    waitUntil(c.io.resultInfo.ready, 10)
    poke(c.io.resultInfo.valid, 1)
    poke(c.io.resultInfo.bits.tag, tag)
    poke(c.io.resultInfo.bits.len, result.size)
    step(1)
    poke(c.io.resultInfo.valid, 0)

    if (result.length > 0) {
      waitUntil(c.io.resultData.ready, 100)
      poke(c.io.resultData.valid, 1)
      for (ch <- result) {
        poke(c.io.resultData.bits, ch)
        step(1)
        waitUntil(c.io.resultData.ready, 10)
      }
      poke(c.io.resultData.valid, 0)
    }
  }

  def coreTemacPassthru(n: Int) {
    poke(c.io.core_tx.valid, 1)
    poke(c.io.temac_tx.ready, 1)

    for (i <- 0 until n) {
      val word = rnd.nextInt & 0xff
      poke(c.io.core_tx.data, word)
      expect(c.io.temac_tx.data, word)
      step(1)
    }

    poke(c.io.core_tx.valid, 1)
    poke(c.io.core_tx.last, 1)
    step(1)
    poke(c.io.core_tx.valid, 0)
    poke(c.io.temac_tx.ready, 0)
  }

  val srcAddr = Array[Byte](1, 2, 3, 4)
  val srcPort = 0x1170
  val dstAddr = Array[Byte](5, 6, 7, 8)
  val dstPort = 0x1171
  val badPacket = Array[Byte](0, 0, 0, 0)
  val tcpPacket = IPv4Packet(TcpProtocol, srcAddr, dstAddr,
    Array[Byte](0, 1, 2, 3))
  val udpPacket = UdpPacket(srcAddr, srcPort, dstAddr, dstPort,
    Array[Byte](0, 1, 2, 3))
  val mcKey = "this is a key"
  val mcPacket = MemcachedGet(srcAddr, srcPort, dstAddr, dstPort, mcKey)
  val ipv6Packet = MemcachedGet(srcAddr, srcPort, dstAddr, dstPort,
    mcKey, ipv6 = true)
  val result = "this is the result"
  val mcResponse = MemcachedResp(dstAddr, dstPort, srcAddr, srcPort, result)

  println("Sending bad packet")
  temacSendPacket(badPacket)

  println("Sending TCP packet")
  temacSendPacket(tcpPacket)

  println("Receiving TCP packet")
  coreRecvPacket(tcpPacket)

  println("Sending memcached packet")
  temacSendPacket(mcPacket, mcKey)

  waitUntil(c.io.temac_rx.ready, 500)

  println("Sending UDP packet")
  temacSendPacket(udpPacket)

  sendResult("", 0)

  println("Receiving UDP packet")
  coreRecvPacket(udpPacket)

  println("Receiving memcached packet")
  coreRecvPacket(mcPacket)

  println("Sending IPv6 packet")
  temacSendPacket(ipv6Packet)

  println("Receiving IPv6 packet")
  coreRecvPacket(ipv6Packet)

  println("Sending memcached packet again")
  temacSendPacket(mcPacket, mcKey)

  // send the result back from the "accelerator"
  waitUntil(c.io.temac_rx.ready, 500)
  println("Sending accelerator result")
  sendResult(result, 1)

  // make sure that first to arbiter gets streamed out
  waitUntil(c.io.temac_tx.valid, 20)
  poke(c.io.core_tx.valid, 1)

  println("Getting memcached response packet")
  temacRecvPacket(mcResponse)

  coreTemacPassthru(10)
}

object PacketFilterMain {
  def main(args: Array[String]) {
    chiselMain.run(args, () => new PacketFilter(),
      (c: PacketFilter) => new PacketFilterTest(c))
  }
}

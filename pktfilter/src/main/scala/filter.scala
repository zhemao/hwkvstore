package pktfilter

import Chisel._
import kvstore.MessageInfo
import kvstore.TestUtils._
import pktfilter.Constants._

class PacketFilter extends Module {
  val KeyLenSize = params[Int]("keylensize")
  val ValLenSize = params[Int]("vallensize")
  val TagSize = params[Int]("tagsize")
  val BufferSize = params[Int]("bufsize")
  val AddrSize = log2Up(BufferSize)
  val ResultWidth = params[Int]("valwordsize")

  val io = new Bundle {
    val temac_rx = Stream(UInt(width = 8)).flip
    val core_rx  = Stream(UInt(width = 8))
    val temac_tx = Stream(UInt(width = 8))
    val core_tx  = Stream(UInt(width = 8)).flip
    val keyInfo = Decoupled(new MessageInfo(KeyLenSize, TagSize))
    val keyData = Decoupled(UInt(width = 8))
    val resultInfo = Decoupled(new MessageInfo(ValLenSize, TagSize)).flip
    val resultData = Decoupled(UInt(width = ResultWidth)).flip
    val readready = Bool(INPUT)
  }

  val curTag = Reg(init = UInt(0, TagSize))
  val getReqLens = Mem(UInt(width = KeyLenSize), 1 << TagSize, true)
  val getReqRoutes = Mem(new RoutingInfo, 1 << TagSize, true)

  val pktLen = Reg(init = UInt(0, 16))
  val keyLen = Reg(init = UInt(0, 16))
  val headerLen = Reg(init = UInt(0, 8))
  val lenOffset = Reg(UInt(width = 16))
  //val protOffset = Reg(UInt(width = 16))

  val (m_idle :: m_dstmac :: m_srcmac :: m_eth :: m_etl ::
    m_ihl :: m_tlh :: m_tll :: m_prot ::
    m_srcaddr_start :: m_srcaddr :: m_dstaddr :: m_ip_end ::
    m_srcport_h :: m_srcport_l :: m_dstport_h :: m_dstport_l :: m_udp_end ::
    rest) = Enum(Bits(), 33)
  val (m_reqid_h :: m_reqid_l :: m_mc_udp_end ::
    m_magic :: m_opcode :: m_keylen_h :: m_keylen_l ::
    m_xtralen :: m_key_info :: m_key_start ::
    m_keydata_send :: m_keydata_read :: m_keydata_last ::
    m_read_rest :: m_stream_out :: Nil) = rest
  val m_state = Reg(init = m_idle)

  val mainWriter = StreamWriter(UInt(width = 8), 16)
  mainWriter.io.stream <> io.temac_rx

  val writeFinished = mainWriter.io.finished
  val pktCount = mainWriter.io.count
  val writeData = mainWriter.io.writeData
  val writeEn = mainWriter.io.writeEn

  val mainBuffer = Module(new PacketBuffer(BufferSize))
  mainBuffer.io.writeData := writeData
  mainBuffer.io.writeEn := writeEn
  mainBuffer.io.stream.valid := (m_state === m_stream_out)
  mainBuffer.io.stream.bits := pktLen
  mainBuffer.io.skip.valid := Bool(false)
  mainBuffer.io.skip.bits := pktLen

  val rx_ready = (m_state != m_key_info) && (m_state != m_stream_out) &&
                 (m_state != m_keydata_send) && (m_state != m_keydata_last) &&
                 !mainBuffer.io.full
  mainWriter.io.enable := rx_ready

  val deferWriter = StreamWriter(UInt(width = 8), 16)

  val deferBuffer = Module(new PacketBuffer(BufferSize))
  deferBuffer.io.writeData := deferWriter.io.writeData
  deferBuffer.io.writeEn := deferWriter.io.writeEn
  deferWriter.io.enable := !deferBuffer.io.full

  val sendDefer = Reg(init = Bool(false))
  val splitSel = Reg(init = Bool(false))

  val streamSplit = StreamSplit(UInt(width = 8))
  streamSplit.io.in <> mainBuffer.io.readData
  streamSplit.io.out_b <> deferWriter.io.stream
  streamSplit.io.sel := splitSel

  io.core_rx <> StreamArbiter(streamSplit.io.out_a, deferBuffer.io.readData)

  io.keyInfo.bits.len := keyLen(KeyLenSize - 1, 0)
  io.keyInfo.bits.tag := curTag
  io.keyInfo.valid := (m_state === m_key_info)

  io.keyData.bits := writeData
  io.keyData.valid := (m_state === m_keydata_send) || (m_state === m_keydata_last)

  val macIndex = Reg(UInt(width = 3))
  val etherTypeHigh = Reg(UInt(width = 8))

  val ipv6 = Reg(Bool())
  val srcAddr = Reg(UInt(width = 32))
  val dstAddr = Reg(UInt(width = 32))
  val srcPort = Reg(UInt(width = 16))
  val dstPort = Reg(UInt(width = 16))
  val srcMac  = Vec.fill(6) { Reg(UInt(width = 8)) }
  val dstMac  = Vec.fill(6) { Reg(UInt(width = 8)) }
  val reqId   = Reg(UInt(width = 16))
  val curRoute = RoutingInfo(
    srcAddr, srcPort, dstAddr, dstPort, reqId, srcMac, dstMac)

  switch (m_state) {
    is (m_idle) {
      when (io.temac_rx.valid && !mainBuffer.io.full) {
        macIndex := UInt(0)
        headerLen := UInt(EthHeaderLen)
        m_state := m_dstmac
      }
    }
    is (m_dstmac) {
      when (writeEn) {
        dstMac(macIndex) := writeData
        when (macIndex === UInt(5)) {
          macIndex := UInt(0)
          m_state := m_srcmac
        } .otherwise {
          macIndex := macIndex + UInt(1)
        }
      }
    }
    is (m_srcmac) {
      when (writeEn) {
        srcMac(macIndex) := writeData
        when (macIndex === UInt(5)) {
          m_state := m_eth
        } .otherwise {
          macIndex := macIndex + UInt(1)
        }
      }
    }
    is (m_eth) {
      when (writeEn) {
        etherTypeHigh := writeData
        m_state := m_etl
      }
    }
    is (m_etl) {
      when (writeEn) {
        val etherType = Cat(etherTypeHigh, writeData)
        val isIP = etherType === UInt(IPv4EtherType) ||
          etherType === UInt(IPv6EtherType)
        when (isIP) {
          m_state := m_ihl
        } .otherwise {
          m_state := m_read_rest
        }
      }
    }
    is (m_ihl) {
      when (writeEn) {
        val version = writeData(7, 4)
        when (version === UInt(4)) {
          // IPv4
          headerLen := headerLen + Cat(writeData(3, 0), UInt(0, 2))
          lenOffset := UInt(EthHeaderLen + IPv4LengthOffset + 1)
          //protOffset := UInt(IPv4ProtocolOffset + 1)
          ipv6 := Bool(false)
          m_state := m_tlh
        } .elsewhen (version === UInt(6)) {
          // IPv6
          headerLen := headerLen + UInt(40)
          lenOffset := UInt(EthHeaderLen + IPv6LengthOffset + 1)
          //protOffset := UInt(IPv6ProtocolOffset + 1)
          ipv6 := Bool(true)
          m_state := m_tlh
        } .otherwise {
          // if the version makes no sense
          // just stream the packet to the core
          m_state := m_read_rest
        }
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
        pktLen := Cat(pktLen(15, 8), writeData) + UInt(EthHeaderLen)
        when (ipv6 || !io.readready) {
          m_state := m_read_rest
        } .otherwise {
          m_state := m_prot
        }
      }
    }
    is (m_prot) {
      when (pktCount === UInt(EthHeaderLen + IPv4ProtocolOffset + 1)) {
        when (writeData === UInt(UdpProtocol)) {
          m_state := m_srcaddr_start
        } .otherwise {
          m_state := m_read_rest
        }
      }
    }
    is (m_srcaddr_start) {
      when (pktCount === UInt(EthHeaderLen + IPv4SrcAddrOffset)) {
        srcAddr := UInt(0)
        m_state := m_srcaddr
      }
    }
    is (m_srcaddr) {
      when (writeEn) {
        srcAddr := Cat(srcAddr(23, 0), writeData)
        when (pktCount === UInt(EthHeaderLen + IPv4DstAddrOffset)) {
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
        } .elsewhen (pktCount === UInt(EthHeaderLen + IPv4OptionsOffset)) {
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
        headerLen := headerLen + UInt(8)
        m_state := m_reqid_h
      }
    }
    is (m_reqid_h) {
      when (headerLen >= pktLen) {
        m_state := m_read_rest
      } .elsewhen (writeEn) {
        reqId := Cat(writeData, UInt(0, 8))
        m_state := m_reqid_l
      }
    }
    is (m_reqid_l) {
      when (writeEn) {
        reqId := Cat(reqId(15, 8), writeData)
        m_state := m_mc_udp_end
      }
    }
    is (m_mc_udp_end) {
      when (pktCount === headerLen) {
        headerLen := headerLen + UInt(24)
        m_state := m_magic
      }
    }
    is (m_magic) {
      when (headerLen >= pktLen) {
        m_state := m_read_rest
      } .elsewhen (writeEn) {
        when (writeData === UInt(MCMagic)) {
          m_state := m_opcode
        } .otherwise {
          m_state := m_read_rest
        }
      }
    }
    is (m_opcode) {
      when (writeEn) {
        when (writeData === UInt(GetOpcode)) {
          m_state := m_keylen_h
        } .otherwise {
          m_state := m_read_rest
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
        m_state := m_key_start
      }
    }
    is (m_key_start) {
      when (pktCount === headerLen) {
        when (io.temac_rx.last) {
          m_state := m_keydata_last
        } .otherwise {
          m_state := m_keydata_send
        }
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
        sendDefer := Bool(true)
        pktLen := pktCount
        getReqLens(curTag) := pktCount
        curTag := curTag + UInt(1)
        m_state := m_stream_out
      }
    }
    is (m_read_rest) {
      when (writeFinished) {
        sendDefer := Bool(false)
        pktLen := pktCount
        m_state := m_stream_out
      }
    }
    is (m_stream_out) {
      when (mainBuffer.io.stream.ready) {
        splitSel := sendDefer
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
  val responder = Module(new Responder(AddrSize, ResponderCacheSize, ResultWidth))
  if (ResultWidth == 8)
    responder.io.resultData <> io.resultData
  else
    responder.io.resultData <> ByteSwapper(io.resultData, ResultWidth)
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
      throw new Exception(s"Error: timed out waiting for ${signal.name}")
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
    waitUntil(stream.valid, 1000)
    poke(stream.ready, 1)
    for (i <- 0 until packet.size) {
      waitUntil(stream.valid, 100)

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
    val resultWords = messToWords(result, c.ResultWidth / 8)

    waitUntil(c.io.resultInfo.ready, 10)
    poke(c.io.resultInfo.valid, 1)
    poke(c.io.resultInfo.bits.tag, tag)
    poke(c.io.resultInfo.bits.len, result.size)
    step(1)
    poke(c.io.resultInfo.valid, 0)

    if (result.length > 0) {
      waitUntil(c.io.resultData.ready, 100)
      poke(c.io.resultData.valid, 1)
      for (w <- resultWords) {
        poke(c.io.resultData.bits, w)
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
  val nonIpPacket = EthernetPacket(
    DefaultDstMac, DefaultSrcMac, 0x1124,
    Array.fill(36) { (rnd.nextInt.byteValue & 0xff).byteValue })
  // add some extra zeros onto the end to test robustness of state machine
  val tcpPacket = IPv4Packet(TcpProtocol, srcAddr, dstAddr,
    Array[Byte](0, 1, 2, 3)) ++ Array[Byte](0, 0)
  val udpPacket = UdpPacket(srcAddr, srcPort, dstAddr, dstPort,
    Array[Byte](0, 1, 2, 3)) ++ Array[Byte](0, 0, 0, 0)
  val mcKey = "this is a key"
  val mcPacket = MemcachedGet(srcAddr, srcPort, dstAddr, dstPort, mcKey, 1)
  val ipv6Packet = MemcachedGet(srcAddr, srcPort, dstAddr, dstPort,
    mcKey, 2, ipv6 = true)
  val result = "this is the result"
  val mcResponse = MemcachedResp(dstAddr, dstPort, srcAddr, srcPort, result, 1,
    false, 100, DefaultSrcMac, DefaultDstMac)

  poke(c.io.readready, 1)

  println("Sending non-IP packet")
  temacSendPacket(nonIpPacket)

  println("Receiving non-IP packet")
  coreRecvPacket(nonIpPacket)

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

  println("Receiving memcached packet")
  coreRecvPacket(mcPacket)

  println("Receiving UDP packet")
  coreRecvPacket(udpPacket)

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

  println("Sending and receiving TCP packet again")
  temacSendPacket(tcpPacket)
  coreRecvPacket(tcpPacket)

  println("Sending memcached request with readready off")
  poke(c.io.readready, 0)
  temacSendPacket(mcPacket)
  coreRecvPacket(mcPacket)

  println("Sending memcached request again")
  poke(c.io.readready, 1)
  temacSendPacket(mcPacket, mcKey)
  sendResult(result, 2)
  temacRecvPacket(mcResponse)
}

object PacketFilterMain {
  def main(args: Array[String]) {
    chiselMain.run(args, () => new PacketFilter(),
      (c: PacketFilter) => new PacketFilterTest(c))
  }
}

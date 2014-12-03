package pktfilter

import Chisel._
import pktfilter.Constants._
import pktfilter.ChecksumUtils._

class Responder(AddrSize: Int, CacheSize: Int) extends Module {
  val io = new Bundle {
    val temac_tx = Stream(UInt(width = 8))
    val resultData = Decoupled(UInt(width = 8)).flip
    val resLen = UInt(INPUT, AddrSize)
    val pktRoute = new RoutingInfo().asInput
    val start = Bool(INPUT)
    val ready = Bool(OUTPUT)
  }

  val EthHeaderLen = 14
  // IP header + UDP header + Memcached header
  val HeaderLen = 20 + 8 + 36

  val ethHeaderIndex = Reg(UInt(width = log2Up(EthHeaderLen)))
  val ethHeaderData = UInt(width = 8)

  val headerIndex = Reg(UInt(width = log2Up(HeaderLen)))
  val headerData = UInt(width = 8)

  val resLen = if (AddrSize < 16)
    Cat(UInt(0, 16 - AddrSize), io.resLen)
  else
    io.resLen(15, 0)

  val ipPktLen = resLen + UInt(HeaderLen)
  val udpPktLen = resLen + UInt(8 + 36)
  val mcPktLen = resLen + UInt(36)

  val ipChecksum = Reg(UInt(width = 16))
  val udpChecksum = Reg(UInt(width = 16))

  val buffer = Mem(UInt(width = 8), CacheSize)

  val bodyIndex = Reg(UInt(width = AddrSize))
  val bodyData = buffer(bodyIndex)

  val TTL = 100

  val IPHeaderSize = 20

  switch (ethHeaderIndex) {
    ethHeaderData := UInt(0)

    // destination MAC address (src and dst switched)
    is (UInt(0))  { ethHeaderData := io.pktRoute.srcMac(0) }
    is (UInt(1))  { ethHeaderData := io.pktRoute.srcMac(1) }
    is (UInt(2))  { ethHeaderData := io.pktRoute.srcMac(2) }
    is (UInt(3))  { ethHeaderData := io.pktRoute.srcMac(3) }
    is (UInt(4))  { ethHeaderData := io.pktRoute.srcMac(4) }
    is (UInt(5))  { ethHeaderData := io.pktRoute.srcMac(5) }

    // source MAC address
    is (UInt(6))  { ethHeaderData := io.pktRoute.dstMac(0) }
    is (UInt(7))  { ethHeaderData := io.pktRoute.dstMac(1) }
    is (UInt(8))  { ethHeaderData := io.pktRoute.dstMac(2) }
    is (UInt(9))  { ethHeaderData := io.pktRoute.dstMac(3) }
    is (UInt(10)) { ethHeaderData := io.pktRoute.dstMac(4) }
    is (UInt(11)) { ethHeaderData := io.pktRoute.dstMac(5) }

    // EtherType
    is (UInt(12)) { ethHeaderData := UInt(0x08) }
  }

  switch(headerIndex) {
    // default value
    headerData := UInt(0)

    // IP header
    // version is 4 and header len is 5
    is (UInt(0))  { headerData := UInt(0x45) }
    // total packet length
    is (UInt(2))  { headerData := ipPktLen(15, 8) }
    is (UInt(3))  { headerData := ipPktLen(7, 0) }
    // TTL
    is (UInt(8))  { headerData := UInt(TTL) }
    // Protocol
    is (UInt(9))  { headerData := UInt(UdpProtocol) }
    // IP Header Checksum
    is (UInt(10)) { headerData := ipChecksum(15, 8) }
    is (UInt(11)) { headerData := ipChecksum(7, 0) }
    // source address (src and dst switched for reply)
    is (UInt(12)) { headerData := io.pktRoute.dstAddr(31, 24) }
    is (UInt(13)) { headerData := io.pktRoute.dstAddr(23, 16) }
    is (UInt(14)) { headerData := io.pktRoute.dstAddr(15, 8) }
    is (UInt(15)) { headerData := io.pktRoute.dstAddr(7, 0) }
    // dest address
    is (UInt(16)) { headerData := io.pktRoute.srcAddr(31, 24) }
    is (UInt(17)) { headerData := io.pktRoute.srcAddr(23, 16) }
    is (UInt(18)) { headerData := io.pktRoute.srcAddr(15, 8) }
    is (UInt(19)) { headerData := io.pktRoute.srcAddr(7, 0) }

    // UDP Header
    // source port
    is (UInt(20)) { headerData := io.pktRoute.dstPort(15, 8) }
    is (UInt(21)) { headerData := io.pktRoute.dstPort(7, 0) }
    // dest port
    is (UInt(22)) { headerData := io.pktRoute.srcPort(15, 8) }
    is (UInt(23)) { headerData := io.pktRoute.srcPort(7, 0) }
    // udp packet length
    is (UInt(24)) { headerData := udpPktLen(15, 8) }
    is (UInt(25)) { headerData := udpPktLen(7, 0) }
    // UDP checksum
    is (UInt(26)) { headerData := udpChecksum(15, 8) }
    is (UInt(27)) { headerData := udpChecksum(7, 0) }

    // Memcached UDP header
    is (UInt(28)) { headerData := io.pktRoute.reqId(15, 8) }
    is (UInt(29)) { headerData := io.pktRoute.reqId(7, 0) }
    is (UInt(33)) { headerData := UInt(1) }

    // Memcached Header
    // Magic
    is (UInt(36)) { headerData := UInt(0x81) }
    // Extra length
    is (UInt(40)) { headerData := UInt(4) }
    // body length
    is (UInt(46)) { headerData := resLen(15, 8) }
    is (UInt(47)) { headerData := resLen(7, 0) }
    // Extras 0xDEADBEEF
    is (UInt(60)) { headerData := UInt(0xde) }
    is (UInt(61)) { headerData := UInt(0xad) }
    is (UInt(62)) { headerData := UInt(0xbe) }
    is (UInt(63)) { headerData := UInt(0xef) }
  }

  val PseudoHeaderSize = 56

  val pseudoHeaderData = UInt(width = 8)
  switch (headerIndex) {
    pseudoHeaderData := UInt(0)
    // source addr
    is (UInt(0))  { pseudoHeaderData := io.pktRoute.dstAddr(31, 24) }
    is (UInt(1))  { pseudoHeaderData := io.pktRoute.dstAddr(23, 16) }
    is (UInt(2))  { pseudoHeaderData := io.pktRoute.dstAddr(15, 8) }
    is (UInt(3))  { pseudoHeaderData := io.pktRoute.dstAddr(7, 0) }
    // dest addr
    is (UInt(4))  { pseudoHeaderData := io.pktRoute.srcAddr(31, 24) }
    is (UInt(5))  { pseudoHeaderData := io.pktRoute.srcAddr(23, 16) }
    is (UInt(6))  { pseudoHeaderData := io.pktRoute.srcAddr(15, 8) }
    is (UInt(7))  { pseudoHeaderData := io.pktRoute.srcAddr(7, 0) }
    // protocol
    is (UInt(9))  { pseudoHeaderData := UInt(UdpProtocol) }
    // UDP length
    is (UInt(10)) { pseudoHeaderData := udpPktLen(15, 8) }
    is (UInt(11)) { pseudoHeaderData := udpPktLen(7, 0) }
    // source port
    is (UInt(12)) { pseudoHeaderData := io.pktRoute.dstPort(15, 8) }
    is (UInt(13)) { pseudoHeaderData := io.pktRoute.dstPort(7, 0) }
    // dest port
    is (UInt(14)) { pseudoHeaderData := io.pktRoute.srcPort(15, 8) }
    is (UInt(15)) { pseudoHeaderData := io.pktRoute.srcPort(7, 0) }
    // length
    is (UInt(16)) { pseudoHeaderData := udpPktLen(15, 8) }
    is (UInt(17)) { pseudoHeaderData := udpPktLen(7, 0) }


    // Memcached UDP Header
    is (UInt(20)) { pseudoHeaderData := io.pktRoute.reqId(15, 8) }
    is (UInt(21)) { pseudoHeaderData := io.pktRoute.reqId(7, 0) }
    is (UInt(25)) { pseudoHeaderData := UInt(1) }

    // Memcached Header
    // Magic
    is (UInt(28)) { pseudoHeaderData := UInt(0x81) }
    // Extra length
    is (UInt(32)) { pseudoHeaderData := UInt(4) }
    // body length
    is (UInt(38)) { pseudoHeaderData := resLen(15, 8) }
    is (UInt(39)) { pseudoHeaderData := resLen(7, 0) }
    // Extras 0xDEADBEEF
    is (UInt(52)) { pseudoHeaderData := UInt(0xde) }
    is (UInt(53)) { pseudoHeaderData := UInt(0xad) }
    is (UInt(54)) { pseudoHeaderData := UInt(0xbe) }
    is (UInt(55)) { pseudoHeaderData := UInt(0xef) }
  }

  val pktData = Reg(UInt(width = 8))
  val pktLen = Reg(UInt(width = AddrSize))
  val pktLast = Reg(init = Bool(false))

  val (s_idle :: s_ip_cs_start :: s_ip_cs_feed :: s_ip_cs_end ::
       s_udp_cs_start :: s_udp_cs_feed_head :: s_udp_cs_read_body ::
       s_udp_cs_feed_body :: s_udp_cs_end :: 
       s_send_eth_header :: s_send_header :: s_send_body ::
       Nil) = Enum(Bits(), 12)
  val state = Reg(init = s_idle)

  val csCompute = Module(new ChecksumCompute(AddrSize))
  csCompute.io.len.bits := pktLen
  csCompute.io.len.valid :=
    (state === s_ip_cs_start) || (state === s_udp_cs_start)
  csCompute.io.data.bits := pktData
  csCompute.io.data.valid := (state === s_ip_cs_feed) ||
    (state === s_udp_cs_feed_head) || (state === s_udp_cs_feed_body)
  csCompute.io.result.ready := (state === s_ip_cs_end) || (state === s_udp_cs_end)

  io.ready := (state === s_idle)
  io.temac_tx.valid := (state === s_send_eth_header) ||
    (state === s_send_header) || (state === s_send_body)
  io.temac_tx.last := pktLast
  io.temac_tx.data := pktData
  io.resultData.ready :=
    (state === s_udp_cs_read_body) || (state === s_udp_cs_feed_body)

  switch (state) {
    is (s_idle) {
      when (io.start) {
        pktLen := UInt(IPHeaderSize)
        headerIndex := UInt(0)
        ethHeaderIndex := UInt(0)
        state := s_ip_cs_start
      }
    }
    is (s_ip_cs_start) {
      when (csCompute.io.len.ready) {
        pktData := headerData
        headerIndex := headerIndex + UInt(1)
        state := s_ip_cs_feed
      }
    }
    is (s_ip_cs_feed) {
      pktData := headerData
      headerIndex := headerIndex + UInt(1)
      // end of IP packet
      when (headerIndex === pktLen) {
        state := s_ip_cs_end
      }
    }
    is (s_ip_cs_end) {
      when (csCompute.io.result.valid) {
        ipChecksum := csCompute.io.result.bits
        headerIndex := UInt(0)
        // UDP packet + pseudo-IP header
        pktLen := udpPktLen + UInt(12)
        state := s_udp_cs_start
      }
    }
    is (s_udp_cs_start) {
      when (csCompute.io.len.ready) {
        pktData := pseudoHeaderData
        headerIndex := headerIndex + UInt(1)
        state := s_udp_cs_feed_head
      }
    }
    is (s_udp_cs_feed_head) {
      pktData := pseudoHeaderData
      headerIndex := headerIndex + UInt(1)
      when (headerIndex === UInt(PseudoHeaderSize)) {
        bodyIndex := UInt(0)
        state := s_udp_cs_read_body
      }
    }
    is (s_udp_cs_read_body) {
      when (io.resultData.valid) {
        pktData := io.resultData.bits
        state := s_udp_cs_feed_body
      }
    }
    is (s_udp_cs_feed_body) {
      buffer(bodyIndex) := pktData
      when (bodyIndex === io.resLen - UInt(1)) {
        headerIndex := UInt(0)
        state := s_udp_cs_end
      } .otherwise {
        bodyIndex := bodyIndex + UInt(1)
        when (io.resultData.valid) {
          pktData := io.resultData.bits
        } .otherwise {
          state := s_udp_cs_read_body
        }
      }
    }
    is (s_udp_cs_end) {
      when (csCompute.io.result.valid) {
        udpChecksum := csCompute.io.result.bits
        pktData := ethHeaderData
        ethHeaderIndex := ethHeaderIndex + UInt(1)
        pktLen := io.resLen
        bodyIndex := UInt(0)
        state := s_send_eth_header
      }
    }
    is (s_send_eth_header) {
      when (io.temac_tx.ready) {
        when (ethHeaderIndex === UInt(EthHeaderLen)) {
          pktData := headerData
          headerIndex := headerIndex + UInt(1)
          state := s_send_header
        } .otherwise {
          pktData := ethHeaderData
          ethHeaderIndex := ethHeaderIndex + UInt(1)
        }
      }
    }
    is (s_send_header) {
      when (io.temac_tx.ready) {
        // the total header size is 64, so the address will roll over to 0
        when (headerIndex === UInt(0)) {
          pktData := bodyData
          bodyIndex := bodyIndex + UInt(1)
          state := s_send_body
        } .otherwise {
          pktData := headerData
          headerIndex := headerIndex + UInt(1)
        }
      }
    }
    is (s_send_body) {
      when (io.temac_tx.ready) {
        when (bodyIndex === pktLen) {
          state := s_idle
        } .otherwise {
          pktData := bodyData
          pktLast := (bodyIndex === io.resLen - UInt(1))
          bodyIndex := bodyIndex + UInt(1)
        }
      }
    }
  }
}

class ResponderTest(c: Responder) extends Tester(c) {
  val srcaddr = Array[Byte](127, 0, 0, 1)
  val srcport = 11270
  val dstaddr = Array[Byte](10, 0, 0, 2)
  val dstport = 11271
  val result = "this is the result"
  val packet = MemcachedResp(
    srcaddr, srcport, dstaddr, dstport, result, 0, false, c.TTL)

  // prepend 0 byte to make sure the ints aren't negative
  val srcAddrInt = BigInt(Array[Byte](0) ++ srcaddr)
  val dstAddrInt = BigInt(Array[Byte](0) ++ dstaddr)

  val srcMacInts = DefaultSrcMac.map(b => BigInt(Array(0.byteValue, b)))
  val dstMacInts = DefaultDstMac.map(b => BigInt(Array(0.byteValue, b)))

  // remember that src and dst are reversed
  poke(c.io.pktRoute.dstAddr, srcAddrInt)
  poke(c.io.pktRoute.dstPort, srcport)
  poke(c.io.pktRoute.dstMac, srcMacInts)
  poke(c.io.pktRoute.srcAddr, dstAddrInt)
  poke(c.io.pktRoute.srcPort, dstport)
  poke(c.io.pktRoute.srcMac, dstMacInts)
  poke(c.io.pktRoute.reqId, 0)
  poke(c.io.resLen, result.length)

  expect(c.io.ready, 1)
  poke(c.io.start, 1)
  step(1)
  poke(c.io.start, 0)

  var cycles = 0
  isTrace = false
  while (cycles < 100 && peek(c.io.resultData.ready) != 1) {
    cycles += 1
    step(1)
  }
  isTrace = true

  poke(c.io.resultData.valid, 1)
  for (ch <- result) {
    expect(c.io.resultData.ready, 1)
    poke(c.io.resultData.bits, ch)
    step(1)
  }

  isTrace = false
  poke(c.io.temac_tx.ready, 1)
  cycles = 0
  while (cycles < 500 && peek(c.io.temac_tx.valid) != 1) {
    cycles += 1
    step(1)
  }
  isTrace = true

  if (cycles == 500) {
    println("Error: waiting for temac_tx timed out")
  } else {
    var ind = 0
    for (b <- packet) {
      val w = b.intValue & 0xff
      println(s"Byte ${ind}")
      expect(c.io.temac_tx.data, w)
      step(1)
      ind += 1
    }
  }

  dumpPacket(packet)
}

object ResponderMain {
  def main(args: Array[String]) {
    chiselMain.run(args, () => new Responder(12, 2048),
      (c: Responder) => new ResponderTest(c))
  }
}

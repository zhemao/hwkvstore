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
    val temac = Stream(UInt(width = 8)).flip
    val core  = Stream(UInt(width = 8))
    val keyInfo = Decoupled(new MessageInfo(KeyLenSize, TagSize))
    val keyData = Decoupled(UInt(width = 8))
    val resultInfo = Decoupled(new MessageInfo(ValLenSize, TagSize)).flip
    val resultData = Decoupled(UInt(width = 8)).flip
  }

  val curTag = Reg(init = UInt(0, TagSize))
  val getReqLens = Mem(UInt(width = KeyLenSize), 1 << TagSize)

  val pktLen = Reg(init = UInt(0, 16))
  val keyLen = Reg(init = UInt(0, 16))
  val headerLen = Reg(init = UInt(0, 8))
  val lenOffset = Reg(UInt(width = 16))
  val protOffset = Reg(UInt(width = 16))

  val ignore = Reg(init = Bool(false))

  val (m_idle :: m_ihl :: m_tlh :: m_tll :: m_prot ::
    m_start_stream :: m_finish :: m_start_skip :: m_ip_end :: m_udp_end ::
    m_magic :: m_opcode :: m_keylen_h :: m_keylen_l :: m_xtralen ::
    m_key_info :: m_key_start :: m_pkt_defer ::
    m_keydata_send :: m_keydata_read :: m_keydata_last :: m_finish_defer ::
    Nil) = Enum(Bits(), 22)
  val m_state = Reg(init = m_idle)

  val recvDefer = Reg(init = Bool(false))
  val streamMux = StreamMux(UInt(width = 8))
  streamMux.io.sel := recvDefer
  streamMux.io.out <> io.core

  val mainWriter = StreamWriter(UInt(width = 8), 16)
  mainWriter.io.stream <> io.temac
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
  deferBuffer.io.readData <> streamMux.io.in_b
  deferBuffer.io.writeData := deferWriter.io.writeData
  deferBuffer.io.writeEn := deferWriter.io.writeEn
  deferWriter.io.enable := !deferBuffer.io.full

  val sendDefer = Reg(init = Bool(false))

  val streamSplit = StreamSplit(UInt(width = 8))
  streamSplit.io.in <> mainBuffer.io.readData
  streamSplit.io.out_a <> streamMux.io.in_a
  streamSplit.io.out_b <> deferWriter.io.stream
  streamSplit.io.sel := sendDefer

  io.keyInfo.bits.len := keyLen(KeyLenSize - 1, 0)
  io.keyInfo.bits.tag := curTag
  io.keyInfo.valid := (m_state === m_key_info)

  io.keyData.bits := writeData
  io.keyData.valid := (m_state === m_keydata_send) || (m_state === m_keydata_last)

  val ipv6 = Reg(Bool())

  switch (m_state) {
    is (m_idle) {
      when (io.temac.valid && !mainBuffer.io.full && !recvDefer) {
        m_state := m_ihl
      }
    }
    is (m_ihl) {
      val version = writeData(7, 4)
      when (version === UInt(4)) {
        // IPv4
        headerLen := Cat(writeData(3, 0), UInt(0, 2))
        lenOffset := UInt(IPv4LengthOffset + 1)
        protOffset := UInt(IPv4ProtocolOffset + 1)
        ipv6 := Bool(false)
        m_state := m_tlh
      } .elsewhen (version === UInt(6)) {
        // IPv6
        headerLen := UInt(40)
        lenOffset := UInt(IPv6LengthOffset + 1)
        protOffset := UInt(IPv6ProtocolOffset + 1)
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
      when (pktCount === protOffset) {
        when (writeData === UInt(UdpProtocol)) {
          m_state := m_ip_end
        } .otherwise {
          m_state := m_start_stream
        }
      }
    }
    is (m_ip_end) {
      when (pktCount === headerLen) {
        headerLen := headerLen + UInt(8)
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
      when (io.temac.valid) {
        when (io.temac.last) {
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
      when (io.temac.valid && io.temac.last) {
        ignore := Bool(false)
        m_state := m_idle
      }
    }
  }

  val streamRunning = Reg(init = Bool(false))
  when (streamSplit.io.out_a.valid && streamSplit.io.out_a.ready) {
    when (streamSplit.io.out_a.last) {
      streamRunning := Bool(false)
    } .otherwise {
      streamRunning := Bool(true)
    }
  }

  val (d_idle :: d_check :: d_switch :: d_start :: d_finish :: Nil) = Enum(Bits(), 5)
  val d_state = Reg(init = d_idle)
  val resTag = Reg(UInt(width = TagSize))
  val resLen = Reg(UInt(width = KeyLenSize))
  val deferPktLen = Reg(UInt(width = AddrSize))

  deferBuffer.io.stream.bits := deferPktLen
  deferBuffer.io.stream.valid := (d_state === d_start)
  deferBuffer.io.skip.bits := deferPktLen

  io.resultInfo.ready := (d_state === d_idle)

  switch (d_state) {
    is (d_idle) {
      when (io.resultInfo.valid) {
        resLen := io.resultInfo.bits.len
        resTag := io.resultInfo.bits.tag
        d_state := d_check
      }
    }
    is (d_check) {
      when (resLen === UInt(0)) {
        deferPktLen := getReqLens(resTag)
        d_state := d_switch
      }
    }
    is (d_switch) {
      when (!streamRunning) {
        recvDefer := Bool(true)
        d_state := d_start
      }
    }
    is (d_start) {
      when (deferBuffer.io.stream.ready) {
        d_state := d_finish
      }
    }
    is (d_finish) {
      val deferFinished = deferBuffer.io.readData.valid &&
        deferBuffer.io.readData.last &&
        deferBuffer.io.readData.ready
      when (deferFinished) {
        recvDefer := Bool(false)
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
      println(s"Error: timed out after ${ticks} cycles")
  }

  def sendPacket(packet: Array[Byte], key: String = null) {
    var keyind = 0

    poke(c.io.temac.valid, 1)
    if (key != null) {
      poke(c.io.keyInfo.ready, 1)
      poke(c.io.keyData.ready, 1)
    }
    for (i <- 0 until packet.size) {
      val byte = packet(i)
      val word = if (byte < 0) (256 + byte.intValue) else byte.intValue
      poke(c.io.temac.data, word)
      if (i == packet.size - 1)
        poke(c.io.temac.last, 1)
      else
        poke(c.io.temac.last, 0)
      step(1)

      var cycles = 10
      while (cycles > 0 && peek(c.io.temac.ready) != 1) {
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
      waitUntil(c.io.temac.ready, 10)
    }
    poke(c.io.temac.valid, 0)
    if (key != null)
      assert(keyind == key.length, "Error: didn't finish reading key data")
    poke(c.io.keyData.ready, 0)
    poke(c.io.keyInfo.ready, 0)
  }

  def recvPacket(packet: Array[Byte]) {
    poke(c.io.core.ready, 1)
    for (i <- 0 until packet.size) {
      waitUntil(c.io.core.valid, 10)

      val byte = packet(i)
      val word = if (byte < 0) (256 + byte.intValue) else byte.intValue
      expect(c.io.core.data, word)
      if (i == packet.size - 1)
        expect(c.io.core.last, 1)
      else
        expect(c.io.core.last, 0)
      step(1)
    }
    poke(c.io.core.ready, 0)
  }

  val badPacket = Array[Byte](0, 0, 0, 0)
  val tcpPacket = IPv4Packet(TcpProtocol, Array[Byte](0, 1, 2, 3))
  val udpPacket = UdpPacket(Array[Byte](0, 1, 2, 3))
  val mcKey = "this is a key"
  val mcPacket = MemcachedGet(mcKey)
  val ipv6Packet = MemcachedGet(mcKey, ipv6 = true)

  println("Sending bad packet")
  sendPacket(badPacket)

  println("Sending TCP packet")
  sendPacket(tcpPacket)

  println("Receiving TCP packet")
  recvPacket(tcpPacket)

  println("Sending memcached packet")
  sendPacket(mcPacket, mcKey)

  waitUntil(c.io.temac.ready, 500)

  println("Sending UDP packet")
  sendPacket(udpPacket)

  waitUntil(c.io.resultInfo.ready, 10)
  poke(c.io.resultInfo.valid, 1)
  poke(c.io.resultInfo.bits.tag, 0)
  poke(c.io.resultInfo.bits.len, 0)
  step(1)
  poke(c.io.resultInfo.valid, 0)

  println("Receiving UDP packet")
  recvPacket(udpPacket)

  println("Receiving memcached packet")
  recvPacket(mcPacket)

  println("Sending IPv6 packet")
  sendPacket(ipv6Packet)

  println("Receiving IPv6 packet")
  recvPacket(ipv6Packet)
}

object PacketFilterMain {
  def main(args: Array[String]) {
    chiselMain.run(args, () => new PacketFilter(),
      (c: PacketFilter) => new PacketFilterTest(c))
  }
}

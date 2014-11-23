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

  val (s_idle :: s_ihl :: s_tlh :: s_tll :: s_prot ::
    s_start_stream :: s_finish :: s_start_skip :: s_ip_end :: s_udp_end ::
    s_magic :: s_opcode :: s_keylen_h :: s_keylen_l :: s_xtralen ::
    s_key_info :: s_key_start :: s_pkt_defer ::
    s_keydata_send :: s_keydata_read :: s_keydata_last :: s_finish_defer ::
    Nil) = Enum(Bits(), 22)
  val state = Reg(init = s_idle)

  val mainWriter = StreamWriter(UInt(width = 8), 16)
  mainWriter.io.stream <> io.temac
  mainWriter.io.ignore := ignore

  val pktCount = mainWriter.io.count
  val writeData = mainWriter.io.writeData
  val writeEn = mainWriter.io.writeEn

  val mainBuffer = Module(new PacketBuffer(BufferSize))
  mainBuffer.io.writeData := writeData
  mainBuffer.io.writeEn := writeEn
  mainBuffer.io.stream.valid := (state === s_start_stream || state === s_pkt_defer)
  mainBuffer.io.stream.bits := pktLen
  mainBuffer.io.skip.valid := (state === s_start_skip)
  mainBuffer.io.skip.bits := pktLen

  val rx_ready = (state != s_start_stream) && (state != s_start_skip) &&
                 (state != s_key_info) && (state != s_pkt_defer) &&
                 (state != s_keydata_send) && (state != s_keydata_last) &&
                 (state != s_finish_defer) &&
                 !mainBuffer.io.full
  mainWriter.io.enable := rx_ready

  val deferWriter = StreamWriter(UInt(width = 8), 16)
  deferWriter.io.ignore := Bool(false)

  val deferBuffer = Module(new PacketBuffer(BufferSize))
  deferBuffer.io.writeData := deferWriter.io.writeData
  deferBuffer.io.writeEn := deferWriter.io.writeEn

  deferWriter.io.enable := !deferBuffer.io.full

  val sendDefer = Reg(init = Bool(false))

  val streamMux = StreamMux(UInt(width = 8))
  streamMux.io.in <> mainBuffer.io.readData
  streamMux.io.out_a <> io.core
  streamMux.io.out_b <> deferWriter.io.stream
  streamMux.io.sel := sendDefer

  io.keyInfo.bits.len := keyLen(KeyLenSize - 1, 0)
  io.keyInfo.bits.tag := curTag
  io.keyInfo.valid := (state === s_key_info)

  io.keyData.bits := writeData
  io.keyData.valid := (state === s_keydata_send) || (state === s_keydata_last)

  switch (state) {
    is (s_idle) {
      when (io.temac.valid && !mainBuffer.io.full) {
        state := s_ihl
      }
    }
    is (s_ihl) {
      val version = writeData(7, 4)
      when (version === UInt(4)) {
        // IPv4
        headerLen := Cat(writeData(3, 0), UInt(0, 2))
        lenOffset := UInt(IPv4LengthOffset + 1)
        protOffset := UInt(IPv4ProtocolOffset + 1)
        state := s_tlh
      } .elsewhen (version === UInt(6)) {
        // IPv6
        headerLen := UInt(40)
        lenOffset := UInt(IPv6LengthOffset + 1)
        protOffset := UInt(IPv6ProtocolOffset + 1)
        state := s_tlh
      } .otherwise {
        // if the version makes no sense
        // ignore the rest of the packet
        // and skip the byte we have already read
        state := s_start_skip
        pktLen := UInt(2)
        ignore := Bool(true)
      }
    }
    is (s_tlh) {
      when (pktCount === lenOffset) {
        pktLen := Cat(writeData, UInt(0, 8))
        state := s_tll
      }
    }
    is (s_tll) {
      when (writeEn) {
        pktLen := Cat(pktLen(15, 8), writeData)
        state := s_prot
      }
    }
    is (s_prot) {
      when (pktCount === protOffset) {
        when (writeData === UInt(UdpProtocol)) {
          state := s_ip_end
        } .otherwise {
          state := s_start_stream
        }
      }
    }
    is (s_ip_end) {
      when (pktCount === headerLen) {
        headerLen := headerLen + UInt(8)
        state := s_udp_end
      }
    }
    is (s_udp_end) {
      when (pktCount === headerLen) {
        headerLen := headerLen + UInt(24)
        state := s_magic
      }
    }
    is (s_magic) {
      when (writeEn) {
        when (writeData === UInt(MCMagic)) {
          state := s_opcode
        } .otherwise {
          state := s_start_stream
        }
      }
    }
    is (s_opcode) {
      when (writeEn) {
        when (writeData === UInt(GetOpcode)) {
          state := s_keylen_h
        } .otherwise {
          state := s_start_stream
        }
      }
    }
    is (s_keylen_h) {
      when (writeEn) {
        keyLen := Cat(writeData, UInt(0, 8))
        state := s_keylen_l
      }
    }
    is (s_keylen_l) {
      when (writeEn) {
        keyLen := Cat(keyLen(15, 8), writeData)
        state := s_xtralen
      }
    }
    is (s_xtralen) {
      when (writeEn) {
        headerLen := headerLen + writeData
        state := s_key_info
      }
    }
    is (s_key_info) {
      when (io.keyInfo.ready) {
        getReqLens(curTag) := pktLen
        curTag := curTag + UInt(1)
        state := s_key_start
      }
    }
    is (s_key_start) {
      when (pktCount === headerLen) {
        sendDefer := Bool(true)
        state := s_pkt_defer
      }
    }
    is (s_pkt_defer) {
      when (mainBuffer.io.stream.ready) {
        state := s_keydata_send
      }
    }
    is (s_keydata_send) {
      when (io.keyData.ready) {
        state := s_keydata_read
      }
    }
    is (s_keydata_read) {
      when (io.temac.valid) {
        when (io.temac.last) {
          state := s_keydata_last
        } .otherwise {
          state := s_keydata_send
        }
      }
    }
    is (s_keydata_last) {
      when (io.keyData.ready) {
        state := s_finish_defer
      }
    }
    is (s_finish_defer) {
      when (mainBuffer.io.stream.ready) {
        sendDefer := Bool(false)
        state := s_idle
      }
    }
    is (s_start_stream) {
      when (mainBuffer.io.stream.ready) {
        state := s_finish
      }
    }
    is (s_start_skip) {
      when (mainBuffer.io.skip.ready) {
        state := s_finish
      }
    }
    is (s_finish) {
      when (io.temac.valid && io.temac.last) {
        ignore := Bool(false)
        state := s_idle
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

      expect(c.io.core.data, packet(i))
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

  println("Receiving UDP packet")
  recvPacket(udpPacket)
}

object PacketFilterMain {
  def main(args: Array[String]) {
    chiselMain.run(args, () => new PacketFilter(),
      (c: PacketFilter) => new PacketFilterTest(c))
  }
}

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

  val pktCount = Reg(init = UInt(0, 16))
  val pktLen   = Reg(init = UInt(0, 16))

  val keyLen = Reg(init = UInt(0, 16))
  val headerLen = Reg(init = UInt(0, 8))
  val lenOffset = Reg(UInt(width = 16))
  val protOffset = Reg(UInt(width = 16))

  val writeData = Reg(init = UInt(0, 8))
  val writeEn = Reg(init = Bool(false))
  val ignore = Reg(init = Bool(false))

  val (s_idle :: s_ihl :: s_tlh :: s_tll :: s_prot ::
    s_start_stream :: s_finish :: s_start_skip :: s_ip_end ::
    s_magic :: s_opcode :: s_keylen_h :: s_keylen_l :: s_xtralen ::
    s_key_start :: Nil) = Enum(Bits(), 15)
  val state = Reg(init = s_idle)

  val buffer = Module(new PacketBuffer(BufferSize))
  buffer.io.readData <> io.core
  buffer.io.writeData := writeData
  buffer.io.writeEn := writeEn
  buffer.io.stream.bits := pktLen
  buffer.io.stream.valid := (state === s_start_stream)
  buffer.io.skip.bits := pktLen
  buffer.io.skip.valid := (state === s_start_skip)

  val rx_ready = (state != s_start_stream) && (state != s_start_skip) &&
                 !buffer.io.full

  switch (state) {
    is (s_idle) {
      when (io.temac.valid) {
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
        state := s_key_start
      }
    }
    is (s_key_start) {
      when (pktCount === headerLen) {
      }
    }
    is (s_start_stream) {
      when (buffer.io.stream.ready) {
        state := s_finish
      }
    }
    is (s_start_skip) {
      when (buffer.io.skip.ready) {
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

  io.temac.ready := rx_ready

  when (io.temac.valid && rx_ready) {
    writeEn := !ignore
    writeData := io.temac.data
    when (io.temac.last) {
      pktCount := UInt(0)
    } .otherwise {
      pktCount := pktCount + UInt(1)
    }
  } .otherwise {
    writeEn := Bool(false)
  }
}

class PacketFilterTest(c: PacketFilter) extends Tester(c) {
  def sendPacket(packet: Array[Byte]) {
    poke(c.io.temac.valid, 1)
    for (i <- 0 until packet.size) {
      poke(c.io.temac.data, packet(i))
      if (i == packet.size - 1)
        poke(c.io.temac.last, 1)
      else
        poke(c.io.temac.last, 0)
      step(1)

      isTrace = false
      while (peek(c.io.temac.ready) != 1)
        step(1)
      isTrace = true
    }
    poke(c.io.temac.valid, 0)
  }

  def recvPacket(packet: Array[Byte]) {
    poke(c.io.core.ready, 1)
    for (i <- 0 until packet.size) {
      isTrace = false
      var ticker = 0
      while (peek(c.io.core.valid) != 1 && ticker < 100) {
        ticker += 1
        step(1)
      }
      isTrace = true

      expect(c.io.core.valid, 1)
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

  println("Sending bad packet")
  sendPacket(badPacket)

  println("Sending TCP packet")
  sendPacket(tcpPacket)

  println("Receiving TCP packet")
  recvPacket(tcpPacket)
}

object PacketFilterMain {
  def main(args: Array[String]) {
    chiselMain.run(args, () => new PacketFilter(),
      (c: PacketFilter) => new PacketFilterTest(c))
  }
}

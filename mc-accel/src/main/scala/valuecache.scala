package McAccel

import Chisel._
import McAccel.TestUtils._

class ValueCache(NumKeys: Int, CacheSize: Int, TagSize: Int) extends Module {
  val HashSize = log2Up(NumKeys)
  val AddrSize = log2Up(CacheSize)

  val io = new Bundle {
    val hashIn = Decoupled(new HashSelection(HashSize, TagSize)).flip
    val resultInfo = Decoupled(new MessageInfo(AddrSize, TagSize))
    val resultData = Decoupled(UInt(width = 8))

    val cacheWriteAddr = UInt(INPUT, AddrSize)
    val cacheWriteData = UInt(INPUT, 8)
    val cacheWriteEn = Bool(INPUT)

    val addrLenAddr = UInt(INPUT, HashSize)
    val addrLenWriteData = new AddrLenPair(AddrSize, INPUT)
    val addrLenWriteEn = Vec.fill(2) { Bool(INPUT) }
    val addrLenReadData = new AddrLenPair(AddrSize, OUTPUT)
  }

  val cacheMem = Mem(UInt(width = 8), CacheSize, true)
  val cacheAddr = Reg(UInt(width = AddrSize))
  val cacheData = cacheMem(cacheAddr)

  val cacheWriteEn = Reg(next = io.cacheWriteEn)
  val cacheWriteAddr = Reg(next = io.cacheWriteAddr)
  val cacheWriteData = Reg(next = io.cacheWriteData)

  when (cacheWriteEn) {
    cacheMem(cacheWriteAddr) := cacheWriteData
  }

  val addrTable = Mem(UInt(width = AddrSize), NumKeys, true)
  val lenTable  = Mem(UInt(width = AddrSize), NumKeys, true)
  val addrLenAddr = Reg(UInt(width = HashSize))
  val addrLenData = new AddrLenPair(AddrSize)
  addrLenData.addr := addrTable(addrLenAddr)
  addrLenData.len  := lenTable(addrLenAddr)

  io.addrLenReadData.addr := addrTable(io.addrLenAddr)
  io.addrLenReadData.len  := lenTable(io.addrLenAddr)

  when (io.addrLenWriteEn(0)) {
    addrTable(io.addrLenAddr) := io.addrLenWriteData.addr
  }
  when (io.addrLenWriteEn(1)) {
    lenTable(io.addrLenAddr)  := io.addrLenWriteData.len
  }

  val tag = Reg(UInt(width = TagSize))
  val len = Reg(UInt(width = AddrSize))

  val (s_wait :: s_notfound :: s_lookup :: s_notify :: s_stream :: Nil) = Enum(UInt(), 5)
  val state = Reg(init = s_wait)

  switch (state) {
    is (s_wait) {
      when (io.hashIn.valid) {
        tag := io.hashIn.bits.tag
        when (io.hashIn.bits.found) {
          addrLenAddr := io.hashIn.bits.hash
          state := s_lookup
        } .otherwise {
          len := UInt(0)
          state := s_notfound
        }
      }
    }
    is (s_notfound) {
      when (io.resultInfo.ready) {
        state := s_wait
      }
    }
    is (s_lookup) {
      cacheAddr := addrLenData.addr
      len := addrLenData.len
      state := s_notify
    }
    is (s_notify) {
      when (io.resultInfo.ready) {
        len := len - UInt(1)
        state := s_stream
      }
    }
    is (s_stream) {
      when (io.resultData.ready) {
        when (len === UInt(0)) {
          state := s_wait
        } .otherwise {
          cacheAddr := cacheAddr + UInt(1)
          len := len - UInt(1)
        }
      }
    }
  }

  io.hashIn.ready := (state === s_wait)
  io.resultInfo.bits.len := len
  io.resultInfo.bits.tag := tag
  io.resultInfo.valid := (state === s_notify || state === s_notfound)
  io.resultData.valid := (state === s_stream)
  io.resultData.bits := cacheData
}

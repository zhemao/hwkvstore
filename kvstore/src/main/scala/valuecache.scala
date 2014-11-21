package kvstore

import Chisel._
import kvstore.TestUtils._
import kvstore.Constants._

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
    val addrLenReadEn = Bool(INPUT)
  }

  val BankMems = params[Boolean]("bankmems")
  val BankSize = params[Int]("banksize")

  val cacheMem = if (BankMems && BankSize != CacheSize) {
    Module(new BankedMem(8, BankSize, CacheSize / BankSize))
  } else {
    Module(new UnbankedMem(8, CacheSize))
  }
  val cacheAddr = Reg(UInt(width = AddrSize))
  val cacheData = cacheMem.io.readData
  val cacheReadEn = Reg(init = Bool(false))

  cacheMem.io.readAddr := cacheAddr
  cacheMem.io.readEn   := cacheReadEn

  val MemReadDelay = cacheMem.ReadDelay

  cacheMem.io.writeAddr := io.cacheWriteAddr
  cacheMem.io.writeData := io.cacheWriteData
  cacheMem.io.writeEn   := io.cacheWriteEn

  val addrLenAddr = Reg(UInt(width = HashSize))
  val realAddrLenAddr = Mux(io.addrLenReadEn, io.addrLenAddr, addrLenAddr)

  val addrTable = Module(new UnbankedMem(AddrSize, NumKeys))
  addrTable.io.readAddr := realAddrLenAddr
  addrTable.io.writeEn := io.addrLenWriteEn(0)
  addrTable.io.writeData := io.addrLenWriteData.addr
  addrTable.io.writeAddr := io.addrLenAddr

  val lenTable  = Module(new UnbankedMem(AddrSize, NumKeys))
  lenTable.io.readAddr := realAddrLenAddr
  lenTable.io.writeEn := io.addrLenWriteEn(1)
  lenTable.io.writeData := io.addrLenWriteData.len
  lenTable.io.writeAddr := io.addrLenAddr

  val AddrLookupDelay = 2

  val addrLenData = new AddrLenPair(AddrSize)
  addrLenData.addr := addrTable.io.readData
  addrLenData.len  := lenTable.io.readData

  io.addrLenReadData.addr := addrTable.io.readData
  io.addrLenReadData.len  := lenTable.io.readData

  val tag = Reg(UInt(width = TagSize))
  val len = Reg(UInt(width = AddrSize))

  val (s_wait :: s_notfound :: s_wait_lookup :: s_lookup :: s_notify ::
       s_delay :: s_stream :: Nil) = Enum(UInt(), 7)
  val state = Reg(init = s_wait)

  val delayCount = Reg(UInt(width = log2Up(MemReadDelay)))

  switch (state) {
    is (s_wait) {
      when (io.hashIn.valid) {
        tag := io.hashIn.bits.tag
        when (io.hashIn.bits.found) {
          addrLenAddr := io.hashIn.bits.hash
          state := s_wait_lookup
          delayCount := UInt(AddrLookupDelay - 1)
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
    is (s_wait_lookup) {
      when (delayCount === UInt(0)) {
        state := s_lookup
      } .otherwise {
        delayCount := delayCount - UInt(1)
      }
    }
    is (s_lookup) {
      cacheAddr := addrLenData.addr
      len := addrLenData.len
      state := s_notify
    }
    is (s_notify) {
      when (io.resultInfo.ready) {
        state := s_delay
        delayCount := UInt(MemReadDelay - 1)
        cacheReadEn := Bool(true)
      }
    }
    is (s_delay) {
      cacheAddr := cacheAddr + UInt(1)
      cacheReadEn := Bool(true)

      when (delayCount === UInt(0)) {
        state := s_stream
        len := len - UInt(1)
      } .otherwise {
        delayCount := delayCount - UInt(1)
      }
    }
    is (s_stream) {
      cacheReadEn := Bool(false)
      when (io.resultData.ready) {
        when (len === UInt(0)) {
          state := s_wait
          cacheReadEn := Bool(false)
        } .otherwise {
          cacheAddr := cacheAddr + UInt(1)
          cacheReadEn := Bool(true)
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

class ValueCacheTest(c: ValueCache) extends Tester(c) {
  val TestSize = 100
  val data = (0 until TestSize).map { i => rnd.nextInt(256) }

  poke(c.io.cacheWriteEn, 1)
  for (i <- 0 until TestSize) {
    poke(c.io.cacheWriteAddr, i)
    poke(c.io.cacheWriteData, data(i))
    step(1)
  }
  poke(c.io.cacheWriteEn, 0)

  poke(c.io.addrLenAddr, 0)
  poke(c.io.addrLenWriteData.addr, 0)
  poke(c.io.addrLenWriteData.len, TestSize)
  poke(c.io.addrLenWriteEn, Array[BigInt](1, 1))
  step(1)
  poke(c.io.addrLenWriteEn, Array[BigInt](0, 0))

  poke(c.io.hashIn.bits.found, 1)
  poke(c.io.hashIn.bits.hash, 0)
  poke(c.io.hashIn.bits.tag, 0)
  poke(c.io.hashIn.valid, 1)

  expect(c.io.hashIn.ready, 1)
  step(1)
  poke(c.io.hashIn.valid, 0)

  poke(c.io.resultInfo.ready, 1)
  step(c.AddrLookupDelay + 1)

  expect(c.io.resultInfo.valid, 1)
  expect(c.io.resultInfo.bits.tag, 0)
  expect(c.io.resultInfo.bits.len, TestSize)
  step(1)
  poke(c.io.resultInfo.ready, 0)
  poke(c.io.resultData.ready, 1)
  step(c.MemReadDelay)

  for (byte <- data) {
    expect(c.io.resultData.valid, 1)
    expect(c.io.resultData.bits, byte)
    step(1)
  }

  expect(c.io.resultData.valid, 0)
}

object ValueCacheMain {
  def main(args: Array[String]) {
    chiselMain.run(args, () => new ValueCache(256, 4096, 2),
      (c: ValueCache) => new ValueCacheTest(c))
  }
}

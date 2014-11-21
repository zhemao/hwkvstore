package kvstore

import Chisel._
import kvstore.TestUtils._

abstract class DelayedMem(val WordSize: Int, val MemSize: Int)
    extends Module {
  val AddrSize = log2Up(MemSize)
  val ReadDelay: Int
  val io = new Bundle {
    val readAddr = UInt(INPUT, AddrSize)
    val readData = UInt(OUTPUT, WordSize)
    val readEn   = Bool(INPUT)

    val writeAddr = UInt(INPUT, AddrSize)
    val writeData = UInt(INPUT, WordSize)
    val writeEn   = Bool(INPUT)
  }
}

class UnbankedMem(WordSize: Int, MemSize: Int)
    extends DelayedMem(WordSize, MemSize) {

  val ReadDelay = 2
  val readAddrReg  = Reg(next = io.readAddr)
  val writeAddrReg = Reg(next = io.writeAddr)
  val writeDataReg = Reg(next = io.writeData)
  val writeEnReg   = Reg(next = io.writeEn)

  val mem = Mem(UInt(width = WordSize), MemSize, true)

  val readData = mem(readAddrReg)

  io.readData := Reg(next = readData)

  when (writeEnReg) {
    mem(writeAddrReg) := writeDataReg
  }
}

class BankedMem(WordSize: Int, val BankSize: Int, val NumBanks: Int)
    extends DelayedMem(WordSize, BankSize * NumBanks) {
  val TotalSize = MemSize
  val MaxFanIn = params[Int]("maxfanin")

  val FullAddrSize = log2Up(TotalSize)
  val BankAddrSize = log2Up(BankSize)

  val FullBankSelSize = FullAddrSize - BankAddrSize
  val BankSelSize = log2Up(MaxFanIn)
  val DecodeDelay = (FullBankSelSize - 1) / BankSelSize + 1
  val ReadDelay = 2 + DecodeDelay

  val bankReadAddr0 = io.readAddr(BankAddrSize - 1, 0)
  val bankReadSel0  = io.readAddr(FullAddrSize - 1, BankAddrSize)
  val bankReadAddr1 = Reg(UInt(width = BankAddrSize))
  val bankReadSel1  = Reg(UInt(width = FullBankSelSize))
  val bankReadSel2  = Reg(UInt(width = FullBankSelSize))

  when (io.readEn) {
    bankReadAddr1 := bankReadAddr0
    bankReadSel1 := bankReadSel0
    bankReadSel2 := bankReadSel1
  }

  val bankWriteAddr0 = io.writeAddr(BankAddrSize - 1, 0)
  val bankWriteSel0  = io.writeAddr(FullAddrSize - 1, BankAddrSize)
  val bankWriteAddr1 = Reg(next = bankWriteAddr0)
  val bankWriteSel1  = Reg(next = bankWriteSel0)
  val bankWriteAddr2 = Reg(next = bankWriteAddr1)

  val writeDataReg = Reg(next = io.writeData)
  val writeEnReg   = Reg(next = io.writeEn)

  val bankWriteData = Reg(next = writeDataReg)
  val bankWriteEn = Vec((0 until NumBanks).map {
    i => Reg(next = writeEnReg && bankWriteSel1 === UInt(i))
  })

  val banks = Array.fill(NumBanks) {
    Mem(UInt(width = WordSize), BankSize, true)
  }
  val bankReadData2 = Vec.fill(NumBanks) { Reg(UInt(width = WordSize)) }

  for (i <- 0 until NumBanks) {
    val bank = banks(i)
    when (io.readEn) {
      bankReadData2(i) := bank(bankReadAddr1)
    }
    when (bankWriteEn(i)) {
      bank(bankWriteAddr2) := bankWriteData
    }
  }

  if (DecodeDelay == 1) {
    val readData2 = bankReadData2(bankReadSel2)
    val readData3 = Reg(UInt(width = WordSize))
    when (io.readEn) {
      readData3 := readData2
    }
    io.readData := readData3
  } else {
    val bankReadDataStages = new Array[Vec[UInt]](DecodeDelay)
    val bankReadSelStages  = new Array[(Int, UInt)](DecodeDelay)

    bankReadDataStages(0) = bankReadData2
    bankReadSelStages(0) = (FullBankSelSize, bankReadSel2)

    for (stage <- 1 until DecodeDelay) {
      val lastStage = bankReadDataStages(stage - 1)
      val (lastWidth, lastSel) = bankReadSelStages(stage - 1)
      val curSel = lastSel(BankSelSize - 1, 0)
      val nextSel = lastSel(lastWidth - 1, BankSelSize)
      val nextWidth = lastWidth - BankSelSize
      val stageSize = lastStage.size / MaxFanIn

      val nextSelReg = Reg(UInt(width = nextWidth))
      when (io.readEn) {
        nextSelReg := nextSel
      }

      val curStage = Vec.fill(stageSize) {
        Reg(UInt(width = WordSize))
      }
      for (i <- 0 until stageSize) {
        val subBank = Vec.tabulate(MaxFanIn) {
          j => lastStage(i * MaxFanIn + j)
        }
        when (io.readEn) {
          curStage(i) := subBank(curSel)
        }
      }
      bankReadDataStages(stage) = curStage
      bankReadSelStages(stage) = (nextWidth, nextSelReg)
    }

    val finalStage = bankReadDataStages(DecodeDelay - 1)
    val finalSel = bankReadSelStages(DecodeDelay - 1)._2
    val readDataFin = finalStage(finalSel)
    val readDataReg = Reg(UInt(width = WordSize))
    when (io.readEn) {
      readDataReg := readDataFin
    }
    io.readData := readDataReg
  }
}

class BankedMemTester(c: BankedMem) extends Tester(c) {
  val NumTests = 10
  val addrs =  Array.fill(NumTests) { rnd.nextInt(c.TotalSize) }
  val values = Array.fill(NumTests) { rnd.nextInt(1 << c.WordSize) }
  val MemReadDelay = 2 + c.DecodeDelay

  poke(c.io.writeEn, 1)
  for (i <- 0 until NumTests) {
    poke(c.io.writeAddr, addrs(i))
    poke(c.io.writeData, values(i))
    step(1)
  }
  poke(c.io.writeEn, 0)

  poke(c.io.readEn, 1)
  for (i <- 0 until MemReadDelay) {
    poke(c.io.readAddr, addrs(i))
    step(1)
  }

  for (i <- MemReadDelay until NumTests) {
    expect(c.io.readData, values(i - MemReadDelay))
    poke(c.io.readAddr, addrs(i))
    step(1)
  }

  poke(c.io.readEn, 0)
  step(2)
  expect(c.io.readData, values(NumTests - MemReadDelay))
  poke(c.io.readEn, 1)

  for (i <- (NumTests - MemReadDelay) until NumTests) {
    expect(c.io.readData, values(i))
    step(1)
  }
}

object BankedMemMain {
  def main(args: Array[String]) {
    chiselMain.run(args, () => new BankedMem(16, 128, 16),
      (c: BankedMem) => new BankedMemTester(c))
  }
}

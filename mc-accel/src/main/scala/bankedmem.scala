package McAccel

import Chisel._
import McAccel.TestUtils._
import McAccel.Constants.MaxFanIn

class UnbankedMem(val WordSize: Int, val MemSize: Int) extends Module {
  val AddrSize = log2Up(MemSize)
  val io = new Bundle {
    val readAddr = UInt(INPUT, AddrSize)
    val readData = UInt(OUTPUT, WordSize)

    val writeAddr = UInt(INPUT, AddrSize)
    val writeData = UInt(INPUT, WordSize)
    val writeEn   = Bool(INPUT)
  }

  val readAddrReg  = Reg(next = io.readAddr)
  val writeAddrReg = Reg(next = io.writeAddr)
  val writeDataReg = Reg(next = io.writeData)
  val writeEnReg   = Reg(next = io.writeEn)

  val mem = Mem(UInt(width = WordSize), MemSize)

  val readData = mem(readAddrReg)

  io.readData := Reg(next = readData)

  when (writeEnReg) {
    mem(writeAddrReg) := writeDataReg
  }
}

class BankedMem(val WordSize: Int, val BankSize: Int, val NumBanks: Int)
    extends Module {
  val TotalSize = BankSize * NumBanks

  val FullAddrSize = log2Up(TotalSize)
  val BankAddrSize = log2Up(BankSize)

  val io = new Bundle {
    val readAddr = UInt(INPUT, FullAddrSize)
    val readData = UInt(OUTPUT, WordSize)

    val writeAddr = UInt(INPUT, FullAddrSize)
    val writeData = UInt(INPUT, WordSize)
    val writeEn   = Bool(INPUT)
  }

  val FullBankSelSize = FullAddrSize - BankAddrSize
  val BankSelSize = log2Up(MaxFanIn)
  val DecodeDelay = (FullBankSelSize - 1) / BankSelSize + 1

  val bankReadAddr0 = io.readAddr(BankAddrSize - 1, 0)
  val bankReadSel0  = io.readAddr(FullAddrSize - 1, BankAddrSize)
  val bankReadAddr1 = Reg(next = bankReadAddr0)
  val bankReadSel1  = Reg(next = bankReadSel0)
  val bankReadSel2  = Reg(next = bankReadSel1)

  val bankWriteAddr0 = io.writeAddr(BankAddrSize - 1, 0)
  val bankWriteSel0  = io.writeAddr(FullAddrSize - 1, BankAddrSize)
  val bankWriteAddr1 = Reg(next = bankWriteAddr0)
  val bankWriteSel1  = Reg(next = bankWriteSel0)

  val writeDataReg = Reg(next = io.writeData)
  val writeEnReg   = Reg(next = io.writeEn)

  val bankWriteEn = Vec((0 until NumBanks).map {
    i => writeEnReg && bankWriteSel1 === UInt(i)
  })

  val banks = Array.fill(NumBanks) {
    Mem(UInt(width = WordSize), BankSize, true)
  }
  val bankReadData2 = Vec.fill(NumBanks) { Reg(UInt(width = WordSize)) }

  for (i <- 0 until NumBanks) {
    val bank = banks(i)
    bankReadData2(i) := bank(bankReadAddr1)
    when (bankWriteEn(i)) {
      bank(bankWriteAddr1) := writeDataReg
    }
  }

  if (DecodeDelay == 1) {
    val readData2 = bankReadData2(bankReadSel2)
    val readData3 = Reg(next = readData2)
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

      val curStage = Vec.fill(stageSize) {
        Reg(UInt(width = WordSize))
      }
      for (i <- 0 until stageSize) {
        val subBank = Vec.tabulate(MaxFanIn) {
          j => lastStage(i * MaxFanIn + j)
        }
        curStage(i) := subBank(curSel)
      }
      bankReadDataStages(stage) = curStage
      bankReadSelStages(stage) = (nextWidth, Reg(next = nextSel))
    }

    val finalStage = bankReadDataStages(DecodeDelay - 1)
    val finalSel = bankReadSelStages(DecodeDelay - 1)._2
    val readDataFin = finalStage(finalSel)
    val readDataReg = Reg(next = readDataFin)
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

  for (i <- 0 until MemReadDelay) {
    poke(c.io.readAddr, addrs(i))
    step(1)
  }

  for (i <- MemReadDelay until NumTests) {
    expect(c.io.readData, values(i - MemReadDelay))
    poke(c.io.readAddr, addrs(i))
    step(1)
  }

  for (i <- (NumTests - MemReadDelay) until NumTests) {
    expect(c.io.readData, values(i))
    step(1)
  }
}

object BankedMemMain {
  def main(args: Array[String]) {
    chiselMain(args, () => Module(new BankedMem(16, 128, 16)),
      (c: BankedMem) => new BankedMemTester(c))
  }
}

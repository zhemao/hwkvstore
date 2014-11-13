package McAccel

import Chisel._
import Chisel.AdvTester._
import McAccel.TestUtils._
import McAccel.Constants._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue
import java.io.BufferedReader
import java.io.InputStreamReader

class OutOfSpaceException extends Exception("No more space in value cache.")

class KeyValueChecker(
    keyInfo: Queue[String],
    keyData: Queue[Char],
    resultInfo: Queue[Int],
    resultData: Queue[Char],
    kvPairs: HashMap[String,String],
    requests: Array[String]) {
  val resultBuffer = new StringBuffer()
  var actualLen = 0
  var misses = 0
  val SendInit = 3
  var sendIndex = SendInit
  var recvIndex = 0

  def sendKey(key: String) {
    keyInfo.enqueue(key)
    for (c <- key) {
      keyData.enqueue(c)
    }
  }

  def finished = (recvIndex == requests.length)

  def start() {
    for (i <- 0 until SendInit) {
      sendKey(requests(i))
    }
  }

  def process() {
    if (actualLen == 0) {
      if (!resultInfo.isEmpty) {
        println(s"Receiving result for ${recvIndex}")
        actualLen = resultInfo.dequeue()
        val key = requests(recvIndex)
        val expectedLen = kvPairs(key).length

        if (actualLen == 0) {
          println(s"Cache miss on key ${recvIndex}: ${key}")
          misses += 1
          recvIndex += 1
        } else if (actualLen != expectedLen) {
          println(s"Wrong size for ${recvIndex}: got ${actualLen}, expected ${expectedLen}")
        }

        if (sendIndex < requests.length) {
          sendKey(requests(sendIndex))
          sendIndex += 1
        }
      }
    } else {
      if (!resultData.isEmpty) {
        resultBuffer.append(resultData.dequeue())
      }
      if (resultBuffer.length == actualLen) {
        val actualVal = resultBuffer.toString
        val expectedVal = kvPairs(requests(recvIndex))
        assert(actualVal == expectedVal, s"Wrong value at index ${recvIndex}")
        actualLen = 0
        recvIndex += 1
        resultBuffer.setLength(0)
      }
    }
  }
}

class PerfTest(
    c: KeyValueStore, kvPairs: HashMap[String,String], requests: Array[String])
      extends AdvTester(c) {
  private def isBusy = peek(c.io.rocc.cmd.ready) == 0

  val Cmd_IHandler = new DecoupledSource(c.io.rocc.cmd,
    (sckt: RoCCCommand, in: TestCmd) => in.inject(this, sckt))
  val Resp_OHandler = new DecoupledSink(c.io.rocc.resp,
    (sckt: RoCCResponse) => TestResp.extract(this, sckt))
  val MemReq_OHandler = new DecoupledSink(c.io.rocc.mem.req,
    (sckt: HellaCacheReq) => TestMemReq.extract(this, sckt))
  val MemResp_IHandler = new ValidSource(c.io.rocc.mem.resp, 
    (sckt: HellaCacheResp, in: TestMemResp) => in.inject(this, sckt))
  val KeyInfo_IHandler = new DecoupledSource(c.io.keyInfo,
    (sckt: MessageInfo, in: String) => {
      reg_poke(sckt.len, in.length)
      reg_poke(sckt.tag, in(0))
    })
  val KeyData_IHandler = new DecoupledSource(c.io.keyData,
    (sckt: UInt, in: Char) => reg_poke(sckt, in))
  val ResultInfo_OHandler = new DecoupledSink(c.io.resultInfo,
    (sckt: MessageInfo) => peek(sckt.len).intValue)
  val ResultData_OHandler = new DecoupledSink(c.io.resultData,
    (sckt: UInt) => peek(sckt).charValue)

  val totalKeySize = requests.map(_.length).sum
  val totalValSize = requests.map(kvPairs(_).length).sum

  println(s"Cumulative size of keys: ${totalKeySize}")
  println(s"Cumulative size of values: ${totalValSize}")

  val kvChecker = new KeyValueChecker(
    KeyInfo_IHandler.inputs, KeyData_IHandler.inputs,
    ResultInfo_OHandler.outputs, ResultData_OHandler.outputs,
    kvPairs, requests)

  val memory = new InstantMemory(64, 64*64*2, 
    MemReq_OHandler.outputs, MemResp_IHandler.inputs)

  def switchMode(write: Boolean) = {
    val writeInt = if (write) 1 else 0
    val inst = TestInst(0, 0, writeInt, 0, false, false, false)
    Cmd_IHandler.inputs.enqueue(TestCmd(inst))
    until (Cmd_IHandler.isIdle && !isBusy, 50) {}
    if (write)
      peek(c.io.writeready) == 1
    else
      peek(c.io.readready) == 1
  }

  var valueAddr = 0

  def assocKey(key: String, value: String): Boolean = {
    val keyWords = messToWords(key, 8)
    val valWords = messToWords(value, 8)
    val valStart = keyWords.length * 8
    memory.store_data(0, keyWords)
    memory.store_data(valStart, valWords)

    val resKey = TestInst(2, 0, 1, 2, true, true, true)
    Cmd_IHandler.inputs.enqueue(TestCmd(resKey, 0, key.length))

    until (Cmd_IHandler.isIdle && !isBusy, 4 * key.length + 50) {
      memory.process()
    }

    var resp = Resp_OHandler.outputs.dequeue()
    var hash = resp.data

    if (hash == HashNotFound) {
      false
    } else if (valueAddr + value.length > c.ValCacheSize) {
      throw new OutOfSpaceException
    } else {
      val assocAddr = TestInst(3, 0, 1, 2, false, true, true)
      val assocLen  = TestInst(4, 0, 1, 2, false, true, true)
      val writeVal  = TestInst(5, 0, 1, 2, false, true, true)

      Cmd_IHandler.inputs.enqueue(TestCmd(assocAddr, hash, valueAddr))
      Cmd_IHandler.inputs.enqueue(TestCmd(assocLen,  hash, value.length))
      Cmd_IHandler.inputs.enqueue(TestCmd(writeVal,  hash, valStart))

      until (Cmd_IHandler.isIdle && !isBusy, 4 * value.length + 50) {
        memory.process()
      }

      valueAddr += value.length
      true
    }
  }

  switchMode(true)

  var keysAdded = 0

  println("Setting keys")
  try {
    for ((key, value) <- kvPairs) {
      if (assocKey(key, value)) {
        keysAdded += 1
      } else {
        println("Failed to add key %s".format(key))
      }
    }
  } catch {
    case e: OutOfSpaceException =>
      println("Ran out of space after %d keys".format(keysAdded))
  }

  switchMode(false)

  val start_cycles = cycles
  println("Feeding data")
  kvChecker.start()
  until (kvChecker.finished, 5000 * requests.length) {
    kvChecker.process()
  }
  val run_cycles = cycles - start_cycles
  println(s"Processed ${requests.length} requests in ${run_cycles} cycles")
  println(s"Missed requests: ${kvChecker.misses}")
}

object PerfTestMain {
  def main(args: Array[String]) {
    val WordSize = 32
    val KeySize = 256
    val NumKeys = 1024
    val ValCacheSize = 512 * 1024
    val TagSize = 5

    val workload = getClass.getResourceAsStream("/workload.txt")
    val reader = new BufferedReader(new InputStreamReader(workload))
    var line = ""
    var finishedPairs = false
    val keyValuePairs = new HashMap[String,String]
    val keyRequests = new ArrayBuffer[String]
    var finishedFile = false

    while (!finishedPairs) {
      line = reader.readLine()
      if (line == null) {
        throw new Exception("Reached end of file before getting to requests")
      }
      line.split(" ") match {
        case Array(key, value) =>
          keyValuePairs(key) = value
        case Array("---") =>
          finishedPairs = true
        case _ => throw new Exception("Malformed file")
      }
    }

    while (!finishedFile) {
      line = reader.readLine()
      if (line == null) {
        finishedFile = true
      } else {
        keyRequests += line
      }
    }

    val requestArray = new Array[String](keyRequests.size)
    keyRequests.copyToArray(requestArray)

    chiselMain.run(args, () => (new KeyValueStore),
      (c: KeyValueStore) => new PerfTest(c, keyValuePairs, requestArray))
  }
}

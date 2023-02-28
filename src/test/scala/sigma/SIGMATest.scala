package sigma

import chisel3._
import chiseltest._
import scala.util.Random
import scala.collection.mutable
import org.scalatest.freespec.AnyFreeSpec

class XBarTest extends AnyFreeSpec with ChiselScalatestTester {
  "XBar should work" in {
    val N = 8
    val rand = new Random()
    for(i <- 0 until 30) {
      test(new XBar(UInt(8.W), N)) { dut =>
        import dut.io._
        val inputData = Seq.fill(N)(rand.nextInt(N))
        val inputValid = Seq.fill(N)(rand.nextInt(2) == 1)
        val destData = Seq.fill(N)(rand.nextInt(N))
        val destValid = Seq.fill(N)(rand.nextInt(2) == 1)

        data zip inputData foreach { x => x._1.bits.poke(x._2.U) }
        data zip inputValid foreach { x => x._1.valid.poke(x._2.B) }
        dest zip destData foreach { x => x._1.bits.poke(x._2.U) }
        dest zip destValid foreach { x => x._1.valid.poke(x._2.B) }
        out.zipWithIndex foreach {
          case (out, idx) =>
            val valid = inputValid(destData(idx)) && destValid(idx)
            out.valid.expect(valid.B)
            if (valid) {
              val data = inputData(destData(idx))
              out.bits.expect(data.U)
            }
        }
      }
    }
  }
}

class FanNetworkTest extends AnyFreeSpec with ChiselScalatestTester {
  def computeOutput[T](inputVec: Seq[T], sameVec: Seq[Boolean])(implicit n: Numeric[T]): Seq[T] = {
    var sum = n.zero
    val buf = mutable.ArrayBuffer[T]()
    for(i <- inputVec.indices) {
      sum = n.plus(sum, inputVec(i))
      if(i >= sameVec.size || !sameVec(i)) {
        buf += sum
        sum = n.zero
      }
    }
    buf.toSeq
  }
  "FanNetwork should work" in {
    val N = 16
    val rand = new Random()
    for(i <- 0 until 30) {
      test(new FanNetwork(UInt(8.W), N)) { dut =>
        import dut.io
        val inputVec = Seq.tabulate(N)(x => x)
        val sameVec = Seq.fill(N - 1)(rand.nextInt(2) == 1)
        val targetVec = computeOutput(inputVec, sameVec)
        io.in_data zip inputVec foreach {
          case (in, v) =>
            in.valid.poke(true.B)
            in.bits.poke(v.U)
        }
        io.in_same zip sameVec foreach { x => x._1.poke(x._2.B) }
        dut.clock.step(dut.latency)
        val outVec = io.out_data.collect {
          case out if out.valid.peekBoolean() =>
            out.bits
        }
        assert(outVec.size == targetVec.size)
        for ((out, target) <- outVec zip targetVec) {
          out.expect(target)
        }
      }
    }
  }
//  "FanNetwork should be correctly generated for Float dtype" in {
//    test(new FanNetwork(Float(8, 8), 16)) { dut =>
//
//    }
//  }
}


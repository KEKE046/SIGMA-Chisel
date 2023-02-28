package sigma

import chisel3._
import chisel3.util._
import java.io.{File, PrintWriter}

class XBar[T <: Data](dtype: T, num: Int) extends Module {
  val io = IO(new Bundle {
    val data = Input(Vec(num, Valid(dtype)))
    val dest = Input(Vec(num, Valid(UInt(log2Ceil(num).W))))
    val out = Output(Vec(num, Valid(dtype)))
  })
  io.out zip io.dest foreach {
    case (out, dst) =>
      when(dst.valid) {
        out := io.data(dst.bits)
      }.otherwise {
        out.valid := 0.B
        out.bits := DontCare
      }
  }
  def latency = 0
}

class PE[T <: Data](inputType: T, outputType: T)(implicit ev: Arithmetic[T]) extends Module {
  import ev._

  val io = IO(new Bundle {
    val in_data = Input(Valid(inputType))
    val in_stationary = Input(Bool())
    val out_data = Valid(outputType)
  })

  private val buf = Reg(inputType)
  private val buf_valid = RegInit(Bool(), 0.B)

  when(io.in_stationary && io.in_data.valid) {
    buf := io.in_data.bits
    buf_valid := true.B
  }

  private val out = Wire(io.out_data.cloneType)
  private val mul_valid = buf_valid && io.in_data.valid
  private val mul_a = Mux(mul_valid, io.in_data.bits, inputType.zero)
  private val mul_b = Mux(mul_valid, buf, inputType.zero)
  private val mul = mul_a.mac(mul_b, outputType.zero)
  out.valid := mul_valid
  out.bits  := mul
  io.out_data := out

  def latency = 0
}

class FanNode[T <: Data](dtype: T)(implicit ev: Arithmetic[T]) extends Module {
  import ev._
  val io = IO(new Bundle {
    val in_l = Input(Valid(dtype))
    val in_r = Input(Valid(dtype))
    val same_l_border = Input(Bool())
    val same_l = Input(Bool())
    val same_m = Input(Bool())
    val same_r = Input(Bool())
    val same_r_border = Input(Bool())
    val left_child = Input(Bool())
    val out_l = Output(Valid(dtype))
    val out_r = Output(Valid(dtype))
    val out_final = Output(Valid(dtype))
    val out_same_l_border = Output(Bool())
    val out_same = Output(Bool())
    val out_same_r_border = Output(Bool())
  })
  io.out_same := io.same_l && io.same_m && io.same_r
  io.out_same_l_border := io.same_l_border
  io.out_same_r_border := io.same_r_border
  private val NoOutput = Wire(Valid(dtype))
  NoOutput.valid := false.B
  NoOutput.bits := DontCare
  io.out_l := NoOutput
  io.out_r := NoOutput
  io.out_final := NoOutput
  private val merge_l = io.same_l_border && io.same_l
  private val merge_r = io.same_r_border && io.same_r
  when(io.same_m) {
    when(io.in_l.valid && io.in_r.valid) {
      val sum = Wire(Valid(dtype))
      sum.valid := true.B
      sum.bits := io.in_l.bits + io.in_r.bits
      when(merge_l && merge_r) {
        when(io.left_child) {
          io.out_r := sum
        } .otherwise {
          io.out_l := sum
        }
      } .elsewhen(merge_l) {
        io.out_l := sum
      } .elsewhen(merge_r) {
        io.out_r := sum
      } .otherwise {
        io.out_final := sum
      }
    }
  } .otherwise {
    when(merge_l) {
      io.out_l := io.in_l
    } .otherwise {
      io.out_final := io.in_l
    }
    when(io.same_r) { io.out_r := io.in_r }
  }
}

class FanNetwork[T <: Data : Arithmetic](dtype: T, num: Int) extends Module {
  val io = IO(new Bundle {
    val in_data = Input(Vec(num, Valid(dtype)))
    val in_same = Input(Vec(num - 1, Bool()))
    val out_data = Output(Vec(num, Valid(dtype)))
  })
  private val nodes = Seq.fill(num - 1)(Module(new FanNode(dtype)))
  private val numLayers = log2Ceil(num)
  private def pow2(p: Int) = math.pow(2, p).toInt
  private def reduceChild(side: Seq[Valid[T]]): Valid[T] = side.reduce[Valid[T]] {
    case (up_valid, cur) =>
      val up = Pipe(up_valid)
      val out = Wire(Valid(dtype))
      out.valid := up.valid || cur.valid
      out.bits := Mux(up.valid, up.bits, cur.bits)
      out
  }
  for(layer <- 0 until numLayers) {
    val step = pow2(layer + 1)
    val start = pow2(layer) - 1
    val indices = start until num by step
    for(idx <- indices) {
      val node = nodes(idx)
      node.io.left_child := ((idx - start) / step % 2 == 0).B
      if(layer == 0) {
        node.io.same_l := true.B
        node.io.same_r := true.B
        node.io.same_m := io.in_same(idx)
        if(idx == 0) node.io.same_l_border := false.B
        else node.io.same_l_border := io.in_same(idx - 1)
        if(idx == num - 2) node.io.same_r_border := false.B
        else node.io.same_r_border := io.in_same(idx + 1)
        node.io.in_l := io.in_data(idx)
        node.io.in_r := io.in_data(idx + 1)
      }
      else {
        val side_l = for (j <- 0 until layer) yield idx - pow2(j)
        val side_r = for (j <- 0 until layer) yield idx + pow2(j)
        val lhs = nodes(side_l.last)
        val rhs = nodes(side_r.last)
        node.io.same_l := RegNext(lhs.io.out_same)
        node.io.same_r := RegNext(rhs.io.out_same)
        node.io.same_l_border := RegNext(lhs.io.out_same_l_border)
        node.io.same_m := RegNext(rhs.io.out_same_l_border)
        node.io.same_r_border := RegNext(rhs.io.out_same_r_border)
        node.io.in_l := Pipe(reduceChild(side_l.map(x => nodes(x).io.out_r)))
        node.io.in_r := Pipe(reduceChild(side_r.map(x => nodes(x).io.out_l)))
      }
      io.out_data(idx) := Pipe(node.io.out_final, numLayers - layer)
    }
  }
  private val side_l = for (j <- 0 until numLayers) yield num - 1 - pow2(j)
  io.out_data(num - 1) := Pipe(reduceChild(side_l.map(x => nodes(x).io.out_r)))
  def latency = numLayers
}

class FlexDPE[T <: Data : Arithmetic](inputType: T, outputType: T, num: Int) extends Module {
  require(num >= 2)
  private val xbar = Module(new XBar(inputType, num))
  private val pe_array = Seq.fill(num)(Module(new PE(inputType, outputType)))
  private val fan = Module(new FanNetwork(outputType, num))
  val io = IO(new Bundle{
    val in_data = Input(xbar.io.data.cloneType)
    val in_dest = Input(xbar.io.dest.cloneType)
    val in_stationary = Input(Bool())
    val in_same = Input(Vec(num - 1, Bool()))
    val out_data = Output(Vec(num, Valid(outputType)))
  })
  xbar.io.data := io.in_data
  xbar.io.dest := io.in_dest
  private val delay_stationary = ShiftRegister(io.in_stationary, xbar.latency + 1)
  pe_array zip xbar.io.out foreach {
    case (pe, out) =>
      pe.io.in_data := Pipe(out)
      pe.io.in_stationary := delay_stationary
  }
  fan.io.in_same := ShiftRegister(io.in_same, xbar.latency + 1 + pe_array.head.latency + 1)
  fan.io.in_data zip pe_array foreach {
    case (in_data, pe) => in_data := Pipe(pe.io.out_data)
  }
  io.out_data zip fan.io.out_data foreach {
    case (out, fan_out) => out := fan_out
  }
  val latency = xbar.latency + 1 + pe_array.head.latency + 1 + fan.latency
}

//object Main extends App {
//  val f = new PrintWriter("gen.fir")
//  f.write(stage.ChiselStage.emitChirrtl(new FlexDPE(UInt(8.W), UInt(8.W), 8)))
//  f.flush()
//  f.close()
//}
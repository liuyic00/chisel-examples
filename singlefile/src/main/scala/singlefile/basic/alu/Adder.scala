package singlefile.basic.alu

import chisel3._

class Adder(width: Int) extends Module {
  val io = IO(new Bundle {
    val valid = Input(Bool())         // n0
    val in1   = Input(UInt(width.W))  // n1
    val in2   = Input(UInt(width.W))  // n2
    val out   = Output(UInt(width.W)) // n3
  })

  when(io.valid) {
    io.out := io.in1 + io.in2
  } otherwise {
    io.out := 0.U
  }

  assert( // Modified
    (io.valid && io.out === io.in1 + io.in2) || (!io.valid && io.out === 0.U)
  )
}

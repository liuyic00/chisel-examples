package singlefile

object ArrayMulDataModuleEmit extends App {
  val width = 64

  import xiangshan.multiplier.ArrayMulDataModule

  import _root_.circt.stage.ChiselStage

  val verilogFile   = new java.io.File("build/verilog/xiangshan", s"ArrayMulDataModule.sv")
  val verilogString = ChiselStage.emitSystemVerilog(new ArrayMulDataModule(width + 1))

  verilogFile.getParentFile.mkdirs()
  val writer = new java.io.PrintWriter(verilogFile)
  try writer.write(verilogString)
  finally writer.close()
}

object Radix2DividerEmit extends App {
  val width = 64

  import xiangshan.radix2divider._

  import _root_.circt.stage.ChiselStage

  val verilogFile   = new java.io.File("build/verilog/xiangshan", s"Radix2Divider.sv")
  val verilogString = ChiselStage.emitSystemVerilog(new Radix2Divider(width))

  verilogFile.getParentFile.mkdirs()
  val writer = new java.io.PrintWriter(verilogFile)
  try writer.write(verilogString)
  finally writer.close()
}

object AdderEmit extends App {
  val width = 8

  import basic.alu.Adder

  import _root_.circt.stage.ChiselStage

  val verilogFile   = new java.io.File("build/verilog/basic/alu", s"Adder.sv")
  val verilogString = ChiselStage.emitSystemVerilog(new Adder(width))

  verilogFile.getParentFile.mkdirs()
  val writer = new java.io.PrintWriter(verilogFile)
  try writer.write(verilogString)
  finally writer.close()
}

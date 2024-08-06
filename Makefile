
EMIT_HEADER:=singlefile/src/main/scala/singlefile/Emit.scala build.sbt
VERILOG_DIR:=$(CURDIR)/build/verilog
VERILATOR_DIR:=$(CURDIR)/build/verilator

# xiangshanMul
name:=xiangshanMul
sv:=$(VERILOG_DIR)/xiangshan/ArrayMulDataModule.sv
gen_dir:=$(VERILATOR_DIR)/xiangshan/ArrayMulDataModule
Vexe:=$(xiangshanMul_gen_dir)/VArrayMulDataModule

$(sv): $(EMIT_HEADER) singlefile/src/main/scala/singlefile/xiangshan/Multiplier.scala
	sbt "singlefile/runMain singlefile.ArrayMulDataModuleEmit 64"

$(Vexe): $(sv) singlefile/src/test/cpp/singlefile/xiangshan/ArrayMulDataModule.cpp
	mkdir -p $(gen_dir)
	verilator --cc --exe --build -j 0 --Mdir $(gen_dir)/cpp --top-module ArrayMulDataModule -o $@ $^

$(name): $(Vexe)

$(name)-run: $(Vexe)
	time ./$<


# xiangshanDiv
name:=xiangshanDiv
package_name:=xiangshan
scala_file_name:=Radix2Divider
module_name:=Radix2Divider

sv:=$(VERILOG_DIR)/$(package_name)/$(module_name).sv
gen_dir:=$(VERILATOR_DIR)/$(package_name)/$(module_name)
Vexe:=$(gen_dir)/V$(module_name)

$(sv): $(EMIT_HEADER) singlefile/src/main/scala/singlefile/$(package_name)/$(scala_file_name).scala
	sbt "singlefile/runMain singlefile.$(module_name)Emit"
$(Vexe): $(sv) singlefile/src/test/cpp/singlefile/$(package_name)/$(module_name).cpp
	mkdir -p $(gen_dir)
	verilator --cc --exe --build -j 0 --Mdir $(gen_dir)/cpp --top-module $(module_name) -o $@ $^
$(name): $(Vexe)
$(name)-run: $(Vexe)
	time ./$<


# adder
name:=adder
package_name:=basic/alu
scala_file_name:=Adder
module_name:=Adder

sv:=$(VERILOG_DIR)/$(package_name)/$(module_name).sv
gen_dir:=$(VERILATOR_DIR)/$(package_name)/$(module_name)
Vexe:=$(gen_dir)/V$(module_name)

$(sv): $(EMIT_HEADER) singlefile/src/main/scala/singlefile/$(package_name)/$(scala_file_name).scala
	sbt "singlefile/runMain singlefile.$(module_name)Emit"
$(Vexe): $(sv) $(CURDIR)/singlefile/src/test/cpp/singlefile/$(package_name)/$(module_name).cpp
	mkdir -p $(gen_dir)
	verilator --cc --exe --build -j 0 --Mdir $(gen_dir)/cpp --top-module $(module_name) -o $@ $^
$(name): $(Vexe)
$(name)-run: $(Vexe)
	time ./$<

clear:
	rm -rf build
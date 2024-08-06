
EMIT_HEADER:=./singlefile/src/main/scala/singlefile/Emit.scala ./build.sbt
VERILOG_DIR:=$(CURDIR)/build/verilog
VERILATOR_DIR:=$(CURDIR)/build/verilator

define create_target
name_$(1):=$(1)
package_name_$(1):=$(2)
scala_file_name_$(1):=$(3)
module_name_$(1):=$(4)

sv_$(1):=$$(VERILOG_DIR)/$$(package_name_$(1))/$$(module_name_$(1)).sv
gen_dir_$(1):=$$(VERILATOR_DIR)/$$(package_name_$(1))/$$(module_name_$(1))
Vexe_$(1):=$$(gen_dir_$(1))/V$$(module_name_$(1))

$$(sv_$(1)): $$(EMIT_HEADER) singlefile/src/main/scala/singlefile/$$(package_name_$(1))/$$(scala_file_name_$(1)).scala
	time sbt "singlefile/runMain singlefile.$$(module_name_$(1))Emit"
$$(Vexe_$(1)): $$(sv_$(1)) $$(CURDIR)/singlefile/src/test/cpp/singlefile/$$(package_name_$(1))/$$(module_name_$(1)).cpp
	mkdir -p $$(gen_dir_$(1))
	time verilator --cc --exe --build -j 0 --trace --Mdir $$(gen_dir_$(1))/cpp --top-module $$(module_name_$(1)) -o $$@ $$^
$$(name_$(1)): $$(Vexe_$(1))
$$(name_$(1))-run: $$(Vexe_$(1))
	time $$<
endef

$(eval $(call create_target,adder,basic/alu,Adder,Adder))
$(eval $(call create_target,xiangshanDiv,xiangshan,Radix2Divider,Radix2Divider))
$(eval $(call create_target,xiangshanMul,xiangshan,Multiplier,ArrayMulDataModule))

clear:
	rm -rf build
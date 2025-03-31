MOD ?= dft.OCC

init:
	git submodule update --init
	cd rocket-chip/dependencies && git submodule update --init cde hardfloat diplomacy

idea:
	mill -i mill.idea.GenIdea/idea

rtl:
	@mkdir -p build
	mill -i xsutils.test.runMain TestTop --full-stacktrace -td build --target systemverilog --split-verilog --module $(MOD) | tee build/make.log

clean:
	@rm -rf build/*

.PHONY:init idea clean rtl
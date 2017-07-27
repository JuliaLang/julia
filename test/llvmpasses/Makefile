SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/../..)
include $(JULIAHOME)/deps/Versions.make
include $(JULIAHOME)/Make.inc
include $(JULIAHOME)/deps/llvm-ver.make
BUILDDIR = $(JULIAHOME)/deps/scratch
include $(JULIAHOME)/deps/llvm-options.mk
LLVM_SRC_DIR:=$(JULIAHOME)/deps/srccache/llvm-$(LLVM_VER)
test:
	PATH=$$PATH:$(LLVM_BUILDDIR_withtype)/bin $(LLVM_SRC_DIR)/utils/lit/lit.py -v .
.PHONY: test

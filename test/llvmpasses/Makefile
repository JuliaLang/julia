SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/../..)
include $(JULIAHOME)/deps/Versions.make
include $(JULIAHOME)/Make.inc
include $(JULIAHOME)/deps/llvm-ver.make
LLVM_SRC_DIR:=$(JULIAHOME)/deps/srccache/llvm-$(LLVM_VER)
LLVM_BUILD_DIR:=$(BUILDDIR)/llvm-$(LLVM_VER)
test:
	PATH=$$PATH:$(LLVM_BUILD_DIR)/bin $(LLVM_SRC_DIR)/utils/lit/lit.py -v .
.PHONY: test

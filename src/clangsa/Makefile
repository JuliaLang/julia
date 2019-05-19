SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/../..)
BUILDDIR := .
include $(JULIAHOME)/Make.inc

$(build_shlibdir)/libGCCheckerPlugin.$(SHLIB_EXT): $(SRCDIR)/GCChecker.cpp
		@$(call PRINT_CC, $(CXX) -g -shared -fno-rtti -fPIC -o $@ -std=c++11 -DCLANG_PLUGIN -I$(build_includedir)  -I$(LLVM_SRC_DIR)/include -L$(build_libdir) $< -lclangAnalysis -lclangStaticAnalyzerCore -lclangASTMatchers -lclangAST -lclangLex -lclangBasic)

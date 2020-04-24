SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/../..)
include $(JULIAHOME)/Make.inc

check: $(SRCDIR)

TESTS = $(patsubst $(SRCDIR)/%,%,$(wildcard $(SRCDIR)/*.jl $(SRCDIR)/*.ll))

# We always include the build directories as we need to link against libjulia
LLVM_ENV_ADJUST = $(call env_adjust,$(build_depsbindir),$(build_shlibdir))
ifeq ($(USE_BINARYBUILDER_LLVM),1)
  LLVM_TOOLS_DIR = $(LLVM_jll_DIR)/tools

  # Add on the JLL directories
  LLVM_ENV_ADJUST += $(call jll_env_adjust,LLVM_jll libLLVM_jll CompilerSupportLibraries_jll)
else
  LLVM_TOOLS_DIR = $(build_depsbindir)
endif

llvmpasses-deps:
	$(MAKE) -C $(JULIAHOME)/deps install-LLVM_jll

$(SRCDIR) $(TESTS): llvmpasses-deps
	$(LLVM_ENV_ADJUST) $(PYTHON) $(call python_cygpath,$(LLVM_TOOLS_DIR)/lit/lit.py) -v $(call python_cygpath,$@)

.PHONY: $(TESTS) $(SRCDIR) check all

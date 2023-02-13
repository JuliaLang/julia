SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/../..)
include $(JULIAHOME)/Make.inc

check: .

TESTS_ll := $(filter-out update-%,$(patsubst $(SRCDIR)/%,%,$(wildcard $(SRCDIR)/*.ll)))
TESTS_jl := $(patsubst $(SRCDIR)/%,%,$(wildcard $(SRCDIR)/*.jl))
TESTS := $(TESTS_ll) $(TESTS_jl)

. $(TESTS):
	PATH=$(build_bindir):$(build_depsbindir):$$PATH \
	LD_LIBRARY_PATH=${build_libdir}:$$LD_LIBRARY_PATH \
	$(build_depsbindir)/lit/lit.py -v "$(addprefix $(SRCDIR)/,$@)"

$(addprefix update-,$(TESTS_ll)):
	@echo 'NOTE: This requires a llvm source files locally, such as via `make -C deps USE_BINARYBUILDER_LLVM=0 DEPS_GIT=llvm checkout-llvm`'
	@read -p "$$(printf $(WARNCOLOR)'This will directly modify %s, are you sure you want to proceed? '$(ENDCOLOR) '$@')" REPLY && [ yy = "y$$REPLY" ]
	sed -e 's/%shlibext/.$(SHLIB_EXT)/g' < "$(@:update-%=$(SRCDIR)/%)" > "$@"
	PATH=$(build_bindir):$(build_depsbindir):$$PATH \
	LD_LIBRARY_PATH=${build_libdir}:$$LD_LIBRARY_PATH \
	$(JULIAHOME)/deps/srccache/llvm/llvm/utils/update_test_checks.py "$@" \
	--preserve-names
	mv "$@" "$(@:update-%=$(SRCDIR)/%)"

update-help:
	PATH=$(build_bindir):$(build_depsbindir):$$PATH \
	LD_LIBRARY_PATH=${build_libdir}:$$LD_LIBRARY_PATH \
	$(JULIAHOME)/deps/srccache/llvm/llvm/utils/update_test_checks.py \
	--help

.PHONY: $(TESTS) $(addprefix update-,$(TESTS_ll)) check all .

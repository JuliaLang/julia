# Tests for the assembly in src/arch and cli/arch (the runtime's assembly is
# built by src/Makefile, the loader's by cli/Makefile). Run with
#
#     make -C src/arch test
#
# Each test links one assembly file into a small C driver. For another
# architecture or operating system, cross-compile and run the tests under an
# emulator, e.g.
#
#     make -C src/arch test XC_HOST=aarch64-linux-gnu OS=Linux \
#         TEST_RUNNER="qemu-aarch64 -L /usr/aarch64-linux-gnu"
#     make -C src/arch test XC_HOST=x86_64-w64-mingw32 TEST_RUNNER=wine
#
# (for wine, initialize the prefix with `wineboot -i` first: 32-bit tests need
# its 32-bit half)

SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/../..)
BUILDDIR := .
include $(JULIAHOME)/Make.inc

ASM_ARCH := $(if $(filter arm%,$(ARCH)),arm,$(ARCH))
TEST_RUNNER ?=
TEST_CFLAGS := -O1 -g -fno-omit-frame-pointer -U_FORTIFY_SOURCE -I$(JULIAHOME)/src -I$(JULIAHOME)/src/support
# (the tests pass vectors by value, whose ABI some compilers warn about)
TEST_CFLAGS += -Wno-psabi
ifneq ($(filter arm%,$(ARCH)),)
# ARM only has unwind tables (which the backtraces need) when asked for
TEST_CFLAGS += -funwind-tables
endif
ifeq ($(OS),WINNT)
TEST_CFLAGS += -static
else ifneq ($(OS),Darwin)
# as for the libraries (JLIBLDFLAGS): the assembly has no .note.GNU-stack
TEST_CFLAGS += -Wl,-z,noexecstack
endif

# The tests of unwinding also unwind with the libunwind Julia's profiler uses
# (LLVM's on macOS), installed for the target by deps/ as for the build (with
# the zlib it needs on Linux), into a prefix of their own.
ifneq ($(filter Linux Darwin,$(OS)),)
UNWIND_DEP := $(if $(filter Darwin,$(OS)),llvmunwind,unwind)
UNWIND_PREFIX := $(abspath $(BUILDDIR))/libunwind-$(BB_TRIPLET)
STEP_DEPS := $(UNWIND_PREFIX)/manifest/$(UNWIND_DEP)
STEP_FLAGS := -DARCHTEST_LIBUNWIND -idirafter $(UNWIND_PREFIX)/include \
	-L$(UNWIND_PREFIX)/lib -Wl,-rpath,$(UNWIND_PREFIX)/lib -lunwind
endif

# test name -> the assembly it tests (an architecture without that file only
# builds the test's skip path), and the headers it includes
start_fiber_ASM := $(SRCDIR)/$(ASM_ARCH)/start_fiber.S
start_fiber_HDRS := $(SRCDIR)/cfi_noreturn.h $(SRCDIR)/test/archstep.h
start_fiber_FLAGS := $(STEP_FLAGS)
start_fiber_DEPS := $(STEP_DEPS)
unix_signal_call_trampoline_ASM := $(SRCDIR)/$(ASM_ARCH)/unix_signal_call_trampoline.S
unix_signal_call_trampoline_HDRS := $(SRCDIR)/test/archstep.h
unix_signal_call_trampoline_FLAGS := $(STEP_FLAGS)
unix_signal_call_trampoline_DEPS := $(STEP_DEPS)
win_setjmp_ASM := $(SRCDIR)/$(ASM_ARCH)/win_setjmp.S
win_restore_trigger_ASM := $(SRCDIR)/$(ASM_ARCH)/win_restore_trigger.S
macos_signal_ASM := $(SRCDIR)/$(ASM_ARCH)/macos_call_frame_unwind.S $(SRCDIR)/$(ASM_ARCH)/macos_restore_trigger.S
macos_signal_FLAGS := $(STEP_FLAGS)
macos_signal_DEPS := $(STEP_DEPS)
trampolines_ASM := $(JULIAHOME)/cli/arch/$(ASM_ARCH)/trampolines.S
plt_resolver_ASM := $(wildcard $(SRCDIR)/$(ASM_ARCH)/*plt_resolver.S) $(SRCDIR)/test/plt_trampolines.S
# (the large code model's stubs have absolute addresses in code)
plt_resolver_FLAGS := -I$(BUILDDIR) $(if $(filter Darwin WINNT,$(OS)),,-no-pie)
# libraries a test links against (the trampolines forward into another library,
# as libjulia does into libjulia-internal)
OTHER_LIB := $(BUILDDIR)/libarchtest_other.$(SHLIB_EXT)
trampolines_LIBS := $(OTHER_LIB)
trampolines_HDRS := $(SRCDIR)/test/archstep.h
trampolines_FLAGS := $(STEP_FLAGS)
trampolines_DEPS := $(STEP_DEPS)
ifeq ($(OS),Darwin)
TEST_LIB_FLAGS := -L$(BUILDDIR) -Wl,-rpath,@loader_path
else ifneq ($(OS),WINNT)
TEST_LIB_FLAGS := -L$(BUILDDIR) -Wl,-rpath,'$$ORIGIN'
else
TEST_LIB_FLAGS := -L$(BUILDDIR)
endif

ifeq ($(OS),WINNT)
TESTS := win_setjmp win_restore_trigger trampolines plt_resolver
else ifeq ($(OS),Darwin)
TESTS := start_fiber macos_signal trampolines plt_resolver
else
TESTS := start_fiber unix_signal_call_trampoline trampolines plt_resolver
endif
TEST_BINS := $(TESTS:%=$(BUILDDIR)/test_%$(EXE))
# The tests of unwinding from every instruction also run with the C code built
# without frame pointers, which leaves only the unwind info to find its callers
# (not on ARM, where they check only frame pointers).
ifeq ($(OS),Linux)
NOFP_TESTS := $(if $(filter arm%,$(ARCH)),,$(filter start_fiber unix_signal_call_trampoline trampolines,$(TESTS)))
endif
TEST_BINS += $(NOFP_TESTS:%=$(BUILDDIR)/test_%_nofp$(EXE))
# On ARM, C code may be ARM or Thumb code (the BinaryBuilder toolchains build
# ARM code, some distribution compilers Thumb code by default), and the assembly
# must work with either: build each test both ways.
ifneq ($(filter arm%,$(ARCH)),)
TEST_MODE_CFLAGS := -marm
TEST_BINS += $(TESTS:%=$(BUILDDIR)/test_%_thumb$(EXE))
endif

ifneq ($(STEP_DEPS),)
$(STEP_DEPS):
	@$(MAKE) -C $(JULIAHOME)/deps install-$(UNWIND_DEP) $(if $(filter Linux,$(OS)),install-zlib) \
		build_prefix=$(UNWIND_PREFIX) $(if $(XC_HOST),XC_HOST=$(XC_HOST) OS=$(OS))
endif

$(OTHER_LIB): $(SRCDIR)/test/trampolines_other_lib.c
	@$(call PRINT_CC, $(CC) $(TEST_CFLAGS) -shared -fPIC $< -o $@)

.SECONDEXPANSION:
$(BUILDDIR)/test_%_nofp$(EXE): $(SRCDIR)/test/%.c $(SRCDIR)/test/archtest.h $(SRCDIR)/common.h $$($$*_HDRS) $$(wildcard $$($$*_ASM)) $$($$*_LIBS) $$($$*_DEPS)
	@$(call PRINT_CC, $(CC) $(TEST_CFLAGS) $(TEST_MODE_CFLAGS) -fomit-frame-pointer -DARCHTEST_NO_FP $< $(wildcard $($*_ASM)) $($*_LIBS) $(if $($*_LIBS),$(TEST_LIB_FLAGS)) $($*_FLAGS) -o $@)
$(BUILDDIR)/test_%_thumb$(EXE): $(SRCDIR)/test/%.c $(SRCDIR)/test/archtest.h $(SRCDIR)/common.h $$($$*_HDRS) $$(wildcard $$($$*_ASM)) $$($$*_LIBS) $$($$*_DEPS)
	@$(call PRINT_CC, $(CC) $(TEST_CFLAGS) -mthumb $< $(wildcard $($*_ASM)) $($*_LIBS) $(if $($*_LIBS),$(TEST_LIB_FLAGS)) $($*_FLAGS) -o $@)
$(BUILDDIR)/test_%$(EXE): $(SRCDIR)/test/%.c $(SRCDIR)/test/archtest.h $(SRCDIR)/common.h $$($$*_HDRS) $$(wildcard $$($$*_ASM)) $$($$*_LIBS) $$($$*_DEPS)
	@$(call PRINT_CC, $(CC) $(TEST_CFLAGS) $(TEST_MODE_CFLAGS) $< $(wildcard $($*_ASM)) $($*_LIBS) $(if $($*_LIBS),$(TEST_LIB_FLAGS)) $($*_FLAGS) -o $@)

# The instructions of the plt_resolver test's trampolines are the templates that
# codegen emits, as assembler macros.
$(BUILDDIR)/plt_stubs.S: $(SRCDIR)/test/plt_stubs.sed $(SRCDIR)/$(ASM_ARCH)/plt_stubs.inc
	sed -f $^ > $@
$(BUILDDIR)/test_plt_resolver$(EXE) $(BUILDDIR)/test_plt_resolver_thumb$(EXE): $(BUILDDIR)/plt_stubs.S

# A test passes if it exits successfully and reports it (a crash under an
# emulator does not always show in the exit status).
test: $(TEST_BINS)
	@for t in $(TEST_BINS); do \
		echo "  RUN     $$t"; \
		out=$$($(TEST_RUNNER) $$t); st=$$?; echo "$$out"; \
		[ $$st = 0 ] && echo "$$out" | grep -qE ': (ok|skipped)' || { echo "  FAILED  $$t"; exit 1; }; \
	done

clean:
	rm -f $(TEST_BINS) $(OTHER_LIB) $(BUILDDIR)/plt_stubs.S
	rm -rf $(BUILDDIR)/libunwind-*

.PHONY: test clean

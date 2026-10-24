# Tests for the assembly in src/arch and cli/arch (the runtime's assembly is
# built by src/Makefile, the loader's by cli/Makefile). Run with
#
#     make -C src/arch test
#
# Each test links one assembly file into a small C driver. For another
# architecture or operating system, cross-compile and run the tests under an
# emulator, e.g.
#
#     make -C src/arch test ARCH=aarch64 CC=aarch64-linux-gnu-gcc \
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

# test name -> the assembly it tests (an architecture without that file only
# builds the test's skip path)
start_fiber_ASM := $(SRCDIR)/$(ASM_ARCH)/start_fiber.S
unix_signal_call_trampoline_ASM := $(SRCDIR)/$(ASM_ARCH)/unix_signal_call_trampoline.S
win_setjmp_ASM := $(SRCDIR)/$(ASM_ARCH)/win_setjmp.S
win_restore_trigger_ASM := $(SRCDIR)/$(ASM_ARCH)/win_restore_trigger.S
macos_signal_ASM := $(SRCDIR)/$(ASM_ARCH)/macos_call_frame_unwind.S $(SRCDIR)/$(ASM_ARCH)/macos_restore_trigger.S
trampolines_ASM := $(JULIAHOME)/cli/arch/$(ASM_ARCH)/trampolines.S
# libraries a test links against (the trampolines forward into another library,
# as libjulia does into libjulia-internal)
OTHER_LIB := $(BUILDDIR)/libarchtest_other.$(SHLIB_EXT)
trampolines_LIBS := $(OTHER_LIB)
ifeq ($(OS),Darwin)
TEST_LIB_FLAGS := -L$(BUILDDIR) -Wl,-rpath,@loader_path
else ifneq ($(OS),WINNT)
TEST_LIB_FLAGS := -L$(BUILDDIR) -Wl,-rpath,'$$ORIGIN'
else
TEST_LIB_FLAGS := -L$(BUILDDIR)
endif

ifeq ($(OS),WINNT)
TESTS := win_setjmp win_restore_trigger trampolines
else ifeq ($(OS),Darwin)
TESTS := start_fiber macos_signal trampolines
else
TESTS := start_fiber unix_signal_call_trampoline trampolines
endif
TEST_BINS := $(TESTS:%=$(BUILDDIR)/test_%$(EXE))
# On ARM, C code may be ARM or Thumb code (the BinaryBuilder toolchains build
# ARM code, some distribution compilers Thumb code by default), and the assembly
# must work with either: build each test both ways.
ifneq ($(filter arm%,$(ARCH)),)
TEST_MODE_CFLAGS := -marm
TEST_BINS += $(TESTS:%=$(BUILDDIR)/test_%_thumb$(EXE))
endif

$(OTHER_LIB): $(SRCDIR)/test/trampolines_other_lib.c
	@$(call PRINT_CC, $(CC) $(TEST_CFLAGS) -shared -fPIC $< -o $@)

.SECONDEXPANSION:
$(BUILDDIR)/test_%_thumb$(EXE): $(SRCDIR)/test/%.c $(SRCDIR)/test/archtest.h $(SRCDIR)/common.h $$(wildcard $$($$*_ASM)) $$($$*_LIBS)
	@$(call PRINT_CC, $(CC) $(TEST_CFLAGS) -mthumb $< $(wildcard $($*_ASM)) $($*_LIBS) $(if $($*_LIBS),$(TEST_LIB_FLAGS)) -o $@)
$(BUILDDIR)/test_%$(EXE): $(SRCDIR)/test/%.c $(SRCDIR)/test/archtest.h $(SRCDIR)/common.h $$(wildcard $$($$*_ASM)) $$($$*_LIBS)
	@$(call PRINT_CC, $(CC) $(TEST_CFLAGS) $(TEST_MODE_CFLAGS) $< $(wildcard $($*_ASM)) $($*_LIBS) $(if $($*_LIBS),$(TEST_LIB_FLAGS)) -o $@)

# A test passes if it exits successfully and reports it (a crash under an
# emulator does not always show in the exit status).
test: $(TEST_BINS)
	@for t in $(TEST_BINS); do \
		echo "  RUN     $$t"; \
		out=$$($(TEST_RUNNER) $$t); st=$$?; echo "$$out"; \
		[ $$st = 0 ] && echo "$$out" | grep -qE ': (ok|skipped)' || { echo "  FAILED  $$t"; exit 1; }; \
	done

clean:
	rm -f $(TEST_BINS) $(OTHER_LIB)

.PHONY: test clean

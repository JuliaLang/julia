JULIAHOME = $(abspath ..)
include ../Make.inc

TESTS = all linalg $(filter-out TestHelpers runtests testdefs,$(subst .jl,,$(wildcard *.jl linalg/*.jl)))

default: all

$(TESTS):
	@$(call PRINT_JULIA, $(call spawn,$(JULIA_EXECUTABLE)) --check-bounds=yes --startup-file=no ./runtests.jl $@)

perf:
	@$(MAKE) -C perf all

clean:
	@$(MAKE) -C perf $@

.PHONY: $(TESTS) perf clean
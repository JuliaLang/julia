SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/../..)
include $(JULIAHOME)/Make.inc
test:
	PATH=$$PATH:$(build_depsbindir) $(build_depsbindir)/lit/lit.py -v $(SRCDIR)
.PHONY: test

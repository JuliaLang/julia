## this is a simple wrapper just to forward on known commands to the
## embedding example with values pulled from Make.inc
SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/..)
BUILDDIR := .
include $(JULIAHOME)/Make.inc

release: # default target
# forward all variables expected by the embedding example
JULIA:=$(call spawn,$(JULIA_EXECUTABLE))
BIN:=$(BUILDDIR)/embedding
CC:=$(CC)
include $(SRCDIR)/embedding/Makefile

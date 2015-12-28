JULIAHOME := $(abspath ../../..)
include $(JULIAHOME)/Make.inc

ifndef PYTHONBIN
	PYTHONBIN = python
endif

all: python julia

python:
	$(PYTHONBIN) lucompletepiv.py

julia:
	@$(call spawn,$(JULIA_EXECUTABLE)) lucompletepiv.jl

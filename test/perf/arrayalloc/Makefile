JULIAHOME = $(abspath ../../..)
include $(JULIAHOME)/Make.inc

ifndef PYTHONBIN
	PYTHONBIN = python
endif

python:
	$(PYTHONBIN) lucompletepiv.py

julia:
	@$(call spawn,$(JULIA_EXECUTABLE)) lucompletepiv.jl

all: python julia

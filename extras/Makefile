JULIAHOME = $(abspath ..)
include $(JULIAHOME)/Make.inc

all: webrepl_msgtypes_h.jl

webrepl_msgtypes_h.jl: ../ui/webserver/message_types.h
	$(QUIET_PERL) $(CC) -E -Dnotdefined $^ > $@

clean:
	rm -f webrepl_msgtypes_h.jl

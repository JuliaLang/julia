JULIAHOME = ..
include ../Make.inc

default: core numbers strings corelib hashing arrayops sparse lapack fft arpack random
extra: unicode
all: default extra

core numbers strings corelib hashing arrayops sparse lapack fft arpack random:
	$(QUIET_JULIA) ../julia $@.j

unicode:
	$(QUIET_JULIA)
	@$(MAKE) -C $@
	@../julia $@.j

clean:
	@$(MAKE) -C unicode $@

.PHONY: all core hashing arrayops unicode clean

all: quick core hashing unicode perf

quick core hashing perf:
	../julia $@.j

unicode:
	$(MAKE) -C unicode
	../julia unicode.j

.PHONY: all quick core hashing unicode perf

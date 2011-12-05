all: quick core hashing unicode perf arrayops

quick core hashing perf arrayops:
	../julia $@.j

unicode:
	$(MAKE) -C unicode
	../julia unicode.j

.PHONY: all quick core hashing unicode perf

all: quick core hashing arrayops unicode perf

quick core hashing arrayops perf:
	../julia $@.j

unicode:
	@$(MAKE) -C $@ -s
	../julia $@.j

.PHONY: all quick core hashing arrayops unicode perf

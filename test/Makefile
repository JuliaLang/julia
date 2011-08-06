testall: test test-utf8 test-perf

unittests:
	../julia unittests.j

test:
	../julia tests.j

test-utf8:
	make -C unicode
	../julia test_utf8.j

test-perf:
	../julia perf.j

testall: test test-utf8 test-perf

quicktests:
	../julia quicktests.j

test:
	../julia tests.j

test-utf8:
	make -C unicode
	../julia test_utf8.j

test-perf:
	../julia perf.j

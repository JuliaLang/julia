OS=$(shell uname)

all:
	$(MAKE) -f Makefile.$(OS)

debug:
	$(MAKE) -f Makefile.$(OS) debug

release:
	$(MAKE) -f Makefile.$(OS) release

clean:
	$(MAKE) -f Makefile.$(OS) clean
	rm -f *~ *#
	rm -f lib/*.do lib/*.o lib/*.a

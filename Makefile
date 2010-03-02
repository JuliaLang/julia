OS=$(shell uname)

all:
	$(MAKE) -f Makefile.$(OS)

clean:
	$(MAKE) -f Makefile.$(OS) clean
	rm -f *~ *#
	rm -f lib/*.do lib/*.o lib/*.a

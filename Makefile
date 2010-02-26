OS=$(shell uname)

all:
	make -f Makefile.$(OS)

clean:
	make -f Makefile.$(OS) clean
	rm -f *~ *#
	rm -f lib/*.do lib/*.o lib/*.a

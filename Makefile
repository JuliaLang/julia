all:
	make -f Makefile.$(shell uname)

clean:
	rm -f julia *~ *# *.o *.a lib/*.o lib/*.a


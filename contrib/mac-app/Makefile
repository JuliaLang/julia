# Make sure that you are building openblas with OPENBLAS_DYNAMIC_ARCH=1
# You may have to wipe your openblas build to ensure that it is built
# with support for all architectures, or else performance may suffer.

all: clean copyfiles rewritepaths

copyfiles:
	make -C ../.. dist
	tar zxf ../../julia-*.tar.gz 
	mv julia-* julia
	cp -a ~/.julia/Cairo/deps/usr/* ./julia/
	cp -a ~/.julia/Tk/deps/usr/* ./julia/
	rm -f julia/lib/*.{a,la}
	chmod a+w julia/lib/*.dylib

rewritepaths:
	(for lib in julia/lib/*.dylib; do ./run-install-name-tool-change.sh $$lib $$HOME/.julia/Cairo/deps/usr/lib/ lib/ change; done)
	(for lib in julia/lib/*.dylib; do ./run-install-name-tool-change.sh $$lib $$HOME/.julia/Tk/deps/usr/lib/ lib/ change; done)
	(cd julia/lib && for lib in *.dylib; do install_name_tool -id @executable_path/../lib/$$lib $$lib; done)

clean:
	rm -fr julia *.app julia.dmgCreator_build *.dmg
	mkdir julia.dmgCreator_build

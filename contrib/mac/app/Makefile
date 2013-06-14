# Make sure that you are building openblas with OPENBLAS_DYNAMIC_ARCH=1
# You may have to wipe your openblas build to ensure that it is built
# with support for all architectures, or else performance may suffer.

JULIAHOME = $(abspath ../../..)
include ../../../Make.inc

all: clean dmg rewritepaths

dmg:
	make -C ../../../deps install-git
	make -C ../../.. dist
	tar zxf ../../../julia-*.tar.gz
	mv julia-* julia
	-mkdir -p ./julia/libexec ./julia/share
	-cp -a $(BUILD)/libexec/git* ./julia/libexec
	-cp -a $(BUILD)/share/git* ./julia/share
	cp -a ~/.julia/Cairo/deps/usr/* ./julia/
	cp -a ~/.julia/Tk/deps/usr/* ./julia/
	rm -f julia/lib/*.{a,la}
	chmod a+w julia/lib/*.dylib
	-mkdir dmg
	platypus -a Julia -p /bin/bash -V $(JULIA_VERSION) -u "The Julia Project" -i julia.icns -Q julia.icns -o "None" -I org.julialang -x -f julia script ./dmg/Julia-$(JULIA_VERSION)-pre-$(JULIA_COMMIT).app
	-cp -f Julia.icns dmg/.VolumeIcon.icns
	-ln -fs /Applications ./dmg/Applications
	hdiutil create Julia-$(JULIA_VERSION)-pre-$(JULIA_COMMIT).dmg -ov -volname "Julia" -imagekey zlib-level=9 -srcfolder dmg

rewritepaths:
	(for lib in julia/lib/*.dylib; do ./run-install-name-tool-change.sh $$lib $$HOME/.julia/Cairo/deps/usr/lib/ lib/ change; done)
	(for lib in julia/lib/*.dylib; do ./run-install-name-tool-change.sh $$lib $$HOME/.julia/Tk/deps/usr/lib/ lib/ change; done)
	(cd julia/lib && for lib in *.dylib; do install_name_tool -id @executable_path/../lib/$$lib $$lib; done)

clean:
	rm -fr julia dmg *.dmg

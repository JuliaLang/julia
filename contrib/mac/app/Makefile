# Make sure that you are building openblas with OPENBLAS_DYNAMIC_ARCH=1
# You may have to wipe your openblas build to ensure that it is built
# with support for all architectures, or else performance may suffer.

JULIAHOME = $(abspath ../../..)
include ../../../Make.inc

JULIA_PKGDIR=$(shell echo $$JULIA_PKGDIR)
ifeq ($(JULIA_PKGDIR),)
	JULIA_PKGDIR=$(shell echo ~)/.julia
endif

VERSION_SUFFIX=$(shell [ $$(git describe --tags --exact-match 2>/dev/null) ] && echo $(JULIA_VERSION) || echo $(JULIA_VERSION)-$(JULIA_COMMIT))

all: clean dmg

dmg:
	make -C ../../../deps install-git
	make -C ../../.. dist
	tar zxf ../../../julia-*.tar.gz
	mv julia-* julia
	-mkdir -p ./julia/libexec ./julia/share
	-cp -a $(build_libexecdir)/git* ./julia/libexec
	-cp -a $(build_datarootdir)/git* ./julia/share
	rm -f julia/lib/*.{a,la}
	-mkdir dmg
	platypus -a Julia -p /bin/bash -V $(JULIA_VERSION) -R -u "The Julia Project" -i julia.icns -Q julia.icns -o "None" -I org.julialang -x -f julia script ./dmg/Julia-$(VERSION_SUFFIX).app
	-codesign -s "AFB379C0B4CBD9DB9A762797FC2AB5460A2B0DBE" --deep ./dmg/Julia-$(VERSION_SUFFIX).app
	-cp -f Julia.icns dmg/.VolumeIcon.icns
	-ln -fs /Applications ./dmg/Applications
	-chmod 755 ./dmg/Julia-$(VERSION_SUFFIX).app/Contents/MacOS/Julia
	@echo "We're going to chown the .app file to root:admin now, which requires sudo.  You may be asked for your password:"
	sudo chown -R root:admin ./dmg/Julia-$(VERSION_SUFFIX).app
	sudo hdiutil create Julia-$(VERSION_SUFFIX).dmg -size 200m -ov -volname "Julia" -imagekey zlib-level=9 -srcfolder dmg


clean:
	@echo "We have to use sudo here to clean out folders owned by root.  You may be asked for your password"
	sudo rm -fr julia dmg *.dmg

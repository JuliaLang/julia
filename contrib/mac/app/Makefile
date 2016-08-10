# Make sure that you are building openblas with OPENBLAS_DYNAMIC_ARCH=1
# You may have to wipe your openblas build to ensure that it is built
# with support for all architectures, or else performance may suffer.

JULIAHOME := $(abspath ../../..)
include ../../../Make.inc

JULIA_PKGDIR:=$(shell echo $$JULIA_PKGDIR)
ifeq ($(JULIA_PKGDIR),)
	JULIA_PKGDIR:=$(shell echo ~)/.julia
endif

# X.Y.Z or X.Y.Z-hash
DMG_VERSION_SUFFIX:=$(shell [ $$(git describe --tags --exact-match 2>/dev/null) ] && echo $(JULIA_VERSION) || echo $(JULIA_VERSION)-$(JULIA_COMMIT))

# X.Y
APP_VERSION_SUFFIX:=$(shell echo $(JULIA_VERSION) | grep -o '^[0-9]\+.[0-9]\+')

DMG_NAME:=Julia-$(DMG_VERSION_SUFFIX).dmg
APP_NAME:=Julia-$(APP_VERSION_SUFFIX).app
VOL_NAME:=Julia-$(DMG_VERSION_SUFFIX)

all: clean $(DMG_NAME)

$(DMG_NAME): dmg/$(APP_NAME)
	-cp -f julia.icns dmg/.VolumeIcon.icns
	-ln -fs /Applications ./dmg/Applications
	-chmod 755 ./dmg/$(APP_NAME)/Contents/MacOS/Julia
	@echo "We're going to chown the .app file to root:admin now, which requires sudo.  You may be asked for your password:"
	sudo chown -R root:admin ./dmg/$(APP_NAME)
	sudo hdiutil create $(DMG_NAME) -size 500m -ov -volname "$(VOL_NAME)" -imagekey zlib-level=9 -srcfolder dmg

dmg/$(APP_NAME): julia.icns
	make -C ../../.. binary-dist
	tar zxf ../../../julia-*.tar.gz
	mv julia-* julia
	-mkdir -p ./julia/libexec ./julia/share
	rm -f julia/lib/*.{a,la}
	-mkdir dmg
	platypus -a Julia -p /bin/bash -V $(JULIA_VERSION) -R -u "The Julia Project" -i julia.icns -Q julia.icns -o "None" -I org.julialang.julia -x -f julia script ./dmg/$(APP_NAME)
	-codesign -s "AFB379C0B4CBD9DB9A762797FC2AB5460A2B0DBE" --deep ./dmg/$(APP_NAME)

clean:
	@echo "We have to use sudo here to clean out folders owned by root.  You may be asked for your password"
	sudo rm -fr julia dmg *.dmg

.PHONY: clean all

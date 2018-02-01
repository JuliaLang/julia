# Make sure that you are building openblas with OPENBLAS_DYNAMIC_ARCH=1
# You may have to wipe your openblas build to ensure that it is built
# with support for all architectures, or else performance may suffer.

JULIAHOME := $(abspath ../../..)
include ../../../Make.inc

# X.Y.Z or X.Y.Z-hash
JULIA_VERSION_OPT_COMMIT:=$(shell [ $$(git describe --tags --exact-match 2>/dev/null) ] && echo $(JULIA_VERSION) || echo $(JULIA_VERSION)-$(JULIA_COMMIT))

# X.Y
JULIA_VERSION_MAJOR_MINOR:=$(shell echo $(JULIA_VERSION) | grep -o '^[0-9]\+.[0-9]\+')
JULIA_VERSION_MAJOR_MINOR_PATCH:=$(shell echo $(JULIA_VERSION) | grep -o '^[0-9]\+.[0-9]\+.[0-9]\+')

DMG_NAME:=$(JULIA_BINARYDIST_FILENAME).dmg
APP_NAME:=Julia-$(JULIA_VERSION_MAJOR_MINOR).app
VOL_NAME:=Julia-$(JULIA_VERSION_OPT_COMMIT)

APP_ID:=org.julialang.launcherapp
APP_COPYRIGHT:=© 2016 The Julia Project


all: clean $(DMG_NAME)

$(DMG_NAME): dmg/$(APP_NAME) dmg/.VolumeIcon.icns dmg/Applications
	hdiutil create $@ -size 1t -ov -volname "$(VOL_NAME)" -imagekey zlib-level=9 -srcfolder dmg

dmg/.VolumeIcon.icns: julia.icns
	-mkdir -p dmg
	cp -f julia.icns $@

dmg/Applications:
	-mkdir -p dmg
	ln -fs /Applications $@

dmg/$(APP_NAME): startup.applescript julia.icns
	-mkdir -p dmg
	osacompile -o $@ startup.applescript
	rm $@/Contents/Resources/applet.icns
	cp julia.icns $@/Contents/Resources/
	plutil -replace CFBundleDevelopmentRegion  -string "en" $@/Contents/Info.plist
	plutil -insert  CFBundleDisplayName        -string "Julia" $@/Contents/Info.plist
	plutil -replace CFBundleIconFile           -string "julia.icns" $@/Contents/Info.plist
	plutil -insert  CFBundleIdentifier         -string "$(APP_ID)" $@/Contents/Info.plist
	plutil -replace CFBundleName               -string "Julia" $@/Contents/Info.plist
	plutil -insert  CFBundleShortVersionString -string "$(JULIA_VERSION_MAJOR_MINOR_PATCH)" $@/Contents/Info.plist
	plutil -insert  CFBundleVersion            -string "$(JULIA_VERSION_OPT_COMMIT)" $@/Contents/Info.plist
	plutil -insert  NSHumanReadableCopyright   -string "$(APP_COPYRIGHT)" $@/Contents/Info.plist
	-mkdir -p $@/Contents/Resources/julia
	make -C $(JULIAHOME) binary-dist
	tar zxf $(JULIAHOME)/$(JULIA_BINARYDIST_FILENAME).tar.gz -C $@/Contents/Resources/julia --strip-components 1
	-codesign -s "AFB379C0B4CBD9DB9A762797FC2AB5460A2B0DBE" --deep $@

ROOTFILES := $(shell ls -ld dmg/*.app *.dmg 2> /dev/null | awk '{print $$3}')
clean:
ifneq ($(filter root,$(ROOTFILES)),)
	@echo "We have to use sudo here to clean out folders owned by root.  You may be asked for your password"
	sudo rm -rf dmg *.dmg
else
	rm -rf dmg *.dmg
endif

.PHONY: clean all

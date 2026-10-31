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
APP_COPYRIGHT:=© $(shell date '+%Y') The Julia Project


all: clean $(DMG_NAME)

$(DMG_NAME): dmg/$(APP_NAME) dmg/.VolumeIcon.icns dmg/Applications
	hdiutil create $@ -size 1t -fs HFS+ -ov -volname "$(VOL_NAME)" -imagekey zlib-level=9 -srcfolder dmg

dmg/.VolumeIcon.icns: julia.icns
	-mkdir -p dmg
	cp -f julia.icns $@

dmg/Applications:
	-mkdir -p dmg
	ln -fs /Applications $@

dmg/$(APP_NAME): Info.plist.in julia.icns
	-mkdir -p $@/Contents/MacOS
	-mkdir -p $@/Contents/Resources/julia
	# Author the bundle's Info.plist from the template, substituting version info.
	sed -e 's|@APP_ID@|$(APP_ID)|g' \
	    -e 's|@APP_COPYRIGHT@|$(APP_COPYRIGHT)|g' \
	    -e 's|@JULIA_VERSION_MAJOR_MINOR_PATCH@|$(JULIA_VERSION_MAJOR_MINOR_PATCH)|g' \
	    -e 's|@JULIA_VERSION_OPT_COMMIT@|$(JULIA_VERSION_OPT_COMMIT)|g' \
	    Info.plist.in > $@/Contents/Info.plist
	printf 'APPL????' > $@/Contents/PkgInfo
	cp julia.icns $@/Contents/Resources/
	$(MAKE) -C $(JULIAHOME) binary-dist
	$(TAR) -xzf $(JULIAHOME)/$(JULIA_BINARYDIST_FILENAME).tar.gz -C $@/Contents/Resources/julia --strip-components 1
	# The app's double-clickable entry point is `julia-terminal` (CFBundleExecutable):
	# a real copy of the julia loader placed directly in Contents/MacOS/. Apple's notary
	# requires the bundle's main executable to be a regular Mach-O in Contents/MacOS/ --
	# a symlink, or a binary living under Contents/Resources/ (where it is also sealed as
	# a resource), is rejected as an invalid signature. When the loader is invoked under
	# this name it relaunches the REPL from the bundled tree in a new Terminal.app window
	# (see cli/loader_exe.c) instead of starting the REPL in the windowless bundle
	# process; the distinct name is also how it avoids hijacking ordinary `julia` runs.
	# A plain `cp` suffices: the loader is linked with an extra rpath into the bundled
	# tree (@executable_path/../Resources/julia/lib, see cli/Makefile) precisely so this
	# copy can resolve @rpath/libjulia from Contents/MacOS/ -- keeping this rule free of
	# Mach-O tooling so it can run on any host (CI assembles the .app on Linux).
	cp $@/Contents/Resources/julia/bin/julia $@/Contents/MacOS/julia-terminal
	find $@/Contents/Resources/julia -type f -exec chmod -w {} \;
	# Even though the tarball may already be signed, we re-sign here to make it easier to add
	# unsigned executables (like the app launcher) and whatnot, without needing to maintain lists
	# of what is or is not signed.  Codesigning is cheap, so might as well do it early and often.
	if [ -n "$$MACOS_CODESIGN_IDENTITY" ]; then \
	    echo "Codesigning with identity $$MACOS_CODESIGN_IDENTITY"; \
		MACHO_FILES=$$(find "$@" -type f -perm -0111 | cut -d: -f1); \
		for f in $${MACHO_FILES}; do \
			echo "Codesigning $${f}..."; \
			codesign -s "$$MACOS_CODESIGN_IDENTITY" --option=runtime --entitlements Entitlements.plist -vvv --timestamp --force "$${f}"; \
		done; \
		codesign -s "$$MACOS_CODESIGN_IDENTITY" --option=runtime --entitlements Entitlements.plist -vvv --timestamp --force "$@"; \
	else \
		true; \
	fi

ROOTFILES := $(shell ls -ld dmg/*.app *.dmg 2> /dev/null | awk '{print $$3}')
clean:
ifneq ($(filter root,$(ROOTFILES)),)
	@echo "We have to use sudo here to clean out folders owned by root.  You may be asked for your password"
	sudo rm -rf dmg *.dmg notarize-*.xml
else
	rm -rf dmg *.dmg notarize-*.xml
endif

notarize-upload-$(DMG_NAME).xml: $(DMG_NAME)
	@# Upload the `.dmg` for notarization
	xcrun altool --notarize-app --primary-bundle-id org.julialang.launcherapp --username "$$APPLEID" --password "$$APPLEID_PASSWORD" -itc_provider A427R7F42H --file "$(DMG_NAME)" --output-format xml > "$@"
	@# Sleep for a few seconds so that we don't immediately error out when we request the UUID from Apple
	@sleep 5


notarize-check: notarize-upload-$(DMG_NAME).xml
	@# We wait in a while loop for notarization to complete
	./notarize_check.sh "$<"

# This is the top-level notarization target.  Note that this is still a somewhat manual
# process; things can go wrong, and so if it fails, you may need to inspect the `.xml`
# files to see what went wrong, but in general you can just run `make notarize` and it
# should upload, notarize, staple, and re-package the .dmg for you.
# Note that for this to work, you need to have exported `APPLEID`, `APPLEID_PASSWORD`
# and `MACOS_CODESIGN_IDENTITY` to have signed the `.app` in the first place.
notarize: notarize-check
	@# Delete old .dmg file
	rm -f $(DMG_NAME)
	@# Staple the .app
	xcrun stapler staple dmg/$(APP_NAME)
	@# re-build the .dmg
	$(MAKE) $(DMG_NAME)


.PHONY: clean all notarize-upload notarize-check

JULIAHOME := $(abspath ../../..)
include $(JULIAHOME)/Make.inc

default: productarchive

# The following variables default to development environment values.
# For a build intended for distribution, they must be overridden.
# Set APPLE_DISTRIBUTE_DEVID=1 to use automatic (It Just Works) distribution
# signing.
ifneq ($(APPLE_DISTRIBUTE_DEVID),1)
# Identity to use for signing products.
DARWIN_CODESIGN_KEYCHAIN_IDENTITY ?= Mac Developer
# Identifier of Apple Developer team.
APPLE_DEVELOPMENT_TEAM ?= unidentified
# method value in -exportOptionsPlist flag to xcodebuild.
XCEXPORT_METHOD ?= development
# Identity for the product archive (the .pkg installer).
DARWIN_CODESIGN_PRODUCT_ARCHIVE_KEYCHAIN_IDENTITY ?= -
else
DARWIN_CODESIGN_KEYCHAIN_IDENTITY ?= AFB379C0B4CBD9DB9A762797FC2AB5460A2B0DBE
APPLE_DEVELOPMENT_TEAM ?= A427R7F42H
XCEXPORT_METHOD ?= developer-id
DARWIN_CODESIGN_PRODUCT_ARCHIVE_KEYCHAIN_IDENTITY ?= Developer ID Installer
endif

# Julia.app is versioned independently of Julia because it's not Julia itself.
#
ifeq ($(JULIA_APP_SHORT_VERSION_STRING),)
$(error JULIA_APP_SHORT_VERSION_STRING not defined)
#https://developer.apple.com/documentation/bundleresources/information_property_list/cfbundleshortversionstring
# https://developer.apple.com/library/archive/documentation/General/Reference/InfoPlistKeyReference/Articles/CoreFoundationKeys.html#//apple_ref/doc/uid/20001431-111349
endif
ifeq ($(JULIA_APP_VERSION),)
$(error JULIA_APP_VERSION not defined)
# https://developer.apple.com/documentation/bundleresources/information_property_list/cfbundleversion
# https://developer.apple.com/library/archive/documentation/General/Reference/InfoPlistKeyReference/Articles/CoreFoundationKeys.html#//apple_ref/doc/uid/20001431-102364
endif

# X.Y.Z
JULIA_VERSION_MAJOR_MINOR_PATCH := $(JULIA_MAJOR_VERSION).$(JULIA_MINOR_VERSION).$(JULIA_PATCH_VERSION)

# X.Y.Z or X.Y.Z-hash
JULIA_VERSION_OPT_COMMIT:=$(shell [ $$(git describe --tags --exact-match 2>/dev/null) ] && echo $(JULIA_VERSION) || echo $(JULIA_VERSION)-$(JULIA_COMMIT))
DMGROOT := $(BUILDROOT)/dmg
APP_NAME:=Julia-$(JULIA_MAJOR_VERSION).$(JULIA_MINOR_VERSION).app
DMG_NAME:=$(BUILDROOT)/$(JULIA_BINARYDIST_FILENAME).dmg
VOL_NAME:=Julia-$(JULIA_VERSION_OPT_COMMIT)

FRAMEWORK_DESTDIR := $(BUILDROOT)/julia-$(JULIA_COMMIT)-framework
JULIA_FRAMEWORK_PATH := $(FRAMEWORK_DESTDIR)/$(framework_directory)
JULIA_FRAMEWORK_LIB := $(FRAMEWORK_DESTDIR)/$(framework_dylib)

# Xcode configurations:
XCCONFIGURATION ?= Debug
XCARCHIVE_SUFFIX := -$(JULIA_COMMIT)-$(XCCONFIGURATION).xcarchive
XCARCHIVE := $(BUILDROOT)/Xcode/Archives/JuliaLauncher$(XCARCHIVE_SUFFIX)
XCDERIVEDDATA := $(BUILDROOT)/Xcode/DerivedData/JuliaLauncher$(XCARCHIVE_SUFFIX)
XCEXPORT := $(BUILDROOT)/Xcode/Exports/JuliaLauncher$(XCARCHIVE_SUFFIX)

PRODUCTARCHIVE := $(BUILDROOT)/Julia-$(JULIA_COMMIT).pkg

$(JULIA_FRAMEWORK_LIB):
	$(MAKE) -C $(JULIAHOME) DARWIN_FRAMEWORK=1 prefix=$(FRAMEWORK_DESTDIR) darwinframework

$(XCARCHIVE)/Info.plist:
	env DEVELOPER_DIR=/Applications/Xcode.app xcrun xcodebuild \
		-project JuliaLauncher.xcodeproj \
		-scheme JuliaLauncher \
		-configuration $(XCCONFIGURATION) \
		-archivePath $(XCARCHIVE) \
		-derivedDataPath $(XCDERIVEDDATA) \
		archive \
		CODE_SIGN_STYLE=Manual \
		DEVELOPMENT_TEAM="$(APPLE_DEVELOPMENT_TEAM)" \
		CODE_SIGN_IDENTITY="$(DARWIN_CODESIGN_KEYCHAIN_IDENTITY)" \
		APP_SHORT_VERSION_STRING="$(JULIA_APP_SHORT_VERSION_STRING)" \
		APP_VERSION="$(JULIA_APP_VERSION)"

$(BUILDROOT)/xcodebuild-export.plist:
	/usr/libexec/PlistBuddy -x -c "Clear dict" $@
	/usr/libexec/PlistBuddy -x -c "Add :method string $(XCEXPORT_METHOD)" $@
	/usr/libexec/PlistBuddy -x -c "Add :teamID string $(APPLE_DEVELOPMENT_TEAM)" $@


$(XCEXPORT)/Julia.app/Contents/MacOS/Julia: $(XCARCHIVE)/Info.plist $(BUILDROOT)/xcodebuild-export.plist
	xcodebuild -exportArchive -archivePath $(XCARCHIVE) -exportPath $(XCEXPORT) -exportOptionsPlist $(BUILDROOT)/xcodebuild-export.plist

appexport: $(XCEXPORT)/Julia.app/Contents/MacOS/Julia

$(DMG_NAME): $(JULIA_FRAMEWORK_LIB) $(XCEXPORT)/Julia.app/Contents/MacOS/Julia
	-rm -rf $(DMGROOT)
	mkdir -p $(DMGROOT)
	cp -a $(JULIA_FRAMEWORK_PATH) $(DMGROOT)
	cp -a $(XCEXPORT)/Julia.app $(DMGROOT)/$(APP_NAME)
	# Temporary hack to see how the framework does on the buildbots with CI
	# NB: This invalidates the code signature of Julia.app.
	ln -s ../../../$(framework_currver) $(DMGROOT)/$(APP_NAME)/Contents/Resources/julia
	hdiutil create $@ -size 1t -fs HFS+ -ov -volname "$(VOL_NAME)" -imagekey zlib-level=9 -srcfolder $(DMGROOT)
ifeq ($(APPLE_DISTRIBUTE_DEVID),1)
	codesign -s $(DARWIN_CODESIGN_KEYCHAIN_IDENTITY) $@
endif

dmg: $(DMG_NAME)

$(BUILDROOT)/framework-component.plist: $(JULIAHOME)/contrib/mac/frameworkapp/framework-component.plist
	sed -e 's/FRAMEWORK_VERSION/$(FRAMEWORK_VERSION)/g' -e 's/FRAMEWORK_NAME/$(FRAMEWORK_NAME)/g' $< > $@

# This target makes a component package for the framework. It has some
# important properties.  Together, the properties allow one "Julia.framework"
# to exist at a location with multiple versions of Julia within.
#
# 1. The component's identifer is versioned to match the bundled framework.
# This allows multiple versions of the component to be installed with the
# Julia.framework/Versions directory.
# 2. The component-plist identifies the Versions/x.y directory as an upgradable
# bundle instead of the parent directory Julia.framework.  We want the
# installer to treat each version as an atomic unit/bundle and not the overall
# framework.
$(BUILDROOT)/$(FRAMEWORK_NAME)-framework.pkg: $(BUILDROOT)/framework-component.plist $(JULIA_FRAMEWORK_LIB)
	pkgbuild \
		--install-location /Library/Frameworks \
		--version $(JULIA_VERSION_MAJOR_MINOR_PATCH) \
		--root $(FRAMEWORK_DESTDIR) \
		--component-plist $< \
		--identifier org.julialang.julia.lib.v$(FRAMEWORK_VERSION) \
		$@

$(BUILDROOT)/launcher.pkg: $(JULIAHOME)/contrib/mac/frameworkapp/launcher-component.plist $(XCEXPORT)/Julia.app/Contents/MacOS/Julia
	find $(XCEXPORT) -not -name Julia.app -d 1 -delete
	pkgbuild \
		--install-location /Applications \
		--version $(JULIA_VERSION_MAJOR_MINOR_PATCH) \
		--root $(XCEXPORT) \
		--component-plist $< \
		--identifier org.julialang.JuliaLauncher \
		$@

$(BUILDROOT)/$(FRAMEWORK_NAME).dist: $(JULIAHOME)/contrib/mac/frameworkapp/Julia.dist
	sed -e 's/MINVERSION/$(MACOSX_VERSION_MIN)/g' -e 's/FRAMEWORK_NAME/$(FRAMEWORK_NAME)/g' $< > $@

$(PRODUCTARCHIVE): $(BUILDROOT)/launcher.pkg $(BUILDROOT)/$(FRAMEWORK_NAME)-framework.pkg $(BUILDROOT)/$(FRAMEWORK_NAME).dist $(JULIAHOME)/contrib/mac/frameworkapp/installresources
	productbuild \
		--package-path $(BUILDROOT) \
		--resources $(JULIAHOME)/contrib/mac/frameworkapp/installresources \
		--distribution $(BUILDROOT)/$(FRAMEWORK_NAME).dist \
		$@

productarchive: $(PRODUCTARCHIVE)

signedproductarchive: $(PRODUCTARCHIVE)
	productsign --sign "$(DARWIN_CODESIGN_PRODUCT_ARCHIVE_KEYCHAIN_IDENTITY)" $< $<.signed
	mv $<.signed $<

clean:
	-rm -rf $(XCARCHIVE) $(XCDERIVEDDATA) $(XCEXPORT)
	-rm -rf $(FRAMEWORK_DESTDIR)
	-rm -f $(PRODUCTARCHIVE)

.PHONY: appexport clean productarchive signedproductarchive dmg
.INTERMEDIATE: $(BUILDROOT)/xcodebuild-export.plist $(BUILDROOT)/framework-component.plist $(BUILDROOT)/$(FRAMEWORK_NAME)-framework.pkg $(BUILDROOT)/launcher.pkg $(BUILDROOT)/$(FRAMEWORK_NAME).dist

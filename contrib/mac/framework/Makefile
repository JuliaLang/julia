JULIAHOME := $(abspath ../../..)
include $(JULIAHOME)/Make.inc

# Build and bundle Julia (release XOR debug) as a Darwin/Apple framework
# usage: make O=<builddir> framework

default: framework

# The codesigning identity on Darwin.
# Used with `codesign -s $(DARWIN_CODESIGN_KEYCHAIN_IDENTITY) $file`.
# The default "-" makes an ad-hoc signature.
DARWIN_CODESIGN_KEYCHAIN_IDENTITY ?= -

# Set DARWIN_CODESIGN_TIMESTAMP = 1 to add a timestamp when codesigning (useful for notarization).
DARWIN_CODESIGN_TIMESTAMP ?= 0
ifeq ($(DARWIN_CODESIGN_TIMESTAMP),1)
darwin_codesign_options=--timestamp
endif

# Set DARWIN_HARDENED_RUNTIME = 1 to enable the hardened runtime on macOS.
DARWIN_HARDENED_RUNTIME ?= 0
ifeq ($(DARWIN_HARDENED_RUNTIME),1)
darwin_codesign_julia_options=-o runtime --entitlements $(JULIAHOME)/contrib/mac/framework/julia.entitlements
endif

# framework directory structure targets
framework_destdirs := $(sort $(addprefix $(DESTDIR)$(prefix)/,$(framework_currver) $(framework_headers) $(framework_headers)/julia $(framework_documentation) $(framework_resources) $(framework_frameworks) $(framework_modules) $(framework_helpers) $(framework_currver)/lib))

# symlink targets
framework_current_symlinks := $(addprefix $(DESTDIR)$(prefix)/$(framework_directory)/,$(FRAMEWORK_NAME) Headers Documentation Resources Frameworks Modules Helpers)
framework_version_symlink := $(DESTDIR)$(prefix)/$(framework_versions)/Current

# targets:

$(framework_destdirs):
	mkdir -p $@
$(framework_current_symlinks): | $(framework_destdirs)
	ln -s -f Versions/Current/$(notdir $@) $@
$(framework_version_symlink): | $(framework_destdirs)
	ln -s -f $(FRAMEWORK_VERSION) $@

$(DESTDIR)$(prefix)/$(framework_currver)/bin: | $(framework_destdirs)
	ln -s -f Helpers $@
$(DESTDIR)$(prefix)/$(framework_currver)/lib/libjulia.$(SOMAJOR).$(SOMINOR).dylib: | $(framework_destdirs)
	ln -s -f ../$(FRAMEWORK_NAME) $@
$(DESTDIR)$(prefix)/$(framework_currver)/lib/libjulia.$(SOMAJOR).dylib: | $(framework_destdirs)
	ln -s -f ../$(FRAMEWORK_NAME) $@
$(DESTDIR)$(prefix)/$(framework_currver)/lib/libjulia.dylib: | $(framework_destdirs)
	ln -s -f ../$(FRAMEWORK_NAME) $@
ifeq ($(BUNDLE_DEBUG_LIBS),1)
$(DESTDIR)$(prefix)/$(framework_currver)/lib/libjulia-debug.$(SOMAJOR).$(SOMINOR).dylib: | $(framework_destdirs)
	ln -s -f ../$(FRAMEWORK_NAME)_debug  $@
$(DESTDIR)$(prefix)/$(framework_currver)/lib/libjulia-debug.$(SOMAJOR).dylib: | $(framework_destdirs)
	ln -s -f ../$(FRAMEWORK_NAME)_debug  $@
$(DESTDIR)$(prefix)/$(framework_currver)/lib/libjulia-debug.dylib: | $(framework_destdirs)
	ln -s -f ../$(FRAMEWORK_NAME)_debug $@
endif
$(DESTDIR)$(prefix)/$(framework_currver)/libexec: | $(framework_destdirs)
	ln -s -f Helpers $@
$(DESTDIR)$(prefix)/$(framework_currver)/share: | $(framework_destdirs)
	ln -s -f Resources $@
$(DESTDIR)$(prefix)/$(framework_currver)/include: | $(framework_destdirs)
	ln -s -f Headers $@
$(DESTDIR)$(prefix)/$(framework_currver)/etc: | $(framework_destdirs)
	ln -s -f Resources $@

hier_symlinks: \
	$(DESTDIR)$(prefix)/$(framework_currver)/bin \
	$(DESTDIR)$(prefix)/$(framework_currver)/lib/libjulia.$(SOMAJOR).$(SOMINOR).dylib \
	$(DESTDIR)$(prefix)/$(framework_currver)/lib/libjulia.$(SOMAJOR).dylib \
	$(DESTDIR)$(prefix)/$(framework_currver)/lib/libjulia.dylib \
	$(DESTDIR)$(prefix)/$(framework_currver)/libexec \
	$(DESTDIR)$(prefix)/$(framework_currver)/share \
	$(DESTDIR)$(prefix)/$(framework_currver)/include \
	$(DESTDIR)$(prefix)/$(framework_currver)/etc
ifeq ($(BUNDLE_DEBUG_LIBS),1)
hier_symlinks: \
	$(DESTDIR)$(prefix)/$(framework_currver)/lib/libjulia-debug.$(SOMAJOR).$(SOMINOR).dylib \
	$(DESTDIR)$(prefix)/$(framework_currver)/lib/libjulia-debug.$(SOMAJOR).dylib \
	$(DESTDIR)$(prefix)/$(framework_currver)/lib/libjulia-debug.dylib
endif

$(DESTDIR)$(prefix)/$(framework_infoplist): | $(framework_destdirs)
	/usr/libexec/PlistBuddy -x -c "Clear dict" $@
	/usr/libexec/PlistBuddy -x -c "Add :CFBundleName string $(FRAMEWORK_NAME)" $@
	/usr/libexec/PlistBuddy -x -c "Add :CFBundleExecutable string $(FRAMEWORK_NAME)" $@
	/usr/libexec/PlistBuddy -x -c "Add :CFBundleIdentifier string $(DARWIN_CODESIGN_ID_BASE).lib" $@
	/usr/libexec/PlistBuddy -x -c "Add :CFBundleVersion string $(JULIA_COMMIT)" $@
	/usr/libexec/PlistBuddy -x -c "Add :CFBundleShortVersionString string $(JULIA_MAJOR_VERSION).$(JULIA_MINOR_VERSION).$(JULIA_PATCH_VERSION)" $@
	/usr/libexec/PlistBuddy -x -c "Add :CFBundleSignature string ???" $@
	/usr/libexec/PlistBuddy -x -c "Add :CFBundlePackageType string FMWK" $@
	/usr/libexec/PlistBuddy -x -c "Add :CFBundleInfoDictionaryVersion string 6.0" $@
	/usr/libexec/PlistBuddy -x -c "Add :NSHumanReadableCopyright string \"Copyright © 2009-2019 Julia project contributors (https://github.com/JuliaLang/julia/contributors). See LICENSE.md.\"" $@

toplevelinstall:
ifneq ($(DARWIN_FRAMEWORK),1)
	$(error Darwin framework is not enabled. Please set DARWIN_FRAMEWORK=1)
endif
	$(MAKE) -C $(BUILDROOT) install

# frameworknoinstall assumes `make install` was already completed.
frameworknoinstall: $(DESTDIR)$(prefix)/$(framework_infoplist) | $(framework_current_symlinks) $(framework_version_symlink) $(framework_destdirs) hier_symlinks

	$(INSTALL_NAME_CHANGE_CMD) @rpath/libjulia.dylib @rpath/$(FRAMEWORK_NAME) $(DESTDIR)$(bindir)/julia
	$(JULIAHOME)/contrib/delete-all-rpaths.sh $(DESTDIR)$(bindir)/julia
	install_name_tool -add_rpath @executable_path/$(libdir_rel) $(DESTDIR)$(bindir)/julia
ifeq ($(BUNDLE_DEBUG_LIBS),1)
	$(INSTALL_NAME_CHANGE_CMD) @rpath/libjulia-debug.dylib @rpath/$(FRAMEWORK_NAME)_debug $(DESTDIR)$(bindir)/julia-debug
	$(JULIAHOME)/contrib/delete-all-rpaths.sh $(DESTDIR)$(bindir)/julia-debug
	install_name_tool -add_rpath @executable_path/$(libdir_rel) $(DESTDIR)$(bindir)/julia-debug
endif

	# fix libjulia paths
	$(INSTALL_NAME_CMD)$(framework_dylib) $(DESTDIR)$(prefix)/$(framework_dylib)
	$(JULIAHOME)/contrib/delete-all-rpaths.sh $(DESTDIR)$(prefix)/$(framework_dylib)
	install_name_tool -add_rpath @loader_path/Frameworks $(DESTDIR)$(prefix)/$(framework_dylib)
ifeq ($(BUNDLE_DEBUG_LIBS),1)
	# Install name should be the non-debug variant.
	# Julia_debug variant is selected with DYLD_IMAGE_SUFFIX (man 1 dyld).
	# julia-debug explicitly links to Julia_debug so no need to manipulate DYLD_IMAGE_SUFFIX for it.
	$(INSTALL_NAME_CMD)$(framework_dylib) $(DESTDIR)$(prefix)/$(framework_dylib)_debug
	$(JULIAHOME)/contrib/delete-all-rpaths.sh $(DESTDIR)$(prefix)/$(framework_dylib)_debug
	install_name_tool -add_rpath @loader_path/Frameworks $(DESTDIR)$(prefix)/$(framework_dylib)_debug
endif

	$(INSTALL_NAME_CHANGE_CMD) @rpath/libjulia.dylib @rpath/$(FRAMEWORK_NAME) $(DESTDIR)$(prefix)/$(framework_frameworks)/sys.dylib
	$(JULIAHOME)/contrib/delete-all-rpaths.sh $(DESTDIR)$(prefix)/$(framework_frameworks)/sys.dylib
	install_name_tool -add_rpath @loader_path/.. $(DESTDIR)$(prefix)/$(framework_frameworks)/sys.dylib
ifeq ($(BUNDLE_DEBUG_LIBS),1)
	$(INSTALL_NAME_CHANGE_CMD) @rpath/libjulia-debug.dylib @rpath/$(FRAMEWORK_NAME)_debug $(DESTDIR)$(prefix)/$(framework_frameworks)/sys-debug.dylib
	$(JULIAHOME)/contrib/delete-all-rpaths.sh $(DESTDIR)$(prefix)/$(framework_frameworks)/sys-debug.dylib
	install_name_tool -add_rpath @loader_path/.. $(DESTDIR)$(prefix)/$(framework_frameworks)/sys-debug.dylib
endif

	# fix private lib paths
	$(JULIAHOME)/contrib/delete-all-rpaths.sh $(DESTDIR)$(prefix)/$(framework_frameworks)/*

	# Add framework header
	sed -e 's/<Julia/<$(FRAMEWORK_NAME)/' $(JULIAHOME)/contrib/mac/framework/Julia.h > $(DESTDIR)$(prefix)/$(framework_headers)/$(FRAMEWORK_NAME).h

	# cleanup unnecessary install outputs
	rm $(DESTDIR)$(datarootdir)/julia/startup.jl
	rm -rf $(DESTDIR)$(datarootdir)/icons $(DESTDIR)$(datarootdir)/applications $(DESTDIR)$(datarootdir)/appdata
	find $(DESTDIR)$(prefix)/$(framework_directory) \( -name '.DS_Store' -o -name '.gitignore' -o -name Makefile -o -name .travis.yml -o -name .codecov.yml \) -delete

	# Include Julia's license info
	$(INSTALL_F) $(JULIAHOME)/LICENSE.md $(DESTDIR)$(prefix)/$(framework_resources)

	# Add the module map file.
	sed -e 's/Julia/$(FRAMEWORK_NAME)/' $(JULIAHOME)/contrib/mac/framework/module.modulemap > $(DESTDIR)$(prefix)/$(framework_modules)/module.modulemap

	# Make sure EUID:EGID owns the framework and permissions are set.
	chmod -R u+w $(DESTDIR)$(prefix)/$(framework_directory)
	chown -R $$(id -un):$$(id -gn) $(DESTDIR)$(prefix)/$(framework_directory)

	# ad-hoc codesigning
	#NB: must be the last lines of the recipe, else signature may be invalidated.

	# Codesign should look at the embedded Info.plist to get the signing identifier.
	# See JLDFLAGS in Make.inc for Darwin platform and Info.plist target in ui/Makefile.
	codesign -s "$(DARWIN_CODESIGN_KEYCHAIN_IDENTITY)" -v $(darwin_codesign_options) $(darwin_codesign_julia_options) $(DESTDIR)$(prefix)/$(framework_helpers)/julia
ifeq ($(BUNDLE_DEBUG_LIBS),1)
	codesign -s "$(DARWIN_CODESIGN_KEYCHAIN_IDENTITY)" -v $(darwin_codesign_options) $(darwin_codesign_julia_options) $(DESTDIR)$(prefix)/$(framework_helpers)/julia-debug
endif

	# Append the library name to the base codesigning id.
	for file in $(DESTDIR)$(prefix)/$(framework_frameworks)/*.dylib* ; do \
		if [ -f "$$file" -a ! -L "$$file" -a -w "$$file" -a -x "$$file" ]; then \
			idsuffix=$$(basename $${file%%.dylib*}) ; \
			codesign -s "$(DARWIN_CODESIGN_KEYCHAIN_IDENTITY)" -v $(darwin_codesign_options) -i $(darwin_codesign_id_julia_deps).$${idsuffix} -f $$file ; \
		fi \
	done

	touch -c $(DESTDIR)$(prefix)/$(framework_directory)

	# Sign the (current version) framework bundle.
ifeq ($(BUNDLE_DEBUG_LIBS),1)
	# Don't forget to sign Frameworks/Julia_debug
	codesign -s "$(DARWIN_CODESIGN_KEYCHAIN_IDENTITY)" -v $(darwin_codesign_options) -i $(DARWIN_CODESIGN_ID_BASE).lib -f \
	    $(DESTDIR)$(prefix)/$(framework_dylib)_debug
endif
	codesign -s "$(DARWIN_CODESIGN_KEYCHAIN_IDENTITY)" -v $(darwin_codesign_options) $(DESTDIR)$(prefix)/$(framework_currver)

framework: toplevelinstall

.PHONY: toplevelinstall framework frameworknoinstall hier_symlinks

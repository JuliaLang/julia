JULIAHOME := $(abspath ../../..)
include $(JULIAHOME)/Make.inc

# Build and bundle Julia (release XOR debug) as a Darwin/Apple framework
# usage: make O=<builddir> framework

default: framework

# The codesigning identity on Darwin.
# Used with `codesign -s $(DARWIN_CODESIGN_KEYCHAIN_IDENTITY) $file`.
# The default "-" makes an ad-hoc signature.
DARWIN_CODESIGN_KEYCHAIN_IDENTITY ?= -

# The prefix for all code sign identifiers.
DARWIN_CODESIGN_ID_BASE ?= org.julialang.julia

# The codesign id for the ui/repl (also embedded in Info.plist).
darwin_codesign_id_julia_ui := $(DARWIN_CODESIGN_ID_BASE).ui
# The prefix for all deps.
darwin_codesign_id_julia_deps := $(DARWIN_CODESIGN_ID_BASE).deps

# framework directory structure targets
framework_destdirs := $(sort $(bindir) $(libdir) $(private_libdir) $(datarootdir) $(docdir) $(mandir) $(man1dir) $(includedir) $(sysconfdir) $(addprefix $(DESTDIR)$(prefix)/,$(framework_currver) $(framework_headers) $(framework_headers)/julia $(framework_documentation) $(framework_resources) $(framework_frameworks) $(framework_modules) $(framework_helpers) $(framework_currver)/lib))

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
$(DESTDIR)$(prefix)/$(framework_currver)/lib/libjulia$(JULIA_LIBSUFFIX).$(SOMAJOR).$(SOMINOR).dylib: | $(framework_destdirs)
	ln -s -f ../$(FRAMEWORK_NAME) $@
$(DESTDIR)$(prefix)/$(framework_currver)/lib/libjulia$(JULIA_LIBSUFFIX).$(SOMAJOR).dylib: | $(framework_destdirs)
	ln -s -f ../$(FRAMEWORK_NAME) $@
$(DESTDIR)$(prefix)/$(framework_currver)/lib/libjulia$(JULIA_LIBSUFFIX).dylib: | $(framework_destdirs)
	ln -s -f ../$(FRAMEWORK_NAME) $@
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
	$(DESTDIR)$(prefix)/$(framework_currver)/lib/libjulia$(JULIA_LIBSUFFIX).$(SOMAJOR).$(SOMINOR).dylib \
	$(DESTDIR)$(prefix)/$(framework_currver)/lib/libjulia$(JULIA_LIBSUFFIX).$(SOMAJOR).dylib \
	$(DESTDIR)$(prefix)/$(framework_currver)/lib/libjulia$(JULIA_LIBSUFFIX).dylib \
	$(DESTDIR)$(prefix)/$(framework_currver)/libexec \
	$(DESTDIR)$(prefix)/$(framework_currver)/share \
	$(DESTDIR)$(prefix)/$(framework_currver)/include \
	$(DESTDIR)$(prefix)/$(framework_currver)/etc

$(DESTDIR)$(prefix)/$(framework_infoplist): | $(framework_destdirs)
	/usr/libexec/PlistBuddy -x -c "Clear dict" $@
	/usr/libexec/PlistBuddy -x -c "Add :CFBundleName string $(FRAMEWORK_NAME)" $@
	/usr/libexec/PlistBuddy -x -c "Add :CFBundleExecutable string $(FRAMEWORK_NAME)" $@
	/usr/libexec/PlistBuddy -x -c "Add :CFBundleIdentifier string $(DARWIN_CODESIGN_ID_BASE).lib" $@
	/usr/libexec/PlistBuddy -x -c "Add :CFBundleVersion string $(JULIA_VERSION_OPT_COMMIT)" $@
	/usr/libexec/PlistBuddy -x -c "Add :CFBundleShortVersionString string $(JULIA_MAJOR_VERSION).$(JULIA_MINOR_VERSION).$(JULIA_PATCH_VERSION)" $@
	/usr/libexec/PlistBuddy -x -c "Add :CFBundleSignature string ???" $@
	/usr/libexec/PlistBuddy -x -c "Add :CFBundlePackageType string FMWK" $@
	/usr/libexec/PlistBuddy -x -c "Add :CFBundleInfoDictionaryVersion string 6.0" $@
	/usr/libexec/PlistBuddy -x -c "Add :NSHumanReadableCopyright string \"Copyright © 2009-2018 Julia project contributors (https://github.com/JuliaLang/julia/contributors). See LICENSE.md.\"" $@

toplevelinstall:
ifneq ($(DARWIN_FRAMEWORK),1)
	$(error Darwin framework is not enabled. Please set DARWIN_FRAMEWORK=1)
endif
	$(MAKE) -C $(BUILDROOT) install

framework: toplevelinstall $(DESTDIR)$(prefix)/$(framework_infoplist) | $(framework_current_symlinks) $(framework_version_symlink) $(framework_destdirs) hier_symlinks

	# retain correct release/debug julia and fix paths
	mv $(DESTDIR)$(bindir)/$(notdir $(JULIA_EXECUTABLE)) $(DESTDIR)$(bindir)/julia
	-rm $(DESTDIR)$(bindir)/julia-debug
	$(INSTALL_NAME_CHANGE_CMD) @rpath/libjulia$(JULIA_LIBSUFFIX).dylib @rpath/$(FRAMEWORK_NAME) $(DESTDIR)$(bindir)/julia
	$(JULIAHOME)/contrib/mac/framework/delete-all-rpaths.sh $(DESTDIR)$(bindir)/julia
	install_name_tool -add_rpath @executable_path/$(libdir_rel) $(DESTDIR)$(bindir)/julia

	# fix libjulia paths
	$(INSTALL_NAME_CMD)$(framework_dylib) $(DESTDIR)$(prefix)/$(framework_dylib)
	$(JULIAHOME)/contrib/mac/framework/delete-all-rpaths.sh $(DESTDIR)$(prefix)/$(framework_dylib)
	install_name_tool -add_rpath @loader_path/Frameworks $(DESTDIR)$(prefix)/$(framework_dylib)

	# retain correct system image and fix path
	for lib in $(DESTDIR)$(private_libdir)/sys*.dylib; do \
	  [ "$$lib" = "$(DESTDIR)$(private_libdir)/sys$(JULIA_LIBSUFFIX).dylib" ] || rm "$$lib" ; \
	done
	$(INSTALL_NAME_CHANGE_CMD) @rpath/libjulia$(JULIA_LIBSUFFIX).dylib @rpath/$(FRAMEWORK_NAME) $(DESTDIR)$(prefix)/$(framework_frameworks)/sys$(JULIA_LIBSUFFIX).dylib
	$(JULIAHOME)/contrib/mac/framework/delete-all-rpaths.sh $(DESTDIR)$(prefix)/$(framework_frameworks)/sys$(JULIA_LIBSUFFIX).dylib
	install_name_tool -add_rpath @loader_path/.. $(DESTDIR)$(prefix)/$(framework_frameworks)/sys$(JULIA_LIBSUFFIX).dylib

	# fix private lib paths
	$(JULIAHOME)/contrib/mac/framework/delete-all-rpaths.sh $(DESTDIR)$(prefix)/$(framework_frameworks)/*

	$(JULIAHOME)/contrib/fixup-libgfortran.sh $(DESTDIR)$(prefix)/$(framework_frameworks)

	# Add framework header
	sed -e 's/<Julia/<$(FRAMEWORK_NAME)/' $(JULIAHOME)/contrib/mac/framework/Julia.h > $(DESTDIR)$(prefix)/$(framework_headers)/$(FRAMEWORK_NAME).h

	# cleanup unnecessary install outputs
	rm $(DESTDIR)$(datarootdir)/julia/startup.jl
	rm -rf $(DESTDIR)$(datarootdir)/julia/site $(DESTDIR)$(datarootdir)/icons $(DESTDIR)$(datarootdir)/applications $(DESTDIR)$(datarootdir)/appdata
	find $(DESTDIR)$(prefix)/$(framework_directory) \( -name '.DS_Store' -o -name '.gitignore' -o -name Makefile -o -name .travis.yml -o -name .codecov.yml \) -delete

	# Include Julia's license info
	$(INSTALL_F) $(JULIAHOME)/LICENSE.md $(DESTDIR)$(prefix)/$(framework_resources)

	# Add the module map file.
	sed -e 's/Julia/$(FRAMEWORK_NAME)/' $(JULIAHOME)/contrib/mac/framework/module.modulemap > $(DESTDIR)$(prefix)/$(framework_modules)/module.modulemap

	# Make sure EUID:EGID owns the framework
	chown -R $$(id -un):$$(id -gn) $(DESTDIR)$(prefix)/$(framework_directory)

	# ad-hoc codesigning
	#NB: must be the last lines of the recipe, else signature may be invalidated.

	# Codesign should look at the embedded Info.plist to get the signing identifier.
	# See JLDFLAGS in Make.inc for Darwin platform and Info.plist target in ui/Makefile.
	codesign -s "$(DARWIN_CODESIGN_KEYCHAIN_IDENTITY)" -v $(DESTDIR)$(prefix)/$(framework_helpers)/julia

	# Append the library name to the base codesigning id.
	for file in $(DESTDIR)$(prefix)/$(framework_frameworks)/*.dylib* ; do \
		if [ -f "$$file" -a ! -L "$$file" -a -w "$$file" -a -x "$$file" ]; then \
			idsuffix=$$(basename $${file%%.dylib*}) ; \
			codesign -s "$(DARWIN_CODESIGN_KEYCHAIN_IDENTITY)" -v -i $(darwin_codesign_id_julia_deps).$${idsuffix} -f $$file ; \
		fi \
	done

	touch -c $(DESTDIR)$(prefix)/$(framework_directory)

	# Sign the (current version) framework bundle.
	codesign -s "$(DARWIN_CODESIGN_KEYCHAIN_IDENTITY)" -v $(DESTDIR)$(prefix)/$(framework_currver)


.PHONY: toplevelinstall framework hier_symlinks

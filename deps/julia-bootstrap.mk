## JULIA-BOOTSTRAP ##
#
# A published julia release, pinned in julia-bootstrap.version, which builds
# the frontend that bootstraps this julia: libjulia-frontend, compiled from
# this tree's JuliaSyntax and JuliaLowering and linked with the release's own
# static runtime (see src/frontend/README.md). Like the build's other
# dependencies it is only downloaded (from the official release server),
# checksummed and extracted here; nothing is installed. To use an existing
# installation of the release instead, set JULIA_BOOTSTRAP_HOME to its root
# directory (see Make.inc).
include $(SRCDIR)/julia-bootstrap.version

# The release's name for this platform, and its directory on the server
JULIA_BOOTSTRAP_MAJMIN := $(word 1,$(subst ., ,$(JULIA_BOOTSTRAP_VER))).$(word 2,$(subst ., ,$(JULIA_BOOTSTRAP_VER)))
ifeq ($(OS),Linux)
JULIA_BOOTSTRAP_PLATFORM := linux-$(ARCH)
JULIA_BOOTSTRAP_DIR := linux/$(if $(filter x86_64,$(ARCH)),x64,$(if $(filter i686,$(ARCH)),x86,$(ARCH)))
else ifeq ($(OS),Darwin)
JULIA_BOOTSTRAP_PLATFORM := $(if $(filter aarch64,$(ARCH)),macaarch64,mac64)
JULIA_BOOTSTRAP_DIR := mac/$(if $(filter x86_64,$(ARCH)),x64,$(ARCH))
else ifeq ($(OS),WINNT)
JULIA_BOOTSTRAP_PLATFORM := $(if $(filter x86_64,$(ARCH)),win64,win32)
JULIA_BOOTSTRAP_DIR := winnt/$(if $(filter x86_64,$(ARCH)),x64,x86)
else ifeq ($(OS),FreeBSD)
JULIA_BOOTSTRAP_PLATFORM := freebsd-$(ARCH)
JULIA_BOOTSTRAP_DIR := freebsd/$(if $(filter x86_64,$(ARCH)),x64,$(ARCH))
endif
JULIA_BOOTSTRAP_TAR := julia-$(JULIA_BOOTSTRAP_VER)-$(JULIA_BOOTSTRAP_PLATFORM).tar.gz
JULIA_BOOTSTRAP_URL := https://julialang-s3.julialang.org/bin/$(JULIA_BOOTSTRAP_DIR)/$(JULIA_BOOTSTRAP_MAJMIN)/$(JULIA_BOOTSTRAP_TAR)
JULIA_BOOTSTRAP_SRCDIR := $(BUILDDIR)/julia-bootstrap-$(JULIA_BOOTSTRAP_VER)

$(SRCCACHE)/$(JULIA_BOOTSTRAP_TAR): | $(SRCCACHE)
	$(JLDOWNLOAD) $@ $(JULIA_BOOTSTRAP_URL)
	touch -c $@

$(JULIA_BOOTSTRAP_SRCDIR)/source-extracted: $(SRCCACHE)/$(JULIA_BOOTSTRAP_TAR)
	$(JLCHECKSUM) $<
	rm -rf $(dir $@)
	mkdir -p $(dir $@)
	$(TAR) -C $(dir $@) --strip-components 1 -xf $<
	echo 1 > $@

checksum-julia-bootstrap: $(SRCCACHE)/$(JULIA_BOOTSTRAP_TAR)
	$(JLCHECKSUM) $<

$(JULIA_BOOTSTRAP_SRCDIR)/build-compiled: $(JULIA_BOOTSTRAP_SRCDIR)/source-extracted
	echo 1 > $@

$(JULIA_BOOTSTRAP_SRCDIR)/build-checked: $(JULIA_BOOTSTRAP_SRCDIR)/build-compiled
	$(JULIA_BOOTSTRAP_SRCDIR)/bin/julia --startup-file=no -e 'VERSION == VersionNumber("$(JULIA_BOOTSTRAP_VER)") || exit(1)'
	echo 1 > $@

clean-julia-bootstrap:
	-rm -f $(JULIA_BOOTSTRAP_SRCDIR)/build-compiled

distclean-julia-bootstrap:
	rm -rf $(SRCCACHE)/$(JULIA_BOOTSTRAP_TAR) $(JULIA_BOOTSTRAP_SRCDIR)

get-julia-bootstrap: $(SRCCACHE)/$(JULIA_BOOTSTRAP_TAR)
extract-julia-bootstrap: $(JULIA_BOOTSTRAP_SRCDIR)/source-extracted
configure-julia-bootstrap: extract-julia-bootstrap
compile-julia-bootstrap: $(JULIA_BOOTSTRAP_SRCDIR)/build-compiled
fastcheck-julia-bootstrap: check-julia-bootstrap
check-julia-bootstrap: $(JULIA_BOOTSTRAP_SRCDIR)/build-checked
install-julia-bootstrap: compile-julia-bootstrap

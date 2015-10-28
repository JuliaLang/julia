## high-level setup ##
SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/..)
ifeq ($(abspath .),$(abspath $(SRCDIR)))
BUILDDIR := build
else
BUILDDIR := .
endif
include $(SRCDIR)/Versions.make
include $(JULIAHOME)/Make.inc

# Special comments:
#
# all targets in here should follow the same structure,
# and provide a get-a, configure-a, compile-a, check-a, and install-a
# additionally all targets should be listed in the getall target for easier off-line compilation
# if you are adding a new target, it can help to copy an similar, existing target
#
# autoconf configure-driven scripts: llvm pcre arpack fftw unwind gmp mpfr patchelf libuv git
# custom Makefile rules: openlibm Rmath-julia dsfmt suitesparse-wrapper suitesparse lapack openblas utf8proc objconv osxunwind
# entirely custom: virtualenv
# CMake libs: libgit2
#
# downloaded from git: llvm-svn, libuv, libopenlibm, utf8proc, openspecfun, libgit2
#
# there are rules in this file with the . replaced by a %
# this is some magic Makefile trick that tells make
# that all targets with a % in them on that line will
# be rebuilt in a single invocation
#

## Some shared configuration options ##

CONFIGURE_COMMON := --prefix=$(abspath $(build_prefix)) --build=$(BUILD_MACHINE) --libdir=$(abspath $(build_libdir)) $(CUSTOM_LD_LIBRARY_PATH)
ifneq ($(XC_HOST),)
CONFIGURE_COMMON += --host=$(XC_HOST)
endif
ifeq ($(OS),WINNT)
ifneq ($(USEMSVC), 1)
CONFIGURE_COMMON += LDFLAGS="$(LDFLAGS) -Wl,--stack,8388608"
endif
endif
CONFIGURE_COMMON += F77="$(FC)" CC="$(CC) $(DEPS_CFLAGS)" CXX="$(CXX) $(DEPS_CXXFLAGS)"

CMAKE_CC_ARG := $(CC_ARG) $(DEPS_CFLAGS)
CMAKE_CXX_ARG := $(CXX_ARG) $(DEPS_CXXFLAGS)

CMAKE_COMMON := -DCMAKE_INSTALL_PREFIX:PATH=$(build_prefix) -DCMAKE_BUILD_TYPE=Release
ifneq ($(VERBOSE), 0)
CMAKE_COMMON += -DCMAKE_VERBOSE_MAKEFILE=ON
endif
CMAKE_COMMON += -DCMAKE_C_COMPILER="$(CC_BASE)"
ifneq ($(strip $(CMAKE_CC_ARG)),)
CMAKE_COMMON += -DCMAKE_C_COMPILER_ARG1="$(CMAKE_CC_ARG)"
endif
CMAKE_COMMON += -DCMAKE_CXX_COMPILER="$(CXX_BASE)"
ifneq ($(strip $(CMAKE_CXX_ARG)),)
CMAKE_COMMON += -DCMAKE_CXX_COMPILER_ARG1="$(CMAKE_CXX_ARG)"
endif

# For now this is LLVM specific, but I expect it won't be in the future
ifneq ($(LLVM_USE_CMAKE),)
ifeq ($(CMAKE_GENERATOR),Ninja)
CMAKE_GENERATOR_COMMAND := -G Ninja
else ifeq ($(CMAKE_GENERATOR),make)
CMAKE_GENERATOR_COMMAND := -G "Unix Makefiles"
else
$(error Unknown CMake generator '$(CMAKE_GENERATOR)'. Options are 'Ninja' and 'make')
endif
endif

# If the top-level Makefile is called with environment variables,
# they will override the values passed above to ./configure
MAKE_COMMON := DESTDIR="" prefix=$(build_prefix) bindir=$(build_bindir) libdir=$(build_libdir) libexecdir=$(build_libexecdir) datarootdir=$(build_datarootdir) includedir=$(build_includedir) sysconfdir=$(build_sysconfdir) O=


## Overall configuration of which rules exist and should be run by default ##

# prevent installing libs into usr/lib64 on opensuse
unexport CONFIG_SITE

ifeq ($(USE_GPL_LIBS), 1)
STAGE1_DEPS :=
STAGE2_DEPS := Rmath-julia
STAGE3_DEPS := suitesparse-wrapper
else
STAGE1_DEPS :=
STAGE2_DEPS :=
STAGE3_DEPS :=
endif

ifeq ($(USE_SYSTEM_LIBUV), 0)
STAGE1_DEPS += libuv
endif

ifeq ($(USE_SYSTEM_LIBUNWIND), 0)
ifeq ($(OS), Linux)
STAGE1_DEPS += unwind
else ifeq ($(OS), FreeBSD)
STAGE1_DEPS += unwind
else ifeq ($(OS), Darwin)
STAGE1_DEPS += osxunwind
endif
endif

ifeq ($(OS), Linux)
ifeq ($(USE_SYSTEM_PATCHELF), 0)
STAGE1_DEPS += patchelf
PATCHELF:=$(build_bindir)/patchelf
else
PATCHELF:=patchelf
endif
endif

## USE_SYSTEM_LIBS options

ifeq ($(USE_SYSTEM_OPENLIBM), 0)
ifeq ($(USE_SYSTEM_LIBM), 0)
STAGE1_DEPS += openlibm
endif
endif

ifeq ($(USE_SYSTEM_OPENSPECFUN), 0)
STAGE1_DEPS += openspecfun
endif

ifeq ($(USE_SYSTEM_DSFMT), 0)
STAGE1_DEPS += dsfmt
endif

ifeq ($(USE_SYSTEM_LLVM), 0)
STAGE1_DEPS += llvm
endif

ifeq ($(USE_SYSTEM_PCRE), 0)
STAGE1_DEPS += pcre
endif

ifeq ($(USE_SYSTEM_BLAS), 0)
STAGE1_DEPS += openblas
ifeq ($(USE_BLAS64), 1)
ifeq ($(OS), Darwin)
STAGE1_DEPS += objconv
endif
endif
endif

ifeq ($(USE_GPL_LIBS), 1)
ifeq ($(USE_SYSTEM_FFTW), 0)
STAGE1_DEPS += fftw
endif
endif

ifeq ($(USE_SYSTEM_GMP), 0)
STAGE1_DEPS += gmp
endif

ifeq ($(USE_SYSTEM_LIBGIT2), 0)
STAGE1_DEPS += libgit2
endif

ifeq ($(USE_SYSTEM_MPFR), 0)
STAGE2_DEPS += mpfr
endif

ifeq ($(USE_SYSTEM_ARPACK), 0)
STAGE2_DEPS += arpack
endif

ifeq ($(USE_GPL_LIBS), 1)
ifeq ($(USE_SYSTEM_SUITESPARSE), 0)
STAGE2_DEPS += suitesparse
endif
endif

ifeq ($(USE_SYSTEM_UTF8PROC), 0)
STAGE2_DEPS += utf8proc
endif

# Only compile standalone LAPACK if we are not using OpenBLAS.
# OpenBLAS otherwise compiles LAPACK as part of its build.
# This is useful where one wants to use the vendor BLAS, but
# build LAPACK as the vendor LAPACK may be too old (eg. Apple vecLib)
ifeq ($(USE_SYSTEM_BLAS), 1)
ifeq ($(USE_SYSTEM_LAPACK), 0)
STAGE2_DEPS += lapack
endif
endif

#Platform specific flags

ifeq ($(OS), WINNT)
LIBTOOL_CCLD := CCLD="$(CC) -no-undefined -avoid-version"
endif

## Common build target prefixes

DEP_LIBS := $(STAGE1_DEPS) $(STAGE2_DEPS) $(STAGE3_DEPS)

default: install | $(build_prefix)
get: $(addprefix get-, $(DEP_LIBS))
configure: $(addprefix configure-, $(DEP_LIBS))
compile: $(addprefix compile-, $(DEP_LIBS))
check: $(addprefix check-, $(DEP_LIBS))
fastcheck: $(addprefix fastcheck-, $(DEP_LIBS))
stage: $(addprefix stage-, $(DEP_LIBS))
install: $(addprefix install-, $(DEP_LIBS))
cleanall: $(addprefix clean-, $(DEP_LIBS))
distcleanall: $(addprefix distclean-, $(DEP_LIBS))
	rm -rf $(build_prefix)
getall: get-llvm get-libuv get-pcre get-openlibm get-openspecfun get-dsfmt get-Rmath-julia get-openblas get-lapack get-fftw get-suitesparse get-arpack get-unwind get-osxunwind get-gmp get-mpfr get-patchelf get-utf8proc get-virtualenv get-objconv get-libgit2

## PATHS ##
# sort is used to remove potential duplicates
DIRS := $(sort $(build_bindir) $(build_libdir) $(build_includedir) $(build_sysconfdir) $(build_datarootdir) $(build_staging))

$(foreach dir,$(DIRS),$(eval $(call dir_target,$(dir))))

$(build_prefix): | $(DIRS)
$(eval $(call dir_target,$(SRCDIR)/srccache))

## A rule for calling `make install` ##
# example usage:
#   $(call staged-install, \
#       1 target, \               # name
#       2 rel-build-directory, \  # BUILDDIR-relative path to binaries
#       3 MAKE_INSTALL, \         # will be called with args SRCDIR,DESTDIR,$4
#       4 add-args, \             # extra args for $3
#       5 target-file, \          # installation-target file
#       6 post-install)           # post-install commands
#
# this rule ensures that make install is more nearly atomic
# so it's harder to get half-installed (or half-reinstalled) dependencies
# and enables sharing deps compiles, uninstall, and fast reinstall
MAKE_INSTALL = +$$(MAKE) -C $1 install $$(MAKE_COMMON) $3 DESTDIR="$2"
define LIBFILE_INSTALL
	mkdir -p $2/$$(build_libdir)
	cp $3 $2/$$(build_libdir)
endef
define BINFILE_INSTALL
	mkdir -p $2/$$(build_bindir)
	cp $3 $2/$$(build_bindir)
endef
define staged-install
stage-$(strip $1): $$(build_staging)/$2.tgz
install-$(strip $1): $5
$$(build_staging)/$2.tgz: # TODO: add get-$(strip $1)
	$$(MAKE) compile-$(strip $1)
	$$(MAKE) fastcheck-$(strip $1)
	rm -rf $$(build_staging)/$2
	mkdir -p $$(build_staging)/$2$$(build_prefix)
	$(call $3,$$(BUILDDIR)/$2,$$(build_staging)/$2,$4)
	cd $$(build_staging)/$2$$(build_prefix) && tar -czf $$@ .
$5: $$(build_staging)/$2.tgz
	mkdir -p $$(build_prefix)
	tar -xzf $$< -C $$(build_prefix)
	$6
	touch -c $$@
endef

## A rule for making a git-external dependency ##
# call syntax:
#   $(eval $(call git-external,dirname,VARNAME,file_from_download,file_from_compile,SRCDIR)
# dirname is the folder name to create
# VARNAME is the uppercased variable name prefix
# file_from_download is the name of a file inside the git repo to indicate when it's cloned (relative to dirname)
# file_from_compile is the rule to run to compile the dependency (relative to dirname)
# SRCDIR is either $(SRCDIR)/srccache or $(BUILDDIR), depending on whether the target supports out-of-tree builds
#
# also, in a file named dirname.version, define variables VARNAME_BRANCH and VARNAME_SHA1
#
# also, define a Makefile variable VARNAME_GIT_URL to use as the clone origin
# and a Makefile variable VARNAME_TAR_URL to use as the clone origin when NO_GIT is set
# this will pass along the VARNAME_SHA1 target in $1 for its use
#
# this defines rules for:
#   VARNAME_SRC_DIR = source directory for the output, relative to $(SRCDIR)/srccache or $(BUILDDIR)
#   VARNAME_SRC_FILE = target file for make get-VARNAME target
#   dirname:
#   dirname/file_from_download:
#   distclean-dirname:
#
define git-external
include $(SRCDIR)/$1.version

ifneq ($(NO_GIT),1)
$2_SRC_DIR := $1
$2_SRC_FILE := $$(SRCDIR)/srccache/$1.git
$$($2_SRC_FILE)/HEAD: | $$(SRCDIR)/srccache
	git clone -q --mirror --branch $$($2_BRANCH) $$($2_GIT_URL) $$(dir $$@)
$5/$1/.git/HEAD: | $$($2_SRC_FILE)/HEAD
	# try to update the cache, if that fails, attempt to continue anyways (the ref might already be local)
	-cd $$($2_SRC_FILE) && git fetch -q $$($2_GIT_URL) $$($2_BRANCH):remotes/origin/$$($2_BRANCH)
	git clone -q --depth=10 --branch $$($2_BRANCH) $$($2_SRC_FILE) $5/$1
	cd $5/$1 && git remote set-url origin $$($2_GIT_URL)
	touch -c $5/$1/$3
ifneq ($5,$$(BUILDDIR))
$$(BUILDDIR)/$1:
	mkdir -p $$@
$5/$1/$3: | $$(BUILDDIR)/$1
endif
$5/$1/$3: $$(SRCDIR)/$1.version | $5/$1/.git/HEAD
	# try to update the cache, if that fails, attempt to continue anyways (the ref might already be local)
	-cd $$(SRCDIR)/srccache/$1.git && git fetch -q $$($2_GIT_URL) $$($2_BRANCH):remotes/origin/$$($2_BRANCH)
	cd $5/$1 && git fetch -q $$(SRCDIR)/srccache/$1.git $$($2_BRANCH):remotes/origin/$$($2_BRANCH)
	cd $5/$1 && git checkout -q --detach $$($2_SHA1)
	@[ '$$($2_SHA1)' = "$$$$(cd $5/$1 && git show -s --format='%H' HEAD)" ] || echo $$(WARNCOLOR)'==> warning: SHA1 hash did not match $1.version file'$$(ENDCOLOR)
	touch -c $$@
$5/$1/$4: $5/$1/.git/HEAD
$$($2_SRC_FILE): | $$($2_SRC_FILE)/HEAD
	touch -c $$@

else # NO_GIT

$2_SRC_DIR := $1-$$($2_SHA1)
$2_SRC_FILE := $$(SRCDIR)/srccache/$$($2_SRC_DIR).tar.gz
$$($2_SRC_FILE): | $$(SRCDIR)/srccache
	$$(JLDOWNLOAD) $$@ $$(call $2_TAR_URL,$$($2_SHA1))
$5/$$($2_SRC_DIR)/$3: $$($2_SRC_FILE)
	$$(JLCHECKSUM) $$<
	mkdir -p $$(dir $$@) && \
	$(TAR) -C $$(dir $$@) --strip-components 1 -xf $$<
	touch -c $$@
endif # NO_GIT

distclean-$1:
	-rm -rf $5/$$($2_SRC_DIR) $$($2_SRC_FILE) $$(BUILDDIR)/$1
endef


## LLVM ##
LLVM_GIT_URL_BASE ?= http://llvm.org/git
LLVM_GIT_URL_LLVM ?= $(LLVM_GIT_URL_BASE)/llvm.git
LLVM_GIT_URL_CLANG ?= $(LLVM_GIT_URL_BASE)/clang.git
LLVM_GIT_URL_COMPILER_RT ?= $(LLVM_GIT_URL_BASE)/compiler-rt.git
LLVM_GIT_URL_LLDB ?= $(LLVM_GIT_URL_BASE)/lldb.git
LLVM_GIT_URL_LIBCXX ?= $(LLVM_GIT_URL_BASE)/libcxx.git
LLVM_GIT_URL_LIBCXXABI ?= $(LLVM_GIT_URL_BASE)/libcxxabi.git

ifeq ($(BUILD_LLDB), 1)
BUILD_LLVM_CLANG := 1
# because it's a build requirement
endif

ifeq ($(LLVM_DEBUG),1)
LLVM_BUILDTYPE := Debug
else
LLVM_BUILDTYPE := Release
endif
ifeq ($(LLVM_ASSERTIONS),1)
LLVM_BUILDTYPE := $(LLVM_BUILDTYPE)+Asserts
endif
LLVM_FLAVOR := $(LLVM_BUILDTYPE)
ifeq ($(LLVM_SANITIZE),1)
ifeq ($(SANITIZE_MEMORY),1)
LLVM_BUILDTYPE := $(LLVM_BUILDTYPE)+MSAN
else
LLVM_BUILDTYPE := $(LLVM_BUILDTYPE)+ASAN
endif
endif

LLVM_SRC_DIR:=$(SRCDIR)/srccache/llvm-$(LLVM_VER)
LLVM_BUILD_DIR:=$(BUILDDIR)/llvm-$(LLVM_VER)
LLVM_LIB_FILE := libLLVMCodeGen.a
ifeq ($(LLVM_USE_CMAKE),1)
LLVM_OBJ_SOURCE := $(LLVM_BUILD_DIR)/build_$(LLVM_BUILDTYPE)/lib/$(LLVM_LIB_FILE)
else
LLVM_OBJ_SOURCE := $(LLVM_BUILD_DIR)/build_$(LLVM_BUILDTYPE)/$(LLVM_FLAVOR)/lib/$(LLVM_LIB_FILE)
endif
LLVM_OBJ_TARGET := $(build_libdir)/$(LLVM_LIB_FILE)

ifneq ($(LLVM_VER),svn)
ifeq ($(LLVM_VER), 3.3)
LLVM_TAR:=$(SRCDIR)/srccache/llvm-$(LLVM_VER).src.tar.gz
else
LLVM_TAR:=$(SRCDIR)/srccache/llvm-$(LLVM_VER).src.tar.xz
endif

ifeq ($(BUILD_LLDB),1)
ifeq ($(LLVM_VER), 3.3)
LLVM_LLDB_TAR:=$(SRCDIR)/srccache/lldb-$(LLVM_VER).src.tar.gz
else
LLVM_LLDB_TAR:=$(SRCDIR)/srccache/lldb-$(LLVM_VER).src.tar.xz
endif # LLVM_VER == 3.3
endif # BUILD_LLDB

ifeq ($(BUILD_LLVM_CLANG),1)
ifeq ($(LLVM_VER), 3.3)
LLVM_CLANG_TAR:=$(SRCDIR)/srccache/cfe-$(LLVM_VER).src.tar.gz
LLVM_COMPILER_RT_TAR:=$(SRCDIR)/srccache/compiler-rt-$(LLVM_VER).src.tar.gz
else
LLVM_CLANG_TAR:=$(SRCDIR)/srccache/cfe-$(LLVM_VER).src.tar.xz
LLVM_COMPILER_RT_TAR:=$(SRCDIR)/srccache/compiler-rt-$(LLVM_VER).src.tar.xz
endif # LLVM_VER
else
LLVM_CLANG_TAR:=
LLVM_COMPILER_RT_TAR:=
LLVM_LIBCXX_TAR:=
endif # BUILD_LLVM_CLANG

ifeq ($(BUILD_CUSTOM_LIBCXX),1)
LLVM_LIBCXX_TAR:=$(SRCDIR)/srccache/libcxx-$(LLVM_VER).src.tar.gz
endif
endif # LLVM_VER != svn

LLVM_CXXFLAGS := $(CXXFLAGS)
LLVM_CPPFLAGS := $(CPPFLAGS)
LLVM_LDFLAGS := $(LDFLAGS)
LLVM_TARGETS := host
LLVM_TARGET_FLAGS := --enable-targets=$(LLVM_TARGETS)
LLVM_CMAKE += -DLLVM_TARGETS_TO_BUILD:STRING="$(LLVM_TARGETS)" -DCMAKE_BUILD_TYPE="$(LLVM_BUILDTYPE)" -DLLVM_BUILD_LLVM_DYLIB:BOOL=ON -DLLVM_DYLIB_EXPORT_ALL:BOOL=ON
LLVM_FLAGS += --disable-profiling --enable-shared --enable-static $(LLVM_TARGET_FLAGS) --disable-bindings --disable-docs
# LLVM has weird install prefixes (see llvm-$(LLVM_VER)/build_$(LLVM_BUILDTYPE)/Makefile.config for the full list)
# We map them here to the "normal" ones, which means just prefixing "PROJ_" to the variable name.
LLVM_MFLAGS := PROJ_libdir=$(build_libdir) PROJ_bindir=$(build_bindir) PROJ_includedir=$(build_includedir)
ifeq ($(LLVM_ASSERTIONS), 1)
LLVM_FLAGS += --enable-assertions
LLVM_CMAKE += -DLLVM_ENABLE_ASSERTIONS:BOOL=ON
ifeq ($(OS), WINNT)
LLVM_FLAGS += --disable-embed-stdcxx
endif # OS == WINNT
else
LLVM_FLAGS += --disable-assertions
endif # LLVM_ASSERTIONS
ifeq ($(LLVM_DEBUG), 1)
LLVM_FLAGS += --disable-optimized --enable-debug-symbols --enable-keep-symbols
else
LLVM_FLAGS += --enable-optimized
endif # LLVM_DEBUG
ifeq ($(USE_LIBCPP), 1)
LLVM_FLAGS += --enable-libcpp
endif # USE_LIBCPP
ifeq ($(OS), WINNT)
LLVM_FLAGS += --with-extra-ld-options="-Wl,--stack,8388608" LDFLAGS=""
# turn off shared LLVM on Windows unless explicitly requested
ifneq ($(USE_LLVM_SHLIB),1)
LLVM_FLAGS += --disable-shared
endif
LLVM_CPPFLAGS += -D__USING_SJLJ_EXCEPTIONS__ -D__CRT__NO_INLINE
endif # OS == WINNT
ifeq ($(USE_INTEL_JITEVENTS), 1)
LLVM_FLAGS += --with-intel-jitevents
ifeq ($(OS), WINNT)
LLVM_FLAGS += --disable-threads
endif # OS == WINNT
else
LLVM_FLAGS += --disable-threads
endif # USE_INTEL_JITEVENTS

ifeq ($(USE_OPROFILE_JITEVENTS), 1)
LLVM_FLAGS += --with-oprofile=/usr/
endif # USE_OPROFILE_JITEVENTS

ifeq ($(BUILD_LLDB),1)
ifeq ($(USECLANG),1)
LLVM_FLAGS += --enable-cxx11
else
LLVM_CXXFLAGS += -std=c++0x
endif # USECLANG
ifeq ($(LLDB_DISABLE_PYTHON),1)
LLVM_CXXFLAGS += -DLLDB_DISABLE_PYTHON
LLVM_CMAKE += -DLLDB_DISABLE_PYTHON=ON
endif # LLDB_DISABLE_PYTHON
endif # BUILD_LLDB

ifeq ($(ARCH), ppc64)
LLVM_CXXFLAGS += -mminimal-toc
endif # ARCH == ppc64


ifeq ($(LLVM_SANITIZE),1)
ifeq ($(SANITIZE_MEMORY),1)
LLVM_CC := CFLAGS="$(CFLAGS) -fsanitize=memory -fsanitize-memory-track-origins"
LLVM_LDFLAGS += -fsanitize=memory -fsanitize-memory-track-origins
LLVM_CXXFLAGS += -fsanitize=memory -fsanitize-memory-track-origins
LLVM_CMAKE += -DLLVM_USE_SANITIZER="MemoryWithOrigins"
else
LLVM_CC := CFLAGS="$(CFLAGS) -fsanitize=address"
LLVM_LDFLAGS += -fsanitize=address
LLVM_CXXFLAGS += -fsanitize=address
LLVM_CMAKE += -DLLVM_USE_SANITIZER="Address"
endif
LLVM_MFLAGS += TOOL_NO_EXPORTS= HAVE_LINK_VERSION_SCRIPT=0
else
LLVM_CC :=
endif # LLVM_SANITIZE

ifneq ($(LLVM_CXXFLAGS),)
LLVM_FLAGS += CXXFLAGS="$(LLVM_CXXFLAGS)"
LLVM_MFLAGS += CXXFLAGS="$(LLVM_CXXFLAGS)"
endif # LLVM_CXXFLAGS
LLVM_MFLAGS += $(LLVM_CC)

ifeq ($(BUILD_CUSTOM_LIBCXX),1)
LLVM_LDFLAGS += -Wl,-R$(build_libdir) -lc++ -lc++abi
ifeq ($(USEICC),1)
LLVM_LDFLAGS += -no_cpprt
endif # USEICC
endif # BUILD_CUSTOM_LIBCXX

ifneq ($(LLVM_CPPFLAGS),)
LLVM_FLAGS += CPPFLAGS="$(LLVM_CPPFLAGS)"
LLVM_MFLAGS += CPPFLAGS="$(LLVM_CPPFLAGS)"
endif
ifneq ($(LLVM_LDFLAGS),)
LLVM_FLAGS += LDFLAGS="$(LLVM_LDFLAGS)"
LLVM_MFLAGS += LDFLAGS="$(LLVM_LDFLAGS)"
endif

ifeq ($(BUILD_LLVM_CLANG),1)
LLVM_MFLAGS += OPTIONAL_PARALLEL_DIRS=clang
else
# block default building of Clang
LLVM_MFLAGS += OPTIONAL_PARALLEL_DIRS=
endif
ifeq ($(BUILD_LLDB),1)
LLVM_MFLAGS += OPTIONAL_DIRS=lldb
else
# block default building of lldb
LLVM_MFLAGS += OPTIONAL_DIRS=
endif

ifneq ($(LLVM_CLANG_TAR),)
$(LLVM_CLANG_TAR): | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ http://llvm.org/releases/$(LLVM_VER)/$(notdir $@)
endif
ifneq ($(LLVM_COMPILER_RT_TAR),)
$(LLVM_COMPILER_RT_TAR): | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ http://llvm.org/releases/$(LLVM_VER)/$(notdir $@)
endif

ifneq ($(LLVM_LIBCXX_TAR),)
$(LLVM_LIBCXX_TAR): | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ http://llvm.org/releases/$(LLVM_VER)/$(notdir $@)
endif
ifneq ($(LLVM_VER),svn)
$(LLVM_TAR): | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ http://llvm.org/releases/$(LLVM_VER)/$(notdir $@)
endif

ifneq ($(LLVM_LLDB_TAR),)
$(LLVM_LLDB_TAR): | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ http://llvm.org/releases/$(LLVM_VER)/$(notdir $@)
endif
ifeq ($(BUILD_LLDB),1)
$(LLVM_SRC_DIR)/tools/lldb:
$(LLVM_SRC_DIR)/configure: $(LLVM_SRC_DIR)/tools/lldb
endif

# LLDB still relies on plenty of python 2.x infrastructure, without checking
llvm_python_workaround=$(SRCDIR)/srccache/python2_path
$(llvm_python_workaround):
	mkdir -p $@
	-python -c 'import sys; sys.exit(not sys.version_info > (3, 0))' && \
	/usr/bin/python2 -c 'import sys; sys.exit(not sys.version_info < (3, 0))' && \
	ln -sf /usr/bin/python2 "$@/python" && \
	ln -sf /usr/bin/python2-config "$@/python-config"
LLVM_FLAGS += --with-python="$(shell $(SRCDIR)/find_python2)"

ifeq ($(BUILD_CUSTOM_LIBCXX),1)

ifeq ($(USEICC),1)
LIBCXX_EXTRA_FLAGS := -Bstatic -lirc -Bdynamic
endif

$(LLVM_SRC_DIR)/projects/libcxx: $(LLVM_LIBCXX_TAR) | $(LLVM_SRC_DIR)/configure
	([ ! -d $@ ] && \
	git clone $(LLVM_GIT_URL_LIBCXX) $@  ) || \
	(cd $@  && \
	git pull --ff-only)
$(LLVM_SRC_DIR)/projects/libcxx/.git/HEAD: | $(LLVM_SRC_DIR)/projects/libcxx/.git/HEAD
$(LLVM_SRC_DIR)/projects/libcxxabi: $(LLVM_LIBCXXABI_TAR) | $(LLVM_SRC_DIR)/configure
	([ ! -d $@ ] && \
	git clone $(LLVM_GIT_URL_LIBCXXABI) $@ ) || \
	(cd $@ && \
	git pull --ff-only)
$(LLVM_SRC_DIR)/projects/libcxxabi/.git/HEAD: | $(LLVM_SRC_DIR)/projects/libcxxabi
$(LLVM_BUILD_DIR)/libcxx-build/Makefile: | $(LLVM_SRC_DIR)/projects/libcxx $(LLVM_SRC_DIR)/projects/libcxxabi
	mkdir -p $(dir $@)
	cd $(dir $@) && \
		$(CMAKE) -G "Unix Makefiles" $(CMAKE_COMMON) $(LLVM_CMAKE) -DLIBCXX_CXX_ABI=libcxxabi -DLIBCXX_CXX_ABI_INCLUDE_PATHS="$(LLVM_SRC_DIR)/projects/libcxxabi/include" $(LLVM_SRC_DIR)/projects/libcxx -DCMAKE_SHARED_LINKER_FLAGS="-L$(build_libdir) $(LIBCXX_EXTRA_FLAGS)" -DCMAKE_CXX_FLAGS="$(CXXFLAGS)"
$(LLVM_BUILD_DIR)/libcxxabi-build/Makefile: | $(LLVM_SRC_DIR)/projects/libcxxabi llvm-$(LLVM_VER)/projects/libcxx
	mkdir -p $(dir $@)
	cd $(dir $@) && \
        $(CMAKE) -G "Unix Makefiles" $(CMAKE_COMMON) $(LLVM_CMAKE) -DLLVM_ABI_BREAKING_CHECKS="WITH_ASSERTS" -DLLVM_PATH="$(LLVM_SRC_DIR)" $(LLVM_SRC_DIR)/projects/libcxxabi -DLIBCXXABI_CXX_ABI_LIBRARIES="$(LIBCXX_EXTRA_FLAGS)" -DCMAKE_CXX_FLAGS="$(CXXFLAGS) -std=c++11"
$(LLVM_BUILD_DIR)/libcxxabi-build/lib/libc++abi.so.1.0: $(LLVM_BUILD_DIR)/libcxxabi-build/Makefile $(LLVM_SRC_DIR)/projects/libcxxabi/.git/HEAD
	$(MAKE) -C $(LLVM_BUILD_DIR)/libcxxabi-build
	touch -c $@
$(build_libdir)/libc++abi.so.1.0: $(LLVM_BUILD_DIR)/libcxxabi-build/lib/libc++abi.so.1.0
	$(MAKE) -C $(LLVM_BUILD_DIR)/libcxxabi-build install
	touch -c $@
$(LLVM_BUILD_DIR)/libcxx-build/lib/libc++.so.1.0: $(build_libdir)/libc++abi.so.1.0 $(LLVM_BUILD_DIR)/libcxx-build/Makefile $(LLVM_SRC_DIR)/projects/libcxx/.git/HEAD
	$(MAKE) -C $(LLVM_BUILD_DIR)/libcxx-build
$(build_libdir)/libc++.so.1.0: $(LLVM_BUILD_DIR)/libcxx-build/lib/libc++.so.1.0
	$(MAKE) -C $(LLVM_BUILD_DIR)/libcxx-build install
	touch -c $@
get-libcxx: $(LLVM_SRC_DIR)/projects/libcxx
get-libcxxabi: $(LLVM_SRC_DIR)/projects/libcxxabi
install-libcxxabi: $(build_libdir)/libc++abi.so.1.0
install-libcxx: $(build_libdir)/libc++.so.1.0
endif

ifeq ($(BUILD_CUSTOM_LIBCXX),1)
LIBCXX_DEPENDENCY := $(build_libdir)/libc++abi.so.1.0 $(build_libdir)/libc++.so.1.0
get-llvm: get-libcxx get-libcxxabi
endif

$(LLVM_SRC_DIR)/configure: $(LLVM_TAR) $(LLVM_CLANG_TAR) $(LLVM_COMPILER_RT_TAR) $(LLVM_LIBCXX_TAR) $(LLVM_LLDB_TAR)
ifneq ($(LLVM_CLANG_TAR),)
	$(JLCHECKSUM) $(LLVM_CLANG_TAR)
endif
ifneq ($(LLVM_COMPILER_RT_TAR),)
	$(JLCHECKSUM) $(LLVM_COMPILER_RT_TAR)
endif
ifneq ($(LLVM_LIBCXX_TAR),)
	$(JLCHECKSUM) $(LLVM_LIBCXX_TAR)
endif
ifneq ($(LLVM_VER),svn)
	$(JLCHECKSUM) $(LLVM_TAR)
endif
ifneq ($(LLVM_LLDB_TAR),)
	$(JLCHECKSUM) $(LLVM_LLDB_TAR)
endif
ifneq ($(LLVM_VER),svn)
	mkdir -p $(LLVM_SRC_DIR)
	$(TAR) -C $(LLVM_SRC_DIR) --strip-components 1 -xf $(LLVM_TAR)
else
	([ ! -d $(LLVM_SRC_DIR) ] && \
		git clone $(LLVM_GIT_URL_LLVM) $(LLVM_SRC_DIR) ) || \
		(cd $(LLVM_SRC_DIR) && \
		git pull --ff-only)
ifneq ($(LLVM_GIT_VER),)
	(cd $(LLVM_SRC_DIR) && \
		git checkout $(LLVM_GIT_VER))
endif # LLVM_GIT_VER
endif # LLVM_VER
ifneq ($(LLVM_VER),svn)
ifneq ($(LLVM_CLANG_TAR),)
	mkdir -p $(LLVM_SRC_DIR)/tools/clang
	$(TAR) -C $(LLVM_SRC_DIR)/tools/clang --strip-components 1 -xf $(LLVM_CLANG_TAR)
endif # LLVM_CLANG_TAR
ifneq ($(LLVM_COMPILER_RT_TAR),)
	mkdir -p $(LLVM_SRC_DIR)/projects/compiler-rt
	$(TAR) -C $(LLVM_SRC_DIR)/projects/compiler-rt --strip-components 1 -xf $(LLVM_COMPILER_RT_TAR)
endif # LLVM_COMPILER_RT_TAR
ifneq ($(LLVM_LLDB_TAR),)
	mkdir -p $(LLVM_SRC_DIR)/tools/lldb
	$(TAR) -C $(LLVM_SRC_DIR)/tools/lldb --strip-components 1 -xf $(LLVM_LLDB_TAR)
endif # LLVM_LLDB_TAR
else # LLVM_VER
ifeq ($(BUILD_LLVM_CLANG),1)
	([ ! -d $(LLVM_SRC_DIR)/tools/clang ] && \
		git clone $(LLVM_GIT_URL_CLANG) $(LLVM_SRC_DIR)/tools/clang  ) || \
		(cd $(LLVM_SRC_DIR)/tools/clang  && \
		git pull --ff-only)
	([ ! -d $(LLVM_SRC_DIR)/projects/compiler-rt ] && \
		git clone $(LLVM_GIT_URL_COMPILER_RT) $(LLVM_SRC_DIR)/projects/compiler-rt  ) || \
		(cd $(LLVM_SRC_DIR)/projects/compiler-rt  && \
		git pull --ff-only)
ifneq ($(LLVM_GIT_VER_CLANG),)
	(cd $(LLVM_SRC_DIR)/tools/clang && \
		git checkout $(LLVM_GIT_VER_CLANG))
endif # LLVM_GIT_VER_CLANG
endif # BUILD_LLVM_CLANG
ifeq ($(BUILD_LLDB),1)
	([ ! -d $(LLVM_SRC_DIR)/tools/lldb ] && \
		git clone $(LLVM_GIT_URL_LLDB) $(LLVM_SRC_DIR)/tools/lldb  ) || \
		(cd $(LLVM_SRC_DIR)/tools/lldb  && \
		git pull --ff-only)
ifneq ($(LLVM_GIT_VER_LLDB),)
	(cd $(LLVM_SRC_DIR)/tools/lldb && \
		git checkout $(LLVM_GIT_VER_LLDB))
endif # LLVM_GIT_VER_CLANG
endif # BUILD_LLDB
endif # LLVM_VER
	touch -c $@

# Apply version-specific LLVM patches
LLVM_PATCH_LIST:=
define LLVM_PATCH
$$(LLVM_SRC_DIR)/$1.patch-applied: $(LLVM_SRC_DIR)/configure | $$(SRCDIR)/$1.patch
	cd $$(LLVM_SRC_DIR) && patch -p1 < $$(SRCDIR)/$1.patch
	echo 1 > $$@
LLVM_PATCH_LIST += $$(LLVM_SRC_DIR)/$1.patch-applied
endef
ifeq ($(LLVM_VER),3.3)
$(eval $(call LLVM_PATCH,llvm-3.3))
$(eval $(call LLVM_PATCH,instcombine-llvm-3.3))
$(eval $(call LLVM_PATCH,int128-vector.llvm-3.3))
$(eval $(call LLVM_PATCH,osx-10.10.llvm-3.3))
$(eval $(call LLVM_PATCH,win64-int128.llvm-3.3))
else ifeq ($(LLVM_VER),3.6.0)
$(eval $(call LLVM_PATCH,zerosign-llvm-3.6.0))
$(eval $(call LLVM_PATCH,win64-allocas-llvm-3.6.0))
endif # LLVM_VER

LLVM_BUILDDIR_withtype := $(LLVM_BUILD_DIR)/build_$(LLVM_BUILDTYPE)
ifneq ($(LLVM_USE_CMAKE),)

$(LLVM_BUILDDIR_withtype)/CMakeCache.txt: $(LLVM_SRC_DIR)/configure | $(llvm_python_workaround) $(LIBCXX_DEPENDENCY)
	mkdir -p $(dir $@)
	cd $(dir $@) && \
		export PATH=$(llvm_python_workaround):$$PATH && \
		$(CMAKE) $(LLVM_SRC_DIR) $(CMAKE_GENERATOR_COMMAND) $(CMAKE_COMMON) $(LLVM_CMAKE)
	touch -c $@

$(LLVM_OBJ_SOURCE): $(LLVM_BUILDDIR_withtype)/CMakeCache.txt $(LLVM_PATCH_LIST) | $(llvm_python_workaround)
	cd $(LLVM_BUILDDIR_withtype) && \
		export PATH=$(llvm_python_workaround):$$PATH && \
		$(CMAKE) --build .
	touch -c $@

else

$(LLVM_BUILDDIR_withtype)/config.status: $(LLVM_SRC_DIR)/configure | $(llvm_python_workaround) $(LIBCXX_DEPENDENCY)
	mkdir -p $(dir $@)
	cd $(dir $@) && \
		export PATH=$(llvm_python_workaround):$$PATH && \
		$< $(CONFIGURE_COMMON) $(LLVM_CC) $(LLVM_FLAGS)
	touch -c $@

$(LLVM_OBJ_SOURCE): $(LLVM_BUILDDIR_withtype)/config.status $(LLVM_PATCH_LIST) | $(llvm_python_workaround)
	cd $(LLVM_BUILDDIR_withtype) && \
		export PATH=$(llvm_python_workaround):$$PATH && \
		$(MAKE) $(LLVM_MFLAGS) $(MAKE_COMMON)
	touch -c $@

endif # LLVM_USE_CMAKE

ifeq ($(LLVM_USE_CMAKE),1)
CHECK_COMMAND := $(CMAKE) --build . check
else
CHECK_COMMAND := $(MAKE) $(LLVM_MFLAGS) check
endif
$(LLVM_BUILDDIR_withtype)/checked: $(LLVM_OBJ_SOURCE) | $(llvm_python_workaround)
ifeq ($(OS),$(BUILD_OS))
	cd $(LLVM_BUILDDIR_withtype) && \
		export PATH=$(llvm_python_workaround):$$PATH && \
		$(CHECK_COMMAND)
endif
	echo 1 > $@

ifeq ($(LLVM_USE_CMAKE),1)
CMAKE_INSTALL_LLVM = \
	cd $1 && $(CMAKE) -DCMAKE_INSTALL_PREFIX="$2" -P cmake_install.cmake)
$(eval $(call staged-install,llvm,llvm-$(LLVM_VER)/build_$(LLVM_BUILDTYPE), \
	CMAKE_INSTALL_LLVM,, \
	$(LLVM_OBJ_TARGET),)))
else
$(eval $(call staged-install, \
	llvm,llvm-$$(LLVM_VER)/build_$$(LLVM_BUILDTYPE), \
	MAKE_INSTALL,$$(LLVM_MFLAGS) PATH="$$(llvm_python_workaround):$$$$PATH", \
	$$(LLVM_OBJ_TARGET),))
stage-llvm: | $(llvm_python_workaround)
endif # LLVM_USE_CMAKE

reinstall-llvm:
	-rm $(LLVM_OBJ_TARGET)
	$(MAKE) -f $(SRCDIR)/Makefile -s install-llvm

clean-llvm:
	-$(MAKE) -C $(LLVM_BUILDDIR_withtype) clean
	-rm -f $(build_bindir)/llvm-config
distclean-llvm:
	-rm -rf $(LLVM_TAR) $(LLVM_CLANG_TAR) \
		$(LLVM_COMPILER_RT_TAR) $(LLVM_LIBCXX_TAR) $(LLVM_LLDB_TAR) \
		$(LLVM_SRC_DIR) $(LLVM_BUILDDIR_withtype)

ifneq ($(LLVM_VER),svn)
get-llvm: $(LLVM_TAR) $(LLVM_CLANG_TAR) $(LLVM_COMPILER_RT_TAR) $(LLVM_LIBCXX_TAR) $(LLVM_LLDB_TAR)
else
get-llvm: $(LLVM_SRC_DIR)/configure
endif
configure-llvm: $(LLVM_BUILD_DIR)/build_$(LLVM_BUILDTYPE)/config.status
compile-llvm: $(LLVM_OBJ_SOURCE)
check-llvm: $(LLVM_BUILD_DIR)/build_$(LLVM_BUILDTYPE)/checked
fastcheck-llvm: #none
#todo: LLVM make check target is broken on julia.mit.edu (and really slow elsewhere)

ifeq ($(LLVM_VER),svn)
update-llvm:
	(cd $(LLVM_SRC_DIR); git pull --ff-only)
	([ -d "$(LLVM_SRC_DIR)/tools/clang"  ] || exit 0;          cd $(LLVM_SRC_DIR)/tools/clang; git pull --ff-only)
	([ -d "$(LLVM_SRC_DIR)/projects/compiler-rt" ] || exit 0;  cd $(LLVM_SRC_DIR)/projects/compiler-rt; git pull --ff-only)
	([ -d "$(LLVM_SRC_DIR)/tools/lldb" ] || exit 0;            cd $(LLVM_SRC_DIR)/tools/lldb; git pull --ff-only)
endif

## LIBUV ##
LIBUV_GIT_URL:=git://github.com/JuliaLang/libuv.git
LIBUV_TAR_URL=https://api.github.com/repos/JuliaLang/libuv/tarball/$1
$(eval $(call git-external,libuv,LIBUV,configure,.libs/libuv.la,$(SRCDIR)/srccache))

UV_SRC_TARGET := $(BUILDDIR)/$(LIBUV_SRC_DIR)/.libs/libuv.la
UV_OBJ_TARGET := $(build_libdir)/libuv.la

UV_CFLAGS :=
ifeq ($(USEMSVC), 1)
UV_CFLAGS += -DBUILDING_UV_SHARED
endif
ifeq ($(USEICC), 1)
UV_CFLAGS += -static-intel
endif

UV_MFLAGS += LDFLAGS="$(LDFLAGS) $(CLDFLAGS) -v"
ifneq ($(UV_CFLAGS),)
UV_MFLAGS += CFLAGS="$(CFLAGS) $(UV_CFLAGS)"
endif
ifneq ($(VERBOSE), 0)
UV_MFLAGS += V=1
endif
ifneq ($(USEMSVC), 1)
UV_FLAGS := $(UV_MFLAGS)
else
UV_FLAGS := --disable-shared $(UV_MFLAGS)
endif

$(BUILDDIR)/$(LIBUV_SRC_DIR)/config.status: $(SRCDIR)/srccache/$(LIBUV_SRC_DIR)/configure
	touch -c $(SRCDIR)/srccache/$(LIBUV_SRC_DIR)/aclocal.m4 # touch a few files to prevent autogen from getting called
	touch -c $(SRCDIR)/srccache/$(LIBUV_SRC_DIR)/Makefile.in
	touch -c $(SRCDIR)/srccache/$(LIBUV_SRC_DIR)/configure
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$< --with-pic $(CONFIGURE_COMMON) $(UV_FLAGS)
	touch -c $@
$(UV_SRC_TARGET): $(BUILDDIR)/$(LIBUV_SRC_DIR)/config.status
	$(MAKE) -C $(dir $<) $(UV_MFLAGS)
	touch -c $@
$(BUILDDIR)/$(LIBUV_SRC_DIR)/checked: $(UV_SRC_TARGET)
ifeq ($(OS),$(BUILD_OS))
	-$(MAKE) -C $(dir $@) check
endif
	echo 1 > $@

$(eval $(call staged-install, \
	libuv,$$(LIBUV_SRC_DIR), \
	MAKE_INSTALL,, \
	$$(UV_OBJ_TARGET), \
	$$(INSTALL_NAME_CMD)libuv.$$(SHLIB_EXT) $$(build_shlibdir)/libuv.$$(SHLIB_EXT)))

clean-libuv:
	-$(MAKE) -C $(BUILDDIR)/$(LIBUV_SRC_DIR) clean
	-rm -rf $(build_libdir)/libuv.a $(build_libdir)/libuv.la $(build_includedir)/libuv.h $(build_includedir)/libuv-private

get-libuv: $(LIBUV_SRC_FILE)
configure-libuv: $(BUILDDIR)/$(LIBUV_SRC_DIR)/config.status
compile-libuv: $(UV_SRC_TARGET)
check-libuv: $(BUILDDIR)/$(LIBUV_SRC_DIR)/checked
fastcheck-libuv: #none

## PCRE ##

PCRE_SRC_TARGET := $(BUILDDIR)/pcre2-$(PCRE_VER)/.libs/libpcre2-8.$(SHLIB_EXT)
PCRE_OBJ_TARGET := $(build_shlibdir)/libpcre2-8.$(SHLIB_EXT)

# Force optimization for PCRE flags (Issue #11668)
PCRE_CFLAGS := -O3
ifneq ($(OS),WINNT)
PCRE_LDFLAGS := "-Wl,-rpath,'$(build_libdir)'"
endif

$(SRCDIR)/srccache/pcre2-$(PCRE_VER).tar.bz2: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ https://ftp.csx.cam.ac.uk/pub/software/programming/pcre/pcre2-$(PCRE_VER).tar.bz2
$(SRCDIR)/srccache/pcre2-$(PCRE_VER)/configure: $(SRCDIR)/srccache/pcre2-$(PCRE_VER).tar.bz2
	$(JLCHECKSUM) $<
	cd $(dir $<) && $(TAR) jxf $(notdir $<)
	touch -c $@
$(BUILDDIR)/pcre2-$(PCRE_VER)/config.status: $(SRCDIR)/srccache/pcre2-$(PCRE_VER)/configure
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$< $(CONFIGURE_COMMON) --enable-jit --includedir=$(build_includedir) CFLAGS="$(CFLAGS) $(PCRE_CFLAGS)" LDFLAGS="$(LDFLAGS) $(PCRE_LDFLAGS)"
	touch -c $@
$(PCRE_SRC_TARGET): $(BUILDDIR)/pcre2-$(PCRE_VER)/config.status
	$(MAKE) -C $(dir $<) $(LIBTOOL_CCLD)
	touch -c $@
$(BUILDDIR)/pcre2-$(PCRE_VER)/checked: $(PCRE_SRC_TARGET)
ifeq ($(OS),$(BUILD_OS))
ifneq ($(OS),WINNT)
	$(MAKE) -C $(dir $@) check -j1
endif
endif
	echo 1 > $@

$(eval $(call staged-install, \
	pcre,pcre2-$$(PCRE_VER), \
	MAKE_INSTALL,$$$$(LIBTOOL_CCLD), \
	$$(PCRE_OBJ_TARGET), \
	$$(INSTALL_NAME_CMD)libpcre2-8.$$(SHLIB_EXT) $$@))

clean-pcre:
	-$(MAKE) -C pcre2-$(PCRE_VER) clean
	-rm -f $(build_shlibdir)/libpcre*
distclean-pcre:
	-rm -rf $(SRCDIR)/srccache/pcre2-$(PCRE_VER).tar.bz2 $(SRCDIR)/srccache/pcre2-$(PCRE_VER) $(BUILDDIR)/pcre2-$(PCRE_VER)

get-pcre: $(SRCDIR)/srccache/pcre2-$(PCRE_VER).tar.bz2
configure-pcre: $(BUILDDIR)/pcre2-$(PCRE_VER)/config.status
compile-pcre: $(PCRE_SRC_TARGET)
check-pcre: $(BUILDDIR)/pcre2-$(PCRE_VER)/checked
fastcheck-pcre: check-pcre


## openlibm ##
OPENLIBM_GIT_URL := git://github.com/JuliaLang/openlibm.git
OPENLIBM_TAR_URL = https://api.github.com/repos/JuliaLang/openlibm/tarball/$1
$(eval $(call git-external,openlibm,OPENLIBM,Makefile,libopenlibm.$(SHLIB_EXT),$(BUILDDIR)))

OPENLIBM_OBJ_TARGET := $(build_shlibdir)/libopenlibm.$(SHLIB_EXT) $(build_libdir)/libopenlibm.a
OPENLIBM_OBJ_SOURCE := $(BUILDDIR)/$(OPENLIBM_SRC_DIR)/libopenlibm.$(SHLIB_EXT)
OPENLIBM_FLAGS := ARCH="$(ARCH)" CC="$(CC)" FC="$(FC)" AR="$(AR)" OS="$(OS)" USECLANG=$(USECLANG) USEGCC=$(USEGCC)

$(OPENLIBM_OBJ_SOURCE): $(BUILDDIR)/$(OPENLIBM_SRC_DIR)/Makefile
	$(MAKE) -C $(dir $<) $(OPENLIBM_FLAGS) $(MAKE_COMMON)
	touch -c $@

$(build_shlibdir)/libopenlibm%$(SHLIB_EXT) $(build_libdir)/libopenlibm%a: $(OPENLIBM_OBJ_SOURCE)
	$(call make-install,$(OPENLIBM_SRC_DIR),$(OPENLIBM_FLAGS))
	touch -c $(OPENLIBM_OBJ_TARGET)

$(eval $(call staged-install, \
	openlibm,$$(OPENLIBM_SRC_DIR), \
	MAKE_INSTALL,$$(OPENLIBM_FLAGS), \
	$$(OPENLIBM_OBJ_TARGET), \
	$(INSTALL_NAME_CMD)libopenlibm.$(SHLIB_EXT) $(build_shlibdir)/libopenlibm.$(SHLIB_EXT)))

clean-openlibm:
	-$(MAKE) -C $(BUILDDIR)/$(OPENLIBM_SRC_DIR) distclean $(OPENLIBM_FLAGS)
	-rm $(OPENLIBM_OBJ_TARGET)
	-rm $(build_libdir)/libopenlibm.a

get-openlibm: $(OPENLIBM_SRC_FILE)
configure-openlibm: $(BUILDDIR)/$(OPENLIBM_SRC_DIR)/Makefile
compile-openlibm: $(OPENLIBM_OBJ_SOURCE)
check-openlibm: compile-openlibm
fastcheck-openlibm: check-openlibm


## openspecfun ##

OPENSPECFUN_GIT_URL := git://github.com/JuliaLang/openspecfun.git
OPENSPECFUN_TAR_URL = https://api.github.com/repos/JuliaLang/openspecfun/tarball/$1
$(eval $(call git-external,openspecfun,OPENSPECFUN,Makefile,libopenspecfun.$(SHLIB_EXT),$(BUILDDIR)))

# issue 8799
OPENSPECFUN_CFLAGS := -O3 -std=c99
ifeq ($(USEICC),1)
  OPENSPECFUN_CFLAGS += -fp-model precise
endif

OPENSPECFUN_OBJ_TARGET := $(build_shlibdir)/libopenspecfun.$(SHLIB_EXT)
OPENSPECFUN_OBJ_SOURCE := $(BUILDDIR)/$(OPENSPECFUN_SRC_DIR)/libopenspecfun.$(SHLIB_EXT)
OPENSPECFUN_FLAGS := ARCH="$(ARCH)" CC="$(CC)" FC="$(FC)" AR="$(AR)" OS="$(OS)" USECLANG=$(USECLANG) USEGCC=$(USEGCC) FFLAGS="$(JFFLAGS)" CFLAGS="$(CFLAGS) $(OPENSPECFUN_CFLAGS)"

ifeq ($(USE_SYSTEM_LIBM),0)
	OPENSPECFUN_FLAGS += USE_OPENLIBM=1
$(OPENSPECFUN_OBJ_SOURCE): $(OPENLIBM_OBJ_TARGET)
endif

$(OPENSPECFUN_OBJ_SOURCE): $(BUILDDIR)/$(OPENSPECFUN_SRC_DIR)/Makefile
	$(MAKE) -C $(dir $<) $(OPENSPECFUN_FLAGS) $(MAKE_COMMON)
	touch -c $@

$(eval $(call staged-install, \
	openspecfun,$$(OPENSPECFUN_SRC_DIR), \
	MAKE_INSTALL,$$(OPENSPECFUN_FLAGS), \
	$$(OPENSPECFUN_OBJ_TARGET), \
	$$(INSTALL_NAME_CMD)libopenspecfun.$$(SHLIB_EXT) $$@))

clean-openspecfun:
	-$(MAKE) -C $(BUILDDIR)/$(OPENSPECFUN_SRC_DIR) distclean $(OPENSPECFUN_FLAGS)
	-rm $(OPENSPECFUN_OBJ_TARGET)
	-rm $(build_libdir)/libopenspecfun.a

get-openspecfun: $(OPENSPECFUN_SRC_FILE)
configure-openspecfun: $(BUILDDIR)/$(OPENSPECFUN_SRC_DIR)/Makefile
compile-openspecfun: $(OPENSPECFUN_OBJ_SOURCE)
check-openspecfun: compile-openspecfun
fastcheck-openspecfun: check-openspecfun

## DSFMT ##

DSFMT_OBJ_TARGET := $(build_shlibdir)/libdSFMT.$(SHLIB_EXT)
DSFMT_OBJ_SOURCE := $(BUILDDIR)/dsfmt-$(DSFMT_VER)/libdSFMT.$(SHLIB_EXT)

DSFMT_CFLAGS := $(CFLAGS) -DNDEBUG -DDSFMT_MEXP=19937 $(fPIC) -DDSFMT_DO_NOT_USE_OLD_NAMES
ifneq ($(USEMSVC), 1)
DSFMT_CFLAGS += -O3 -finline-functions -fomit-frame-pointer -fno-strict-aliasing \
		--param max-inline-insns-single=1800 -Wmissing-prototypes -Wall  -std=c99 -shared
else
DSFMT_CFLAGS += -Wl,-dll,-def:../../libdSFMT.def
endif
ifeq ($(ARCH), x86_64)
DSFMT_CFLAGS += -msse2 -DHAVE_SSE2
endif

$(SRCDIR)/srccache/dsfmt-$(DSFMT_VER).tar.gz: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/dSFMT-src-$(DSFMT_VER).tar.gz
	touch -c $@
$(BUILDDIR)/dsfmt-$(DSFMT_VER)/config.status: $(SRCDIR)/srccache/dsfmt-$(DSFMT_VER).tar.gz
	$(JLCHECKSUM) $<
	mkdir -p $(dir $@) && \
	$(TAR) -C $(dir $@) --strip-components 1 -xf $< && \
	cd $(dir $@) && patch < $(SRCDIR)/dSFMT.h.patch
	cd $(dir $@) && patch < $(SRCDIR)/dSFMT.c.patch
	echo 1 > $@
$(DSFMT_OBJ_SOURCE): $(BUILDDIR)/dsfmt-$(DSFMT_VER)/config.status
	cd $(dir $<) && \
	$(CC) $(CPPFLAGS) $(DSFMT_CFLAGS) $(LDFLAGS) dSFMT.c -o libdSFMT.$(SHLIB_EXT)

define DSFMT_INSTALL
	mkdir -p $2/$$(build_includedir) $2/$$(build_libdir)
	cp $1/dSFMT.h $2/$$(build_includedir)
	cp $$(DSFMT_OBJ_SOURCE) $2/$$(build_libdir)
endef
$(eval $(call staged-install, \
	dsfmt,dsfmt-$(DSFMT_VER), \
	DSFMT_INSTALL,, \
	$$(DSFMT_OBJ_TARGET), \
	$$(INSTALL_NAME_CMD)libdSFMT.$$(SHLIB_EXT) $$(build_shlibdir)/libdSFMT.$$(SHLIB_EXT)))

clean-dsfmt:
	-rm -f $(BUILDDIR)/dsfmt-$(DSFMT_VER)/libdSFMT.$(SHLIB_EXT)
distclean-dsfmt:
	-rm -rf $(SRCDIR)/srccache/dsfmt*.tar.gz $(SRCDIR)/srccache/dsfmt-$(DSFMT_VER) $(BUILDDIR)/dsfmt-$(DSFMT_VER)

get-dsfmt: $(SRCDIR)/srccache/dsfmt-$(DSFMT_VER).tar.gz
configure-dsfmt: $(BUILDDIR)/dsfmt-$(DSFMT_VER)/config.status
compile-dsfmt: $(DSFMT_OBJ_SOURCE)
check-dsfmt: compile-dsfmt
fastcheck-dsfmt: check-dsfmt


## Rmath-julia ##

RMATH_JULIA_OBJ_TARGET := $(build_shlibdir)/libRmath-julia.$(SHLIB_EXT)
RMATH_JULIA_OBJ_SOURCE := $(BUILDDIR)/Rmath-julia-$(RMATH_JULIA_VER)/src/libRmath-julia.$(SHLIB_EXT)

ifeq ($(USE_SYSTEM_DSFMT),0)
$(RMATH_JULIA_OBJ_SOURCE): $(DSFMT_OBJ_TARGET)
endif

RMATH_JULIA_FLAGS += CC="$(CC)" USECLANG=$(USECLANG) USEGCC=$(USEGCC) \
			   OS="$(OS)" ARCH="$(ARCH)" \
			   USE_DSFMT=1 DSFMT_libdir="$(build_shlibdir)" \
			   DSFMT_includedir="$(build_includedir)"

$(SRCDIR)/srccache/Rmath-julia-$(RMATH_JULIA_VER).tar.gz: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ https://api.github.com/repos/JuliaLang/Rmath-julia/tarball/v$(RMATH_JULIA_VER)
$(BUILDDIR)/Rmath-julia-$(RMATH_JULIA_VER)/Makefile: $(SRCDIR)/srccache/Rmath-julia-$(RMATH_JULIA_VER).tar.gz | $(SRCDIR)/srccache
	$(JLCHECKSUM) $<
	mkdir -p $(BUILDDIR)/Rmath-julia-$(RMATH_JULIA_VER)
	$(TAR) -C $(BUILDDIR)/Rmath-julia-$(RMATH_JULIA_VER) --strip-components 1 -xf $<
	touch -c $@
$(RMATH_JULIA_OBJ_SOURCE): $(BUILDDIR)/Rmath-julia-$(RMATH_JULIA_VER)/Makefile
	$(MAKE) -C $(BUILDDIR)/Rmath-julia-$(RMATH_JULIA_VER)/src $(RMATH_JULIA_FLAGS) $(MAKE_COMMON)
	touch -c $@

$(eval $(call staged-install, \
	Rmath-julia,Rmath-julia-$(RMATH_JULIA_VER), \
	LIBFILE_INSTALL,$$(RMATH_JULIA_OBJ_SOURCE), \
	$$(RMATH_JULIA_OBJ_TARGET), \
	$$(INSTALL_NAME_CMD)libRmath-julia.$$(SHLIB_EXT) $$@))


clean-Rmath-julia:
	-$(MAKE) -C $(BUILDDIR)/Rmath-julia-$(RMATH_JULIA_VER) clean
distclean-Rmath-julia: clean-Rmath-julia
	-rm -rf $(SRCDIR)/srccache/Rmath-julia-$(RMATH_JULIA_VER).tar.gz $(SRCDIR)/srccache/Rmath-julia-$(RMATH_JULIA_VER) $(BUILDDIR)/Rmath-julia-$(RMATH_JULIA_VER)

get-Rmath-julia: $(SRCDIR)/srccache/Rmath-julia-$(RMATH_JULIA_VER).tar.gz
configure-Rmath-julia: $(BUILDDIR)/Rmath-julia-$(RMATH_JULIA_VER)/Makefile
compile-Rmath-julia: $(RMATH_JULIA_OBJ_SOURCE)
check-Rmath-julia: compile-Rmath-julia
fastcheck-Rmath-julia: check-Rmath-julia


## objconv ##

OBJCONV_SOURCE := $(BUILDDIR)/objconv/objconv
OBJCONV_TARGET := $(build_bindir)/objconv

$(SRCDIR)/srccache/objconv.zip: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ http://www.agner.org/optimize/objconv.zip
$(BUILDDIR)/objconv/config.status: $(SRCDIR)/srccache/objconv.zip
	-rm -r $(dir $@)
	unzip -d $(dir $@) $<
	cd $(dir $@) && unzip source.zip
	echo 1 > $@
$(OBJCONV_SOURCE): $(BUILDDIR)/objconv/config.status
	cd $(dir $<) && $(CXX) -o objconv -O2 *.cpp

$(eval $(call staged-install, \
	objconv,objconv, \
	BINFILE_INSTALL,$$(OBJCONV_SOURCE), \
	$$(OBJCONV_TARGET),))

clean-objconv:
	-rm -f $(OBJCONV_TARGET)
distclean-objconv:
	-rm -rf $(SRCDIR)/srccache/objconv.zip $(BUILDDIR)/objconv

get-objconv: $(SRCDIR)/srccache/objconv.zip
configure-objconv: $(BUILDDIR)/objconv/config.status
compile-objconv: $(OBJCONV_SOURCE)
check-objconv: compile-objconv
fastcheck-objconv: check-objconv


## OpenBLAS ##
# LAPACK is built into OpenBLAS by default
OPENBLAS_GIT_URL := git://github.com/xianyi/OpenBLAS.git
OPENBLAS_TAR_URL = https://api.github.com/repos/xianyi/OpenBLAS/tarball/$1
$(eval $(call git-external,openblas,OPENBLAS,Makefile,libopenblas$(OPENBLAS_LIBNAMESUFFIX).$(SHLIB_EXT),$(BUILDDIR)))

OPENBLAS_OBJ_SOURCE := $(BUILDDIR)/$(OPENBLAS_SRC_DIR)/libopenblas$(OPENBLAS_LIBNAMESUFFIX).$(SHLIB_EXT)
OPENBLAS_OBJ_TARGET := $(build_shlibdir)/libopenblas$(OPENBLAS_LIBNAMESUFFIX).$(SHLIB_EXT)
OPENBLAS_BUILD_OPTS := CC="$(CC)" FC="$(FC)" RANLIB="$(RANLIB)" FFLAGS="$(FFLAGS) $(JFFLAGS)" TARGET=$(OPENBLAS_TARGET_ARCH) BINARY=$(BINARY)

# Thread support
ifeq ($(OPENBLAS_USE_THREAD), 1)
OPENBLAS_BUILD_OPTS += USE_THREAD=1
OPENBLAS_BUILD_OPTS += GEMM_MULTITHREADING_THRESHOLD=50
# Maximum number of threads for parallelism
ifneq ($(ARCH),x86_64)
# Assume we can't address much memory to spawn many threads
# It is also unlikely that 32-bit architectures have too many cores
OPENBLAS_BUILD_OPTS += NUM_THREADS=8
else ifeq ($(OS),WINNT)
# Windows seems unable to handle very many
OPENBLAS_BUILD_OPTS += NUM_THREADS=16
else ifeq ($(OS),Darwin)
# This should suffice for the largest macs
OPENBLAS_BUILD_OPTS += NUM_THREADS=16
else
# On linux, try to provision for the largest possible machine currently
OPENBLAS_BUILD_OPTS += NUM_THREADS=16
endif
else
OPENBLAS_BUILD_OPTS += USE_THREAD=0
endif

# don't touch scheduler affinity since we manage this ourselves
OPENBLAS_BUILD_OPTS += NO_AFFINITY=1

# Build for all architectures - required for distribution
ifeq ($(OPENBLAS_DYNAMIC_ARCH), 1)
OPENBLAS_BUILD_OPTS += DYNAMIC_ARCH=1
endif

# 64-bit BLAS interface
ifeq ($(USE_BLAS64), 1)
OPENBLAS_BUILD_OPTS += INTERFACE64=1 SYMBOLSUFFIX="$(OPENBLAS_SYMBOLSUFFIX)" LIBPREFIX="libopenblas$(OPENBLAS_LIBNAMESUFFIX)"
ifeq ($(OS), Darwin)
OPENBLAS_BUILD_OPTS += OBJCONV=$(abspath $(BUILDDIR)/objconv/objconv)
$(OPENBLAS_OBJ_SOURCE): $(OBJCONV_SOURCE)
endif
endif

# Decide whether to build for 32-bit or 64-bit arch
ifneq ($(BUILD_OS),$(OS))
OPENBLAS_BUILD_OPTS += OSNAME=$(OS) CROSS=1 HOSTCC=$(HOSTCC)
endif
ifeq ($(OS),WINNT)
ifneq ($(ARCH),x86_64)
OPENBLAS_BUILD_OPTS += CFLAGS="$(CFLAGS) -mincoming-stack-boundary=2"
OPENBLAS_BUILD_OPTS += FFLAGS="$(FFLAGS) -mincoming-stack-boundary=2"
endif
endif

# Debug OpenBLAS
ifeq ($(OPENBLAS_DEBUG), 1)
OPENBLAS_BUILD_OPTS += DEBUG=1
endif

# Allow disabling AVX for older binutils
ifeq ($(OPENBLAS_NO_AVX), 1)
OPENBLAS_BUILD_OPTS += NO_AVX=1 NO_AVX2=1
else ifeq ($(OPENBLAS_NO_AVX2), 1)
OPENBLAS_BUILD_OPTS += NO_AVX2=1
endif

$(BUILDDIR)/$(OPENBLAS_SRC_DIR)/config.status: $(BUILDDIR)/$(OPENBLAS_SRC_DIR)/Makefile
	perl -i -ple 's/^\s*(EXTRALIB\s*\+=\s*-lSystemStubs)\s*$$/# $$1/g' $<.system
	touch $@
$(OPENBLAS_OBJ_SOURCE): $(BUILDDIR)/$(OPENBLAS_SRC_DIR)/config.status
	echo $(MAKE) -C $(dir $<) $(OPENBLAS_BUILD_OPTS) # echo first, so we only print the error message below in a failure case
	@$(MAKE) -C $(dir $<) $(OPENBLAS_BUILD_OPTS) || (echo $(WARNCOLOR)"*** Clean the OpenBLAS build with 'make -C deps clean-openblas'. Rebuild with 'make OPENBLAS_USE_THREAD=0 if OpenBLAS had trouble linking libpthread.so, and with 'make OPENBLAS_TARGET_ARCH=NEHALEM' if there were errors building SandyBridge support. Both these options can also be used simultaneously. ***"$(ENDCOLOR) && false)
	touch -c $@

define OPENBLAS_INSTALL
    $(call LIBFILE_INSTALL,$1,$2,$3)
ifeq ($$(OS), Linux)
	ln -sf libopenblas$(OPENBLAS_LIBNAMESUFFIX).$(SHLIB_EXT) $2/$$(build_libdir)/libopenblas$(OPENBLAS_LIBNAMESUFFIX).$(SHLIB_EXT).0
endif
endef
$(eval $(call staged-install, \
	openblas,$(OPENBLAS_SRC_DIR), \
	OPENBLAS_INSTALL,$(OPENBLAS_OBJ_SOURCE), \
	$$(OPENBLAS_OBJ_TARGET), \
	$$(INSTALL_NAME_CMD)libopenblas$$(OPENBLAS_LIBNAMESUFFIX).$$(SHLIB_EXT) $$@))

clean-openblas:
	-$(MAKE) -C $(BUILDDIR)/$(OPENBLAS_SRC_DIR) clean

get-openblas: $(OPENBLAS_SRC_FILE)
configure-openblas: $(BUILDDIR)/$(OPENBLAS_SRC_DIR)/config.status
compile-openblas: $(OPENBLAS_OBJ_SOURCE)
check-openblas: compile-openblas
fastcheck-openblas: check-openblas


## ATLAS (currently 3.10.0) ##

# no threading, with full lapack, shared library
# should always be compiled with (a real) gcc, it's
# configure script will search for the best match
# (gcc 4.7, gcc, clang,ICC/microsoft/others)
ATLAS_OBJ_SOURCE := $(BUILDDIR)/atlas/build/lib/libsatlas.$(SHLIB_EXT)
ATLAS_OBJ_TARGET := $(build_shlibdir)/libsatlas.$(SHLIB_EXT)
ATLAS_FLAGS := --shared --prefix=$(build_prefix) --cc=gcc -t 0 \
	--with-netlib-lapack-tarfile=$(JULIAHOME)/deps/lapack-$(LAPACK_VER).tgz
ifeq ($(OS), WINNT)
ATLAS_FLAGS += -b 32
endif

#force backwards compatibility (pick any 1)
#ATLAS_FLAGS += -V 192 -A 13  # requires SSE2 (P4 & later)
#ATLAS_FLAGS += -V 128 -A 12 # requires SSE1 (P3 & later)
#ATLAS_FLAGS += -V -1 -A 11 # any x87 (PentiumPro or Athlon & later)
#ATLAS_FLAGS += -A 25  # requires Corei132 (Corei232 doesn't have definition yet)

$(SRCDIR)/srccache/atlas/configure:
	git clone git://github.com/vtjnash/atlas-3.10.0.git $(SRCDIR)/srccache/atlas
ifeq "$(MAKECMDGOALS)" "compile-atlas"
# only allow building atlas as the sole target (without -jN)
# since it internally handles parallelism, for tuning timing accuracy
$(BUILDDIR)/atlas/Make.top: $(SRCDIR)/srccache/atlas/configure $(SRCDIR)/srccache/lapack-$(LAPACK_VER).tgz
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$< $(ATLAS_FLAGS)
	touch -c $@
$(ATLAS_OBJ_SOURCE): $(BUILDDIR)/atlas/Make.top
	$(MAKE) -C $(dir $<) -j1
	touch -c $@
else
$(ATLAS_OBJ_SOURCE):
	$(error cannot build atlas in parallel with anything else)
endif

$(eval $(call staged-install, \
	atlas,atlas-3.10.0, \
	ATLAS_INSTALL,, \
	LIBFILE_INSTALL,$(ATLAS_OBJ_SOURCE), \
	$$(ATLAS_OBJ_TARGET), \
	$$(INSTALL_NAME_CMD)libsatlas.$$(SHLIB_EXT) $$@))

clean-atlas:
	rm -rf $(BUILDDIR)/atlas/build
distclean-atlas:
	rm -rf $(BUILDDIR)/atlas

get-atlas: $(SRCDIR)/srccache/atlas/configure $(SRCDIR)/srccache/lapack-$(LAPACK_VER).tgz
configure-atlas: $(BUILDDIR)/atlas/Make.top
compile-atlas: $(ATLAS_OBJ_SOURCE)
check-atlas: compile-atlas
fastcheck-atlas: check-atlas

## Mac gfortran BLAS wrapper ##
GFORTBLAS_FFLAGS :=
ifeq ($(OS),Darwin)
ifeq ($(USE_SYSTEM_BLAS),1)
ifeq ($(USE_SYSTEM_LAPACK),0)
GFORTBLAS_FFLAGS += -cpp -ffree-line-length-0 -ffixed-line-length-0 \
			    -Dsasum=sasum_gfort -Dscasum=scasum_gfort \
				-Dscnrm2=scnrm2_gfort -Dsdot=sdot_gfort \
				-Dsdsdot=sdsdot_gfort -Dsnrm2=snrm2_gfort \
				-Dcdotc=cdotc_gfort -Dcdotu=cdotu_gfort \
				-Dzdotc=zdotc_gfort -Dzdotu=zdotu_gfort \
				\
			    -DSASUM=SASUM_GFORT -DSCASUM=SCASUM_GFORT \
				-DSCNRM2=SCNRM2_GFORT -DSDOT=SDOT_GFORT \
				-DSDSDOT=SDSDOT_GFORT -DSNRM2=SNRM2_GFORT \
				-DCDOTC=CDOTC_GFORT -DCDOTU=CDOTU_GFORT \
				-DZDOTC=ZDOTC_GFORT -DZDOTU=ZDOTU_GFORT
endif
endif

$(BUILDDIR)/libgfortblas.$(SHLIB_EXT): $(SRCDIR)/gfortblas.c $(SRCDIR)/gfortblas.alias
	$(CC) -Wall -O3 $(CPPFLAGS) $(CFLAGS) $(fPIC) -shared $< -o $@ -pipe \
				-Wl,-reexport_framework,Accelerate -Wl,-alias_list,$(SRCDIR)/gfortblas.alias
$(build_shlibdir)/libgfortblas.$(SHLIB_EXT): $(BUILDDIR)/libgfortblas.$(SHLIB_EXT)
	cp -f $< $@
	$(INSTALL_NAME_CMD)libgfortblas.$(SHLIB_EXT) $@
endif

## LAPACK ##

ifeq ($(USE_SYSTEM_LAPACK), 0)
LAPACK_OBJ_TARGET := $(build_shlibdir)/liblapack.$(SHLIB_EXT)
LAPACK_OBJ_SOURCE := $(BUILDDIR)/lapack-$(LAPACK_VER)/liblapack.$(SHLIB_EXT)
else
LAPACK_OBJ_TARGET :=
LAPACK_OBJ_SOURCE :=
endif

LAPACK_MFLAGS := NOOPT="$(FFLAGS) $(JFFLAGS) $(GFORTBLAS_FFLAGS) -O0" OPTS="$(FFLAGS) $(JFFLAGS) $(GFORTBLAS_FFLAGS)" FORTRAN="$(FC)" LOADER="$(FC)"
ifneq ($(OS),WINNT)
LAPACK_MFLAGS += BLASLIB="-Wl,-rpath,'$(build_libdir)' $(LIBBLAS)"
endif

$(SRCDIR)/srccache/lapack-$(LAPACK_VER).tgz: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ http://www.netlib.org/lapack/$(notdir $@)
$(BUILDDIR)/lapack-$(LAPACK_VER)/make.inc: $(SRCDIR)/srccache/lapack-$(LAPACK_VER).tgz
	$(JLCHECKSUM) $<
	cd $(BUILDDIR) && $(TAR) zxf $<
	cp $(dir $@)INSTALL/make.inc.gfortran $(dir $@)make.inc
	touch -c $@
ifeq ($(USE_SYSTEM_BLAS), 0)
$(BUILDDIR)/lapack-$(LAPACK_VER)/liblapack.a: | $(OPENBLAS_OBJ_TARGET)
else ifeq ($(OS),Darwin)
$(BUILDDIR)/lapack-$(LAPACK_VER)/liblapack.a: | $(build_shlibdir)/libgfortblas.$(SHLIB_EXT)
endif
$(BUILDDIR)/lapack-$(LAPACK_VER)/liblapack.a: $(BUILDDIR)/lapack-$(LAPACK_VER)/make.inc
	$(MAKE) -C $(dir $@) lapacklib $(LAPACK_MFLAGS)
	touch -c $@
$(BUILDDIR)/lapack-$(LAPACK_VER)/checked: $(BUILDDIR)/lapack-$(LAPACK_VER)/liblapack.a
ifeq ($(BUILD_OS),$(OS))
	$(MAKE) -C $(dir $@) lapack_testing $(LAPACK_MFLAGS) -k
endif
	touch $@
$(LAPACK_OBJ_SOURCE): $(BUILDDIR)/lapack-$(LAPACK_VER)/liblapack.a
	$(FC) -shared $(FFLAGS) $(JFFLAGS) $(dir $<)/SRC/*.o $(dir $<)/INSTALL/dlamch.o $(dir $<)/INSTALL/dsecnd_INT_ETIME.o $(dir $<)/INSTALL/ilaver.o $(dir $<)/INSTALL/slamch.o $(LIBBLAS) -o $@

$(eval $(call staged-install, \
	lapack,lapack-$(LAPACK_VER), \
	LIBFILE_INSTALL,$(LAPACK_OBJ_SOURCE), \
	$$(LAPACK_OBJ_TARGET), \
	$$(INSTALL_NAME_CMD)liblapack.$$(SHLIB_EXT) $$@))


clean-lapack:
	-$(MAKE) -C $(BUILDDIR)/lapack-$(LAPACK_VER) clean
	-rm -f $(LAPACK_OBJ_SOURCE) $(LAPACK_OBJ_TARGET)
distclean-lapack:
	-rm -rf $(SRCDIR)/srccache/lapack-$(LAPACK_VER).tgz $(BUILDDIR)/lapack-$(LAPACK_VER)

get-lapack: $(SRCDIR)/srccache/lapack-$(LAPACK_VER).tgz
configure-lapack: $(BUILDDIR)/lapack-$(LAPACK_VER)/make.inc
compile-lapack: $(LAPACK_OBJ_SOURCE)
check-lapack: $(BUILDDIR)/lapack-$(LAPACK_VER)/checked
fastcheck-lapack: check-lapack


## ARPACK ##
ARPACK_FFLAGS := $(GFORTBLAS_FFLAGS)
ARPACK_CFLAGS :=

ifeq ($(USE_BLAS64), 1)
ARPACK_CFLAGS += -DBLASINT=int64_t
ifeq ($(USEIFC),1)
ARPACK_FFLAGS += -i8
else
ARPACK_FFLAGS += -fdefault-integer-8
ifeq ($(USE_SYSTEM_BLAS), 0)
ifeq ($(OPENBLAS_SYMBOLSUFFIX), 64_)
ARPACK_FFLAGS += -cpp -ffixed-line-length-none
ARPACK_OPENBLASFCNS1 := axpy copy gemv geqr2 lacpy lahqr lanhs larnv lartg lascl laset scal trevc trmm trsen gbmv gbtrf gbtrs gttrf gttrs pttrf pttrs
ARPACK_OPENBLASFCNS2 := dot ger labad laev2 lamch lanst lanv2 lapy2 larf larfg lasr nrm2 orm2r rot steqr swap
ARPACK_OPENBLASFCNS3 := dotc geru unm2r
ARPACK_OPENBLASFCNS4 := COPY LABAD LAMCH LANHS LANV2 LARFG ROT GEMV
ARPACK_FFLAGS += $(foreach fcn, $(ARPACK_OPENBLASFCNS1) $(ARPACK_OPENBLASFCNS2), -Ds$(fcn)=s$(fcn)_64 -Dd$(fcn)=d$(fcn)_64)
ARPACK_FFLAGS += $(foreach fcn, $(ARPACK_OPENBLASFCNS1) $(ARPACK_OPENBLASFCNS3), -Dc$(fcn)=c$(fcn)_64 -Dz$(fcn)=z$(fcn)_64)
ARPACK_FFLAGS += $(foreach fcn, $(ARPACK_OPENBLASFCNS4), -DS$(fcn)=S$(fcn)_64 -DD$(fcn)=D$(fcn)_64)
ARPACK_FFLAGS += -Dscnrm2=scnrm2_64 -Ddznrm2=dznrm2_64 -Dcsscal=csscal_64 -Dzdscal=zdscal_64
# CFLAGS are for the configure checks
ARPACK_CFLAGS += -Dsgemm_=sgemm_64_ -Dcheev_=cheev_64_
endif
endif
endif
endif

ifeq ($(OS),WINNT)
ARPACK_OBJ_SOURCE := $(BUILDDIR)/arpack-ng-$(ARPACK_VER)/.libs/libarpack-2.$(SHLIB_EXT)
else
ARPACK_OBJ_SOURCE := $(BUILDDIR)/arpack-ng-$(ARPACK_VER)/.libs/libarpack.$(SHLIB_EXT)
endif
ARPACK_OBJ_TARGET := $(build_shlibdir)/libarpack.$(SHLIB_EXT)

ARPACK_MFLAGS := F77="$(FC)" MPIF77="$(FC)"
ARPACK_FFLAGS += $(FFLAGS) $(JFFLAGS)
ARPACK_FLAGS := --with-blas="$(LIBBLAS)" --with-lapack="$(LIBLAPACK)" --disable-mpi --enable-shared FFLAGS="$(ARPACK_FFLAGS)" CFLAGS="$(CFLAGS) $(ARPACK_CFLAGS)"
ifneq ($(OS),WINNT)
ARPACK_FLAGS += LDFLAGS="$(LDFLAGS) -Wl,-rpath,'$(build_libdir)'"
endif

# ARPACK-NG upstream keeps changing their download filenames
$(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER).tar.gz: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ https://github.com/opencollab/arpack-ng/archive/$(ARPACK_VER).tar.gz
	touch -c $@
$(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER)-testA.mtx: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ https://raw.githubusercontent.com/opencollab/arpack-ng/$(ARPACK_VER)/TESTS/testA.mtx
	touch -c $@
$(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER)/configure: $(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER).tar.gz
	$(JLCHECKSUM) $<
	cd $(dir $<) && $(TAR) zxf $<
	touch -c $@

ifeq ($(USE_ATLAS), 1)
$(BUILDDIR)/arpack-ng-$(ARPACK_VER)/config.status: | $(ATLAS_OBJ_TARGET)
endif

ifeq ($(USE_SYSTEM_BLAS), 0)
$(BUILDDIR)/arpack-ng-$(ARPACK_VER)/config.status: | $(OPENBLAS_OBJ_TARGET)
else ifeq ($(USE_SYSTEM_LAPACK), 0)
$(BUILDDIR)/arpack-ng-$(ARPACK_VER)/config.status: | $(LAPACK_OBJ_TARGET)
endif

$(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER)/arpack-tests-blasint.patch-applied: $(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER)/configure
	cd $(dir $@) && patch -p1 < $(SRCDIR)/arpack-tests-blasint.patch
	echo 1 > $@
$(BUILDDIR)/arpack-ng-$(ARPACK_VER)/config.status: $(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER)/configure $(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER)/arpack-tests-blasint.patch-applied
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$< $(CONFIGURE_COMMON) $(ARPACK_FLAGS)
	touch -c $@
$(ARPACK_OBJ_SOURCE): $(BUILDDIR)/arpack-ng-$(ARPACK_VER)/config.status
	$(MAKE) -C $(dir $<) $(ARPACK_MFLAGS)
	touch -c $@
$(BUILDDIR)/arpack-ng-$(ARPACK_VER)/checked: $(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER)-testA.mtx $(ARPACK_OBJ_SOURCE)
	$(JLCHECKSUM) $<
	cp $< $(dir $@)/TESTS/testA.mtx
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) check $(ARPACK_MFLAGS)
endif
	echo 1 > $@

ifneq ($(PATCHELF),patchelf)
# actually required by the stage-arpack target, but there's no easy way to hook into that
$(ARPACK_OBJ_SOURCE): $(PATCHELF)
endif

define ARPACK_INSTALL
	$(call MAKE_INSTALL,$1,$2,$3)
ifeq ($(OS), WINNT)
	mv $2/$$(build_shlibdir)/libarpack-2.dll $2/$$(ARPACK_OBJ_TARGET)
endif
ifeq ($(OS), Linux)
	for filename in $2/$$(build_shlibdir)/libarpack.so* ; do \
		[ -L $$$$filename ] || $$(PATCHELF) --set-rpath '$$$$ORIGIN' $$$$filename ;\
	done
endif
endef
$(eval $(call staged-install, \
	arpack,arpack-ng-$(ARPACK_VER), \
	ARPACK_INSTALL,$(ARPACK_MFLAGS), \
	$$(ARPACK_OBJ_TARGET), \
	$$(INSTALL_NAME_CMD)libarpack.$$(SHLIB_EXT) $$@))

clean-arpack:
	-$(MAKE) -C $(BUILDDIR)/arpack-ng-$(ARPACK_VER) clean
	-rm -f $(ARPACK_OBJ_SOURCE) $(ARPACK_OBJ_TARGET)
distclean-arpack:
	-rm -rf $(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER).tar.gz \
		$(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER) \
		$(SRCDIR)/srccache/arpack-ng-3.2.0-testA.mtx \
		$(BUILDDIR)/arpack-ng-$(ARPACK_VER)

get-arpack: $(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER).tar.gz $(SRCDIR)/srccache/arpack-ng-$(ARPACK_VER)-testA.mtx
configure-arpack: $(BUILDDIR)/arpack-ng-$(ARPACK_VER)/config.status
compile-arpack: $(ARPACK_OBJ_SOURCE)
check-arpack: $(BUILDDIR)/arpack-ng-$(ARPACK_VER)/checked
# XXX: bug_1315 ARPACK tests fail stochastically
fastcheck-arpack: #check-arpack


## FFTW ##
ifeq ($(OS),WINNT)
FFTW_SINGLE_SRC_TARGET := $(BUILDDIR)/fftw-$(FFTW_VER)-single/.libs/libfftw3f-3.$(SHLIB_EXT)
FFTW_DOUBLE_SRC_TARGET := $(BUILDDIR)/fftw-$(FFTW_VER)-double/.libs/libfftw3-3.$(SHLIB_EXT)
else
FFTW_SINGLE_SRC_TARGET := $(BUILDDIR)/fftw-$(FFTW_VER)-single/.libs/libfftw3f.$(SHLIB_EXT)
FFTW_DOUBLE_SRC_TARGET := $(BUILDDIR)/fftw-$(FFTW_VER)-double/.libs/libfftw3.$(SHLIB_EXT)
endif
FFTW_SINGLE_OBJ_TARGET := $(build_shlibdir)/libfftw3f.$(SHLIB_EXT)
FFTW_DOUBLE_OBJ_TARGET := $(build_shlibdir)/libfftw3.$(SHLIB_EXT)

FFTW_CONFIG := --enable-shared --disable-fortran --disable-mpi --enable-threads
ifneq (,$(findstring arm,$(ARCH)))
  FFTW_CONFIG +=
else ifeq ($(ARCH), ppc)
  FFTW_CONFIG += --enable-altivec
else ifeq ($(ARCH), x86_64)
  FFTW_CONFIG += --enable-sse2 --enable-fma
endif
ifeq ($(OS),WINNT)
FFTW_CONFIG += --with-our-malloc --with-combined-threads
ifneq ($(ARCH),x86_64)
FFTW_CONFIG += --with-incoming-stack-boundary=2
endif
endif
FFTW_ENABLE_single := --enable-single
FFTW_ENABLE_double :=

$(SRCDIR)/srccache/fftw-$(FFTW_VER).tar.gz: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ http://www.fftw.org/$(notdir $@)
$(SRCDIR)/srccache/fftw-$(FFTW_VER)/configure: $(SRCDIR)/srccache/fftw-$(FFTW_VER).tar.gz
	$(JLCHECKSUM) $<
	mkdir -p $(dir $@) && \
	$(TAR) -C $(dir $@) --strip-components 1 -xf $<
	touch -c $@
$(BUILDDIR)/fftw-$(FFTW_VER)-%/config.status: $(SRCDIR)/srccache/fftw-$(FFTW_VER)/configure
	mkdir -p $(dir $@)
	@# try to configure with avx support. if that fails, try again without it
	cd $(dir $@) && \
	($< $(CONFIGURE_COMMON) $(FFTW_CONFIG) $(FFTW_ENABLE_$*) --enable-avx || \
	  $< $(CONFIGURE_COMMON) $(FFTW_CONFIG) $(FFTW_ENABLE_$*))
	$(MAKE) -C $(dir $@) clean
	touch -c $@

$(FFTW_SINGLE_SRC_TARGET): $(BUILDDIR)/fftw-$(FFTW_VER)-single/config.status
	$(MAKE) -C $(dir $<)
	touch -c $@
$(BUILDDIR)/fftw-$(FFTW_VER)-single/checked: $(FFTW_SINGLE_SRC_TARGET)
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) check
endif
	echo 1 > $@

$(FFTW_DOUBLE_SRC_TARGET): $(BUILDDIR)/fftw-$(FFTW_VER)-double/config.status
	$(MAKE) -C $(dir $<)
	touch -c $@
$(BUILDDIR)/fftw-$(FFTW_VER)-double/checked: $(FFTW_DOUBLE_SRC_TARGET)
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) check
endif
	echo 1 > $@

define FFTW_INSTALL
	$(call MAKE_INSTALL,$1,$2,)
ifeq ($$(OS), WINNT)
	mv $2/$$(build_shlibdir)/$3-3.dll $2/$$(build_shlibdir)/$3.dll
endif
ifeq ($$(OS), Linux)
	for filename in $$(build_shlibdir)/$3_threads.so* ; do \
		[ -L $$$$filename ] || $$(PATCHELF) --set-rpath '$$$$ORIGIN' $$$$filename ;\
	done
endif
endef
$(eval $(call staged-install, \
	fftw-single,fftw-$(FFTW_VER)-single, \
	FFTW_INSTALL,libfftw3f, \
	$$(FFTW_SINGLE_OBJ_TARGET), \
	$$(INSTALL_NAME_CMD)libfftw3f.$$(SHLIB_EXT) $$(build_shlibdir)/libfftw3f.$$(SHLIB_EXT) && \
	$$(INSTALL_NAME_CMD)libfftw3f_threads.$$(SHLIB_EXT) $$(build_shlibdir)/libfftw3f_threads.$$(SHLIB_EXT) && \
	$$(INSTALL_NAME_CHANGE_CMD) $$(build_shlibdir)/libfftw3f.3.$$(SHLIB_EXT) $$(INSTALL_NAME_ID_DIR)libfftw3f.$$(SHLIB_EXT) $$(build_shlibdir)/libfftw3f_threads.$$(SHLIB_EXT)))
$(eval $(call staged-install, \
	fftw-double,fftw-$(FFTW_VER)-double, \
	FFTW_INSTALL,libfftw3, \
	$$(FFTW_DOUBLE_OBJ_TARGET), \
	$$(INSTALL_NAME_CMD)libfftw3.$$(SHLIB_EXT) $$(build_shlibdir)/libfftw3.$$(SHLIB_EXT) && \
	$$(INSTALL_NAME_CMD)libfftw3_threads.$$(SHLIB_EXT) $$(build_shlibdir)/libfftw3_threads.$$(SHLIB_EXT) && \
	$$(INSTALL_NAME_CHANGE_CMD) $$(build_shlibdir)/libfftw3.3.$$(SHLIB_EXT) $$(INSTALL_NAME_ID_DIR)libfftw3.$$(SHLIB_EXT) $$(build_shlibdir)/libfftw3_threads.$$(SHLIB_EXT)))

ifneq ($(PATCHELF),patchelf)
# actually required by the stage-fftw target, but there's no easy way to hook into that
$(FFTW_DOUBLE_OBJ_SOURCE): $(PATCHELF)
$(FFTW_SINGLE_OBJ_SOURCE): $(PATCHELF)
endif

clean-fftw: clean-fftw-single clean-fftw-double
clean-fftw-single:
	-$(MAKE) -C $(BUILDDIR)/fftw-$(FFTW_VER)-single clean
	-rm -f $(FFTW_SINGLE_OBJ_TARGET)
clean-fftw-double:
	-$(MAKE) -C $(BUILDDIR)/fftw-$(FFTW_VER)-double clean
	-rm -f $(FFTW_DOUBLE_OBJ_TARGET)
distclean-fftw: distclean-fftw-single distclean-fftw-double
	-rm -rf $(SRCDIR)/srccache/fftw-$(FFTW_VER).tar.gz $(SRCDIR)/srccache/fftw-$(FFTW_VER)
distclean-fftw-single:
	-rm -rf $(BUILDDIR)/fftw-$(FFTW_VER)-single
distclean-fftw-double:
	-rm -rf $(BUILDDIR)/fftw-$(FFTW_VER)-double

get-fftw: get-fftw-single get-fftw-double
configure-fftw: configure-fftw-single configure-fftw-double
compile-fftw: compile-fftw-single compile-fftw-double
check-fftw: check-fftw-single check-fftw-double
fastcheck-fftw: fastcheck-fftw-single fastcheck-fftw-double
install-fftw: install-fftw-single install-fftw-double

get-fftw-single: $(SRCDIR)/srccache/fftw-$(FFTW_VER).tar.gz
configure-fftw-single: $(BUILDDIR)/fftw-$(FFTW_VER)-single/config.status
compile-fftw-single: $(FFTW_SINGLE_SRC_TARGET)
check-fftw-single: $(BUILDDIR)/fftw-$(FFTW_VER)-single/checked
fastcheck-fftw-single: #none

get-fftw-double: $(SRCDIR)/srccache/fftw-$(FFTW_VER).tar.gz
configure-fftw-double: $(BUILDDIR)/fftw-$(FFTW_VER)-double/config.status
compile-fftw-double: $(FFTW_DOUBLE_SRC_TARGET)
check-fftw-double: $(BUILDDIR)/fftw-$(FFTW_VER)-double/checked
fastcheck-fftw-double: #none

## UTF8PROC ##
UTF8PROC_GIT_URL := git://github.com/JuliaLang/utf8proc.git
UTF8PROC_TAR_URL = https://api.github.com/repos/JuliaLang/utf8proc/tarball/$1
$(eval $(call git-external,utf8proc,UTF8PROC,Makefile,libutf8proc.a,$(BUILDDIR)))

UTF8PROC_SRC_TARGET := $(BUILDDIR)/$(UTF8PROC_SRC_DIR)/libutf8proc.a
UTF8PROC_OBJ_LIB    := $(build_libdir)/libutf8proc.a
UTF8PROC_OBJ_HEADER := $(build_includedir)/utf8proc.h
UTF8PROC_OBJ_TARGET := $(UTF8PROC_OBJ_LIB)
UTF8PROC_CFLAGS     := -O2
UTF8PROC_MFLAGS     := CC="$(CC) $(DEPS_CFLAGS)" CFLAGS="$(CFLAGS) $(UTF8PROC_CFLAGS)" PICFLAG="$(fPIC)" AR="$(AR)"

$(UTF8PROC_SRC_TARGET): $(BUILDDIR)/$(UTF8PROC_SRC_DIR)/Makefile
	$(MAKE) -C $(dir $<) $(UTF8PROC_MFLAGS) libutf8proc.a
	touch -c $@
# make check doesn't actually compile (missing prototype / implementation for getline)
$(BUILDDIR)/$(UTF8PROC_SRC_DIR)/checked: $(UTF8PROC_SRC_TARGET)
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) $(UTF8PROC_MFLAGS) check
endif
	echo 1 > $@

define UTF8PROC_INSTALL
	mkdir -p $2/$$(build_includedir) $2/$$(build_libdir)
	cp $1/utf8proc.h $2/$$(build_includedir)
	cp $1/libutf8proc.a $2/$$(build_libdir)
endef
$(eval $(call staged-install, \
	utf8proc,$(UTF8PROC_SRC_DIR), \
	UTF8PROC_INSTALL,, \
	$$(UTF8PROC_OBJ_TARGET),))

clean-utf8proc:
	-$(MAKE) -C $(BUILDDIR)/$(UTF8PROC_SRC_DIR) clean
	-rm -rf $(build_libdir)/libutf8proc.a $(build_includedir)/utf8proc.h

get-utf8proc: $(UTF8PROC_SRC_FILE)
configure-utf8proc: $(BUILDDIR)/$(UTF8PROC_SRC_DIR)/Makefile
compile-utf8proc: $(UTF8PROC_SRC_TARGET)
check-utf8proc: $(BUILDDIR)/$(UTF8PROC_SRC_DIR)/checked
fastcheck-utf8proc: #check-utf8proc
# utf8proc tests disabled since they require a download

## SUITESPARSE ##

SUITESPARSE_OBJ_SOURCE := $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/UMFPACK/Lib/libumfpack.a
SUITESPARSE_OBJ_TARGET := $(build_shlibdir)/libspqr.$(SHLIB_EXT)

ifeq ($(USE_BLAS64), 1)
UMFPACK_CONFIG := -DLONGBLAS='long long'
CHOLMOD_CONFIG := -DLONGBLAS='long long'
SPQR_CONFIG := -DLONGBLAS='long long'
ifeq ($(USE_SYSTEM_BLAS), 0)
ifeq ($(OPENBLAS_SYMBOLSUFFIX), 64_)
UMFPACK_CONFIG += -DSUN64
CHOLMOD_CONFIG += -DSUN64
SPQR_CONFIG += -DSUN64
endif
endif
endif

SUITE_SPARSE_LIB := -lm
ifneq ($(OS), Darwin)
ifneq ($(OS), WINNT)
SUITE_SPARSE_LIB += -lrt
endif
endif
ifneq ($(OS), WINNT)
SUITE_SPARSE_LIB += -Wl,-rpath,'$(build_libdir)'
endif
SUITESPARSE_MFLAGS := CC="$(CC)" CXX="$(CXX)" F77="$(FC)" AR="$(AR)" RANLIB="$(RANLIB)" BLAS="$(LIBBLAS)" LAPACK="$(LIBLAPACK)" \
	  INSTALL_LIB="$(build_libdir)" INSTALL_INCLUDE="$(build_includedir)" LIB="$(SUITE_SPARSE_LIB)" \
	  UMFPACK_CONFIG="$(UMFPACK_CONFIG)" CHOLMOD_CONFIG="$(CHOLMOD_CONFIG)" SPQR_CONFIG="$(SPQR_CONFIG)"

$(SRCDIR)/srccache/SuiteSparse-$(SUITESPARSE_VER).tar.gz: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ http://faculty.cse.tamu.edu/davis/SuiteSparse/$(notdir $@)
$(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/Makefile: $(SRCDIR)/srccache/SuiteSparse-$(SUITESPARSE_VER).tar.gz
	$(JLCHECKSUM) $<
	mkdir -p $(dir $@)
	$(TAR) -C $(dir $@) --strip-components 1 -zxf $<
	touch -c $@

ifeq ($(USE_ATLAS), 1)
$(SUITESPARSE_OBJ_SOURCE): | $(ATLAS_OBJ_TARGET)
endif

ifeq ($(USE_SYSTEM_BLAS), 0)
$(SUITESPARSE_OBJ_SOURCE): | $(OPENBLAS_OBJ_TARGET)
else ifeq ($(USE_SYSTEM_LAPACK), 0)
$(SUITESPARSE_OBJ_SOURCE): | $(LAPACK_OBJ_TARGET)
endif
$(SUITESPARSE_OBJ_SOURCE): $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/Makefile
	$(MAKE) -C $(dir $<) library $(SUITESPARSE_MFLAGS)
	touch -c $@
$(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/checked: $(SUITESPARSE_OBJ_SOURCE)
	$(MAKE) -C $(dir $@) default $(SUITESPARSE_MFLAGS)
	touch $@
$(SUITESPARSE_OBJ_TARGET): $(SUITESPARSE_OBJ_SOURCE)
	mkdir -p $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/lib && \
	cd $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/lib && \
	rm -f *.a && \
	cp -f `find .. -name libamd.a -o -name libcolamd.a -o -name libcamd.a -o -name libccolamd.a -o -name libcholmod.a -o -name libumfpack.a -o -name libsuitesparseconfig.a -o -name libspqr.a 2>/dev/null` . && \
	$(CC) -shared $(WHOLE_ARCHIVE) libsuitesparseconfig.a $(NO_WHOLE_ARCHIVE) -o $(build_shlibdir)/libsuitesparseconfig.$(SHLIB_EXT) && \
	$(INSTALL_NAME_CMD)libsuitesparseconfig.$(SHLIB_EXT) $(build_shlibdir)/libsuitesparseconfig.$(SHLIB_EXT) && \
	$(CC) -shared $(WHOLE_ARCHIVE) libamd.a $(NO_WHOLE_ARCHIVE) -o $(build_shlibdir)/libamd.$(SHLIB_EXT) $(LDFLAGS) -L$(build_shlibdir) -lsuitesparseconfig $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libamd.$(SHLIB_EXT) $(build_shlibdir)/libamd.$(SHLIB_EXT) && \
	$(CC) -shared $(WHOLE_ARCHIVE) libcolamd.a $(NO_WHOLE_ARCHIVE) -o $(build_shlibdir)/libcolamd.$(SHLIB_EXT) $(LDFLAGS) -L$(build_shlibdir) -lsuitesparseconfig $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libcolamd.$(SHLIB_EXT) $(build_shlibdir)/libcolamd.$(SHLIB_EXT) && \
	$(CC) -shared $(WHOLE_ARCHIVE) libcamd.a $(NO_WHOLE_ARCHIVE) -o $(build_shlibdir)/libcamd.$(SHLIB_EXT) $(LDFLAGS) -L$(build_shlibdir) -lsuitesparseconfig $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libcamd.$(SHLIB_EXT) $(build_shlibdir)/libcamd.$(SHLIB_EXT) && \
	$(CC) -shared $(WHOLE_ARCHIVE) libccolamd.a $(NO_WHOLE_ARCHIVE) -o $(build_shlibdir)/libccolamd.$(SHLIB_EXT) $(LDFLAGS) -L$(build_shlibdir) -lsuitesparseconfig $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libccolamd.$(SHLIB_EXT) $(build_shlibdir)/libccolamd.$(SHLIB_EXT) && \
	$(CXX) -shared $(WHOLE_ARCHIVE) libcholmod.a $(NO_WHOLE_ARCHIVE) -o $(build_shlibdir)/libcholmod.$(SHLIB_EXT) $(LDFLAGS) -L$(build_shlibdir) -lcolamd -lamd -lcamd -lccolamd -lsuitesparseconfig $(LIBBLAS) $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libcholmod.$(SHLIB_EXT) $(build_shlibdir)/libcholmod.$(SHLIB_EXT) && \
	$(CXX) -shared $(WHOLE_ARCHIVE) libumfpack.a $(NO_WHOLE_ARCHIVE) -o $(build_shlibdir)/libumfpack.$(SHLIB_EXT) $(LDFLAGS) -L$(build_shlibdir) -lcholmod -lcolamd -lamd -lsuitesparseconfig $(LIBBLAS) $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libumfpack.$(SHLIB_EXT) $(build_shlibdir)/libumfpack.$(SHLIB_EXT) && \
	$(CXX) -shared $(WHOLE_ARCHIVE) libspqr.a $(NO_WHOLE_ARCHIVE) -o $(build_shlibdir)/libspqr.$(SHLIB_EXT) $(LDFLAGS) -L$(build_shlibdir) -lcholmod -lcolamd -lamd -lsuitesparseconfig $(LIBLAPACK) $(LIBBLAS) $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libspqr.$(SHLIB_EXT) $(build_shlibdir)/libspqr.$(SHLIB_EXT)

clean-suitesparse:
	-$(MAKE) -C $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER) clean
	-rm -fr $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/lib
	-rm -f $(SUITESPARSE_OBJ_TARGET)
distclean-suitesparse:
	-rm -rf $(SRCDIR)/srccache/SuiteSparse-$(SUITESPARSE_VER).tar.gz \
		$(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)

get-suitesparse: $(SRCDIR)/srccache/SuiteSparse-$(SUITESPARSE_VER).tar.gz
configure-suitesparse: $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/Makefile
compile-suitesparse: $(SUITESPARSE_OBJ_SOURCE)
check-suitesparse: $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/checked
fastcheck-suitesparse: #none
install-suitesparse: $(SUITESPARSE_OBJ_TARGET) install-suitesparse-wrapper

# SUITESPARSE WRAPPER

ifeq ($(USE_SYSTEM_SUITESPARSE), 1)
SUITESPARSE_INC := -I /usr/include/suitesparse
SUITESPARSE_LIB := -lumfpack -lcholmod -lamd -lcamd -lcolamd -lspqr
else
SUITESPARSE_INC := -I $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/CHOLMOD/Include -I $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/SuiteSparse_config -I $(BUILDDIR)/SuiteSparse-$(SUITESPARSE_VER)/SPQR/Include
SUITESPARSE_LIB := -L$(build_shlibdir) -lcholmod -lumfpack -lspqr $(RPATH_ORIGIN)
$(build_shlibdir)/libsuitesparse_wrapper.$(SHLIB_EXT):  $(SUITESPARSE_OBJ_TARGET)
endif

$(build_shlibdir)/libsuitesparse_wrapper.$(SHLIB_EXT): $(SRCDIR)/SuiteSparse_wrapper.c
	$(CC) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) -O2 -shared $(fPIC) $(SUITESPARSE_INC) $< -o $@ $(SUITESPARSE_LIB)
	$(INSTALL_NAME_CMD)libsuitesparse_wrapper.$(SHLIB_EXT) $@
	touch -c $@

clean-suitesparse-wrapper:
	-rm -f $(build_shlibdir)/libsuitesparse_wrapper.$(SHLIB_EXT)
distclean-suitesparse-wrapper: clean-suitesparse-wrapper

get-suitesparse-wrapper:
configure-suitesparse-wrapper:
compile-suitesparse-wrapper:
check-suitesparse-wrapper:
fastcheck-suitesparse-wrapper: #none
install-suitesparse-wrapper: $(build_shlibdir)/libsuitesparse_wrapper.$(SHLIB_EXT)


## UNWIND ##

LIBUNWIND_TARGET_OBJ := $(build_libdir)/libunwind.a
LIBUNWIND_TARGET_SOURCE := $(BUILDDIR)/libunwind-$(UNWIND_VER)/src/.libs/libunwind.a
LIBUNWIND_CFLAGS := -U_FORTIFY_SOURCE $(fPIC)
LIBUNWIND_CPPFLAGS :=

$(SRCDIR)/srccache/libunwind-$(UNWIND_VER).tar.gz: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ http://download.savannah.gnu.org/releases/libunwind/$(notdir $@)
$(SRCDIR)/srccache/libunwind-$(UNWIND_VER)/configure: $(SRCDIR)/srccache/libunwind-$(UNWIND_VER).tar.gz
	$(JLCHECKSUM) $<
	cd $(dir $<) && $(TAR) xfz $<
	cd $(dir $<)/libunwind-$(UNWIND_VER) && patch -p1 < $(SRCDIR)/libunwind.patch
	touch -c $@
$(BUILDDIR)/libunwind-$(UNWIND_VER)/config.status: $(SRCDIR)/srccache/libunwind-$(UNWIND_VER)/configure
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$< $(CONFIGURE_COMMON) CPPFLAGS="$(CPPFLAGS) $(LIBUNWIND_CPPFLAGS)" CFLAGS="$(CFLAGS) $(LIBUNWIND_CFLAGS)" --disable-shared --disable-minidebuginfo
	touch -c $@
$(LIBUNWIND_TARGET_SOURCE): $(BUILDDIR)/libunwind-$(UNWIND_VER)/config.status
	$(MAKE) -C $(dir $<)
	touch -c $@
$(BUILDDIR)/libunwind-$(UNWIND_VER)/checked: $(LIBUNWIND_TARGET_SOURCE)
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) check
endif
	echo 1 > $@
$(LIBUNWIND_TARGET_OBJ): $(LIBUNWIND_TARGET_SOURCE)
	$(call make-install,libunwind-$(UNWIND_VER),)
ifeq ($(ARCH), ppc64)
	@# workaround for configure script bug
	mv $(build_prefix)/lib64/libunwind*.a $(build_libdir)
endif
	touch $@

clean-unwind:
	-$(MAKE) -C $(BUILDDIR)/libunwind-$(UNWIND_VER) clean
	-rm -f $(LIBUNWIND_TARGET_OBJ) $(LIBUNWIND_TARGET_SOURCE)
distclean-unwind:
	-rm -rf $(SRCDIR)/srccache/libunwind-$(UNWIND_VER).tar.gz \
		$(SRCDIR)/srccache/libunwind-$(UNWIND_VER) \
		$(BUILDDIR)/libunwind-$(UNWIND_VER)

get-unwind: $(SRCDIR)/srccache/libunwind-$(UNWIND_VER).tar.gz
configure-unwind: $(BUILDDIR)/libunwind-$(UNWIND_VER)/config.status
compile-unwind: $(LIBUNWIND_TARGET_SOURCE)
check-unwind: $(BUILDDIR)/libunwind-$(UNWIND_VER)/checked
#todo: libunwind tests are known to fail, so they aren't run
fastcheck-unwind: #none
install-unwind: $(LIBUNWIND_TARGET_OBJ)

## OS X Unwind ##

OSXUNWIND_FLAGS := ARCH="$(ARCH)" CC="$(CC)" FC="$(FC)" AR="$(AR)" OS="$(OS)" USECLANG=$(USECLANG) USEGCC=$(USEGCC) CFLAGS="$(CFLAGS) -ggdb3 -O0" CXXFLAGS="$(CXXFLAGS) -ggdb3 -O0" SFLAGS="-ggdb3" LDFLAGS="$(LDFLAGS) -Wl,-macosx_version_min,10.7"

OSXUNWIND_OBJ_TARGET := $(build_shlibdir)/libosxunwind.$(SHLIB_EXT)
OSXUNWIND_OBJ_SOURCE := $(BUILDDIR)/libosxunwind-$(OSXUNWIND_VER)/libosxunwind.$(SHLIB_EXT)

$(SRCDIR)/srccache/libosxunwind-$(OSXUNWIND_VER).tar.gz: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ https://github.com/JuliaLang/libosxunwind/archive/v$(OSXUNWIND_VER).tar.gz

$(BUILDDIR)/libosxunwind-$(OSXUNWIND_VER)/Makefile: $(SRCDIR)/srccache/libosxunwind-$(OSXUNWIND_VER).tar.gz
	$(JLCHECKSUM) $<
	cd $(BUILDDIR) && $(TAR) xfz $<
	touch -c $@

$(OSXUNWIND_OBJ_SOURCE): $(BUILDDIR)/libosxunwind-$(OSXUNWIND_VER)/Makefile
	$(MAKE) -C $(dir $<) $(OSXUNWIND_FLAGS)
	touch -c $@
$(OSXUNWIND_OBJ_TARGET): $(OSXUNWIND_OBJ_SOURCE) | $(build_shlibdir)
	cp $(BUILDDIR)/libosxunwind-$(OSXUNWIND_VER)/libosxunwind.a $(build_libdir)/libosxunwind.a
	cp $< $@
	cp -R $(BUILDDIR)/libosxunwind-$(OSXUNWIND_VER)/include/* $(build_includedir)
	$(INSTALL_NAME_CMD)libosxunwind.$(SHLIB_EXT) $@

clean-osxunwind:
	-$(MAKE) -C $(BUILDDIR)/libosxunwind-$(OSXUNWIND_VER) clean $(OSXUNWIND_FLAGS)
	-rm $(OSXUNWIND_OBJ_TARGET)
distclean-osxunwind:
	-rm -rf $(SRCDIR)/srccache/libosxunwind-$(OSXUNWIND_VER).tar.gz \
		$(BUILDDIR)/libosxunwind-$(OSXUNWIND_VER)


get-osxunwind: $(SRCDIR)/srccache/libosxunwind-$(OSXUNWIND_VER).tar.gz
configure-osxunwind: $(BUILDDIR)/libosxunwind-$(OSXUNWIND_VER)/Makefile
compile-osxunwind: $(OSXUNWIND_OBJ_SOURCE)
check-osxunwind: compile-osxunwind
fastcheck-osxunwind: check-osxunwind
install-osxunwind: $(OSXUNWIND_OBJ_TARGET)

## GMP ##

GMP_SRC_TARGET := $(BUILDDIR)/gmp-$(GMP_VER)/.libs/libgmp.$(SHLIB_EXT)
GMP_OBJ_TARGET := $(build_shlibdir)/libgmp.$(SHLIB_EXT)

ifeq ($(SANITIZE),1)
GMP_CONFIGURE_OPTS += --disable-assembly
endif

$(SRCDIR)/srccache/gmp-$(GMP_VER).tar.bz2: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ https://gmplib.org/download/gmp/$(notdir $@)
$(SRCDIR)/srccache/gmp-$(GMP_VER)/configure: $(SRCDIR)/srccache/gmp-$(GMP_VER).tar.bz2
	$(JLCHECKSUM) $<
	cd $(dir $<) && $(TAR) jxf $<
	touch -c $@
$(SRCDIR)/srccache/gmp-$(GMP_VER)/patched: $(SRCDIR)/srccache/gmp-$(GMP_VER)/configure
	cd $(dir $@) && patch -p1 < $(SRCDIR)/gmp_6.0.0_osx.patch
	echo 1 > $@
$(BUILDDIR)/gmp-$(GMP_VER)/config.status: $(SRCDIR)/srccache/gmp-$(GMP_VER)/configure $(SRCDIR)/srccache/gmp-$(GMP_VER)/patched
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$< $(CONFIGURE_COMMON) F77= --enable-shared --disable-static $(GMP_CONFIGURE_OPTS)
	touch -c $@
$(GMP_SRC_TARGET): $(BUILDDIR)/gmp-$(GMP_VER)/config.status
	$(MAKE) -C $(dir $<) $(LIBTOOL_CCLD)
	touch -c $@
$(BUILDDIR)/gmp-$(GMP_VER)/checked: $(GMP_SRC_TARGET)
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) $(LIBTOOL_CCLD) check
endif
	echo 1 > $@

define GMP_INSTALL
	mkdir -p $2/$(build_shlibdir) $2/$(build_includedir)
	$(INSTALL_M) $1/.libs/libgmp.*$(SHLIB_EXT)* $2/$(build_shlibdir)
	$(INSTALL_F) $1/gmp.h $2/$(build_includedir)
endef
$(eval $(call staged-install, \
	gmp,gmp-$(GMP_VER), \
	GMP_INSTALL,, \
	$$(GMP_OBJ_TARGET), \
	$$(INSTALL_NAME_CMD)libgmp.$$(SHLIB_EXT) $$@))

clean-gmp:
	-$(MAKE) -C $(BUILDDIR)/gmp-$(GMP_VER) clean
	-rm -f $(GMP_OBJ_TARGET)
distclean-gmp:
	-rm -rf $(SRCDIR)/srccache/gmp-$(GMP_VER).tar.bz2 \
		$(SRCDIR)/srccache/gmp-$(GMP_VER) \
		$(BUILDDIR)/gmp-$(GMP_VER)

get-gmp: $(SRCDIR)/srccache/gmp-$(GMP_VER).tar.bz2
configure-gmp: $(BUILDDIR)/gmp-$(GMP_VER)/config.status
compile-gmp: $(GMP_SRC_TARGET)
check-gmp: $(BUILDDIR)/gmp-$(GMP_VER)/checked
fastcheck-gmp: check-gmp

ifeq ($(USE_SYSTEM_GMP), 0)
MPFR_DEPS := $(GMP_OBJ_TARGET)
endif

## MPFR ##

ifeq ($(USE_SYSTEM_MPFR), 0)
ifeq ($(USE_SYSTEM_GMP), 0)
MPFR_OPTS := --with-gmp-include=$(abspath $(build_includedir)) --with-gmp-lib=$(abspath $(build_shlibdir))
endif
endif
ifeq ($(BUILD_OS),WINNT)
ifeq ($(OS),WINNT)
MPFR_OPTS += --disable-thread-safe CFLAGS="$(CFLAGS) -DNPRINTF_L -DNPRINTF_T -DNPRINTF_J"
endif
endif


MPFR_SRC_TARGET := $(BUILDDIR)/mpfr-$(MPFR_VER)/src/.libs/libmpfr.$(SHLIB_EXT)
MPFR_OBJ_TARGET := $(build_shlibdir)/libmpfr.$(SHLIB_EXT)
ifeq ($(OS),Darwin)
MPFR_CHECK_MFLAGS := LDFLAGS="$(LDFLAGS) -Wl,-rpath,'$(build_libdir)'"
endif

ifeq ($(SANITIZE),1)
# Force generic C build
MPFR_OPTS += --host=none-unknown-linux
endif

$(SRCDIR)/srccache/mpfr-$(MPFR_VER).tar.bz2: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ http://www.mpfr.org/mpfr-$(MPFR_VER)/$(notdir $@)
$(SRCDIR)/srccache/mpfr-$(MPFR_VER)/configure: $(SRCDIR)/srccache/mpfr-$(MPFR_VER).tar.bz2
	$(JLCHECKSUM) $<
	cd $(dir $<) && $(TAR) jxf $<
	touch -c $@
$(BUILDDIR)/mpfr-$(MPFR_VER)/config.status: $(SRCDIR)/srccache/mpfr-$(MPFR_VER)/configure | $(MPFR_DEPS)
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$< $(CONFIGURE_COMMON) $(MPFR_OPTS) F77= --enable-shared --disable-static
	touch -c $@
$(MPFR_SRC_TARGET): $(BUILDDIR)/mpfr-$(MPFR_VER)/config.status
	$(MAKE) -C $(dir $<) $(LIBTOOL_CCLD)
	touch -c $@
$(BUILDDIR)/mpfr-$(MPFR_VER)/checked: $(MPFR_SRC_TARGET)
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) $(LIBTOOL_CCLD) check $(MPFR_CHECK_MFLAGS)
endif
	echo 1 > $@

$(eval $(call staged-install, \
	mpfr,mpfr-$(MPFR_VER), \
	MAKE_INSTALL,$$(LIBTOOL_CCLD), \
	$$(MPFR_OBJ_TARGET), \
	$$(INSTALL_NAME_CMD)libmpfr.$$(SHLIB_EXT) $$@))

clean-mpfr:
	-$(MAKE) -C $(BUILDDIR)/mpfr-$(MPFR_VER) clean
	-rm -f $(MPFR_OBJ_TARGET)
distclean-mpfr:
	-rm -rf $(SRCDIR)/srccache/mpfr-$(MPFR_VER).tar.bz2 \
		$(SRCDIR)/srccache/mpfr-$(MPFR_VER) \
		$(BUILDDIR)/mpfr-$(MPFR_VER)

get-mpfr: $(SRCDIR)/srccache/mpfr-$(MPFR_VER).tar.bz2
configure-mpfr: $(BUILDDIR)/mpfr-$(MPFR_VER)/config.status
compile-mpfr: $(MPFR_SRC_TARGET)
check-mpfr: $(BUILDDIR)/mpfr-$(MPFR_VER)/checked
fastcheck-mpfr: check-mpfr


## patchelf ##

PATCHELF_SOURCE := $(BUILDDIR)/patchelf-$(PATCHELF_VER)/src/patchelf
PATCHELF_TARGET := $(build_bindir)/patchelf

$(SRCDIR)/srccache/patchelf-$(PATCHELF_VER).tar.gz: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ http://nixos.org/releases/patchelf/patchelf-$(PATCHELF_VER)/patchelf-$(PATCHELF_VER).tar.gz
$(SRCDIR)/srccache/patchelf-$(PATCHELF_VER)/configure: $(SRCDIR)/srccache/patchelf-$(PATCHELF_VER).tar.gz
	$(JLCHECKSUM) $<
	cd $(dir $<) && $(TAR) zxf $<
	touch -c $@
$(BUILDDIR)/patchelf-$(PATCHELF_VER)/config.status: $(SRCDIR)/srccache/patchelf-$(PATCHELF_VER)/configure
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$< $(CONFIGURE_COMMON)
	touch -c $@
$(PATCHELF_SOURCE): $(BUILDDIR)/patchelf-$(PATCHELF_VER)/config.status
	$(MAKE) -C $(dir $<)
	touch -c $@
$(BUILDDIR)/patchelf-$(PATCHELF_VER)/checked: $(PATCHELF_SOURCE)
ifeq ($(OS),$(BUILD_OS))
	# disabled due to bug in v0.6
	#$(MAKE) -C $(dir $@) check
endif
	echo 1 > $@

$(eval $(call staged-install, \
	patchelf,patchelf-$(PATCHELF_VER), \
	MAKE_INSTALL,$$(LIBTOOL_CCLD), \
	$$(PATCHELF_TARGET),))

clean-patchelf:
	-$(MAKE) -C $(BUILDDIR)/patchelf-$(PATCHELF_VER) clean
	-rm -f $(PATCHELF_OBJ_TARGET)
distclean-patchelf:
	-rm -rf $(SRCDIR)/srccache/patchelf-$(PATCHELF_VER).tar.gz \
		$(SRCDIR)/srccache/patchelf-$(PATCHELF_VER) \
		$(BUILDDIR)/patchelf-$(PATCHELF_VER)

get-patchelf: $(SRCDIR)/srccache/patchelf-$(PATCHELF_VER).tar.gz
configure-patchelf: $(BUILDDIR)/patchelf-$(PATCHELF_VER)/config.status
compile-patchelf: $(PATCHELF_SOURCE)
check-patchelf: $(BUILDDIR)/patchelf-$(PATCHELF_VER)/checked
fastcheck-patchelf: check-patchelf

## Git
# only used for the mac binaries in contrib/mac/app/Makefile

GIT_SOURCE := $(BUILDDIR)/git-$(GIT_VER)/git
GIT_TARGET := $(build_libexecdir)/git

$(SRCDIR)/srccache/git-$(GIT_VER).tar.gz: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ https://www.kernel.org/pub/software/scm/git/git-$(GIT_VER).tar.gz
$(BUILDDIR)/git-$(GIT_VER)/configure: $(SRCDIR)/srccache/git-$(GIT_VER).tar.gz
	$(JLCHECKSUM) $<
	cd $(BUILDDIR) && $(TAR) zxf $<
	touch -c $@
$(BUILDDIR)/git-$(GIT_VER)/config.status: $(BUILDDIR)/git-$(GIT_VER)/configure
	cd $(dir $@) && \
	./configure $(CONFIGURE_COMMON) --bindir="$(build_libexecdir)"
	touch -c $@
$(GIT_SOURCE): $(BUILDDIR)/git-$(GIT_VER)/config.status
	$(MAKE) -C $(dir $<)
	touch -c $@
$(BUILDDIR)/git-$(GIT_VER)/checked: $(GIT_SOURCE)
	echo 1 > $@
$(GIT_TARGET): $(GIT_SOURCE) $(BUILDDIR)/git-$(GIT_VER)/checked
	$(call make-install,git-$(GIT_VER),NO_INSTALL_HARDLINKS=1 bindir="$(build_libexecdir)")
	touch -c $@

clean-git:
	-$(MAKE) -C $(BUILDDIR)/git-$(GIT_VER) clean
	-rm -f $(GIT_OBJ_TARGET)
distclean-git:
	-rm -rf $(SRCDIR)/srccache/git-$(GIT_VER).tar.gz \
		$(BUILDDIR)/git-$(GIT_VER)

get-git: $(SRCDIR)/srccache/git-$(GIT_VER).tar.gz
configure-git: $(BUILDDIR)/git-$(GIT_VER)/config.status
compile-git: $(GIT_SOURCE)
check-git: $(BUILDDIR)/git-$(GIT_VER)/checked
fastcheck-git: check-git
install-git: $(GIT_TARGET)

## virtualenv

VIRTUALENV_SOURCE := $(SRCDIR)/srccache/virtualenv-$(VIRTUALENV_VER)/virtualenv.py
VIRTUALENV_TARGET := $(BUILDDIR)/julia-env

$(SRCDIR)/srccache/virtualenv-$(VIRTUALENV_VER).tar.gz: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ https://pypi.python.org/packages/source/v/virtualenv/$(notdir $@)
$(VIRTUALENV_SOURCE): $(SRCDIR)/srccache/virtualenv-$(VIRTUALENV_VER).tar.gz
	$(JLCHECKSUM) $<
	cd $(dir $<) && $(TAR) zxf $<
	touch -c $@
$(VIRTUALENV_TARGET): $(VIRTUALENV_SOURCE)
	"$(shell $(SRCDIR)/find_python2)" $< $@
ifeq ($(BUILD_OS), WINNT)
	-[ -e $@/Scripts ] && ! [ -e $@/bin ] && cmd //C mklink //J $@\\bin $@\\Scripts
endif
	touch -c $@

clean-virtualenv:
	-rm -rf $(VIRTUALENV_TARGET)
distclean-virtualenv: clean-virtualenv
	-rm -rf $(SRCDIR)/srccache/virtualenv-$(VIRTUALENV_VER).tar.gz \
		$(SRCDIR)/srccache/virtualenv-$(VIRTUALENV_VER)

get-virtualenv: $(SRCDIR)/srccache/virtualenv-$(VIRTUALENV_VER).tar.gz
configure-virtualenv: get-virtualenv
compile-virtualenv: $(VIRTUALENV_SOURCE)
check-virtualenv: compile-virtualenv
fastcheck-virtualenv: check-virtualenv
install-virtualenv: $(VIRTUALENV_TARGET)

## libgit2

LIBGIT2_GIT_URL := git://github.com/libgit2/libgit2.git
LIBGIT2_TAR_URL = https://api.github.com/repos/libgit2/libgit2/tarball/$1
$(eval $(call git-external,libgit2,LIBGIT2,CMakeLists.txt,build/libgit2.$(SHLIB_EXT),$(SRCDIR)/srccache))

LIBGIT2_OBJ_SOURCE := $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/libgit2.$(SHLIB_EXT)
LIBGIT2_OBJ_TARGET := $(build_shlibdir)/libgit2.$(SHLIB_EXT)

LIBGIT2_OPTS := $(CMAKE_COMMON) -DTHREADSAFE=ON
ifeq ($(OS),WINNT)
LIBGIT2_OPTS += -DWIN32=ON -DMINGW=ON -DUSE_SSH=OFF -DCMAKE_SYSTEM_NAME=Windows -DCMAKE_BUILD_TYPE=RelWithDebInfo
ifneq ($(ARCH),x86_64)
LIBGIT2_OPTS += -DCMAKE_C_FLAGS="-mincoming-stack-boundary=2"
endif
ifeq ($(BUILD_OS),WINNT)
LIBGIT2_OPTS += -G"MSYS Makefiles"
else
LIBGIT2_OPTS += -DBUILD_CLAR=OFF -DCMAKE_RC_COMPILER=`which $(CROSS_COMPILE)windres` -DDLLTOOL=`which $(CROSS_COMPILE)dlltool`
LIBGIT2_OPTS += -DCMAKE_FIND_ROOT_PATH=/usr/$(XC_HOST) -DCMAKE_FIND_ROOT_PATH_MODE_INCLUDE=ONLY
endif
else ifneq ($(OS), Darwin)
LIBGIT2_OPTS += -DREQUIRE_OPENSSL=ON
endif

$(BUILDDIR)/$(LIBGIT2_SRC_DIR)/Makefile: $(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR)/CMakeLists.txt
	-cd $(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR) && patch -p1 -f < $(SRCDIR)/libgit2-require-openssl.patch
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$(CMAKE) $(dir $<) $(LIBGIT2_OPTS)
	touch -c $@
$(LIBGIT2_OBJ_SOURCE): $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/Makefile
	$(MAKE) -C $(dir $<)
	touch -c $@
$(BUILDDIR)/$(LIBGIT2_SRC_DIR)/checked: $(LIBGIT2_OBJ_SOURCE)
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) test
endif
	echo 1 > $@

define LIBGIT2_INSTALL
	mkdir -p $2/$$(build_shlibdir)
	cp $$(LIBGIT2_OBJ_SOURCE) $2/$$(LIBGIT2_OBJ_TARGET)
	@#$(call MAKE_INSTALL,$1,$2,$3) # currently don't need the full install
ifeq ($$(OS),Linux)
	@# If we're on linux, copy over libssl and libcrypto for libgit2
	-LIBGIT_LIBS=$$$$(ldd "$$@" | tail -n +2 | awk '{print $$$$(NF-1)}'); \
	for LIB in libssl libcrypto; do \
		LIB_PATH=$$$$(echo "$$$$LIBGIT_LIBS" | grep "$$$$LIB"); \
		echo "LIB_PATH for $$$$LIB: $$$$LIB_PATH"; \
		[ ! -z "$$$$LIB_PATH" ] && cp -v "$$$$LIB_PATH" $$(build_shlibdir); \
	done
endif
endef
$(eval $(call staged-install, \
	libgit2,$(LIBGIT2_SRC_DIR), \
	LIBGIT2_INSTALL,, \
	$$(LIBGIT2_OBJ_TARGET),))

clean-libgit2:
	-rm -rf $(BUILDDIR)/$(LIBGIT2_SRC_DIR)
	-rm -f $(LIBGIT2_OBJ_TARGET)

configure-libgit2: $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/Makefile
compile-libgit2: $(LIBGIT2_OBJ_SOURCE)
check-libgit2: $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/checked
fastcheck-libgit2: #none
install-libgit2: $(LIBGIT2_OBJ_TARGET)

## phony targets ##

.PHONY: default compile install cleanall distcleanall \
	get-* configure-* compile-* check-* install-* \
	clean-* distclean-* reinstall-* update-llvm

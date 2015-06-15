# -*- mode: makefile-gmake -*-

# disable automatic Makefile rules
.SUFFIXES:

# Figure out OS and architecture
BUILD_OS := $(shell uname)

HOSTCC = $(CC)

# Figure out OS and architecture
OS := $(BUILD_OS)

# flag for disabling assertions
ifeq ($(FORCE_ASSERTIONS), 1)
DISABLE_ASSERTIONS =
else
DISABLE_ASSERTIONS = -DNDEBUG
endif

# Compiler specific stuff

ifeq ($(USEIFC), 1)
FC = ifort
else
FC = gfortran
endif

ifeq ($(USECLANG),1)
ifeq ($(OS), Darwin)
ifeq ($(USE_LIBCPP), 1)
override FC += -mmacosx-version-min=10.7
endif
endif
endif

ifneq ($(USEMSVC),1)
CPP = $(CC) -E
AR := ar
AS := as
LD := ld
else #USEMSVC
CPP = $(CC) -EP
AR := lib
ifeq ($(ARCH),x86_64)
AS := ml64
else
AS := ml
endif #ARCH
LD := link
endif #USEMSVC

ifeq ($(SANITIZE),1)
ifeq ($(SANITIZE_MEMORY),1)
SANITIZE_OPTS = -fsanitize=memory -fsanitize-memory-track-origins -fno-omit-frame-pointer
else
SANITIZE_OPTS = -fsanitize=address -mllvm -asan-stack=0
endif
override JCXXFLAGS += $(SANITIZE_OPTS)
override JCFLAGS += $(SANITIZE_OPTS)
LDFLAGS += $(SANITIZE_OPTS)
override DEPS_CFLAGS += $(SANITIZE_OPTS)
override DEPS_CXXFLAGS += $(SANITIZE_OPTS)
endif

# ===========================================================================

# Select the cpu architecture to target, or automatically detects the user's compiler
# ARCH is the first element of the triple, and gives the CPU class (e.g. x86_64)
# MARCH is the CPU type, and accepts anything that can be passed to the gcc -march flag
#    it is set equal to ARCH (for cases where the two are the same, such as i686)
#    it can be set to native to optimize all code for the user's machine (not just the JIT code)
#    if MARCH is set newer than the native processor, be forewarned that the compile might fail
# JULIA_CPU_TARGET is the JIT-only complement to MARCH. Setting it explicitly is not generally necessary,
#    since it is set equal to MARCH by default
BUILD_MACHINE := $(shell $(HOSTCC) -dumpmachine)
ifeq ($(ARCH),)
override ARCH := $(shell $(CC) -dumpmachine | sed "s/\([^-]*\).*$$/\1/")
ifeq ($(ARCH),mingw32)
$(error "the mingw32 compiler you are using fails the openblas testsuite. please see the README.windows document for a replacement")
else ifeq (cygwin, $(shell $(CC) -dumpmachine | cut -d\- -f3))
$(error "cannot build julia with cygwin-target compilers. set XC_HOST to i686-w64-mingw32 or x86_64-w64-mingw32 for mingw cross-compile")
else ifeq (msys, $(shell $(CC) -dumpmachine | cut -d\- -f3))
$(error "cannot build julia with msys-target compilers. please see the README.windows document for instructions on setting up mingw-w64 compilers")
endif
ifeq ($(BUILD_OS),Darwin)
## Mac is a rather amazing 64-bit user-space on 32-bit kernel architecture, so to determine arch we detect
## the capabilities of the hardware, rather than the compiler or kernel, and make a substitution
ifeq ($(ARCH),x86_64)
override ARCH = i686
else ifeq ($(ARCH),i386)
override ARCH = i686
endif
ifeq ($(ARCH),i686)
ifeq ($(shell sysctl -n hw.cpu64bit_capable),1)
override ARCH = x86_64
endif
BUILD_MACHINE := $(ARCH)$(shell echo $(BUILD_MACHINE) | sed "s/[^-]*\(.*\)$$/\1/")
endif
endif
else
XC_HOST := $(ARCH)$(shell echo $(BUILD_MACHINE) | sed "s/[^-]*\(.*\)$$/\1/")
MARCH = $(ARCH)
endif

ifneq ($(MARCH),)
CC += -march=$(MARCH)
CXX += -march=$(MARCH)
FC += -march=$(MARCH)
JULIA_CPU_TARGET ?= $(MARCH)
ifeq ($(OS),Darwin)
# on Darwin, the standalone `as` program doesn't know
# how to handle AVX instructions, but it does know how
# to dispatch to the clang assembler (if we ask it to)
ifeq ($(USECLANG),1)
CC += -integrated-as
CXX += -integrated-as
else
CC += -Wa,-q
CXX += -Wa,-q
endif
FC += -Wa,-q
AS += -q
endif
endif

JULIA_CPU_TARGET ?= native

# Detect common pre-SSE2 JULIA_CPU_TARGET values known not to work (#7185)
ifneq ($(findstring $(JULIA_CPU_TARGET),i386 i486 i586 i686 pentium pentium2 pentium3),)
$(error Pre-SSE2 CPU targets not supported. To create a generic 32-bit x86 binary, \
pass 'MARCH=i686 JULIA_CPU_TARGET=pentium4', or 'MARCH=pentium4' if not building any dependencies.)
endif

# We map amd64 to x86_64 for compatibility with systems that identify 64-bit systems as such
ifeq ($(ARCH),amd64)
override ARCH = x86_64
endif

ifeq ($(ARCH),i386)
BINARY=32
ISX86=1
else ifeq ($(ARCH),i387)
BINARY=32
ISX86=1
else ifeq ($(ARCH),i486)
BINARY=32
ISX86=1
else ifeq ($(ARCH),i586)
BINARY=32
ISX86=1
else ifeq ($(ARCH),i686)
BINARY=32
ISX86=1
else ifeq ($(ARCH),x86_64)
BINARY=64
ISX86=1
else ifneq (,$(findstring arm,$(ARCH)))
ISX86=0
else ifneq (,$(findstring powerpc,$(ARCH)))
ISX86=0
else ifneq (,$(findstring ppc,$(ARCH)))
ISX86=0
else
$(error "unknown word-size for arch: $(ARCH)")
endif

# If we are running on ARM, set certain options automatically
ifneq (,$(findstring arm,$(ARCH)))
override JCFLAGS += -fsigned-char

LLVM_ASSERTIONS=1
#LLVM_FLAGS+="--with-float=hard --with-abi=aapcs-vfp"
LLVM_VER=3.6.1

override USE_BLAS64=0
override OPENBLAS_DYNAMIC_ARCH=0
override OPENBLAS_TARGET_ARCH=ARMV7

endif

# Set some ARCH-specific flags
ifneq ($(USEICC),1)
ifeq ($(ISX86),1)
CC += -m$(BINARY)
CXX += -m$(BINARY)
FC += -m$(BINARY)
CC_ARG += -m$(BINARY)
CXX_ARG += -m$(BINARY)
endif
endif

ifeq ($(USEGCC),1)
ifeq ($(ISX86),1)
  override SHIPFLAGS += -momit-leaf-frame-pointer
endif
endif

ifeq ($(USE_SYSTEM_LLVM), 1)
LLVM_VER = $(shell $(LLVM_CONFIG) --version)
endif

ifeq ($(USE_SYSTEM_PCRE), 1)
PCRE_CONFIG = pcre2-config
else
PCRE_CONFIG = $(build_bindir)/pcre2-config
endif

# Use ILP64 BLAS interface when building openblas from source on 64-bit architectures
ifeq ($(BINARY), 64)
ifeq ($(USE_SYSTEM_BLAS), 1)
ifeq ($(USE_INTEL_MKL), 1)
USE_BLAS64 ?= 1
else # non MKL system blas is most likely LP64
USE_BLAS64 ?= 0
endif
else
USE_BLAS64 ?= 1
endif
endif

ifeq ($(USE_SYSTEM_BLAS), 1)
ifeq ($(OS), Darwin)
USE_BLAS64 = 0
USE_SYSTEM_LAPACK = 0
LIBBLAS = -L$(build_libdir) -lgfortblas
LIBBLASNAME = libgfortblas
else
LIBBLAS ?= -lblas
LIBBLASNAME ?= libblas
endif
else
LIBBLAS = -L$(build_shlibdir) -lopenblas
LIBBLASNAME = libopenblas
endif

# OpenBLAS builds LAPACK as part of its build.
# We only need to build LAPACK if we are not using OpenBLAS.
ifeq ($(USE_SYSTEM_BLAS), 0)
LIBLAPACK = $(LIBBLAS)
LIBLAPACKNAME = $(LIBBLASNAME)
else
ifeq ($(USE_SYSTEM_LAPACK), 1)
LIBLAPACK ?= -llapack
LIBLAPACKNAME ?= liblapack
else
LIBLAPACK = -L$(build_shlibdir) -llapack $(LIBBLAS)
LIBLAPACKNAME = liblapack
endif
endif

ifeq ($(OS), WINNT)
LIBFFTWNAME = libfftw3
LIBFFTWFNAME = libfftw3f
else
LIBFFTWNAME = libfftw3_threads
LIBFFTWFNAME = libfftw3f_threads
endif

ifeq ($(USE_SYSTEM_LIBM), 1)
LIBM = -lm
LIBMNAME = libm
else
LIBM = -lopenlibm
LIBMNAME = libopenlibm
endif

# OS specific stuff

# install_name_tool
ifeq ($(OS), Darwin)
  # must end with a / and have no trailing spaces
  INSTALL_NAME_ID_DIR = @rpath/
  INSTALL_NAME_CMD = install_name_tool -id $(INSTALL_NAME_ID_DIR)
  INSTALL_NAME_CHANGE_CMD = install_name_tool -change
ifeq ($(shell test `dsymutil -v | cut -d\- -f2 | cut -d. -f1` -gt 102 && echo yes), yes)
  DSYMUTIL = dsymutil
else
  DSYMUTIL = true -ignore
endif
else
  INSTALL_NAME_ID_DIR =
  INSTALL_NAME_CMD = true -ignore
  INSTALL_NAME_CHANGE_CMD = true -ignore
  DSYMUTIL = true -ignore
endif

# shared library runtime paths
ifeq ($(OS), WINNT)
  RPATH =
  RPATH_ORIGIN =
else ifeq ($(OS), Darwin)
  RPATH = -Wl,-rpath,'@executable_path/$(build_private_libdir_rel)' -Wl,-rpath,'@executable_path/$(build_libdir_rel)'
  RPATH_ORIGIN = -Wl,-rpath,'@loader_path/'
else
  RPATH = -Wl,-rpath,'$$ORIGIN/$(build_private_libdir_rel)' -Wl,-rpath,'$$ORIGIN/$(build_libdir_rel)' -Wl,-z,origin
  RPATH_ORIGIN = -Wl,-rpath,'$$ORIGIN' -Wl,-z,origin
endif

ifeq ($(OS), Darwin)
INSTALL_NAME_CMD = install_name_tool -id $(INSTALL_NAME_ID_DIR)
INSTALL_NAME_CHANGE_CMD = install_name_tool -change
endif

# Intel libraries
ifeq ($(USE_INTEL_LIBM), 1)
USE_SYSTEM_LIBM = 1
LIBM = -L$(MKLROOT)/../compiler/lib/intel64 -limf
LIBMNAME = libimf
endif

ifeq ($(USE_INTEL_MKL), 1)
ifeq ($(USE_BLAS64), 1)
export MKL_INTERFACE_LAYER := ILP64
MKLLIB = $(MKLROOT)/lib/intel64
else
MKLLIB = $(MKLROOT)/lib/ia32
endif
USE_SYSTEM_BLAS=1
USE_SYSTEM_LAPACK=1
LIBBLASNAME = libmkl_rt
LIBLAPACKNAME = libmkl_rt
MKL_LDFLAGS = -L$(MKLLIB) -lmkl_rt
ifneq ($(strip $(MKLLIB)),)
  ifeq ($(OS), Linux)
    RPATH_MKL = -Wl,-rpath,$(MKLLIB)
    RPATH     += $(RPATH_MKL)
    MKL_LDFLAGS += $(RPATH_MKL)
  endif
endif
LIBBLAS   = $(MKL_LDFLAGS)
LIBLAPACK = $(MKL_LDFLAGS)
endif

ifeq ($(USE_INTEL_MKL_FFT), 1)
USE_SYSTEM_FFTW = 1
LIBFFTWNAME = libmkl_rt
LIBFFTWFNAME = libmkl_rt
endif

# ATLAS

# ATLAS must have been previously built with "make -C deps compile-atlas" (without -jN),
# or installed to usr/lib/libatlas from some another source (built as
# a shared library, for your platform, single threaded)
USE_ATLAS = 0
ATLAS_LIBDIR = $(build_libdir)
#or ATLAS_LIBDIR = /path/to/system/atlas/lib

ifeq ($(USE_ATLAS), 1)
USE_BLAS64 = 0
USE_SYSTEM_BLAS = 1
USE_SYSTEM_LAPACK = 1
LIBBLAS = -L$(ATLAS_LIBDIR) -lsatlas
LIBLAPACK = $(LIBBLAS)
LIBBLASNAME = libsatlas
LIBLAPACKNAME = $(LIBBLASNAME)
endif

# Renaming OpenBLAS symbols, see #4923 and #8734
ifeq ($(USE_SYSTEM_BLAS), 0)
ifeq ($(USE_BLAS64), 1)
OPENBLAS_SYMBOLSUFFIX = 64_
endif
endif

# Custom libcxx
ifeq ($(BUILD_CUSTOM_LIBCXX),1)
LDFLAGS += -L$(build_libdir) -lc++abi
CXXLDFLAGS += -L$(build_libdir) -lc++abi -stdlib=libc++ -lc++
CPPFLAGS += -I$(build_includedir)/c++/v1
CUSTOM_LD_LIBRARY_PATH = LD_LIBRARY_PATH="$(build_libdir)"
ifeq ($(USEICC),1)
CXXFLAGS += -cxxlib-nostd -static-intel
CLDFLAGS += -static-intel
LDFLAGS += -cxxlib-nostd -static-intel
endif
endif

# Make tricks

define dir_target
$$(subst $$(abspath $(JULIAHOME))/,,$$(abspath $(1))): $$(abspath $(1))
$$(abspath $(1)):
	@mkdir -p $$@
endef

ifeq ($(BUILD_OS), WINNT)
define mingw_to_dos
$(subst /,\\,$(subst $(shell $(2) pwd),$(shell $(2) cmd //C cd),$(abspath $(1))))
endef
endif

define symlink_target
CLEAN_TARGETS += clean-$(2)/$(1)
clean-$$(abspath $(2)/$(1)):
ifeq ($(BUILD_OS), WINNT)
	@-cmd //C rmdir $$(call mingw_to_dos,$(2)/$(1),cd $(2) &&)
else
	@-rm $$(abspath $(2)/$(1))
endif
$$(subst $$(abspath $(JULIAHOME))/,,$$(abspath $(2)/$(1))): $$(abspath $(2)/$(1))
$$(abspath $(2)/$(1)): | $$(abspath $(2))
ifeq ($(BUILD_OS), WINNT)
	@cmd //C mklink //J $$(call mingw_to_dos,$(2)/$(1),cd $(2) &&) $$(call mingw_to_dos,$(1),)
else ifneq (,$(findstring CYGWIN,$(BUILD_OS)))
	@cmd /C mklink /J $$(call cygpath_w,$(2)/$(1)) $$(call cygpath_w,$(1))
else ifdef JULIA_VAGRANT_BUILD
	@cp -R $$(abspath $(1)) $$@
else
	@ln -sf $$(abspath $(1)) $$@
endif
endef

ifeq ($(BUILD_OS), WINNT) # MSYS
spawn = $(1)
cygpath_w = $(1)
else ifneq (,$(findstring CYGWIN,$(BUILD_OS))) # Cygwin
spawn = $(1)
cygpath_w = `cygpath -w $(1)`
else
ifeq ($(OS), WINNT) # unix-to-Windows cross-compile
spawn = wine $(1)
cygpath_w = `winepath -w $(1)`
else # not Windows
spawn = $(1)
cygpath_w = $(1)
endif
endif

exec = $(shell $(call spawn,$(1)))

ifneq (,$(findstring CYGWIN,$(BUILD_OS)))
wine_pathsearch = $(firstword $(wildcard $(addsuffix /$(1),$(subst :, ,$(2)))))
else
wine_pathsearch = $(firstword $(wildcard $(addsuffix /$(1),$(shell printf %s\n '$(2)' | xargs -d";" winepath -u | tr '\n' ' '))))
endif
pathsearch = $(firstword $(wildcard $(addsuffix /$(1),$(subst :, ,$(2)))))

JULIA_BUILD_MODE = release
ifeq (,$(findstring release,$(MAKECMDGOALS)))
ifneq (,$(findstring debug,$(MAKECMDGOALS)))
JULIA_BUILD_MODE = debug
endif
endif

JULIA_EXECUTABLE_debug = $(build_bindir)/julia-debug$(EXE)
JULIA_EXECUTABLE_release = $(build_bindir)/julia$(EXE)
JULIA_EXECUTABLE = $(JULIA_EXECUTABLE_$(JULIA_BUILD_MODE))

# Colors for make
ifndef VERBOSE
VERBOSE = 0
endif

WARNCOLOR="\033[33;1m"

ifeq ($(VERBOSE), 0)

QUIET_MAKE = -s

CCCOLOR="\033[34m"
LINKCOLOR="\033[34;1m"
PERLCOLOR="\033[35m"
FLISPCOLOR="\033[32m"
JULIACOLOR="\033[32;1m"

SRCCOLOR="\033[33m"
BINCOLOR="\033[37;1m"
JULCOLOR="\033[34;1m"
ENDCOLOR="\033[0m"

GOAL=$(subst ','\'',$(subst $(abspath $(JULIAHOME))/,,$(abspath $@)))

PRINT_CC = printf '    %b %b\n' $(CCCOLOR)CC$(ENDCOLOR) $(SRCCOLOR)$(GOAL)$(ENDCOLOR); $(1)
PRINT_LINK = printf '    %b %b\n' $(LINKCOLOR)LINK$(ENDCOLOR) $(BINCOLOR)$(GOAL)$(ENDCOLOR); $(1)
PRINT_PERL = printf '    %b %b\n' $(PERLCOLOR)PERL$(ENDCOLOR) $(BINCOLOR)$(GOAL)$(ENDCOLOR); $(1)
PRINT_FLISP = printf '    %b %b\n' $(FLISPCOLOR)FLISP$(ENDCOLOR) $(BINCOLOR)$(GOAL)$(ENDCOLOR); $(1)
PRINT_JULIA = printf '    %b %b\n' $(JULIACOLOR)JULIA$(ENDCOLOR) $(BINCOLOR)$(GOAL)$(ENDCOLOR); $(1)

else
QUIET_MAKE =
PRINT_CC = echo '$(subst ','\'',$(1))'; $(1)
PRINT_LINK = echo '$(subst ','\'',$(1))'; $(1)
PRINT_PERL = echo '$(subst ','\'',$(1))'; $(1)
PRINT_FLISP = echo '$(subst ','\'',$(1))'; $(1)
PRINT_JULIA = echo '$(subst ','\'',$(1))'; $(1)

endif

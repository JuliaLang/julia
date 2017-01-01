##
# This Makefile will be executed in $(BUILDDIR)/compiler-rt-$(LLVM_VER)
# Expected variables from the parent
# CRT_SRCDIR
# LIBFILE
# SLIBFILE
# OS (CRT_OS not JL_OS)
# ARCH (CRT_ARCH not JL_ARCH)
# USE_CLANG
# fPIC
##

# The standalone compiler-rt build is inspired by
# https://github.com/ReservedField/arm-compiler-rt
SRCDIR := $(CRT_SRCDIR)/lib/builtins

ifeq ($(ARCH), armhf)
ARCH_SRCDIR := $(SRCDIR)/arm
else ifeq ($(ARCH), aarch64)
ARCH_SRCDIR := $(SRCDIR)/arm64
else
ARCH_SRCDIR := $(SRCDIR)/$(ARCH)
endif

INCLUDES := -I$(SRCDIR) -I$(ARCH_SRCDIR)
# TODO(vchuravy) discover architecture flags
CRT_CFLAGS := $(CPPFLAGS) $(CFLAGS) -O2 -std=c11 \
		$(fPIC) $(INCLUDES) \
		-fno-builtin -ffreestanding
ifeq ($(USE_CLANG),1)
CRT_CFLAGS += -Wno-unknown-attributes -Wno-macro-redefined
endif

##
# Blacklist a few files we don't want to deal with
##
MAKEFLAGS := --no-builtin-rules
BLACKLIST := atomic.o atomic_flag_clear.o atomic_flag_clear_explicit.o \
		atomic_flag_test_and_set.o atomic_flag_test_and_set_explicit.o \
		atomic_signal_fence.o atomic_thread_fence.o emutls.o

CRT_LDFLAGS :=
ifeq ($(ARCH),ppc)
BLACKLIST += saveFP.o restFP.o
else ifeq ($(ARCH), armhf)
CRT_LDFLAGS += -Wl,--allow-multiple-definition
endif


ifeq ($(OS),darwin)
# Which blacklist should we choose
BLACKLIST += $(shell cat $(SRCDIR)/Darwin-excludes/osx.txt)
else ifeq ($(OS), winnt)
CRT_CFLAGS += -D_WIN32
endif

CFILES := $(wildcard $(SRCDIR)/*.c)
GENERAL_OBJS1 := $(filter-out $(BLACKLIST), $(notdir $(CFILES:.c=.o)))

ARCH_CFILES := $(wildcard $(ARCH_SRCDIR)/*.c)
ARCH_SFILES := $(wildcard $(ARCH_SRCDIR)/*.S)
ARCH_OBJS   := $(filter-out $(BLACKLIST), $(notdir $(join $(ARCH_CFILES:.c=.o),$(ARCH_SFILES:.S=.o))))

GENERAL_OBJS := $(filter-out $(ARCH_OBJS), $(GENERAL_OBJS1))

OBJS := $(GENERAL_OBJS) $(ARCH_OBJS)

%.o: $(SRCDIR)/%.c
	$(CC) $(CRT_CFLAGS) -c $< -o $@

%.o: $(SRCDIR)/%.S
	$(CC) $(CRT_CFLAGS) -c $< -o $@

%.o: $(ARCH_SRCDIR)/%.c
	$(CC) $(CRT_CFLAGS) -c $< -o $@

%.o: $(ARCH_SRCDIR)/%.S
	$(CC) $(CRT_CFLAGS) -c $< -o $@

$(LIBFILE): $(OBJS)
	$(CC) $(CRT_LDFLAGS) $(CRT_CFLAGS) $(LDFLAGS) -shared -o $@ $^

$(SLIBFILE): $(OBJS)
	$(AR) rs $@ $^

.PHONY: all
all: $(LIBFILE) $(SLIBFILE)
clean: $(OBJS) $(LIBFILE) $(SLIBFILE)
	rm $^


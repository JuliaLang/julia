SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
BUILDDIR := .
JULIAHOME := $(SRCDIR)
include $(JULIAHOME)/Make.inc

default: sysimg-$(JULIA_BUILD_MODE) # contains either "debug" or "release"
all: sysimg-release sysimg-debug
sysimg-ji: $(build_private_libdir)/sys.ji
sysimg-bc: $(build_private_libdir)/sys-bc.a
sysimg-release: $(build_private_libdir)/sys.$(SHLIB_EXT)
sysimg-debug: $(build_private_libdir)/sys-debug.$(SHLIB_EXT)

VERSDIR := v`cut -d. -f1-2 < $(JULIAHOME)/VERSION`

$(build_private_libdir)/%.$(SHLIB_EXT): $(build_private_libdir)/%-o.a
	@$(call PRINT_LINK, $(CXX) $(LDFLAGS) -shared $(fPIC) -L$(build_private_libdir) -L$(build_libdir) -L$(build_shlibdir) -o $@ \
		$(WHOLE_ARCHIVE) $< $(NO_WHOLE_ARCHIVE) \
		$(if $(findstring -debug,$(notdir $@)),-ljulia-internal-debug -ljulia-debug,-ljulia-internal -ljulia) \
		$$([ $(OS) = WINNT ] && echo '' -lssp))
	@$(INSTALL_NAME_CMD)$(notdir $@) $@
	@$(DSYMUTIL) $@

BASE_DIR := $(build_datarootdir)/julia/src
COMPILER_SRCS := $(addprefix $(BASE_DIR)/base/, \
		boot.jl \
		docs/core.jl \
		abstractarray.jl \
		abstractdict.jl \
		array.jl \
		bitarray.jl \
		bitset.jl \
		bool.jl \
		ctypes.jl \
		error.jl \
		essentials.jl \
		expr.jl \
		generator.jl \
		int.jl \
		indices.jl \
		iterators.jl \
		namedtuple.jl \
		number.jl \
		operators.jl \
		options.jl \
		pair.jl \
		pointer.jl \
		promotion.jl \
		range.jl \
		reflection.jl \
		traits.jl \
		refvalue.jl \
		tuple.jl)
COMPILER_SRCS += $(shell find $(BASE_DIR)/base/compiler -name \*.jl)
BASE_SRCS := $(shell find $(BASE_DIR) -name \*.jl -and -not -name sysimg.jl)
STDLIB_SRCS := $(BASE_DIR)/base/sysimg.jl $(shell find $(build_datarootdir)/julia/stdlib/$(VERSDIR)/*/src -name \*.jl)

$(build_private_libdir)/corecompiler.ji: $(COMPILER_SRCS)
	@$(call PRINT_JULIA, cd $(BASE_DIR)/base && \
	$(call spawn,$(JULIA_EXECUTABLE)) -C "$(JULIA_CPU_TARGET)" --output-ji $(call cygpath_w,$@).tmp \
		--startup-file=no --warn-overwrite=yes -g$(BOOTSTRAP_DEBUG_LEVEL) -O0 compiler/compiler.jl)
	@mv $@.tmp $@

$(build_private_libdir)/sys.ji: $(build_private_libdir)/corecompiler.ji $(JULIAHOME)/VERSION $(BASE_SRCS) $(STDLIB_SRCS)
	@$(call PRINT_JULIA, cd $(BASE_DIR)/base && \
	if ! JULIA_BINDIR=$(call cygpath_w,$(build_bindir)) WINEPATH="$(call cygpath_w,$(build_bindir));$$WINEPATH" \
			$(call spawn, $(JULIA_EXECUTABLE)) -g1 -O0 -C "$(JULIA_CPU_TARGET)" --output-ji $(call cygpath_w,$@).tmp $(JULIA_SYSIMG_BUILD_FLAGS) \
			--startup-file=no --warn-overwrite=yes --sysimage $(call cygpath_w,$<) sysimg.jl; then \
		echo '*** This error might be fixed by running `make clean`. If the error persists$(COMMA) try `make cleanall`. ***'; \
		false; \
	fi )
	@mv $@.tmp $@

define sysimg_builder
$$(build_private_libdir)/sys$1-o.a $$(build_private_libdir)/sys$1-bc.a : $$(build_private_libdir)/sys$1-%.a : $$(build_private_libdir)/sys.ji
	@$$(call PRINT_JULIA, cd $$(BASE_DIR)/base && \
	if ! JULIA_BINDIR=$$(call cygpath_w,$(build_bindir)) WINEPATH="$$(call cygpath_w,$$(build_bindir));$$$$WINEPATH" \
			JULIA_NUM_THREADS=1 \
			$$(call spawn, $3) $2 -C "$$(JULIA_CPU_TARGET)" --output-$$* $$(call cygpath_w,$$@).tmp $$(JULIA_SYSIMG_BUILD_FLAGS) \
			--startup-file=no --warn-overwrite=yes --sysimage $$(call cygpath_w,$$<) $$(call cygpath_w,$$(JULIAHOME)/contrib/generate_precompile.jl) $(JULIA_PRECOMPILE); then \
		echo '*** This error is usually fixed by running `make clean`. If the error persists$$(COMMA) try `make cleanall`. ***'; \
		false; \
	fi )
	@mv $$@.tmp $$@
.SECONDARY: $$(build_private_libdir)/sys$1-o.a $(build_private_libdir)/sys$1-bc.a # request Make to keep these files around
endef
$(eval $(call sysimg_builder,,-O3,$(JULIA_EXECUTABLE_release)))
$(eval $(call sysimg_builder,-debug,-O0,$(JULIA_EXECUTABLE_debug)))

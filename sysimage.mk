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
		$(if $(findstring -debug,$(notdir $@)),-ljulia-debug,-ljulia) \
		$$([ $(OS) = WINNT ] && echo '' -lssp))
	@$(INSTALL_NAME_CMD)$(notdir $@) $@
	@$(DSYMUTIL) $@

COMPILER_SRCS := $(addprefix $(JULIAHOME)/, \
		base/boot.jl \
		base/docs/core.jl \
		base/abstractarray.jl \
		base/abstractdict.jl \
		base/array.jl \
		base/bitarray.jl \
		base/bitset.jl \
		base/bool.jl \
		base/ctypes.jl \
		base/error.jl \
		base/essentials.jl \
		base/expr.jl \
		base/generator.jl \
		base/int.jl \
		base/indices.jl \
		base/iterators.jl \
		base/namedtuple.jl \
		base/number.jl \
		base/operators.jl \
		base/options.jl \
		base/pair.jl \
		base/pointer.jl \
		base/promotion.jl \
		base/range.jl \
		base/reflection.jl \
		base/traits.jl \
		base/refvalue.jl \
		base/tuple.jl)
COMPILER_SRCS += $(shell find $(JULIAHOME)/base/compiler -name \*.jl)
# sort these to remove duplicates
BASE_SRCS := $(sort $(shell find $(JULIAHOME)/base -name \*.jl -and -not -name sysimg.jl) \
                    $(shell find $(BUILDROOT)/base -name \*.jl  -and -not -name sysimg.jl))
STDLIB_SRCS := $(JULIAHOME)/base/sysimg.jl $(shell find $(build_datarootdir)/julia/stdlib/$(VERSDIR)/*/src -name \*.jl) \
                    $(build_prefix)/manifest/Pkg
RELBUILDROOT := $(shell $(JULIAHOME)/contrib/relative_path.sh "$(JULIAHOME)/base" "$(BUILDROOT)/base/")

$(build_private_libdir)/corecompiler.ji: $(COMPILER_SRCS)
	@$(call PRINT_JULIA, cd $(JULIAHOME)/base && \
	$(call spawn,$(JULIA_EXECUTABLE)) -C "$(JULIA_CPU_TARGET)" --output-ji $(call cygpath_w,$@).tmp \
		--startup-file=no -g1 -O0 compiler/compiler.jl)
	@mv $@.tmp $@

$(build_private_libdir)/sys.ji: $(build_private_libdir)/corecompiler.ji $(JULIAHOME)/VERSION $(BASE_SRCS) $(STDLIB_SRCS)
	@$(call PRINT_JULIA, cd $(JULIAHOME)/base && \
	if ! JULIA_BINDIR=$(call cygpath_w,$(build_bindir)) $(call spawn, $(JULIA_EXECUTABLE)) -g1 -O0 -C "$(JULIA_CPU_TARGET)" --output-ji $(call cygpath_w,$@).tmp $(JULIA_SYSIMG_BUILD_FLAGS) \
			--startup-file=no --warn-overwrite=yes --sysimage $(call cygpath_w,$<) sysimg.jl $(RELBUILDROOT); then \
		echo '*** This error might be fixed by running `make clean`. If the error persists$(COMMA) try `make cleanall`. ***'; \
		false; \
	fi )
	@mv $@.tmp $@

define sysimg_builder
$$(build_private_libdir)/sys$1-o.a $$(build_private_libdir)/sys$1-bc.a : $$(build_private_libdir)/sys$1-%.a : $$(build_private_libdir)/sys.ji
	@$$(call PRINT_JULIA, cd $$(JULIAHOME)/base && \
	if ! JULIA_BINDIR=$$(call cygpath_w,$(build_bindir)) $$(call spawn, $3) $2 -C "$$(JULIA_CPU_TARGET)" --output-$$* $$(call cygpath_w,$$@).tmp $$(JULIA_SYSIMG_BUILD_FLAGS) \
		--startup-file=no --warn-overwrite=yes --sysimage $$(call cygpath_w,$$<) $$(call cygpath_w,$$(JULIAHOME)/contrib/generate_precompile.jl) $(JULIA_PRECOMPILE); then \
		echo '*** This error is usually fixed by running `make clean`. If the error persists$$(COMMA) try `make cleanall`. ***'; \
		false; \
	fi )
	@mv $$@.tmp $$@
.SECONDARY: $$(build_private_libdir)/sys$1-o.a $(build_private_libdir)/sys$1-bc.a # request Make to keep these files around
endef
$(eval $(call sysimg_builder,,-O3,$(JULIA_EXECUTABLE_release)))
$(eval $(call sysimg_builder,-debug,-O0,$(JULIA_EXECUTABLE_debug)))

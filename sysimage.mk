SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
BUILDDIR := .
JULIAHOME := $(SRCDIR)
include $(JULIAHOME)/Make.inc
include $(JULIAHOME)/stdlib/stdlib.mk

default: sysimg-$(JULIA_BUILD_MODE) # contains either "debug" or "release"
all: sysimg-release sysimg-debug
basecompiler-ji: $(build_private_libdir)/basecompiler.ji
sysimg-ji: $(build_private_libdir)/sys.ji
sysimg-bc: $(build_private_libdir)/sys-bc.a
sysimg-release: $(build_private_libdir)/sys.$(SHLIB_EXT)
sysimg-debug: $(build_private_libdir)/sys-debug.$(SHLIB_EXT)

VERSDIR := v$(shell cut -d. -f1-2 < $(JULIAHOME)/VERSION)

$(build_private_libdir)/%.$(SHLIB_EXT): $(build_private_libdir)/%-o.a
	@$(call PRINT_LINK, $(CXX) $(LDFLAGS) -shared $(fPIC) -L$(build_private_libdir) -L$(build_libdir) -L$(build_shlibdir) -o $@ \
		$(WHOLE_ARCHIVE) $< $(NO_WHOLE_ARCHIVE) \
		$(if $(findstring -debug,$(notdir $@)),-ljulia-internal-debug -ljulia-debug,-ljulia-internal -ljulia) \
		$$([ $(OS) = WINNT ] && echo '' $(LIBM) -lssp --disable-auto-import --disable-runtime-pseudo-reloc))
	@$(INSTALL_NAME_CMD)$(notdir $@) $@
	@$(DSYMUTIL) $@

COMPILER_SRCS := $(addprefix $(JULIAHOME)/, \
		base/Base_compiler.jl \
		base/boot.jl \
		base/docs/core.jl \
		base/abstractarray.jl \
		base/abstractdict.jl \
		base/abstractset.jl \
		base/iddict.jl \
		base/idset.jl \
		base/array.jl \
		base/bitarray.jl \
		base/bitset.jl \
		base/bool.jl \
		base/ctypes.jl \
		base/error.jl \
		base/essentials.jl \
		base/expr.jl \
		base/exports.jl \
		base/generator.jl \
		base/int.jl \
		base/indices.jl \
		base/iterators.jl \
		base/invalidation.jl \
		base/namedtuple.jl \
		base/number.jl \
		base/operators.jl \
		base/options.jl \
		base/pair.jl \
		base/pointer.jl \
		base/promotion.jl \
		base/range.jl \
		base/runtime_internals.jl \
		base/traits.jl \
		base/refvalue.jl \
		base/tuple.jl)
COMPILER_SRCS += $(shell find $(JULIAHOME)/Compiler/src -name \*.jl)
# sort these to remove duplicates
BASE_SRCS := $(sort $(shell find $(JULIAHOME)/base -name \*.jl -and -not -name sysimg.jl) \
                    $(shell find $(BUILDROOT)/base -name \*.jl  -and -not -name sysimg.jl))
STDLIB_SRCS := $(JULIAHOME)/base/sysimg.jl $(SYSIMG_STDLIBS_SRCS)
RELBUILDROOT := $(call rel_path,$(JULIAHOME)/base,$(BUILDROOT)/base)/ # <-- make sure this always has a trailing slash
RELDATADIR := $(call rel_path,$(JULIAHOME)/base,$(build_datarootdir))/ # <-- make sure this always has a trailing slash

$(build_private_libdir)/basecompiler.ji: $(COMPILER_SRCS)
	@$(call PRINT_JULIA, cd $(JULIAHOME)/base && \
	JULIA_NUM_THREADS=1 $(call spawn,$(JULIA_EXECUTABLE)) -C "$(JULIA_CPU_TARGET)" $(HEAPLIM) --output-ji $(call cygpath_w,$@).tmp \
		--startup-file=no --warn-overwrite=yes -g$(BOOTSTRAP_DEBUG_LEVEL) -O1 Base_compiler.jl --buildroot $(RELBUILDROOT) --dataroot $(RELDATADIR))
	@mv $@.tmp $@

$(build_private_libdir)/basecompiler-o.a $(build_private_libdir)/basecompiler-bc.a: $(build_private_libdir)/basecompiler-%.a : $(COMPILER_SRCS)
	@$(call PRINT_JULIA, cd $(JULIAHOME)/base && \
	JULIA_NUM_THREADS=1 $(call spawn,$(JULIA_EXECUTABLE)) -C "$(JULIA_CPU_TARGET)" $(HEAPLIM) --output-$* $(call cygpath_w,$@).tmp \
		--startup-file=no --warn-overwrite=yes -g$(BOOTSTRAP_DEBUG_LEVEL) -O1 Base_compiler.jl --buildroot $(RELBUILDROOT) --dataroot $(RELDATADIR))
	@mv $@.tmp $@

$(build_private_libdir)/sys.ji: $(build_private_libdir)/basecompiler.$(SHLIB_EXT) $(JULIAHOME)/VERSION $(BASE_SRCS) $(STDLIB_SRCS)
	@$(call PRINT_JULIA, cd $(JULIAHOME)/base && \
	if ! JULIA_BINDIR=$(call cygpath_w,$(build_bindir)) WINEPATH="$(call cygpath_w,$(build_bindir));$$WINEPATH" \
			JULIA_NUM_THREADS=1 $(call spawn, $(JULIA_EXECUTABLE)) -g1 -O1 -C "$(JULIA_CPU_TARGET)" $(HEAPLIM) --output-ji $(call cygpath_w,$@).tmp $(JULIA_SYSIMG_BUILD_FLAGS) \
			--startup-file=no --warn-overwrite=yes --sysimage $(call cygpath_w,$<) sysimg.jl --buildroot $(RELBUILDROOT) --dataroot $(RELDATADIR); then \
		echo '*** This error might be fixed by running `make clean`. If the error persists$(COMMA) try `make cleanall`. ***'; \
		false; \
	fi )
	@mv $@.tmp $@

define sysimg_builder
$$(build_private_libdir)/sys$1-o.a $$(build_private_libdir)/sys$1-bc.a : $$(build_private_libdir)/sys$1-%.a : $$(build_private_libdir)/sys.ji $$(JULIAHOME)/contrib/generate_precompile.jl
	@$$(call PRINT_JULIA, cd $$(JULIAHOME)/base && \
	if ! JULIA_BINDIR=$$(call cygpath_w,$(build_bindir)) \
		 WINEPATH="$$(call cygpath_w,$$(build_bindir));$$$$WINEPATH" \
		 JULIA_LOAD_PATH='@stdlib' \
		 JULIA_PROJECT= \
		 JULIA_DEPOT_PATH=':' \
		 JULIA_NUM_THREADS=1 \
			$$(call spawn, $3) $2 -C "$$(JULIA_CPU_TARGET)" $$(HEAPLIM) --output-$$* $$(call cygpath_w,$$@).tmp $$(JULIA_SYSIMG_BUILD_FLAGS) \
			--startup-file=no --warn-overwrite=yes --sysimage $$(call cygpath_w,$$<) $$(call cygpath_w,$$(JULIAHOME)/contrib/generate_precompile.jl) $(JULIA_PRECOMPILE); then \
		echo '*** This error is usually fixed by running `make clean`. If the error persists$$(COMMA) try `make cleanall`. ***'; \
		false; \
	fi )
	@mv $$@.tmp $$@
.SECONDARY: $$(build_private_libdir)/sys$1-o.a $(build_private_libdir)/sys$1-bc.a # request Make to keep these files around
endef
$(eval $(call sysimg_builder,,-O3,$(JULIA_EXECUTABLE_release)))
$(eval $(call sysimg_builder,-debug,-O0,$(JULIA_EXECUTABLE_debug)))

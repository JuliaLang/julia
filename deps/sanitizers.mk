# Interrogate the compiler about where it is keeping its sanitizer libraries
ifeq ($(USECLANG),1)
SANITIZER_LIB_PATH := $(shell LANG=C $(CC) -print-runtime-dir)\:$(subst $(eval ) ,:,$(abspath $(wildcard $(shell LANG=C $(CC) -print-resource-dir)/lib/*/)))
else
SANITIZER_LIB_PATH := $(dir $(shell LANG=C $(CC) -print-file-name=libasan.so))
endif

# Given a colon-separated list of paths in $(2), find the location of the library given in $(1)
define pathsearch_all
$(firstword $(wildcard $(addsuffix /$(1),$(subst :, ,$(2)))))
endef

define copy_sanitizer_lib
install-sanitizers: $$(addprefix $$(build_libdir)/, $$(notdir $$(call pathsearch_all,$(1),$$(SANITIZER_LIB_PATH)))) | $$(build_shlibdir)
	@result=$$(call pathsearch_all,$(1),$$(SANITIZER_LIB_PATH)); \
	if [ -z "$$$$result" ]; then \
		echo "Sanitizer library $(1) not found in $$(SANITIZER_LIB_PATH)"; \
		exit 1; \
	fi
$$(addprefix $$(build_shlibdir)/,$(2)): $$(addprefix $$(dir $$(call pathsearch_all,$(1),$$(SANITIZER_LIB_PATH))),$(2)) | $$(build_shlibdir)
	-cp $$< $$@
	$(if $(filter $(OS), Linux), \
		  -$(PATCHELF) $(PATCHELF_SET_RPATH_ARG) '$$$$ORIGIN' $$@ , 0)
endef

ifeq ($(USECLANG),1)

## Clang libraries
$(eval $(call copy_sanitizer_lib,$(call versioned_libname,libclang_rt.asan-*),$(call versioned_libname,libclang_rt.asan-%)))

endif

get-sanitizers:
clean-sanitizers:
	-rm -f $(build_shlibdir)/libclang_rt.asan*$(SHLIB_EXT)*
distclean-sanitizers: clean-sanitizers

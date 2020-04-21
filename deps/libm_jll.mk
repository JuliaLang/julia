# If we're using OpenBLAS_jll, load it in!
ifeq ($(USE_BINARYBUILDER_OPENLIBM),1)
LIBM_JLL_LIBM := OpenLibm_jll.libopenlibm
LIBM_JLL_DEPS := OpenLibm_jll=05823500-19ac-5b8b-9628-191a04bc5112

else
# Otherwise, just try to `dlopen()` the given LIBM name:
LIBM_JLL_LIBM := \"$(LIBMNAME)\"
endif

# Generate our fake Libm_jll package
$(build_prefix)/manifest/libm:
$(eval $(call jll-generate,Libm_jll,libm=$(LIBM_JLL_LIBM),,b49a7c6d-e6d2-0dba-4221-0fcbec513d2b,$(LIBM_JLL_DEPS)))
# If we're using OpenBLAS_jll, load it in!
ifeq ($(USE_BINARYBUILDER_OPENBLAS),1)
BLAS_JLL_BLASLIB := OpenBLAS_jll.libopenblas
BLAS_JLL_LAPACKLIB := OpenBLAS_jll.libopenblas
BLAS_JLL_DEPS := OpenBLAS_jll=4536629a-c528-5b80-bd46-f80d51c5b363

# If we're using MKL_jll, load it in! (Note: this is pending a wrapper library for MKL ILP64)
else ifeq ($(USE_BINARYBUILDER_MKL),1)
ifeq ($(USE_BLAS64))
BLAS_JLL_BLASLIB := MKL_jll.libmkl_intel_ilp64
BLAS_JLL_LAPACKLIB := MKL_jll.libmkl_intel_ilp64
else
BLAS_JLL_BLASLIB := MKL_jll.libmkl_intel_lp64
BLAS_JLL_LAPACKLIB := MKL_jll.libmkl_intel_lp64
endif
BLAS_JLL_DEPS := MKL_jl=856f044c-d86e-5d09-b602-aeab76dc8ba7
else

# Otherwise, just try to `dlopen()` the given BLASNAME:
BLAS_JLL_BLASLIB := \"$(LIBBLASNAME)\"
BLAS_JLL_LAPACKLIB := \"$(LIBLAPACKNAME)\"
endif

# Generate our fake BLAS_jll package
$(build_prefix)/manifest/blas:
$(eval $(call jll-generate,BLAS_jll,libblas=$(BLAS_JLL_BLASLIB) liblapack=$(BLAS_JLL_LAPACKLIB),, \
						   f1936524-4db9-4c7a-6f3e-6fc869057263, \
						   $(BLAS_JLL_DEPS)))
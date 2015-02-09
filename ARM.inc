FORCE_ARMV7=1
LLVM_ASSERTIONS=1
LLVM_FLAGS+="--with-cpu=cortex-a9 --with-float=hard --with-abi=aapcs-vfp --with-fpu=neon --enable-targets=arm --enable-optimized --enable-assertions"

override OPENBLAS_DYNAMIC_ARCH=0
override OPENBLAS_TARGET_ARCH=ARMV7
override USE_BLAS64=0

override LLVM_VER=3.5.0

override USE_SYSTEM_FFTW=1
override USE_SYSTEM_GMP=1
override USE_SYSTEM_MPFR=1

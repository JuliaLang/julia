if(HAVE_AVX)
configureSourceFiles(ALL_CODELETS "simd-avx.h" "${CMAKE_CURRENT_BINARY_DIR}" "${TOP_SRCDIR}/dft/simd/avx" SOURCES)

set(CMAKE_C_FLAGS ${AVX_CFLAGS})
addObjects(rdft_avx_codelets ${SOURCES})
endif(HAVE_AVX)


########### install files ###############




#original Makefile.am contents follow:

#AM_CFLAGS = $(AVX_CFLAGS)
#SIMD_HEADER=simd-avx.h
#
#include $(top_srcdir)/rdft/simd/codlist.mk
#include $(top_srcdir)/rdft/simd/simd.mk
#
#if HAVE_AVX
#
#noinst_LTLIBRARIES = librdft_avx_codelets.la
#BUILT_SOURCES = $(EXTRA_DIST)
#librdft_avx_codelets_la_SOURCES = $(BUILT_SOURCES)
#
#endif
#
#

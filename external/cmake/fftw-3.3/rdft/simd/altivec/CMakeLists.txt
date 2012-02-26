if(HAVE_ALTIVEC)
configureSourceFiles(ALL_CODELETS "simd-altivec.h" "${CMAKE_CURRENT_BINARY_DIR}" "${TOP_SRCDIR}/rdft/simd/altivec" SOURCES)

set(CMAKE_C_FLAGS ${ALTIVEC_CFLAGS})
#add_library(rdft_altivec_codelets ${SOURCES})
endif(HAVE_ALTIVEC)

########### install files ###############




#original Makefile.am contents follow:

#AM_CFLAGS = $(ALTIVEC_CFLAGS)
#SIMD_HEADER=simd-altivec.h
#
#include $(top_srcdir)/rdft/simd/codlist.mk
#include $(top_srcdir)/rdft/simd/simd.mk
#
#if HAVE_ALTIVEC
#
#noinst_LTLIBRARIES = librdft_altivec_codelets.la
#BUILT_SOURCES = $(EXTRA_DIST)
#librdft_altivec_codelets_la_SOURCES = $(BUILT_SOURCES)
#
#endif

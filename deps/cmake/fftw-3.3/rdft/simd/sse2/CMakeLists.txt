IF(HAVE_SSE2)
configureSourceFiles(ALL_CODELETS "simd-sse2.h" "${CMAKE_CURRENT_BINARY_DIR}" "${TOP_SRCDIR}/rdft/simd/sse2" SOURCES)

set(CMAKE_C_FLAGS ${SSE2_CFLAGS})
addObjects(rdft_sse2_codelets ${SOURCES})
ENDIF()

########### install files ###############




#original Makefile.am contents follow:

#AM_CFLAGS = $(SSE2_CFLAGS)
#SIMD_HEADER=simd-sse2.h
#
#include $(top_srcdir)/rdft/simd/codlist.mk
#include $(top_srcdir)/rdft/simd/simd.mk
#
#if HAVE_SSE2
#
#BUILT_SOURCES = $(EXTRA_DIST)
#noinst_LTLIBRARIES = librdft_sse2_codelets.la
#librdft_sse2_codelets_la_SOURCES = $(BUILT_SOURCES)
#
#endif
#
#

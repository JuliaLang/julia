########### next target ###############
if(HAVE_SSE2)
configureSourceFiles(ALL_CODELETS "simd-sse2.h" "${CMAKE_CURRENT_BINARY_DIR}" "${TOP_SRCDIR}/dft/simd/sse2" SOURCES)

set(CMAKE_C_FLAGS ${SSE2_CFLAGS})
addObjects(dft_sse2_codelets ${SOURCES})
endif()

########### install files ###############

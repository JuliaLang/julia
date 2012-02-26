########### next target ###############
if(HAVE_AVX)
configureSourceFiles(ALL_CODELETS "simd-avx.h" "${CMAKE_CURRENT_BINARY_DIR}" "${TOP_SRCDIR}/dft/simd/avx" SOURCES)

#set(CMAKE_C_FLAGS ${AVX_CFLAGS})
addObjects(dft_avx_codelets ${SOURCES})
endif()

########### install files ###############

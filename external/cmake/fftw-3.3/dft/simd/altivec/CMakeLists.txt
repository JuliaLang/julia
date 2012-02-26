########### next target ###############
if(HAVE_ALTIVEC)
configureSourceFiles(ALL_CODELETS "simd-altivec.h" "${CMAKE_CURRENT_BINARY_DIR}" "${TOP_SRCDIR}/dft/simd/altivec" SOURCES)

set(CMAKE_C_FLAGS  ${ALTIVEC_CFLAGS})
addObjects(dft_altivec_codelets ${SOURCES})
endif()

########### install files ###############

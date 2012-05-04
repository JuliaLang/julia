########### install files ###############

set(HC2CFDFTV
hc2cfdftv_2.c
hc2cfdftv_4.c
hc2cfdftv_6.c
hc2cfdftv_8.c
hc2cfdftv_10.c
hc2cfdftv_12.c
hc2cfdftv_16.c
hc2cfdftv_32.c
hc2cfdftv_20.c)

set(HC2CBDFTV 
hc2cbdftv_2.c
hc2cbdftv_4.c
hc2cbdftv_6.c
hc2cbdftv_8.c
hc2cbdftv_10.c
hc2cbdftv_12.c
hc2cbdftv_16.c
hc2cbdftv_32.c
hc2cbdftv_20.c)

###########################################################################
list(APPEND SIMD_CODELETS ${HC2CFDFTV} ${HC2CBDFTV})
list(APPEND ALL_CODELETS ${SIMD_CODELETS} genus.c codlist.c)

function(configureSourceFiles codeletsVariable header binDir srcDir srcVar)
foreach(f ${${codeletsVariable}})
	file(WRITE ${binDir}/${f} "/* Generated automatically.  DO NOT EDIT! */\n #define SIMD_HEADER \"${header}\" \n#include \"${srcDir}/../common/${f}.c\"")
	list(APPEND srcVar_TMP ${f})
endforeach()
set(${srcVar} "${srcVar_TMP}" PARENT_SCOPE)
endfunction()

include_directories("${TOP_SRCDIR}/simd-support" "${TOP_SRCDIR}/rdft" "${TOP_SRCDIR}/rdft/simd" "${TOP_SRCDIR}/rdft/common" "${TOP_SRCDIR}/kernel")
add_subdirectory(common)
add_subdirectory(sse2)
add_subdirectory(avx)
add_subdirectory(altivec)



#original Makefile.am contents follow:

#SUBDIRS = common sse2 avx altivec
#EXTRA_DIST = hc2cbv.h hc2cfv.h codlist.mk simd.mk

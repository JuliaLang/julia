
include_directories(${KDE4_INCLUDES} ${KDE4_INCLUDE_DIR} ${QT_INCLUDES} )


########### install files ###############




#original Makefile.am contents follow:

## include the list of codelets
#
#include $(top_srcdir)/rdft/simd/codlist.mk
#
#ALL_CODELETS = $(SIMD_CODELETS)
#BUILT_SOURCES= $(SIMD_CODELETS) $(CODLIST)
#EXTRA_DIST = $(BUILT_SOURCES) genus.c
#INCLUDE_SIMD_HEADER="\#include SIMD_HEADER"
#XRENAME=XSIMD
#SOLVTAB_NAME = XSIMD(solvtab_rdft)
#
## include special rules for regenerating codelets.
#include $(top_srcdir)/support/Makefile.codelets
#
#if MAINTAINER_MODE
#FLAGS_HC2C=-simd $(FLAGS_COMMON) -pipeline-latency 8 -trivial-stores -variables 32 -no-generate-bytw
#
#hc2cfdftv_%.c:  $(CODELET_DEPS) $(GEN_HC2CDFT_C)
#	($(PRELUDE_COMMANDS_RDFT); $(TWOVERS) $(GEN_HC2CDFT_C) $(FLAGS_HC2C) -n $* -dit -name hc2cfdftv_$* -include "hc2cfv.h") | $(ADD_DATE) | $(INDENT) >$@
#
#hc2cbdftv_%.c:  $(CODELET_DEPS) $(GEN_HC2CDFT_C)
#	($(PRELUDE_COMMANDS_RDFT); $(TWOVERS) $(GEN_HC2CDFT_C) $(FLAGS_HC2C) -n $* -dif -sign 1 -name hc2cbdftv_$* -include "hc2cbv.h") | $(ADD_DATE) | $(INDENT) >$@
#
#endif # MAINTAINER_MODE

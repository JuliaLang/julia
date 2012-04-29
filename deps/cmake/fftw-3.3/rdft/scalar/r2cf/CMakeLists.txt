include_directories ("${TOP_SRCDIR}/kernel" "${TOP_SRCDIR}/rdft" "${TOP_SRCDIR}/rdft/scalar")

## r2cf_<n> is a hard-coded real-to-complex FFT of size <n> (base cases
## of real-input FFT recursion)
set(R2CF 
r2cf_2.c
r2cf_3.c
r2cf_4.c
r2cf_5.c
r2cf_6.c
r2cf_7.c
r2cf_8.c
r2cf_9.c
r2cf_10.c
r2cf_11.c
r2cf_12.c
r2cf_13.c
r2cf_14.c
r2cf_15.c
r2cf_16.c
r2cf_32.c
r2cf_64.c
r2cf_128.c
r2cf_20.c
r2cf_25.c 
# r2cf_30.c
# r2cf_40.c
# r2cf_50.c
)

## hf_<r> is a "twiddle" FFT of size <r>, implementing a radix-r DIT
## step for a real-input FFT.  Every hf codelet must have a
## corresponding r2cfII codelet (see below)!
set(HF 
hf_2.c
hf_3.c
hf_4.c
hf_5.c
hf_6.c
hf_7.c
hf_8.c
hf_9.c
hf_10.c
hf_12.c
hf_15.c
hf_16.c
hf_32.c
hf_64.c
hf_20.c
hf_25.c 
# hf_30.c
# hf_40.c
# hf_50.c
)

## like hf, but generates part of its trig table on the fly (good for large n)
set(HF2 
hf2_4.c
hf2_8.c
hf2_16.c
hf2_32.c
hf2_5.c
hf2_20.c
hf2_25.c
)
#
## an r2cf transform where the input is shifted by half a sample (output
## is multiplied by a phase).  This is needed as part of the DIT recursion;
## every hf_<r> or
#hf2_<r> codelet should have a corresponding r2cfII_<r>
set(R2CFII 
r2cfII_2.c
r2cfII_3.c
r2cfII_4.c
r2cfII_5.c
r2cfII_6.c	
r2cfII_7.c
r2cfII_8.c
r2cfII_9.c
r2cfII_10.c
r2cfII_12.c
r2cfII_15.c
r2cfII_16.c
r2cfII_32.c
r2cfII_64.c 
r2cfII_20.c
r2cfII_25.c 
# r2cfII_30.c
#r2cfII_40.c
#r2cfII_50.c
)
#
############################################################################
## hc2cf_<r> is a "twiddle" FFT of size <r>, implementing a radix-r DIT
## step for a real-input FFT with rdft2-style output.  <r> must be even.
set(HC2CF 
hc2cf_2.c
hc2cf_4.c
hc2cf_6.c
hc2cf_8.c
hc2cf_10.c
hc2cf_12.c	
hc2cf_16.c
hc2cf_32.c 
hc2cf_20.c 
# hc2cf_30.c
)
#
set(HC2CFDFT 
hc2cfdft_2.c
hc2cfdft_4.c
hc2cfdft_6.c
hc2cfdft_8.c
hc2cfdft_10.c
hc2cfdft_12.c
hc2cfdft_16.c
hc2cfdft_32.c
hc2cfdft_20.c 
# hc2cfdft_30.c
)
#
## like hc2cf, but generates part of its trig table on the fly (good
## for large n)
set(HC2CF2 
hc2cf2_4.c
hc2cf2_8.c
hc2cf2_16.c
hc2cf2_32.c
hc2cf2_20.c 
# hc2cf2_30.c
)
set(HC2CFDFT2 
hc2cfdft2_4.c
hc2cfdft2_8.c
hc2cfdft2_16.c
hc2cfdft2_32.c
hc2cfdft2_20.c 
# hc2cfdft2_30.c
)

########### install files ###############

list(APPEND ALL_CODELETS ${R2CF} ${HF} ${HF2} ${R2CFII} ${HC2CF} ${HC2CF2} ${HC2CFDFT} ${HC2CFDFT2})
list(APPEND BUILT_SOURCES ${ALL_CODELETS} codlist.c)

include_directories("${TOP_SCRDIR}/kernel" "${TOP_SCRDIR}/rdft" "${TOP_SCRDIR}/rdft/scalar")

addObjects(rdft_scalar_r2cf ${BUILT_SOURCES})
#target_link_libraries(rdft_scalar_r2cf rdft_scalar rdft rdft_scalar_r2cb  rdft_scalar_r2r)

#original Makefile.am contents follow:

## This Makefile.am specifies a set of codelets, efficient transforms
## of small sizes, that are used as building blocks (kernels) by FFTW
## to build up large transforms, as well as the options for generating
## and compiling them.
#
## You can customize FFTW for special needs, e.g. to handle certain
## sizes more efficiently, by adding new codelets to the lists of those
## included by default.  If you change the list of codelets, any new
## ones you added will be automatically generated when you run the
## bootstrap script (see "Generating your own code" in the FFTW
## manual).
#
############################################################################
############################################################################
#ALL_CODELETS = $(R2CF) $(HF) $(HF2) $(R2CFII) $(HC2CF) $(HC2CF2)	\
#$(HC2CFDFT) $(HC2CFDFT2)
#
#BUILT_SOURCES= $(ALL_CODELETS) $(CODLIST)
#
#librdft_scalar_r2cf_la_SOURCES = $(BUILT_SOURCES)
#
#SOLVTAB_NAME = X(solvtab_rdft_r2cf)
#XRENAME=X
#
## special rules for regenerating codelets.
#include $(top_srcdir)/support/Makefile.codelets
#
#if MAINTAINER_MODE
#FLAGS_R2CF=$(RDFT_FLAGS_COMMON)
#FLAGS_HF=$(RDFT_FLAGS_COMMON)
#FLAGS_HF2=$(RDFT_FLAGS_COMMON) -twiddle-log3 -precompute-twiddles
#FLAGS_HC2CF=$(RDFT_FLAGS_COMMON)
#FLAGS_HC2CF2=$(RDFT_FLAGS_COMMON) -twiddle-log3 -precompute-twiddles
#FLAGS_R2CFII=$(RDFT_FLAGS_COMMON)
#
#r2cf_%.c:  $(CODELET_DEPS) $(GEN_R2CF)
#	($(PRELUDE_COMMANDS_RDFT); $(TWOVERS) $(GEN_R2CF) $(FLAGS_R2CF) -n $* -name r2cf_$* -include "r2cf.h") | $(ADD_DATE) | $(INDENT) >$@
#
#hf_%.c:  $(CODELET_DEPS) $(GEN_HC2HC)
#	($(PRELUDE_COMMANDS_RDFT); $(TWOVERS) $(GEN_HC2HC) $(FLAGS_HF) -n $* -dit -name hf_$* -include "hf.h") | $(ADD_DATE) | $(INDENT) >$@
#
#hf2_%.c:  $(CODELET_DEPS) $(GEN_HC2HC)
#	($(PRELUDE_COMMANDS_RDFT); $(TWOVERS) $(GEN_HC2HC) $(FLAGS_HF2) -n $* -dit -name hf2_$* -include "hf.h") | $(ADD_DATE) | $(INDENT) >$@
#
#r2cfII_%.c:  $(CODELET_DEPS) $(GEN_R2CF)
#	($(PRELUDE_COMMANDS_RDFT); $(TWOVERS) $(GEN_R2CF) $(FLAGS_R2CF) -n $* -name r2cfII_$* -dft-II -include "r2cfII.h") | $(ADD_DATE) | $(INDENT) >$@
#
#hc2cf_%.c:  $(CODELET_DEPS) $(GEN_HC2C)
#	($(PRELUDE_COMMANDS_RDFT); $(TWOVERS) $(GEN_HC2C) $(FLAGS_HC2CF) -n $* -dit -name hc2cf_$* -include "hc2cf.h") | $(ADD_DATE) | $(INDENT) >$@
#
#hc2cf2_%.c:  $(CODELET_DEPS) $(GEN_HC2C)
#	($(PRELUDE_COMMANDS_RDFT); $(TWOVERS) $(GEN_HC2C) $(FLAGS_HC2CF2) -n $* -dit -name hc2cf2_$* -include "hc2cf.h") | $(ADD_DATE) | $(INDENT) >$@
#
#hc2cfdft_%.c:  $(CODELET_DEPS) $(GEN_HC2CDFT)
#	($(PRELUDE_COMMANDS_RDFT); $(TWOVERS) $(GEN_HC2CDFT) $(FLAGS_HC2CF) -n $* -dit -name hc2cfdft_$* -include "hc2cf.h") | $(ADD_DATE) | $(INDENT) >$@
#
#hc2cfdft2_%.c:  $(CODELET_DEPS) $(GEN_HC2CDFT)
#	($(PRELUDE_COMMANDS_RDFT); $(TWOVERS) $(GEN_HC2CDFT) $(FLAGS_HC2CF2) -n $* -dit -name hc2cfdft2_$* -include "hc2cf.h") | $(ADD_DATE) | $(INDENT) >$@
#
#endif # MAINTAINER_MODE

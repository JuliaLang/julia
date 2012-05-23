include_directories ("${TOP_SRCDIR}/kernel" "${TOP_SRCDIR}/rdft" "${TOP_SRCDIR}/rdft/scalar")

########### next target ###############

set(R2CB 
r2cb_2.c
r2cb_3.c
r2cb_4.c
r2cb_5.c
r2cb_6.c
r2cb_7.c
r2cb_8.c
r2cb_9.c
r2cb_10.c
r2cb_11.c
r2cb_12.c
r2cb_13.c
r2cb_14.c
r2cb_15.c
r2cb_16.c
r2cb_32.c
r2cb_64.c
r2cb_128.c
r2cb_20.c
r2cb_25.c
# r2cb_30.c
# r2cb_40.c
# r2cb_50.c
)
#
############################################################################
## hb_<r> is a "twiddle" FFT of size <r>, implementing a radix-r DIF
## step for a real-output FFT.  Every hb codelet must have a
## corresponding r2cbIII codelet (see below)!
set(HB 
hb_2.c
hb_3.c
hb_4.c
hb_5.c
hb_6.c
hb_7.c
hb_8.c
hb_9.c
hb_10.c
hb_12.c
hb_15.c
hb_16.c
hb_32.c
hb_64.c
hb_20.c
hb_25.c 
# hb_30.c
# hb_40.c
# hb_50.c
)
#
## like hb, but generates part of its trig table on the fly (good for large n)
set(HB2 hb2_4.c
hb2_8.c
hb2_16.c
hb2_32.c
hb2_5.c
hb2_20.c
hb2_25.c)
#
## an r2cb transform where the output is shifted by half a sample (input
## is multiplied by a phase).  This is needed as part of the DIF recursion;
## every hb_<r> or hb2_<r> codelet should have a corresponding r2cbIII_<r>
set(R2CBIII r2cbIII_2.c
r2cbIII_3.c
r2cbIII_4.c
r2cbIII_5.c
r2cbIII_6.c
r2cbIII_7.c
r2cbIII_8.c
r2cbIII_9.c
r2cbIII_10.c
r2cbIII_12.c
r2cbIII_15.c
r2cbIII_16.c
r2cbIII_32.c
r2cbIII_64.c
r2cbIII_20.c
r2cbIII_25.c 
# r2cbIII_30.c
# r2cbIII_40.c
# r2cbIII_50.c
)
#
############################################################################
## hc2cb_<r> is a "twiddle" FFT of size <r>, implementing a radix-r DIF
## step for a real-input FFT with rdft2-style output.  <r> must be even.
set(HC2CB hc2cb_2.c
hc2cb_4.c
hc2cb_6.c
hc2cb_8.c
hc2cb_10.c
hc2cb_12.c
hc2cb_16.c
hc2cb_32.c 
hc2cb_20.c 
# hc2cb_30.c
)
#
set(HC2CBDFT hc2cbdft_2.c
hc2cbdft_4.c
hc2cbdft_6.c
hc2cbdft_8.c
hc2cbdft_10.c
hc2cbdft_12.c
hc2cbdft_16.c
hc2cbdft_32.c
hc2cbdft_20.c 
# hc2cbdft_30.c
)
#
## like hc2cb, but generates part of its trig table on the fly (good
## for large n)
set(HC2CB2 hc2cb2_4.c
hc2cb2_8.c
hc2cb2_16.c
hc2cb2_32.c
hc2cb2_20.c 
# hc2cb2_30.c
)
set(HC2CBDFT2 hc2cbdft2_4.c
hc2cbdft2_8.c
hc2cbdft2_16.c
hc2cbdft2_32.c
hc2cbdft2_20.c 
# hc2cbdft2_30.c
)

list(APPEND ALL_CODELETS ${R2CB} ${HB} ${HB2} ${R2CBIII} ${HC2CB} ${HC2CB2} ${HC2CBDFT} ${HC2CBDFT2})

list(APPEND ALL_CODELETS codlist.c)


addObjects(rdft_scalar_r2cb ${ALL_CODELETS})
#target_link_libraries(rdft_scalar_r2cb rdft_scalar_r2cf rdft_scalar rdft rdft_scalar_r2r)


########### install files ###############


#if MAINTAINER_MODE
#FLAGS_R2CB=$(RDFT_FLAGS_COMMON) -sign 1
#FLAGS_HB=$(RDFT_FLAGS_COMMON) -sign 1
#FLAGS_HB2=$(RDFT_FLAGS_COMMON) -sign 1 -twiddle-log3 -precompute-twiddles
#FLAGS_HC2CB=$(RDFT_FLAGS_COMMON) -sign 1
#FLAGS_HC2CB2=$(RDFT_FLAGS_COMMON) -sign 1 -twiddle-log3 -precompute-twiddles
#FLAGS_R2CBIII=$(RDFT_FLAGS_COMMON) -sign 1
#
#r2cb_%.c:  $(CODELET_DEPS) $(GEN_R2CB)
#	($(PRELUDE_COMMANDS_RDFT); $(TWOVERS) $(GEN_R2CB) $(FLAGS_R2CB) -n $* -name r2cb_$* -include "r2cb.h") | $(ADD_DATE) | $(INDENT) >$@
#
#hb_%.c:  $(CODELET_DEPS) $(GEN_HC2HC)
#	($(PRELUDE_COMMANDS_RDFT); $(TWOVERS) $(GEN_HC2HC) $(FLAGS_HB) -n $* -dif -name hb_$* -include "hb.h") | $(ADD_DATE) | $(INDENT) >$@
#
#hb2_%.c:  $(CODELET_DEPS) $(GEN_HC2HC)
#	($(PRELUDE_COMMANDS_RDFT); $(TWOVERS) $(GEN_HC2HC) $(FLAGS_HB2) -n $* -dif -name hb2_$* -include "hb.h") | $(ADD_DATE) | $(INDENT) >$@
#
#r2cbIII_%.c:  $(CODELET_DEPS) $(GEN_R2CB)
#	($(PRELUDE_COMMANDS_RDFT); $(TWOVERS) $(GEN_R2CB) $(FLAGS_R2CB) -n $* -name r2cbIII_$* -dft-III -include "r2cbIII.h") | $(ADD_DATE) | $(INDENT) >$@
#
#hc2cb_%.c:  $(CODELET_DEPS) $(GEN_HC2C)
#	($(PRELUDE_COMMANDS_RDFT); $(TWOVERS) $(GEN_HC2C) $(FLAGS_HC2CB) -n $* -dif -name hc2cb_$* -include "hc2cb.h") | $(ADD_DATE) | $(INDENT) >$@
#
#hc2cb2_%.c:  $(CODELET_DEPS) $(GEN_HC2C)
#	($(PRELUDE_COMMANDS_RDFT); $(TWOVERS) $(GEN_HC2C) $(FLAGS_HC2CB2) -n $* -dif -name hc2cb2_$* -include "hc2cb.h") | $(ADD_DATE) | $(INDENT) >$@
#
#hc2cbdft_%.c:  $(CODELET_DEPS) $(GEN_HC2CDFT)
#	($(PRELUDE_COMMANDS_RDFT); $(TWOVERS) $(GEN_HC2CDFT) $(FLAGS_HC2CB) -n $* -dif -name hc2cbdft_$* -include "hc2cb.h") | $(ADD_DATE) | $(INDENT) >$@
#
#hc2cbdft2_%.c:  $(CODELET_DEPS) $(GEN_HC2CDFT)
#	($(PRELUDE_COMMANDS_RDFT); $(TWOVERS) $(GEN_HC2CDFT) $(FLAGS_HC2CB) -n $* -dif -name hc2cbdft2_$* -include "hc2cb.h") | $(ADD_DATE) | $(INDENT) >$@
#
#endif # MAINTAINER_MODE

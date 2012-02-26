include_directories ("${TOP_SRCDIR}/kernel" "${TOP_SRCDIR}/rdft" "${TOP_SRCDIR}/rdft/scalar")


########### next target ###############
set(E00 
#e00_2.c 
#e00_3.c 
#e00_4.c
#e00_5.c
#e00_6.c
#e00_7.c
#e00_8.c
)
set(E01 
e01_8.c 
# e01_2.c 
# e01_3.c 
# e01_4.c
# e01_5.c 
# e01_6.c 
# e01_7.c
)
set(E10 
e10_8.c
# e10_2.c 
#e10_3.c 
#e10_4.c 
#e10_5.c 
#e10_6.c 
#e10_7.c
)
set(E11 
#e11_2.c 
#e11_3.c 
#e11_4.c 
#e11_5.c 
#e11_6.c 
#e11_7.c 
#e11_8.c
)

## o<a><b>_<n> is a hard-coded RODFT<a><b> FFT (DST) of size <n>
set(O00 
# o00_2.c 
#o00_3.c 
#o00_4.c 
#o00_5.c 
#o00_6.c 
#o00_7.c 
#o00_8.c
)
set(O01 
# o01_2.c
#o01_3.c
#o01_4.c
#o01_5.c
#o01_6.c
#o01_7.c
#o01_8.c
)
set(O10 
# o10_2.c 
#o10_3.c
#o10_4.c
#o10_5.c 
#o10_6.c 
#o10_7.c 
#o10_8.c
)
set(O11 
# o11_2.c
#o11_3.c
#o11_4.c
#o11_5.c
#o11_6.c
#o11_7.c
#o11_8.c
)
#
## dht_<n> is a hard-coded DHT of size <n>
set(DHT 
# dht_2.c
#dht_3.c
#dht_4.c
#dht_5.c
#dht_6.c
#dht_7.c
#dht_8.c
)

########### install files ###############

include_directories("${TOP_SCRDIR}/kernel" "${TOP_SCRDIR}/rdft" "${TOP_SCRDIR}/rdft/scalar")

list(APPEND ALL_CODELETS ${E00} ${E01} ${E10} ${E11} ${O00} ${O01} ${O10} ${O11} ${DHT})

addObjects(rdft_scalar_r2r codlist.c ${ALL_CODELETS})
#target_link_libraries(rdft_scalar_r2r rdft_scalar_r2cf rdft_scalar rdft rdft_scalar_r2cb)

set(SOLVTAB_NAME "X(solvtab_rdft_r2r)")
set(XRENAME X)


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
#AM_CPPFLAGS = -I$(top_srcdir)/kernel -I$(top_srcdir)/rdft	\
#-I$(top_srcdir)/rdft/scalar
#noinst_LTLIBRARIES = librdft_scalar_r2r.la
#
############################################################################
## The following lines specify the REDFT/RODFT/DHT sizes for which to generate
## specialized codelets.  Currently, only REDFT01/10 of size 8 (used in JPEG).
#
## e<a><b>_<n> is a hard-coded REDFT<a><b> FFT (DCT) of size <n>
#E00 = # e00_2.c e00_3.c e00_4.c e00_5.c e00_6.c e00_7.c e00_8.c
#E01 = e01_8.c # e01_2.c e01_3.c e01_4.c e01_5.c e01_6.c e01_7.c
#E10 = e10_8.c # e10_2.c e10_3.c e10_4.c e10_5.c e10_6.c e10_7.c
#E11 = # e11_2.c e11_3.c e11_4.c e11_5.c e11_6.c e11_7.c e11_8.c
#
## o<a><b>_<n> is a hard-coded RODFT<a><b> FFT (DST) of size <n>
#O00 = # o00_2.c o00_3.c o00_4.c o00_5.c o00_6.c o00_7.c o00_8.c
#O01 = # o01_2.c o01_3.c o01_4.c o01_5.c o01_6.c o01_7.c o01_8.c
#O10 = # o10_2.c o10_3.c o10_4.c o10_5.c o10_6.c o10_7.c o10_8.c
#O11 = # o11_2.c o11_3.c o11_4.c o11_5.c o11_6.c o11_7.c o11_8.c
#
## dht_<n> is a hard-coded DHT of size <n>
#DHT = # dht_2.c dht_3.c dht_4.c dht_5.c dht_6.c dht_7.c dht_8.c
#
############################################################################
#ALL_CODELETS = $(E00) $(E01) $(E10) $(E11) $(O00) $(O01) $(O10) $(O11) $(DHT)
#
#BUILT_SOURCES= $(ALL_CODELETS) $(CODLIST)
#
#librdft_scalar_r2r_la_SOURCES = $(BUILT_SOURCES)
#
#SOLVTAB_NAME = X(solvtab_rdft_r2r)
#XRENAME=X
#
## special rules for regenerating codelets.
#include $(top_srcdir)/support/Makefile.codelets
#
#if MAINTAINER_MODE
#FLAGS_E00=$(RDFT_FLAGS_COMMON)
#FLAGS_E01=$(RDFT_FLAGS_COMMON)
#FLAGS_E10=$(RDFT_FLAGS_COMMON)
#FLAGS_E11=$(RDFT_FLAGS_COMMON)
#FLAGS_O00=$(RDFT_FLAGS_COMMON)
#FLAGS_O01=$(RDFT_FLAGS_COMMON)
#FLAGS_O10=$(RDFT_FLAGS_COMMON)
#FLAGS_O11=$(RDFT_FLAGS_COMMON)
#FLAGS_DHT=$(RDFT_FLAGS_COMMON)
#
#e00_%.c:  $(CODELET_DEPS) $(GEN_R2R)
#	($(PRELUDE_COMMANDS_RDFT); $(TWOVERS) $(GEN_R2R) $(FLAGS_E00) -redft00 -n $* -name e00_$* -include "r2r.h") | $(ADD_DATE) | $(INDENT) >$@
#
#e01_%.c:  $(CODELET_DEPS) $(GEN_R2R)
#	($(PRELUDE_COMMANDS_RDFT); $(TWOVERS) $(GEN_R2R) $(FLAGS_E01) -redft01 -n $* -name e01_$* -include "r2r.h") | $(ADD_DATE) | $(INDENT) >$@
#
#e10_%.c:  $(CODELET_DEPS) $(GEN_R2R)
#	($(PRELUDE_COMMANDS_RDFT); $(TWOVERS) $(GEN_R2R) $(FLAGS_E10) -redft10 -n $* -name e10_$* -include "r2r.h") | $(ADD_DATE) | $(INDENT) >$@
#
#e11_%.c:  $(CODELET_DEPS) $(GEN_R2R)
#	($(PRELUDE_COMMANDS_RDFT); $(TWOVERS) $(GEN_R2R) $(FLAGS_E11) -redft11 -n $* -name e11_$* -include "r2r.h") | $(ADD_DATE) | $(INDENT) >$@
#
#
#o00_%.c:  $(CODELET_DEPS) $(GEN_R2R)
#	($(PRELUDE_COMMANDS_RDFT); $(TWOVERS) $(GEN_R2R) $(FLAGS_O00) -rodft00 -n $* -name o00_$* -include "r2r.h") | $(ADD_DATE) | $(INDENT) >$@
#
#o01_%.c:  $(CODELET_DEPS) $(GEN_R2R)
#	($(PRELUDE_COMMANDS_RDFT); $(TWOVERS) $(GEN_R2R) $(FLAGS_O01) -rodft01 -n $* -name o01_$* -include "r2r.h") | $(ADD_DATE) | $(INDENT) >$@
#
#o10_%.c:  $(CODELET_DEPS) $(GEN_R2R)
#	($(PRELUDE_COMMANDS_RDFT); $(TWOVERS) $(GEN_R2R) $(FLAGS_O10) -rodft10 -n $* -name o10_$* -include "r2r.h") | $(ADD_DATE) | $(INDENT) >$@
#
#o11_%.c:  $(CODELET_DEPS) $(GEN_R2R)
#	($(PRELUDE_COMMANDS_RDFT); $(TWOVERS) $(GEN_R2R) $(FLAGS_O11) -rodft11 -n $* -name o11_$* -include "r2r.h") | $(ADD_DATE) | $(INDENT) >$@
#
#
#dht_%.c:  $(CODELET_DEPS) $(GEN_R2R)
#	($(PRELUDE_COMMANDS_RDFT); $(TWOVERS) $(GEN_R2R) $(FLAGS_DHT) -dht -sign 1 -n $* -name dht_$* -include "r2r.h") | $(ADD_DATE) | $(INDENT) >$@
#
#endif # MAINTAINER_MODE

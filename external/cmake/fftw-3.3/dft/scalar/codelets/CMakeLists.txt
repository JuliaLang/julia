########### next target ###############

set(dft_scalar_codelets_STAT_SRCS $(BUILT_SOURCES))

include_directories("${TOP_SRCDIR}/kernel" "${TOP_SRCDIR}/dft" "${TOP_SRCDIR}/dft/scalar")

set(N1 
n1_2.c
n1_3.c
n1_4.c
n1_5.c
n1_6.c
n1_7.c
n1_8.c
n1_9.c
n1_10.c
n1_11.c
n1_12.c
n1_13.c
n1_14.c 
n1_15.c
n1_16.c
n1_32.c
n1_64.c
n1_20.c
n1_25.c
#n1_30.c
#n1_40.c
#n1_50.c
)

set(T1
t1_2.c
t1_3.c
t1_4.c
t1_5.c
t1_6.c
t1_7.c
t1_8.c
t1_9.c
t1_10.c
t1_12.c
t1_15.c
t1_16.c
t1_32.c
t1_64.c 
t1_20.c
t1_25.c
#t1_30.c
#t1_40.c
#t1_50.c
)

set(T2 
t2_4.c
t2_8.c
t2_16.c
t2_32.c
t2_64.c
t2_5.c
t2_10.c
t2_20.c
t2_25.c)

set(F1 
#f1_2.c
#f1_3.c
#f1_4.c
#f1_5.c
#f1_6.c
#f1_7.c
#f1_8.c
#f1_9.c
#f1_10.c
#f1_12.c
#f1_15.c
#f1_16.c
#f1_32.c
#f1_64.c
)

set(F2 
#f2_4.c
#f2_8.c
#f2_16.c
#f2_32.c
#f2_64.c
)
set(Q1 
q1_2.c
q1_4.c
q1_8.c
q1_3.c
q1_5.c
q1_6.c)

list(APPEND codelets ${N1} ${T1} ${T2} ${F1} ${F2} ${Q1})

addObjects(dft_scalar_codelets codlist.c ${codelets})
#target_link_libraries(dft_scalar_codelets dft_scalar dft)


########### install files ###############




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
#AM_CPPFLAGS = -I$(top_srcdir)/kernel -I$(top_srcdir)/dft	\
#-I$(top_srcdir)/dft/scalar
#noinst_LTLIBRARIES = libdft_scalar_codelets.la
#
############################################################################
## n1_<n> is a hard-coded FFT of size <n> (base cases of FFT recursion)
#N1 = n1_2.c n1_3.c n1_4.c n1_5.c n1_6.c n1_7.c n1_8.c n1_9.c n1_10.c	\
#n1_11.c n1_12.c n1_13.c n1_14.c n1_15.c n1_16.c n1_32.c n1_64.c \
#n1_20.c n1_25.c # n1_30.c n1_40.c n1_50.c
#
############################################################################
## t1_<r> is a "twiddle" FFT of size <r>, implementing a radix-r DIT step
#T1 = t1_2.c t1_3.c t1_4.c t1_5.c t1_6.c t1_7.c t1_8.c t1_9.c	\
#t1_10.c t1_12.c t1_15.c t1_16.c t1_32.c t1_64.c \
#t1_20.c t1_25.c # t1_30.c t1_40.c t1_50.c
#
## t2_<r> is also a twiddle FFT, but instead of using a complete lookup table
## of trig. functions, it partially generates the trig. values on the fly
## (this is faster for large sizes).
#T2 = t2_4.c t2_8.c t2_16.c t2_32.c t2_64.c \
#     t2_5.c t2_10.c t2_20.c t2_25.c
#
############################################################################
## The F (DIF) codelets are used for a kind of in-place transform algorithm,
## but the planner seems to never (or hardly ever) use them on the machines
## we have access to, preferring the Q codelets and the use of buffers
## for sub-transforms.  So, we comment them out, at least for now.
#
## f1_<r> is a "twiddle" FFT of size <r>, implementing a radix-r DIF step
#F1 = # f1_2.c f1_3.c f1_4.c f1_5.c f1_6.c f1_7.c f1_8.c f1_9.c f1_10.c f1_12.c f1_15.c f1_16.c f1_32.c f1_64.c
#
## like f1, but partially generates its trig. table on the fly
#F2 = # f2_4.c f2_8.c f2_16.c f2_32.c f2_64.c
#
############################################################################
## q1_<r> is <r> twiddle FFTs of size <r> (DIF step), where the output is
## transposed.  This is used for in-place transposes in sizes that are
## divisible by <r>^2.  These codelets have size ~ <r>^2, so you should
## probably not use <r> bigger than 8 or so.
#Q1 = q1_2.c q1_4.c q1_8.c  q1_3.c q1_5.c q1_6.c
#
############################################################################
#ALL_CODELETS = $(N1) $(T1) $(T2) $(F1) $(F2) $(Q1)
#BUILT_SOURCES= $(ALL_CODELETS) $(CODLIST)
#
#libdft_scalar_codelets_la_SOURCES = $(BUILT_SOURCES)
#
#SOLVTAB_NAME = X(solvtab_dft_standard)
#XRENAME=X
#
## special rules for regenerating codelets.
#include $(top_srcdir)/support/Makefile.codelets
#
#if MAINTAINER_MODE
#FLAGS_N1=$(DFT_FLAGS_COMMON)
#FLAGS_T1=$(DFT_FLAGS_COMMON)
#FLAGS_T2=$(DFT_FLAGS_COMMON) -twiddle-log3 -precompute-twiddles
#FLAGS_F1=$(DFT_FLAGS_COMMON)
#FLAGS_F2=$(DFT_FLAGS_COMMON) -twiddle-log3  -precompute-twiddles
#FLAGS_Q1=$(DFT_FLAGS_COMMON) -reload-twiddle
#FLAGS_Q2=$(DFT_FLAGS_COMMON) -twiddle-log3  -precompute-twiddles
#
#n1_%.c:  $(CODELET_DEPS) $(GEN_NOTW)
#	($(PRELUDE_COMMANDS_DFT); $(TWOVERS) $(GEN_NOTW) $(FLAGS_N1) -n $* -name n1_$* -include "n.h") | $(ADD_DATE) | $(INDENT) >$@
#
#t1_%.c:  $(CODELET_DEPS) $(GEN_TWIDDLE)
#	($(PRELUDE_COMMANDS_DFT); $(TWOVERS) $(GEN_TWIDDLE) $(FLAGS_T1) -n $* -name t1_$* -include "t.h") | $(ADD_DATE) | $(INDENT) >$@
#
#t2_%.c:  $(CODELET_DEPS) $(GEN_TWIDDLE)
#	($(PRELUDE_COMMANDS_DFT); $(TWOVERS) $(GEN_TWIDDLE) $(FLAGS_T2) -n $* -name t2_$* -include "t.h") | $(ADD_DATE) | $(INDENT) >$@
#
#f1_%.c:  $(CODELET_DEPS) $(GEN_TWIDDLE)
#	($(PRELUDE_COMMANDS_DFT); $(TWOVERS) $(GEN_TWIDDLE) $(FLAGS_F1) -dif -n $* -name f1_$* -include "f.h") | $(ADD_DATE) | $(INDENT) >$@
#
#f2_%.c:  $(CODELET_DEPS) $(GEN_TWIDDLE)
#	($(PRELUDE_COMMANDS_DFT); $(TWOVERS) $(GEN_TWIDDLE) $(FLAGS_F2) -dif -n $* -name f2_$* -include "f.h") | $(ADD_DATE) | $(INDENT) >$@
#
#q1_%.c:  $(CODELET_DEPS) $(GEN_TWIDSQ)
#	($(PRELUDE_COMMANDS_DFT); $(TWOVERS) $(GEN_TWIDSQ) $(FLAGS_Q1) -dif -n $* -name q1_$* -include "q.h") | $(ADD_DATE) | $(INDENT) >$@
#
#q2_%.c:  $(CODELET_DEPS) $(GEN_TWIDSQ)
#	($(PRELUDE_COMMANDS_DFT); $(TWOVERS) $(GEN_TWIDSQ) $(FLAGS_Q2) -dif -n $* -name q2_$* -include "q.h") | $(ADD_DATE) | $(INDENT) >$@
#
#endif # MAINTAINER_MODE

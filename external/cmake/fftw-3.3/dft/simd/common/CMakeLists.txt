########### install files ###############




#original Makefile.am contents follow:

## include the list of codelets
#
#include $(top_srcdir)/dft/simd/codlist.mk
#
#ALL_CODELETS = $(SIMD_CODELETS)
#BUILT_SOURCES= $(SIMD_CODELETS) $(CODLIST)
#EXTRA_DIST = $(BUILT_SOURCES) genus.c
#INCLUDE_SIMD_HEADER="\#include SIMD_HEADER"
#XRENAME=XSIMD
#SOLVTAB_NAME = XSIMD(solvtab_dft)
#
## include special rules for regenerating codelets.
#include $(top_srcdir)/support/Makefile.codelets
#
#if MAINTAINER_MODE
#
#GFLAGS = -simd $(FLAGS_COMMON) -pipeline-latency 8
#FLAGS_T2S=-twiddle-log3 -precompute-twiddles
#FLAGS_T3=-twiddle-log3 -precompute-twiddles -no-generate-bytw
#
#n1fv_%.c:  $(CODELET_DEPS) $(GEN_NOTW_C)
#	($(PRELUDE_COMMANDS_DFT); $(TWOVERS) $(GEN_NOTW_C) $(GFLAGS) -n $* -name n1fv_$* -include "n1f.h") | $(ADD_DATE) | $(INDENT) >$@
#
#n2fv_%.c:  $(CODELET_DEPS) $(GEN_NOTW_C)
#	($(PRELUDE_COMMANDS_DFT); $(TWOVERS) $(GEN_NOTW_C) $(GFLAGS) -n $* -name n2fv_$* -with-ostride 2 -include "n2f.h" -store-multiple 2) | $(ADD_DATE) | $(INDENT) >$@
#
#n1bv_%.c:  $(CODELET_DEPS) $(GEN_NOTW_C)
#	($(PRELUDE_COMMANDS_DFT); $(TWOVERS) $(GEN_NOTW_C) $(GFLAGS) -sign 1 -n $* -name n1bv_$* -include "n1b.h") | $(ADD_DATE) | $(INDENT) >$@
#
#n2bv_%.c:  $(CODELET_DEPS) $(GEN_NOTW_C)
#	($(PRELUDE_COMMANDS_DFT); $(TWOVERS) $(GEN_NOTW_C) $(GFLAGS) -sign 1 -n $* -name n2bv_$* -with-ostride 2 -include "n2b.h"  -store-multiple 2) | $(ADD_DATE) | $(INDENT) >$@
#
#n2sv_%.c:  $(CODELET_DEPS) $(GEN_NOTW)
#	($(PRELUDE_COMMANDS_DFT); $(TWOVERS) $(GEN_NOTW) $(GFLAGS) -n $* -name n2sv_$* -with-ostride 1 -include "n2s.h" -store-multiple 4) | $(ADD_DATE) | $(INDENT) >$@
#
#t1fv_%.c:  $(CODELET_DEPS) $(GEN_TWIDDLE_C)
#	($(PRELUDE_COMMANDS_DFT); $(TWOVERS) $(GEN_TWIDDLE_C) $(GFLAGS) -n $* -name t1fv_$* -include "t1f.h") | $(ADD_DATE) | $(INDENT) >$@
#
#t1fuv_%.c:  $(CODELET_DEPS) $(GEN_TWIDDLE_C)
#	($(PRELUDE_COMMANDS_DFT); $(TWOVERS) $(GEN_TWIDDLE_C) $(GFLAGS) -n $* -name t1fuv_$* -include "t1fu.h") | $(ADD_DATE) | $(INDENT) >$@
#
#t2fv_%.c:  $(CODELET_DEPS) $(GEN_TWIDDLE_C)
#	($(PRELUDE_COMMANDS_DFT); $(TWOVERS) $(GEN_TWIDDLE_C) $(GFLAGS) -n $* -name t2fv_$* -include "t2f.h") | $(ADD_DATE) | $(INDENT) >$@
#
#t3fv_%.c:  $(CODELET_DEPS) $(GEN_TWIDDLE_C)
#	($(PRELUDE_COMMANDS_DFT); $(TWOVERS) $(GEN_TWIDDLE_C) $(GFLAGS) $(FLAGS_T3) -n $* -name t3fv_$* -include "t3f.h") | $(ADD_DATE) | $(INDENT) >$@
#
#t1bv_%.c:  $(CODELET_DEPS) $(GEN_TWIDDLE_C)
#	($(PRELUDE_COMMANDS_DFT); $(TWOVERS) $(GEN_TWIDDLE_C) $(GFLAGS) -n $* -name t1bv_$* -include "t1b.h" -sign 1) | $(ADD_DATE) | $(INDENT) >$@
#
#t1buv_%.c:  $(CODELET_DEPS) $(GEN_TWIDDLE_C)
#	($(PRELUDE_COMMANDS_DFT); $(TWOVERS) $(GEN_TWIDDLE_C) $(GFLAGS) -n $* -name t1buv_$* -include "t1bu.h" -sign 1) | $(ADD_DATE) | $(INDENT) >$@
#
#t2bv_%.c:  $(CODELET_DEPS) $(GEN_TWIDDLE_C)
#	($(PRELUDE_COMMANDS_DFT); $(TWOVERS) $(GEN_TWIDDLE_C) $(GFLAGS) -n $* -name t2bv_$* -include "t2b.h" -sign 1) | $(ADD_DATE) | $(INDENT) >$@
#
#t3bv_%.c:  $(CODELET_DEPS) $(GEN_TWIDDLE_C)
#	($(PRELUDE_COMMANDS_DFT); $(TWOVERS) $(GEN_TWIDDLE_C) $(GFLAGS) $(FLAGS_T3) -n $* -name t3bv_$* -include "t3b.h" -sign 1) | $(ADD_DATE) | $(INDENT) >$@
#
#t1sv_%.c:  $(CODELET_DEPS) $(GEN_TWIDDLE)
#	($(PRELUDE_COMMANDS_DFT); $(TWOVERS) $(GEN_TWIDDLE) $(GFLAGS) -n $* -name t1sv_$* -include "ts.h") | $(ADD_DATE) | $(INDENT) >$@
#
#t2sv_%.c:  $(CODELET_DEPS) $(GEN_TWIDDLE)
#	($(PRELUDE_COMMANDS_DFT); $(TWOVERS) $(GEN_TWIDDLE) $(GFLAGS) $(FLAGS_T2S) -n $* -name t2sv_$* -include "ts.h") | $(ADD_DATE) | $(INDENT) >$@
#
#q1fv_%.c:  $(CODELET_DEPS) $(GEN_TWIDSQ_C)
#	($(PRELUDE_COMMANDS_DFT); $(TWOVERS) $(GEN_TWIDSQ_C) $(GFLAGS) -n $* -dif -name q1fv_$* -include "q1f.h") | $(ADD_DATE) | $(INDENT) >$@
#
#q1bv_%.c:  $(CODELET_DEPS) $(GEN_TWIDSQ_C)
#	($(PRELUDE_COMMANDS_DFT); $(TWOVERS) $(GEN_TWIDSQ_C) $(GFLAGS) -n $* -dif -name q1bv_$* -include "q1b.h" -sign 1) | $(ADD_DATE) | $(INDENT) >$@
#
#
#endif # MAINTAINER_MODE

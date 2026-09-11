// This file is a part of Julia. License is MIT: https://julialang.org/license

// RUN: clang -D__clang_safetyanalysis__ -Wthread-safety -Wthread-safety-negative -Xclang -verify -fsyntax-only -I%julia_home/src -I%julia_home/src/support -I%julia_home/usr/include ${CLANGSA_FLAGS} ${CPPFLAGS} ${CFLAGS} -x c %s

// Exercises the Clang Thread Safety Analysis ("capability") model of Julia
// safepoints that -D__clang_safetyanalysis__ selects in analyzer_annotations.h.
// Two tokens are modeled: the reentrant `jl_notsafepoint`, held inside a no-safepoint
// region (a no-gc lock), and the non-reentrant `jl_gcunsaferegion`, held while
// the thread is in a gc-unsafe region. A safepoint (JL_CANSAFEPOINT) requires
// jl_gcunsaferegion held and jl_notsafepoint NOT held.

#include "analyzer_annotations.h"

void may_safepoint(void) JL_CANSAFEPOINT;
void notsafepoint(void);
void lock_nogc(void) JL_NOTSAFEPOINT_ENTER;
void unlock_nogc(void) JL_NOTSAFEPOINT_LEAVE;
int trylock_nogc(void) JL_NOTSAFEPOINT_ENTER_CONDITIONAL(1);
int trylock0_nogc(void) JL_NOTSAFEPOINT_ENTER_CONDITIONAL(0); // acquires on a zero return
int gc_unsafe_enter(void) JL_CANSAFEPOINT_ENTER; // becomes gc-unsafe
void gc_unsafe_leave(void) JL_CANSAFEPOINT_LEAVE; // becomes gc-safe

void cansafepoint_calls_both(void) JL_CANSAFEPOINT {
    may_safepoint();
    notsafepoint();
}

void notsafepoint_body(void) {
    may_safepoint(); // expected-warning{{calling function 'may_safepoint' requires holding gcunsaferegion 'jl_gcunsaferegion' exclusively}}
                     // expected-warning@-1{{calling function 'may_safepoint' requires negative capability '!jl_notsafepoint'}}
}

void nogc_region(void) JL_CANSAFEPOINT {
    may_safepoint();
    lock_nogc();
    notsafepoint();
    may_safepoint(); // expected-warning{{cannot call function 'may_safepoint' while notsafepoint 'jl_notsafepoint' is held}}
    unlock_nogc();
    may_safepoint();
}

void nested_regions(void) JL_CANSAFEPOINT {
    lock_nogc();
    lock_nogc();
    notsafepoint();
    unlock_nogc();
    unlock_nogc();
    may_safepoint();
}

void conditional_region(void) JL_CANSAFEPOINT {
    if (trylock_nogc()) {
        may_safepoint(); // expected-warning{{cannot call function 'may_safepoint' while notsafepoint 'jl_notsafepoint' is held}}
        unlock_nogc();
    }
    may_safepoint();
}

void conditional_region_zero(void) JL_CANSAFEPOINT {
    if (trylock0_nogc()) {
        may_safepoint();
    } else {
        may_safepoint(); // expected-warning{{cannot call function 'may_safepoint' while notsafepoint 'jl_notsafepoint' is held}}
        unlock_nogc();
    }
}

void gc_safe_region(void) JL_CANSAFEPOINT {
    may_safepoint();
    gc_unsafe_leave();
    may_safepoint(); // expected-warning{{calling function 'may_safepoint' requires holding gcunsaferegion 'jl_gcunsaferegion' exclusively}}
    gc_unsafe_enter();
    may_safepoint();
}

void enter_leave_balanced(void) JL_CANSAFEPOINT_ENTER_LEAVE {
    gc_unsafe_enter();
    may_safepoint();
    gc_unsafe_leave();
}

void enter_leave_unbalanced(void) JL_CANSAFEPOINT_ENTER_LEAVE {
    gc_unsafe_enter(); // expected-note{{gcunsaferegion acquired here}}
    may_safepoint();
    // expected-warning@+1{{gcunsaferegion 'jl_gcunsaferegion' is still held at the end of function}}
}

// JL_NO_SAFEPOINT_ANALYSIS opts a body out of the analysis entirely: the
// unbalanced acquire below is not flagged.
void opted_out(void) JL_NO_SAFEPOINT_ANALYSIS {
    lock_nogc();
    may_safepoint();
}

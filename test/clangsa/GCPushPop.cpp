// This file is a part of Julia. License is MIT: https://julialang.org/license

// RUN: clang -D__clang_gcanalyzer__ --analyze -Xanalyzer -analyzer-output=text -Xclang -load -Xclang libGCCheckerPlugin%shlibext -Xclang -verify -I%julia_home/src -I%julia_home/src/support -I%julia_home/usr/include ${CLANGSA_FLAGS} ${CLANGSA_CXXFLAGS} ${CPPFLAGS} ${CFLAGS} -Xclang -analyzer-checker=core,julia.GCChecker --analyzer-no-default-checks -x c++ %s

#include "julia.h"
#include <string>

void missingPop() {
  jl_value_t *x = NULL;
  JL_GC_PUSH1(&x); // expected-note{{GC frame changed here}}
} // expected-warning@-1{{Non-popped GC frame present at end of function}}
  // expected-note@-2{{Non-popped GC frame present at end of function}}


void missingPop2() {
  jl_value_t **x;
  JL_GC_PUSHARGS(x, 2); // expected-note{{GC frame changed here}}
} // expected-warning@-1{{Non-popped GC frame present at end of function}}
  // expected-note@-2{{Non-popped GC frame present at end of function}}

void superfluousPop() {
  JL_GC_POP(); // expected-warning{{JL_GC_POP without corresponding push}}
}              // expected-note@-1{{JL_GC_POP without corresponding push}}

// From gc.c, jl_gc_push_arraylist creates a custom stack frame.
extern void jl_gc_push_arraylist(jl_ptls_t ptls, arraylist_t *list);
extern void run_finalizer(jl_ptls_t ptls, jl_value_t *o, jl_value_t *ff);
void jl_gc_run_finalizers_in_list(jl_ptls_t ptls, arraylist_t *list)
{
    size_t len = list->len;
    jl_value_t **items = (jl_value_t**)list->items;
    jl_gc_push_arraylist(ptls, list);
    (void)len; (void)items;
    //for (size_t i = 2;i < len;i += 2)
    //    run_finalizer(ptls, items[i], items[i + 1]);
    JL_GC_POP();
}

// Write barrier tests

// Store to GC-tracked field without a write barrier should warn.
void missing_write_barrier(jl_datatype_t *dt, jl_datatype_t *super) {
    dt->super = super; // expected-warning{{Store to GC-tracked field without a write barrier}}
                        // expected-note@-1{{Store to GC-tracked field without a write barrier}}
}

// Using jl_gc_write should not warn.
void correct_write_barrier_macro(jl_datatype_t *dt, jl_datatype_t *super) {
    jl_gc_write(dt, dt->super, super); // no warning
}

// Using jl_gc_wb_pre/post manually should not warn.
void correct_write_barrier_manual(jl_datatype_t *dt, jl_datatype_t *super) {
    jl_gc_wb_pre(dt, dt->super);
    dt->super = super; // no warning
    jl_gc_wb_post(dt, super);
}

// Using jl_gc_wb_fresh_pre for freshly allocated objects should not warn.
extern jl_datatype_t *allocate_datatype(void);
void fresh_allocation_wb(jl_datatype_t *super) {
    jl_datatype_t *dt = allocate_datatype();
    jl_gc_wb_fresh_pre(dt, super);
    dt->super = super; // no warning
    jl_gc_wb_fresh_post(dt, super);
}

// Freshly allocated objects should not need a write barrier.
void fresh_allocation_no_wb(jl_datatype_t *super) {
    jl_datatype_t *dt = allocate_datatype();
    dt->super = super; // no warning (freshly allocated)
}

// jl_gc_wb_fresh_post without matching jl_gc_wb_fresh_pre should warn.
void fresh_post_without_pre(jl_datatype_t *dt, jl_datatype_t *super) {
    dt->super = super;
    jl_gc_wb_fresh_post(dt, super); // expected-warning{{jl_gc_wb_fresh_post called without a matching jl_gc_wb_fresh_pre}}
                                     // expected-note@-1{{jl_gc_wb_fresh_post called without a matching jl_gc_wb_fresh_pre}}
}

bool testfunc1() JL_NOTSAFEPOINT
{
    struct implied_struct1 { // expected-note{{Tried to call method defined here}}
        std::string s;
        struct implied_constructor { } x;
    } x; // expected-warning{{Calling potential safepoint as CXXConstructorCall from function annotated JL_NOTSAFEPOINT}}
         // expected-note@-1{{Calling potential safepoint as CXXConstructorCall from function annotated JL_NOTSAFEPOINT}}
    return 1;
}
bool testfunc2() JL_NOTSAFEPOINT
{
    struct implied_struct2 { // expected-note{{Tried to call method defined here}}
        std::string s;
    } x{""};
    return 1; // expected-warning{{Calling potential safepoint as CXXDestructorCall from function annotated JL_NOTSAFEPOINT}}
              // expected-note@-1{{Calling potential safepoint as CXXDestructorCall from function annotated JL_NOTSAFEPOINT}}
}

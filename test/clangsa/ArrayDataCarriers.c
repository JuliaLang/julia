// This file is a part of Julia. License is MIT: https://julialang.org/license

// RUN: clang -D__clang_gcanalyzer__ --analyze -Xanalyzer -analyzer-output=text -Xclang -load -Xclang libGCCheckerPlugin%{shlibext} -I%{julia_home}/src -I%{julia_home}/src/support -I%{julia_home}/usr/include %{clangsa_flags} %{clangsa_cxxflags} %{cppflags} %{cflags} -Xclang -analyzer-checker=core,julia.GCChecker --analyzer-no-default-checks -Xclang -verify -x c %s

// Pointers into the data of an Array or GenericMemory are only valid while
// that object is kept alive.

#include "julia.h"
#include "julia_internal.h"

extern void look_at_value(jl_value_t *v);
extern void jl_gc_safepoint();
extern void fill_out(jl_value_t **out) JL_CANSAFEPOINT;

void array_ptr_data_root_dropped(jl_value_t *other) {
    jl_value_t *root = NULL;
    JL_GC_PUSH1(&root); // expected-note{{GC frame changed here}}
    root = (jl_value_t*)jl_alloc_vec_any(4); // expected-note{{Started tracking value here}}
                                             // expected-note@-1{{Value was rooted here}}
    jl_value_t **d = jl_array_ptr_data((jl_array_t*)root);
    root = other; // expected-note{{Root was released here}}
    jl_gc_safepoint(); // expected-note{{Value may have been GCed here}}
    look_at_value(d[0]); // expected-warning{{Creating derivative of value that may have been GCed}}
                         // expected-note@-1{{Creating derivative of value that may have been GCed}}
    JL_GC_POP();
}

void array_data_root_dropped(jl_value_t *other) {
    jl_value_t *root = NULL;
    JL_GC_PUSH1(&root); // expected-note{{GC frame changed here}}
    root = (jl_value_t*)jl_alloc_vec_any(4); // expected-note{{Started tracking value here}}
                                             // expected-note@-1{{Value was rooted here}}
    jl_value_t **d = jl_array_data((jl_array_t*)root, jl_value_t*);
    root = other; // expected-note{{Root was released here}}
    jl_gc_safepoint(); // expected-note{{Value may have been GCed here}}
    look_at_value(d[0]); // expected-warning{{Creating derivative of value that may have been GCed}}
                         // expected-note@-1{{Creating derivative of value that may have been GCed}}
    JL_GC_POP();
}

void memory_ptr_data_root_dropped(jl_value_t *other) {
    jl_value_t *root = NULL;
    JL_GC_PUSH1(&root); // expected-note{{GC frame changed here}}
    root = (jl_value_t*)jl_alloc_memory_any(4); // expected-note{{Started tracking value here}}
                                                // expected-note@-1{{Value was rooted here}}
    jl_value_t **d = jl_genericmemory_ptr_data((jl_genericmemory_t*)root);
    root = other; // expected-note{{Root was released here}}
    jl_gc_safepoint(); // expected-note{{Value may have been GCed here}}
    look_at_value(d[0]); // expected-warning{{Creating derivative of value that may have been GCed}}
                         // expected-note@-1{{Creating derivative of value that may have been GCed}}
    JL_GC_POP();
}

// The value comes from an opaque callee, so the analyzer did not see it being
// allocated; the frame slot it is stored into is still its only known root.
static jl_value_t *get_via_out_param(void) JL_CANSAFEPOINT {
    jl_value_t *out = NULL;
    JL_GC_PUSH1(&out); // expected-note{{GC frame changed here}}
    fill_out(&out);
    JL_GC_POP(); // expected-note{{GC frame changed here}}
    return out;
}

void out_param_root_dropped(jl_value_t *other) {
    jl_value_t *root = NULL;
    JL_GC_PUSH1(&root); // expected-note{{GC frame changed here}}
    root = get_via_out_param(); // expected-note{{Calling 'get_via_out_param'}}
                                // expected-note@-1{{Returning from 'get_via_out_param'}}
                                // expected-note@-2{{Started tracking value here}}
                                // expected-note@-3{{Value was rooted here}}
    jl_value_t **d = jl_array_ptr_data((jl_array_t*)root);
    root = other; // expected-note{{Root was released here}}
    jl_gc_safepoint(); // expected-note{{Value may have been GCed here}}
    look_at_value(d[0]); // expected-warning{{Creating derivative of value that may have been GCed}}
                         // expected-note@-1{{Creating derivative of value that may have been GCed}}
    JL_GC_POP();
}

// Keeping the owner rooted keeps the data pointers valid.
void array_ptr_data_root_kept(void) {
    jl_value_t *root = NULL;
    JL_GC_PUSH1(&root);
    root = (jl_value_t*)jl_alloc_vec_any(4);
    jl_value_t **d = jl_array_ptr_data((jl_array_t*)root);
    jl_gc_safepoint();
    look_at_value(d[0]);
    JL_GC_POP();
}

void array_data_root_kept(void) {
    jl_value_t *root = NULL;
    JL_GC_PUSH1(&root);
    root = (jl_value_t*)jl_alloc_vec_any(4);
    jl_value_t **d = jl_array_data((jl_array_t*)root, jl_value_t*);
    jl_gc_safepoint();
    look_at_value(d[0]);
    JL_GC_POP();
}

void memory_ptr_data_argument(jl_genericmemory_t *m) {
    jl_value_t **d = jl_genericmemory_ptr_data(m);
    jl_gc_safepoint();
    look_at_value(d[0]);
}

void out_param_root_kept(void) {
    jl_value_t *root = NULL;
    JL_GC_PUSH1(&root);
    root = get_via_out_param();
    jl_value_t **d = jl_array_ptr_data((jl_array_t*)root);
    jl_gc_safepoint();
    look_at_value(d[0]);
    JL_GC_POP();
}

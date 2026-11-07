// This file is a part of Julia. License is MIT: https://julialang.org/license

// RUN: clang -D__clang_gcanalyzer__ --analyze -Xanalyzer -analyzer-output=text -Xclang -load -Xclang libGCCheckerPlugin%{shlibext} -I%{julia_home}/src -I%{julia_home}/src/support -I%{julia_home}/usr/include %{clangsa_flags} %{clangsa_cxxflags} %{cppflags} %{cflags} -Xclang -analyzer-checker=core,julia.GCChecker --analyzer-no-default-checks -Xclang -verify -x c %s

// Only julia.h, not julia_internal.h, which defines some of the types julia.h
// merely declares: a type's annotation must be on a declaration that every
// includer of julia.h sees.
#include "julia.h"

extern jl_excstack_t *excstack_alloc(void);
extern void excstack_use(jl_excstack_t *s);

void excstack_is_tracked(void) {
    excstack_use(excstack_alloc()); // expected-warning{{Passing non-rooted value as argument to function that may GC}}
                                    // expected-note@-1{{Passing non-rooted value as argument to function}}
                                    // expected-note@-2{{Started tracking value here}}
}

// This file is a part of Julia. License is MIT: https://julialang.org/license

// RUN: clang -D__clang_gcanalyzer__ --analyze -Xanalyzer -analyzer-output=text -Xclang -load -Xclang libGCCheckerPlugin%{shlibext} -I%{julia_home}/src -I%{julia_home}/src/support -I%{julia_home}/usr/include %{clangsa_flags} %{clangsa_cxxflags} %{cppflags} %{cflags} -Xclang -analyzer-checker=core,julia.GCChecker --analyzer-no-default-checks -Xclang -verify -x c %s

#include "julia.h"
#include "julia_internal.h"

// Annotating the struct makes pointers to it tracked, including typedef aliases.
struct JL_GC_TRACKED_TYPE EmbedderBag;
typedef struct EmbedderBag *EmbedderValue;
typedef EmbedderValue EmbedderValueAlias;

// Unannotated structs are not tracked.
struct PlainBox;
typedef struct PlainBox *PlainValue;

extern EmbedderValue embedder_alloc(void);
extern void embedder_use(EmbedderValue v);
extern PlainValue plain_alloc(void);
extern void plain_use(PlainValue v);

void embedder_unrooted_argument(void) {
    embedder_use(embedder_alloc()); // expected-warning{{Passing non-rooted value as argument to function that may GC}}
                                    // expected-note@-1{{Passing non-rooted value as argument to function}}
                                    // expected-note@-2{{Started tracking value here}}
}

void embedder_rooted_argument(void) {
    EmbedderValue v = embedder_alloc();
    JL_GC_PUSH1(&v);
    embedder_use(v);
    JL_GC_POP();
}

// Both aliases refer to the annotated struct.
void embedder_alias_is_tracked(void) {
    EmbedderValueAlias v = embedder_alloc();
    embedder_use(v); // expected-warning{{Passing non-rooted value as argument to function that may GC}}
                     // expected-note@-1{{Passing non-rooted value as argument to function}}
                     // expected-note@-3{{Started tracking value here}}
}

// An annotation on a typedef of void also applies through further aliases.
typedef void EmbedderBuffer JL_GC_TRACKED_TYPE;
typedef EmbedderBuffer EmbedderBufferAlias;
extern EmbedderBufferAlias *buffer_alloc(void);
extern void buffer_use(EmbedderBufferAlias *b);

void embedder_typedef_alias_is_tracked(void) {
    buffer_use(buffer_alloc()); // expected-warning{{Passing non-rooted value as argument to function that may GC}}
                                // expected-note@-1{{Passing non-rooted value as argument to function}}
                                // expected-note@-2{{Started tracking value here}}
}

void unannotated_type_is_not_tracked(void) {
    plain_use(plain_alloc()); // no-warning
}

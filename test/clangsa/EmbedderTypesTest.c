// This file is a part of Julia. License is MIT: https://julialang.org/license

// RUN: clang -D__clang_gcanalyzer__ --analyze -Xanalyzer -analyzer-output=text -Xclang -load -Xclang libGCCheckerPlugin%shlibext -I%julia_home/src -I%julia_home/src/support -I%julia_home/usr/include ${CLANGSA_FLAGS} ${CLANGSA_CXXFLAGS} ${CPPFLAGS} ${CFLAGS} -Xclang -analyzer-checker=core,julia.GCChecker --analyzer-no-default-checks -Xclang -verify -x c %s

#include "julia.h"
#include "julia_internal.h"

// An embedder's object type. Julia's own types are recognised by name; this
// one opts in with an attribute instead. It sits on the tag, so both the tag
// and every typedef of it are covered.
struct JL_GC_TRACKED_TYPE EmbedderBag;
typedef struct EmbedderBag *EmbedderValue;
typedef EmbedderValue EmbedderValueAlias;

// Deliberately not annotated, to pin down that the attribute is what does the
// work and an arbitrary embedder struct is still ignored.
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

// The annotation is found through a typedef of a typedef, since it lives on
// the tag both of them resolve to.
void embedder_alias_is_tracked(void) {
    EmbedderValueAlias v = embedder_alloc();
    embedder_use(v); // expected-warning{{Passing non-rooted value as argument to function that may GC}}
                     // expected-note@-1{{Passing non-rooted value as argument to function}}
                     // expected-note@-3{{Started tracking value here}}
}

void unannotated_type_is_not_tracked(void) {
    plain_use(plain_alloc()); // no-warning
}

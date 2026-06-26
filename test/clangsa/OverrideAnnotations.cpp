// This file is a part of Julia. License is MIT: https://julialang.org/license

// RUN: clang -D__clang_gcanalyzer__ --analyze -Xanalyzer -analyzer-output=text -Xclang -load -Xclang libGCCheckerPlugin%shlibext -Xclang -verify -I%julia_home/src -I%julia_home/src/support -I%julia_home/usr/include ${CLANGSA_FLAGS} ${CLANGSA_CXXFLAGS} ${CPPFLAGS} ${CFLAGS} -Xclang -analyzer-checker=core,julia.GCChecker --analyzer-no-default-checks -x c++ %s

// Julia annotations on a C++ virtual method are not copied by Clang onto a
// derived-class override (an override is a distinct declaration, not a
// redeclaration). declHasAnnotation therefore walks the transitively
// overridden methods so that an override inherits the contract declared on the
// method(s) it overrides.

#include "julia.h"

struct Base {
    // Annotated on the base only; overrides below omit the annotation.
    virtual void notSafepoint() JL_NOTSAFEPOINT;
    // No annotation: a genuine potential safepoint.
    virtual void isSafepoint();
};

struct Derived : Base {
    void notSafepoint() override;
    void isSafepoint() override; // expected-note{{Tried to call method defined here}}
};

// A two-level chain: only the top of the hierarchy carries the annotation, so
// reaching it from the leaf override requires a transitive walk.
struct GrandChild : Derived {
    void notSafepoint() override;
};

// The override inherits JL_NOTSAFEPOINT from Base::notSafepoint, so calling it
// from a JL_NOTSAFEPOINT function must not warn.
void callInheritedNotSafepoint(Derived *d) JL_NOTSAFEPOINT {
    d->notSafepoint();
}

// Transitive inheritance through two levels of override.
void callInheritedNotSafepointDeep(GrandChild *g) JL_NOTSAFEPOINT {
    g->notSafepoint();
}

// Control: the override carries no annotation and neither does anything it
// overrides, so this is a real safepoint and must warn.
void callSafepoint(Derived *d) JL_NOTSAFEPOINT {
    d->isSafepoint(); // expected-warning{{Calling potential safepoint as CXXMemberCall from function annotated JL_NOTSAFEPOINT}}
                      // expected-note@-1{{Calling potential safepoint as CXXMemberCall from function annotated JL_NOTSAFEPOINT}}
}

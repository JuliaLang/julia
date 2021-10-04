// This file is a part of Julia. License is MIT: https://julialang.org/license

// RUN: clang --analyze -Xanalyzer -analyzer-output=text -Xclang -load -Xclang libImplicitAtomicsPlugin%shlibext -Xclang -verify -I%julia_home/src -I%julia_home/src/support -I%julia_home/usr/include ${CLANGSA_FLAGS} ${CPPFLAGS} ${CFLAGS} ${CXXFLAGS} -Xclang -analyzer-checker=core,julia.ImplicitAtomics --analyzer-no-default-checks -x c++ -std=c++11 %s -v
// RUN: clang --analyze -Xanalyzer -analyzer-output=text -Xclang -load -Xclang libImplicitAtomicsPlugin%shlibext -Xclang -verify -I%julia_home/src -I%julia_home/src/support -I%julia_home/usr/include ${CLANGSA_FLAGS} ${CPPFLAGS} ${CFLAGS} -Xclang -analyzer-checker=core,julia.ImplicitAtomics --analyzer-no-default-checks -x c -std=c11 %s -v

#include "julia_atomics.h"

_Atomic(int) x, *px;
struct Atomic_xy_t {
    _Atomic(int) x;
    _Atomic(int) *px;
    int y;
} y, *py;
_Atomic(int) z[2];


// jwn: add tests for casts, and *py = y;

void hiddenAtomics(void) {
    px = &x;
    py = &y;
    y.px = &y.x;
    ++x; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    --x; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    x++; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    x--; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    x += 2; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    x -= 2; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
#ifndef __cplusplus // invalid C++ code
    x *= 2; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    x = // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
        x; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
#endif
    x = 2; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    x + 2; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}

    ++*px; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    --*px; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    px++;
    px--;
    1 + *px++; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    1 + *px--; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    (*px)++; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    (*px)--; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    *px += 2; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    *px -= 2; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
#ifndef __cplusplus // invalid C++ code
    *px *= 2; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    *px = // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
        x; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    x = // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
        *px; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
#endif
    *px = 2; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    *px + 2; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}

    *(int*)&x = 3;
    *(int*)px = 3;

    y.y = 2;
    py->y = 2;
#ifndef __cplusplus // invalid C++ code
    *py = // TODO
        y; // TODO
    y = // TODO
       *py; // TODO
#endif
    *(_Atomic(int)*)&y.y = 2; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    *(_Atomic(int)*)&py->y = 2; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}

    y.x = 1; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    *y.px = 1; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}

    py->x = 1; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    *py->px = 1; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}

    z[1] = 1; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    *z = 1; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    *z += 1; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}

#ifdef __cplusplus // check initialization / finalization
    _Atomic(int) lx{2};
    lx = 3; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    lx += 1; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}

    struct large_type { int x[16]; };
    auto *ly = new std::atomic<struct large_type>();
    *ly =    // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
        ly->load();
    struct large_type a = *ly; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    delete ly;

#if 0 // enable for C++2a
    std::atomic_ref<int> lz(*(int*)px);
    lz = 3; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    lz += 1; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
#endif
#endif
}

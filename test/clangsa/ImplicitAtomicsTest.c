// This file is a part of Julia. License is MIT: https://julialang.org/license

// RUN-TODO: clang-tidy %s --checks=-*,concurrency-implicit-atomics -load libImplicitAtomics2Plugin%shlibext -- -I%julia_home/src -I%julia_home/src/support -I%julia_home/usr/include ${CLANGSA_FLAGS} ${CPPFLAGS} ${CFLAGS} -x c -std=c11 | FileCheck --check-prefixes=CHECK,CHECK-C %s
// RUN-TODO: clang-tidy %s --checks=-*,concurrency-implicit-atomics -load libImplicitAtomics2Plugin%shlibext -- -I%julia_home/src -I%julia_home/src/support -I%julia_home/usr/include ${CLANGSA_FLAGS} ${CPPFLAGS} ${CFLAGS} ${CXXFLAGS} -x c++ -std=c++11 | FileCheck --check-prefixes=CHECK,CHECK-CXX %s
// RUN: clang --analyze -Xanalyzer -analyzer-output=text -Xclang -load -Xclang libImplicitAtomicsPlugin%shlibext -Xclang -verify -I%julia_home/src -I%julia_home/src/support -I%julia_home/usr/include ${CLANGSA_FLAGS} ${CPPFLAGS} ${CFLAGS} -Xclang -analyzer-checker=core,julia.ImplicitAtomics --analyzer-no-default-checks -x c -std=c11 %s -v
// RUN: clang --analyze -Xanalyzer -analyzer-output=text -Xclang -load -Xclang libImplicitAtomicsPlugin%shlibext -Xclang -verify -I%julia_home/src -I%julia_home/src/support -I%julia_home/usr/include ${CLANGSA_FLAGS} ${CPPFLAGS} ${CFLAGS} ${CXXFLAGS} -Xclang -analyzer-checker=core,julia.ImplicitAtomics --analyzer-no-default-checks -x c++ -std=c++11 %s -v

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
    // CHECK-NOT: [[@LINE+1]]
    px = &x;
    // CHECK-NOT: [[@LINE+1]]
    py = &y;
    // CHECK-NOT: [[@LINE+1]]
    y.px = &y.x;
    // CHECK: [[@LINE+1]]:7: warning: Implicit Atomic seq_cst synchronization
    ++x; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    // CHECK: [[@LINE+1]]:7: warning: Implicit Atomic seq_cst synchronization
    --x; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    // CHECK: [[@LINE+1]]:5: warning: Implicit Atomic seq_cst synchronization
    x++; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    // CHECK: [[@LINE+1]]:5: warning: Implicit Atomic seq_cst synchronization
    x--; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    // CHECK: [[@LINE+1]]:5: warning: Implicit Atomic seq_cst synchronization
    x += 2; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    // CHECK: [[@LINE+1]]:5: warning: Implicit Atomic seq_cst synchronization
    x -= 2; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
#ifndef __cplusplus // invalid C++ code
    // CHECK-CXX-NOT: [[@LINE+2]]:5:
    // CHECK-C: [[@LINE+1]]:5: warning: Implicit Atomic seq_cst synchronization
    x *= 2; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    // CHECK-C: [[@LINE+1]]:5: warning: Implicit Atomic seq_cst synchronization
    x = // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    // CHECK-C: [[@LINE+1]]:9: warning: Implicit Atomic seq_cst synchronization
        x; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
#endif
    // CHECK: [[@LINE+1]]:5: warning: Implicit Atomic seq_cst synchronization
    x = 2; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    // CHECK: [[@LINE+1]]:5: warning: Implicit Atomic seq_cst synchronization
    x + 2; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}

    // CHECK: [[@LINE+1]]:8: warning: Implicit Atomic seq_cst synchronization
    ++*px; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    // CHECK: [[@LINE+1]]:8: warning: Implicit Atomic seq_cst synchronization
    --*px; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    // CHECK-NOT: [[@LINE+1]]
    px++;
    // CHECK-NOT: [[@LINE+1]]
    px--;
    // CHECK: [[@LINE+1]]:10: warning: Implicit Atomic seq_cst synchronization
    1 + *px++; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    // CHECK: [[@LINE+1]]:10: warning: Implicit Atomic seq_cst synchronization
    1 + *px--; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    // CHECK: [[@LINE+1]]:7: warning: Implicit Atomic seq_cst synchronization
    (*px)++; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    // CHECK: [[@LINE+1]]:7: warning: Implicit Atomic seq_cst synchronization
    (*px)--; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    // CHECK: [[@LINE+1]]:6: warning: Implicit Atomic seq_cst synchronization
    *px += 2; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    // CHECK: [[@LINE+1]]:6: warning: Implicit Atomic seq_cst synchronization
    *px -= 2; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
#ifndef __cplusplus // invalid C++ code
    // CHECK-CXX-NOT: [[@LINE+2]]
    // CHECK-C: [[@LINE+1]]:6: warning: Implicit Atomic seq_cst synchronization
    *px *= 2; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    // CHECK-C: [[@LINE+1]]:6: warning: Implicit Atomic seq_cst synchronization
    *px = // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    // CHECK-C: [[@LINE+1]]:9: warning: Implicit Atomic seq_cst synchronization
        x; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    // CHECK-C: [[@LINE+1]]:5: warning: Implicit Atomic seq_cst synchronization
    x = // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    // CHECK-C: [[@LINE+1]]:10: warning: Implicit Atomic seq_cst synchronization
        *px; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
#endif
    // CHECK: [[@LINE+1]]:6: warning: Implicit Atomic seq_cst synchronization
    *px = 2; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    // CHECK: [[@LINE+1]]:6: warning: Implicit Atomic seq_cst synchronization
    *px + 2; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}

    // CHECK-NOT: [[@LINE+1]]
    *(int*)&x = 3;
    // CHECK-NOT: [[@LINE+1]]
    *(int*)px = 3;

    // CHECK-NOT: [[@LINE+1]]
    y.y = 2;
    // CHECK-NOT: [[@LINE+1]]
    py->y = 2;
#ifndef __cplusplus // invalid C++ code
    // CHECK-CXX-NOT: [[@LINE+1]]
    *py = // TODO
        y; // TODO
    y = // TODO
       *py; // TODO
#endif
    // CHECK: [[@LINE+1]]:22: warning: Implicit Atomic seq_cst synchronization
    *(_Atomic(int)*)&y.y = 2; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    // CHECK: [[@LINE+1]]:22: warning: Implicit Atomic seq_cst synchronization
    *(_Atomic(int)*)&py->y = 2; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}

    // CHECK: [[@LINE+1]]:5: warning: Implicit Atomic seq_cst synchronization
    y.x = 1; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    // CHECK: [[@LINE+1]]:6: warning: Implicit Atomic seq_cst synchronization
    *y.px = 1; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}

#ifndef __cplusplus // invalid C++ code
    // CHECK-CXX-NOT: [[@LINE+2]]
    // CHECK-C: [[@LINE+1]]:5: warning: Implicit Atomic seq_cst synchronization
    x = // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    // CHECK-C: [[@LINE+1]]:13: warning: Implicit Atomic seq_cst synchronization
        py->x; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    // CHECK-C: [[@LINE+1]]:5: warning: Implicit Atomic seq_cst synchronization
    x = // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    // CHECK-C: [[@LINE+1]]:10: warning: Implicit Atomic seq_cst synchronization
        *py->px; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
#endif
    // CHECK: [[@LINE+1]]:5: warning: Implicit Atomic seq_cst synchronization
    py->x = 1; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    // CHECK: [[@LINE+1]]:6: warning: Implicit Atomic seq_cst synchronization
    *py->px = 1; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}

    // CHECK: [[@LINE+1]]:5: warning: Implicit Atomic seq_cst synchronization
    z[1] = 1; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    // CHECK: [[@LINE+1]]:6: warning: Implicit Atomic seq_cst synchronization
    *z = 1; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    // CHECK: [[@LINE+1]]:6: warning: Implicit Atomic seq_cst synchronization
    *z += 1; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}

#ifdef __cplusplus // check initialization / finalization
    // CHECK-NOT: [[@LINE+1]]
    _Atomic(int) lx{2};
    // CHECK-CXX: [[@LINE+1]]:5: warning: Implicit Atomic seq_cst synchronization
    lx = 3; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    // CHECK-CXX: [[@LINE+1]]:5: warning: Implicit Atomic seq_cst synchronization
    lx += 1; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}

    // CHECK-NOT: [[@LINE+1]]
    struct large_type { int x[16]; };
    // CHECK-NOT: [[@LINE+1]]
    auto *ly = new std::atomic<struct large_type>();
    // CHECK-CXX: [[@LINE+1]]:6: warning: Implicit Atomic seq_cst synchronization
    *ly =    // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    // CHECK-NOT: [[@LINE+1]]
        ly->load();
    // CHECK-CXX: [[@LINE+1]]:28: warning: Implicit Atomic seq_cst synchronization
    struct large_type a = *ly; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    // CHECK-NOT: [[@LINE+1]]
    delete ly;

#if 0 // enable for C++2a
    std::atomic_ref<int> lz(*(int*)px);
    lz = 3; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
    lz += 1; // expected-warning{{Implicit Atomic seq_cst synchronization}} expected-note{{Implicit Atomic seq_cst synchronization}}
#endif
#endif
}

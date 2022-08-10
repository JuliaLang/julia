// This file is a part of Julia. License is MIT: https://julialang.org/license

// RUN: clang-tidy %s --checks=-*,concurrency-implicit-atomics -load libImplicitAtomicsPlugin%shlibext -- -I%julia_home/src -I%julia_home/src/support -I%julia_home/usr/include ${CLANGSA_FLAGS} ${CPPFLAGS} ${CFLAGS} -x c -std=c11 | FileCheck --check-prefixes=CHECK,CHECK-C %s
// RUN: clang-tidy %s --checks=-*,concurrency-implicit-atomics -load libImplicitAtomicsPlugin%shlibext -- -I%julia_home/src -I%julia_home/src/support -I%julia_home/usr/include ${CLANGSA_FLAGS} ${CPPFLAGS} ${CFLAGS} ${CXXFLAGS} -x c++ -std=c++11 | FileCheck --check-prefixes=CHECK,CHECK-CXX %s

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
    px = &x; // CHECK-NOT: [[@LINE]]
    py = &y; // CHECK-NOT: [[@LINE]]
    y.px = &y.x; // CHECK-NOT: [[@LINE]]
    ++x; // CHECK: [[@LINE]]:7: warning: Implicit Atomic seq_cst synchronization
    --x; // CHECK: [[@LINE]]:7: warning: Implicit Atomic seq_cst synchronization
    x++; // CHECK: [[@LINE]]:5: warning: Implicit Atomic seq_cst synchronization
    x--; // CHECK: [[@LINE]]:5: warning: Implicit Atomic seq_cst synchronization
    x += 2; // CHECK: [[@LINE]]:5: warning: Implicit Atomic seq_cst synchronization
    x -= 2; // CHECK: [[@LINE]]:5: warning: Implicit Atomic seq_cst synchronization
#ifndef __cplusplus // invalid C++ code
    // CHECK-CXX-NOT: [[@LINE+1]]
    x *= 2; // CHECK-C: [[@LINE]]:5: warning: Implicit Atomic seq_cst synchronization
    x = // CHECK-C: [[@LINE]]:5: warning: Implicit Atomic seq_cst synchronization
        x; // CHECK-C: [[@LINE]]:9: warning: Implicit Atomic seq_cst synchronization
#endif
    x = 2; // CHECK: [[@LINE]]:5: warning: Implicit Atomic seq_cst synchronization
    x + 2; // CHECK: [[@LINE]]:5: warning: Implicit Atomic seq_cst synchronization

    ++*px; // CHECK: [[@LINE]]:8: warning: Implicit Atomic seq_cst synchronization
    --*px; // CHECK: [[@LINE]]:8: warning: Implicit Atomic seq_cst synchronization
    px++; // CHECK-NOT: [[@LINE]]
    px--; // CHECK-NOT: [[@LINE]]
    1 + *px++; // CHECK: [[@LINE]]:10: warning: Implicit Atomic seq_cst synchronization
    1 + *px--; // CHECK: [[@LINE]]:10: warning: Implicit Atomic seq_cst synchronization
    (*px)++; // CHECK: [[@LINE]]:7: warning: Implicit Atomic seq_cst synchronization
    (*px)--; // CHECK: [[@LINE]]:7: warning: Implicit Atomic seq_cst synchronization
    *px += 2; // CHECK: [[@LINE]]:6: warning: Implicit Atomic seq_cst synchronization
    *px -= 2; // CHECK: [[@LINE]]:6: warning: Implicit Atomic seq_cst synchronization
#ifndef __cplusplus // invalid C++ code
    // CHECK-CXX-NOT: [[@LINE+1]]
    *px *= 2; // CHECK-C: [[@LINE]]:6: warning: Implicit Atomic seq_cst synchronization
    *px = // CHECK-C: [[@LINE]]:6: warning: Implicit Atomic seq_cst synchronization
        x; // CHECK-C: [[@LINE]]:9: warning: Implicit Atomic seq_cst synchronization
    x = // CHECK-C: [[@LINE]]:5: warning: Implicit Atomic seq_cst synchronization
        *px; // CHECK-C: [[@LINE]]:10: warning: Implicit Atomic seq_cst synchronization
#endif
    *px = 2; // CHECK: [[@LINE]]:6: warning: Implicit Atomic seq_cst synchronization
    *px + 2; // CHECK: [[@LINE]]:6: warning: Implicit Atomic seq_cst synchronization

    *(int*)&x = 3; // CHECK-NOT: [[@LINE]]
    *(int*)px = 3; // CHECK-NOT: [[@LINE]]

    y.y = 2; // CHECK-NOT: [[@LINE]]
    py->y = 2; // CHECK-NOT: [[@LINE]]
#ifndef __cplusplus // invalid C++ code
    // CHECK-CXX-NOT: [[@LINE+1]]
    *py = // TODO
        y; // TODO
    y = // TODO
       *py; // TODO
#endif
    *(_Atomic(int)*)&y.y = 2; // CHECK: [[@LINE]]:22: warning: Implicit Atomic seq_cst synchronization
    *(_Atomic(int)*)&py->y = 2; // CHECK: [[@LINE]]:22: warning: Implicit Atomic seq_cst synchronization

    y.x = 1; // CHECK: [[@LINE]]:5: warning: Implicit Atomic seq_cst synchronization
    *y.px = 1; // CHECK: [[@LINE]]:6: warning: Implicit Atomic seq_cst synchronization

#ifndef __cplusplus // invalid C++ code
    // CHECK-CXX-NOT: [[@LINE+1]]
    x = // CHECK-C: [[@LINE]]:5: warning: Implicit Atomic seq_cst synchronization
        py->x; // CHECK-C: [[@LINE]]:9: warning: Implicit Atomic seq_cst synchronization
    x = // CHECK-C: [[@LINE]]:5: warning: Implicit Atomic seq_cst synchronization
        *py->px; // CHECK-C: [[@LINE]]:10: warning: Implicit Atomic seq_cst synchronization
#endif
    py->x = 1; // CHECK: [[@LINE]]:5: warning: Implicit Atomic seq_cst synchronization
    *py->px = 1; // CHECK: [[@LINE]]:6: warning: Implicit Atomic seq_cst synchronization

    z[1] = 1; // CHECK: [[@LINE]]:5: warning: Implicit Atomic seq_cst synchronization
    *z = 1; // CHECK: [[@LINE]]:6: warning: Implicit Atomic seq_cst synchronization
    *z += 1; // CHECK: [[@LINE]]:6: warning: Implicit Atomic seq_cst synchronization

#ifdef __cplusplus // check initialization / finalization
    // CHECK-NOT: [[@LINE+1]]
    _Atomic(int) lx{2};
    lx = 3; // CHECK-CXX: [[@LINE]]:5: warning: Implicit Atomic seq_cst synchronization
    lx += 1; // CHECK-CXX: [[@LINE]]:5: warning: Implicit Atomic seq_cst synchronization

    // CHECK-NOT: [[@LINE+1]]
    struct large_type { int x[16]; };
    // CHECK-NOT: [[@LINE+1]]
    auto *ly = new std::atomic<struct large_type>();
    *ly = // CHECK-CXX: [[@LINE]]:6: warning: Implicit Atomic seq_cst synchronization
        ly->load(); // CHECK-NOT: [[@LINE]]
    struct large_type a = *ly; // CHECK-CXX: [[@LINE]]:28: warning: Implicit Atomic seq_cst synchronization
    delete ly; // CHECK-NOT: [[@LINE]]

#if 0 // enable for C++2a
    std::atomic_ref<int> lz(*(int*)px);
    lz = 3;
    lz += 1;
#endif
#endif
}

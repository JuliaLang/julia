// This file is a part of Julia. License is MIT: https://julialang.org/license

// RUN: clang-tidy %s --checks=-*,julia-static-or-declared -load libStaticOrDeclaredPlugin%shlibext -- -I%julia_home/src -I%julia_home/src/support -I%julia_home/usr/include ${CLANGSA_FLAGS} ${CLANGSA_CXXFLAGS} ${CPPFLAGS} ${CFLAGS} -x c -std=c11 | FileCheck --check-prefixes=CHECK %s
// RUN: clang-tidy %s --checks=-*,julia-static-or-declared -load libStaticOrDeclaredPlugin%shlibext -- -I%julia_home/src -I%julia_home/src/support -I%julia_home/usr/include ${CLANGSA_FLAGS} ${CLANGSA_CXXFLAGS} ${CPPFLAGS} ${CFLAGS} ${CXXFLAGS} -x c++ -std=c++11 | FileCheck --check-prefixes=CHECK,CHECK-CXX %s
// RUN: clang-tidy %s --checks=-*,julia-static-or-declared -load libStaticOrDeclaredPlugin%shlibext -- --target=x86_64-w64-windows-gnu -I%julia_home/src -I%julia_home/src/support -x c -std=c11 | FileCheck --check-prefixes=CHECK %s
//
// Each flagged C++ type gets a fix-it that wraps it in an anonymous namespace.
// The fixes are checked from the exported-fixes YAML (the source itself already
// contains `namespace {` blocks, which would defeat a fixed-source CHECK).
// RUN: clang-tidy %s --quiet --checks=-*,julia-static-or-declared -load libStaticOrDeclaredPlugin%shlibext --export-fixes=%t.yaml -- -I%julia_home/src -I%julia_home/src/support -I%julia_home/usr/include ${CLANGSA_FLAGS} ${CLANGSA_CXXFLAGS} ${CPPFLAGS} ${CFLAGS} ${CXXFLAGS} -x c++ -std=c++11
// RUN: FileCheck --check-prefix=FIXYAML %s < %t.yaml

// FIXYAML: Message:{{.*}}sod_vis_hidden
// FIXYAML: ReplacementText: 'static '
// FIXYAML: Message:{{.*}}sod_fwd_local
// FIXYAML: ReplacementText: 'static '
// FIXYAML: ReplacementText: 'static '
// FIXYAML: Message:{{.*}}sod_orphan
// FIXYAML: ReplacementText: 'static '
// FIXYAML: Message:{{.*}}sod_extern_c
// FIXYAML-NOT: ReplacementText
// FIXYAML: Message:{{.*}}sod_LocalType
// FIXYAML: ReplacementText: "namespace {\n"
// FIXYAML: ReplacementText: "\n}  // anonymous namespace"
// FIXYAML: Message:{{.*}}sod_LocalUnion
// FIXYAML: ReplacementText: "namespace {\n"
// FIXYAML: ReplacementText: "\n}  // anonymous namespace"
// FIXYAML: Message:{{.*}}sod_Outer
// FIXYAML: ReplacementText: "namespace {\n"
// FIXYAML: ReplacementText: "\n}  // anonymous namespace"
// FIXYAML: Message:{{.*}}sod_FwdDeclared
// FIXYAML: ReplacementText: "namespace {\n"
// FIXYAML: ReplacementText: "\n}  // anonymous namespace"
// FIXYAML: ReplacementText: "namespace {\n"
// FIXYAML: ReplacementText: "\n}  // anonymous namespace"
// FIXYAML-NOT: ReplacementText: "namespace {\n"

#include "StaticOrDeclaredTest.h"

void sod_public(void) {} // CHECK-NOT: :[[@LINE]]:{{[0-9]+}}: warning
int sod_public_val(int x) { return x; } // CHECK-NOT: :[[@LINE]]:{{[0-9]+}}: warning
static int sod_static(int x) { return x + 1; } // CHECK-NOT: :[[@LINE]]:{{[0-9]+}}: warning
__attribute__((visibility("default"))) void sod_vis_default(void) {} // CHECK-NOT: :[[@LINE]]:{{[0-9]+}}: warning
__attribute__((visibility("hidden"))) void sod_vis_hidden(void) {} // CHECK: [[@LINE]]:44: warning: function 'sod_vis_hidden' has external linkage but is not declared in any header; declare it in a header or make it 'static'
extern void sod_extern_marked(void) {} // CHECK-NOT: :[[@LINE]]:{{[0-9]+}}: warning

#ifdef _WIN32
__declspec(dllexport) void sod_dllexport(void) {} // CHECK-NOT: :[[@LINE]]:{{[0-9]+}}: warning
#endif

void sod_fwd_local(void);
void sod_fwd_local(void) {} // CHECK: [[@LINE]]:6: warning: function 'sod_fwd_local' has external linkage but is not declared in any header; declare it in a header or make it 'static'

int sod_orphan(int x) { return sod_static(x); } // CHECK: [[@LINE]]:5: warning: function 'sod_orphan' has external linkage but is not declared in any header; declare it in a header or make it 'static'

void sod_redundant_hdr(void); // CHECK: [[@LINE]]:6: warning: function 'sod_redundant_hdr' is forward-declared in a source file rather than a header; move the declaration to a header or mark it 'extern'
void sod_fwd_undef(void); // CHECK: [[@LINE]]:6: warning: function 'sod_fwd_undef' is forward-declared in a source file rather than a header; move the declaration to a header or mark it 'extern'
extern void sod_fwd_extern(void); // CHECK-NOT: :[[@LINE]]:{{[0-9]+}}: warning

__attribute__((visibility("default"))) void sod_fwd_exported(void); // CHECK-NOT: :[[@LINE]]:{{[0-9]+}}: warning
__attribute__((visibility("default"))) void sod_fwd_exported_undef(void); // CHECK: [[@LINE]]:45: warning: function 'sod_fwd_exported_undef' is forward-declared in a source file rather than a header; move the declaration to a header or mark it 'extern'

#ifdef __cplusplus
void sod_HdrType::method() {} // CHECK-CXX-NOT: :[[@LINE]]:{{[0-9]+}}: warning
namespace {
void sod_anon(void) {} // CHECK-CXX-NOT: :[[@LINE]]:{{[0-9]+}}: warning
}

template <typename T> T sod_tmpl(T x) { return x; } // CHECK-CXX-NOT: :[[@LINE]]:{{[0-9]+}}: warning
template int sod_tmpl<int>(int); // CHECK-CXX-NOT: :[[@LINE]]:{{[0-9]+}}: warning
extern "C" void sod_extern_c(void) {} // CHECK-CXX: [[@LINE]]:17: warning: function 'sod_extern_c' has external linkage but is not declared in any header; declare it in a header or make it 'static'
struct sod_FwdDeclared; // CHECK-CXX-NOT: :[[@LINE]]:{{[0-9]+}}: warning
class sod_Opaque { int z; }; // CHECK-CXX-NOT: :[[@LINE]]:{{[0-9]+}}: warning
struct sod_LocalType { int y; }; // CHECK-CXX: [[@LINE]]:8: warning: type 'sod_LocalType' has external linkage but is not declared in any header; declare it in a header or move it into an anonymous namespace
union sod_LocalUnion { int u; float f; }; // CHECK-CXX: [[@LINE]]:7: warning: type 'sod_LocalUnion' has external linkage but is not declared in any header; declare it in a header or move it into an anonymous namespace
namespace {
struct sod_AnonType { int a; }; // CHECK-CXX-NOT: :[[@LINE]]:{{[0-9]+}}: warning
}

struct __attribute__((visibility("default"))) sod_ExportedType { int e; }; // CHECK-CXX-NOT: :[[@LINE]]:{{[0-9]+}}: warning

struct sod_Outer { // CHECK-CXX: [[@LINE]]:8: warning: type 'sod_Outer' has external linkage but is not declared in any header; declare it in a header or move it into an anonymous namespace
    struct sod_Inner { int i; }; // CHECK-CXX-NOT: :[[@LINE]]:{{[0-9]+}}: warning
};

static void sod_uses_local() {
    struct sod_FnLocal { int l; }; // CHECK-CXX-NOT: :[[@LINE]]:{{[0-9]+}}: warning
    (void)sizeof(sod_FnLocal);
}

template <typename T> struct sod_TmplType { T t; }; // CHECK-CXX-NOT: :[[@LINE]]:{{[0-9]+}}: warning
template struct sod_TmplType<int>; // CHECK-CXX-NOT: :[[@LINE]]:{{[0-9]+}}: warning

struct sod_FwdDeclared { int f; }; // CHECK-CXX: [[@LINE]]:8: warning: type 'sod_FwdDeclared' has external linkage but is not declared in any header; declare it in a header or move it into an anonymous namespace
#endif

int main(void) { // CHECK-NOT: :[[@LINE]]:{{[0-9]+}}: warning
    return sod_orphan(0) + sod_public_val(0);
}

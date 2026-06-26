// This file is a part of Julia. License is MIT: https://julialang.org/license

// RUN: clang-tidy %s --checks=-*,julia-first-decl-annotations -header-filter='.*' -load libFirstDeclAnnotationsPlugin%shlibext -- -D__clang_gcanalyzer__ -I%julia_home/src -I%julia_home/src/support -I%julia_home/usr/include ${CLANGSA_FLAGS} ${CLANGSA_CXXFLAGS} ${CPPFLAGS} ${CFLAGS} -x c -std=c11 | FileCheck --check-prefixes=CHECK --implicit-check-not=warning: %s
// RUN: clang-tidy %s --checks=-*,julia-first-decl-annotations -header-filter='.*' -load libFirstDeclAnnotationsPlugin%shlibext -- -D__clang_gcanalyzer__ -I%julia_home/src -I%julia_home/src/support -I%julia_home/usr/include ${CLANGSA_FLAGS} ${CLANGSA_CXXFLAGS} ${CPPFLAGS} ${CFLAGS} ${CXXFLAGS} -x c++ -std=c++11 | FileCheck --check-prefixes=CHECK,CHECK-CXX --implicit-check-not=warning: %s

// Each diagnostic carries a fix-it that moves the annotation to the first
// declaration. Copy the test and its header into a temp directory, apply the
// fixes with --fix, and check both files: the definitions in the .c lose their
// annotations (CHECK-FIXES) and the header gains them (CHECK-FIXES-H). The
// patterns are anchored with {{^}}/{{$}} so they match the rewritten source
// lines and never the CHECK comment lines that contain the same text.
// RUN: rm -rf %t && mkdir -p %t
// RUN: cp %s %t/FirstDeclAnnotationsTest.c
// RUN: cp %S/FirstDeclAnnotationsTest.h %t/FirstDeclAnnotationsTest.h
// RUN: clang-tidy %t/FirstDeclAnnotationsTest.c --checks=-*,julia-first-decl-annotations -header-filter='.*' --fix -load libFirstDeclAnnotationsPlugin%shlibext -- -D__clang_gcanalyzer__ -I%t -I%julia_home/src -I%julia_home/src/support -I%julia_home/usr/include ${CLANGSA_FLAGS} ${CLANGSA_CXXFLAGS} ${CPPFLAGS} ${CFLAGS} -x c -std=c11
// RUN: FileCheck --check-prefix=CHECK-FIXES --input-file=%t/FirstDeclAnnotationsTest.c %s
// RUN: FileCheck --check-prefix=CHECK-FIXES-H --input-file=%t/FirstDeclAnnotationsTest.h %s

#include "FirstDeclAnnotationsTest.h"

void fda_ok_both(void) JL_NOTSAFEPOINT {}
void fda_ok_header_only(void) {}
// CHECK: warning: Julia annotation "julia_not_safepoint" is on this declaration of 'fda_missing_func' but missing from its first declaration
// CHECK-FIXES: {{^}}void fda_missing_func(void) {}{{$}}
// CHECK-FIXES-H: {{^}}void fda_missing_func(void) JL_NOTSAFEPOINT;{{$}}
void fda_missing_func(void) JL_NOTSAFEPOINT {}

int fda_ok_param(int *p) { return *p; }
// CHECK: warning: Julia annotation "julia_propagates_root" is on parameter 1 of this declaration of 'fda_missing_param' but missing from its first declaration
// CHECK-FIXES: {{^}}int fda_missing_param(int *p) { return *p; }{{$}}
// CHECK-FIXES-H: {{^}}int fda_missing_param(int *p JL_PROPAGATES_ROOT);{{$}}
int fda_missing_param(int *p JL_PROPAGATES_ROOT) { return *p; }

void fda_ok_vis(void) JL_DLLEXPORT {}
// CHECK: warning: Julia annotation "visibility("default")" is on this declaration of 'fda_missing_vis' but missing from its first declaration
// CHECK-FIXES: {{^}}void fda_missing_vis(void) {}{{$}}
// CHECK-FIXES-H: {{^}}void fda_missing_vis(void) JL_DLLEXPORT;{{$}}
void fda_missing_vis(void) JL_DLLEXPORT {}

// CHECK: warning: Julia annotation "visibility("default")" is on this declaration of 'fda_raw_vis' but missing from its first declaration
// CHECK-FIXES: {{^}}void fda_raw_vis(void) __attribute__((visibility("default"))) {}{{$}}
void fda_raw_vis(void) __attribute__((visibility("default"))) {}

// CHECK: warning: Julia annotation "julia_not_safepoint" is on this declaration of 'fda_chain' but missing from its first declaration
// CHECK-FIXES: {{^}}void fda_chain(void) JL_NOTSAFEPOINT;{{$}}
// CHECK-FIXES: {{^}}void fda_chain(void);{{$}}
void fda_chain(void);
void fda_chain(void) JL_NOTSAFEPOINT;
void fda_chain(void) {}

// CHECK-FIXES: {{^}}void fda_local(void);{{$}}
// CHECK-FIXES: {{^ *}}void fda_local(void) JL_NOTSAFEPOINT;{{$}}
void fda_local(void);
void fda_local_caller(void) {
    void fda_local(void) JL_NOTSAFEPOINT;
    fda_local();
}
void fda_local(void) {}

// CHECK-FIXES: {{^}}void fda_localdef(void) {}{{$}}
// CHECK-FIXES: {{^ *}}void fda_localdef(void) JL_NOTSAFEPOINT;{{$}}
void fda_localdef(void) {}
void fda_localdef_caller(void) {
    void fda_localdef(void) JL_NOTSAFEPOINT;
    fda_localdef();
}

// CHECK: warning: Julia annotation "julia_not_safepoint" is on this declaration of 'fda_blockonly' but missing from its first declaration
void fda_blockonly_caller1(void) {
    void fda_blockonly(void);
    fda_blockonly();
}
void fda_blockonly_caller2(void) {
    void fda_blockonly(void) JL_NOTSAFEPOINT;
    fda_blockonly();
}

// Function-pointer annotation compatibility. Converting a function to fda_cb_t
// (which is JL_NOTSAFEPOINT) requires the function to carry that annotation, in
// every context where the conversion happens.
fda_cb_t fda_cb_okglobal = fda_cb_ok;

// CHECK: warning: 'fda_cb_bad' is converted to a function pointer of type 'fda_cb_t'{{.*}}requires Julia annotation "julia_not_safepoint", but 'fda_cb_bad' is not annotated "julia_not_safepoint"
fda_cb_t fda_cb_badglobal = fda_cb_bad;

void fda_cb_uses(void) {
    fda_cb_t a = fda_cb_ok;
    fda_cb_t b = uv_fda_fake;
    // CHECK: warning: 'fda_cb_bad' is converted to a function pointer of type 'fda_cb_t'{{.*}}"julia_not_safepoint"
    fda_cb_t c = fda_cb_bad;
    // CHECK: warning: 'fda_cb_bad' is converted to a function pointer of type 'fda_cb_t'{{.*}}"julia_not_safepoint"
    a = fda_cb_bad;
    // CHECK: warning: 'fda_cb_bad' is converted to a function pointer of type 'fda_cb_t'{{.*}}"julia_not_safepoint"
    fda_take_cb(fda_cb_bad);
    // CHECK: warning: 'fda_cb_bad' is converted to a function pointer of type 'fda_cb_t'{{.*}}"julia_not_safepoint"
    fda_take_cb(&fda_cb_bad);
    (void)a; (void)b; (void)c;
}

// CHECK: warning: 'fda_cb_bad' is converted to a function pointer of type 'fda_cb_t'{{.*}}"julia_not_safepoint"
fda_cb_t fda_cb_ret(void) { return fda_cb_bad; }

struct fda_cb_holder { fda_cb_t cb; };
// CHECK: warning: 'fda_cb_bad' is converted to a function pointer of type 'fda_cb_t'{{.*}}"julia_not_safepoint"
struct fda_cb_holder fda_cb_inst = { fda_cb_bad };

#ifdef __cplusplus
// CHECK-CXX: warning: Julia annotation "julia_not_safepoint" is on this declaration of 'm' but missing from its first declaration
struct fda_S { void m(void); };
void fda_S::m(void) JL_NOTSAFEPOINT {}
#endif

// The system header is simulated with a GCC line marker whose '3' flag marks
// the region as a system header.
// Kept last so the line-marker renumbering does not affect the cases above.
// CHECK-FIXES: {{^}}void fda_sysproto(void);{{$}}
// CHECK-FIXES: {{^}}void fda_sysproto(void) JL_NOTSAFEPOINT {}{{$}}
# 1 "fda_first_decl_annotations_fake_sys.h" 1 3
void fda_sysproto(void);
# 1 "FirstDeclAnnotationsTest.c" 2
void fda_sysproto(void) JL_NOTSAFEPOINT {}

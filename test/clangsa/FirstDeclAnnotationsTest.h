// This file is a part of Julia. License is MIT: https://julialang.org/license

// First (header) declarations for FirstDeclAnnotationsTest. The
// julia-first-decl-annotations check requires that every Julia annotation a
// function (or its parameters) carries is present on this first declaration.

#include "analyzer_annotations.h"

// Mimic the visibility/linkage macros from src/support/dtypes.h as expanded on
// the (non-Windows) analyzer platform, so the check sees the same attributes
// and the fix moves the same macro spelling that real Julia sources use.
#define JL_DLLEXPORT __attribute__((visibility("default")))
#define JL_HIDDEN __attribute__((visibility("hidden")))

void fda_ok_both(void) JL_CANSAFEPOINT;
void fda_ok_header_only(void) JL_CANSAFEPOINT;
int fda_ok_param(int *p JL_PROPAGATES_ROOT);

void fda_missing_func(void);
int fda_missing_param(int *p);

int fda_ok_cbparam(void (*cb)(int) JL_CANSAFEPOINT);
int fda_missing_cbparam(void (*cb)(int));

void fda_ok_vis(void) JL_DLLEXPORT;
void fda_missing_vis(void);
void fda_raw_vis(void);

typedef void (*fda_cb_t)(int);
typedef void (*fda_cb_safepoint_t)(int) JL_CANSAFEPOINT;
void fda_cb_ok(int x);
void fda_cb_bad(int x) JL_CANSAFEPOINT;
void uv_fda_fake(int x);
void fda_take_cb(fda_cb_t cb);
void fda_take_cansafepoint_cb(void (*cb)(int) JL_CANSAFEPOINT);

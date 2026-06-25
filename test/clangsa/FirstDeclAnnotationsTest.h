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

// Annotation present here: definitions may repeat it or inherit it -> OK.
void fda_ok_both(void) JL_NOTSAFEPOINT;
void fda_ok_header_only(void) JL_NOTSAFEPOINT;
int fda_ok_param(int *p JL_PROPAGATES_ROOT);

// No annotation here: a definition that adds one is what the check flags.
void fda_missing_func(void);
int fda_missing_param(int *p);

// Visibility attribute (as JL_DLLEXPORT/JL_HIDDEN expand to) present here -> OK;
// absent here but added on the definition is what the check flags.
void fda_ok_vis(void) JL_DLLEXPORT;
void fda_missing_vis(void);
void fda_raw_vis(void);

// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "ccalltest_common.h"

// We expect this to come from `libccalllazyfoo`
extern int foo(int);

DLLEXPORT int bar(int a) {
    return foo(a + 1);
}

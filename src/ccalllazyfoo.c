// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "ccalltest_common.h"

DLLEXPORT int foo(int a) {
    return a*2;
}

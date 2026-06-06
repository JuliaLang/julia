// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "ccalltest_common.h"

// Doubles the input integer a.
// @param a The integer to double.
// @return The doubled value of a.
DLLEXPORT int foo(int a) {
    return a*2;
}

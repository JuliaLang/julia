// This file is a part of Julia. License is MIT: https://julialang.org/license

// Julia LLVM dialect, generated from JuliaDialect.td by llvm-dialects-tblgen.
// See JuliaDialect.td for the specification of the dialect.

#pragma once

#define GET_INCLUDES
#include "JuliaDialect.h.inc"

namespace julia {

enum AddressSpace {
    Generic = 0,
    Tracked = 10,
    Derived = 11,
    CalleeRooted = 12,
    Loaded = 13,
    FirstSpecial = Tracked,
    LastSpecial = Loaded,
};

} // namespace julia

#define GET_DIALECT_DECLS
#include "JuliaDialect.h.inc"

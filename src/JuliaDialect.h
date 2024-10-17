
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

} // namespace xd

#define GET_DIALECT_DECLS
#include "JuliaDialect.h.inc"

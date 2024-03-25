
#pragma once

#define GET_INCLUDES
#include "JuliaDialect.h.inc"

namespace julia {

enum class AddressSoace {
    Generic = 0,
    Tracked = 10,
    Derived = 11,
    CalleeRooted = 12,
    Loaded = 13,
};

} // namespace xd

#define GET_DIALECT_DECLS
#include "JuliaDialect.h.inc"

// This file is a part of Julia. License is MIT: https://julialang.org/license

// Julia LLVM dialect, generated from JuliaDialect.td by llvm-dialects-tblgen.
// See JuliaDialect.td for the specification of the dialect.

#pragma once

#ifndef JL_NOTSAFEPOINT
#define JL_NOTSAFEPOINT
#endif

#include <llvm/IR/Module.h>
#include <llvm-dialects/Dialect/OpDescription.h>

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

namespace julia {

// Returns the declaration of the dialect op `OpT` in `M`, or nullptr if no
// call to the op has been emitted into the module. Useful for passes that
// want to skip work when a module does not use an op at all, or that need
// the declaration itself (e.g. to walk all users of an op).
template <typename OpT>
inline llvm::Function *getOpDeclaration(llvm::Module &M) JL_NOTSAFEPOINT
{
    return M.getFunction(llvm_dialects::OpDescription::get<OpT>().getMnemonic());
}

} // namespace julia

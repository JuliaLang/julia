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

// RAII attachment of the Julia dialect to an LLVMContext, required while
// dialect ops are being *created* in the context (recognition via isa<> is
// pure name matching and needs no attachment). Attachments are refcounted
// per context, so emission state (jl_codegen_output_t), optimization passes,
// and external producers can nest attachments freely. The last release
// destroys the underlying llvm_dialects::DialectContext, which must happen
// before the LLVMContext itself is destroyed.
class ScopedDialects {
    llvm::LLVMContext *ctx;

public:
    explicit ScopedDialects(llvm::LLVMContext &ctx) JL_NOTSAFEPOINT;
    ~ScopedDialects() JL_NOTSAFEPOINT;
    ScopedDialects(ScopedDialects &&other) JL_NOTSAFEPOINT : ctx(other.ctx) { other.ctx = nullptr; }
    ScopedDialects(const ScopedDialects &) = delete;
    ScopedDialects &operator=(const ScopedDialects &) = delete;
    ScopedDialects &operator=(ScopedDialects &&) = delete;
};

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

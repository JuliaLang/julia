// This file is a part of Julia. License is MIT: https://julialang.org/license

// Julia LLVM dialect, generated from JuliaDialect.td by llvm-dialects-tblgen.
// See JuliaDialect.td for the specification of the dialect.

#pragma once

#ifndef JL_NOTSAFEPOINT
#define JL_NOTSAFEPOINT
#endif

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
// and external producers (via the C API) can nest attachments freely. The
// last release destroys the underlying llvm_dialects::DialectContext, which
// must happen before the LLVMContext itself is destroyed.
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

} // namespace julia

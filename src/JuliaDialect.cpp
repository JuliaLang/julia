// This file is a part of Julia. License is MIT: https://julialang.org/license

// Julia LLVM dialect, generated from JuliaDialect.td by llvm-dialects-tblgen,
// plus a C API for external producers (GPUCompiler.jl, Enzyme.jl, and
// eventually a Julia port of codegen itself) to build IR that is legal in the
// Julia dialect without linking against the C++ op classes.

#include "JuliaDialect.h"

#define GET_INCLUDES
#include "JuliaDialect.cpp.inc"

#define GET_DIALECT_DEFS
#include "JuliaDialect.cpp.inc"

#include "jitlayers.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/Support/CBindingWrapping.h>

#include <llvm-dialects/Dialect/Builder.h>
#include <llvm-dialects/Dialect/Dialect.h>
#include <llvm-dialects/Dialect/Verifier.h>

using namespace llvm;

typedef struct JLOpaqueDialectContext *JLDialectContextRef;
DEFINE_SIMPLE_CONVERSION_FUNCTIONS(llvm_dialects::DialectContext, JLDialectContextRef)

template <typename OpT, typename... ArgTs>
static LLVMValueRef build_op(LLVMBuilderRef B, ArgTs... args)
{
    IRBuilder<> *irb = unwrap(B);
    assert(irb->GetInsertBlock() && "builder must have an insertion point");
    llvm_dialects::Builder db(irb->GetInsertBlock(), irb->GetInsertPoint());
    db.SetCurrentDebugLocation(irb->getCurrentDebugLocation());
    return wrap(db.create<OpT>(unwrap(args)...));
}

extern "C" {

// Attach the Julia dialect to a context. The returned handle must be
// disposed with `JLDialectsDisposeContext` before the context is destroyed
// (and before any re-attachment for a context living at the same address).
JL_DLLEXPORT_CODEGEN JLDialectContextRef JLDialectsAttachContext_impl(LLVMContextRef C)
{
    auto dc = llvm_dialects::DialectContext::make<julia::JuliaDialect>(*unwrap(C));
    return wrap(dc.release());
}

JL_DLLEXPORT_CODEGEN void JLDialectsDisposeContext_impl(JLDialectContextRef DC)
{
    delete unwrap(DC);
}

// Verify that a module only uses ops of the Julia dialect in a well-formed
// way. Returns 1 on success. Diagnostics are printed to stderr.
JL_DLLEXPORT_CODEGEN LLVMBool JLDialectsVerifyModule_impl(LLVMModuleRef M)
{
    return llvm_dialects::verify(*unwrap(M), errs());
}

// Op builders. Each of these creates the op at the insertion point of `B`,
// with the correct types and attributes as specified in JuliaDialect.td.
// The dialect must have been attached to the module's context, either by
// emitting into a context set up by Julia's own codegen or by calling
// `JLDialectsAttachContext`.

JL_DLLEXPORT_CODEGEN LLVMValueRef JLBuildGetPGCStack_impl(LLVMBuilderRef B)
{
    return build_op<julia::GetPGCStack>(B);
}

JL_DLLEXPORT_CODEGEN LLVMValueRef JLBuildGetPGCStackOrNew_impl(LLVMBuilderRef B)
{
    return build_op<julia::GetPGCStackOrNew>(B);
}

JL_DLLEXPORT_CODEGEN LLVMValueRef JLBuildGCLoaded_impl(LLVMBuilderRef B, LLVMValueRef base, LLVMValueRef tracked)
{
    return build_op<julia::GCLoaded>(B, base, tracked);
}

JL_DLLEXPORT_CODEGEN LLVMValueRef JLBuildNewGCFrame_impl(LLVMBuilderRef B, LLVMValueRef size)
{
    return build_op<julia::NewGCFrame>(B, size);
}

JL_DLLEXPORT_CODEGEN LLVMValueRef JLBuildPushGCFrame_impl(LLVMBuilderRef B, LLVMValueRef frame, LLVMValueRef size)
{
    return build_op<julia::PushGCFrame>(B, frame, size);
}

JL_DLLEXPORT_CODEGEN LLVMValueRef JLBuildPopGCFrame_impl(LLVMBuilderRef B, LLVMValueRef frame)
{
    return build_op<julia::PopGCFrame>(B, frame);
}

JL_DLLEXPORT_CODEGEN LLVMValueRef JLBuildGetGCFrameSlot_impl(LLVMBuilderRef B, LLVMValueRef frame, LLVMValueRef index)
{
    return build_op<julia::GetGCFrameSlot>(B, frame, index);
}

JL_DLLEXPORT_CODEGEN LLVMValueRef JLBuildGCAllocBytes_impl(LLVMBuilderRef B, LLVMValueRef ptls, LLVMValueRef size, LLVMValueRef type)
{
    return build_op<julia::GCAllocBytes>(B, ptls, size, type);
}

JL_DLLEXPORT_CODEGEN LLVMValueRef JLBuildQueueGCRoot_impl(LLVMBuilderRef B, LLVMValueRef root)
{
    return build_op<julia::QueueGCRoot>(B, root);
}

JL_DLLEXPORT_CODEGEN LLVMValueRef JLBuildSafepoint_impl(LLVMBuilderRef B, LLVMValueRef signal_page)
{
    return build_op<julia::Safepoint>(B, signal_page);
}

} // extern "C"

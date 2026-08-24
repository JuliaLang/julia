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
#include "passes.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>
#include <llvm/ADT/DenseMap.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/Support/CBindingWrapping.h>

#include <mutex>

#include <llvm-dialects/Dialect/Builder.h>
#include <llvm-dialects/Dialect/Dialect.h>
#include <llvm-dialects/Dialect/Verifier.h>

using namespace llvm;

namespace julia {

// Per-context refcounted registry backing ScopedDialects. The entry is
// erased when the last attachment goes away, so a context address that gets
// reused by a later LLVMContext starts from a clean slate.
namespace {
struct DialectAttachment {
    std::unique_ptr<llvm_dialects::DialectContext> dc;
    unsigned refcount = 0;
};
std::mutex attachmentsLock;
DenseMap<LLVMContext *, DialectAttachment> attachments;
} // anonymous namespace

ScopedDialects::ScopedDialects(LLVMContext &ctx) : ctx(&ctx)
{
    std::lock_guard<std::mutex> lock(attachmentsLock);
    auto &attachment = attachments[&ctx];
    if (attachment.refcount++ == 0)
        attachment.dc = llvm_dialects::DialectContext::make<JuliaDialect>(ctx);
}

ScopedDialects::~ScopedDialects()
{
    if (!ctx) // moved from
        return;
    std::lock_guard<std::mutex> lock(attachmentsLock);
    auto it = attachments.find(ctx);
    assert(it != attachments.end() && it->second.refcount > 0);
    if (--it->second.refcount == 0)
        attachments.erase(it);
}

} // namespace julia

PreservedAnalyses JuliaDialectsVerifierPass::run(Module &M, ModuleAnalysisManager &AM)
{
    julia::ScopedDialects dialects(M.getContext());
    if (!llvm_dialects::verify(M, errs())) {
        errs() << "Julia dialect verification failed, dumping entire module!\n\n";
        errs() << M << "\n";
        abort();
    }
    return PreservedAnalyses::all();
}

typedef struct JLOpaqueDialectContext *JLDialectContextRef;
DEFINE_SIMPLE_CONVERSION_FUNCTIONS(julia::ScopedDialects, JLDialectContextRef)

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
// disposed with `JLDialectsDisposeContext` before the context is destroyed.
// Attachments are refcounted per context, so this composes with Julia's own
// emission and passes attaching to the same context.
JL_DLLEXPORT_CODEGEN JLDialectContextRef JLDialectsAttachContext_impl(LLVMContextRef C)
{
    return wrap(new julia::ScopedDialects(*unwrap(C)));
}

JL_DLLEXPORT_CODEGEN void JLDialectsDisposeContext_impl(JLDialectContextRef DC)
{
    delete unwrap(DC);
}

// Verify that a module only uses ops of the Julia dialect in a well-formed
// way. Follows the LLVMVerifyModule convention: returns 0 on success, 1 on a
// broken module. If OutMessage is non-null, *OutMessage receives the
// diagnostics (empty on success; always dispose it with LLVMDisposeMessage);
// otherwise they are printed to stderr.
JL_DLLEXPORT_CODEGEN LLVMBool JLDialectsVerifyModule_impl(LLVMModuleRef M, char **OutMessage)
{
    julia::ScopedDialects dialects(unwrap(M)->getContext());
    if (!OutMessage)
        return !llvm_dialects::verify(*unwrap(M), errs());
    std::string msg;
    raw_string_ostream os(msg);
    bool ok = llvm_dialects::verify(*unwrap(M), os);
    *OutMessage = LLVMCreateMessage(msg.c_str());
    return !ok;
}

// The integer type of julia.gc_alloc_bytes' size and type-tag arguments,
// which is target dependent (the module datalayout's pointer-sized integer).
// The op verifies with any integer width, but this is the type the GC
// lowering itself produces and the runtime expects.
JL_DLLEXPORT_CODEGEN LLVMTypeRef JLDialectsGCAllocBytesSizeType_impl(LLVMModuleRef M)
{
    Module *mod = unwrap(M);
    return wrap(mod->getDataLayout().getIntPtrType(mod->getContext()));
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

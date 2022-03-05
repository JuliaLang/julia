// This file is a part of Julia. License is MIT: https://julialang.org/license

//
// This file implements common functionality that is useful for the late GC frame
// lowering and final GC intrinsic lowering passes. See the corresponding header
// for docs.

#include "llvm-version.h"

#include <llvm/IR/Function.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>

#include "codegen_shared.h"
#include "julia_assert.h"
#include "llvm-pass-helpers.h"

using namespace llvm;

void JuliaCommonGCFunctions::initialize(Module &M) {
    gc_preserve_begin_func = M.getFunction("llvm.julia.gc_preserve_begin");
    gc_preserve_end_func = M.getFunction("llvm.julia.gc_preserve_end");
    write_barrier_func = M.getFunction("julia.write_barrier");
    alloc_obj_func = M.getFunction("julia.gc_alloc_obj");
    pointer_from_objref_func = M.getFunction("julia.pointer_from_objref");
    typeof_func = M.getFunction("julia.typeof");
}

JuliaPassContext::JuliaPassContext()
    : T_prjlvalue(nullptr)
{
}

void JuliaPassContext::initAll(LLVMContext &ctx)
{
    // Construct derived types.
    T_prjlvalue = JuliaType::get_prjlvalue_ty(ctx);
}

llvm::CallInst *JuliaPassContext::getPGCstack(llvm::Function *pgcstack_getter, llvm::Function &F)
{
    for (auto I = F.getEntryBlock().begin(), E = F.getEntryBlock().end();
         pgcstack_getter && I != E; ++I) {
        if (CallInst *callInst = dyn_cast<CallInst>(&*I)) {
            if (callInst->getCalledOperand() == pgcstack_getter) {
                return callInst;
            }
        }
    }
    return nullptr;
}

llvm::Function *JuliaPassContext::getOrNull(Module &M,
    const jl_intrinsics::IntrinsicDescription &desc)
{
    return M.getFunction(desc.name);
}

llvm::Function *JuliaPassContext::getOrDeclare(Module &M,
    const jl_intrinsics::IntrinsicDescription &desc)
{
    auto local = getOrNull(M, desc);
    if (local) {
        // If the function exists already, then we'll
        // just return it.
        return local;
    }
    else {
        // Otherwise, we'll declare it and add it to the module.
        // Declare the function.
        auto func = desc.declare(M.getContext());
        // Add it to the function list.
        M.getFunctionList().push_back(func);
        // Return the newly created function.
        return func;
    }
}

namespace jl_intrinsics {
    static const char *GET_GC_FRAME_SLOT_NAME = "julia.get_gc_frame_slot";
    static const char *GC_ALLOC_BYTES_NAME = "julia.gc_alloc_bytes";
    static const char *NEW_GC_FRAME_NAME = "julia.new_gc_frame";
    static const char *PUSH_GC_FRAME_NAME = "julia.push_gc_frame";
    static const char *POP_GC_FRAME_NAME = "julia.pop_gc_frame";
    static const char *QUEUE_GC_ROOT_NAME = "julia.queue_gc_root";

    // Annotates a function with attributes suitable for GC allocation
    // functions. Specifically, the return value is marked noalias and nonnull.
    // The allocation size is set to the first argument.
    static Function *addGCAllocAttributes(Function *target, LLVMContext &context)
    {
        addRetAttr(target, Attribute::NoAlias);
        addRetAttr(target, Attribute::NonNull);
        target->addFnAttr(Attribute::getWithAllocSizeArgs(context, 1, None)); // returns %1 bytes
        return target;
    }

    const IntrinsicDescription getGCFrameSlot(
        GET_GC_FRAME_SLOT_NAME,
        [](LLVMContext &context) {
            auto pprjlvalue = JuliaType::get_pprjlvalue_ty(context);
            return Function::Create(
                FunctionType::get(
                    pprjlvalue, {pprjlvalue, Type::getInt32Ty(context)},
                    false),
                Function::ExternalLinkage,
                GET_GC_FRAME_SLOT_NAME);
        });

    const IntrinsicDescription GCAllocBytes(
        GC_ALLOC_BYTES_NAME,
        [](LLVMContext &context) {
            auto intrinsic = Function::Create(
                FunctionType::get(
                    JuliaType::get_prjlvalue_ty(context),
                    { Type::getInt8PtrTy(context),
                        sizeof(size_t) == sizeof(uint32_t) ?
                        Type::getInt32Ty(context) :
                        Type::getInt64Ty(context) },
                    false),
                Function::ExternalLinkage,
                GC_ALLOC_BYTES_NAME);

            return addGCAllocAttributes(intrinsic, context);
        });

    const IntrinsicDescription newGCFrame(
        NEW_GC_FRAME_NAME,
        [](LLVMContext &context) {
            auto intrinsic = Function::Create(
                FunctionType::get(JuliaType::get_pprjlvalue_ty(context), {Type::getInt32Ty(context)}, false),
                Function::ExternalLinkage,
                NEW_GC_FRAME_NAME);
            addRetAttr(intrinsic, Attribute::NoAlias);
            addRetAttr(intrinsic, Attribute::NonNull);

            return intrinsic;
        });

    const IntrinsicDescription pushGCFrame(
        PUSH_GC_FRAME_NAME,
        [](LLVMContext &context) {
            return Function::Create(
                FunctionType::get(
                    Type::getVoidTy(context),
                    {JuliaType::get_pprjlvalue_ty(context), Type::getInt32Ty(context)},
                    false),
                Function::ExternalLinkage,
                PUSH_GC_FRAME_NAME);
        });

    const IntrinsicDescription popGCFrame(
        POP_GC_FRAME_NAME,
        [](LLVMContext &context) {
            return Function::Create(
                FunctionType::get(
                    Type::getVoidTy(context),
                    {JuliaType::get_pprjlvalue_ty(context)},
                    false),
                Function::ExternalLinkage,
                POP_GC_FRAME_NAME);
        });

    const IntrinsicDescription queueGCRoot(
        QUEUE_GC_ROOT_NAME,
        [](LLVMContext &context) {
            auto intrinsic = Function::Create(
                FunctionType::get(
                    Type::getVoidTy(context),
                    { JuliaType::get_prjlvalue_ty(context) },
                    false),
                Function::ExternalLinkage,
                QUEUE_GC_ROOT_NAME);
            intrinsic->addFnAttr(Attribute::InaccessibleMemOrArgMemOnly);
            return intrinsic;
        });
}

namespace jl_well_known {
    static const char *GC_BIG_ALLOC_NAME = XSTR(jl_gc_big_alloc);
    static const char *GC_POOL_ALLOC_NAME = XSTR(jl_gc_pool_alloc);
    static const char *GC_QUEUE_ROOT_NAME = XSTR(jl_gc_queue_root);

    using jl_intrinsics::addGCAllocAttributes;

    const WellKnownFunctionDescription GCBigAlloc(
        GC_BIG_ALLOC_NAME,
        [](LLVMContext &context) {
            auto bigAllocFunc = Function::Create(
                FunctionType::get(
                    JuliaType::get_pprjlvalue_ty(context),
                    { Type::getInt8PtrTy(context),
                        sizeof(size_t) == sizeof(uint32_t) ?
                        Type::getInt32Ty(context) :
                        Type::getInt64Ty(context) },
                    false),
                Function::ExternalLinkage,
                GC_BIG_ALLOC_NAME);

            return addGCAllocAttributes(bigAllocFunc, context);
        });

    const WellKnownFunctionDescription GCPoolAlloc(
        GC_POOL_ALLOC_NAME,
        [](LLVMContext &context) {
            auto poolAllocFunc = Function::Create(
                FunctionType::get(
                    JuliaType::get_prjlvalue_ty(context),
                    { Type::getInt8PtrTy(context), Type::getInt32Ty(context), Type::getInt32Ty(context) },
                    false),
                Function::ExternalLinkage,
                GC_POOL_ALLOC_NAME);

            return addGCAllocAttributes(poolAllocFunc, context);
        });

    const WellKnownFunctionDescription GCQueueRoot(
        GC_QUEUE_ROOT_NAME,
        [](LLVMContext &context) {
            auto func = Function::Create(
                FunctionType::get(
                    Type::getVoidTy(context),
                    { JuliaType::get_prjlvalue_ty(context) },
                    false),
                Function::ExternalLinkage,
                GC_QUEUE_ROOT_NAME);
            func->addFnAttr(Attribute::InaccessibleMemOrArgMemOnly);
            return func;
        });
}

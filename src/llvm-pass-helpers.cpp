// This file is a part of Julia. License is MIT: https://julialang.org/license

//
// This file implements common functionality that is useful for the late GC frame
// lowering and final GC intrinsic lowering passes. See the corresponding header
// for docs.

#include "llvm-version.h"

#include "llvm/IR/Attributes.h"
#include "llvm/IR/DerivedTypes.h"
#include <llvm/IR/Function.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>

#include "llvm-codegen-shared.h"
#include "julia_assert.h"
#include "llvm-pass-helpers.h"

using namespace llvm;

JuliaPassContext::JuliaPassContext()
    : T_prjlvalue(nullptr),

        tbaa_gcframe(nullptr), tbaa_tag(nullptr),

        pgcstack_getter(nullptr), adoptthread_func(nullptr), gc_flush_func(nullptr),
        gc_preserve_begin_func(nullptr), gc_preserve_end_func(nullptr),
        pointer_from_objref_func(nullptr), gc_loaded_func(nullptr), alloc_obj_func(nullptr),
        typeof_func(nullptr), write_barrier_func(nullptr), pop_handler_noexcept_func(nullptr),
        call_func(nullptr), call2_func(nullptr), call3_func(nullptr), module(nullptr)
{
}

void JuliaPassContext::initFunctions(Module &M)
{
    module = &M;
    LLVMContext &llvmctx = M.getContext();

    tbaa_gcframe = tbaa_make_child_with_context(llvmctx, "jtbaa_gcframe").first;
    MDNode *tbaa_data;
    MDNode *tbaa_data_scalar;
    std::tie(tbaa_data, tbaa_data_scalar) = tbaa_make_child_with_context(llvmctx, "jtbaa_data");
    tbaa_tag = tbaa_make_child_with_context(llvmctx, "jtbaa_tag", tbaa_data_scalar).first;

    pgcstack_getter = M.getFunction("julia.get_pgcstack");
    adoptthread_func = M.getFunction("julia.get_pgcstack_or_new");
    gc_flush_func = M.getFunction("julia.gcroot_flush");
    gc_preserve_begin_func = M.getFunction("llvm.julia.gc_preserve_begin");
    gc_preserve_end_func = M.getFunction("llvm.julia.gc_preserve_end");
    pointer_from_objref_func = M.getFunction("julia.pointer_from_objref");
    gc_loaded_func = M.getFunction("julia.gc_loaded");
    typeof_func = M.getFunction("julia.typeof");
    write_barrier_func = M.getFunction("julia.write_barrier");
    alloc_obj_func = M.getFunction("julia.gc_alloc_obj");
    pop_handler_noexcept_func = M.getFunction(XSTR(jl_pop_handler_noexcept));
    call_func = M.getFunction("julia.call");
    call2_func = M.getFunction("julia.call2");
    call3_func = M.getFunction("julia.call3");
}

void JuliaPassContext::initAll(Module &M)
{
    // First initialize the functions.
    initFunctions(M);

    // Then initialize types and metadata nodes.
    auto &ctx = M.getContext();

    // Construct derived types.
    T_prjlvalue = JuliaType::get_prjlvalue_ty(ctx);
}

llvm::Value *JuliaPassContext::getPGCstack(llvm::Function &F) const
{
    if (pgcstack_getter || adoptthread_func) {
        for (auto &I : F.getEntryBlock()) {
            if (CallInst *callInst = dyn_cast<CallInst>(&I)) {
                Value *callee = callInst->getCalledOperand();
                if ((pgcstack_getter && callee == pgcstack_getter) ||
                    (adoptthread_func && callee == adoptthread_func)) {
                    return callInst;
                }
            }
        }
    }
    if (F.getCallingConv() == CallingConv::Swift) {
        for (auto &arg : F.args()) {
            if (arg.hasSwiftSelfAttr())
                return &arg;
        }
    }
    return nullptr;
}

llvm::Function *JuliaPassContext::getOrNull(
    const jl_intrinsics::IntrinsicDescription &desc) const
{
    return module->getFunction(desc.name);
}

llvm::Function *JuliaPassContext::getOrDeclare(
    const jl_intrinsics::IntrinsicDescription &desc)
{
    auto local = getOrNull(desc);
    if (local) {
        // If the function exists already, then we'll
        // just return it.
        return local;
    }
    else {
        // Otherwise, we'll declare it and add it to the module.
        // Declare the function.
        auto T_size = module->getDataLayout().getIntPtrType(module->getContext());
        auto func = desc.declare(T_size);
        // Add it to the function list.
        module->getFunctionList().push_back(func);
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
    static const char *SAFEPOINT_NAME = "julia.safepoint";

    // Annotates a function with attributes suitable for GC allocation
    // functions. Specifically, the return value is marked noalias and nonnull.
    static Function *addGCAllocAttributes(Function *target)
    {
        auto FnAttrs = AttrBuilder(target->getContext());
        FnAttrs.addMemoryAttr(MemoryEffects::argMemOnly(ModRefInfo::Ref) | MemoryEffects::inaccessibleMemOnly(ModRefInfo::ModRef));
        FnAttrs.addAllocKindAttr(AllocFnKind::Alloc);
        FnAttrs.addAttribute(Attribute::WillReturn);
        FnAttrs.addAttribute(Attribute::NoUnwind);
        target->addFnAttrs(FnAttrs);
        addRetAttr(target, Attribute::NoAlias);
        addRetAttr(target, Attribute::NonNull);
        return target;
    }

    const IntrinsicDescription getGCFrameSlot(
        GET_GC_FRAME_SLOT_NAME,
        [](Type *T_size) {
            auto &ctx = T_size->getContext();
            auto T_pprjlvalue = JuliaType::get_pprjlvalue_ty(ctx);
            return Function::Create(
                FunctionType::get(
                    T_pprjlvalue,
                    {T_pprjlvalue, Type::getInt32Ty(ctx)},
                    false),
                Function::ExternalLinkage,
                GET_GC_FRAME_SLOT_NAME);
        });

    const IntrinsicDescription GCAllocBytes(
        GC_ALLOC_BYTES_NAME,
        [](Type *T_size) {
            auto &ctx = T_size->getContext();
            auto T_prjlvalue = JuliaType::get_prjlvalue_ty(ctx);
            auto intrinsic = Function::Create(
                FunctionType::get(
                    T_prjlvalue,
                    { PointerType::get(ctx, 0),
                        T_size,
                        T_size }, // type
                    false),
                Function::ExternalLinkage,
                GC_ALLOC_BYTES_NAME);
            intrinsic->addFnAttr(Attribute::getWithAllocSizeArgs(ctx, 1, None));
            return addGCAllocAttributes(intrinsic);
        });

    const IntrinsicDescription newGCFrame(
        NEW_GC_FRAME_NAME,
        [](Type *T_size) {
            auto &ctx = T_size->getContext();
            auto T_pprjlvalue = JuliaType::get_pprjlvalue_ty(ctx);
            auto intrinsic = Function::Create(
                FunctionType::get(T_pprjlvalue, {Type::getInt32Ty(ctx)}, false),
                Function::ExternalLinkage,
                NEW_GC_FRAME_NAME);
            addRetAttr(intrinsic, Attribute::NoAlias);
            addRetAttr(intrinsic, Attribute::NonNull);

            return intrinsic;
        });

    const IntrinsicDescription pushGCFrame(
        PUSH_GC_FRAME_NAME,
        [](Type *T_size) {
            auto &ctx = T_size->getContext();
            auto T_pprjlvalue = JuliaType::get_pprjlvalue_ty(ctx);
            return Function::Create(
                FunctionType::get(
                    Type::getVoidTy(ctx),
                    {T_pprjlvalue, Type::getInt32Ty(ctx)},
                    false),
                Function::ExternalLinkage,
                PUSH_GC_FRAME_NAME);
        });

    const IntrinsicDescription popGCFrame(
        POP_GC_FRAME_NAME,
        [](Type *T_size) {
            auto &ctx = T_size->getContext();
            auto T_pprjlvalue = JuliaType::get_pprjlvalue_ty(ctx);
            return Function::Create(
                FunctionType::get(
                    Type::getVoidTy(ctx),
                    {T_pprjlvalue},
                    false),
                Function::ExternalLinkage,
                POP_GC_FRAME_NAME);
        });

    const IntrinsicDescription queueGCRoot(
        QUEUE_GC_ROOT_NAME,
        [](Type *T_size) {
            auto &ctx = T_size->getContext();
            auto T_prjlvalue = JuliaType::get_prjlvalue_ty(ctx);
            auto intrinsic = Function::Create(
                FunctionType::get(
                    Type::getVoidTy(ctx),
                    { T_prjlvalue },
                    false),
                Function::ExternalLinkage,
                QUEUE_GC_ROOT_NAME);
            intrinsic->setMemoryEffects(MemoryEffects::inaccessibleOrArgMemOnly());
            return intrinsic;
        });

    const IntrinsicDescription safepoint(
        SAFEPOINT_NAME,
        [](Type *T_size) {
            auto &ctx = T_size->getContext();
            auto T_psize = PointerType::getUnqual(ctx);
            auto intrinsic = Function::Create(
                FunctionType::get(
                    Type::getVoidTy(ctx),
                    {T_psize},
                    false),
                Function::ExternalLinkage,
                SAFEPOINT_NAME);
            intrinsic->setMemoryEffects(MemoryEffects::inaccessibleOrArgMemOnly());
            return intrinsic;
        });
}

namespace jl_well_known {
    static const char *GC_BIG_ALLOC_NAME = XSTR(jl_gc_big_alloc);
    static const char *GC_SMALL_ALLOC_NAME = XSTR(jl_gc_small_alloc);
    static const char *GC_QUEUE_ROOT_NAME = XSTR(jl_gc_queue_root);
    static const char *GC_ALLOC_TYPED_NAME = XSTR(jl_gc_alloc_typed);

    using jl_intrinsics::addGCAllocAttributes;

    const WellKnownFunctionDescription GCBigAlloc(
        GC_BIG_ALLOC_NAME,
        [](Type *T_size) {
            auto &ctx = T_size->getContext();
            auto T_prjlvalue = JuliaType::get_prjlvalue_ty(ctx);
            auto bigAllocFunc = Function::Create(
                FunctionType::get(
                    T_prjlvalue,
                    { PointerType::get(ctx, 0), T_size , T_size},
                    false),
                Function::ExternalLinkage,
                GC_BIG_ALLOC_NAME);
            bigAllocFunc->addFnAttr(Attribute::getWithAllocSizeArgs(ctx, 1, None));
            return addGCAllocAttributes(bigAllocFunc);
        });

    const WellKnownFunctionDescription GCSmallAlloc(
        GC_SMALL_ALLOC_NAME,
        [](Type *T_size) {
            auto &ctx = T_size->getContext();
            auto T_prjlvalue = JuliaType::get_prjlvalue_ty(ctx);
            auto smallAllocFunc = Function::Create(
                FunctionType::get(
                    T_prjlvalue,
                    { PointerType::get(ctx, 0), Type::getInt32Ty(ctx), Type::getInt32Ty(ctx), T_size },
                    false),
                Function::ExternalLinkage,
                GC_SMALL_ALLOC_NAME);
            smallAllocFunc->addFnAttr(Attribute::getWithAllocSizeArgs(ctx, 2, None));
            return addGCAllocAttributes(smallAllocFunc);
        });

    const WellKnownFunctionDescription GCQueueRoot(
        GC_QUEUE_ROOT_NAME,
        [](Type *T_size) {
            auto &ctx = T_size->getContext();
            auto T_prjlvalue = JuliaType::get_prjlvalue_ty(ctx);
            auto func = Function::Create(
                FunctionType::get(
                    Type::getVoidTy(ctx),
                    { T_prjlvalue },
                    false),
                Function::ExternalLinkage,
                GC_QUEUE_ROOT_NAME);
            func->setMemoryEffects(MemoryEffects::inaccessibleOrArgMemOnly());
            return func;
        });

    const WellKnownFunctionDescription GCAllocTyped(
        GC_ALLOC_TYPED_NAME,
        [](Type *T_size) {
            auto &ctx = T_size->getContext();
            auto T_prjlvalue = JuliaType::get_prjlvalue_ty(ctx);
            auto allocTypedFunc = Function::Create(
                FunctionType::get(
                    T_prjlvalue,
                    { PointerType::get(ctx, 0),
                        T_size,
                        T_size }, // type
                    false),
                Function::ExternalLinkage,
                GC_ALLOC_TYPED_NAME);
            allocTypedFunc->addFnAttr(Attribute::getWithAllocSizeArgs(ctx, 1, None));
            return addGCAllocAttributes(allocTypedFunc);
        });
}

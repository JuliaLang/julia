// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-version.h"
#include "passes.h"

#include <llvm/ADT/Statistic.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Pass.h>
#include <llvm/Support/Debug.h>
#include <llvm/Transforms/Utils/ModuleUtils.h>

#include "llvm-codegen-shared.h"
#include "julia.h"
#include "julia_internal.h"
#include "llvm-pass-helpers.h"

#define DEBUG_TYPE "final_gc_lowering"
STATISTIC(NewGCFrameCount, "Number of lowered newGCFrameFunc intrinsics");
STATISTIC(PushGCFrameCount, "Number of lowered pushGCFrameFunc intrinsics");
STATISTIC(PopGCFrameCount, "Number of lowered popGCFrameFunc intrinsics");
STATISTIC(GetGCFrameSlotCount, "Number of lowered getGCFrameSlotFunc intrinsics");
STATISTIC(GCAllocBytesCount, "Number of lowered GCAllocBytesFunc intrinsics");
STATISTIC(QueueGCRootCount, "Number of lowered queueGCRootFunc intrinsics");
STATISTIC(SafepointCount, "Number of lowered safepoint intrinsics");

using namespace llvm;

// The final GC lowering pass. This pass lowers platform-agnostic GC
// intrinsics to platform-dependent instruction sequences. The
// intrinsics it targets are those produced by the late GC frame
// lowering pass.
//
// This pass targets typical back-ends for which the standard Julia
// runtime library is available. Atypical back-ends should supply
// their own lowering pass.

struct FinalLowerGC: private JuliaPassContext {
    bool runOnFunction(Function &F);

private:
    Function *queueRootFunc;
    Function *poolAllocFunc;
    Function *bigAllocFunc;
    Function *allocTypedFunc;
    Instruction *pgcstack;
    Type *T_size;

    // Lowers a `julia.new_gc_frame` intrinsic.
    void lowerNewGCFrame(CallInst *target, Function &F);

    // Lowers a `julia.push_gc_frame` intrinsic.
    void lowerPushGCFrame(CallInst *target, Function &F);

    // Lowers a `julia.pop_gc_frame` intrinsic.
    void lowerPopGCFrame(CallInst *target, Function &F);

    // Lowers a `julia.get_gc_frame_slot` intrinsic.
    void lowerGetGCFrameSlot(CallInst *target, Function &F);

    // Lowers a `julia.gc_alloc_bytes` intrinsic.
    void lowerGCAllocBytes(CallInst *target, Function &F);

    // Lowers a `julia.queue_gc_root` intrinsic.
    void lowerQueueGCRoot(CallInst *target, Function &F);

    // Lowers a `julia.safepoint` intrinsic.
    void lowerSafepoint(CallInst *target, Function &F);
};

void FinalLowerGC::lowerNewGCFrame(CallInst *target, Function &F)
{
    ++NewGCFrameCount;
    assert(target->arg_size() == 1);
    unsigned nRoots = cast<ConstantInt>(target->getArgOperand(0))->getLimitedValue(INT_MAX);

    // Create the GC frame.
    IRBuilder<> builder(target);
    auto gcframe_alloca = builder.CreateAlloca(T_prjlvalue, ConstantInt::get(Type::getInt32Ty(F.getContext()), nRoots + 2));
    gcframe_alloca->setAlignment(Align(16));
    // addrspacecast as needed for non-0 alloca addrspace
    auto gcframe = cast<Instruction>(builder.CreateAddrSpaceCast(gcframe_alloca, T_prjlvalue->getPointerTo(0)));
    gcframe->takeName(target);

    // Zero out the GC frame.
    auto ptrsize = F.getParent()->getDataLayout().getPointerSize();
    builder.CreateMemSet(gcframe, Constant::getNullValue(Type::getInt8Ty(F.getContext())), ptrsize * (nRoots + 2), Align(16), tbaa_gcframe);

    target->replaceAllUsesWith(gcframe);
    target->eraseFromParent();
}

void FinalLowerGC::lowerPushGCFrame(CallInst *target, Function &F)
{
    ++PushGCFrameCount;
    assert(target->arg_size() == 2);
    auto gcframe = target->getArgOperand(0);
    unsigned nRoots = cast<ConstantInt>(target->getArgOperand(1))->getLimitedValue(INT_MAX);

    IRBuilder<> builder(target);
    StoreInst *inst = builder.CreateAlignedStore(
                ConstantInt::get(T_size, JL_GC_ENCODE_PUSHARGS(nRoots)),
                builder.CreateBitCast(
                        builder.CreateConstInBoundsGEP1_32(T_prjlvalue, gcframe, 0, "frame.nroots"),
                        T_size->getPointerTo(), "frame.nroots"), // GEP of 0 becomes a noop and eats the name
                Align(sizeof(void*)));
    inst->setMetadata(LLVMContext::MD_tbaa, tbaa_gcframe);
    auto T_ppjlvalue = JuliaType::get_ppjlvalue_ty(F.getContext());
    inst = builder.CreateAlignedStore(
            builder.CreateAlignedLoad(T_ppjlvalue, pgcstack, Align(sizeof(void*)), "task.gcstack"),
            builder.CreatePointerCast(
                    builder.CreateConstInBoundsGEP1_32(T_prjlvalue, gcframe, 1, "frame.prev"),
                    PointerType::get(T_ppjlvalue, 0)),
            Align(sizeof(void*)));
    inst->setMetadata(LLVMContext::MD_tbaa, tbaa_gcframe);
    builder.CreateAlignedStore(
            gcframe,
            builder.CreateBitCast(pgcstack, PointerType::get(PointerType::get(T_prjlvalue, 0), 0)),
            Align(sizeof(void*)));
    target->eraseFromParent();
}

void FinalLowerGC::lowerPopGCFrame(CallInst *target, Function &F)
{
    ++PopGCFrameCount;
    assert(target->arg_size() == 1);
    auto gcframe = target->getArgOperand(0);

    IRBuilder<> builder(target);
    Instruction *gcpop =
        cast<Instruction>(builder.CreateConstInBoundsGEP1_32(T_prjlvalue, gcframe, 1));
    Instruction *inst = builder.CreateAlignedLoad(T_prjlvalue, gcpop, Align(sizeof(void*)), "frame.prev");
    inst->setMetadata(LLVMContext::MD_tbaa, tbaa_gcframe);
    inst = builder.CreateAlignedStore(
        inst,
        builder.CreateBitCast(pgcstack,
            PointerType::get(T_prjlvalue, 0)),
        Align(sizeof(void*)));
    inst->setMetadata(LLVMContext::MD_tbaa, tbaa_gcframe);
    target->eraseFromParent();
}

void FinalLowerGC::lowerGetGCFrameSlot(CallInst *target, Function &F)
{
    ++GetGCFrameSlotCount;
    assert(target->arg_size() == 2);
    auto gcframe = target->getArgOperand(0);
    auto index = target->getArgOperand(1);

    // Initialize an IR builder.
    IRBuilder<> builder(target);

    // The first two slots are reserved, so we'll add two to the index.
    index = builder.CreateAdd(index, ConstantInt::get(Type::getInt32Ty(F.getContext()), 2));

    // Lower the intrinsic as a GEP.
    auto gep = builder.CreateInBoundsGEP(T_prjlvalue, gcframe, index);
    gep->takeName(target);
    target->replaceAllUsesWith(gep);
    target->eraseFromParent();
}

void FinalLowerGC::lowerQueueGCRoot(CallInst *target, Function &F)
{
    ++QueueGCRootCount;
    assert(target->arg_size() == 1);
    target->setCalledFunction(queueRootFunc);
}

void FinalLowerGC::lowerSafepoint(CallInst *target, Function &F)
{
    ++SafepointCount;
    assert(target->arg_size() == 1);
    IRBuilder<> builder(target);
    Value* signal_page = target->getOperand(0);
    builder.CreateLoad(T_size, signal_page, true);
    target->eraseFromParent();
}

void FinalLowerGC::lowerGCAllocBytes(CallInst *target, Function &F)
{
    ++GCAllocBytesCount;
    assert(target->arg_size() == 3);
    CallInst *newI;

    IRBuilder<> builder(target);
    auto ptls = target->getArgOperand(0);
    auto type = target->getArgOperand(2);
    uint64_t derefBytes = 0;
    if (auto CI = dyn_cast<ConstantInt>(target->getArgOperand(1))) {
        size_t sz = (size_t)CI->getZExtValue();
        // This is strongly architecture and OS dependent
        int osize;
        int offset = jl_gc_classify_pools(sz, &osize);
        if (offset < 0) {
            newI = builder.CreateCall(
                bigAllocFunc,
                { ptls, ConstantInt::get(T_size, sz + sizeof(void*)), type });
            if (sz > 0)
                derefBytes = sz;
        }
        else {
            auto pool_offs = ConstantInt::get(Type::getInt32Ty(F.getContext()), offset);
            auto pool_osize = ConstantInt::get(Type::getInt32Ty(F.getContext()), osize);
            newI = builder.CreateCall(poolAllocFunc, { ptls, pool_offs, pool_osize, type });
            if (sz > 0)
                derefBytes = sz;
        }
    } else {
        auto size = builder.CreateZExtOrTrunc(target->getArgOperand(1), T_size);
        // allocTypedFunc does not include the type tag in the allocation size!
        newI = builder.CreateCall(allocTypedFunc, { ptls, size, type });
        derefBytes = sizeof(void*);
    }
    newI->setAttributes(newI->getCalledFunction()->getAttributes());
    unsigned align = std::max((unsigned)target->getRetAlign().valueOrOne().value(), (unsigned)sizeof(void*));
    newI->addRetAttr(Attribute::getWithAlignment(F.getContext(), Align(align)));
    if (derefBytes > 0)
        newI->addDereferenceableRetAttr(derefBytes);
    newI->takeName(target);
    target->replaceAllUsesWith(newI);
    target->eraseFromParent();
}

bool FinalLowerGC::runOnFunction(Function &F)
{
    initAll(*F.getParent());
    if (!pgcstack_getter && !adoptthread_func) {
        LLVM_DEBUG(dbgs() << "FINAL GC LOWERING: Skipping function " << F.getName() << "\n");
        return false;
    }

    // Look for a call to 'julia.get_pgcstack'.
    pgcstack = getPGCstack(F);
    if (!pgcstack) {
        LLVM_DEBUG(dbgs() << "FINAL GC LOWERING: Skipping function " << F.getName() << " no pgcstack\n");
        return false;
    }
    LLVM_DEBUG(dbgs() << "FINAL GC LOWERING: Processing function " << F.getName() << "\n");
    queueRootFunc = getOrDeclare(jl_well_known::GCQueueRoot);
    poolAllocFunc = getOrDeclare(jl_well_known::GCPoolAlloc);
    bigAllocFunc = getOrDeclare(jl_well_known::GCBigAlloc);
    allocTypedFunc = getOrDeclare(jl_well_known::GCAllocTyped);
    T_size = F.getParent()->getDataLayout().getIntPtrType(F.getContext());

    // Lower all calls to supported intrinsics.
    for (auto &BB : F) {
        for (auto &I : make_early_inc_range(BB)) {
            auto *CI = dyn_cast<CallInst>(&I);
            if (!CI)
                continue;

            Value *callee = CI->getCalledOperand();
            assert(callee);

#define LOWER_INTRINSIC(INTRINSIC, LOWER_INTRINSIC_FUNC) \
            do { \
                auto intrinsic = getOrNull(jl_intrinsics::INTRINSIC); \
                if (intrinsic == callee) { \
                    LOWER_INTRINSIC_FUNC(CI, F); \
                } \
            } while (0)

            LOWER_INTRINSIC(newGCFrame, lowerNewGCFrame);
            LOWER_INTRINSIC(pushGCFrame, lowerPushGCFrame);
            LOWER_INTRINSIC(popGCFrame, lowerPopGCFrame);
            LOWER_INTRINSIC(getGCFrameSlot, lowerGetGCFrameSlot);
            LOWER_INTRINSIC(GCAllocBytes, lowerGCAllocBytes);
            LOWER_INTRINSIC(queueGCRoot, lowerQueueGCRoot);
            LOWER_INTRINSIC(safepoint, lowerSafepoint);

#undef LOWER_INTRINSIC
        }
    }

    return true;
}

PreservedAnalyses FinalLowerGCPass::run(Function &F, FunctionAnalysisManager &AM)
{
    if (FinalLowerGC().runOnFunction(F)) {
#ifdef JL_VERIFY_PASSES
        assert(!verifyLLVMIR(F));
#endif
        return PreservedAnalyses::allInSet<CFGAnalyses>();
    }
    return PreservedAnalyses::all();
}

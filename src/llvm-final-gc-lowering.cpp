// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-gc-interface-passes.h"

#define DEBUG_TYPE "final_gc_lowering"
STATISTIC(NewGCFrameCount, "Number of lowered newGCFrameFunc intrinsics");
STATISTIC(PushGCFrameCount, "Number of lowered pushGCFrameFunc intrinsics");
STATISTIC(PopGCFrameCount, "Number of lowered popGCFrameFunc intrinsics");
STATISTIC(GetGCFrameSlotCount, "Number of lowered getGCFrameSlotFunc intrinsics");
STATISTIC(GCAllocBytesCount, "Number of lowered GCAllocBytesFunc intrinsics");
STATISTIC(QueueGCRootCount, "Number of lowered queueGCRootFunc intrinsics");
STATISTIC(SafepointCount, "Number of lowered safepoint intrinsics");

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
    auto gcframe = cast<Instruction>(builder.CreateAddrSpaceCast(gcframe_alloca, PointerType::getUnqual(T_prjlvalue->getContext())));
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
                builder.CreateConstInBoundsGEP1_32(T_prjlvalue, gcframe, 0, "frame.nroots"),// GEP of 0 becomes a noop and eats the name
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
            pgcstack,
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
        pgcstack,
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
            newI = builder.CreateCall(smallAllocFunc, { ptls, pool_offs, pool_osize, type });
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

static bool hasUse(const JuliaPassContext &ctx, const jl_intrinsics::IntrinsicDescription &v)
{
    auto Intr = ctx.getOrNull(v);
    return Intr && !Intr->use_empty();
}

bool FinalLowerGC::shouldRunFinalGC()
{
    bool should_run = 0;
    should_run |= hasUse(*this, jl_intrinsics::newGCFrame);
    should_run |= hasUse(*this, jl_intrinsics::getGCFrameSlot);
    should_run |= hasUse(*this, jl_intrinsics::pushGCFrame);
    should_run |= hasUse(*this, jl_intrinsics::popGCFrame);
    should_run |= hasUse(*this, jl_intrinsics::GCAllocBytes);
    should_run |= hasUse(*this, jl_intrinsics::queueGCRoot);
    should_run |= hasUse(*this, jl_intrinsics::safepoint);
    return should_run;
}

bool FinalLowerGC::runOnFunction(Function &F)
{
    initAll(*F.getParent());
    pgcstack = getPGCstack(F);
    if (!pgcstack || !shouldRunFinalGC())
        goto verify_skip;

    LLVM_DEBUG(dbgs() << "FINAL GC LOWERING: Processing function " << F.getName() << "\n");
    queueRootFunc = getOrDeclare(jl_well_known::GCQueueRoot);
    smallAllocFunc = getOrDeclare(jl_well_known::GCSmallAlloc);
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
            LOWER_INTRINSIC(getGCFrameSlot, lowerGetGCFrameSlot);
            LOWER_INTRINSIC(pushGCFrame, lowerPushGCFrame);
            LOWER_INTRINSIC(popGCFrame, lowerPopGCFrame);
            LOWER_INTRINSIC(GCAllocBytes, lowerGCAllocBytes);
            LOWER_INTRINSIC(queueGCRoot, lowerQueueGCRoot);
            LOWER_INTRINSIC(safepoint, lowerSafepoint);

#undef LOWER_INTRINSIC
        }
    }
    return true;
    // Verify that skipping was in fact correct
    verify_skip:
    #ifdef JL_VERIFY_PASSES
        for (auto &BB : F) {
            for (auto &I : make_early_inc_range(BB)) {
                auto *CI = dyn_cast<CallInst>(&I);
                if (!CI)
                    continue;

            Value *callee = CI->getCalledOperand();
            assert(callee);
            auto IS_INTRINSIC = [&](auto intrinsic) {
                auto intrinsic2 = getOrNull(intrinsic);
                if (intrinsic2 == callee) {
                    errs() << "Final-GC-lowering didn't eliminate all intrinsics from '" << F.getName() << "', dumping entire module!\n\n";
                    errs() << *F.getParent() << "\n";
                    abort();
                }
            };
            IS_INTRINSIC(jl_intrinsics::newGCFrame);
            IS_INTRINSIC(jl_intrinsics::pushGCFrame);
            IS_INTRINSIC(jl_intrinsics::popGCFrame);
            IS_INTRINSIC(jl_intrinsics::getGCFrameSlot);
            IS_INTRINSIC(jl_intrinsics::GCAllocBytes);
            IS_INTRINSIC(jl_intrinsics::queueGCRoot);
            IS_INTRINSIC(jl_intrinsics::safepoint);
            }
        }
    #endif
    return false;
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

// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-gc-interface-passes.h"
#include "llvm/Support/CommandLine.h"

#define DEBUG_TYPE "final_gc_lowering"
STATISTIC(NewGCFrameCount, "Number of lowered newGCFrameFunc intrinsics");
STATISTIC(PushGCFrameCount, "Number of lowered pushGCFrameFunc intrinsics");
STATISTIC(PopGCFrameCount, "Number of lowered popGCFrameFunc intrinsics");
STATISTIC(GetGCFrameSlotCount, "Number of lowered getGCFrameSlotFunc intrinsics");
STATISTIC(QueueGCRootCount, "Number of lowered queueGCRootFunc intrinsics");
STATISTIC(SafepointCount, "Number of lowered safepoint intrinsics");

// Debugging aid for shadow-stack corruption (enable e.g. via
// JULIA_LLVM_ARGS="--julia-check-gc-frame-balance"): every push checks that it
// does not self-link the frame and every pop checks that the frame it unlinks
// is the current top of the shadow stack, so an unbalanced or misplaced
// push/pop faults deterministically at the offending site instead of leaving a
// dangling frame for a later collection to crash on.
static cl::opt<bool> ClCheckGCFrameBalance(
    "julia-check-gc-frame-balance", cl::init(false), cl::Hidden,
    cl::desc("Instrument GC frame pushes/pops with shadow-stack balance checks"));

// Fault at the current insertion point when Bad is true, without needing CFG
// surgery: volatile-store through a pointer that is null exactly when the
// check fails. On success this clobbers the frame's nroots word, which every
// caller either overwrites (push) or has retired (pop).
static void emitFrameBalanceCheck(IRBuilder<> &builder, Value *Bad, Value *gcframe, Type *T_size)
{
    Value *Null = ConstantPointerNull::get(cast<PointerType>(gcframe->getType()));
    Value *Addr = builder.CreateSelect(Bad, Null, gcframe, "gcframe.check");
    builder.CreateAlignedStore(ConstantInt::get(T_size, 0), Addr, Align(sizeof(void*)))
        ->setVolatile(true);
}

void FinalLowerGC::lowerNewGCFrame(CallInst *target, Function &F)
{
    ++NewGCFrameCount;
    assert(target->arg_size() == 1);
    unsigned nRoots = cast<ConstantInt>(target->getArgOperand(0))->getLimitedValue(INT_MAX);

    // Keep the backing allocation static even if the intrinsic was sunk.
    IRBuilder<> builder(target);
    IRBuilder<> entry_builder(&F.getEntryBlock(), F.getEntryBlock().begin());
    auto gcframe_alloca = entry_builder.CreateAlloca(T_prjlvalue, ConstantInt::get(Type::getInt32Ty(F.getContext()), nRoots + 2));
    gcframe_alloca->setAlignment(Align(16));
    // addrspacecast as needed for non-0 alloca addrspace
    auto gcframe = cast<Instruction>(builder.CreateAddrSpaceCast(gcframe_alloca, PointerType::getUnqual(T_prjlvalue->getContext())));
    gcframe->takeName(target);

    // For a sunk frame, start its lifetime at setup so unused paths can share
    // the stack storage. Entry-block frames span the whole function, where the
    // markers buy nothing; omitting them keeps that codegen unchanged.
    if (target->getParent() != &F.getEntryBlock())
        builder.CreateLifetimeStart(gcframe_alloca);
    auto ptrsize = F.getParent()->getDataLayout().getPointerSize();
    auto memset_instr = builder.CreateMemSet(gcframe, Constant::getNullValue(Type::getInt8Ty(F.getContext())), ptrsize * (nRoots + 2), Align(16));
    memset_instr->setMetadata(LLVMContext::MD_tbaa, tbaa_gcframe);

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
    if (ClCheckGCFrameBalance) {
        // A re-executed push would self-link the shadow stack.
        auto *PtrTy = PointerType::getUnqual(F.getContext());
        auto *top = builder.CreateAlignedLoad(PtrTy, pgcstack, Align(sizeof(void*)), "gcstack.top");
        Value *SelfLinked = builder.CreateICmpEQ(
                top, builder.CreatePointerCast(gcframe, PtrTy), "gcframe.selflinked");
        emitFrameBalanceCheck(builder, SelfLinked, gcframe, T_size);
    }
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
                    PointerType::get(T_ppjlvalue->getContext(), 0)),
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
    if (ClCheckGCFrameBalance) {
        // Popping anything but the current shadow-stack top would leave a
        // dangling frame behind.
        auto *PtrTy = PointerType::getUnqual(F.getContext());
        auto *top = builder.CreateAlignedLoad(PtrTy, pgcstack, Align(sizeof(void*)), "gcstack.top");
        Value *Mismatch = builder.CreateICmpNE(
                top, builder.CreatePointerCast(gcframe, PtrTy), "gcframe.mismatch");
        emitFrameBalanceCheck(builder, Mismatch, gcframe, T_size);
    }
    Instruction *gcpop =
        cast<Instruction>(builder.CreateConstInBoundsGEP1_32(T_prjlvalue, gcframe, 1));
    Instruction *inst = builder.CreateAlignedLoad(T_prjlvalue, gcpop, Align(sizeof(void*)), "frame.prev");
    inst->setMetadata(LLVMContext::MD_tbaa, tbaa_gcframe);
    inst = builder.CreateAlignedStore(
        inst,
        pgcstack,
        Align(sizeof(void*)));
    inst->setMetadata(LLVMContext::MD_tbaa, tbaa_gcframe);
    // End the lifetime after unlinking if this is a lowered frame alloca whose
    // lifetime was started at a sunk setup point.
    if (auto *AI = dyn_cast<AllocaInst>(gcframe->stripPointerCasts())) {
        bool HasLifetimeStart = llvm::any_of(AI->users(), [](User *U) {
            auto *II = dyn_cast<IntrinsicInst>(U);
            return II && II->getIntrinsicID() == Intrinsic::lifetime_start;
        });
        if (HasLifetimeStart)
            builder.CreateLifetimeEnd(AI);
    }
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
    // The site may execute with a reset region published (metadata inherited
    // from the write barrier CancellationLowering annotated).
    target->setCalledFunction(target->hasMetadata("julia.reset_region")
                                  ? queueRootResetSafeFunc
                                  : queueRootFunc);
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
    should_run |= (write_barrier_func && !write_barrier_func->use_empty());
    return should_run;
}

bool FinalLowerGC::runOnFunction(Function &F)
{
    initAll(*F.getParent());
    pgcstack = getPGCstack(F);

    auto gc_alloc_bytes = getOrNull(jl_intrinsics::GCAllocBytes);
    auto new_gc_frame = getOrNull(jl_intrinsics::newGCFrame);
    SmallVector<CallInst*, 0> write_barriers;
    SmallVector<CallInst*, 0> alloc_bytes;

    if (!pgcstack || !shouldRunFinalGC())
        goto verify_skip;

    LLVM_DEBUG(dbgs() << "FINAL GC LOWERING: Processing function " << F.getName() << "\n");
    queueRootFunc = getOrDeclare(jl_well_known::GCQueueRoot);
    smallAllocFunc = getOrDeclare(jl_well_known::GCSmallAlloc);
    bigAllocFunc = getOrDeclare(jl_well_known::GCBigAlloc);
    allocTypedFunc = getOrDeclare(jl_well_known::GCAllocTyped);
    queueRootResetSafeFunc = getOrDeclare(jl_well_known::GCQueueRootResetSafe);
    smallAllocResetSafeFunc = getOrDeclare(jl_well_known::GCSmallAllocResetSafe);
    bigAllocResetSafeFunc = getOrDeclare(jl_well_known::GCBigAllocResetSafe);
    allocTypedResetSafeFunc = getOrDeclare(jl_well_known::GCAllocTypedResetSafe);
    T_size = F.getParent()->getDataLayout().getIntPtrType(F.getContext());


    // The replacement for these may require creating new BasicBlocks
    // So we process them separately
    for (auto &BB : F) {
        for (auto it = BB.begin(); it != BB.end();) {
            auto *CI = dyn_cast<CallInst>(&*it);
            if (!CI) {
                ++it;
                continue;
            }
            Value *callee = CI->getCalledOperand();

            if (write_barrier_func && callee == write_barrier_func) {
                assert(CI->arg_size() >= 1);
                write_barriers.push_back(CI);
            }
            if (gc_alloc_bytes && callee == gc_alloc_bytes) {
                assert(CI->arg_size() >= 1);
                alloc_bytes.push_back(CI);
            }

            ++it;
        }
    }

    if (gc_alloc_bytes) {
        for (auto CI : alloc_bytes ) {
            auto newI = lowerGCAllocBytes(CI, F);
            if (newI != CI) {
                CI->replaceAllUsesWith(newI);
                CI->eraseFromParent();
                continue;
            }
        }
    }

    // Write barriers should always be processed beforehand
    // since they may insert julia.queue_gc_root intrinsics
    if(write_barrier_func) {
        for (auto CI : write_barriers) {
            lowerWriteBarrier(CI, F);
            CI->eraseFromParent();
        }
    }

    // Block layout may place a pop before its sunk allocation. Lower
    // allocations first so pop lowering can find the backing alloca.
    if (new_gc_frame) {
        for (auto &BB : F) {
            for (auto &I : make_early_inc_range(BB)) {
                auto *CI = dyn_cast<CallInst>(&I);
                if (CI && CI->getCalledOperand() == new_gc_frame)
                    lowerNewGCFrame(CI, F);
            }
        }
    }

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

            LOWER_INTRINSIC(getGCFrameSlot, lowerGetGCFrameSlot);
            LOWER_INTRINSIC(pushGCFrame, lowerPushGCFrame);
            LOWER_INTRINSIC(popGCFrame, lowerPopGCFrame);
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
            if (write_barrier_func == callee) {
                errs() << "Final-GC-lowering didn't eliminate all write barriers from '" << F.getName() << "', dumping entire module!\n\n";
                errs() << *F.getParent() << "\n";
                abort();
            }

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
        return PreservedAnalyses::none();
    }
    return PreservedAnalyses::all();
}

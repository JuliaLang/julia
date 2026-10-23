// This file is a part of Julia. License is MIT: https://julialang.org/license

// This pass lowers the julia.cancellation_point intrinsic to:
// 1. A stack buffer allocation for a jl_reset_ctx_t
// 2. Saves of the task's gcstack and eh into the buffer
// 3. A setjmp call on the buffer's mctx field
// 4. Assignment of the buffer address to task->reset_ctx (atomic release)
//
// It also walks the function to find the unsafe points reachable from a
// publication - every store, atomic, volatile load and call that is not the
// pass's own bookkeeping - and inserts reset_ctx = NULL before each of them,
// so that a delivered reset can only ever land in code that has done
// nothing yet: the region a point publishes ends at the first operation
// whose partial effects could be observed, and nothing but the next
// cancellation point re-opens one (delivery is level-triggered, so a request
// that finds no region is recovered at that next point). In particular a
// region never spans a call. Whatever a call binds to - the callee's body,
// the lazy symbol lookup or dispatch stub in front of it, a compilation, the
// allocator and the finalizers a collection runs - is outside the region,
// so none of it needs to be (or could be) made safe to abandon. Functions
// without cancellation points of their own are never instrumented, since no
// region can be open while they run.

#include "llvm-version.h"
#include "passes.h"

#include <llvm/ADT/SmallPtrSet.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/Debug.h>
#include <llvm/ADT/Statistic.h>
#include <llvm/TargetParser/Triple.h>

#include "llvm-codegen-shared.h"
#include "llvm-pass-helpers.h"
#include "julia.h"
#include "julia_internal.h"
#include "julia_threads.h"

#define DEBUG_TYPE "cancellation_lowering"

STATISTIC(CancellationPointsLowered, "Number of cancellation points lowered");
STATISTIC(ResetCtxClearsInserted, "Number of reset_ctx clears inserted");

using namespace llvm;

// Check if an instruction has the julia.reset_safe metadata
static bool hasResetSafeMetadata(Instruction *I) {
    return I->getMetadata("julia.reset_safe") != nullptr;
}

// Mark an instruction created by this pass as reset-safe, so the unsafe-point
// walk does not treat the pass's own reset_ctx bookkeeping as program state
// that a reset could tear.
static void setResetSafeMetadata(Instruction *I) {
    I->setMetadata("julia.reset_safe", MDNode::get(I->getContext(), {}));
}

namespace {

struct CancellationLowering : public JuliaPassContext {
    Value *pgcstack;
    Value *reset_ctx_ptr;  // Computed once in entry block, dominates all uses
    Value *ptls_field_ptr; // Pointer to task->ptls, computed alongside reset_ctx_ptr
    Value *eh_field_ptr;   // Pointer to task->eh, computed alongside reset_ctx_ptr

    CancellationLowering(Module &M) : pgcstack(nullptr), reset_ctx_ptr(nullptr), ptls_field_ptr(nullptr), eh_field_ptr(nullptr) {
        initFunctions(M);
    }

    bool runOnFunction(Function &F);

private:
    // Compute reset_ctx_ptr once in entry block
    // If insertAfter is provided, insert after that instruction
    // Otherwise insert at the beginning of the entry block (after allocas)
    void computeResetCtxPtr(Function &F, Instruction *insertAfter);
};

void CancellationLowering::computeResetCtxPtr(Function &F, Instruction *insertAfter) {
    if (!pgcstack)
        return;

    LLVMContext &LLVMCtx = F.getContext();
    Type *I8Ty = Type::getInt8Ty(LLVMCtx);
    Type *I64Ty = Type::getInt64Ty(LLVMCtx);

    IRBuilder<> Builder(LLVMCtx);
    if (insertAfter) {
        // Insert right after pgcstack call
        Builder.SetInsertPoint(insertAfter->getNextNode());
    } else {
        // pgcstack is an argument, insert at the start of entry block
        // but after any allocas
        BasicBlock &entry = F.getEntryBlock();
        BasicBlock::iterator insertPt = entry.begin();
        while (insertPt != entry.end() && isa<AllocaInst>(&*insertPt)) {
            ++insertPt;
        }
        Builder.SetInsertPoint(&entry, insertPt);
    }

    // Get the offset of gcstack in jl_task_t
    size_t gcstack_offset = offsetof(jl_task_t, gcstack);

    Value *task_ptr = Builder.CreateGEP(I8Ty, pgcstack,
                                        ConstantInt::get(I64Ty, -(int64_t)gcstack_offset),
                                        "current_task");

    // Get pointer to reset_ctx field in current task
    size_t reset_ctx_offset = offsetof(jl_task_t, reset_ctx);

    reset_ctx_ptr = Builder.CreateGEP(I8Ty, task_ptr,
                                       ConstantInt::get(I64Ty, reset_ctx_offset),
                                       "reset_ctx_ptr");

    ptls_field_ptr = Builder.CreateGEP(I8Ty, task_ptr,
                                       ConstantInt::get(I64Ty, offsetof(jl_task_t, ptls)),
                                       "ptls_field_ptr");

    eh_field_ptr = Builder.CreateGEP(I8Ty, task_ptr,
                                     ConstantInt::get(I64Ty, offsetof(jl_task_t, eh)),
                                     "eh_field_ptr");
}

bool CancellationLowering::runOnFunction(Function &F) {
    bool Changed = false;

    // Find pgcstack - either as a call to julia.get_pgcstack or as an argument with "gcstack" attribute
    pgcstack = nullptr;
    reset_ctx_ptr = nullptr;
    eh_field_ptr = nullptr;
    Instruction *pgcstack_inst = nullptr;  // Only set if pgcstack is from a call, not an argument
    if (pgcstack_getter || adoptthread_func) {
        for (auto &I : F.getEntryBlock()) {
            if (CallInst *callInst = dyn_cast<CallInst>(&I)) {
                Value *callee = callInst->getCalledOperand();
                if ((pgcstack_getter && callee == pgcstack_getter) ||
                    (adoptthread_func && callee == adoptthread_func)) {
                    pgcstack = callInst;
                    pgcstack_inst = callInst;
                    break;
                }
            }
        }
    }
    // If not found via call, check for argument with "gcstack" attribute
    if (!pgcstack) {
        for (auto &arg : F.args()) {
            AttributeSet attrs = F.getAttributes().getParamAttrs(arg.getArgNo());
            if (attrs.hasAttribute("gcstack")) {
                pgcstack = &arg;
                break;
            }
        }
    }

    // First, find all cancellation_point intrinsics (walking the users of
    // the declaration rather than every instruction of the function)
    SmallVector<CallInst*, 4> CancellationPoints;

    if (cancel_point_func) {
        for (User *U : cancel_point_func->users()) {
            if (auto *CI = dyn_cast<CallInst>(U)) {
                if (CI->getFunction() == &F &&
                    CI->getCalledOperand() == cancel_point_func)
                    CancellationPoints.push_back(CI);
            }
        }
    }

    // A function without cancellation points of its own can never run inside
    // an open region (a region never spans a call) and needs no
    // instrumentation.
    if (CancellationPoints.empty())
        return false;

    // Sanitizer instrumentation runs AFTER this pass (see addSanitizerPasses
    // in pipeline.cpp) and rewrites stores into sanitizer runtime calls -
    // including this pass's own reset_ctx bookkeeping - inside code already
    // classified as reset-safe. A delivered reset could then abandon a
    // sanitizer runtime frame, whose bookkeeping cannot be repaired since
    // Julia bypasses the sanitizers' longjmp interceptors (see jl_longjmp in
    // julia.h). Under TSan, do not establish reset regions at all: lower
    // cancellation points to a plain safepoint + status check (delivery
    // degrades to level-triggered polling).
    bool no_publish = F.hasFnAttribute(Attribute::SanitizeThread);

    // Compute reset_ctx_ptr once in entry block (dominates all uses)
    // pgcstack_inst is set when pgcstack comes from a call; null when it's an argument
    computeResetCtxPtr(F, pgcstack_inst);

    // Lower each cancellation point, remembering each region's publication
    // for the reachability computation below. All points of a function share
    // one region buffer: regions never nest within a frame - each point's
    // publication supersedes the previous one, and "the region" is always
    // "since the most recently crossed point" - so the buffer identifies the
    // frame, not the point.
    SmallVector<Instruction*, 4> PointPublishes;
    AllocaInst *UContextBuf = nullptr;
    for (CallInst *CI : CancellationPoints) {
        ++CancellationPointsLowered;
        Changed = true;

        IRBuilder<> Builder(CI);
        LLVMContext &LLVMCtx = F.getContext();
        Type *I8Ty = Type::getInt8Ty(LLVMCtx);
        Type *I32Ty = Type::getInt32Ty(LLVMCtx);
        Type *I64Ty = Type::getInt64Ty(LLVMCtx);
        Type *PtrTy = PointerType::getUnqual(LLVMCtx);

        // Codegen materializes the current task before emitting the marker,
        // so a function containing a cancellation point always has a
        // pgcstack; silently folding the point away would swallow
        // cancellation.
        assert(reset_ctx_ptr && "julia.cancellation_point in a function without pgcstack");

        if (no_publish) {
            // A cancellation point remains a GC safepoint and a poll even
            // without a reset region.
            Value *ptls = Builder.CreateAlignedLoad(PtrTy, ptls_field_ptr, Align(sizeof(void*)), "ptls");
            Type *T_size = F.getParent()->getDataLayout().getIntPtrType(LLVMCtx);
            emit_gc_safepoint(Builder, T_size, ptls, nullptr, false);
            CI->replaceAllUsesWith(ConstantInt::get(I32Ty, 0));
            CI->eraseFromParent();
            continue;
        }

        // Allocate one jl_reset_ctx_t on the stack, shared by every point
        // of this function (see above).
        if (!UContextBuf) {
            const size_t UContextSize = sizeof(jl_reset_ctx_t);
            const size_t UContextAlign = alignof(jl_reset_ctx_t);
            // Create the alloca at the start of the function
            IRBuilder<> AllocaBuilder(&F.getEntryBlock().front());
            Type *UContextTy = ArrayType::get(I8Ty, UContextSize);
            UContextBuf = AllocaBuilder.CreateAlloca(UContextTy, nullptr, "cancel_ucontext");
            UContextBuf->setAlignment(Align(UContextAlign));
        }

        // If this frame's region is still published, the whole establishment
        // (and its safepoint) is skipped: the buffer already holds a valid
        // setjmp of an earlier point of this frame, and landing there is
        // equivalent to landing here. For a cancellation delivery the
        // landing throws either way; for a preempt delivery the landing
        // resumes at the earlier point and re-executes forward - sound
        // because everything a surviving region spans is re-executable by
        // construction (that is what keeps a region open), the same
        // re-execution any preempt reset performs, just from further back.
        // The equivalence also covers the governing token: codegen emits
        // the point's binding bookkeeping before the marker, and its rebind
        // stores are untagged - unsafe points whose pass-inserted clear
        // tears the region - so a region that is still live here was
        // established under the very token this point just verified as
        // bound (exception handlers restore the pair together, and finalizers
        // only run with the region unpublished).
        // The load is
        // exact ground truth - the region survived if and only if no unsafe
        // point (whose pass-inserted clear nulls reset_ctx) ran since the
        // establishment - so this is robust against any CFG shape the
        // optimizer produced, and re-establishes precisely when needed
        // (function entry, after a region-tearing operation, after the
        // slow path's handle_cancellation! call). No consumption race: the
        // published context is only ever consumed by same-thread signal
        // delivery (control never returns here) or with this thread frozen.
        // Skipping the safepoint means a pure-compute polling loop only
        // polls at its establishing crossing - the historical behavior of
        // uninstrumented pure loops (which never poll); any loop doing real
        // work still polls through its allocations, or re-establishes (with
        // the safepoint) after the calls that tore its region - and unlike
        // an uninstrumented spin, this one stays cancellable, which is also
        // the escape hatch for a stop-the-world it delays.
        BasicBlock *ContBB = CI->getParent()->splitBasicBlock(CI, "cancel_pt_cont");
        BasicBlock *CheckBB = ContBB->getSinglePredecessor();
        BasicBlock *EstablishBB = BasicBlock::Create(LLVMCtx, "cancel_establish", &F, ContBB);
        CheckBB->getTerminator()->eraseFromParent();
        Builder.SetInsertPoint(CheckBB);
        LoadInst *prev = Builder.CreateAlignedLoad(PtrTy, reset_ctx_ptr, Align(sizeof(void*)), /*isVolatile*/true, "reset_ctx_prev");
        prev->setOrdering(AtomicOrdering::Monotonic);
        setResetSafeMetadata(prev);
        Value *already = Builder.CreateICmpEQ(prev, UContextBuf, "region_live");
        Builder.CreateCondBr(already, ContBB, EstablishBB);
        Builder.SetInsertPoint(EstablishBB);

        // N.B.: The buffer may only be published in `reset_ctx` while its
        // contents are valid; a cancellation signal arriving in between would
        // otherwise longjmp into garbage. Since `setjmp` below (re-)writes the
        // buffer (cancellation points in loops re-execute it), unpublish the
        // buffer first, then write it, then publish it. All reset_ctx stores
        // and the buffer writes are volatile: delivery observes them
        // asynchronously (a signal on the same thread, or a suspender), so
        // they must be emitted exactly where placed. The single-thread
        // (signal) fence keeps the buffer writes from being reordered above
        // the unpublish by any later pass.
        Value *null_ptr0 = ConstantPointerNull::get(cast<PointerType>(PtrTy));
        StoreInst *unpub = Builder.CreateAlignedStore(null_ptr0, reset_ctx_ptr, Align(sizeof(void*)), /*isVolatile*/true);
        unpub->setOrdering(AtomicOrdering::Release);
        setResetSafeMetadata(unpub);
        Builder.CreateFence(AtomicOrdering::SequentiallyConsistent, SyncScope::SingleThread);

        // An establishing cancellation point is also a GC safepoint: it sits
        // on every entry into region-covered code (and on the re-arm path
        // after anything tore the region), so cancellation-instrumented code
        // cannot starve a stop-the-world request any worse than the same
        // code without instrumentation. This must be emitted while reset_ctx
        // is unpublished, so that a concurrently delivered cancellation
        // signal cannot longjmp us out of the safepoint wait.
        Value *ptls = Builder.CreateAlignedLoad(PtrTy, ptls_field_ptr, Align(sizeof(void*)), "ptls");
        Type *T_size = F.getParent()->getDataLayout().getIntPtrType(LLVMCtx);
        emit_gc_safepoint(Builder, T_size, ptls, nullptr, false);

        // Stamp the buffer's discriminator: a nonzero `sp` (the buffer's own
        // frame address) marks it as a reset-flavor context (see
        // jl_reset_ctx_t). Atomic monotonic since the delivery machinery
        // reads it from another thread.
        Value *sp_val = Builder.CreatePtrToInt(UContextBuf, T_size);
        StoreInst *sp_store = Builder.CreateAlignedStore(sp_val, UContextBuf, Align(alignof(jl_reset_ctx_t)), /*isVolatile*/true);
        sp_store->setOrdering(AtomicOrdering::Monotonic);
        setResetSafeMetadata(sp_store);

        // Record the task's GC-frame chain head and innermost exception
        // handler at establishment, which the delivery restores before the
        // longjmp (see jl_reset_ctx_t). (pgcstack points directly at the
        // task's gcstack field.)
        Value *gcstack_val = Builder.CreateAlignedLoad(PtrTy, pgcstack, Align(sizeof(void*)), "saved_gcstack");
        Value *GCStackSlot = Builder.CreateGEP(I8Ty, UContextBuf,
                                               ConstantInt::get(I64Ty, offsetof(jl_reset_ctx_t, gcstack)),
                                               "cancel_gcstack_slot");
        StoreInst *gcstack_store = Builder.CreateAlignedStore(gcstack_val, GCStackSlot, Align(sizeof(void*)), /*isVolatile*/true);
        setResetSafeMetadata(gcstack_store);
        Value *eh_val = Builder.CreateAlignedLoad(PtrTy, eh_field_ptr, Align(sizeof(void*)), "saved_eh");
        Value *EhSlot = Builder.CreateGEP(I8Ty, UContextBuf,
                                          ConstantInt::get(I64Ty, offsetof(jl_reset_ctx_t, eh)),
                                          "cancel_eh_slot");
        StoreInst *eh_store = Builder.CreateAlignedStore(eh_val, EhSlot, Align(sizeof(void*)), /*isVolatile*/true);
        setResetSafeMetadata(eh_store);

        // Call setjmp on the mctx field.
        // Use the platform-specific setjmp function name defined in julia.h
        Value *MCtxPtr = Builder.CreateGEP(I8Ty, UContextBuf,
                                           ConstantInt::get(I64Ty, offsetof(jl_reset_ctx_t, mctx)),
                                           "cancel_mctx");
        // Match the target-specific signature codegen uses for jl_setjmp
        // (see setjmp_func in codegen.cpp): Windows' _setjmp variant takes
        // only the buffer argument.
        const Triple TT(F.getParent()->getTargetTriple());
        FunctionType *SetjmpTy;
        SmallVector<Value*, 2> SetjmpArgs;
        if (TT.isOSWindows()) {
            SetjmpTy = FunctionType::get(I32Ty, {PtrTy}, false);
            SetjmpArgs.push_back(MCtxPtr);
        }
        else {
            SetjmpTy = FunctionType::get(I32Ty, {PtrTy, I32Ty}, false);
            SetjmpArgs.push_back(MCtxPtr);
            SetjmpArgs.push_back(ConstantInt::get(I32Ty, 0));
        }
        FunctionCallee SetjmpFn = F.getParent()->getOrInsertFunction(jl_setjmp_name, SetjmpTy);

        CallInst *SetjmpCall = Builder.CreateCall(SetjmpFn, SetjmpArgs);
        SetjmpCall->addFnAttr(Attribute::ReturnsTwice);

        // Publish the now-valid buffer address to reset_ctx (release, so that
        // the buffer contents are visible before the pointer; the setjmp call
        // itself keeps the store from moving up).
        StoreInst *store = Builder.CreateAlignedStore(UContextBuf, reset_ctx_ptr, Align(sizeof(void*)), /*isVolatile*/true);
        store->setOrdering(AtomicOrdering::Release);
        setResetSafeMetadata(store);
        PointPublishes.push_back(store);
        Builder.CreateBr(ContBB);

        // Replace uses and remove the intrinsic. (The marker's value is
        // unused by codegen; keep a PHI for IR sanity should that change.)
        if (!CI->use_empty()) {
            IRBuilder<> PhiBuilder(&ContBB->front());
            PHINode *phi = PhiBuilder.CreatePHI(I32Ty, 2);
            phi->addIncoming(ConstantInt::get(I32Ty, 0), CheckBB);
            phi->addIncoming(SetjmpCall, EstablishBB);
            CI->replaceAllUsesWith(phi);
        }
        CI->eraseFromParent();
    }

    // When regions are not published (sanitizer builds), there is nothing
    // to maintain either.
    if (no_publish)
        return Changed;

    // A region can only be open at an instruction reachable from one of the
    // publications. Instructions outside that set need neither clears nor
    // teardown.
    SmallPtrSet<BasicBlock*, 16> MayBeOpenAtEntry;
    {
        SmallVector<BasicBlock*, 8> Worklist;
        for (Instruction *Pub : PointPublishes)
            for (BasicBlock *Succ : successors(Pub->getParent()))
                Worklist.push_back(Succ);
        while (!Worklist.empty()) {
            BasicBlock *B = Worklist.pop_back_val();
            if (MayBeOpenAtEntry.insert(B).second)
                for (BasicBlock *Succ : successors(B))
                    Worklist.push_back(Succ);
        }
    }
    auto blockMayOpenAtEntry = [&](BasicBlock *B) {
        return MayBeOpenAtEntry.count(B) != 0;
    };
    auto isPublishStore = [&](Instruction *I) {
        auto *SI = dyn_cast<StoreInst>(I);
        return SI && SI->getPointerOperand() == reset_ctx_ptr &&
               !isa<ConstantPointerNull>(SI->getValueOperand());
    };

    // Now walk the function to find unsafe points and insert reset_ctx =
    // NULL before them: every store, atomic, volatile load and call that is
    // not the pass's own bookkeeping (which carries the julia.reset_safe
    // metadata) is one.
    SmallVector<Instruction*, 16> UnsafePoints;

    // We need to skip instructions that are part of our setup (pgcstack, task, reset_ctx_ptr)
    // since they occur before reset_ctx_ptr is available
    Instruction *reset_ctx_ptr_inst = dyn_cast_or_null<Instruction>(reset_ctx_ptr);
    Instruction *setup_sentinel = reset_ctx_ptr_inst ? reset_ctx_ptr_inst : pgcstack_inst;

    for (auto &BB : F) {
        bool past_setup = (&BB != &F.getEntryBlock()) || setup_sentinel == nullptr;
        bool may_open = blockMayOpenAtEntry(&BB);
        for (auto &I : BB) {
            // In the entry block, skip instructions until after the setup point
            if (!past_setup) {
                if (&I == setup_sentinel) {
                    past_setup = true;
                }
                continue;
            }
            if (isPublishStore(&I)) {
                may_open = true;
                continue;
            }
            if (!may_open)
                continue;

            // Check for stores (the reset_ctx stores we just created carry
            // the reset_safe metadata). Atomic stores are unsafe points like
            // plain ones: a reset delivered around e.g. a lock-release store
            // would leave the lock owned forever.
            if (auto *SI = dyn_cast<StoreInst>(&I)) {
                if (!hasResetSafeMetadata(SI))
                    UnsafePoints.push_back(SI);
            }
            // Atomic read-modify-write operations publish state a reset
            // could tear (e.g. a cmpxchg acquiring a user spin lock).
            else if (isa<AtomicCmpXchgInst>(&I) || isa<AtomicRMWInst>(&I)) {
                if (!hasResetSafeMetadata(&I))
                    UnsafePoints.push_back(&I);
            }
            // Volatile loads can have side effects (MMIO, read-to-clear
            // hardware registers): a reset delivered after such a load has
            // taken effect would silently abandon it. The pass's own
            // bookkeeping loads are non-volatile and thus exempt.
            else if (auto *LI = dyn_cast<LoadInst>(&I)) {
                if (LI->isVolatile() && !hasResetSafeMetadata(LI))
                    UnsafePoints.push_back(LI);
            }
            // Every call is an unsafe point, whatever its callee claims
            // about itself: the call may bind to a lazy symbol-lookup or
            // dispatch stub, compile the callee, allocate, or run
            // finalizers, none of which can be abandoned by a reset. Only a
            // few intrinsics that neither observe nor publish any state are
            // exempt.
            else if (auto *CI = dyn_cast<CallInst>(&I)) {
                if (isa<DbgInfoIntrinsic>(CI))
                    continue;
                if (CI->isLifetimeStartOrEnd())
                    continue;
                Function *Callee = CI->getCalledFunction();
                if (Callee && Callee->isIntrinsic()) {
                    Intrinsic::ID ID = Callee->getIntrinsicID();
                    if (ID == Intrinsic::lifetime_start ||
                        ID == Intrinsic::lifetime_end ||
                        ID == Intrinsic::dbg_declare ||
                        ID == Intrinsic::dbg_value ||
                        ID == Intrinsic::dbg_label ||
                        ID == Intrinsic::assume ||
                        ID == Intrinsic::expect ||
                        ID == Intrinsic::prefetch) {
                        continue;
                    }
                }
                // Skip the setjmp and safepoint calls we just created
                if (Callee && (Callee->getName() == jl_setjmp_name ||
                               Callee->getName() == "julia.safepoint"))
                    continue;
                // Known-safe julia runtime intrinsics: pure address
                // computations that neither observe nor publish state a
                // reset could tear (ccall argument-conversion glue, e.g.
                // unsafe_convert of a mutable object).
                if (Callee && (Callee == pointer_from_objref_func || Callee == gc_loaded_func))
                    continue;
                UnsafePoints.push_back(CI);
            }
        }
    }

    // Insert reset_ctx = NULL before each unsafe point. The clear is
    // volatile and separated from the unsafe operation by a single-thread
    // (signal) fence: no later pass may move the operation above the clear -
    // a delivery observing the still-published region after the operation
    // executed would longjmp over its effects.
    for (Instruction *I : UnsafePoints) {
        ++ResetCtxClearsInserted;
        Changed = true;

        IRBuilder<> Builder(I);
        LLVMContext &LLVMCtx = F.getContext();
        Type *PtrTy = PointerType::getUnqual(LLVMCtx);

        Value *null_ptr = ConstantPointerNull::get(cast<PointerType>(PtrTy));
        StoreInst *store = Builder.CreateAlignedStore(null_ptr, reset_ctx_ptr, Align(sizeof(void*)), /*isVolatile*/true);
        store->setOrdering(AtomicOrdering::Release);
        setResetSafeMetadata(store);
        Builder.CreateFence(AtomicOrdering::SequentiallyConsistent, SyncScope::SingleThread);
    }

    // Insert reset_ctx = NULL before all return instructions
    // This is necessary because the cancel_ucontext buffer is stack-allocated,
    // and becomes invalid when the function returns.
    {
        LLVMContext &LLVMCtx = F.getContext();
        Type *PtrTy = PointerType::getUnqual(LLVMCtx);
        Value *null_ptr = ConstantPointerNull::get(cast<PointerType>(PtrTy));

        for (auto &BB : F) {
            auto *RI = dyn_cast<ReturnInst>(BB.getTerminator());
            if (!RI)
                continue;
            // No teardown where no region can be open at the return
            bool may_open = blockMayOpenAtEntry(&BB);
            if (!may_open) {
                for (auto &I : BB) {
                    if (isPublishStore(&I)) {
                        may_open = true;
                        break;
                    }
                }
            }
            if (!may_open)
                continue;
            // LLVM requires a musttail call to be immediately followed
            // (modulo an optional bitcast) by the return, so no teardown may
            // be inserted between them: it goes in front of the call
            // instead. This is done unconditionally - metadata is not a
            // reliable proxy for whether the unsafe-point walk already
            // cleared before the call (exempt calls, e.g.
            // julia.pointer_from_objref, get no clear), and a duplicate
            // clear is harmless.
            Instruction *InsertPt = RI;
            if (CallInst *MTC = BB.getTerminatingMustTailCall())
                InsertPt = MTC;
            IRBuilder<> Builder(InsertPt);
            StoreInst *store = Builder.CreateAlignedStore(null_ptr, reset_ctx_ptr, Align(sizeof(void*)), /*isVolatile*/true);
            store->setOrdering(AtomicOrdering::Release);
            setResetSafeMetadata(store);
            ++ResetCtxClearsInserted;
        }
    }

    return Changed;
}

} // anonymous namespace

PreservedAnalyses CancellationLoweringPass::run(Function &F, FunctionAnalysisManager &AM) {
    CancellationLowering CL(*F.getParent());
    if (CL.runOnFunction(F)) {
#ifdef JL_VERIFY_PASSES
        assert(!verifyLLVMIR(F));
#endif
        return PreservedAnalyses::allInSet<CFGAnalyses>();
    }
    return PreservedAnalyses::all();
}

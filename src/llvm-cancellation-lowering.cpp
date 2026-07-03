// This file is a part of Julia. License is MIT: https://julialang.org/license

// This pass lowers the julia.cancellation_point intrinsic to:
// 1. A stack buffer allocation for the jl_ucontext_t
// 2. A setjmp call on the uc_mcontext field
// 3. Assignment of the buffer address to task->reset_ctx (atomic release)
//
// It also walks the function to find stores/calls without julia.reset_safe
// metadata and inserts reset_ctx = NULL before them.

#include "llvm-version.h"
#include "passes.h"

#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/Debug.h>
#include <llvm/ADT/Statistic.h>

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

struct CancellationLowering {
    Function *cancel_point_func;
    Value *pgcstack;
    Value *reset_ctx_ptr;  // Computed once in entry block, dominates all uses

    CancellationLowering(Module &M) : cancel_point_func(nullptr), pgcstack(nullptr), reset_ctx_ptr(nullptr) {
        cancel_point_func = M.getFunction("julia.cancellation_point");
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
}

bool CancellationLowering::runOnFunction(Function &F) {
    // Skip if there's no cancellation_point function in the module
    if (!cancel_point_func)
        return false;

    bool Changed = false;

    // Find pgcstack - either as a call to julia.get_pgcstack or as an argument with "gcstack" attribute
    pgcstack = nullptr;
    reset_ctx_ptr = nullptr;
    Instruction *pgcstack_inst = nullptr;  // Only set if pgcstack is from a call, not an argument
    Function *pgcstack_getter = F.getParent()->getFunction("julia.get_pgcstack");
    Function *adoptthread_func = F.getParent()->getFunction("julia.get_pgcstack_or_new");
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

    // First, find all cancellation_point intrinsics
    SmallVector<CallInst*, 4> CancellationPoints;

    for (auto &BB : F) {
        for (auto &I : BB) {
            if (auto *CI = dyn_cast<CallInst>(&I)) {
                Value *callee = CI->getCalledOperand();
                if (callee && callee == cancel_point_func) {
                    CancellationPoints.push_back(CI);
                }
            }
        }
    }

    if (CancellationPoints.empty()) {
        return false;
    }

    // Compute reset_ctx_ptr once in entry block (dominates all uses)
    // pgcstack_inst is set when pgcstack comes from a call; null when it's an argument
    computeResetCtxPtr(F, pgcstack_inst);

    // Lower each cancellation point
    for (CallInst *CI : CancellationPoints) {
        ++CancellationPointsLowered;
        Changed = true;

        IRBuilder<> Builder(CI);
        LLVMContext &LLVMCtx = F.getContext();
        Type *I8Ty = Type::getInt8Ty(LLVMCtx);
        Type *I32Ty = Type::getInt32Ty(LLVMCtx);
        Type *PtrTy = PointerType::getUnqual(LLVMCtx);

        if (!reset_ctx_ptr) {
            // Can't lower without access to the task, just remove the intrinsic
            CI->replaceAllUsesWith(ConstantInt::get(I32Ty, 0));
            CI->eraseFromParent();
            continue;
        }

        // Allocate a _jl_ucontext_t on the stack
        const size_t UContextSize = sizeof(_jl_ucontext_t);
        const size_t UContextAlign = alignof(_jl_ucontext_t);

        // Create the alloca at the start of the function
        IRBuilder<> AllocaBuilder(&F.getEntryBlock().front());
        Type *UContextTy = ArrayType::get(I8Ty, UContextSize);
        AllocaInst *UContextBuf = AllocaBuilder.CreateAlloca(UContextTy, nullptr, "cancel_ucontext");
        UContextBuf->setAlignment(Align(UContextAlign));

        // Store the ucontext address to reset_ctx with atomic release ordering
        StoreInst *store = Builder.CreateAlignedStore(UContextBuf, reset_ctx_ptr, Align(sizeof(void*)));
        store->setOrdering(AtomicOrdering::Release);

        // Call setjmp on the uc_mcontext field (which is at offset 0 of the struct)
        // Use the platform-specific setjmp function name defined in julia.h
        FunctionType *SetjmpTy = FunctionType::get(I32Ty, {PtrTy}, false);
        FunctionCallee SetjmpFn = F.getParent()->getOrInsertFunction(jl_setjmp_name, SetjmpTy);

        CallInst *SetjmpCall = Builder.CreateCall(SetjmpFn, {UContextBuf});
        SetjmpCall->addFnAttr(Attribute::ReturnsTwice);

        // Replace uses and remove the intrinsic
        CI->replaceAllUsesWith(SetjmpCall);
        CI->eraseFromParent();
    }

    // Now walk the function to find stores/calls without reset_safe metadata
    // and insert reset_ctx = NULL before them
    SmallVector<Instruction*, 16> UnsafePoints;

    // We need to skip instructions that are part of our setup (pgcstack, task, reset_ctx_ptr)
    // since they occur before reset_ctx_ptr is available
    Instruction *reset_ctx_ptr_inst = dyn_cast_or_null<Instruction>(reset_ctx_ptr);

    for (auto &BB : F) {
        bool past_setup = (&BB != &F.getEntryBlock());
        for (auto &I : BB) {
            // In the entry block, skip instructions until after reset_ctx_ptr is defined
            if (!past_setup) {
                if (&I == reset_ctx_ptr_inst) {
                    past_setup = true;
                }
                continue;
            }

            // Check for stores (but skip the reset_ctx stores we just created)
            if (auto *SI = dyn_cast<StoreInst>(&I)) {
                if (!hasResetSafeMetadata(SI)) {
                    // Skip atomic stores (including reset_ctx stores we just created)
                    if (SI->isAtomic())
                        continue;
                    UnsafePoints.push_back(SI);
                }
            }
            // Check for calls (but not debug intrinsics, lifetime markers, etc.)
            else if (auto *CI = dyn_cast<CallInst>(&I)) {
                // Skip debug intrinsics and other harmless intrinsics
                if (isa<DbgInfoIntrinsic>(CI))
                    continue;
                if (CI->isLifetimeStartOrEnd())
                    continue;

                // Check for reset_safe metadata
                if (!hasResetSafeMetadata(CI)) {
                    // Also skip intrinsic calls that are known safe
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
                    // Skip the setjmp calls we just created
                    if (Callee && Callee->getName() == jl_setjmp_name)
                        continue;
                    UnsafePoints.push_back(CI);
                }
            }
        }
    }

    // Insert reset_ctx = NULL before each unsafe point
    for (Instruction *I : UnsafePoints) {
        if (!reset_ctx_ptr)
            continue;

        ++ResetCtxClearsInserted;
        Changed = true;

        IRBuilder<> Builder(I);
        LLVMContext &LLVMCtx = F.getContext();
        Type *PtrTy = PointerType::getUnqual(LLVMCtx);

        // Store NULL to reset_ctx with atomic release ordering
        Value *null_ptr = ConstantPointerNull::get(cast<PointerType>(PtrTy));
        StoreInst *store = Builder.CreateAlignedStore(null_ptr, reset_ctx_ptr, Align(sizeof(void*)));
        store->setOrdering(AtomicOrdering::Release);
    }

    // Insert reset_ctx = NULL before all return instructions
    // This is necessary because the cancel_ucontext buffer is stack-allocated,
    // and becomes invalid when the function returns
    if (reset_ctx_ptr) {
        LLVMContext &LLVMCtx = F.getContext();
        Type *PtrTy = PointerType::getUnqual(LLVMCtx);
        Value *null_ptr = ConstantPointerNull::get(cast<PointerType>(PtrTy));

        for (auto &BB : F) {
            if (auto *RI = dyn_cast<ReturnInst>(BB.getTerminator())) {
                IRBuilder<> Builder(RI);
                StoreInst *store = Builder.CreateAlignedStore(null_ptr, reset_ctx_ptr, Align(sizeof(void*)));
                store->setOrdering(AtomicOrdering::Release);
                ++ResetCtxClearsInserted;
            }
        }
    }

    return Changed;
}

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

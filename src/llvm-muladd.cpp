// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-version.h"
#include "passes.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

#include <llvm/ADT/Statistic.h>
#include <llvm/Analysis/OptimizationRemarkEmitter.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Operator.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Pass.h>
#include <llvm/Support/Debug.h>

#include "julia.h"
#include "julia_assert.h"

#define DEBUG_TYPE "combine-muladd"
#undef DEBUG

using namespace llvm;
STATISTIC(TotalContracted, "Total number of multiplies marked for FMA");

#ifndef __clang_gcanalyzer__
#define REMARK(remark) ORE.emit(remark)
#else
#define REMARK(remark) (void) 0;
#endif

/**
 * Combine
 * ```
 * %v0 = fmul ... %a, %b
 * %v = fadd fast ... %v0, %c
 * ```
 * to
 * `%v = call fast @llvm.fmuladd.<...>(... %a, ... %b, ... %c)`
 * when `%v0` has no other use
 */

// Return true if we changed the mulOp
static bool checkCombine(Value *maybeMul, OptimizationRemarkEmitter &ORE) JL_NOTSAFEPOINT
{
    auto mulOp = dyn_cast<Instruction>(maybeMul);
    if (!mulOp || mulOp->getOpcode() != Instruction::FMul)
        return false;
    if (!mulOp->hasOneUse()) {
        LLVM_DEBUG(dbgs() << "mulOp has multiple uses: " << *maybeMul << "\n");
        REMARK([&](){
            return OptimizationRemarkMissed(DEBUG_TYPE, "Multiuse FMul", mulOp)
                << "fmul had multiple uses " << ore::NV("fmul", mulOp);
        });
        return false;
    }
    // On 5.0+ we only need to mark the mulOp as contract and the backend will do the work for us.
    auto fmf = mulOp->getFastMathFlags();
    if (!fmf.allowContract()) {
        LLVM_DEBUG(dbgs() << "Marking mulOp for FMA: " << *maybeMul << "\n");
        REMARK([&](){
            return OptimizationRemark(DEBUG_TYPE, "Marked for FMA", mulOp)
                << "marked for fma " << ore::NV("fmul", mulOp);
        });
        ++TotalContracted;
        fmf.setAllowContract(true);
        mulOp->copyFastMathFlags(fmf);
        return true;
    }
    return false;
}

static bool combineMulAdd(Function &F) JL_NOTSAFEPOINT
{
    OptimizationRemarkEmitter ORE(&F);
    bool modified = false;
    for (auto &BB: F) {
        for (auto it = BB.begin(); it != BB.end();) {
            auto &I = *it;
            it++;
            switch (I.getOpcode()) {
            case Instruction::FAdd: {
                if (!I.isFast())
                    continue;
                modified |= checkCombine(I.getOperand(0), ORE) || checkCombine(I.getOperand(1), ORE);
                break;
            }
            case Instruction::FSub: {
                if (!I.isFast())
                    continue;
                modified |= checkCombine(I.getOperand(0), ORE) || checkCombine(I.getOperand(1), ORE);
                break;
            }
            default:
                break;
            }
        }
    }
#ifdef JL_VERIFY_PASSES
    assert(!verifyFunction(F, &errs()));
#endif
    return modified;
}

PreservedAnalyses CombineMulAdd::run(Function &F, FunctionAnalysisManager &AM) JL_NOTSAFEPOINT
{
    if (combineMulAdd(F)) {
        return PreservedAnalyses::allInSet<CFGAnalyses>();
    }
    return PreservedAnalyses::all();
}


struct CombineMulAddLegacy : public FunctionPass {
    static char ID;
    CombineMulAddLegacy() : FunctionPass(ID)
    {}

private:
    bool runOnFunction(Function &F) override {
        return combineMulAdd(F);
    }
};

char CombineMulAddLegacy::ID = 0;
static RegisterPass<CombineMulAddLegacy> X("CombineMulAdd", "Combine mul and add to muladd",
                                     false /* Only looks at CFG */,
                                     false /* Analysis Pass */);

Pass *createCombineMulAddPass()
{
    return new CombineMulAddLegacy();
}

extern "C" JL_DLLEXPORT void LLVMExtraAddCombineMulAddPass_impl(LLVMPassManagerRef PM)
{
    unwrap(PM)->add(createCombineMulAddPass());
}

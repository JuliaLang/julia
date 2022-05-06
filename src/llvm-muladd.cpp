// This file is a part of Julia. License is MIT: https://julialang.org/license

#define DEBUG_TYPE "combine_muladd"
#undef DEBUG
#include "llvm-version.h"
#include "passes.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

#include <llvm/ADT/Statistic.h>
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

using namespace llvm;
STATISTIC(TotalContracted, "Total number of multiplies marked for FMA");

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
static bool checkCombine(Value *maybeMul)
{
    auto mulOp = dyn_cast<Instruction>(maybeMul);
    if (!mulOp || mulOp->getOpcode() != Instruction::FMul)
        return false;
    if (!mulOp->hasOneUse())
        return false;
    // On 5.0+ we only need to mark the mulOp as contract and the backend will do the work for us.
    auto fmf = mulOp->getFastMathFlags();
    if (!fmf.allowContract()) {
        ++TotalContracted;
        fmf.setAllowContract(true);
        mulOp->copyFastMathFlags(fmf);
        return true;
    }
    return false;
}

static bool combineMulAdd(Function &F)
{
    bool modified = false;
    for (auto &BB: F) {
        for (auto it = BB.begin(); it != BB.end();) {
            auto &I = *it;
            it++;
            switch (I.getOpcode()) {
            case Instruction::FAdd: {
                if (!I.isFast())
                    continue;
                modified |= checkCombine(I.getOperand(0)) || checkCombine(I.getOperand(1));
                break;
            }
            case Instruction::FSub: {
                if (!I.isFast())
                    continue;
                modified |= checkCombine(I.getOperand(0)) || checkCombine(I.getOperand(1));
                break;
            }
            default:
                break;
            }
        }
    }
    assert(!verifyFunction(F));
    return modified;
}

PreservedAnalyses CombineMulAdd::run(Function &F, FunctionAnalysisManager &AM)
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

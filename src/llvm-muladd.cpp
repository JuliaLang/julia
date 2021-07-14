// This file is a part of Julia. License is MIT: https://julialang.org/license

#define DEBUG_TYPE "combine_muladd"
#undef DEBUG
#include "llvm-version.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

#include <llvm/IR/Value.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Operator.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/Pass.h>
#include <llvm/Support/Debug.h>

#include "julia.h"
#include "julia_assert.h"

using namespace llvm;

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

// Return true if this function shouldn't be called again on the other operand
// This will always return false on LLVM 5.0+
static bool checkCombine(Module *m, Instruction *addOp, Value *maybeMul, Value *addend,
                         bool negadd, bool negres)
{
    auto mulOp = dyn_cast<Instruction>(maybeMul);
    if (!mulOp || mulOp->getOpcode() != Instruction::FMul)
        return false;
    if (!mulOp->hasOneUse())
        return false;
    // On 5.0+ we only need to mark the mulOp as contract and the backend will do the work for us.
    auto fmf = mulOp->getFastMathFlags();
    fmf.setAllowContract(true);
    mulOp->copyFastMathFlags(fmf);
    return false;
}

static bool combineMulAdd(Function &F)
{
    Module *m = F.getParent();
    for (auto &BB: F) {
        for (auto it = BB.begin(); it != BB.end();) {
            auto &I = *it;
            it++;
            switch (I.getOpcode()) {
            case Instruction::FAdd: {
                if (!I.isFast())
                    continue;
                checkCombine(m, &I, I.getOperand(0), I.getOperand(1), false, false) ||
                    checkCombine(m, &I, I.getOperand(1), I.getOperand(0), false, false);
                break;
            }
            case Instruction::FSub: {
                if (!I.isFast())
                    continue;
                checkCombine(m, &I, I.getOperand(0), I.getOperand(1), true, false) ||
                    checkCombine(m, &I, I.getOperand(1), I.getOperand(0), true, true);
                break;
            }
            default:
                break;
            }
        }
    }
    return true;
}

struct CombineMulAdd : PassInfoMixin<CombineMulAdd> {
    PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM);
};

PreservedAnalyses CombineMulAdd::run(Function &F, FunctionAnalysisManager &AM)
{
    combineMulAdd(F);
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

extern "C" JL_DLLEXPORT void LLVMExtraAddCombineMulAddPass(LLVMPassManagerRef PM)
{
    unwrap(PM)->add(createCombineMulAddPass());
}

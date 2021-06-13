// This file is a part of Julia. License is MIT: https://julialang.org/license

#define DEBUG_TYPE "replace"
#undef DEBUG
#include "llvm-version.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

#include <llvm/IR/Value.h>
#include <llvm/IR/LegacyPassManager.h>
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

struct ReplaceCall : public FunctionPass {
    static char ID;
    ReplaceCall() : FunctionPass(ID)
    {}

private:
    bool runOnFunction(Function &F) override;
};

bool ReplaceCall::runOnFunction(Function &F)
{
    for (auto &BB: F) {
        for (auto it = BB.begin(); it != BB.end();) {
            auto &I = *it;
            it++;
            if (auto CI = dyn_cast<CallInst>(&I)) {
                for (auto &attr : CI->getAttributes().getAttributes(AttributeList::FunctionIndex)) {
                    if (attr.isStringAttribute()) {
                        auto str = attr.getKindAsString();
                        std::string prefix = "replace_";
                        if (str.startswith(prefix)) {
                            auto NewF = F.getParent()->getOrInsertFunction(str.substr(prefix.size()),
                                CI->getFunctionType());
                            CI->setCalledOperand(NewF.getCallee());
                            break;
                        }
                    }
                }
            }
        }
    }
    return true;
}

char ReplaceCall::ID = 0;
static RegisterPass<ReplaceCall> X("ReplaceCall", "Replace calls",
                                     false /* Only looks at CFG */,
                                     false /* Analysis Pass */);

Pass *createReplaceCallPass()
{
    return new ReplaceCall();
}

extern "C" JL_DLLEXPORT void LLVMExtraAddReplaceCallPass(LLVMPassManagerRef PM)
{
    unwrap(PM)->add(createReplaceCallPass());
}

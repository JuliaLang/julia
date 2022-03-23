// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-version.h"
#include "passes.h"

#include <llvm/Pass.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Support/Debug.h>

#include "julia.h"

#define DEBUG_TYPE "remove_ni"

using namespace llvm;

namespace {

static bool removeNI(Module &M)
{
    auto dlstr = M.getDataLayoutStr();
    auto nistart = dlstr.find("-ni:");
    if (nistart == std::string::npos)
        return false;
    auto len = dlstr.size();
    auto niend = nistart + 1;
    for (; niend < len; niend++) {
        if (dlstr[niend] == '-') {
            break;
        }
    }
    dlstr.erase(nistart, niend - nistart);
    M.setDataLayout(dlstr);
    return true;
}
}

PreservedAnalyses RemoveNI::run(Module &M, ModuleAnalysisManager &AM)
{
    if (removeNI(M)) {
        return PreservedAnalyses::allInSet<CFGAnalyses>();
    }
    return PreservedAnalyses::all();
}

namespace {
struct RemoveNILegacy : public ModulePass {
    static char ID;
    RemoveNILegacy() : ModulePass(ID) {};

    bool runOnModule(Module &M)
    {
        return removeNI(M);
    }
};

char RemoveNILegacy::ID = 0;
static RegisterPass<RemoveNILegacy>
        Y("RemoveNI",
          "Remove non-integral address space.",
          false,
          false);
}

Pass *createRemoveNIPass()
{
    return new RemoveNILegacy();
}

extern "C" JL_DLLEXPORT void LLVMExtraAddRemoveNIPass_impl(LLVMPassManagerRef PM)
{
    unwrap(PM)->add(createRemoveNIPass());
}

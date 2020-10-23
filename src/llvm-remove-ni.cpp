// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-version.h"

#include <llvm/IR/Module.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Support/Debug.h>

#define DEBUG_TYPE "remove_ni"

using namespace llvm;

namespace {

struct RemoveNIPass : public ModulePass {
    static char ID;
    RemoveNIPass() : ModulePass(ID) {};

    bool runOnModule(Module &M)
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
};

char RemoveNIPass::ID = 0;
static RegisterPass<RemoveNIPass>
        Y("RemoveNI",
          "Remove non-integral address space.",
          false,
          false);
}

Pass *createRemoveNIPass()
{
    return new RemoveNIPass();
}

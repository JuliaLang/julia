// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-version.h"
#include "passes.h"

#include <llvm/Pass.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/Support/Debug.h>

#include "julia.h"

#define DEBUG_TYPE "remove_ni"

using namespace llvm;

namespace {

static bool removeNI(Module &M) JL_NOTSAFEPOINT
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

PreservedAnalyses RemoveNIPass::run(Module &M, ModuleAnalysisManager &AM)
{
    if (removeNI(M)) {
        return PreservedAnalyses::allInSet<CFGAnalyses>();
    }
    return PreservedAnalyses::all();
}

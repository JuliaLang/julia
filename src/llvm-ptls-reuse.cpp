// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-version.h"

#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/Pass.h>
#include <llvm/Support/Debug.h>
#include <llvm/Transforms/Utils/ModuleUtils.h>

#include "codegen_shared.h"
#include "julia.h"
#include "julia_internal.h"
#include "llvm-pass-helpers.h"

#define DEBUG_TYPE "ptls_reuse_lowering"

using namespace llvm;

// Materialize PTLS if used.
//
// During the code gen, normal uses of the PTLS are marked by the placeholder
// function `reuse_jltls_states_func` at each use-site while explicit refetches
// are marked by another plaholder function `refetch_jltls_states_func`. These
// placeholders are materialized using `jltls_states_func` after the control
// flow structure is finalized. We merge all the reuses, possibly using the phi
// nodes if at least one predecessor refetches the PTLS.
struct LowerPTLSReuse : public ModulePass, private JuliaPassContext {
    static char ID;
    LowerPTLSReuse() : ModulePass(ID) {}

private:
    bool runOnModule(Module &M) override;
    bool runOnFunction(Function &F);
};

bool LowerPTLSReuse::runOnModule(Module &M)
{
    LLVM_DEBUG(dbgs() << "LOWER PTLS REUSE (" << static_cast<void *>(this)
                      << "): Processing module " << M.getName() << "\n");
    initAll(M);
    bool changed = false;
    for (auto &F: M) {
        changed |= runOnFunction(F);
    }
    if (reuse_jltls_states_func) {
        reuse_jltls_states_func->eraseFromParent();
    }
    return changed;
}

bool LowerPTLSReuse::runOnFunction(Function &F)
{
    LLVM_DEBUG(dbgs() << "LOWER PTLS REUSE (" << static_cast<void *>(this)
                      << "): Processing function  " << F.getName() << "\n");
    getOrNullReusePtlsStatesFunc();
    if (!reuse_jltls_states_func) {
        return false;
    }
    // BBToInstructionMapTy bb_to_ptls;
    SmallVector<CallInst *, 8> reuses;
    for (auto &BB : F) {
        for (auto &I : BB) {
            if (auto *C = dyn_cast<CallInst>(&I)) {
                if (C->getCalledFunction() == reuse_jltls_states_func) {
                    reuses.push_back(C);
                }
            }
        }
    }
    bool changed = false;
    if (reuses.size()) {
        auto ptls = ensureEntryBlockPtls(F);
        // TODO: Handle refetch_jltls_states_func
        for (auto C : reuses) {
            C->replaceAllUsesWith(ptls);
            C->eraseFromParent();
        }
        changed = true;
    }
    return changed;
}

char LowerPTLSReuse::ID = 0;
static RegisterPass<LowerPTLSReuse> X("LowerPTLSReuse", "Lower PTLS reuses", false, false);

Pass *createLowerPTLSReusePass()
{
    return new LowerPTLSReuse();
}

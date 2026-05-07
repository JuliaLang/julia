// This file is a part of Julia. License is MIT: https://julialang.org/license

// Annotate conditional branches whose successors split into a may-return side
// and an always-unreachable side with cold branch_weights metadata. "Always
// unreachable" here means no path from the successor block reaches a
// `ReturnInst`; "may return" means at least one path does. The classification
// is computed from forward CFG reachability to the function's return blocks
// (a reverse traversal from those blocks), so it is robust to local
// restructuring such as inlined error helpers branching internally before
// reaching `unreachable`.
//
// Helping LLVM mark these error / throw paths as cold improves block layout
// and discourages speculation of error-path setup code into the hot path.

#include "llvm-version.h"
#include "passes.h"

#include <llvm/ADT/SmallPtrSet.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/Statistic.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/MDBuilder.h>
#include <llvm/IR/PassManager.h>
#include <llvm/Support/Debug.h>

#define DEBUG_TYPE "cold-unreachable-branches"

using namespace llvm;

STATISTIC(NumAnnotated, "Number of conditional branches annotated as cold");

static bool runColdUnreachableBranches(Function &F) JL_NOTSAFEPOINT
{
    // Compute the set of blocks that "may return": from which at least one
    // path reaches a ReturnInst. We seed with all return blocks and walk
    // backwards through predecessors. Anything not in this set has no path
    // to a return, i.e. is "always-unreachable".
    SmallPtrSet<const BasicBlock *, 16> MayReturn;
    SmallVector<const BasicBlock *, 16> Worklist;
    for (const BasicBlock &BB : F) {
        if (isa<ReturnInst>(BB.getTerminator())) {
            MayReturn.insert(&BB);
            Worklist.push_back(&BB);
        }
    }
    while (!Worklist.empty()) {
        const BasicBlock *BB = Worklist.pop_back_val();
        for (const BasicBlock *Pred : predecessors(BB)) {
            if (MayReturn.insert(Pred).second)
                Worklist.push_back(Pred);
        }
    }

    if (MayReturn.empty())
        return false; // no return at all; nothing useful to annotate

    MDBuilder MDB(F.getContext());
    SmallVector<uint32_t, 2> ColdWeights{2000, 1};
    SmallVector<uint32_t, 2> ColdWeightsSwapped{1, 2000};
    bool Changed = false;
    for (BasicBlock &BB : F) {
        // Only annotate branches in blocks that themselves may return; if the
        // branch is already in dead code, the weights are pointless.
        if (!MayReturn.contains(&BB))
            continue;
        auto *BI = dyn_cast<BranchInst>(BB.getTerminator());
        if (!BI || !BI->isConditional())
            continue;
        if (BI->getMetadata(LLVMContext::MD_prof))
            continue;
        BasicBlock *S0 = BI->getSuccessor(0);
        BasicBlock *S1 = BI->getSuccessor(1);
        bool S0MayReturn = MayReturn.contains(S0);
        bool S1MayReturn = MayReturn.contains(S1);
        if (S0MayReturn && !S1MayReturn) {
            BI->setMetadata(LLVMContext::MD_prof, MDB.createBranchWeights(ColdWeights));
            ++NumAnnotated;
            Changed = true;
        } else if (!S0MayReturn && S1MayReturn) {
            BI->setMetadata(LLVMContext::MD_prof, MDB.createBranchWeights(ColdWeightsSwapped));
            ++NumAnnotated;
            Changed = true;
        }
    }

    return Changed;
}

PreservedAnalyses ColdUnreachableBranchesPass::run(Function &F, FunctionAnalysisManager &AM)
{
    if (runColdUnreachableBranches(F)) {
        // Only metadata is attached; CFG is unchanged.
        return PreservedAnalyses::allInSet<CFGAnalyses>();
    }
    return PreservedAnalyses::all();
}

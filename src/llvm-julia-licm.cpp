// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-version.h"

#include <llvm/Analysis/LoopInfo.h>
#include <llvm/Analysis/LoopPass.h>
#include "llvm/Analysis/LoopIterator.h"
#include <llvm/IR/Dominators.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Transforms/Utils/LoopUtils.h>

#include "llvm-pass-helpers.h"

#define DEBUG_TYPE "julia-licm"

using namespace llvm;

/*
 * Julia LICM pass.
 * This takes care of some julia intrinsics that is safe to move around/out of loops but
 * can't be handled by LLVM's LICM. These intrinsics can be moved outside of
 * loop context as well but it is inside a loop where they matter the most.
 */

namespace {

struct JuliaLICMPass : public LoopPass, public JuliaPassContext {
    static char ID;
    JuliaLICMPass() : LoopPass(ID) {};

    bool runOnLoop(Loop *L, LPPassManager &LPM) override
    {
        // Get the preheader block to move instructions into,
        // required to run this pass.
        BasicBlock *preheader = L->getLoopPreheader();
        if (!preheader)
            return false;
        BasicBlock *header = L->getHeader();
        initFunctions(*header->getModule());
        // Also require `gc_preserve_begin_func` whereas
        // `gc_preserve_end_func` is optional since the input to
        // `gc_preserve_end_func` must be from `gc_preserve_begin_func`.
        if (!gc_preserve_begin_func)
            return false;
        auto LI = &getAnalysis<LoopInfoWrapperPass>().getLoopInfo();
        auto DT = &getAnalysis<DominatorTreeWrapperPass>().getDomTree();

        // Lazy initialization of exit blocks insertion points.
        bool exit_pts_init = false;
        SmallVector<Instruction*, 8> _exit_pts;
        auto get_exit_pts = [&] () -> ArrayRef<Instruction*> {
            if (!exit_pts_init) {
                exit_pts_init = true;
                SmallVector<BasicBlock*, 8> exit_bbs;
                L->getUniqueExitBlocks(exit_bbs);
                for (BasicBlock *bb: exit_bbs) {
                    _exit_pts.push_back(&*bb->getFirstInsertionPt());
                }
            }
            return _exit_pts;
        };

        bool changed = false;
        // Scan in the right order so that we'll hoist the `begin`
        // before we consider sinking `end`.
        LoopBlocksRPO worklist(L);
        worklist.perform(LI);
        for (auto *bb : worklist) {
            for (BasicBlock::iterator II = bb->begin(), E = bb->end(); II != E;) {
                auto call = dyn_cast<CallInst>(&*II++);
                if (!call)
                    continue;
                auto callee = call->getCalledValue();
                assert(callee);
                // It is always legal to extend the preserve period
                // so we only need to make sure it is legal to move/clone
                // the calls.
                // If all the input arguments dominates the whole loop we can
                // hoist the `begin` and if a `begin` dominates the loop the
                // corresponding `end` can be moved to the loop exit.
                if (callee == gc_preserve_begin_func) {
                    bool canhoist = true;
                    for (Use &U : call->arg_operands()) {
                        // Check if all arguments are generated outside the loop
                        auto origin = dyn_cast<Instruction>(U.get());
                        if (!origin)
                            continue;
                        if (!DT->properlyDominates(origin->getParent(), header)) {
                            canhoist = false;
                            break;
                        }
                    }
                    if (!canhoist)
                        continue;
                    call->moveBefore(preheader->getTerminator());
                    changed = true;
                }
                else if (callee == gc_preserve_end_func) {
                    auto begin = cast<Instruction>(call->getArgOperand(0));
                    if (!DT->properlyDominates(begin->getParent(), header))
                        continue;
                    changed = true;
                    auto exit_pts = get_exit_pts();
                    if (exit_pts.empty()) {
                        call->eraseFromParent();
                        continue;
                    }
                    call->moveBefore(exit_pts[0]);
                    for (unsigned i = 1; i < exit_pts.size(); i++) {
                        // Clone exit
                        CallInst::Create(call, {}, exit_pts[i]);
                    }
                }
            }
        }
        return changed;
    }

    void getAnalysisUsage(AnalysisUsage &AU) const override
    {
        getLoopAnalysisUsage(AU);
    }
};

char JuliaLICMPass::ID = 0;
static RegisterPass<JuliaLICMPass>
        Y("JuliaLICM", "LICM for julia specific intrinsics.",
          false, false);
}

Pass *createJuliaLICMPass()
{
    return new JuliaLICMPass();
}

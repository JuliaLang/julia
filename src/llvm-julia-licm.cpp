// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-version.h"
#include "passes.h"

#include <llvm/ADT/Statistic.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/Analysis/LoopPass.h>
#include "llvm/Analysis/LoopIterator.h"
#include <llvm/IR/Dominators.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Transforms/Utils/LoopUtils.h>
#include <llvm/Analysis/ValueTracking.h>

#include "llvm-pass-helpers.h"
#include "julia.h"
#include "llvm-alloc-helpers.h"
#include "codegen_shared.h"

#define DEBUG_TYPE "julia-licm"

using namespace llvm;

STATISTIC(HoistedPreserveBegin, "Number of gc_preserve_begin instructions hoisted out of a loop");
STATISTIC(SunkPreserveEnd, "Number of gc_preserve_end instructions sunk out of a loop");
STATISTIC(ErasedPreserveEnd, "Number of gc_preserve_end instructions removed from nonterminating loops");
STATISTIC(HoistedWriteBarrier, "Number of write barriers hoisted out of a loop");
STATISTIC(HoistedAllocation, "Number of allocations hoisted out of a loop");

/*
 * Julia LICM pass.
 * This takes care of some julia intrinsics that is safe to move around/out of loops but
 * can't be handled by LLVM's LICM. These intrinsics can be moved outside of
 * loop context as well but it is inside a loop where they matter the most.
 */

namespace {

struct JuliaLICMPassLegacy : public LoopPass {
    static char ID;
    JuliaLICMPassLegacy() : LoopPass(ID) {};

    bool runOnLoop(Loop *L, LPPassManager &LPM) override;

    protected:
        void getAnalysisUsage(AnalysisUsage &AU) const override {
            getLoopAnalysisUsage(AU);
        }
};

struct JuliaLICM : public JuliaPassContext {
    function_ref<DominatorTree &()> GetDT;
    function_ref<LoopInfo &()> GetLI;
    JuliaLICM(function_ref<DominatorTree &()> GetDT,
              function_ref<LoopInfo &()> GetLI) : GetDT(GetDT), GetLI(GetLI) {}

    bool runOnLoop(Loop *L)
    {
        // Get the preheader block to move instructions into,
        // required to run this pass.
        BasicBlock *preheader = L->getLoopPreheader();
        if (!preheader)
            return false;
        BasicBlock *header = L->getHeader();
        const llvm::DataLayout &DL = header->getModule()->getDataLayout();
        initFunctions(*header->getModule());
        // Also require `gc_preserve_begin_func` whereas
        // `gc_preserve_end_func` is optional since the input to
        // `gc_preserve_end_func` must be from `gc_preserve_begin_func`.
        // We also hoist write barriers here, so we don't exit if write_barrier_func exists
        if (!gc_preserve_begin_func && !write_barrier_func && !write_barrier_binding_func &&
            !alloc_obj_func)
            return false;
        auto LI = &GetLI();
        auto DT = &GetDT();

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
                Value *callee = call->getCalledOperand();
                assert(callee != nullptr);
                // It is always legal to extend the preserve period
                // so we only need to make sure it is legal to move/clone
                // the calls.
                // If all the input arguments dominates the whole loop we can
                // hoist the `begin` and if a `begin` dominates the loop the
                // corresponding `end` can be moved to the loop exit.
                if (callee == gc_preserve_begin_func) {
                    bool canhoist = true;
                    for (Use &U : call->args()) {
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
                    ++HoistedPreserveBegin;
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
                        ++ErasedPreserveEnd;
                        call->eraseFromParent();
                        continue;
                    }
                    ++SunkPreserveEnd;
                    call->moveBefore(exit_pts[0]);
                    for (unsigned i = 1; i < exit_pts.size(); i++) {
                        // Clone exit
                        CallInst::Create(call, {}, exit_pts[i]);
                    }
                }
                else if (callee == write_barrier_func ||
                         callee == write_barrier_binding_func) {
                    bool valid = true;
                    for (std::size_t i = 0; i < call->arg_size(); i++) {
                        if (!L->makeLoopInvariant(call->getArgOperand(i), changed)) {
                            valid = false;
                            break;
                        }
                    }
                    if (valid) {
                        ++HoistedWriteBarrier;
                        call->moveBefore(preheader->getTerminator());
                        changed = true;
                    }
                }
                else if (callee == alloc_obj_func) {
                    jl_alloc::AllocUseInfo use_info;
                    jl_alloc::CheckInst::Stack check_stack;
                    jl_alloc::EscapeAnalysisRequiredArgs required{use_info, check_stack, *this, DL};
                    jl_alloc::runEscapeAnalysis(call, required, jl_alloc::EscapeAnalysisOptionalArgs().with_valid_set(&L->getBlocksSet()));
                    if (use_info.escaped || use_info.addrescaped) {
                        continue;
                    }
                    bool valid = true;
                    for (std::size_t i = 0; i < call->arg_size(); i++) {
                        if (!L->makeLoopInvariant(call->getArgOperand(i), changed)) {
                            valid = false;
                            break;
                        }
                    }
                    if (use_info.refstore) {
                        // We need to add write barriers to any stores
                        // that may start crossing generations
                        continue;
                    }
                    if (valid) {
                        ++HoistedAllocation;
                        call->moveBefore(preheader->getTerminator());
                        changed = true;
                    }
                }
            }
        }
        assert(!verifyFunction(*L->getHeader()->getParent()));
        return changed;
    }
};

bool JuliaLICMPassLegacy::runOnLoop(Loop *L, LPPassManager &LPM) {
    auto GetDT = [this]() -> DominatorTree & {
        return getAnalysis<DominatorTreeWrapperPass>().getDomTree();
    };
    auto GetLI = [this]() -> LoopInfo & {
        return getAnalysis<LoopInfoWrapperPass>().getLoopInfo();
    };
    auto juliaLICM = JuliaLICM(GetDT, GetLI);
    return juliaLICM.runOnLoop(L);
}

char JuliaLICMPassLegacy::ID = 0;
static RegisterPass<JuliaLICMPassLegacy>
        Y("JuliaLICM", "LICM for julia specific intrinsics.",
          false, false);
} //namespace

PreservedAnalyses JuliaLICMPass::run(Loop &L, LoopAnalysisManager &AM,
                          LoopStandardAnalysisResults &AR, LPMUpdater &U)
{
    auto GetDT = [&AR]() -> DominatorTree & {
        return AR.DT;
    };
    auto GetLI = [&AR]() -> LoopInfo & {
        return AR.LI;
    };
    auto juliaLICM = JuliaLICM(GetDT, GetLI);
    if (juliaLICM.runOnLoop(&L)) {
        auto preserved = PreservedAnalyses::allInSet<CFGAnalyses>();
        preserved.preserve<LoopAnalysis>();
        preserved.preserve<DominatorTreeAnalysis>();
        return preserved;
    }
    return PreservedAnalyses::all();
}

Pass *createJuliaLICMPass()
{
    return new JuliaLICMPassLegacy();
}

extern "C" JL_DLLEXPORT void LLVMExtraJuliaLICMPass_impl(LLVMPassManagerRef PM)
{
    unwrap(PM)->add(createJuliaLICMPass());
}

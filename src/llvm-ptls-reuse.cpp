// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-version.h"

#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/ValueMap.h>
#include <llvm/Pass.h>
#include <llvm/Support/Debug.h>
#include <llvm/Transforms/Utils/ModuleUtils.h>
#ifdef JL_DEBUG_BUILD
#include <llvm/IR/Verifier.h>
#endif

#include "codegen_shared.h"
#include "julia.h"
#include "julia_internal.h"
#include "llvm-pass-helpers.h"

#define DEBUG_TYPE "ptls_reuse_lowering"

using namespace llvm;

using BbToInstructionMapTy = ValueMap<const BasicBlock *, Instruction *>;

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
    Type *PtlsType;

    bool runOnModule(Module &M) override;
    bool runOnFunction(Function &F);
    void propagatePtls(Instruction *Ptls, BbToInstructionMapTy &BbToPtls,
                       SmallDenseSet<const Instruction *> &Seen);
    Instruction *mergePtls(BasicBlock *BB, BbToInstructionMapTy &BbToPtls);
    Instruction *ptlsForBb(BasicBlock *BB, BbToInstructionMapTy &BbToPtls);
    void populatePtls(BasicBlock *BB, BbToInstructionMapTy &BbToPtls);
};

bool LowerPTLSReuse::runOnModule(Module &M)
{
    LLVM_DEBUG(dbgs() << "LOWER PTLS REUSE (" << static_cast<void *>(this)
                      << "): Processing module " << M.getName() << "\n");
    initAll(M);
    PtlsType = PointerType::get(T_ppjlvalue, 0);
    bool changed = false;
    for (auto &F : M) {
#ifdef JL_DEBUG_BUILD
        assert(!verifyFunction(F, &dbgs()));
#endif
        changed |= runOnFunction(F);
#ifdef JL_DEBUG_BUILD
        assert(!verifyFunction(F, &dbgs()));
#endif
    }
    if (reuse_jltls_states_func) {
        reuse_jltls_states_func->eraseFromParent();
    }
    if (refetch_jltls_states_func) {
        refetch_jltls_states_func->eraseFromParent();
    }
    return changed;
}

void LowerPTLSReuse::propagatePtls(Instruction *Ptls, BbToInstructionMapTy &BbToPtls,
                                   SmallDenseSet<const Instruction *> &Seen)
{
    if (!Seen.insert(Ptls).second) {
        return;
    }
    auto BB = Ptls->getParent();
    auto InsertResult = BbToPtls.insert({BB, Ptls});
    if (isa<SelectInst>(Ptls)) {
        // TODO: Ask if this branch is required. Since we don't introduce select
        // for PTLS variables explicitly, this won't be necessary if we can be
        // sure that other LLVM passes won't introduces select instructions.
        // TODO: Test this branch if we keep this branch.
        if (!InsertResult.second) {
            auto OldPtls = InsertResult.first->second;
            if (OldPtls->comesBefore(Ptls)) {
                BbToPtls[BB] = Ptls;
            }
        }
    }
    else if (isa<PHINode>(Ptls)) {
        // Nothing to do, since PHINode is always before other instructions.
    }
    else {
        return;
    }
    for (auto User : Ptls->users()) {
        if (auto I = dyn_cast<Instruction>(User)) {
            propagatePtls(I, BbToPtls, Seen);
        }
    }
}

Instruction *LowerPTLSReuse::mergePtls(BasicBlock *BB, BbToInstructionMapTy &BbToPtls)
{
    if (BB == &BB->getParent()->getEntryBlock()) {
        return ensureEntryBlockPtls(*BB->getParent());
    }
    SmallVector<BasicBlock *, 2> PreBbs;
    for (auto Pre : predecessors(BB)) {
        PreBbs.push_back(Pre);
    }
    auto Phi = PHINode::Create(PointerType::get(T_ppjlvalue, 0), PreBbs.size(), "ptls",
                               BB->getFirstNonPHI() ? BB->getFirstNonPHI() : &BB->front());
    if (!BbToPtls[BB]) {
        // PTLS from this BB is requested as a transitive dependency. So,
        // marking the phi node as the PTLS at the end of this basic block. This
        // has to be done before the recursions.
        BbToPtls[BB] = Phi;
        assert(Phi->getType() == PtlsType);
    }
    for (auto Pre : PreBbs) {
        Phi->addIncoming(ptlsForBb(Pre, BbToPtls), Pre);
    }
    return Phi;
}

// Get PTLS usable at the end of `BB`.
Instruction *LowerPTLSReuse::ptlsForBb(BasicBlock *BB, BbToInstructionMapTy &BbToPtls)
{
    if (auto Ptls = BbToPtls[BB]) {
        return Ptls;
    }
    auto Ptls = mergePtls(BB, BbToPtls);
    assert(Ptls->getType() == PtlsType);
    return Ptls;
}

void LowerPTLSReuse::populatePtls(BasicBlock *BB, BbToInstructionMapTy &BbToPtls)
{
    // First, go over instructions in `BB` and construct a mapping between
    // "source" PTLS and its reuses.
    Instruction *Ptls = mergePtls(BB, BbToPtls);
    SmallVector<CallInst *> Refetches;
    SmallVector<std::pair<Instruction *, std::unique_ptr<SmallVector<Instruction *>>>>
        SourceAndReuses;
    auto Reuses = std::make_unique<SmallVector<Instruction *>>();
    for (Instruction &I : *BB) {
        if (auto *C = dyn_cast<CallInst>(&I)) {
            if (!C->getCalledFunction()) {
                continue;
            }
            else if (C->getCalledFunction() == ptls_getter ||
                     C->getCalledFunction() == refetch_jltls_states_func) {
                auto Tmp = std::make_unique<SmallVector<Instruction *>>();
                Tmp.swap(Reuses);
                SourceAndReuses.push_back(std::make_pair(Ptls, std::move(Tmp)));
                Ptls = &I;
                if (C->getCalledFunction() == refetch_jltls_states_func) {
                    Refetches.push_back(C);
                }
            }
            else if (C->getCalledFunction() == reuse_jltls_states_func) {
                Reuses->push_back(&I);
            }
        }
    }
    if (!Reuses->empty()) {
        SourceAndReuses.push_back(std::make_pair(Ptls, std::move(Reuses)));
    }

    // Second, replace the reuses with the source PTLS.
    for (auto &PtlsAndReuses : SourceAndReuses) {
        auto Source = PtlsAndReuses.first;
        auto Reuses = PtlsAndReuses.second.release();
        for (auto &I : *Reuses) {
            I->replaceAllUsesWith(Source);
            I->eraseFromParent();
        }
    }
    assert(Ptls->getType() == PtlsType);
    BbToPtls[BB] = Ptls;

    // Finally, materialize refetches in-place.
    for (auto I : Refetches) {
        I->setCalledFunction(ptls_getter);
    }
}

bool LowerPTLSReuse::runOnFunction(Function &F)
{
    LLVM_DEBUG(dbgs() << "LOWER PTLS REUSE (" << static_cast<void *>(this)
                      << "): Processing function  " << F.getName() << "\n");
    getOrNullReusePtlsStatesFunc();
    if (!usePtls(F)) {
        return false;
    }
    ensureEntryBlockPtls(F);

    // Create a mapping from BB to PTLS-at-the-end-of-BB:
    BbToInstructionMapTy BbToPtls;
    SmallVector<Instruction *> PtlsGetters;
    SmallVector<BasicBlock *> BbWithPtls;
    for (BasicBlock &BB : F) {
        auto inserted = false;
        for (Instruction &I : BB) {
            if (auto *C = dyn_cast<CallInst>(&I)) {
                if (!C->getCalledFunction()) {
                    continue;
                }
                else if (C->getCalledFunction() == ptls_getter ||
                         C->getCalledFunction() == reuse_jltls_states_func ||
                         C->getCalledFunction() == refetch_jltls_states_func) {
                    assert(C->getType() == PtlsType);
                    BbToPtls[&BB] = C;
                    PtlsGetters.push_back(C);

                    if (!inserted) {
                        inserted = true;
                        BbWithPtls.push_back(&BB);
                    }
                }
            }
        }
    }

    // Forward-propagate PTLS through phi and select instructions.
    {
        SmallDenseSet<const Instruction *> Seen;
        for (auto I : PtlsGetters) {
            propagatePtls(I, BbToPtls, Seen);
        }
    }

    // Process BBs that directly require PTLS and introduce the phi nodes in
    // other BBs as needed.
    for (BasicBlock *BB : BbWithPtls) {
        populatePtls(BB, BbToPtls);
    }
    return true;
}

char LowerPTLSReuse::ID = 0;
static RegisterPass<LowerPTLSReuse> X("LowerPTLSReuse", "Lower PTLS reuses", false, false);

Pass *createLowerPTLSReusePass()
{
    return new LowerPTLSReuse();
}

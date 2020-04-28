// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-version.h"

#define DEBUG_TYPE "lower_simd_loop"

// This file defines a LLVM pass that:
// 1. Set's loop information in form of metadata
// 2. If the metadata contains `julia.simdloop` finds reduction chains and marks
//    floating-point operations as fast-math. `See enableUnsafeAlgebraIfReduction`.
// 3. If the metadata contains `julia.ivdep` marks all memory accesses in the loop
//    as independent of each other.
//
// The pass hinges on a call to a marker function that has metadata attached to it.
// To construct the pass call `createLowerSimdLoopPass`.

#include "support/dtypes.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

#include <llvm/Analysis/LoopPass.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Metadata.h>
#include <llvm/Support/Debug.h>

#include <cstdio>

#include "julia_assert.h"

namespace llvm {


/// This pass should run after reduction variables have been converted to phi nodes,
/// otherwise floating-point reductions might not be recognized as such and
/// prevent SIMDization.
struct LowerSIMDLoop : public ModulePass {
    static char ID;
    LowerSIMDLoop() : ModulePass(ID)
    {
    }

    protected:
    void getAnalysisUsage(AnalysisUsage &AU) const override
    {
        ModulePass::getAnalysisUsage(AU);
        AU.addRequired<LoopInfoWrapperPass>();
        AU.addPreserved<LoopInfoWrapperPass>();
        AU.setPreservesCFG();
    }

    private:
    bool runOnModule(Module &M) override;

    bool markLoopInfo(Module &M, Function *marker);

    /// If Phi is part of a reduction cycle of FAdd, FSub, FMul or FDiv,
    /// mark the ops as permitting reassociation/commuting.
    /// As of LLVM 4.0, FDiv is not handled by the loop vectorizer
    void enableUnsafeAlgebraIfReduction(PHINode *Phi, Loop *L) const;
};

static unsigned getReduceOpcode(Instruction *J, Instruction *operand)
{
    switch (J->getOpcode()) {
    case Instruction::FSub:
        if (J->getOperand(0) != operand)
            return 0;
        JL_FALLTHROUGH;
    case Instruction::FAdd:
        return Instruction::FAdd;
    case Instruction::FDiv:
        if (J->getOperand(0) != operand)
            return 0;
        JL_FALLTHROUGH;
    case Instruction::FMul:
        return Instruction::FMul;
    default:
        return 0;
    }
}

void LowerSIMDLoop::enableUnsafeAlgebraIfReduction(PHINode *Phi, Loop *L) const
{
    typedef SmallVector<Instruction*, 8> chainVector;
    chainVector chain;
    Instruction *J;
    unsigned opcode = 0;
    for (Instruction *I = Phi; ; I=J) {
        J = NULL;
        // Find the user of instruction I that is within loop L.
        for (User *UI : I->users()) { /*}*/
            Instruction *U = cast<Instruction>(UI);
            if (L->contains(U)) {
                if (J) {
                    LLVM_DEBUG(dbgs() << "LSL: not a reduction var because op has two internal uses: " << *I << "\n");
                    return;
                }
                J = U;
            }
        }
        if (!J) {
            LLVM_DEBUG(dbgs() << "LSL: chain prematurely terminated at " << *I << "\n");
            return;
        }
        if (J == Phi) {
            // Found the entire chain.
            break;
        }
        if (opcode) {
            // Check that arithmetic op matches prior arithmetic ops in the chain.
            if (getReduceOpcode(J, I) != opcode) {
                LLVM_DEBUG(dbgs() << "LSL: chain broke at " << *J << " because of wrong opcode\n");
                return;
            }
        }
        else {
            // First arithmetic op in the chain.
            opcode = getReduceOpcode(J, I);
            if (!opcode) {
                LLVM_DEBUG(dbgs() << "LSL: first arithmetic op in chain is uninteresting" << *J << "\n");
                return;
            }
        }
        chain.push_back(J);
    }
    for (chainVector::const_iterator K=chain.begin(); K!=chain.end(); ++K) {
        LLVM_DEBUG(dbgs() << "LSL: marking " << **K << "\n");
        (*K)->setFast(true);
    }
}

bool LowerSIMDLoop::runOnModule(Module &M)
{
    Function *loopinfo_marker = M.getFunction("julia.loopinfo_marker");

    bool Changed = false;
    if (loopinfo_marker)
        Changed |= markLoopInfo(M, loopinfo_marker);

    return Changed;
}

bool LowerSIMDLoop::markLoopInfo(Module &M, Function *marker)
{
    bool Changed = false;
    std::vector<Instruction*> ToDelete;
    for (User *U : marker->users()) {
        Instruction *I = cast<Instruction>(U);
        ToDelete.push_back(I);

        LoopInfo &LI = getAnalysis<LoopInfoWrapperPass>(*I->getParent()->getParent()).getLoopInfo();
        Loop *L = LI.getLoopFor(I->getParent());
        I->removeFromParent();
        if (!L)
            continue;

        LLVM_DEBUG(dbgs() << "LSL: loopinfo marker found\n");
        bool simd = false;
        bool ivdep = false;
        SmallVector<Metadata *, 8> MDs;

        BasicBlock *Lh = L->getHeader();
        LLVM_DEBUG(dbgs() << "LSL: loop header: " << *Lh << "\n");

        // Reserve first location for self reference to the LoopID metadata node.
        TempMDTuple TempNode = MDNode::getTemporary(Lh->getContext(), None);
        MDs.push_back(TempNode.get());

        // Walk `julia.loopinfo` metadata and filter out `julia.simdloop` and `julia.ivdep`
        if (I->hasMetadataOtherThanDebugLoc()) {
            MDNode *JLMD= I->getMetadata("julia.loopinfo");
            if (JLMD) {
                LLVM_DEBUG(dbgs() << "LSL: has julia.loopinfo metadata with " << JLMD->getNumOperands() <<" operands\n");
                for (unsigned i = 0, ie = JLMD->getNumOperands(); i < ie; ++i) {
                    Metadata *Op = JLMD->getOperand(i);
                    const MDString *S = dyn_cast<MDString>(Op);
                    if (S) {
                        LLVM_DEBUG(dbgs() << "LSL: found " << S->getString() << "\n");
                        if (S->getString().startswith("julia")) {
                            if (S->getString().equals("julia.simdloop"))
                                simd = true;
                            if (S->getString().equals("julia.ivdep"))
                                ivdep = true;
                            continue;
                        }
                    }
                    MDs.push_back(Op);
                }
            }
        }

        LLVM_DEBUG(dbgs() << "LSL: simd: " << simd << " ivdep: " << ivdep << "\n");

        MDNode *n = L->getLoopID();
        if (n) {
            // Loop already has a LoopID so copy over Metadata
            // original loop id is operand 0
            for (unsigned i = 1, ie = n->getNumOperands(); i < ie; ++i) {
                Metadata *Op = n->getOperand(i);
                MDs.push_back(Op);
            }
        }
        MDNode *LoopID = MDNode::getDistinct(Lh->getContext(), MDs);
        // Replace the temporary node with a self-reference.
        LoopID->replaceOperandWith(0, LoopID);
        L->setLoopID(LoopID);
        assert(L->getLoopID());

        MDNode *m = MDNode::get(Lh->getContext(), ArrayRef<Metadata *>(LoopID));

        // If ivdep is true we assume that there is no memory dependency between loop iterations
        // This is a fairly strong assumption and does often not hold true for generic code.
        if (ivdep) {
            // Mark memory references so that Loop::isAnnotatedParallel will return true for this loop.
            for (BasicBlock *BB : L->blocks()) {
               for (Instruction &I : *BB) {
                   if (I.mayReadOrWriteMemory()) {
                       I.setMetadata(LLVMContext::MD_mem_parallel_loop_access, m);
                   }
               }
            }
            assert(L->isAnnotatedParallel());
        }

        if (simd) {
            // Mark floating-point reductions as okay to reassociate/commute.
            for (BasicBlock::iterator I = Lh->begin(), E = Lh->end(); I != E; ++I) {
                if (PHINode *Phi = dyn_cast<PHINode>(I))
                    enableUnsafeAlgebraIfReduction(Phi, L);
                else
                    break;
            }
        }

        Changed = true;
    }

    for (Instruction *I : ToDelete)
        I->deleteValue();
    marker->eraseFromParent();

    return Changed;
}

char LowerSIMDLoop::ID = 0;

static RegisterPass<LowerSIMDLoop> X("LowerSIMDLoop", "LowerSIMDLoop Pass",
                                     false /* Only looks at CFG */,
                                     false /* Analysis Pass */);

JL_DLLEXPORT Pass *createLowerSimdLoopPass()
{
    return new LowerSIMDLoop();
}

extern "C" JL_DLLEXPORT void LLVMExtraAddLowerSimdLoopPass(LLVMPassManagerRef PM)
{
    unwrap(PM)->add(createLowerSimdLoopPass());
}

} // namespace llvm

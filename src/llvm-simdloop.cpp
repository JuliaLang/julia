// This file is a part of Julia. License is MIT: https://julialang.org/license

#define DEBUG_TYPE "lower_simd_loop"
#undef DEBUG

// This file defines two entry points:
//     global function annotateSimdLoop: mark a loop as a SIMD loop.
//     createLowerSimdLoopPass: construct LLVM for lowering a marked loop later.

#include "llvm-version.h"
#include "support/dtypes.h"
#include <llvm/Analysis/LoopPass.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Metadata.h>
#include <llvm/Support/Debug.h>

#include <cstdio>

#include "julia_assert.h"

namespace llvm {

// simd loop
static unsigned simd_loop_mdkind = 0;
static MDNode *simd_loop_md = NULL;

/// Mark loop as a SIMD loop.  Return false if loop cannot be marked.
/// incr should be the basic block that increments the loop counter.
bool annotateSimdLoop(BasicBlock *incr)
{
    DEBUG(dbgs() << "LSL: annotating simd_loop\n");
    // Lazy initialization
    if (!simd_loop_mdkind) {
        simd_loop_mdkind = incr->getContext().getMDKindID("simd_loop");
        simd_loop_md = MDNode::get(incr->getContext(), ArrayRef<Metadata*>());
    }
    // Ideally, the decoration would go on the block itself, but LLVM 3.3 does not
    // support putting metadata on blocks.  So instead, put the decoration on the last
    // Add instruction, which (somewhat riskily) is assumed to be the loop increment.
    for (BasicBlock::reverse_iterator ri = incr->rbegin(); ri!=incr->rend(); ++ri) {
        Instruction& i = *ri;
        unsigned op = i.getOpcode();
        if (op==Instruction::Add) {
            if (i.getType()->isIntegerTy()) {
                DEBUG(dbgs() << "LSL: setting simd_loop metadata\n");
                i.setMetadata(simd_loop_mdkind, simd_loop_md);
                return true;
            }
            else {
                return false;
            }
        }
    }
    return false;
}

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

    bool markSIMDLoop(Module &M, Function *marker, bool ivdep);

    /// Check if loop has "simd_loop" annotation.
    /// If present, the annotation is an MDNode attached to an instruction in the loop's latch.
    bool hasSIMDLoopMetadata( Loop *L) const;

    /// If Phi is part of a reduction cycle of FAdd, FSub, FMul or FDiv,
    /// mark the ops as permitting reassociation/commuting.
    /// As of LLVM 4.0, FDiv is not handled by the loop vectorizer
    void enableUnsafeAlgebraIfReduction(PHINode *Phi, Loop *L) const;
};

bool LowerSIMDLoop::hasSIMDLoopMetadata(Loop *L) const
{
    // Note: If a loop has 0 or multiple latch blocks, it's probably not a simd_loop anyway.
    if (BasicBlock *latch = L->getLoopLatch())
        for (BasicBlock::iterator II = latch->begin(), EE = latch->end(); II!=EE; ++II)
            if (II->getMetadata(simd_loop_mdkind))
                return true;
    return false;
}

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
                    DEBUG(dbgs() << "LSL: not a reduction var because op has two internal uses: " << *I << "\n");
                    return;
                }
                J = U;
            }
        }
        if (!J) {
            DEBUG(dbgs() << "LSL: chain prematurely terminated at " << *I << "\n");
            return;
        }
        if (J == Phi) {
            // Found the entire chain.
            break;
        }
        if (opcode) {
            // Check that arithmetic op matches prior arithmetic ops in the chain.
            if (getReduceOpcode(J, I) != opcode) {
                DEBUG(dbgs() << "LSL: chain broke at " << *J << " because of wrong opcode\n");
                return;
            }
        }
        else {
            // First arithmetic op in the chain.
            opcode = getReduceOpcode(J, I);
            if (!opcode) {
                DEBUG(dbgs() << "LSL: first arithmetic op in chain is uninteresting" << *J << "\n");
                return;
            }
        }
        chain.push_back(J);
    }
    for (chainVector::const_iterator K=chain.begin(); K!=chain.end(); ++K) {
        DEBUG(dbgs() << "LSL: marking " << **K << "\n");
        (*K)->setFast(true);
    }
}

bool LowerSIMDLoop::runOnModule(Module &M)
{
    Function *simdloop_marker = M.getFunction("julia.simdloop_marker");
    Function *simdivdep_marker = M.getFunction("julia.simdivdep_marker");

    bool Changed = false;
    if (simdloop_marker)
        Changed |= markSIMDLoop(M, simdloop_marker, false);

    if (simdivdep_marker)
        Changed |= markSIMDLoop(M, simdivdep_marker, true);

    return Changed;
}

bool LowerSIMDLoop::markSIMDLoop(Module &M, Function *marker, bool ivdep)
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

        DEBUG(dbgs() << "LSL: simd_loop found\n");
        DEBUG(dbgs() << "LSL: ivdep is: " << ivdep << "\n");
        BasicBlock *Lh = L->getHeader();
        DEBUG(dbgs() << "LSL: loop header: " << *Lh << "\n");
        MDNode *n = L->getLoopID();
        if (!n) {
            // Loop does not have a LoopID yet, so give it one.
            n = MDNode::get(Lh->getContext(), ArrayRef<Metadata *>(NULL));
            n->replaceOperandWith(0, n);
            L->setLoopID(n);
        }

        assert(L->getLoopID());

        MDNode *m = MDNode::get(Lh->getContext(), ArrayRef<Metadata *>(n));

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

        // Mark floating-point reductions as okay to reassociate/commute.
        for (BasicBlock::iterator I = Lh->begin(), E = Lh->end(); I != E; ++I) {
            if (PHINode *Phi = dyn_cast<PHINode>(I))
                enableUnsafeAlgebraIfReduction(Phi, L);
            else
                break;
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

} // namespace llvm

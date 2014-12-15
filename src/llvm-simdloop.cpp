#define DEBUG_TYPE "lower_simd_loop"
#undef DEBUG

// This file defines two entry points:
//     global function annotateSimdLoop: mark a loop as a SIMD loop.
//     createLowerSimdLoopPass: construct LLVM for lowering a marked loop later.

#include "llvm-version.h"
#include <llvm/Analysis/LoopPass.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Metadata.h>
#include <llvm/Support/Debug.h>
#include <cstdio>

namespace llvm {

// simd loop
static unsigned simd_loop_mdkind = 0;
static MDNode* simd_loop_md = NULL;

/// Mark loop as a SIMD loop.  Return false if loop cannot be marked.
/// incr should be the basic block that increments the loop counter.
bool annotateSimdLoop(BasicBlock* incr)
{
    DEBUG(dbgs() << "LSL: annotating simd_loop\n");
    // Lazy initialization
    if (!simd_loop_mdkind) {
        simd_loop_mdkind = getGlobalContext().getMDKindID("simd_loop");
#ifdef LLVM36
        simd_loop_md = MDNode::get(getGlobalContext(), ArrayRef<Metadata*>());
#else
        simd_loop_md = MDNode::get(getGlobalContext(), ArrayRef<Value*>());
#endif
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

/// Pass that lowers a loop marked by annotateSimdLoop.
/// This pass should run after reduction variables have been converted to phi nodes,
/// otherwise floating-point reductions might not be recognized as such and
/// prevent SIMDization.
struct LowerSIMDLoop: public LoopPass {
    static char ID;
    LowerSIMDLoop() : LoopPass(ID) {}

private:
    /*override*/ bool runOnLoop(Loop *, LPPassManager &LPM);

    /// Check if loop has "simd_loop" annotation.
    /// If present, the annotation is an MDNode attached to an instruction in the loop's latch.
    bool hasSIMDLoopMetadata( Loop *L) const;

    /// If Phi is part of a reduction cycle of FAdd or FMul, mark the ops as permitting reassociation/commuting.
    void enableUnsafeAlgebraIfReduction(PHINode* Phi, Loop* L) const;
};

bool LowerSIMDLoop::hasSIMDLoopMetadata(Loop *L) const
{
    // Note: If a loop has 0 or multiple latch blocks, it's probably not a simd_loop anyway.
    if (BasicBlock* latch = L->getLoopLatch())
        for (BasicBlock::iterator II = latch->begin(), EE = latch->end(); II!=EE; ++II)
            if (II->getMetadata(simd_loop_mdkind))
                return true;
    return false;
}

void LowerSIMDLoop::enableUnsafeAlgebraIfReduction(PHINode* Phi, Loop* L) const
{
    typedef SmallVector<Instruction*, 8> chainVector;
    chainVector chain;
    Instruction *J;
    unsigned opcode = 0;
    for (Instruction *I = Phi; ; I=J) {
        J = NULL;
        // Find the user of instruction I that is within loop L.
#if defined(LLVM_VERSION_MAJOR) && LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR >= 5
        for (User *UI : I->users()) { /*}*/
            Instruction *U = cast<Instruction>(UI);
#else
        for (Value::use_iterator UI = I->use_begin(), UE = I->use_end(); UI != UE; ++UI) {
            Instruction *U = cast<Instruction>(*UI);
#endif
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
        if (J==Phi) {
            // Found the entire chain.
            break;
        }
        if (opcode) {
            // Check that arithmetic op matches prior arithmetic ops in the chain.
            if (J->getOpcode()!=opcode) {
                DEBUG(dbgs() << "LSL: chain broke at " << *J << " because of wrong opcode\n");
                return;
            }
        }
        else {
            // First arithmetic op in the chain.
            opcode = J->getOpcode();
            if (opcode!=Instruction::FAdd && opcode!=Instruction::FMul) {
                DEBUG(dbgs() << "LSL: first arithmetic op in chain is uninteresting" << *J << "\n");
                return;
            }
        }
        chain.push_back(J);
    }
    for (chainVector::const_iterator K=chain.begin(); K!=chain.end(); ++K) {
        DEBUG(dbgs() << "LSL: marking " << **K << "\n");
        (*K)->setHasUnsafeAlgebra(true);
    }
}

bool LowerSIMDLoop::runOnLoop(Loop *L, LPPassManager &LPM)
{
    if (!simd_loop_mdkind)
        return false;           // Fast rejection test.

    if (!hasSIMDLoopMetadata(L))
        return false;

    DEBUG(dbgs() << "LSL: simd_loop found\n");
#if defined(LLVM_VERSION_MAJOR) && LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR >= 4
    MDNode* n = L->getLoopID();
    if (!n) {
        // Loop does not have a LoopID yet, so give it one.
#ifdef LLVM36
        n = MDNode::get(getGlobalContext(), ArrayRef<Metadata*>(NULL));
#else
        n = MDNode::get(getGlobalContext(), ArrayRef<Value*>(NULL));
#endif
        n->replaceOperandWith(0,n);
        L->setLoopID(n);
    }
#else
    MDNode* n = MDNode::get(getGlobalContext(), ArrayRef<Value*>());
    L->getLoopLatch()->getTerminator()->setMetadata("llvm.loop.parallel", n);
#endif
#ifdef LLVM36
    MDNode* m = MDNode::get(getGlobalContext(), ArrayRef<Metadata*>(n));
#else
    MDNode* m = MDNode::get(getGlobalContext(), ArrayRef<Value*>(n));
#endif

    // Mark memory references so that Loop::isAnnotatedParallel will return true for this loop.
    for(Loop::block_iterator BBI = L->block_begin(), E=L->block_end(); BBI!=E; ++BBI)
        for (BasicBlock::iterator I = (*BBI)->begin(), EE = (*BBI)->end(); I!=EE; ++I)
            if (I->mayReadOrWriteMemory())
                I->setMetadata("llvm.mem.parallel_loop_access", m);
    assert(L->isAnnotatedParallel());

    // Mark floating-point reductions as okay to reassociate/commute.
    BasicBlock* Lh = L->getHeader();
    DEBUG(dbgs() << "LSL: loop header: " << *Lh << "\n");
    for (BasicBlock::iterator I = Lh->begin(), E = Lh->end(); I!=E; ++I)
        if (PHINode *Phi = dyn_cast<PHINode>(I))
            enableUnsafeAlgebraIfReduction(Phi,L);

    return true;
}

char LowerSIMDLoop::ID = 0;

static RegisterPass<LowerSIMDLoop> X("LowerSIMDLoop", "LowerSIMDLoop Pass",
                                     false /* Only looks at CFG */,
                                     false /* Analysis Pass */);

Pass* createLowerSimdLoopPass() {
    return new LowerSIMDLoop();
}

} // namespace llvm

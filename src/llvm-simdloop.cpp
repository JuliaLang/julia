// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-version.h"
#include "passes.h"

// This file defines a LLVM pass that:
// 1. Set's loop information in form of metadata
// 2. If the metadata contains `julia.simdloop` finds reduction chains and marks
//    floating-point operations as fast-math. `See enableUnsafeAlgebraIfReduction`.
// 3. If the metadata contains `julia.ivdep` marks all memory accesses in the loop
//    as independent of each other.
//
// The pass hinges on a call to a marker function that has metadata attached to it.

#include "support/dtypes.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

#include <llvm/ADT/Statistic.h>
#include <llvm/Analysis/LoopPass.h>
#include <llvm/Analysis/OptimizationRemarkEmitter.h>
#include <llvm/Analysis/MemorySSA.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/Debug.h>

#include "julia_assert.h"

#define DEBUG_TYPE "lower_simd_loop"

using namespace llvm;

STATISTIC(TotalMarkedLoops, "Total number of loops marked with simdloop");
STATISTIC(IVDepLoops, "Number of loops with no loop-carried dependencies");
STATISTIC(SimdLoops, "Number of loops with SIMD instructions");
STATISTIC(IVDepInstructions, "Number of instructions marked ivdep");
STATISTIC(ReductionChains, "Number of reduction chains folded");
STATISTIC(ReductionChainLength, "Total sum of instructions folded from reduction chain");
STATISTIC(MaxChainLength, "Max length of reduction chain");
STATISTIC(AddChains, "Addition reduction chains");
STATISTIC(MulChains, "Multiply reduction chains");

#ifndef __clang_gcanalyzer__
#define REMARK(remark) ORE.emit(remark)
#else
#define REMARK(remark) (void) 0;
#endif
namespace {

static unsigned getReduceOpcode(Instruction *J, Instruction *operand) JL_NOTSAFEPOINT
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

/// If Phi is part of a reduction cycle of FAdd, FSub, FMul or FDiv,
/// mark the ops as permitting reassociation/commuting.
/// As of LLVM 4.0, FDiv is not handled by the loop vectorizer
static void enableUnsafeAlgebraIfReduction(PHINode *Phi, Loop &L, OptimizationRemarkEmitter &ORE, ScalarEvolution *SE) JL_NOTSAFEPOINT
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
            if (L.contains(U)) {
                if (J) {
                    LLVM_DEBUG(dbgs() << "LSL: not a reduction var because op has two internal uses: " << *I << "\n");
                    REMARK([&]() {
                        return OptimizationRemarkMissed(DEBUG_TYPE, "NotReductionVar", U)
                               << "not a reduction variable because operation has two internal uses";
                    });
                    return;
                }
                J = U;
            }
        }
        if (!J) {
            LLVM_DEBUG(dbgs() << "LSL: chain prematurely terminated at " << *I << "\n");
            REMARK([&]() {
                return OptimizationRemarkMissed(DEBUG_TYPE, "ChainPrematurelyTerminated", I)
                       << "chain prematurely terminated at " << ore::NV("Instruction", I);
            });
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
                REMARK([&](){
                    return OptimizationRemarkMissed(DEBUG_TYPE, "ChainBroke", J)
                           << "chain broke at " << ore::NV("Instruction", J) << " because of wrong opcode";
                });
                return;
            }
        }
        else {
            // First arithmetic op in the chain.
            opcode = getReduceOpcode(J, I);
            if (!opcode) {
                LLVM_DEBUG(dbgs() << "LSL: first arithmetic op in chain is uninteresting" << *J << "\n");
                REMARK([&]() {
                    return OptimizationRemarkMissed(DEBUG_TYPE, "FirstArithmeticOpInChainIsUninteresting", J)
                           << "first arithmetic op in chain is uninteresting";
                });
                return;
            }
        }
        chain.push_back(J);
    }
    switch (opcode) {
        case Instruction::FAdd:
            ++AddChains;
            break;
        case Instruction::FMul:
            ++MulChains;
            break;
    }
    ++ReductionChains;
    int length = 0;
    for (chainVector::const_iterator K=chain.begin(); K!=chain.end(); ++K) {
        LLVM_DEBUG(dbgs() << "LSL: marking " << **K << "\n");
        REMARK([&]() {
            return OptimizationRemark(DEBUG_TYPE, "MarkedUnsafeAlgebra", *K)
                   << "marked unsafe algebra on " << ore::NV("Instruction", *K);
        });
        (*K)->setHasAllowReassoc(true);
        (*K)->setHasAllowContract(true);
        if (SE)
            SE->forgetValue(*K);
        ++length;
    }
    ReductionChainLength += length;
    MaxChainLength.updateMax(length);
}

static bool processLoop(Loop &L, OptimizationRemarkEmitter &ORE, ScalarEvolution *SE) JL_NOTSAFEPOINT
{
    MDNode *LoopID = L.getLoopID();
    if (!LoopID)
        return false;
    bool simd = false;
    bool ivdep = false;

    BasicBlock *Lh = L.getHeader();
    LLVM_DEBUG(dbgs() << "LSL: loop header: " << *Lh << "\n");

    SmallVector<Metadata*, 4> MDs(1);
    // First Operand is self-reference
    // Drop `julia.` prefixes
    for (unsigned i = 1, ie = LoopID->getNumOperands(); i < ie; ++i) {
        Metadata *Op = LoopID->getOperand(i);
        const MDString *S = dyn_cast<MDString>(Op);
        if (S) {
            LLVM_DEBUG(dbgs() << "LSL: found " << S->getString() << "\n");
            if (S->getString().starts_with("julia")) {
                if (S->getString().equals("julia.simdloop"))
                    simd = true;
                if (S->getString().equals("julia.ivdep"))
                    ivdep = true;
                continue;
            }
        }
        MDs.push_back(Op);
    }

    LLVM_DEBUG(dbgs() << "LSL: simd: " << simd << " ivdep: " << ivdep << "\n");
    if (!simd && !ivdep)
        return false;
    ++TotalMarkedLoops;
    LLVMContext &Context = L.getHeader()->getContext();
    LoopID = MDNode::get(Context, MDs);
    // Set operand 0 to refer to the loop id itself
    LoopID->replaceOperandWith(0, LoopID);
    L.setLoopID(LoopID);

    REMARK([&]() {
        return OptimizationRemarkAnalysis(DEBUG_TYPE, "Loop SIMD Flags", L.getStartLoc(), L.getHeader())
            << "Loop marked for SIMD vectorization with flags { \"simd\": " << (simd ? "true" : "false") << ", \"ivdep\": " << (ivdep ? "true" : "false") << " }";
    });

    // If ivdep is true we assume that there is no memory dependency between loop iterations
    // This is a fairly strong assumption and does often not hold true for generic code.
    if (ivdep) {
        ++IVDepLoops;
        MDNode *m = MDNode::get(Lh->getContext(), ArrayRef<Metadata *>(LoopID));
        // Mark memory references so that Loop::isAnnotatedParallel will return true for this loop.
        for (BasicBlock *BB : L.blocks()) {
            for (Instruction &I : *BB) {
                if (I.mayReadOrWriteMemory()) {
                    ++IVDepInstructions;
                    I.setMetadata(LLVMContext::MD_mem_parallel_loop_access, m);
                }
            }
        }
        assert(L.isAnnotatedParallel());
    }

    if (simd) {
        ++SimdLoops;
        // Mark floating-point reductions as okay to reassociate/commute.
        for (BasicBlock::iterator I = Lh->begin(), E = Lh->end(); I != E; ++I) {
            if (PHINode *Phi = dyn_cast<PHINode>(I))
                enableUnsafeAlgebraIfReduction(Phi, L, ORE, SE);
            else
                break;
        }

        if (SE)
#if JL_LLVM_VERSION >= 160000
            SE->forgetLoopDispositions();
#else
            SE->forgetLoopDispositions(&L);
#endif
    }

#ifdef JL_VERIFY_PASSES
    assert(!verifyLLVMIR(L));
#endif
    return true;
}

} // end anonymous namespace


/// This pass should run after reduction variables have been converted to phi nodes,
/// otherwise floating-point reductions might not be recognized as such and
/// prevent SIMDization.


PreservedAnalyses LowerSIMDLoopPass::run(Loop &L, LoopAnalysisManager &AM,
                          LoopStandardAnalysisResults &AR, LPMUpdater &U)

{
    OptimizationRemarkEmitter ORE(L.getHeader()->getParent());
    if (processLoop(L, ORE, &AR.SE)) {
#ifdef JL_DEBUG_BUILD
        if (AR.MSSA)
            AR.MSSA->verifyMemorySSA();
#endif
        auto preserved = getLoopPassPreservedAnalyses();
        preserved.preserveSet<CFGAnalyses>();
        preserved.preserve<MemorySSAAnalysis>();
        return preserved;
    }

    return PreservedAnalyses::all();
}

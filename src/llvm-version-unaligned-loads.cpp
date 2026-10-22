// This file is a part of Julia. License is MIT: https://julialang.org/license

// This pass versions under-aligned scalar loads on alignment, for targets that cannot perform
// them quickly. `unsafe_load` has to assume an arbitrary pointer is misaligned, so it emits
// `align 1` loads, which a target without fast misaligned access (e.g. RISC-V cores that trap
// on misaligned accesses) has to split into byte loads, even when the address is aligned at run
// time. For each such load, this pass emits a check of the address and a naturally-aligned load
// for when it succeeds, keeping the original load for when it does not.
//
// A single check covers a run of loads of the same size in a basic block whose addresses differ
// from the first by multiples of that size, so that e.g. an unrolled loop over a buffer checks
// once per iteration. The loads of such a run are moved up to the first one, so a run ends at any
// call, store, atomic or volatile access; it may cross other loads and arithmetic.
//
// This pass is intended to run late in the pipeline, after alignment inference, so that only
// loads whose alignment cannot be proven are versioned.

#include "llvm-version.h"
#include "passes.h"

#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/SmallPtrSet.h>
#include <llvm/ADT/Statistic.h>
#include <llvm/Analysis/ScalarEvolution.h>
#include <llvm/Analysis/TargetTransformInfo.h>
#include <llvm/Analysis/ValueTracking.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Instructions.h>
#include <llvm/Support/KnownBits.h>
#include <llvm/Transforms/Utils/BasicBlockUtils.h>

#define DEBUG_TYPE "version_unaligned_loads"

using namespace llvm;

STATISTIC(VersionedLoads, "Number of loads versioned on alignment");
STATISTIC(VersionChecks, "Number of alignment checks inserted");
STATISTIC(AlignedLoads, "Number of loads proven aligned from known address bits");

// Bounds on the number of loads covered by one check, which limits the live ranges of the
// moved loads, and on the number of instructions scanned to find them, which limits the cost of
// the pass on large blocks.
static const unsigned MaxRunLength = 16;
static const unsigned MaxRunScan = 64;

// Returns the access size of `I` if it is a load that may benefit from versioning.
static unsigned candidateSize(const Instruction &I, const DataLayout &DL,
                              const TargetTransformInfo &TTI)
{
    auto *LI = dyn_cast<LoadInst>(&I);
    if (!LI || !LI->isSimple() || LI->getPointerAddressSpace() != 0 ||
        LI->hasMetadata(LLVMContext::MD_nontemporal))
        return 0;
    Type *T = LI->getType();
    if (!(T->isIntegerTy(16) || T->isIntegerTy(32) || T->isIntegerTy(64) ||
          T->isFloatTy() || T->isDoubleTy()))
        return 0;
    unsigned Size = DL.getTypeStoreSize(T).getFixedValue();
    if (LI->getAlign().value() >= Size)
        return 0;
    unsigned Fast = 0;
    if (TTI.allowsMisalignedMemoryAccesses(LI->getContext(), Size * 8, 0, LI->getAlign(), &Fast) &&
        Fast)
        return 0;
    return Size;
}

// Instructions that end a run: the loads of a run are moved up to the first one, which must not
// cross anything that writes memory, synchronizes, or may not transfer execution to the next
// instruction.
static bool isRunBarrier(const Instruction &I)
{
    if (isa<CallBase>(I) || I.isTerminator() || I.mayWriteToMemory() || I.isAtomic())
        return true;
    if (auto *LI = dyn_cast<LoadInst>(&I))
        return LI->isVolatile();
    return false;
}

// Collects the instructions that compute `V` and must be moved before `Pt`, in program order.
// Returns false if one of them cannot be moved.
static bool collectHoist(Value *V, Instruction *Pt, SmallVectorImpl<Instruction *> &Hoist,
                         SmallPtrSetImpl<Instruction *> &Seen)
{
    auto *I = dyn_cast<Instruction>(V);
    if (!I || I->getParent() != Pt->getParent() || I->comesBefore(Pt) || !Seen.insert(I).second)
        return true;
    if (isa<PHINode>(I) || I->mayReadOrWriteMemory() ||
        !isSafeToSpeculativelyExecute(I, Pt, nullptr, nullptr, nullptr, true,
                                      /*IgnoreUBImplyingAttrs=*/false))
        return false;
    for (Value *Op : I->operands())
        if (!collectHoist(Op, Pt, Hoist, Seen))
            return false;
    Hoist.push_back(I);
    return true;
}

namespace {
struct Run {
    SmallVector<LoadInst *, 8> Loads;
    SmallVector<Instruction *, 16> Hoist; // address computations to move before the first load
    unsigned Size;
};
}

static void versionRun(Run &R, const DataLayout &DL)
{
    LoadInst *First = R.Loads.front();
    // An earlier run in the same block may already have moved some of these before its own
    // first load, possibly into another block, and thus before this one.
    llvm::erase_if(R.Hoist, [First](Instruction *I) {
        return I->getParent() != First->getParent() || I->comesBefore(First);
    });
    llvm::sort(R.Hoist, [](Instruction *A, Instruction *B) { return A->comesBefore(B); });
    for (Instruction *I : R.Hoist)
        I->moveBefore(First->getIterator());

    IRBuilder<> Builder(First);
    Value *Ptr = First->getPointerOperand();
    Type *IntPtrTy = DL.getIntPtrType(Ptr->getType());
    Value *Misalignment = Builder.CreateAnd(Builder.CreatePtrToInt(Ptr, IntPtrTy), R.Size - 1);
    Value *IsAligned = Builder.CreateICmpEQ(Misalignment, ConstantInt::get(IntPtrTy, 0), "is_aligned");
    Instruction *AlignedTerm, *UnalignedTerm;
    SplitBlockAndInsertIfThenElse(IsAligned, First->getIterator(), &AlignedTerm, &UnalignedTerm);
    BasicBlock *Tail = AlignedTerm->getSuccessor(0);
    for (LoadInst *LI : R.Loads) {
        auto *Aligned = cast<LoadInst>(LI->clone());
        Aligned->setAlignment(Align(R.Size));
        Aligned->insertBefore(AlignedTerm->getIterator());
        auto *Unaligned = LI->clone();
        Unaligned->insertBefore(UnalignedTerm->getIterator());
        PHINode *Phi = PHINode::Create(LI->getType(), 2, "", Tail->begin());
        Phi->takeName(LI);
        Phi->addIncoming(Aligned, Aligned->getParent());
        Phi->addIncoming(Unaligned, Unaligned->getParent());
        Phi->setDebugLoc(LI->getDebugLoc());
        LI->replaceAllUsesWith(Phi);
        LI->eraseFromParent();
    }
    VersionedLoads += R.Loads.size();
    ++VersionChecks;
}

static bool versionUnalignedLoads(Function &F, FunctionAnalysisManager &AM)
{
    if (F.hasOptSize())
        return false;
    const DataLayout &DL = F.getDataLayout();
    auto &TTI = AM.getResult<TargetIRAnalysis>(F);
    bool Changed = false;

    // Gather candidates, resolving those that known address bits decide.
    SmallVector<LoadInst *, 0> Candidates;
    DenseMap<LoadInst *, unsigned> Sizes;
    for (Instruction &I : instructions(F)) {
        unsigned Size = candidateSize(I, DL, TTI);
        if (!Size)
            continue;
        auto *LI = cast<LoadInst>(&I);
        KnownBits Known = computeKnownBits(LI->getPointerOperand(), DL);
        unsigned LowBits = Log2_32(Size);
        if (Known.countMinTrailingZeros() >= LowBits) {
            LI->setAlignment(Align(Size));
            ++AlignedLoads;
            Changed = true;
        }
        else if (Known.One.extractBits(LowBits, 0).isZero()) {
            Candidates.push_back(LI);
            Sizes[LI] = Size;
        }
    }
    if (Candidates.empty())
        return Changed;

    // Partition the candidates into runs, in program order. A load that cannot join a run, e.g.
    // because its address computation cannot be moved, remains available to start a later run.
    ScalarEvolution *SE = nullptr;
    SmallPtrSet<LoadInst *, 16> Taken;
    SmallVector<Run, 0> Runs;
    for (LoadInst *First : Candidates) {
        if (Taken.count(First))
            continue;
        Run R{{First}, {}, Sizes[First]};
        SmallPtrSet<Instruction *, 16> Seen;
        const SCEV *Base = nullptr;
        unsigned Scanned = 0;
        for (auto It = std::next(First->getIterator());
             It != First->getParent()->end() && R.Loads.size() < MaxRunLength &&
             Scanned < MaxRunScan && !isRunBarrier(*It);
             ++It, ++Scanned) {
            auto *LI = dyn_cast<LoadInst>(&*It);
            if (!LI || Taken.count(LI) || Sizes.lookup(LI) != R.Size)
                continue;
            if (!Base) {
                if (!SE)
                    SE = &AM.getResult<ScalarEvolutionAnalysis>(F);
                Base = SE->getSCEV(First->getPointerOperand());
            }
            std::optional<APInt> Diff =
                SE->computeConstantDifference(SE->getSCEV(LI->getPointerOperand()), Base);
            if (!Diff || Diff->countr_zero() < Log2_32(R.Size))
                continue;
            size_t N = R.Hoist.size();
            SmallPtrSet<Instruction *, 16> SeenBefore = Seen;
            if (collectHoist(LI->getPointerOperand(), First, R.Hoist, Seen)) {
                R.Loads.push_back(LI);
            }
            else {
                Seen = std::move(SeenBefore);
                R.Hoist.truncate(N);
            }
        }
        for (LoadInst *LI : R.Loads)
            Taken.insert(LI);
        Runs.push_back(std::move(R));
    }

    // All runs are formed before the IR changes, since versioning a run splits its block.
    for (Run &R : Runs)
        versionRun(R, DL);
    return true;
}

PreservedAnalyses VersionUnalignedLoadsPass::run(Function &F, FunctionAnalysisManager &AM)
{
    if (versionUnalignedLoads(F, AM))
        return PreservedAnalyses::none();
    return PreservedAnalyses::all();
}

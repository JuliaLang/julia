// This file is a part of Julia. License is MIT: https://julialang.org/license

// NewSink - Sink instructions to paths where they are actually needed
//
// Useful for Julia's bounds checking where error message data is computed
// eagerly but only needed when bounds checks fail.
//
// Algorithm:
//   1. Pure values: sink to nearest common dominator of all use blocks
//   2. Memory reads: sink to an immediate successor that dominates all uses
//   3. Memory writes: sink to a successor verified safe by MemorySSA,
//      splitting critical edges for multi-predecessor targets
//   4. Iterate to fixed point (chains of sinkable instructions)
//
// Example:
//   Before:                              After:
//     store i64 %i, ptr %tuple             %cmp = icmp ult %i, %bound
//     %cmp = icmp ult %i, %bound           br i1 %cmp, label %ok, label %error
//     br i1 %cmp, label %ok, label %error  error:
//   error:                                   store i64 %i, ptr %tuple
//     call @throw(%tuple)                    call @throw(%tuple)
//     unreachable                            unreachable

#include "llvm-version.h"

// GCC false positive about SmallDenseMap in BatchAAResults
#ifdef _COMPILER_GCC_
#pragma GCC diagnostic ignored "-Wmaybe-uninitialized"
#endif

#include "passes.h"

#include <llvm/ADT/PostOrderIterator.h>
#include <llvm/ADT/SmallPtrSet.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/Statistic.h>
#include <llvm/Analysis/AliasAnalysis.h>
#include <llvm/Analysis/MemoryLocation.h>
#include <llvm/Analysis/MemorySSA.h>
#include <llvm/Analysis/MemorySSAUpdater.h>
#include <llvm/Analysis/OptimizationRemarkEmitter.h>
#include <llvm/Analysis/TargetLibraryInfo.h>
#include <llvm/Analysis/ValueTracking.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/Analysis/PostDominators.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/Debug.h>
#include <llvm/Transforms/Utils/BasicBlockUtils.h>

#include "julia.h"

#define DEBUG_TYPE "new-sink"

using namespace llvm;

STATISTIC(NumSunk, "Number of instructions sunk");
STATISTIC(NumSunkToNoReturn, "Number of instructions sunk to noreturn paths");
STATISTIC(NumSunkGeneral, "Number of instructions sunk to general paths");
STATISTIC(NumIterations, "Total sinking iterations across all functions");
STATISTIC(NumAbortedMSSAWalk, "MSSA walks aborted due to limit");
STATISTIC(NumAbortedIterations, "Functions where iteration limit reached");

// Limits to bound worst-case complexity (similar to DSE)
static cl::opt<unsigned> MemorySSAScanLimit(
    "newsink-memoryssa-scanlimit", cl::init(150), cl::Hidden,
    cl::desc("The number of memory accesses to scan for sinking (default = 150)"));

static cl::opt<unsigned> MaxSinkingIterations(
    "newsink-max-iterations", cl::init(32), cl::Hidden,
    cl::desc("Maximum fixed-point iterations for sinking (default = 32)"));

namespace {

// Check if a memory write can be moved (not volatile, not ordered atomic).
// Follows DSE's isRemovable pattern.
static bool isSinkableMemoryWrite(Instruction *I)
{
    if (auto *SI = dyn_cast<StoreInst>(I))
        return SI->isUnordered();

    if (auto *CB = dyn_cast<CallBase>(I)) {
        // Memory intrinsics: just check volatile
        if (auto *MI = dyn_cast<MemIntrinsic>(CB))
            return !MI->isVolatile();

        // Other calls with a known write destination: must have no uses
        // (can't sink a value producer via the write path), must return,
        // and must not throw (sinking past a branch could skip the throw).
        return CB->use_empty() && CB->willReturn() && CB->doesNotThrow() &&
               !CB->isTerminator();
    }

    return false;
}

static bool isNoReturnBlock(const BasicBlock *BB)
{
    return isa<UnreachableInst>(BB->getTerminator());
}

// State struct holding all analysis dependencies, following DSE's pattern.
struct NewSinkState {
    Function &F;
    DominatorTree &DT;
    PostDominatorTree &PDT;
    LoopInfo &LI;
    AliasAnalysis &AA;
    BatchAAResults BatchAA; // Caches AA queries
    MemorySSA &MSSA;
    MemorySSAUpdater MSSAUpdater; // Keeps MSSA consistent after modifications
    const TargetLibraryInfo &TLI;
    OptimizationRemarkEmitter &ORE;

    // Precomputed: blocks where all paths lead to unreachable
    SmallPtrSet<BasicBlock *, 16> NoReturnBlocks;

    NewSinkState(Function &F, DominatorTree &DT, PostDominatorTree &PDT,
                 LoopInfo &LI, AliasAnalysis &AA, MemorySSA &MSSA,
                 const TargetLibraryInfo &TLI, OptimizationRemarkEmitter &ORE)
      : F(F),
        DT(DT),
        PDT(PDT),
        LI(LI),
        AA(AA),
        BatchAA(AA),
        MSSA(MSSA),
        MSSAUpdater(&MSSA),
        TLI(TLI),
        ORE(ORE)
    {
        collectNoReturnBlocks();
    }

    std::optional<MemoryLocation> getLocForWrite(Instruction *I) const
    {
        if (!I->mayWriteToMemory())
            return std::nullopt;
        if (auto *CB = dyn_cast<CallBase>(I))
            return MemoryLocation::getForDest(CB, TLI);
        return MemoryLocation::getOrNone(I);
    }

    bool isMemoryWrite(Instruction *I) const { return getLocForWrite(I).has_value(); }

    Value *getWritePointer(Instruction *I) const
    {
        if (auto *SI = dyn_cast<StoreInst>(I))
            return SI->getPointerOperand();

        if (auto *CB = dyn_cast<CallBase>(I)) {
            if (auto Loc = MemoryLocation::getForDest(CB, TLI))
                return const_cast<Value *>(Loc->Ptr);
            return nullptr;
        }

        return nullptr;
    }

    void collectNoReturnBlocks();
    bool canSinkInstruction(Instruction *I) const;
    BasicBlock *canSinkMemoryWriteToSuccessor(Instruction *WriteInst,
                                              BasicBlock *TargetSucc);
    BasicBlock *findSinkTargetForValue(Instruction *I);
    BasicBlock *findSinkTargetForWrite(Instruction *I);
    bool trySinkInstruction(Instruction *I);
    bool run();
};

// Collect blocks where all paths lead to unreachable.
// Uses a worklist to propagate backward from unreachable terminators in O(V+E).
void NewSinkState::collectNoReturnBlocks()
{
    SmallVector<BasicBlock *, 8> Worklist;
    for (BasicBlock &BB : F) {
        if (!DT.isReachableFromEntry(&BB))
            continue;
        if (isNoReturnBlock(&BB)) {
            NoReturnBlocks.insert(&BB);
            Worklist.push_back(&BB);
        }
    }

    while (!Worklist.empty()) {
        BasicBlock *BB = Worklist.pop_back_val();
        for (BasicBlock *Pred : predecessors(BB)) {
            if (!DT.isReachableFromEntry(Pred))
                continue;
            if (NoReturnBlocks.count(Pred))
                continue;
            if (succ_empty(Pred))
                continue;

            if (llvm::all_of(successors(Pred), [&](BasicBlock *Succ) {
                    return NoReturnBlocks.count(Succ);
                })) {
                NoReturnBlocks.insert(Pred);
                Worklist.push_back(Pred);
            }
        }
    }
}

// Check if a memory write can be safely sunk to target successor.
// Uses MemorySSA to verify no reads observe the wrong value.
BasicBlock *NewSinkState::canSinkMemoryWriteToSuccessor(Instruction *WriteInst,
                                                        BasicBlock *TargetSucc)
{
    auto MaybeLoc = getLocForWrite(WriteInst);
    if (!MaybeLoc)
        return nullptr;
    MemoryLocation WriteLoc = *MaybeLoc;

    BasicBlock *CurrentBB = WriteInst->getParent();
    assert(llvm::is_contained(successors(CurrentBB), TargetSucc) &&
           "TargetSucc must be an immediate successor");

    Value *WritePtr = getWritePointer(WriteInst);
    if (!WritePtr)
        return nullptr;

    const Value *UnderlyingObj = getUnderlyingObject(WritePtr);
    if (!isIdentifiedFunctionLocal(UnderlyingObj))
        return nullptr;

    MemoryAccess *WriteDef = MSSA.getMemoryAccess(WriteInst);
    if (!WriteDef)
        return nullptr;

    // Walk forward from our MemoryDef to check that no reader outside the
    // target would observe the wrong value if we sink this write.
    // Loop iteration safety is handled separately in findSinkTargetForWrite.
    SmallVector<MemoryAccess *, 16> Worklist;
    SmallPtrSet<MemoryAccess *, 16> Visited;

    for (User *U : WriteDef->users())
        if (auto *MA = dyn_cast<MemoryAccess>(U))
            Worklist.push_back(MA);

    while (!Worklist.empty()) {
        // Bail out if we've visited too many accesses
        if (Visited.size() >= MemorySSAScanLimit) {
            ++NumAbortedMSSAWalk;
            LLVM_DEBUG(dbgs() << "NewSink: MSSA scan limit reached\n");
            return nullptr;
        }

        MemoryAccess *MA = Worklist.pop_back_val();
        if (!Visited.insert(MA).second)
            continue;

        BasicBlock *MABB = MA->getBlock();

        if (isa<MemoryPhi>(MA)) {
            for (User *U : MA->users())
                if (auto *UserMA = dyn_cast<MemoryAccess>(U))
                    Worklist.push_back(UserMA);
            continue;
        }

        Instruction *MemInst = nullptr;
        if (auto *MU = dyn_cast<MemoryUseOrDef>(MA))
            MemInst = MU->getMemoryInst();

        if (!MemInst || MemInst == WriteInst)
            continue;

        // Reads in the target or blocks dominated by it will see the sunk write.
        if (MABB == TargetSucc || DT.dominates(TargetSucc, MABB))
            continue;

        // In the source block, any aliasing access blocks sinking.
        if (MABB == CurrentBB) {
            ModRefInfo MRI = BatchAA.getModRefInfo(MemInst, WriteLoc);
            if (isModOrRefSet(MRI))
                return nullptr;
            if (auto *UserDef = dyn_cast<MemoryDef>(MA)) {
                for (User *U : UserDef->users())
                    if (auto *UserMA = dyn_cast<MemoryAccess>(U))
                        Worklist.push_back(UserMA);
            }
            continue;
        }

        ModRefInfo MRI = BatchAA.getModRefInfo(MemInst, WriteLoc);

        if (!isRefSet(MRI)) {
            // Doesn't read our location. Follow through MemoryDefs because
            // MSSA chains all memory ops, not just aliasing ones.
            if (auto *UserDef = dyn_cast<MemoryDef>(MA)) {
                for (User *U : UserDef->users())
                    if (auto *UserMA = dyn_cast<MemoryAccess>(U))
                        Worklist.push_back(UserMA);
            }
            continue;
        }

        return nullptr;
    }

    return TargetSucc;
}

// Check that nothing clobbers a memory read between it and the block terminator.
static bool canSinkMemoryRead(Instruction *I, AliasAnalysis &AA)
{
    BasicBlock *BB = I->getParent();
    Instruction *Term = BB->getTerminator();

    if (I->getNextNode() == Term)
        return true;

    // For loads, use the efficient MemoryLocation-based range query.
    if (auto *LI = dyn_cast<LoadInst>(I)) {
        MemoryLocation Loc = MemoryLocation::get(LI);
        return !AA.canInstructionRangeModRef(*LI->getNextNode(), *Term, Loc,
                                              ModRefInfo::Mod);
    }

    // For readonly calls, check each intervening instruction individually
    // (no single MemoryLocation to use with canInstructionRangeModRef).
    auto *CB = dyn_cast<CallBase>(I);
    if (!CB)
        return false;

    for (Instruction *Cur = CB->getNextNode(); Cur != Term;
         Cur = Cur->getNextNode()) {
        if (!Cur->mayWriteToMemory())
            continue;

        if (auto *CurCB = dyn_cast<CallBase>(Cur)) {
            if (isModSet(AA.getModRefInfo(CurCB, CB)))
                return false;
            continue;
        }

        if (auto Loc = MemoryLocation::getOrNone(Cur)) {
            if (isRefSet(AA.getModRefInfo(CB, *Loc)))
                return false;
            continue;
        }

        // Unknown memory-writing instruction — conservatively block sinking.
        return false;
    }

    return true;
}

// Check that nothing modifies the memcpy/memmove source between it and the terminator.
static bool canSinkMemTransfer(MemTransferInst *MTI, AliasAnalysis &AA)
{
    BasicBlock *BB = MTI->getParent();
    Instruction *Term = BB->getTerminator();

    if (MTI->getNextNode() == Term)
        return true;

    // Check that nothing modifies (or terminates) the source location
    MemoryLocation SourceLoc = MemoryLocation::getForSource(MTI);
    return !AA.canInstructionRangeModRef(*MTI->getNextNode(), *Term, SourceLoc,
                                         ModRefInfo::Mod);
}

static bool operandsDominateBlock(Instruction *I, BasicBlock *Target, DominatorTree &DT)
{
    for (Use &Op : I->operands()) {
        if (auto *OpInst = dyn_cast<Instruction>(Op.get())) {
            // The operand must dominate the target block
            if (!DT.dominates(OpInst, Target))
                return false;
        }
        // Arguments and constants always dominate
    }
    return true;
}

// Find sink target for value-producing instructions.
// Memory reads sink to an immediate successor; pure values sink to the NCD.
BasicBlock *NewSinkState::findSinkTargetForValue(Instruction *I)
{
    assert(!I->use_empty() && "Value must have uses");
    BasicBlock *CurrentBB = I->getParent();

    SmallPtrSet<BasicBlock *, 8> UserBlocks;
    for (Use &U : I->uses()) {
        auto *UserInst = cast<Instruction>(U.getUser());
        BasicBlock *UseBlock = UserInst->getParent();
        if (auto *PN = dyn_cast<PHINode>(UserInst))
            UseBlock = PN->getIncomingBlock(
                PHINode::getIncomingValueNumForOperand(U.getOperandNo()));
        if (DT.isReachableFromEntry(UseBlock))
            UserBlocks.insert(UseBlock);
    }

    if (UserBlocks.empty())
        return nullptr;

    // For memory reads, we can only check for clobbers in the source block,
    // so the target must be an immediate successor. Find which successor
    // dominates all uses instead of computing a potentially distant NCD.
    if (I->mayReadFromMemory()) {
        BasicBlock *Target = nullptr;
        for (BasicBlock *Succ : successors(CurrentBB)) {
            // Skip self-loops and backedges — sinking a load backward
            // in the CFG could move it before a store it depends on.
            if (Succ == CurrentBB || DT.dominates(Succ, CurrentBB))
                continue;
            bool DominatesAll = llvm::all_of(UserBlocks, [&](BasicBlock *UB) {
                return UB == Succ || DT.dominates(Succ, UB);
            });
            if (DominatesAll) {
                if (Target)
                    return nullptr; // ambiguous
                Target = Succ;
            }
        }
        if (!Target)
            return nullptr;
        if (!canSinkMemoryRead(I, AA))
            return nullptr;
        return Target;
    }

    // For non-memory instructions, sink to the NCD of all use blocks.
    BasicBlock *Target = nullptr;
    for (BasicBlock *UB : UserBlocks) {
        if (Target)
            Target = DT.findNearestCommonDominator(Target, UB);
        else
            Target = UB;
    }
    if (Target == CurrentBB)
        Target = nullptr;

    return Target;
}

// Find sink target for memory writes (stores, memset, memcpy, etc.)
// Uses alias analysis (MemorySSA) to verify no reads observe the wrong value.
BasicBlock *NewSinkState::findSinkTargetForWrite(Instruction *I)
{
    assert(I->use_empty() && "Sinkable memory writes should have no uses");

    BasicBlock *CurrentBB = I->getParent();

    if (succ_size(CurrentBB) <= 1)
        return nullptr;

    // For memcpy/memmove, verify the source isn't modified before the terminator.
    if (auto *MTI = dyn_cast<MemTransferInst>(I)) {
        if (!canSinkMemTransfer(MTI, AA))
            return nullptr;
    }

    Loop *CurLoop = LI.getLoopFor(CurrentBB);

    Value *WritePtr = getWritePointer(I);

    BasicBlock *Target = nullptr;
    for (BasicBlock *Succ : successors(CurrentBB)) {
        if (Succ == CurrentBB)
            continue;

        Loop *SuccLoop = LI.getLoopFor(Succ);

        if (CurLoop != SuccLoop) {
            // Crossing a loop boundary. Sinking out of a loop is only safe
            // when all iterations write to the same address (loop-invariant
            // destination) — otherwise earlier iterations' writes are lost.
            // Sinking into a different loop is never safe.
            if (!CurLoop || !WritePtr || !CurLoop->isLoopInvariant(WritePtr))
                continue;
        }
        else if (CurLoop) {
            // Same loop: require post-dominance so the store executes on
            // every iteration (can't sink to a conditional branch inside
            // the loop body).
            if (!PDT.dominates(Succ, CurrentBB))
                continue;
        }

        if (canSinkMemoryWriteToSuccessor(I, Succ)) {
            if (!Target) {
                Target = Succ;
            }
            else {
                // Ambiguous: both targets are valid, so neither reads
                // the store (dead write). Don't bother sinking.
                return nullptr;
            }
        }
    }

    if (!Target)
        return nullptr;

    // Split critical edges for multi-predecessor targets.
    if (!Target->hasNPredecessors(1)) {
        CriticalEdgeSplittingOptions Options(&DT, &LI, &MSSAUpdater, &PDT);
        auto *NewBB = SplitCriticalEdge(CurrentBB, Target, Options);
        if (!NewBB)
            return nullptr;
        LLVM_DEBUG(dbgs() << "NewSink:   Split critical edge: "
                          << CurrentBB->getName() << " -> " << NewBB->getName()
                          << " -> " << Target->getName() << "\n");
        Target = NewBB;
    }

    return Target;
}

// Check if an instruction can be sunk (not a terminator, PHI, alloca, etc.).
bool NewSinkState::canSinkInstruction(Instruction *I) const
{
    if (I->isTerminator() || isa<PHINode>(I) || I->isEHPad())
        return false;

    if (I->mayThrow())
        return false;

    // Instructions that might not return (e.g., infinite loops)
    if (!I->willReturn())
        return false;

    if (isa<AllocaInst>(I))
        return false;

    // Don't sink lifetime intrinsics. They're markers for alloca liveness
    // and moving them can cause issues with memory analysis.
    if (auto *II = dyn_cast<IntrinsicInst>(I)) {
        if (II->getIntrinsicID() == Intrinsic::lifetime_start ||
            II->getIntrinsicID() == Intrinsic::lifetime_end)
            return false;
    }

    // Token types are used for coroutines and other constructs that
    // have strict placement requirements
    if (I->getType()->isTokenTy())
        return false;

    if (auto *CB = dyn_cast<CallBase>(I)) {
        // Convergent operations cannot be made control-dependent on additional values
        if (CB->isConvergent())
            return false;
        // Inline asm constraints can break if moved
        if (CB->isInlineAsm())
            return false;
        // nomerge attribute prevents code motion
        if (CB->cannotMerge())
            return false;
        // returns_twice (setjmp) is a control-flow barrier — it returns once
        // normally and once via longjmp with potentially different memory state.
        // Must not be sunk or reordered.
        if (CB->hasFnAttr(Attribute::ReturnsTwice))
            return false;
    }

    if (I->mayHaveSideEffects() && !I->mayWriteToMemory())
        return false;

    if (isMemoryWrite(I))
        return isSinkableMemoryWrite(I);

    return !I->mayHaveSideEffects();
}

// Try to sink an instruction to a target block. Returns true if sunk.
bool NewSinkState::trySinkInstruction(Instruction *I)
{
    // Skip debug output for trivially non-sinkable instructions
    bool TriviallyNonSinkable =
        I->isTerminator() || isa<PHINode>(I) || isa<AllocaInst>(I) || I->isEHPad();
    if (!TriviallyNonSinkable) {
        LLVM_DEBUG(dbgs() << "NewSink: Trying to sink: " << *I << "\n");
    }

    if (!canSinkInstruction(I)) {
        if (!TriviallyNonSinkable) {
            LLVM_DEBUG(dbgs() << "NewSink:   Failed: instruction not sinkable\n");
        }
        return false;
    }

    BasicBlock *BB = I->getParent();
    BasicBlock *Target = nullptr;

    if (isMemoryWrite(I)) {
        // Alias analysis determines where memory writes can sink
        Target = findSinkTargetForWrite(I);
        if (!Target) {
            LLVM_DEBUG(dbgs() << "NewSink:   Failed: no valid target for memory write\n");
        }
    }
    else if (!I->use_empty()) {
        // Dominance analysis determines where values can sink
        Target = findSinkTargetForValue(I);
        if (!Target) {
            LLVM_DEBUG(dbgs() << "NewSink:   Failed: no valid target for value\n");
        }
    }
    else {
        LLVM_DEBUG(dbgs() << "NewSink:   Failed: unused non-memory instruction\n");
        return false;
    }

    if (!Target)
        return false;

    if (!operandsDominateBlock(I, Target, DT)) {
        LLVM_DEBUG(dbgs() << "NewSink:   Failed: operands don't dominate target\n");
        return false;
    }

    bool IsNoReturnPath = NoReturnBlocks.count(Target);

    LLVM_DEBUG(dbgs() << "NewSink:   Sunk to " << Target->getName()
                      << (IsNoReturnPath ? " (noreturn path)" : " (general)") << "\n");

    ORE.emit([&]() {
        return OptimizationRemark(DEBUG_TYPE, "InstructionSunk", I)
               << "sunk " << ore::NV("Instruction", I) << " from "
               << ore::NV("FromBlock", BB) << " to " << ore::NV("ToBlock", Target)
               << (IsNoReturnPath ? " (noreturn path)" : " (general path)");
    });

#if JL_LLVM_VERSION >= 200000
    I->moveBefore(Target->getFirstInsertionPt());
#else
    I->moveBefore(&*Target->getFirstInsertionPt());
#endif

    // Update MemorySSA if this instruction has a MemoryAccess
    if (MemoryUseOrDef *MA = dyn_cast_or_null<MemoryUseOrDef>(MSSA.getMemoryAccess(I)))
        MSSAUpdater.moveToPlace(MA, Target, MemorySSA::Beginning);

    ++NumSunk;
    if (IsNoReturnPath)
        ++NumSunkToNoReturn;
    else
        ++NumSunkGeneral;

    return true;
}

// Fixed-point iteration: process blocks in RPO, instructions in reverse order.
bool NewSinkState::run()
{
    LLVM_DEBUG(dbgs() << "NewSink: noreturn blocks in " << F.getName() << ":";
               for (BasicBlock *BB
                    : NoReturnBlocks) dbgs()
               << " " << BB->getName();
               dbgs() << "\n");

    bool Changed = false;
    bool MadeProgress = true;
    unsigned Iterations = 0;

    while (MadeProgress && Iterations < MaxSinkingIterations) {
        MadeProgress = false;
        ++Iterations;
        ++NumIterations;
        LLVM_DEBUG(dbgs() << "NEWSINK ITERATION #" << Iterations << " on " << F.getName()
                          << "\n");
        ReversePostOrderTraversal<Function *> RPOT(&F);

        for (BasicBlock *BB : RPOT) {
            // Skip blocks with no successors and blocks where all paths
            // lead to unreachable — there's nowhere useful to sink to.
            if (succ_empty(BB) || NoReturnBlocks.count(BB))
                continue;

            // Process in reverse order to allow chains to sink together
            SmallVector<Instruction *, 16> Worklist;
            for (Instruction &I : *BB)
                Worklist.push_back(&I);

            for (auto It = Worklist.rbegin(); It != Worklist.rend(); ++It) {
                if (trySinkInstruction(*It)) {
                    MadeProgress = true;
                    Changed = true;
                }
            }
        }
    }

    if (Iterations >= MaxSinkingIterations) {
        ++NumAbortedIterations;
        LLVM_DEBUG(dbgs() << "NewSink: iteration limit reached for " << F.getName()
                          << "\n");
    }

    return Changed;
}

} // anonymous namespace

PreservedAnalyses NewSinkPass::run(Function &F, FunctionAnalysisManager &AM)
{
    auto &DT = AM.getResult<DominatorTreeAnalysis>(F);
    auto &PDT = AM.getResult<PostDominatorTreeAnalysis>(F);
    auto &LI = AM.getResult<LoopAnalysis>(F);
    auto &AA = AM.getResult<AAManager>(F);
    auto &MSSA = AM.getResult<MemorySSAAnalysis>(F).getMSSA();
    auto &TLI = AM.getResult<TargetLibraryAnalysis>(F);
    auto &ORE = AM.getResult<OptimizationRemarkEmitterAnalysis>(F);

    NewSinkState State(F, DT, PDT, LI, AA, MSSA, TLI, ORE);
    if (!State.run())
        return PreservedAnalyses::all();
#ifdef JL_VERIFY_PASSES
    assert(!verifyLLVMIR(F));
#endif
    PreservedAnalyses PA;
    PA.preserveSet<CFGAnalyses>();
    PA.preserve<MemorySSAAnalysis>();
    return PA;
}

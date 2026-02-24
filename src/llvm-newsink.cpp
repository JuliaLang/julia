// This file is a part of Julia. License is MIT: https://julialang.org/license

// NewSink - Sink instructions to paths where they are actually needed
//
// Useful for Julia's bounds checking where error message data is computed
// eagerly but only needed when bounds checks fail.
//
// Algorithm:
//   1. Identify "noreturn" blocks (all paths lead to unreachable)
//   2. Iteratively sink instructions to noreturn paths or single-predecessor
//      successors where all uses are located
//   3. For value-producing instructions: use dominance analysis to find where
//      uses allow sinking (+ alias check for loads)
//   4. For memory writes: use alias analysis (MemorySSA) to verify no reads
//      observe the wrong value
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
#include <llvm/Analysis/CaptureTracking.h>
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

        // Other calls (library functions like strcpy): more conservative
        // Must not have uses (return value), must return, must not throw
        return CB->use_empty() && CB->willReturn() && CB->doesNotThrow() &&
               !CB->isTerminator();
    }

    // Atomics with ordering constraints can't be sunk
    if (isa<AtomicCmpXchgInst>(I) || isa<AtomicRMWInst>(I))
        return false;

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

    // Get the memory location written by an instruction.
    // Handles stores, atomics, memory intrinsics, and library calls.
    std::optional<MemoryLocation> getLocForWrite(Instruction *I) const
    {
        if (!I->mayWriteToMemory())
            return std::nullopt;

        // For calls (memset/memcpy/memmove, and library calls like strcpy)
        if (auto *CB = dyn_cast<CallBase>(I))
            return MemoryLocation::getForDest(CB, TLI);

        // For stores and atomics (getOrNone handles Store, AtomicCmpXchg, AtomicRMW)
        return MemoryLocation::getOrNone(I);
    }

    bool isMemoryWrite(Instruction *I) const { return getLocForWrite(I).has_value(); }

    Value *getWritePointer(Instruction *I) const
    {
        if (auto *SI = dyn_cast<StoreInst>(I))
            return SI->getPointerOperand();

        // For calls, use MemoryLocation::getForDest which handles both
        // memory intrinsics and library calls like strcpy
        if (auto *CB = dyn_cast<CallBase>(I)) {
            if (auto Loc = MemoryLocation::getForDest(CB, TLI))
                return const_cast<Value *>(Loc->Ptr);
            return nullptr;
        }

        if (auto *AXCHG = dyn_cast<AtomicCmpXchgInst>(I))
            return AXCHG->getPointerOperand();

        if (auto *ARMW = dyn_cast<AtomicRMWInst>(I))
            return ARMW->getPointerOperand();

        return nullptr;
    }

    // Check if pointer escapes outside the target block (and blocks it dominates)
    bool pointerEscapesOutsideTarget(const Value *Ptr, BasicBlock *TargetSucc);

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

// Collect all blocks where I is used (for PHIs, the incoming block).
static void collectUseBlocks(Instruction *I, BasicBlock *CurrentBB,
                             SmallPtrSetImpl<BasicBlock *> &UseBlocks)
{
    for (User *U : I->users()) {
        Instruction *UserInst = dyn_cast<Instruction>(U);
        if (!UserInst)
            continue;

        if (PHINode *PN = dyn_cast<PHINode>(UserInst)) {
            for (unsigned i = 0; i < PN->getNumIncomingValues(); i++) {
                if (PN->getIncomingValue(i) == I) {
                    BasicBlock *IncomingBB = PN->getIncomingBlock(i);
                    // For PHIs, the value must be live at the end of the incoming block
                    // If incoming from current block, use the PHI's block
                    if (IncomingBB == CurrentBB)
                        UseBlocks.insert(PN->getParent());
                    else
                        UseBlocks.insert(IncomingBB);
                }
            }
        }
        else {
            UseBlocks.insert(UserInst->getParent());
        }
    }
}

// Find the nearest block to uses that we can sink to via single-predecessor blocks.
// Returns the block closest to the NCD (nearest common dominator) of all uses,
// reachable through single-predecessor blocks from CurrentBB. Pure dominance analysis.
static BasicBlock *findNearestDominatingTarget(Instruction *I, DominatorTree &DT)
{
    BasicBlock *CurrentBB = I->getParent();

    if (I->use_empty())
        return nullptr;

    // Collect all use blocks
    SmallPtrSet<BasicBlock *, 8> UseBlocks;
    collectUseBlocks(I, CurrentBB, UseBlocks);

    if (UseBlocks.empty())
        return nullptr;

    // Filter out unreachable blocks (not in dominator tree).
    // These can occur with dead code that references values from reachable blocks.
    SmallVector<BasicBlock *, 8> ReachableUseBlocks;
    for (BasicBlock *UB : UseBlocks) {
        if (DT.isReachableFromEntry(UB))
            ReachableUseBlocks.push_back(UB);
    }

    if (ReachableUseBlocks.empty())
        return nullptr;

    // Find which successor dominates all uses
    BasicBlock *EntrySucc = nullptr;
    for (BasicBlock *Succ : successors(CurrentBB)) {
        bool DominatesAll = true;
        for (BasicBlock *UB : ReachableUseBlocks) {
            if (UB != Succ && !DT.dominates(Succ, UB)) {
                DominatesAll = false;
                break;
            }
        }
        if (DominatesAll) {
            if (EntrySucc) // Multiple successors dominate all uses - ambiguous
                return nullptr;
            EntrySucc = Succ;
        }
    }

    if (!EntrySucc || !EntrySucc->hasNPredecessors(1))
        return nullptr;

    // Find NCD (nearest common dominator) of all use blocks - this is our target
    BasicBlock *NCD = nullptr;
    for (BasicBlock *UB : ReachableUseBlocks) {
        if (!NCD)
            NCD = UB;
        else
            NCD = DT.findNearestCommonDominator(NCD, UB);
    }

    // Walk from EntrySucc towards NCD through single-predecessor blocks
    BasicBlock *Deepest = EntrySucc;
    BasicBlock *Current = EntrySucc;

    while (Current != NCD) {
        // Check if NCD is dominated by current (we're walking towards it)
        if (!DT.dominates(Current, NCD))
            break;

        // Try to go deeper - find the successor that leads to NCD
        BasicBlock *NextBlock = nullptr;
        for (BasicBlock *Succ : successors(Current)) {
            if (Succ == NCD || DT.dominates(Succ, NCD)) {
                NextBlock = Succ;
                break;
            }
        }

        if (!NextBlock || !NextBlock->hasNPredecessors(1))
            break;

        Deepest = NextBlock;
        Current = NextBlock;
    }

    return Deepest;
}

static bool isFunctionLocalObject(const Value *V)
{
    return isIdentifiedFunctionLocal(V);
}

// Path-sensitive CaptureTracker: ignores captures in blocks where predicate returns true.
template<typename BlockPredicate>
struct PathSensitiveCaptureTracker : public CaptureTracker {
    BlockPredicate IgnoreBlock;
    bool Captured = false;

    PathSensitiveCaptureTracker(BlockPredicate Pred) : IgnoreBlock(Pred) {}

    void tooManyUses() override { Captured = true; }

    bool captured(const Use *U) override
    {
        if (auto *I = dyn_cast<Instruction>(U->getUser())) {
            if (IgnoreBlock(I->getParent()))
                return false;
        }
        Captured = true;
        return true;
    }
};

template<typename BlockPredicate>
static bool pointerMayBeCapturedInPaths(const Value *Ptr, BlockPredicate IgnoreBlock)
{
    const Value *UnderlyingObj = getUnderlyingObject(Ptr);
    PathSensitiveCaptureTracker<BlockPredicate> Tracker(IgnoreBlock);
    PointerMayBeCaptured(UnderlyingObj, &Tracker);
    return Tracker.Captured;
}

// Check if pointer escapes outside the target block.
// We ignore captures in the target block or blocks dominated by it
// (the write will happen before reaching those blocks).
bool NewSinkState::pointerEscapesOutsideTarget(const Value *Ptr, BasicBlock *TargetSucc)
{
    return pointerMayBeCapturedInPaths(Ptr, [&](BasicBlock *BB) {
        // Capture in target block is fine
        if (BB == TargetSucc)
            return true;
        // Capture in blocks dominated by target is also fine
        if (DT.dominates(TargetSucc, BB))
            return true;
        return false;
    });
}

// Check if a memory write can be safely sunk to target successor.
// Uses MemorySSA to verify no reads observe the wrong value.
BasicBlock *NewSinkState::canSinkMemoryWriteToSuccessor(Instruction *WriteInst,
                                                        BasicBlock *TargetSucc)
{
    if (!isSinkableMemoryWrite(WriteInst))
        return nullptr;

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
    if (!isFunctionLocalObject(UnderlyingObj))
        return nullptr;
    if (pointerEscapesOutsideTarget(WritePtr, TargetSucc))
        return nullptr;

    MemoryAccess *WriteDef = MSSA.getMemoryAccess(WriteInst);
    if (!WriteDef)
        return nullptr;

    // Walk MSSA users to find reads that depend on this write.
    //
    // We walk DOWN from our MemoryDef to find all MemoryUses that would observe
    // the wrong value if we sink. This is the opposite of MSSA's Walker which
    // walks UP from a use to find the clobbering def.
    //
    // Key insight: MSSA doesn't understand loop iteration semantics. It tells us
    // "is there a path from this def to a use?" but not "will future loop
    // iterations need this def?". For loops, we handle this separately via
    // LoopInfo + PostDominatorTree checks in findSinkTargetForWrite.
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

        // Reads in target block or blocks dominated by it are acceptable
        // (write will happen before reads since we're sinking to target)
        if (MABB == TargetSucc || DT.dominates(TargetSucc, MABB))
            continue;

        // Handle lifetime intrinsics specially.
        if (auto *II = dyn_cast<IntrinsicInst>(MemInst)) {
            if (II->getIntrinsicID() == Intrinsic::lifetime_end) {
                // In source block, lifetime.end of write location blocks sinking
                // (can't write to memory after its lifetime ended).
                if (MABB == CurrentBB) {
                    ModRefInfo MRI = BatchAA.getModRefInfo(II, WriteLoc);
                    if (isModOrRefSet(MRI))
                        return nullptr;
                }
                // Doesn't affect our write location - just follow users
                if (auto *UserDef = dyn_cast<MemoryDef>(MA)) {
                    for (User *U : UserDef->users())
                        if (auto *UserMA = dyn_cast<MemoryAccess>(U))
                            Worklist.push_back(UserMA);
                }
                continue;
            }
            if (II->getIntrinsicID() == Intrinsic::lifetime_start) {
                // lifetime.start doesn't block sinking - just follow users
                if (auto *UserDef = dyn_cast<MemoryDef>(MA)) {
                    for (User *U : UserDef->users())
                        if (auto *UserMA = dyn_cast<MemoryAccess>(U))
                            Worklist.push_back(UserMA);
                }
                continue;
            }
        }

        // For source block, any aliasing read OR write blocks sinking.
        // This conservatively avoids reordering past clobbering writes.
        if (MABB == CurrentBB) {
            // Check if this access aliases our write location
            ModRefInfo MRI = BatchAA.getModRefInfo(MemInst, WriteLoc);
            if (isModOrRefSet(MRI))
                // This access may observe or clobber our write - can't sink
                return nullptr;
            // Follow through MemoryDefs to find later accesses in the source block
            if (auto *UserDef = dyn_cast<MemoryDef>(MA)) {
                for (User *U : UserDef->users())
                    if (auto *UserMA = dyn_cast<MemoryAccess>(U))
                        Worklist.push_back(UserMA);
            }
            continue;
        }

        // Use ModRefInfo to check if this instruction reads our write location
        ModRefInfo MRI = BatchAA.getModRefInfo(MemInst, WriteLoc);

        if (!isRefSet(MRI)) {
            // Doesn't read our location - safe to skip, but follow through
            // MemoryDefs to find readers beyond them. MemorySSA chains all
            // memory ops together, not just aliasing ones, so a non-aliasing
            // def may have users that DO alias our write location.
            if (auto *UserDef = dyn_cast<MemoryDef>(MA)) {
                for (User *U : UserDef->users())
                    if (auto *UserMA = dyn_cast<MemoryAccess>(U))
                        Worklist.push_back(UserMA);
            }
            continue;
        }

        // This instruction reads our write location - blocks sinking
        return nullptr;
    }

    return TargetSucc;
}

// Check if a memory-reading instruction (load or readonly call) can be sunk
// past all instructions between it and the block terminator without observing
// a different value. Walks forward from I to the terminator checking for
// intervening clobbers via AA. Handles both loads and readonly calls uniformly.
// The AA layer naturally handles returns_twice (BasicAA models setjmp as
// clobbering non-local memory).
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

    // For readonly calls, check each intervening instruction individually.
    // We can't use canInstructionRangeModRef because calls don't have a
    // single MemoryLocation, but the logic is the same: does any instruction
    // between I and the terminator modify memory that I reads?
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

// Check if a memcpy/memmove can be sunk (no intervening modifications to source).
// This includes writes to the source and lifetime.end of the source.
// Note: This only checks if unsinkable modifications exist between the memcpy
// and terminator. Sinkable modifications will be processed first (since we
// iterate in reverse order) and the check will see them as already moved.
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

// Check if all operands of an instruction dominate a target block.
// This is necessary to ensure the instruction remains valid after sinking.
// For example, a store using a PHI can only be sunk to blocks dominated by
// the PHI's block - otherwise the PHI value won't be available.
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

    if (isa<GetElementPtrInst>(I))
        return true;

    if (isMemoryWrite(I))
        return isSinkableMemoryWrite(I);

    return !I->mayHaveSideEffects();
}

// Find sink target for value-producing instructions (loads, arithmetic, GEPs, etc.)
// Uses dominance analysis to find where uses allow sinking.
// For loads, also verifies no writes clobber the loaded value (alias analysis).
BasicBlock *NewSinkState::findSinkTargetForValue(Instruction *I)
{
    assert(!I->use_empty() && "Value must have uses");
    BasicBlock *CurrentBB = I->getParent();

    // Collect user blocks, filtering out unreachable blocks (not in dominator tree)
    SmallPtrSet<BasicBlock *, 8> UserBlocks;
    for (User *U : I->users()) {
        Instruction *UserInst = cast<Instruction>(U);
        if (PHINode *PN = dyn_cast<PHINode>(UserInst)) {
            for (unsigned i = 0; i < PN->getNumIncomingValues(); i++) {
                if (PN->getIncomingValue(i) == I) {
                    BasicBlock *IncomingBB = PN->getIncomingBlock(i);
                    if (DT.isReachableFromEntry(IncomingBB))
                        UserBlocks.insert(IncomingBB);
                }
            }
        }
        else {
            BasicBlock *UserBB = UserInst->getParent();
            if (DT.isReachableFromEntry(UserBB))
                UserBlocks.insert(UserBB);
        }
    }

    // If all uses are in unreachable blocks, don't sink
    if (UserBlocks.empty())
        return nullptr;

    // Check if all users are in noreturn blocks
    bool AllInNoReturnBlocks = true;
    for (BasicBlock *UB : UserBlocks) {
        if (!NoReturnBlocks.count(UB)) {
            AllInNoReturnBlocks = false;
            break;
        }
    }

    BasicBlock *Target = nullptr;

    if (AllInNoReturnBlocks) {
        // Sink to NCD of all noreturn user blocks
        for (BasicBlock *UB : UserBlocks) {
            if (Target)
                Target = DT.findNearestCommonDominator(Target, UB);
            else
                Target = UB;
        }
        if (Target == CurrentBB)
            Target = nullptr;
    }
    else {
        // Use dominance to find block nearest to uses
        if (succ_size(CurrentBB) > 1)
            Target = findNearestDominatingTarget(I, DT);
    }

    if (!Target)
        return nullptr;

    // Don't sink into or out of a loop — that's LICM/LoopSink's job.
    Loop *CurLoop = LI.getLoopFor(CurrentBB);
    Loop *TargetLoop = LI.getLoopFor(Target);
    if (CurLoop != TargetLoop)
        return nullptr;

    // For instructions that read memory, verify no intervening instructions
    // clobber the value between the instruction and the block terminator.
    // This catches both intervening writes and returns_twice barriers
    // (BasicAA models setjmp as clobbering non-local memory).
    //
    // canSinkMemoryRead only checks the source block, so restrict to immediate
    // successors to avoid sinking past clobbering writes in intermediate blocks.
    if (I->mayReadFromMemory()) {
        if (!llvm::is_contained(successors(CurrentBB), Target))
            return nullptr;
        if (!canSinkMemoryRead(I, AA))
            return nullptr;
    }

    return Target;
}

// Find sink target for memory writes (stores, memset, memcpy, etc.)
// Uses alias analysis (MemorySSA) to verify no reads observe the wrong value.
BasicBlock *NewSinkState::findSinkTargetForWrite(Instruction *I)
{
    // All sinkable memory writes should have no uses:
    // - StoreInst: void
    // - MemIntrinsic: void
    // - CallBase: isSinkableMemoryWrite requires use_empty()
    assert(I->use_empty() && "Sinkable memory writes should have no uses");

    BasicBlock *CurrentBB = I->getParent();

    // For memcpy/memmove, verify the source isn't modified (or lifetime-ended)
    // between the instruction and the block terminator
    if (auto *MTI = dyn_cast<MemTransferInst>(I)) {
        if (!canSinkMemTransfer(MTI, AA))
            return nullptr;
    }

    Loop *CurLoop = LI.getLoopFor(CurrentBB);

    // Check noreturn successors first
    BasicBlock *NoReturnTarget = nullptr;
    for (BasicBlock *Succ : successors(CurrentBB)) {
        if (NoReturnBlocks.count(Succ)) {
            if (NoReturnTarget)
                NoReturnTarget = DT.findNearestCommonDominator(NoReturnTarget, Succ);
            else
                NoReturnTarget = Succ;
        }
    }

    // Verify noreturn target is safe via alias analysis.
    // Don't sink into a different loop. Post-dominance is NOT required here
    // (unlike the general path) because the goal is to remove the store from
    // the hot path — the MSSA walk in canSinkMemoryWriteToSuccessor ensures
    // no reader on the non-noreturn path depends on this write.
    if (NoReturnTarget && NoReturnBlocks.count(NoReturnTarget) &&
        NoReturnTarget != CurrentBB) {
        Loop *TargetLoop = LI.getLoopFor(NoReturnTarget);
        bool LoopSafe = (TargetLoop == nullptr || TargetLoop == CurLoop);
        if (LoopSafe && canSinkMemoryWriteToSuccessor(I, NoReturnTarget))
            return NoReturnTarget;
    }

    // General sinking requires multiple successors (otherwise no benefit)
    if (succ_size(CurrentBB) <= 1)
        return nullptr;

    BasicBlock *GeneralTarget = nullptr;
    for (BasicBlock *Succ : successors(CurrentBB)) {
        if (!Succ->hasNPredecessors(1))
            continue;

        // Don't sink into or out of a loop. Sinking out of a loop would
        // reduce a store that executes every iteration to one that only
        // executes once (after the last iteration), which is wrong when the
        // store target varies per iteration.
        Loop *SuccLoop = LI.getLoopFor(Succ);
        if (CurLoop != nullptr && SuccLoop != CurLoop)
            continue;

        // In loops, require post-dominance so all iterations execute the store
        if (CurLoop && !PDT.dominates(Succ, CurrentBB))
            continue;

        if (canSinkMemoryWriteToSuccessor(I, Succ)) {
            if (GeneralTarget)
                return nullptr; // Multiple valid targets - ambiguous
            GeneralTarget = Succ;
        }
    }

    if (GeneralTarget && GeneralTarget != CurrentBB && GeneralTarget->hasNPredecessors(1))
        return GeneralTarget;

    return nullptr;
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

    // Verify all operands dominate the target. This catches cases like
    // a store using a PHI where the target block has multiple predecessors
    // and isn't dominated by the PHI's block.
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
            if (NoReturnBlocks.count(BB))
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

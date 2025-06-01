// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  LLVM passes that may be partially modified by a third-party GC implementation.
*/

#include "llvm-version.h"
#include "passes.h"

#include "llvm/IR/DerivedTypes.h"
#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

#include <llvm/ADT/Statistic.h>
#include <llvm/ADT/BitVector.h>
#include <llvm/ADT/SparseBitVector.h>
#include <llvm/ADT/PostOrderIterator.h>
#include <llvm/ADT/SetVector.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/SmallSet.h>
#include <llvm/Analysis/CFG.h>
#include <llvm/Analysis/DomTreeUpdater.h>
#include <llvm/Analysis/InstSimplifyFolder.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/MDBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/ModuleSlotTracker.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Pass.h>
#include <llvm/Support/Debug.h>
#include <llvm/Transforms/Utils/BasicBlockUtils.h>
#include <llvm/Transforms/Utils/ModuleUtils.h>

#include <llvm/InitializePasses.h>

#include "llvm-codegen-shared.h"
#include "julia.h"
#include "julia_internal.h"
#include "julia_assert.h"
#include "llvm-pass-helpers.h"
#include <map>
#include <string>
#include <optional>

#ifndef LLVM_GC_PASSES_H
#define LLVM_GC_PASSES_H

using namespace llvm;

/* Julia GC Root Placement pass. For a general overview of the design of GC
   root lowering, see the devdocs. This file is the actual implementation.

   The actual algorithm is fairly straightforward. First recall the goal of this
   pass:

   Minimize the number of needed gc roots/stores to them subject to the constraint
   that at every safepoint, any live gc-tracked pointer (i.e. for which there is
   a path after this point that contains a use of this pointer) is in some gc slot.

   In particular, in order to understand this algorithm, it is important to
   realize that the only places where rootedness matters is at safepoints.

   Now, the primary phases of the algorithm are:

   1. Local Scan

      During this step, each Basic Block is inspected and analyzed for local
      properties. In particular, we want to determine the ordering of any of
      the following activities:

        - Any Def of a gc-tracked pointer. In general Defs are the results of
          calls or loads from appropriate memory locations. Phi nodes and
          selects do complicate this story slightly as described below.
        - Any use of a gc-tracked or derived pointer. As described in the
          devdocs, a use is in general one of
              a) a load from a tracked/derived value
              b) a store to a tracked/derived value
              c) a store OF a tracked/derived value
              d) a use of a value as a call operand (including operand bundles)
        - Any safepoint

      Crucially, we also perform pointer numbering during the local scan,
      assigning every Def a unique integer and caching the integer for each
      derived pointer. This allows us to operate only on the set of Defs (
      represented by these integers) for the rest of the algorithm. We also
      maintain some local utility information that is needed by later passes
      (see the BBState struct for details).

    2. Dataflow Computation

      This computation operates entirely over the function's control flow graph
      and does not look into a basic block. The algorithm is essentially
      textbook iterative data flow for liveness computation. However, the
      data flow equations are slightly more complicated because we also
      forward propagate rootedness information in addition to backpropagating
      liveness.

    3. Live Set Computation

      With the liveness information from the previous step, we can now compute,
      for every safepoint, the set of values live at that particular safepoint.
      There are three pieces of information being combined here:
           i. Values that needed to be live due to local analysis (e.g. there
              was a def, then a safepoint, then a use). This was computed during
              local analysis.
          ii. Values that are live across the basic block (i.e. they are live
              at every safepoint within the basic block). This relies entirely
              on the liveness information.
         iii. Values that are now live-out from the basic block (i.e. they are
              live at every safepoint following their def). During local
              analysis, we keep, for every safepoint, those values that would
              be live if they were live out. Here we can check if they are
              actually live-out and make the appropriate additions to the live
              set.

       Lastly, we also explicitly compute, for each value, the list of values
       that are simultaneously live at some safepoint. This is known as an
       "interference graph" and is the input to the next step.

    4. GC Root coloring

      Two values which are not simultaneously live at a safepoint can share the
      same slot. This is an important optimization, because otherwise long
      functions would have exceptionally large GC slots, reducing performance
      and bloating the size of the stack. Assigning values to these slots is
      equivalent to doing graph coloring on the interference graph - the graph
      where nodes are values and two values have an edge if they are
      simultaneously live at a safepoint - which we computed in the previous
      step. Now graph coloring in general is a hard problem. However, for SSA
      form programs, (and most programs in general, by virtue of their
      structure), the resulting interference graphs are chordal and can be
      colored optimally in linear time by performing greedy coloring in a
      perfect elimination order. Now, our interference graphs are likely not
      entirely chordal due to some non-SSA corner cases. However, using the same
      algorithm should still give a very good coloring while having sufficiently
      low runtime.

    5. JLCall frame optimizations

      Unlike earlier iterations of the gc root placement logic, jlcall frames
      are no longer treated as a special case and need not necessarily be sunk
      into the gc frame. Additionally, we now emit lifetime
      intrinsics, so regular stack slot coloring will merge any jlcall frames
      not sunk into the gc frame. Nevertheless performing such sinking can still
      be profitable. Since all arguments to a jlcall are guaranteed to be live
      at that call in some gc slot, we can attempt to rearrange the slots within
      the gc-frame, or reuse slots not assigned at that particular location
      for the gcframe. However, even without this optimization, stack frames
      are at most two times larger than optimal (because regular stack coloring
      can merge the jlcall allocas).

      N.B.: This step is not yet implemented.

    6. Root placement

      This performs the actual insertion of the GCFrame pushes/pops, zeros out
      the gc frame and creates the stores to the gc frame according to the
      stack slot assignment computed in the previous step. GC frames stores
      are generally sunk right before the first safe point that use them
      (this is beneficial for code where the primary path does not have
      safepoints, but some other path - e.g. the error path does). However,
      if the first safepoint is not dominated by the definition (this can
      happen due to the non-ssa corner cases), the store is inserted right after
      the definition.

    7. Cleanup

      This step performs necessary cleanup before passing the IR to codegen. In
      particular, it removes any calls to julia_from_objref intrinsics and
      removes the extra operand bundles from ccalls. In the future it could
      also strip the addrspace information from all values as this
      information is no longer needed.


  There are a couple important special cases that deserve special attention:

    A. PHIs and Selects

      In general PHIs and selects are treated as separate defs for the purposes
      of the algorithm and their operands as uses of those values. It is
      important to consider however WHERE the uses of PHI's operands are
      located. It is neither at the start of the basic block, because the values
      do not dominate the block (so can't really consider them live-in), nor
      at the end of the predecessor (because they are actually live out).
      Instead it is best to think of those uses as living on the edge between
      the appropriate predecessor and the block containing the PHI.

      Another concern is PHIs of derived values. Since we cannot simply root
      these values by storing them to a GC slot, we need to insert a new,
      artificial PHI that tracks the base pointers for the derived values. E.g.
      in:

      A:
        %Abase = load addrspace(10) *...
        %Aderived = addrspacecast %Abase to addrspace(11)
      B:
        %Bbase = load addrspace(10) *...
        %Bderived = addrspacecast %Bbase to addrspace(11)
      C:
        %phi = phi [%Aderived, %A
                    %Bderived, %B]

      we will insert another phi in C to track the relevant base pointers:

        %philift = phi [%Abase, %A
                        %Bbase, %B]

      We then pretend, for the purposes of numbering that %phi was derived from
      %philift. Note that in order to be able to do this, we need to be able to
      perform this lifting either during numbering or instruction scanning.

    B. Vectors of pointers/Union representations

      Since this pass runs very late in the pass pipeline, it runs after the
      various vectorization passes. As a result, we have to potentially deal
      with vectors of gc-tracked pointers. For the purposes of most of the
      algorithm, we simply assign every element of the vector a separate number
      and no changes are needed. However, those parts of the algorithm that
      look at IR need to be aware of the possibility of encountering vectors of
      pointers.

      Similarly, unions (e.g. in call returns) are represented as a struct of
      a gc-tracked value and an argument selector. We simply assign a single
      number to this struct and proceed as if it was a single pointer. However,
      this again requires care at the IR level.

    C. Non mem2reg'd allocas

      Under some circumstances, allocas will still be present in the IR when
      we get to this pass. We don't try very hard to handle this case, and
      simply sink the alloca into the GCFrame.
*/

// 4096 bits == 64 words (64 bit words). Larger bit numbers are faster and doing something
// substantially smaller here doesn't actually save much memory because of malloc overhead.
// Too large is bad also though - 4096 was found to be a reasonable middle ground.
using LargeSparseBitVector = SparseBitVector<4096>;

struct BBState {
    // Uses in this BB
    // These do not get updated after local analysis
    LargeSparseBitVector Defs;
    LargeSparseBitVector PhiOuts;
    LargeSparseBitVector UpExposedUses;
    // These get updated during dataflow
    LargeSparseBitVector LiveIn;
    LargeSparseBitVector LiveOut;
    SmallVector<int, 0> Safepoints;
    int TopmostSafepoint = -1;
    bool HasSafepoint = false;
    // Have we gone through this basic block in our local scan yet?
    bool Done = false;
};

struct State {
    Function *const F;
    DominatorTree *DT;

    // The maximum assigned value number
    int MaxPtrNumber;
    // The maximum assigned safepoint number
    int MaxSafepointNumber;
    // Cache of numbers assigned to IR values. This includes caching of numbers
    // for derived values
    std::map<Value *, int> AllPtrNumbering;
    std::map<Value *, SmallVector<int, 0>> AllCompositeNumbering;
    // The reverse of the previous maps
    std::map<int, Value *> ReversePtrNumbering;
    // Neighbors in the coloring interference graph. I.e. for each value, the
    // indices of other values that are used simultaneously at some safe point.
    SmallVector<LargeSparseBitVector, 0> Neighbors;
    // The result of the local analysis
    std::map<const BasicBlock *, BBState> BBStates;

    // Refinement map. If all of the values are rooted
    // (-1 means an externally rooted value and -2 means a globally/permanently rooted value),
    // the key is already rooted (but not the other way around).
    // A value that can be refined to -2 never need any rooting or write barrier.
    // A value that can be refined to -1 don't need local root but still need write barrier.
    // At the end of `LocalScan` this map has a few properties
    // 1. Values are either < 0 or dominates the key
    // 2. Therefore this is a DAG
    std::map<int, SmallVector<int, 1>> Refinements;

    // GC preserves map. All safepoints dominated by the map key, but not any
    // of its uses need to preserve the values listed in the map value.
    std::map<Instruction *, SmallVector<int, 0>> GCPreserves;

    // The assignment of numbers to safepoints. The indices in the map
    // are indices into the next three maps which store safepoint properties
    std::map<Instruction *, int> SafepointNumbering;

    // Reverse mapping index -> safepoint
    SmallVector<Instruction *, 0> ReverseSafepointNumbering;

    // Instructions that can return twice. For now, all values live at these
    // instructions will get their own, dedicated GC frame slots, because they
    // have unobservable control flow, so we can't be sure where they're
    // actually live. All of these are also considered safepoints.
    SmallVector<Instruction *, 0> ReturnsTwice;

    // The set of values live at a particular safepoint
    SmallVector< LargeSparseBitVector , 0> LiveSets;
    // Those values that - if live out from our parent basic block - are live
    // at this safepoint.
    SmallVector<SmallVector<int, 0>> LiveIfLiveOut;
    // The set of values that are kept alive by the callee.
    SmallVector<SmallVector<int, 0>> CalleeRoots;
    // We don't bother doing liveness on Allocas that were not mem2reg'ed.
    // they just get directly sunk into the root array.
    DenseMap<AllocaInst *, unsigned> ArrayAllocas;
    DenseMap<AllocaInst *, AllocaInst *> ShadowAllocas;
    SmallVector<std::pair<StoreInst *, unsigned>, 0> TrackedStores;
    State(Function &F) : F(&F), DT(nullptr), MaxPtrNumber(-1), MaxSafepointNumber(-1) {}
};


struct LateLowerGCFrame:  private JuliaPassContext {
    function_ref<DominatorTree &()> GetDT;
    LateLowerGCFrame(function_ref<DominatorTree &()> GetDT) : GetDT(GetDT) {}

public:
    bool runOnFunction(Function &F, bool *CFGModified = nullptr);

private:
    Value *pgcstack;
    Function *smallAllocFunc;

    void MaybeNoteDef(State &S, BBState &BBS, Value *Def, const ArrayRef<int> &SafepointsSoFar,
                      SmallVector<int, 1> &&RefinedPtr = SmallVector<int, 1>());
    void NoteUse(State &S, BBState &BBS, Value *V, LargeSparseBitVector &Uses, Function &F);
    void NoteUse(State &S, BBState &BBS, Value *V, Function &F) {
        NoteUse(S, BBS, V, BBS.UpExposedUses, F);
    }

    void LiftPhi(State &S, PHINode *Phi);
    void LiftSelect(State &S, SelectInst *SI);
    Value *MaybeExtractScalar(State &S, std::pair<Value*,int> ValExpr, Instruction *InsertBefore);
    SmallVector<Value*, 0> MaybeExtractVector(State &S, Value *BaseVec, Instruction *InsertBefore);
    Value *GetPtrForNumber(State &S, unsigned Num, Instruction *InsertBefore);

    int Number(State &S, Value *V);
    int NumberBase(State &S, Value *Base);
    SmallVector<int, 0> NumberAll(State &S, Value *V);
    SmallVector<int, 0> NumberAllBase(State &S, Value *Base);

    void NoteOperandUses(State &S, BBState &BBS, Instruction &UI);
    void MaybeTrackDst(State &S, MemTransferInst *MI);
    void MaybeTrackStore(State &S, StoreInst *I);
    State LocalScan(Function &F);
    void ComputeLiveness(State &S);
    void ComputeLiveSets(State &S);
    std::pair<SmallVector<int, 0>, int> ColorRoots(const State &S);
    void PlaceGCFrameStore(State &S, unsigned R, unsigned MinColorRoot, ArrayRef<int> Colors, Value *GCFrame, Instruction *InsertBefore);
    void PlaceGCFrameStores(State &S, unsigned MinColorRoot, ArrayRef<int> Colors, int PreAssignedColors, Value *GCFrame);
    void PlaceGCFrameReset(State &S, unsigned R, unsigned MinColorRoot, ArrayRef<int> Colors, Value *GCFrame, Instruction *InsertBefore);
    void PlaceRootsAndUpdateCalls(ArrayRef<int> Colors, int PreAssignedColors, State &S, std::map<Value *, std::pair<int, int>>);
    void CleanupWriteBarriers(Function &F, State *S, const SmallVector<CallInst*, 0> &WriteBarriers, bool *CFGModified);
    bool CleanupIR(Function &F, State *S, bool *CFGModified);
    void NoteUseChain(State &S, BBState &BBS, User *TheUser);
    SmallVector<int, 1> GetPHIRefinements(PHINode *phi, State &S);
    void FixUpRefinements(ArrayRef<int> PHINumbers, State &S);
    void RefineLiveSet(LargeSparseBitVector &LS, State &S, ArrayRef<int> CalleeRoots);
    Value *EmitTagPtr(IRBuilder<> &builder, Type *T, Type *T_size, Value *V);
    Value *EmitLoadTag(IRBuilder<> &builder, Type *T_size, Value *V);
    Value* lowerGCAllocBytesLate(CallInst *target, Function &F);
};

// The final GC lowering pass. This pass lowers platform-agnostic GC
// intrinsics to platform-dependent instruction sequences. The
// intrinsics it targets are those produced by the late GC frame
// lowering pass.
//
// This pass targets typical back-ends for which the standard Julia
// runtime library is available. Atypical back-ends should supply
// their own lowering pass.

struct FinalLowerGC: private JuliaPassContext {
    bool runOnFunction(Function &F);

private:
    Function *queueRootFunc;
    Function *smallAllocFunc;
    Function *bigAllocFunc;
    Function *allocTypedFunc;
    Value *pgcstack;
    Type *T_size;

    // Lowers a `julia.new_gc_frame` intrinsic.
    void lowerNewGCFrame(CallInst *target, Function &F);

    // Lowers a `julia.push_gc_frame` intrinsic.
    void lowerPushGCFrame(CallInst *target, Function &F);

    // Lowers a `julia.pop_gc_frame` intrinsic.
    void lowerPopGCFrame(CallInst *target, Function &F);

    // Lowers a `julia.get_gc_frame_slot` intrinsic.
    void lowerGetGCFrameSlot(CallInst *target, Function &F);

    // Lowers a `julia.gc_alloc_bytes` intrinsic.
    void lowerGCAllocBytes(CallInst *target, Function &F);

    // Lowers a `julia.queue_gc_root` intrinsic.
    void lowerQueueGCRoot(CallInst *target, Function &F);

    // Lowers a `julia.safepoint` intrinsic.
    void lowerSafepoint(CallInst *target, Function &F);

    // Check if the pass should be run
    bool shouldRunFinalGC();
};

#endif // LLVM_GC_PASSES_H

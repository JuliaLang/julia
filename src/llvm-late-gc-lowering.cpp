// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-version.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

#include <llvm/ADT/BitVector.h>
#include <llvm/ADT/PostOrderIterator.h>
#include <llvm/ADT/SetVector.h>
#include <llvm/ADT/SmallVector.h>
#include "llvm/Analysis/CFG.h"
#include <llvm/IR/Value.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#if JL_LLVM_VERSION < 110000
#include <llvm/IR/CallSite.h>
#endif
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/MDBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Pass.h>
#include <llvm/Support/Debug.h>
#include <llvm/Transforms/Utils/BasicBlockUtils.h>
#include <llvm/Transforms/Utils/ModuleUtils.h>

#if JL_LLVM_VERSION >= 100000
#include <llvm/InitializePasses.h>
#endif

#include "codegen_shared.h"
#include "julia.h"
#include "julia_internal.h"
#include "julia_assert.h"
#include "llvm-pass-helpers.h"

#define DEBUG_TYPE "late_lower_gcroot"

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
      the gc-frame, or re-use slots not assigned at that particular location
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

struct BBState {
    // Uses in this BB
    // These do not get updated after local analysis
    BitVector Defs;
    BitVector PhiOuts;
    BitVector UpExposedUses;
    // These get updated during dataflow
    BitVector LiveIn;
    BitVector LiveOut;
    std::vector<int> Safepoints;
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
    std::map<Value *, std::vector<int>> AllCompositeNumbering;
    // The reverse of the previous maps
    std::map<int, Value *> ReversePtrNumbering;
    // Neighbors in the coloring interference graph. I.e. for each value, the
    // indices of other values that are used simultaneously at some safe point.
    std::vector<SetVector<int>> Neighbors;
    // The result of the local analysis
    std::map<BasicBlock *, BBState> BBStates;

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
    std::map<Instruction *, std::vector<int>> GCPreserves;

    // The assignment of numbers to safepoints. The indices in the map
    // are indices into the next three maps which store safepoint properties
    std::map<Instruction *, int> SafepointNumbering;

    // Reverse mapping index -> safepoint
    std::vector<Instruction *> ReverseSafepointNumbering;

    // Instructions that can return twice. For now, all values live at these
    // instructions will get their own, dedicated GC frame slots, because they
    // have unobservable control flow, so we can't be sure where they're
    // actually live. All of these are also considered safepoints.
    std::vector<Instruction *> ReturnsTwice;

    // The set of values live at a particular safepoint
    std::vector<BitVector> LiveSets;
    // Those values that - if live out from our parent basic block - are live
    // at this safepoint.
    std::vector<std::vector<int>> LiveIfLiveOut;
    // We don't bother doing liveness on Allocas that were not mem2reg'ed.
    // they just get directly sunk into the root array.
    std::vector<AllocaInst *> Allocas;
    DenseMap<AllocaInst *, unsigned> ArrayAllocas;
    DenseMap<AllocaInst *, AllocaInst *> ShadowAllocas;
    std::vector<std::pair<StoreInst *, unsigned>> TrackedStores;
    State(Function &F) : F(&F), DT(nullptr), MaxPtrNumber(-1), MaxSafepointNumber(-1) {}
};

namespace llvm {
    void initializeLateLowerGCFramePass(PassRegistry &Registry);
}

struct LateLowerGCFrame: public FunctionPass, private JuliaPassContext {
    static char ID;
    LateLowerGCFrame() : FunctionPass(ID)
    {
        llvm::initializeDominatorTreeWrapperPassPass(*PassRegistry::getPassRegistry());
    }

protected:
    void getAnalysisUsage(AnalysisUsage &AU) const override {
        FunctionPass::getAnalysisUsage(AU);
        AU.addRequired<DominatorTreeWrapperPass>();
        AU.addPreserved<DominatorTreeWrapperPass>();
        AU.setPreservesCFG();
    }

private:
    CallInst *ptlsStates;

    void MaybeNoteDef(State &S, BBState &BBS, Value *Def, const std::vector<int> &SafepointsSoFar, SmallVector<int, 1> &&RefinedPtr = SmallVector<int, 1>());
    void NoteUse(State &S, BBState &BBS, Value *V, BitVector &Uses);
    void NoteUse(State &S, BBState &BBS, Value *V) {
        NoteUse(S, BBS, V, BBS.UpExposedUses);
    }

    void LiftPhi(State &S, PHINode *Phi);
    void LiftSelect(State &S, SelectInst *SI);
    Value *MaybeExtractScalar(State &S, std::pair<Value*,int> ValExpr, Instruction *InsertBefore);
    std::vector<Value*> MaybeExtractVector(State &S, Value *BaseVec, Instruction *InsertBefore);
    Value *GetPtrForNumber(State &S, unsigned Num, Instruction *InsertBefore);

    int Number(State &S, Value *V);
    int NumberBase(State &S, Value *Base);
    std::vector<int> NumberAll(State &S, Value *V);
    std::vector<int> NumberAllBase(State &S, Value *Base);

    void NoteOperandUses(State &S, BBState &BBS, User &UI);
    void MaybeTrackDst(State &S, MemTransferInst *MI);
    void MaybeTrackStore(State &S, StoreInst *I);
    State LocalScan(Function &F);
    void ComputeLiveness(State &S);
    void ComputeLiveSets(State &S);
    std::vector<int> ColorRoots(const State &S);
    void PlaceGCFrameStore(State &S, unsigned R, unsigned MinColorRoot, const std::vector<int> &Colors, Value *GCFrame, Instruction *InsertBefore);
    void PlaceGCFrameStores(State &S, unsigned MinColorRoot, const std::vector<int> &Colors, Value *GCFrame);
    void PlaceRootsAndUpdateCalls(std::vector<int> &Colors, State &S, std::map<Value *, std::pair<int, int>>);
    bool doInitialization(Module &M) override;
    bool runOnFunction(Function &F) override;
    bool CleanupIR(Function &F, State *S=nullptr);
    void NoteUseChain(State &S, BBState &BBS, User *TheUser);
    SmallVector<int, 1> GetPHIRefinements(PHINode *phi, State &S);
    void FixUpRefinements(ArrayRef<int> PHINumbers, State &S);
    void RefineLiveSet(BitVector &LS, State &S);
    Value *EmitTagPtr(IRBuilder<> &builder, Type *T, Value *V);
    Value *EmitLoadTag(IRBuilder<> &builder, Value *V);
};

static unsigned getValueAddrSpace(Value *V) {
    return V->getType()->getPointerAddressSpace();
}

static unsigned isTrackedValue(Value *V) {
    PointerType *PT = dyn_cast<PointerType>(V->getType()->getScalarType());
    return PT && PT->getAddressSpace() == AddressSpace::Tracked;
}

static bool isSpecialPtr(Type *Ty) {
    PointerType *PTy = dyn_cast<PointerType>(Ty);
    if (!PTy)
        return false;
    unsigned AS = PTy->getAddressSpace();
    return AddressSpace::FirstSpecial <= AS && AS <= AddressSpace::LastSpecial;
}

// return how many Special pointers are in T (count > 0),
// and if there is anything else in T (all == false)
CountTrackedPointers::CountTrackedPointers(Type *T) {
    if (isa<PointerType>(T)) {
        if (isSpecialPtr(T)) {
            count++;
            if (T->getPointerAddressSpace() != AddressSpace::Tracked)
                derived = true;
        }
    } else if (isa<StructType>(T) || isa<ArrayType>(T) || isa<VectorType>(T)) {
        for (Type *ElT : T->subtypes()) {
            auto sub = CountTrackedPointers(ElT);
            count += sub.count;
            all &= sub.all;
            derived |= sub.derived;
        }
        if (isa<ArrayType>(T))
            count *= cast<ArrayType>(T)->getNumElements();
        else if (isa<VectorType>(T))
            count *= cast<VectorType>(T)->getNumElements();
    }
    if (count == 0)
        all = false;
}

unsigned getCompositeNumElements(Type *T) {
    if (auto *ST = dyn_cast<StructType>(T))
        return ST->getNumElements();
    else if (auto *AT = dyn_cast<ArrayType>(T))
        return AT->getNumElements();
    else
        return cast<VectorType>(T)->getNumElements();
}

// Walk through a Type, and record the element path to every tracked value inside
void TrackCompositeType(Type *T, std::vector<unsigned> &Idxs, std::vector<std::vector<unsigned>> &Numberings) {
    if (isa<PointerType>(T)) {
        if (T->getPointerAddressSpace() == AddressSpace::Tracked)
            Numberings.push_back(Idxs);
    }
    else if (isa<StructType>(T) || isa<ArrayType>(T) || isa<VectorType>(T)) {
        unsigned Idx, NumEl = getCompositeNumElements(T);
        for (Idx = 0; Idx < NumEl; Idx++) {
            Idxs.push_back(Idx);
#if JL_LLVM_VERSION >= 110000
            Type *ElT = GetElementPtrInst::getTypeAtIndex(T, Idx);
#else
            Type *ElT = cast<CompositeType>(T)->getTypeAtIndex(Idx);
#endif
            TrackCompositeType(ElT, Idxs, Numberings);
            Idxs.pop_back();
        }
    }
}

std::vector<std::vector<unsigned>> TrackCompositeType(Type *T) {
    std::vector<unsigned> Idxs;
    std::vector<std::vector<unsigned>> Numberings;
    TrackCompositeType(T, Idxs, Numberings);
    return Numberings;
}



// Walk through simple expressions to until we hit something that requires root numbering
// If the input value is a scalar (pointer), we may return a composite value as base
// in which case the second member of the pair is the index of the value in the vector.
static std::pair<Value*,int> FindBaseValue(const State &S, Value *V, bool UseCache = true) {
    Value *CurrentV = V;
    int fld_idx = -1;
    while (true) {
        if (UseCache) {
            if (CurrentV->getType()->isPointerTy()) {
                auto it = S.AllPtrNumbering.find(CurrentV);
                if (it != S.AllPtrNumbering.end())
                    return std::make_pair(CurrentV, fld_idx);
            } else {
                auto it = S.AllCompositeNumbering.find(CurrentV);
                if (it != S.AllCompositeNumbering.end())
                    return std::make_pair(CurrentV, fld_idx);
            }
        }
        // Note that this is true:
        //   assert(fld_idx == -1 ? CurrentV->getType()->isPointerTy() : CurrentV->getType()->isVectorPointerTy());
        if (isa<BitCastInst>(CurrentV))
            CurrentV = cast<BitCastInst>(CurrentV)->getOperand(0);
        else if (isa<AddrSpaceCastInst>(CurrentV)) {
            Value *NewV = cast<AddrSpaceCastInst>(CurrentV)->getOperand(0);
            if (getValueAddrSpace(NewV) == 0)
                break;
            CurrentV = NewV;
        } else if (auto *GEP = dyn_cast<GetElementPtrInst>(CurrentV)) {
            CurrentV = GEP->getOperand(0);
            // GEP can make vectors from a single base pointer
            if (fld_idx != -1 && !isa<VectorType>(CurrentV->getType())) {
                fld_idx = -1;
            }
        }
        else if (auto EEI = dyn_cast<ExtractElementInst>(CurrentV)) {
            assert(CurrentV->getType()->isPointerTy() && fld_idx == -1);
            // TODO: For now, only support constant index.
            auto IdxOp = cast<ConstantInt>(EEI->getIndexOperand());
            fld_idx = IdxOp->getLimitedValue(INT_MAX);
            CurrentV = EEI->getVectorOperand();
        }
        else if (auto LI = dyn_cast<LoadInst>(CurrentV)) {
            if (auto PtrT = dyn_cast<PointerType>(LI->getType()->getScalarType())) {
                if (PtrT->getAddressSpace() == AddressSpace::Loaded) {
                    CurrentV = LI->getPointerOperand();
                    fld_idx = -1;
                    if (!isSpecialPtr(CurrentV->getType())) {
                        // This could really be anything, but it's not loaded
                        // from a tracked pointer, so it doesn't matter what
                        // it is--just pick something simple.
                        CurrentV = ConstantPointerNull::get(Type::getInt8PtrTy(V->getContext()));
                    }
                    continue;
                }
            }
            // In general a load terminates a walk
            break;
        }
        else if (auto II = dyn_cast<IntrinsicInst>(CurrentV)) {
            // Some intrinsics behave like LoadInst followed by a SelectInst
            // This should never happen in a derived addrspace (since those cannot be stored to memory)
            // so we don't need to lift these operations, but we do need to check if it's loaded and continue walking the base pointer
            if (II->getIntrinsicID() == Intrinsic::masked_load ||
                II->getIntrinsicID() == Intrinsic::masked_gather) {
                if (auto VTy = dyn_cast<VectorType>(II->getType())) {
                    if (auto PtrT = dyn_cast<PointerType>(VTy->getElementType())) {
                        if (PtrT->getAddressSpace() == AddressSpace::Loaded) {
                            Value *Mask = II->getOperand(2);
                            Value *Passthrough = II->getOperand(3);
                            if (!isa<Constant>(Mask) || !cast<Constant>(Mask)->isAllOnesValue()) {
                                assert(isa<UndefValue>(Passthrough) && "unimplemented");
                                (void)Passthrough;
                            }
                            CurrentV = II->getOperand(0);
                            if (II->getIntrinsicID() == Intrinsic::masked_load) {
                                fld_idx = -1;
                                if (!isSpecialPtr(CurrentV->getType())) {
                                    CurrentV = ConstantPointerNull::get(Type::getInt8PtrTy(V->getContext()));
                                }
                            } else {
                                if (auto VTy2 = dyn_cast<VectorType>(CurrentV->getType())) {
                                    if (!isSpecialPtr(VTy2->getElementType())) {
                                        CurrentV = ConstantPointerNull::get(Type::getInt8PtrTy(V->getContext()));
                                        fld_idx = -1;
                                    }
                                }
                            }
                            continue;
                        }
                    }
                }
                // In general a load terminates a walk
                break;
            }
        }
        else {
            break;
        }
    }
    assert(isa<LoadInst>(CurrentV) || isa<CallInst>(CurrentV) ||
           isa<Argument>(CurrentV) || isa<SelectInst>(CurrentV) ||
           isa<PHINode>(CurrentV) || isa<AddrSpaceCastInst>(CurrentV) ||
           isa<Constant>(CurrentV) || isa<AllocaInst>(CurrentV) ||
           isa<InsertValueInst>(CurrentV) ||
           isa<ExtractValueInst>(CurrentV) ||
           isa<InsertElementInst>(CurrentV) ||
           isa<ShuffleVectorInst>(CurrentV));
    return std::make_pair(CurrentV, fld_idx);
}

Value *LateLowerGCFrame::MaybeExtractScalar(State &S, std::pair<Value*,int> ValExpr, Instruction *InsertBefore) {
    Value *V = ValExpr.first;
    if (isa<PointerType>(V->getType())) {
        assert(ValExpr.second == -1);
        if (!isTrackedValue(V)) {
            int BaseNumber = NumberBase(S, V);
            if (BaseNumber >= 0)
                V = GetPtrForNumber(S, BaseNumber, InsertBefore);
            else
                V = ConstantPointerNull::get(cast<PointerType>(T_prjlvalue));
        }
    }
    else if (ValExpr.second != -1) {
        auto Tracked = TrackCompositeType(V->getType());
        auto Idxs = makeArrayRef(Tracked.at(ValExpr.second));
        auto IdxsNotVec = Idxs.slice(0, Idxs.size() - 1);
        Type *FinalT = ExtractValueInst::getIndexedType(V->getType(), IdxsNotVec);
        bool IsVector = isa<VectorType>(FinalT);
        PointerType *T = cast<PointerType>(
#if JL_LLVM_VERSION >= 110000
            GetElementPtrInst::getTypeAtIndex(FinalT, Idxs.back()));
#else
            cast<CompositeType>(FinalT)->getTypeAtIndex(Idxs.back()));
#endif
        if (T->getAddressSpace() != AddressSpace::Tracked) {
            // if V isn't tracked, get the shadow def
            auto Numbers = NumberAllBase(S, V);
            int BaseNumber = Numbers.at(ValExpr.second);
            if (BaseNumber >= 0)
                V = GetPtrForNumber(S, BaseNumber, InsertBefore);
            else
                V = ConstantPointerNull::get(cast<PointerType>(T_prjlvalue));
            return V;
        }
        if (Idxs.size() > IsVector)
            V = ExtractValueInst::Create(V, IsVector ? IdxsNotVec : Idxs, "", InsertBefore);
        if (IsVector)
            V = ExtractElementInst::Create(V,
                    ConstantInt::get(Type::getInt32Ty(V->getContext()), Idxs.back()),
                    "", InsertBefore);
    }
    return V;
}

std::vector<Value*> LateLowerGCFrame::MaybeExtractVector(State &S, Value *BaseVec, Instruction *InsertBefore) {
    auto Numbers = NumberAllBase(S, BaseVec);
    std::vector<Value*> V{Numbers.size()};
    Value *V_rnull = ConstantPointerNull::get(cast<PointerType>(T_prjlvalue));
    for (unsigned i = 0; i < V.size(); ++i) {
        if (Numbers[i] >= 0)
            V[i] = GetPtrForNumber(S, Numbers[i], InsertBefore);
        else
            V[i] = V_rnull;
    }
    return V;
}

Value *LateLowerGCFrame::GetPtrForNumber(State &S, unsigned Num, Instruction *InsertBefore)
{
    Value *Val = S.ReversePtrNumbering[Num];
    unsigned Idx = -1;
    if (!isa<PointerType>(Val->getType())) {
        const std::vector<int> &AllNums = S.AllCompositeNumbering[Val];
        for (Idx = 0; Idx < AllNums.size(); ++Idx) {
            if ((unsigned)AllNums[Idx] == Num)
                break;
        }
        assert(Idx < AllNums.size());
    }
    return MaybeExtractScalar(S, std::make_pair(Val, Idx), InsertBefore);
}

void LateLowerGCFrame::LiftSelect(State &S, SelectInst *SI) {
    if (isa<PointerType>(SI->getType()) ?
            S.AllPtrNumbering.count(SI) :
            S.AllCompositeNumbering.count(SI)) {
        // already visited here--nothing to do
        return;
    }
    std::vector<int> Numbers;
    unsigned NumRoots = 1;
    if (auto VTy = dyn_cast<VectorType>(SI->getType()))
        Numbers.resize(VTy->getNumElements(), -1);
    else
        assert(isa<PointerType>(SI->getType()) && "unimplemented");
    assert(!isTrackedValue(SI));
    // find the base root for the arguments
    Value *TrueBase = MaybeExtractScalar(S, FindBaseValue(S, SI->getTrueValue(), false), SI);
    Value *FalseBase = MaybeExtractScalar(S, FindBaseValue(S, SI->getFalseValue(), false), SI);
    std::vector<Value*> TrueBases;
    std::vector<Value*> FalseBases;
    if (!isa<PointerType>(TrueBase->getType())) {
        TrueBases = MaybeExtractVector(S, TrueBase, SI);
        assert(TrueBases.size() == Numbers.size());
        NumRoots = TrueBases.size();
    }
    if (!isa<PointerType>(FalseBase->getType())) {
        FalseBases = MaybeExtractVector(S, FalseBase, SI);
        assert(FalseBases.size() == Numbers.size());
        NumRoots = FalseBases.size();
    }
    if (isa<PointerType>(SI->getType()) ?
            S.AllPtrNumbering.count(SI) :
            S.AllCompositeNumbering.count(SI)) {
        // MaybeExtractScalar or MaybeExtractVector handled this for us (recursively, though a PHINode)
        return;
    }
    // need to handle each element (may just be one scalar)
    for (unsigned i = 0; i < NumRoots; ++i) {
        Value *TrueElem;
        if (isa<PointerType>(TrueBase->getType()))
            TrueElem = TrueBase;
        else
            TrueElem = TrueBases[i];
        Value *FalseElem;
        if (isa<PointerType>(FalseBase->getType()))
            FalseElem = FalseBase;
        else
            FalseElem = FalseBases[i];
        Value *Cond = SI->getCondition();
        if (isa<VectorType>(Cond->getType())) {
            Cond = ExtractElementInst::Create(Cond,
                    ConstantInt::get(Type::getInt32Ty(Cond->getContext()), i),
                    "", SI);
        }
        if (FalseElem->getType() != TrueElem->getType())
            FalseElem = new BitCastInst(FalseElem, TrueElem->getType(), "", SI);
        SelectInst *SelectBase = SelectInst::Create(Cond, TrueElem, FalseElem, "gclift", SI);
        int Number = ++S.MaxPtrNumber;
        S.AllPtrNumbering[SelectBase] = Number;
        S.ReversePtrNumbering[Number] = SelectBase;
        if (isa<PointerType>(SI->getType()))
            S.AllPtrNumbering[SI] = Number;
        else
            Numbers[i] = Number;
    }
    if (auto VTy = dyn_cast<VectorType>(SI->getType())) {
        if (NumRoots != Numbers.size()) {
            // broadcast the scalar root number to fill the vector
            assert(NumRoots == 1);
            int Number = Numbers[0];
            Numbers.resize(0);
            Numbers.resize(VTy->getNumElements(), Number);
        }
    }
    if (!isa<PointerType>(SI->getType()))
        S.AllCompositeNumbering[SI] = Numbers;
}

void LateLowerGCFrame::LiftPhi(State &S, PHINode *Phi) {
    if (isa<PointerType>(Phi->getType()) ?
            S.AllPtrNumbering.count(Phi) :
            S.AllCompositeNumbering.count(Phi))
        return;
    // need to handle each element (may just be one scalar)
    SmallVector<PHINode *, 2> lifted;
    std::vector<int> Numbers;
    unsigned NumRoots = 1;
    if (auto VTy = dyn_cast<VectorType>(Phi->getType())) {
        NumRoots = VTy->getNumElements();
        Numbers.resize(NumRoots);
    }
    else {
        assert(isa<PointerType>(Phi->getType()) && "unimplemented");
    }
    for (unsigned i = 0; i < NumRoots; ++i) {
        PHINode *lift = PHINode::Create(T_prjlvalue, Phi->getNumIncomingValues(), "gclift", Phi);
        int Number = ++S.MaxPtrNumber;
        S.AllPtrNumbering[lift] = Number;
        S.ReversePtrNumbering[Number] = lift;
        if (!isa<VectorType>(Phi->getType()))
            S.AllPtrNumbering[Phi] = Number;
        else
            Numbers[i] = Number;
        lifted.push_back(lift);
    }
    if (!isa<PointerType>(Phi->getType()))
        S.AllCompositeNumbering[Phi] = Numbers;
    for (unsigned i = 0; i < Phi->getNumIncomingValues(); ++i) {
        Value *Incoming = Phi->getIncomingValue(i);
        BasicBlock *IncomingBB = Phi->getIncomingBlock(i);
        Instruction *Terminator = IncomingBB->getTerminator();
        Value *Base = MaybeExtractScalar(S, FindBaseValue(S, Incoming, false), Terminator);
        std::vector<Value*> IncomingBases;
        if (!isa<PointerType>(Base->getType())) {
            IncomingBases = MaybeExtractVector(S, Base, Terminator);
            assert(IncomingBases.size() == NumRoots);
        }
        for (unsigned i = 0; i < NumRoots; ++i) {
            PHINode *lift = lifted[i];
            Value *BaseElem;
            if (isa<PointerType>(Base->getType()))
                BaseElem = Base;
            else
                BaseElem = IncomingBases[i];
            if (BaseElem->getType() != T_prjlvalue)
                BaseElem = new BitCastInst(BaseElem, T_prjlvalue, "", Terminator);
            lift->addIncoming(BaseElem, IncomingBB);
        }
    }
}

int LateLowerGCFrame::NumberBase(State &S, Value *CurrentV)
{
    auto it = S.AllPtrNumbering.find(CurrentV);
    if (it != S.AllPtrNumbering.end())
        return it->second;
    int Number;
    if (isa<Constant>(CurrentV)) {
        // Perm rooted
        Number = -2;
    } else if (isa<Argument>(CurrentV) || isa<AllocaInst>(CurrentV) ||
            (isa<AddrSpaceCastInst>(CurrentV) && !isTrackedValue(CurrentV))) {
        // We know this is rooted in the parent
        // future note: we could chose to exclude argument of type CalleeRooted here
        Number = -1;
    } else if (!isSpecialPtr(CurrentV->getType())) {
        // Externally rooted somehow hopefully (otherwise there's a bug in the
        // input IR)
        Number = -1;
    } else if (isa<SelectInst>(CurrentV) && !isTrackedValue(CurrentV)) {
        LiftSelect(S, cast<SelectInst>(CurrentV));
        Number = S.AllPtrNumbering.at(CurrentV);
        return Number;
    } else if (isa<PHINode>(CurrentV) && !isTrackedValue(CurrentV)) {
        LiftPhi(S, cast<PHINode>(CurrentV));
        Number = S.AllPtrNumbering.at(CurrentV);
        return Number;
    } else if (isa<ExtractValueInst>(CurrentV)) {
        auto Numbers = NumberAllBase(S, CurrentV);
        assert(Numbers.size() == 1);
        Number = Numbers[0];
    } else {
        assert((CurrentV->getType()->isPointerTy() && isTrackedValue(CurrentV)));
        Number = ++S.MaxPtrNumber;
        S.ReversePtrNumbering[Number] = CurrentV;
    }
    S.AllPtrNumbering[CurrentV] = Number;
    return Number;
}

int LateLowerGCFrame::Number(State &S, Value *V) {
    assert(isSpecialPtr(V->getType()));
    auto CurrentV = FindBaseValue(S, V);
    int Number;
    if (CurrentV.second == -1) {
        Number = NumberBase(S, CurrentV.first);
    } else {
        auto Numbers = NumberAllBase(S, CurrentV.first);
        Number = Numbers.at(CurrentV.second);
    }
    if (V != CurrentV.first)
        S.AllPtrNumbering[V] = Number;
    return Number;
}

// assign pointer numbers to a def instruction
std::vector<int> LateLowerGCFrame::NumberAllBase(State &S, Value *CurrentV) {
    if (isa<PointerType>(CurrentV->getType())) {
        auto it = S.AllPtrNumbering.find(CurrentV);
        if (it != S.AllPtrNumbering.end())
            return std::vector<int>({it->second});
    } else {
        auto it = S.AllCompositeNumbering.find(CurrentV);
        if (it != S.AllCompositeNumbering.end())
            return it->second;
    }

    std::vector<int> Numbers;
    auto tracked = CountTrackedPointers(CurrentV->getType());
    if (tracked.count == 0)
        return Numbers;
    if (isa<Constant>(CurrentV) || isa<AllocaInst>(CurrentV) || isa<Argument>(CurrentV) ||
            (isa<AddrSpaceCastInst>(CurrentV) && !isTrackedValue(CurrentV))) {
        Numbers.resize(tracked.count, -1);
    }
    else if (auto *SVI = dyn_cast<ShuffleVectorInst>(CurrentV)) {
        std::vector<int> Numbers1 = NumberAll(S, SVI->getOperand(0));
        std::vector<int> Numbers2 = NumberAll(S, SVI->getOperand(1));
        auto Mask = SVI->getShuffleMask();
        for (auto idx : Mask) {
            assert(idx != -1 && "Undef tracked value is invalid");
            if ((unsigned)idx < Numbers1.size()) {
                Numbers.push_back(Numbers1.at(idx));
            } else {
                Numbers.push_back(Numbers2.at(idx - Numbers1.size()));
            }
        }
    } else if (auto *IEI = dyn_cast<InsertElementInst>(CurrentV)) {
        // TODO: handle non-constant: LiftInsertElement(S, IEI)
        unsigned idx = cast<ConstantInt>(IEI->getOperand(2))->getZExtValue();
        Numbers = NumberAll(S, IEI->getOperand(0));
        int ElNumber = Number(S, IEI->getOperand(1));
        Numbers[idx] = ElNumber;
    } else if (auto *IVI = dyn_cast<InsertValueInst>(CurrentV)) {
        Numbers = NumberAll(S, IVI->getAggregateOperand());
        auto Tracked = TrackCompositeType(IVI->getType());
        assert(Tracked.size() == Numbers.size());
        std::vector<int> InsertNumbers = NumberAll(S, IVI->getInsertedValueOperand());
        auto Idxs = IVI->getIndices();
        unsigned j = 0;
        for (unsigned i = 0; i < Tracked.size(); ++i) {
            auto Elem = makeArrayRef(Tracked[i]);
            if (Elem.size() < Idxs.size())
                continue;
            if (Idxs.equals(Elem.slice(0, Idxs.size()))) // Tracked.startswith(Idxs)
                Numbers[i] = InsertNumbers[j++];
        }
        assert(j == InsertNumbers.size());
    } else if (auto *EVI = dyn_cast<ExtractValueInst>(CurrentV)) {
        auto BaseNumbers = NumberAll(S, EVI->getAggregateOperand());
        auto Tracked = TrackCompositeType(EVI->getAggregateOperand()->getType());
        assert(Tracked.size() == BaseNumbers.size());
        auto Idxs = EVI->getIndices();
        for (unsigned i = 0; i < Tracked.size(); ++i) {
            auto Elem = makeArrayRef(Tracked[i]);
            if (Elem.size() < Idxs.size())
                continue;
            if (Idxs.equals(Elem.slice(0, Idxs.size()))) // Tracked.startswith(Idxs)
                Numbers.push_back(BaseNumbers[i]);
        }
        assert(CountTrackedPointers(EVI->getType()).count == Numbers.size());
    } else if (tracked.derived) {
        if (isa<SelectInst>(CurrentV)) {
            LiftSelect(S, cast<SelectInst>(CurrentV));
        } else if (isa<PHINode>(CurrentV)) {
            LiftPhi(S, cast<PHINode>(CurrentV));
        // } else if (isa<ExtractElementInst>(CurrentV)) { // TODO: lifting for non constant index
        } else {
            CurrentV->print(errs());
            llvm_unreachable("Unexpected generating operation for derived values");
        }
        if (isa<PointerType>(CurrentV->getType())) {
            auto Number = S.AllPtrNumbering.at(CurrentV);
            Numbers.resize(1, Number);
        } else {
            Numbers = S.AllCompositeNumbering.at(CurrentV);
        }
    } else {
        assert((isa<LoadInst>(CurrentV) || isa<CallInst>(CurrentV) || isa<PHINode>(CurrentV) || isa<SelectInst>(CurrentV))
                && "unexpected def expression");
        // This is simple, we can just number them sequentially
        for (unsigned i = 0; i < tracked.count; ++i) {
            int Num = ++S.MaxPtrNumber;
            Numbers.push_back(Num);
            S.ReversePtrNumbering[Num] = CurrentV;
        }
    }
    if (isa<PointerType>(CurrentV->getType())) {
        assert(Numbers.size() == 1);
        S.AllPtrNumbering[CurrentV] = Numbers[0];
    } else {
        S.AllCompositeNumbering[CurrentV] = Numbers;
    }
    return Numbers;
}

// gets the pointer number for every gc tracked value inside V
std::vector<int> LateLowerGCFrame::NumberAll(State &S, Value *V) {
    if (isa<PointerType>(V->getType())) {
        auto it = S.AllPtrNumbering.find(V);
        if (it != S.AllPtrNumbering.end())
            return std::vector<int>({it->second});
    } else {
        auto it = S.AllCompositeNumbering.find(V);
        if (it != S.AllCompositeNumbering.end())
            return it->second;
    }
    std::vector<int> Numbers;
    auto tracked = CountTrackedPointers(V->getType());
    if (tracked.count == 0)
        return Numbers;
    auto CurrentV = FindBaseValue(S, V);
    int Number = -1;
    if (isa<PointerType>(CurrentV.first->getType())) {
        // Base turned out to be a single pointer--number it
        assert(CurrentV.second == -1);
        Number = NumberBase(S, CurrentV.first);
        Numbers.resize(tracked.count, Number);
    } else {
        // Base turned out to be an aggregate--get all numbers for it, then sub-select
        Numbers = NumberAllBase(S, CurrentV.first);
        if (CurrentV.second != -1) {
            Number = Numbers[CurrentV.second]; // only needed a subset of the values
            Numbers.resize(tracked.count, Number);
        }
        else
            assert(!isa<PointerType>(V->getType()));
    }
    if (CurrentV.first != V) {
        if (isa<PointerType>(V->getType())) {
            S.AllPtrNumbering[V] = Number;
        } else {
            S.AllCompositeNumbering[V] = Numbers;
        }
    }
    return Numbers;
}


static void MaybeResize(BBState &BBS, unsigned Idx) {
    if (BBS.Defs.size() <= Idx) {
        BBS.Defs.resize(Idx + 1);
        BBS.UpExposedUses.resize(Idx + 1);
        BBS.PhiOuts.resize(Idx + 1);
    }
}

static bool HasBitSet(const BitVector &BV, unsigned Bit) {
    return Bit < BV.size() && BV[Bit];
}

static void NoteDef(State &S, BBState &BBS, int Num, const std::vector<int> &SafepointsSoFar) {
    assert(Num >= 0);
    MaybeResize(BBS, Num);
    assert(BBS.Defs[Num] == 0 && "SSA Violation or misnumbering?");
    BBS.Defs[Num] = 1;
    BBS.UpExposedUses[Num] = 0;
    // This value could potentially be live at any following safe point
    // if it ends up live out, so add it to the LiveIfLiveOut lists for all
    // following safepoints.
    for (int Safepoint : SafepointsSoFar) {
        S.LiveIfLiveOut[Safepoint].push_back(Num);
    }
}

void LateLowerGCFrame::MaybeNoteDef(State &S, BBState &BBS, Value *Def, const std::vector<int> &SafepointsSoFar, SmallVector<int, 1> &&RefinedPtr) {
    Type *RT = Def->getType();
    if (isa<PointerType>(RT)) {
        if (!isSpecialPtr(RT))
            return;
        assert(isTrackedValue(Def) && "Returned value of GC interest, but not tracked?");
        int Num = Number(S, Def);
        NoteDef(S, BBS, Num, SafepointsSoFar);
        if (!RefinedPtr.empty())
            S.Refinements[Num] = std::move(RefinedPtr);
    }
    else {
        std::vector<int> Nums = NumberAll(S, Def);
        for (int Num : Nums) {
            NoteDef(S, BBS, Num, SafepointsSoFar);
            if (!RefinedPtr.empty())
                S.Refinements[Num] = RefinedPtr;
        }
    }
}

static int NoteSafepoint(State &S, BBState &BBS, CallInst *CI) {
    int Number = ++S.MaxSafepointNumber;
    S.SafepointNumbering[CI] = Number;
    S.ReverseSafepointNumbering.push_back(CI);
    // Note which pointers are upward exposed live here. They need to be
    // considered live at this safepoint even when they have a def earlier
    // in this BB (i.e. even when they don't participate in the dataflow
    // computation)
    S.LiveSets.push_back(BBS.UpExposedUses);
    S.LiveIfLiveOut.push_back(std::vector<int>{});
    return Number;
}

void LateLowerGCFrame::NoteUse(State &S, BBState &BBS, Value *V, BitVector &Uses) {
    // Short circuit to avoid having to deal with vectors of constants, etc.
    if (isa<Constant>(V))
        return;
    if (isa<PointerType>(V->getType())) {
        if (isSpecialPtr(V->getType())) {
            int Num = Number(S, V);
            if (Num < 0)
                return;
            MaybeResize(BBS, Num);
            Uses[Num] = 1;
        }
    } else {
        std::vector<int> Nums = NumberAll(S, V);
        for (int Num : Nums) {
            if (Num < 0)
                continue;
            MaybeResize(BBS, Num);
            Uses[Num] = 1;
        }
    }
}

void LateLowerGCFrame::NoteOperandUses(State &S, BBState &BBS, User &UI) {
    for (Use &U : UI.operands()) {
        NoteUse(S, BBS, U);
    }
}

template <typename VisitInst, typename callback>
void RecursivelyVisit(callback f, Value *V) {
    for (Use &VU : V->uses()) {
        User *TheUser = VU.getUser();
        if (isa<VisitInst>(TheUser))
            f(VU);
        if (isa<CallInst>(TheUser) || isa<LoadInst>(TheUser) ||
            isa<SelectInst>(TheUser) || isa<PHINode>(TheUser) ||
            isa<StoreInst>(TheUser) || isa<PtrToIntInst>(TheUser))
            continue;
        if (isa<GetElementPtrInst>(TheUser) || isa<BitCastInst>(TheUser) || isa<AddrSpaceCastInst>(TheUser)) {
            RecursivelyVisit<VisitInst, callback>(f, TheUser);
            continue;
        }
        llvm_dump(V);
        llvm_dump(TheUser);
        assert(false && "Unexpected instruction");
    }
}

static void dumpBitVectorValues(State &S, BitVector &BV) {
    bool first = true;
    for (int Idx = BV.find_first(); Idx >= 0; Idx = BV.find_next(Idx)) {
        if (!first)
            dbgs() << ", ";
        first = false;
        S.ReversePtrNumbering[Idx]->printAsOperand(dbgs());
    }
}

/* Debugging utility to dump liveness information */
JL_USED_FUNC static void dumpLivenessState(Function &F, State &S) {
    for (auto &BB : F) {
        dbgs() << "Liveness analysis for BB " << BB.getName();
        dbgs() << "\n\tDefs: ";
        dumpBitVectorValues(S, S.BBStates[&BB].Defs);
        dbgs() << "\n\tPhiOuts: ";
        dumpBitVectorValues(S, S.BBStates[&BB].PhiOuts);
        dbgs() << "\n\tUpExposedUses: ";
        dumpBitVectorValues(S, S.BBStates[&BB].UpExposedUses);
        dbgs() << "\n\tLiveIn: ";
        dumpBitVectorValues(S, S.BBStates[&BB].LiveIn);
        dbgs() << "\n\tLiveOut: ";
        dumpBitVectorValues(S, S.BBStates[&BB].LiveOut);
        dbgs() << "\n";
    }
}

static bool isTBAA(MDNode *TBAA, std::initializer_list<const char*> const strset)
{
    if (!TBAA)
        return false;
    while (TBAA->getNumOperands() > 1) {
        TBAA = cast<MDNode>(TBAA->getOperand(1).get());
        auto str = cast<MDString>(TBAA->getOperand(0))->getString();
        for (auto str2 : strset) {
            if (str == str2) {
                return true;
            }
        }
    }
    return false;
}

// Check if this is a load from an immutable value. The easiest
// way to do so is to look at the tbaa and see if it derives from
// jtbaa_immut.
static bool isLoadFromImmut(LoadInst *LI)
{
    if (LI->getMetadata(LLVMContext::MD_invariant_load))
        return true;
    MDNode *TBAA = LI->getMetadata(LLVMContext::MD_tbaa);
    if (isTBAA(TBAA, {"jtbaa_immut", "jtbaa_const"}))
        return true;
    return false;
}

static bool isConstGV(GlobalVariable *gv)
{
    return gv->isConstant() || gv->getMetadata("julia.constgv");
}

static bool isLoadFromConstGV(LoadInst *LI, bool &task_local);
static bool isLoadFromConstGV(Value *v, bool &task_local)
{
    v = v->stripInBoundsOffsets();
    if (auto LI = dyn_cast<LoadInst>(v))
        return isLoadFromConstGV(LI, task_local);
    if (auto gv = dyn_cast<GlobalVariable>(v))
        return isConstGV(gv);
    // null pointer
    if (isa<ConstantData>(v))
        return true;
    // literal pointers
    if (auto CE = dyn_cast<ConstantExpr>(v))
        return (CE->getOpcode() == Instruction::IntToPtr &&
                isa<ConstantData>(CE->getOperand(0)));
    if (auto SL = dyn_cast<SelectInst>(v))
        return (isLoadFromConstGV(SL->getTrueValue(), task_local) &&
                isLoadFromConstGV(SL->getFalseValue(), task_local));
    if (auto Phi = dyn_cast<PHINode>(v)) {
        auto n = Phi->getNumIncomingValues();
        for (unsigned i = 0; i < n; ++i) {
            if (!isLoadFromConstGV(Phi->getIncomingValue(i), task_local)) {
                return false;
            }
        }
        return true;
    }
    if (auto call = dyn_cast<CallInst>(v)) {
        auto callee = call->getCalledFunction();
        if (callee && callee->getName() == "julia.typeof") {
            return true;
        }
        if (callee && callee->getName() == "julia.ptls_states") {
            task_local = true;
            return true;
        }
    }
    if (isa<Argument>(v)) {
        task_local = true;
        return true;
    }
    return false;
}

// Check if this is can be traced through constant loads to an constant global
// or otherwise globally rooted value.
// Almost all `tbaa_const` loads satisfies this with the exception of
// task local constants which are constant as far as the code is concerned but aren't
// global constants. For task local constant `task_local` will be true when this function
// returns.
//
// The white list implemented here and above in `isLoadFromConstGV(Value*)` should
// cover all the cases we and LLVM generates.
static bool isLoadFromConstGV(LoadInst *LI, bool &task_local)
{
    // We only emit single slot GV in codegen
    // but LLVM global merging can change the pointer operands to GEPs/bitcasts
    auto load_base = LI->getPointerOperand()->stripInBoundsOffsets();
    auto gv = dyn_cast<GlobalVariable>(load_base);
    if (isTBAA(LI->getMetadata(LLVMContext::MD_tbaa), {"jtbaa_immut", "jtbaa_const"})) {
        if (gv)
            return true;
        return isLoadFromConstGV(load_base, task_local);
    }
    if (gv)
        return isConstGV(gv);
    return false;
}

static uint64_t getLoadValueAlign(LoadInst *LI)
{
    MDNode *md = LI->getMetadata(LLVMContext::MD_align);
    if (!md)
        return 1;
    return mdconst::extract<ConstantInt>(md->getOperand(0))->getLimitedValue();
}

static bool LooksLikeFrameRef(Value *V) {
    if (isSpecialPtr(V->getType()))
        return false;
    if (isa<GetElementPtrInst>(V))
        return LooksLikeFrameRef(cast<GetElementPtrInst>(V)->getOperand(0));
    return isa<Argument>(V);
}

SmallVector<int, 1> LateLowerGCFrame::GetPHIRefinements(PHINode *Phi, State &S)
{
    // The returned vector can violate the domination property of the Refinements map.
    // However, we can't know for sure if this is valid here since incoming values
    // that does not dominate the PHI node may be externally rooted (i.e. can be refined to -1)
    // We only know that after scanning the whole function so we'll record the possibly invalid
    // edges here and fix them up at the end of `LocalScan`. (See `FixUpRefinements` below).
    auto nIncoming = Phi->getNumIncomingValues();
    SmallVector<int, 1> RefinedPtr(nIncoming);
    for (unsigned i = 0; i < nIncoming; ++i)
        RefinedPtr[i] = Number(S, Phi->getIncomingValue(i));
    return RefinedPtr;
}

JL_USED_FUNC static void DumpRefinements(State *S)
{
    for (auto &kv: S->Refinements) {
        int Num = kv.first;
        if (Num < 0)
            continue;
        dbgs() << "Refinements for " << Num << "  --  ";
        auto V = S->ReversePtrNumbering[Num];
        llvm_dump(V);
        for (auto refine: kv.second) {
            if (refine < 0) {
                dbgs() << "  " << (int)refine;
                continue;
            }
            dbgs() << "  " << (int)refine << ": ";
            auto R = S->ReversePtrNumbering[refine];
            llvm_dump(R);
        }
    }
}

void LateLowerGCFrame::FixUpRefinements(ArrayRef<int> PHINumbers, State &S)
{
    // Now we have all the possible refinement information, we can remove ones for the invalid

    // * First find all values that must be externally rooted.
    //   Values may either be obviously externally rooted (e.g. arguments) - (this is indicated by a
    //   value of -1 or -2 in the refinement map), or may be externally rooted by refinement to other
    //   values. Thus a value is not externally rooted if it either:
    //   either:
    //     - Has no refinements (all obiviously externally rooted values are annotated by -1/-2 in the
    //       refinement map).
    //     - Recursively reaches a not-externally rooted value through its refinements
    //
    //   We compute this set by first assuming all values are externally rooted and then iteratively
    //   removing the ones that are not.
    BitVector extern_rooted(S.MaxPtrNumber + 1, true);
    BitVector perm_rooted(S.MaxPtrNumber + 1, true);

    //   * First clear all values that are not derived from anything.
    //     This only needs to be done once.
    for (int i = 0; i <= S.MaxPtrNumber; i++) {
        auto it = S.Refinements.find(i);
        if (it == S.Refinements.end() || it->second.empty()) {
            extern_rooted[i] = false;
            perm_rooted[i] = false;
        }
    }
    //   * Then remove values reachable from those values recursively
    bool changed;
    do {
        changed = false;
        for (auto &kv: S.Refinements) {
            int Num = kv.first;
            // Already cleared.
            if (!HasBitSet(extern_rooted, Num))
                continue;
            for (auto refine: kv.second) {
                if (refine == -2) {
                    continue;
                }
                else if (refine == -1) {
                    if (HasBitSet(perm_rooted, Num)) {
                        changed = true;
                        perm_rooted[Num] = false;
                    }
                    continue;
                }
                else if (!HasBitSet(extern_rooted, refine)) {
                    changed = true;
                    extern_rooted[Num] = false;
                    perm_rooted[Num] = false;
                    break;
                }
                else if (!HasBitSet(perm_rooted, refine)) {
                    if (HasBitSet(perm_rooted, Num)) {
                        changed = true;
                        perm_rooted[Num] = false;
                    }
                }
            }
        }
    } while (changed);
    //   * Now the `extern_rooted` and `perm_rooted` map is accurate,
    //     normalize all externally rooted values.
    for (auto &kv: S.Refinements) {
        int Num = kv.first;
        if (HasBitSet(perm_rooted, Num)) {
            // For permanently rooted values, set their refinements simply to `{-2}`
            kv.second.resize(1);
            kv.second[0] = -2;
            continue;
        }
        else if (HasBitSet(extern_rooted, Num)) {
            // For externally rooted values, set their refinements simply to `{-1}`
            kv.second.resize(1);
            kv.second[0] = -1;
            continue;
        }
        for (auto &refine: kv.second) {
            // For other values,
            // remove all externally rooted values from their refinements (replace with -1)
            // No need to handle -2 specially since it won't make a difference.
            if (HasBitSet(extern_rooted, refine)) {
                refine = -1;
            }
        }
    }
    // Scan all phi node refinements and remove all invalid ones.
    // As a generalization to what we did to externally rooted values above,
    // we can also relax non-dominating (invalid) refinements to the refinements of those values
    // If all of those values dominate the phi node then the phi node can be refined to
    // those values instead.
    // While we recursively relax the refinement, we need to keep track of the values we've
    // visited in order to not scan them again.
    BitVector visited(S.MaxPtrNumber + 1, false);
    for (auto Num: PHINumbers) {
        // Not sure if `Num` can be `-1`
        if (Num < 0 || HasBitSet(extern_rooted, Num))
            continue;
        // N.B.: We reset the bit vector below on every iteration
        visited[Num] = true;
        auto Phi = cast<PHINode>(S.ReversePtrNumbering[Num]);
        auto &RefinedPtr = S.Refinements[Num];
        unsigned j = 0; // new length
        for (unsigned i = 0; i < RefinedPtr.size(); i++) {
            auto refine = RefinedPtr[i];
            if (refine < 0 || visited[refine])
                continue;
            visited[refine] = true;
            if (i != j)
                RefinedPtr[j] = refine;
            j++;
            if (auto inst = dyn_cast<Instruction>(S.ReversePtrNumbering[refine])) {
                if (!S.DT)
                    S.DT = &getAnalysis<DominatorTreeWrapperPass>().getDomTree();
                if (S.DT->dominates(inst, Phi))
                    continue;
                // Decrement `j` so we'll overwrite/ignore it.
                j--;
                // Non-dominating refinement
                auto it = S.Refinements.find(refine);
                if (it != S.Refinements.end() && !it->second.empty()) {
                    // Found a replacement, replace current element.
                    auto &NewRefinedPtr = it->second;
                    unsigned n = NewRefinedPtr.size();
                    // First fill in the gap between `i` and `j`
                    unsigned k = 0;
                    for (; k < n && i >= j + k; k++)
                        RefinedPtr[i - k] = NewRefinedPtr[k];
                    i = i - k;
                    if (k < n)
                        RefinedPtr.append(it->second.begin() + k, it->second.end());
                    continue;
                }
                // Invalid - Remove All refinements
                RefinedPtr.resize(0);
                break;
            }
        }
        if (!RefinedPtr.empty()) {
            // `j == 0` here means that everything is externally rooted.
            // This should have been handled by the first loop above.
            assert(j != 0 && j <= RefinedPtr.size());
            RefinedPtr.resize(j);
        }
        visited.reset();
    }
}

State LateLowerGCFrame::LocalScan(Function &F) {
    State S(F);
    SmallVector<int, 8> PHINumbers;
    for (BasicBlock &BB : F) {
        BBState &BBS = S.BBStates[&BB];
        for (auto it = BB.rbegin(); it != BB.rend(); ++it) {
            Instruction &I = *it;
            if (CallInst *CI = dyn_cast<CallInst>(&I)) {
                if (isa<IntrinsicInst>(CI)) {
                    // Most intrinsics are not gc uses/defs, however some have
                    // memory operands and could thus be GC uses. To be conservative,
                    // we only skip processing for those that we know we emit often
                    // and cannot possibly be GC uses.
                    IntrinsicInst *II = cast<IntrinsicInst>(CI);
                    if (isa<DbgInfoIntrinsic>(CI) ||
                        II->getIntrinsicID() == Intrinsic::lifetime_start ||
                        II->getIntrinsicID() == Intrinsic::lifetime_end) {
                        continue;
                    }
                    if (II->getIntrinsicID() == Intrinsic::masked_load ||
                        II->getIntrinsicID() == Intrinsic::masked_gather) {
                        if (auto VTy = dyn_cast<VectorType>(II->getType())) {
                            if (auto PtrT = dyn_cast<PointerType>(VTy->getElementType())) {
                                if (isSpecialPtr(PtrT)) {
                                    // LLVM sometimes tries to materialize these operations with undefined pointers in our non-integral address space.
                                    // Hopefully LLVM didn't already propagate that information and poison our users. Set those to NULL now.
                                    Value *passthru = II->getArgOperand(3);
                                    if (isa<UndefValue>(passthru)) {
                                        II->setArgOperand(3, Constant::getNullValue(passthru->getType()));
                                    }
                                    if (PtrT->getAddressSpace() == AddressSpace::Loaded) {
                                        // These are not real defs
                                        continue;
                                    }
                                }
                            }
                        }
                    }
                }
                auto callee = CI->getCalledFunction();
                if (callee && callee == typeof_func) {
                    MaybeNoteDef(S, BBS, CI, BBS.Safepoints, SmallVector<int, 1>{-2});
                }
                else {
                    MaybeNoteDef(S, BBS, CI, BBS.Safepoints);
                }
                if (CI->hasStructRetAttr()) {
                    Type *ElT = (CI->arg_begin()[0])->getType()->getPointerElementType();
                    auto tracked = CountTrackedPointers(ElT);
                    if (tracked.count) {
                        AllocaInst *SRet = dyn_cast<AllocaInst>((CI->arg_begin()[0])->stripInBoundsOffsets());
                        assert(SRet);
                        {
                            if (!(SRet->isStaticAlloca() && isa<PointerType>(ElT) && ElT->getPointerAddressSpace() == AddressSpace::Tracked)) {
                                assert(!tracked.derived);
                                if (tracked.all) {
                                    S.ArrayAllocas[SRet] = tracked.count * cast<ConstantInt>(SRet->getArraySize())->getZExtValue();
                                }
                                else {
                                    AllocaInst *SRet_gc = dyn_cast<AllocaInst>((CI->arg_begin()[1])->stripInBoundsOffsets());
                                    Type *ElT = SRet_gc->getAllocatedType();
                                    if (!(SRet_gc->isStaticAlloca() && isa<PointerType>(ElT) && ElT->getPointerAddressSpace() == AddressSpace::Tracked)) {
                                        S.ArrayAllocas[SRet_gc] = tracked.count * cast<ConstantInt>(SRet_gc->getArraySize())->getZExtValue();
                                    }
                                }
                            }
                        }
                    }
                }
                NoteOperandUses(S, BBS, I);
                if (CI->canReturnTwice()) {
                    S.ReturnsTwice.push_back(CI);
                }
                if (callee) {
                    if (callee == gc_preserve_begin_func) {
                        std::vector<int> args;
                        for (Use &U : CI->arg_operands()) {
                            Value *V = U;
                            if (isa<Constant>(V))
                                continue;
                            if (isa<PointerType>(V->getType())) {
                                if (isSpecialPtr(V->getType())) {
                                    int Num = Number(S, V);
                                    if (Num >= 0)
                                        args.push_back(Num);
                                }
                            } else {
                                std::vector<int> Nums = NumberAll(S, V);
                                for (int Num : Nums) {
                                    if (Num < 0)
                                        continue;
                                    if (Num >= 0)
                                        args.push_back(Num);
                                }
                            }
                        }
                        S.GCPreserves[CI] = args;
                        continue;
                    }
                    // Known functions emitted in codegen that are not safepoints
                    if (callee == pointer_from_objref_func || callee == gc_preserve_begin_func ||
                        callee == gc_preserve_end_func || callee == typeof_func ||
                        callee == ptls_getter ||
                        callee == write_barrier_func || callee->getName() == "memcmp") {
                        continue;
                    }
                    if (callee->hasFnAttribute(Attribute::ReadNone) ||
                        callee->hasFnAttribute(Attribute::ReadOnly) ||
                        callee->hasFnAttribute(Attribute::ArgMemOnly)) {
                        continue;
                    }
                    if (MemTransferInst *MI = dyn_cast<MemTransferInst>(CI)) {
                        MaybeTrackDst(S, MI);
                    }
                }
                if (isa<IntrinsicInst>(CI) || CI->hasFnAttr(Attribute::ArgMemOnly) ||
                    CI->hasFnAttr(Attribute::ReadNone) || CI->hasFnAttr(Attribute::ReadOnly)) {
                    // Intrinsics are never safepoints.
                    continue;
                }
                int SafepointNumber = NoteSafepoint(S, BBS, CI);
                BBS.HasSafepoint = true;
                BBS.TopmostSafepoint = SafepointNumber;
                BBS.Safepoints.push_back(SafepointNumber);
            } else if (LoadInst *LI = dyn_cast<LoadInst>(&I)) {
                // If this is a load from an immutable, we know that
                // this object will always be rooted as long as the
                // object we're loading from is, so we can refine uses
                // of this object to uses of the object we're loading
                // from.
                SmallVector<int, 1> RefinedPtr{};
                Type *Ty = LI->getType()->getScalarType();
                bool task_local = false;
                if (isLoadFromImmut(LI) && isSpecialPtr(LI->getPointerOperand()->getType())) {
                    RefinedPtr.push_back(Number(S, LI->getPointerOperand()));
                } else if (LI->getType()->isPointerTy() &&
                        isSpecialPtr(Ty) &&
                        LooksLikeFrameRef(LI->getPointerOperand())) {
                    // Loads from a jlcall argument array
                    RefinedPtr.push_back(-1);
                }
                else if (isLoadFromConstGV(LI, task_local)) {
                    // If this is a const load from a global,
                    // we know that the object is a constant as well and doesn't need rooting.
                    // If this is a task local constant, we don't need to root it within the
                    // task but we do need to issue write barriers for when the current task dies.
                    RefinedPtr.push_back(task_local ? -1 : -2);
                }
                if (!Ty->isPointerTy() || Ty->getPointerAddressSpace() != AddressSpace::Loaded) {
                    MaybeNoteDef(S, BBS, LI, BBS.Safepoints, std::move(RefinedPtr));
                }
                NoteOperandUses(S, BBS, I);
            } else if (SelectInst *SI = dyn_cast<SelectInst>(&I)) {
                auto tracked = CountTrackedPointers(SI->getType());
                if (tracked.count && !tracked.derived) {
                    // record the select definition of these values
                    SmallVector<int, 2> RefinedPtr;
                    if (isa<PointerType>(SI->getType())) {
                        // TODO: Refinements for vector select
                        RefinedPtr = {
                            Number(S, SI->getTrueValue()),
                            Number(S, SI->getFalseValue())
                        };
                    }
                    MaybeNoteDef(S, BBS, SI, BBS.Safepoints, std::move(RefinedPtr));
                    NoteOperandUses(S, BBS, I);
                } else if (tracked.count) {
                    // We need to insert extra selects for the GC roots
                    LiftSelect(S, SI);
                }
            } else if (PHINode *Phi = dyn_cast<PHINode>(&I)) {
                auto tracked = CountTrackedPointers(Phi->getType());
                if (tracked.count && !tracked.derived) {
                    // record the phi definition of these values
                    SmallVector<int, 1> PHIRefinements;
                    if (isa<PointerType>(Phi->getType()))
                        // TODO: Vector refinements
                        PHIRefinements = GetPHIRefinements(Phi, S);
                    MaybeNoteDef(S, BBS, Phi, BBS.Safepoints, std::move(PHIRefinements));
                    if (isa<PointerType>(Phi->getType())) {
                        PHINumbers.push_back(Number(S, Phi));
                    } else {
                        std::vector<int> Nums = NumberAll(S, Phi);
                        for (int Num : Nums)
                            PHINumbers.push_back(Num);
                    }
                    unsigned nIncoming = Phi->getNumIncomingValues();
                    for (unsigned i = 0; i < nIncoming; ++i) {
                        BBState &IncomingBBS = S.BBStates[Phi->getIncomingBlock(i)];
                        NoteUse(S, IncomingBBS, Phi->getIncomingValue(i), IncomingBBS.PhiOuts);
                    }
                } else if (tracked.count) {
                    // We need to insert extra phis for the GC roots
                    LiftPhi(S, Phi);
                }
            } else if (StoreInst *SI = dyn_cast<StoreInst>(&I)) {
                NoteOperandUses(S, BBS, I);
                MaybeTrackStore(S, SI);
            } else if (isa<ReturnInst>(&I)) {
                NoteOperandUses(S, BBS, I);
            } else if (auto *ASCI = dyn_cast<AddrSpaceCastInst>(&I)) {
                if (isTrackedValue(ASCI)) {
                    SmallVector<int, 1> RefinedPtr{};
                    bool task_local = false;
                    auto origin = ASCI->getPointerOperand()->stripPointerCasts();
                    if (auto LI = dyn_cast<LoadInst>(origin)) {
                        if (isLoadFromConstGV(LI, task_local)) {
                            RefinedPtr.push_back(task_local ? -1 : -2);
                        }
                    }
                    MaybeNoteDef(S, BBS, ASCI, BBS.Safepoints, std::move(RefinedPtr));
                }
            } else if (auto *AI = dyn_cast<AllocaInst>(&I)) {
                Type *ElT = AI->getAllocatedType();
                if (AI->isStaticAlloca() && isa<PointerType>(ElT) && ElT->getPointerAddressSpace() == AddressSpace::Tracked) {
                    S.Allocas.push_back(AI);
                }
            }
        }
        // Pre-seed the dataflow variables;
        BBS.LiveIn = BBS.UpExposedUses;
        BBS.Done = true;
    }
    FixUpRefinements(PHINumbers, S);
    return S;
}

#if JL_LLVM_VERSION >= 110000
static Value *ExtractScalar(Value *V, Type *VTy, bool isptr, ArrayRef<unsigned> Idxs, IRBuilder<> &irbuilder) {
#else
static Value *ExtractScalar(Value *V, Type *VTy, bool isptr, ArrayRef<unsigned> Idxs, IRBuilder<> irbuilder) {
#endif
    Type *T_int32 = Type::getInt32Ty(V->getContext());
    if (isptr) {
        std::vector<Value*> IdxList{Idxs.size() + 1};
        IdxList[0] = ConstantInt::get(T_int32, 0);
        for (unsigned j = 0; j < Idxs.size(); ++j) {
            IdxList[j + 1] = ConstantInt::get(T_int32, Idxs[j]);
        }
        Value *GEP = irbuilder.CreateInBoundsGEP(VTy, V, IdxList);
        Type *T = GetElementPtrInst::getIndexedType(VTy, IdxList);
        assert(T->isPointerTy());
        V = irbuilder.CreateAlignedLoad(T, GEP, Align(sizeof(void*)));
        // since we're doing stack operations, it should be safe do this non-atomically
        cast<LoadInst>(V)->setOrdering(AtomicOrdering::NotAtomic);
    }
    else if (isa<PointerType>(V->getType())) {
        assert(Idxs.empty());
    }
    else if (!Idxs.empty()) {
        auto IdxsNotVec = Idxs.slice(0, Idxs.size() - 1);
        Type *FinalT = ExtractValueInst::getIndexedType(V->getType(), IdxsNotVec);
        bool IsVector = isa<VectorType>(FinalT);
        if (Idxs.size() > IsVector)
            V = irbuilder.Insert(ExtractValueInst::Create(V, IsVector ? IdxsNotVec : Idxs));
        if (IsVector)
            V = irbuilder.Insert(ExtractElementInst::Create(V,
                    ConstantInt::get(Type::getInt32Ty(V->getContext()), Idxs.back())));
    }
    return V;
}

static unsigned getFieldOffset(const DataLayout &DL, Type *STy, ArrayRef<unsigned> Idxs)
{
    SmallVector<Value*,4> IdxList{Idxs.size() + 1};
    Type *T_int32 = Type::getInt32Ty(STy->getContext());
    IdxList[0] = ConstantInt::get(T_int32, 0);
    for (unsigned j = 0; j < Idxs.size(); ++j)
        IdxList[j + 1] = ConstantInt::get(T_int32, Idxs[j]);
    auto offset = DL.getIndexedOffsetInType(STy, IdxList);
    assert(offset >= 0);
    return (unsigned)offset;
}

#if JL_LLVM_VERSION >= 110000
std::vector<Value*> ExtractTrackedValues(Value *Src, Type *STy, bool isptr, IRBuilder<> &irbuilder, ArrayRef<unsigned> perm_offsets) {
#else
std::vector<Value*> ExtractTrackedValues(Value *Src, Type *STy, bool isptr, IRBuilder<> irbuilder, ArrayRef<unsigned> perm_offsets) {
#endif
    auto Tracked = TrackCompositeType(STy);
    std::vector<Value*> Ptrs;
    unsigned perm_idx = 0;
    auto ignore_field = [&] (ArrayRef<unsigned> Idxs) {
        if (perm_idx >= perm_offsets.size())
            return false;
        // Assume the indices returned from `TrackCompositeType` is ordered and do a
        // single pass over `perm_offsets`.
        assert(!isptr);
        auto offset = getFieldOffset(irbuilder.GetInsertBlock()->getModule()->getDataLayout(),
                                     STy, Idxs);
        do {
            auto perm_offset = perm_offsets[perm_idx];
            if (perm_offset > offset)
                return false;
            perm_idx++;
            if (perm_offset == offset) {
                return true;
            }
        } while (perm_idx < perm_offsets.size());
        return false;
    };
    for (unsigned i = 0; i < Tracked.size(); ++i) {
        auto Idxs = makeArrayRef(Tracked[i]);
        if (ignore_field(Idxs))
            continue;
        Value *Elem = ExtractScalar(Src, STy, isptr, Idxs, irbuilder);
        Ptrs.push_back(Elem);
    }
    return Ptrs;
}

#if JL_LLVM_VERSION >= 110000
unsigned TrackWithShadow(Value *Src, Type *STy, bool isptr, Value *Dst, IRBuilder<> &irbuilder) {
#else
unsigned TrackWithShadow(Value *Src, Type *STy, bool isptr, Value *Dst, IRBuilder<> irbuilder) {
#endif
    auto Ptrs = ExtractTrackedValues(Src, STy, isptr, irbuilder);
    for (unsigned i = 0; i < Ptrs.size(); ++i) {
        Value *Elem = Ptrs[i];
        assert(Elem->getType()->isPointerTy());
        Value *Slot = irbuilder.CreateConstInBoundsGEP1_32(Elem->getType(), Dst, i);
        StoreInst *shadowStore = irbuilder.CreateAlignedStore(Elem, Slot, Align(sizeof(void*)));
        shadowStore->setOrdering(AtomicOrdering::NotAtomic);
        // TODO: shadowStore->setMetadata(LLVMContext::MD_tbaa, tbaa_gcframe);
    }
    return Ptrs.size();
}


// turn a memcpy into a set of loads
void LateLowerGCFrame::MaybeTrackDst(State &S, MemTransferInst *MI) {
    //Value *Dst = MI->getRawDest()->stripInBoundsOffsets();
    //if (AllocaInst *AI = dyn_cast<AllocaInst>(Dst)) {
    //    Type *STy = AI->getAllocatedType();
    //    if (!AI->isStaticAlloca() || (isa<PointerType>(STy) && STy->getPointerAddressSpace() == AddressSpace::Tracked) || S.ArrayAllocas.count(AI))
    //        return; // already numbered this
    //    auto tracked = CountTrackedPointers(STy);
    //    unsigned nroots = tracked.count * cast<ConstantInt>(AI->getArraySize())->getZExtValue();
    //    if (nroots) {
    //        assert(!tracked.derived);
    //        if (!tracked.all) {
    //            // materialize shadow LoadInst and StoreInst ops to make a copy of just the tracked values inside
    //            //assert(MI->getLength() == DL.getTypeAllocSize(AI->getAllocatedType()) && !AI->isArrayAllocation()); // XXX: handle partial copy
    //            Value *Src = MI->getSource();
    //            Src = new BitCastInst(Src, STy->getPointerTo(MI->getSourceAddressSpace()), "", MI);
    //            auto &Shadow = S.ShadowAllocas[AI];
    //            if (!Shadow)
    //                Shadow = new AllocaInst(T_prjlvalue, 0, ConstantInt::get(T_int32, nroots), "", MI);
    //            AI = Shadow;
    //            unsigned count = TrackWithShadow(Src, STy, true, AI, IRBuilder<>(MI));
    //            assert(count == tracked.count); (void)count;
    //        }
    //        S.ArrayAllocas[AI] = nroots;
    //    }
    //}
    //// TODO: else???
}

void LateLowerGCFrame::MaybeTrackStore(State &S, StoreInst *I) {
    Value *PtrBase = I->getPointerOperand()->stripInBoundsOffsets();
    auto tracked = CountTrackedPointers(I->getValueOperand()->getType());
    if (!tracked.count)
        return; // nothing to track is being stored
    if (AllocaInst *AI = dyn_cast<AllocaInst>(PtrBase)) {
        Type *STy = AI->getAllocatedType();
        if (!AI->isStaticAlloca() || (isa<PointerType>(STy) && STy->getPointerAddressSpace() == AddressSpace::Tracked) || S.ArrayAllocas.count(AI))
            return; // already numbered this
        auto tracked = CountTrackedPointers(STy);
        if (tracked.count) {
            assert(!tracked.derived);
            if (tracked.all) {
                // track the Alloca directly
                S.ArrayAllocas[AI] = tracked.count * cast<ConstantInt>(AI->getArraySize())->getZExtValue();
                return;
            }
        }
    }
    else {
        return; // assume it is rooted--TODO: should we be more conservative?
    }
    // track the Store with a Shadow
    //auto &Shadow = S.ShadowAllocas[AI];
    //if (!Shadow)
    //    Shadow = new AllocaInst(T_prjlvalue, 0, ConstantInt::get(T_int32, tracked.count), "", MI);
    //AI = Shadow;
    //Value *Src = I->getValueOperand();
    //unsigned count = TrackWithShadow(Src, Src->getType(), false, AI, MI, TODO which slots are we actually clobbering?);
    //assert(count == tracked.count); (void)count;
    S.TrackedStores.push_back(std::make_pair(I, tracked.count));
}

/*
 * DataFlow equations:
 * LiveIn[BB] = UpExposedUses[BB] ∪ (LiveOut[BB] - Defs[BB])
 * LiveOut[BB] =  PhiUses[BB] ∪ ∪_{Succ} LiveIn[Succ]
 *
 * We'll perform textbook iterative dataflow to compute this. There are better
 * algorithms. If this starts becoming a problem, we should use one of them.
 */
void LateLowerGCFrame::ComputeLiveness(State &S) {
    bool Converged = false;
    /* Liveness is a reverse problem, so RPOT is a good way to
     * perform this iteration.
     */
    ReversePostOrderTraversal<Function *> RPOT(S.F);
    while (!Converged) {
        bool AnyChanged = false;
        for (BasicBlock *BB : RPOT) {
            // This could all be done more efficiently, by only updating what
            // changed - Let's get it working first though.
            BBState &BBS = S.BBStates[BB];
            BitVector NewLiveOut = BBS.PhiOuts;
            for (BasicBlock *Succ : successors(BB)) {
                NewLiveOut |= S.BBStates[Succ].LiveIn;
            }
            if (NewLiveOut != BBS.LiveOut) {
                AnyChanged = true;
                BBS.LiveOut = NewLiveOut;
                MaybeResize(BBS, BBS.LiveOut.size() - 1);
            }
            BitVector NewLiveIn = BBS.LiveOut;
            BitVector FlippedDefs = BBS.Defs;
            FlippedDefs.flip();
            NewLiveIn &= FlippedDefs;
            NewLiveIn |= BBS.UpExposedUses;
            if (NewLiveIn != BBS.LiveIn) {
                AnyChanged = true;
                BBS.LiveIn = NewLiveIn;
            }
        }
        Converged = !AnyChanged;
    }
    ComputeLiveSets(S);
}

// For debugging
JL_USED_FUNC static void dumpSafepointsForBBName(Function &F, State &S, const char *BBName) {
    for (auto it : S.SafepointNumbering) {
        if (it.first->getParent()->getName() == BBName) {
            dbgs() << "Live at " << *it.first << "\n";
            BitVector &LS = S.LiveSets[it.second];
            for (int Idx = LS.find_first(); Idx >= 0; Idx = LS.find_next(Idx)) {
                dbgs() << "\t";
                S.ReversePtrNumbering[Idx]->printAsOperand(dbgs());
                dbgs() << "\n";
            }
        }
    }
}

void LateLowerGCFrame::RefineLiveSet(BitVector &LS, State &S)
{
    BitVector FullLS(S.MaxPtrNumber + 1, false);
    FullLS |= LS;
    // First expand the live set according to the refinement map
    // so that we can see all the values that are effectively live.
    bool changed;
    do {
        changed = false;
        for (auto &kv: S.Refinements) {
            int Num = kv.first;
            if (Num < 0 || HasBitSet(FullLS, Num) || kv.second.empty())
                continue;
            bool live = true;
            for (auto &refine: kv.second) {
                if (refine < 0 || HasBitSet(FullLS, refine))
                    continue;
                live = false;
                break;
            }
            if (live) {
                changed = true;
                FullLS[Num] = 1;
            }
        }
    } while (changed);
    // Now remove all values from the LiveSet that's kept alive by other objects
    do {
        changed = false;
        for (int Idx = LS.find_first(); Idx >= 0; Idx = LS.find_next(Idx)) {
            if (!S.Refinements.count(Idx))
                continue;
            auto &RefinedPtr = S.Refinements[Idx];
            if (RefinedPtr.empty())
                continue;
            bool rooted = true;
            for (auto RefPtr: RefinedPtr) {
                if (RefPtr < 0 || HasBitSet(FullLS, RefPtr))
                    continue;
                rooted = false;
                break;
            }
            if (rooted) {
                changed = true;
                LS[Idx] = 0;
            }
        }
    } while (changed);
}

void LateLowerGCFrame::ComputeLiveSets(State &S) {
    // Iterate over all safe points. Add to live sets all those variables that
    // are now live across their parent block.
    for (auto it : S.SafepointNumbering) {
        int idx = it.second;
        Instruction *Safepoint = it.first;
        BasicBlock *BB = Safepoint->getParent();
        BBState &BBS = S.BBStates[BB];
        BitVector LiveAcross = BBS.LiveIn;
        LiveAcross &= BBS.LiveOut;
        BitVector &LS = S.LiveSets[idx];
        LS |= LiveAcross;
        for (int Live : S.LiveIfLiveOut[idx]) {
            if (HasBitSet(BBS.LiveOut, Live))
                LS[Live] = 1;
        }
        RefineLiveSet(LS, S);
        // If the function has GC preserves, figure out whether we need to
        // add in any extra live values.
        if (!S.GCPreserves.empty()) {
            if (!S.DT) {
                S.DT = &getAnalysis<DominatorTreeWrapperPass>().getDomTree();
            }
            for (auto it2 : S.GCPreserves) {
                if (!S.DT->dominates(it2.first, Safepoint))
                    continue;
                bool OutsideRange = false;
                for (const User *U : it2.first->users()) {
                    // If this is dominated by an end, we don't need to add
                    // the values to our live set.
                    if (S.DT->dominates(cast<Instruction>(U), Safepoint)) {
                        OutsideRange = true;
                        break;
                    }
                }
                if (OutsideRange)
                    continue;
                for (unsigned Num : it2.second) {
                    if (Num >= LS.size())
                        LS.resize(Num + 1);
                    LS[Num] = 1;
                }
            }
        }
    }
    // Compute the interference graph
    for (int i = 0; i <= S.MaxPtrNumber; ++i) {
        SetVector<int> Neighbors;
        BitVector NeighborBits(S.MaxPtrNumber);
        for (auto it : S.SafepointNumbering) {
            const BitVector &LS = S.LiveSets[it.second];
            if ((unsigned)i >= LS.size() || !LS[i])
                continue;
            NeighborBits |= LS;
        }
        for (int Idx = NeighborBits.find_first(); Idx >= 0; Idx = NeighborBits.find_next(Idx)) {
            // We explicitly let i be a neighbor of itself, to distinguish
            // between being the only value live at a safepoint, vs not
            // being live at any safepoint.
            Neighbors.insert(Idx);
        }
        S.Neighbors.push_back(Neighbors);
    }
}

/* For chordal interference graphs, this class gives the vertices in a (reverse
 * - depending on definition) perfect elimination ordering, in such a way that
 * greedy coloring gives an optimal coloring. Since our roots are in SSA form,
 * the interference should be chordal.
 */
struct PEOIterator {
    struct Element {
        unsigned weight;
        unsigned pos;
    };
    std::vector<Element> Elements;
    std::vector<std::vector<int>> Levels;
    const std::vector<SetVector<int>> &Neighbors;
    PEOIterator(const std::vector<SetVector<int>> &Neighbors) : Neighbors(Neighbors) {
        // Initialize State
        std::vector<int> FirstLevel;
        for (unsigned i = 0; i < Neighbors.size(); ++i) {
            FirstLevel.push_back(i);
            Element E{0, i};
            Elements.push_back(E);
        }
        Levels.push_back(FirstLevel);
    }
    int next() {
        // Find the element in the highest bucket
        int NextElement = -1;
        while (NextElement == -1 && !Levels.empty()) {
            std::vector<int> &LastLevel = Levels.back();
            while (NextElement == -1 && !LastLevel.empty()) {
                NextElement = LastLevel.back();
                LastLevel.pop_back();
            }
            if (LastLevel.empty())
                Levels.pop_back();
        }
        if (NextElement == -1)
            return NextElement;
        // Make sure not to try to re-use this later.
        Elements[NextElement].weight = (unsigned)-1;
        // Raise neighbors
        for (int Neighbor : Neighbors[NextElement]) {
            if (Neighbor == NextElement)
                continue;
            Element &NElement = Elements[Neighbor];
            // Already processed. Don't re-enqueue
            if (NElement.weight == (unsigned)-1)
                continue;
            // Kill old queue position
            Levels[NElement.weight][NElement.pos] = -1;
            // Raise the neighbor to the next level.
            NElement.weight += 1;
            if (NElement.weight >= Levels.size())
                Levels.push_back(std::vector<int>{});
            Levels[NElement.weight].push_back(Neighbor);
            NElement.pos = Levels[NElement.weight].size()-1;
        }
        // As an enhancement, we might want to periodically compactify the whole
        // data structure. This could be done here.
        return NextElement;
    }
};

JL_USED_FUNC static void dumpColorAssignments(const State &S, std::vector<int> &Colors)
{
    for (unsigned i = 0; i < Colors.size(); ++i) {
        if (Colors[i] == -1)
            continue;
        dbgs() << "\tValue ";
        S.ReversePtrNumbering.at(i)->printAsOperand(dbgs());
        dbgs() << " assigned color " << Colors[i] << "\n";
    }
}

std::vector<int> LateLowerGCFrame::ColorRoots(const State &S) {
    std::vector<int> Colors;
    Colors.resize(S.MaxPtrNumber + 1, -1);
    PEOIterator Ordering(S.Neighbors);
    int PreAssignedColors = 0;
    /* First assign permanent slots to things that need them due
       to returns_twice */
    for (auto it : S.ReturnsTwice) {
        int Num = S.SafepointNumbering.at(it);
        const BitVector &LS = S.LiveSets[Num];
        for (int Idx = LS.find_first(); Idx >= 0; Idx = LS.find_next(Idx)) {
            if (Colors[Idx] == -1)
                Colors[Idx] = PreAssignedColors++;
        }
    }
    /* Greedy coloring */
    int MaxAssignedColor = -1;
    int ActiveElement = 1;
    BitVector UsedColors;
    while ((ActiveElement = Ordering.next()) != -1) {
        if (Colors[ActiveElement] != -1)
            continue;
        UsedColors.resize(MaxAssignedColor + 2, false);
        UsedColors.reset();
        if (S.Neighbors[ActiveElement].empty()) {
            // No need to color a value not live at any safe point
            continue;
        }
        for (int Neighbor : S.Neighbors[ActiveElement]) {
            int NeighborColor = Colors[Neighbor];
            if (NeighborColor == -1)
                continue;
            if (NeighborColor < PreAssignedColors)
                continue;
            UsedColors[NeighborColor - PreAssignedColors] = 1;
        }
        int NewColor = UsedColors.flip().find_first();
        if (NewColor > MaxAssignedColor)
            MaxAssignedColor = NewColor;
        NewColor += PreAssignedColors;
        Colors[ActiveElement] = NewColor;
    }
    return Colors;
}

// Size of T is assumed to be `sizeof(void*)`
Value *LateLowerGCFrame::EmitTagPtr(IRBuilder<> &builder, Type *T, Value *V)
{
    assert(T == T_size || isa<PointerType>(T));
    auto TV = cast<PointerType>(V->getType());
    auto cast = builder.CreateBitCast(V, T->getPointerTo(TV->getAddressSpace()));
    return builder.CreateInBoundsGEP(T, cast, ConstantInt::get(T_size, -1));
}

Value *LateLowerGCFrame::EmitLoadTag(IRBuilder<> &builder, Value *V)
{
    auto addr = EmitTagPtr(builder, T_size, V);
    LoadInst *load = builder.CreateAlignedLoad(T_size, addr, Align(sizeof(size_t)));
    load->setOrdering(AtomicOrdering::Unordered);
    load->setMetadata(LLVMContext::MD_tbaa, tbaa_tag);
    MDBuilder MDB(load->getContext());
    auto *NullInt = ConstantInt::get(T_size, 0);
    // We can be sure that the tag is larger than page size.
    // Hopefully this is enough to convince LLVM that the value is still not NULL
    // after masking off the tag bits
    auto *NonNullInt = ConstantExpr::getAdd(NullInt, ConstantInt::get(T_size, 4096));
    load->setMetadata(LLVMContext::MD_range, MDB.createRange(NonNullInt, NullInt));
    return load;
}

// Enable this optimization only on LLVM 4.0+ since this cause LLVM to optimize
// constant store loop to produce a `memset_pattern16` with a global variable
// that's initialized by `addrspacecast`. Such a global variable is not supported by the backend.
// This is not a problem on 4.0+ since that transformation (in loop-idiom) is disabled
// for NI pointers.
static SmallVector<int, 1> *FindRefinements(Value *V, State *S)
{
    if (!S)
        return nullptr;
    auto it = S->AllPtrNumbering.find(V);
    if (it == S->AllPtrNumbering.end())
        return nullptr;
    auto rit = S->Refinements.find(it->second);
    return rit != S->Refinements.end() && !rit->second.empty() ? &rit->second : nullptr;
}

static bool IsPermRooted(Value *V, State *S)
{
    if (isa<Constant>(V))
        return true;
    if (auto *RefinePtr = FindRefinements(V, S))
        return RefinePtr->size() == 1 && (*RefinePtr)[0] == -2;
    return false;
}

static inline void UpdatePtrNumbering(Value *From, Value *To, State *S)
{
    if (!S)
        return;
    auto it = S->AllPtrNumbering.find(From);
    if (it == S->AllPtrNumbering.end())
        return;
    auto Num = it->second;
    S->AllPtrNumbering.erase(it);
    if (To) {
        S->AllPtrNumbering[To] = Num;
    }
}

MDNode *createMutableTBAAAccessTag(MDNode *Tag) {
    return MDBuilder(Tag->getContext()).createMutableTBAAAccessTag(Tag);
}


bool LateLowerGCFrame::CleanupIR(Function &F, State *S) {
    bool ChangesMade = false;
    // We create one alloca for all the jlcall frames that haven't been processed
    // yet. LLVM would merge them anyway later, so might as well save it a bit
    // of work
    size_t maxframeargs = 0;
    Instruction *StartOff = &*(F.getEntryBlock().begin());
    PointerType *T_pprjlvalue = nullptr;
    AllocaInst *Frame = nullptr;
    if (T_prjlvalue) {
        T_pprjlvalue = T_prjlvalue->getPointerTo();
        Frame = new AllocaInst(T_prjlvalue, 0,
            ConstantInt::get(T_int32, maxframeargs), "", StartOff);
    }
    std::vector<CallInst*> write_barriers;
    for (BasicBlock &BB : F) {
        for (auto it = BB.begin(); it != BB.end();) {
            Instruction *I = &*it;
            // strip all constant alias information, as it might depend on the gc having
            // preserved a gc root, which stops being true after this pass (#32215)
            // similar to RewriteStatepointsForGC::stripNonValidData, but less aggressive
            if (I->getMetadata(LLVMContext::MD_invariant_load))
                I->setMetadata(LLVMContext::MD_invariant_load, NULL);
            if (MDNode *TBAA = I->getMetadata(LLVMContext::MD_tbaa)) {
                if (TBAA->getNumOperands() == 4 && isTBAA(TBAA, {"jtbaa_const"})) {
                    MDNode *MutableTBAA = createMutableTBAAAccessTag(TBAA);
                    if (MutableTBAA != TBAA)
                        I->setMetadata(LLVMContext::MD_tbaa, MutableTBAA);
                }
            }
            auto *CI = dyn_cast<CallInst>(&*it);
            if (!CI) {
                ++it;
                continue;
            }
            CallingConv::ID CC = CI->getCallingConv();
            Value *callee = CI->getCalledOperand();
            if (callee && (callee == gc_flush_func || callee == gc_preserve_begin_func
                        || callee == gc_preserve_end_func)) {
                /* No replacement */
            } else if (pointer_from_objref_func != nullptr && callee == pointer_from_objref_func) {
                auto *obj = CI->getOperand(0);
                auto *ASCI = new AddrSpaceCastInst(obj, T_pjlvalue, "", CI);
                ASCI->takeName(CI);
                CI->replaceAllUsesWith(ASCI);
                UpdatePtrNumbering(CI, ASCI, S);
            } else if (alloc_obj_func && callee == alloc_obj_func) {
                assert(CI->getNumArgOperands() == 3);

                // Initialize an IR builder.
                IRBuilder<> builder(CI);
                builder.SetCurrentDebugLocation(CI->getDebugLoc());

                // Create a call to the `julia.gc_alloc_bytes` intrinsic, which is like
                // `julia.gc_alloc_obj` except it doesn't set the tag.
                auto allocBytesIntrinsic = getOrDeclare(jl_intrinsics::GCAllocBytes);
                auto newI = builder.CreateCall(
                    allocBytesIntrinsic,
                    {
                        CI->getArgOperand(0),
                        builder.CreateIntCast(
                            CI->getArgOperand(1),
                            allocBytesIntrinsic->getFunctionType()->getParamType(1),
                            false)
                    });
                newI->takeName(CI);

                // LLVM alignment/bit check is not happy about addrspacecast and refuse
                // to remove write barrier because of it.
                // We pretty much only load using `T_size` so try our best to strip
                // as many cast as possible.
#if JL_LLVM_VERSION >= 100000
                auto tag = CI->getArgOperand(2)->stripPointerCastsAndAliases();
#else
                auto tag = CI->getArgOperand(2)->stripPointerCasts();
#endif
                if (auto C = dyn_cast<ConstantExpr>(tag)) {
                    if (C->getOpcode() == Instruction::IntToPtr) {
                        tag = C->getOperand(0);
                    }
                }
                else if (auto LI = dyn_cast<LoadInst>(tag)) {
                    // Make sure the load is correctly marked as aligned
                    // since LLVM might have removed them.
                    // We can't do this in general since the load might not be
                    // a type in other branches.
                    // However, it should be safe for us to do this on const globals
                    // which should be the important cases as well.
                    bool task_local = false;
                    if (isLoadFromConstGV(LI, task_local) && getLoadValueAlign(LI) < 16) {
                        Type *T_int64 = Type::getInt64Ty(LI->getContext());
                        auto op = ConstantAsMetadata::get(ConstantInt::get(T_int64, 16));
                        LI->setMetadata(LLVMContext::MD_align,
                                        MDNode::get(LI->getContext(), { op }));
                    }
                }
                // As a last resort, if we didn't manage to strip down the tag
                // for LLVM, emit an alignment assumption.
                auto tag_type = tag->getType();
                if (tag_type->isPointerTy()) {
                    auto &DL = CI->getModule()->getDataLayout();
#if JL_LLVM_VERSION >= 110000
                    auto align = tag->getPointerAlignment(DL).value();
#elif JL_LLVM_VERSION >= 100000
                    auto align = tag->getPointerAlignment(DL).valueOrOne().value();
#else
                    auto align = tag->getPointerAlignment(DL);
#endif
                    if (align < 16) {
                        // On 5 <= LLVM < 12, it is illegal to call this on
                        // non-integral pointer. This relies on stripping the
                        // non-integralness from datalayout before this pass
                        builder.CreateAlignmentAssumption(DL, tag, 16);
                    }
                }
                // Set the tag.
                StoreInst *store = builder.CreateAlignedStore(
                    tag, EmitTagPtr(builder, tag_type, newI), Align(sizeof(size_t)));
                store->setOrdering(AtomicOrdering::Unordered);
                store->setMetadata(LLVMContext::MD_tbaa, tbaa_tag);

                // Replace uses of the call to `julia.gc_alloc_obj` with the call to
                // `julia.gc_alloc_bytes`.
                CI->replaceAllUsesWith(newI);

                // Update the pointer numbering.
                UpdatePtrNumbering(CI, newI, S);
            } else if (typeof_func && callee == typeof_func) {
                assert(CI->getNumArgOperands() == 1);
                IRBuilder<> builder(CI);
                builder.SetCurrentDebugLocation(CI->getDebugLoc());
                auto tag = EmitLoadTag(builder, CI->getArgOperand(0));
                auto masked = builder.CreateAnd(tag, ConstantInt::get(T_size, ~(uintptr_t)15));
                auto typ = builder.CreateAddrSpaceCast(builder.CreateIntToPtr(masked, T_pjlvalue),
                                                       T_prjlvalue);
                typ->takeName(CI);
                CI->replaceAllUsesWith(typ);
                UpdatePtrNumbering(CI, typ, S);
            } else if (write_barrier_func && callee == write_barrier_func) {
                // The replacement for this requires creating new BasicBlocks
                // which messes up the loop. Queue all of them to be replaced later.
                assert(CI->getNumArgOperands() >= 1);
                write_barriers.push_back(CI);
                ChangesMade = true;
                ++it;
                continue;
            } else if (CC == JLCALL_F_CC ||
                       CC == JLCALL_F2_CC) {
                assert(T_prjlvalue);
                size_t nargs = CI->getNumArgOperands();
                size_t nframeargs = nargs;
                if (CC == JLCALL_F_CC)
                    nframeargs -= 1;
                else if (CC == JLCALL_F2_CC)
                    nframeargs -= 2;
                SmallVector<Value*, 4> ReplacementArgs;
                auto arg_it = CI->arg_begin();
                assert(arg_it != CI->arg_end());
                ReplacementArgs.push_back(*(arg_it++));
                if (CC != JLCALL_F_CC) {
                    assert(arg_it != CI->arg_end());
                    ReplacementArgs.push_back(*(arg_it++));
                }
                maxframeargs = std::max(maxframeargs, nframeargs);
                int slot = 0;
                IRBuilder<> Builder (CI);
                for (; arg_it != CI->arg_end(); ++arg_it) {
                    Builder.CreateAlignedStore(*arg_it,
                            Builder.CreateInBoundsGEP(T_prjlvalue, Frame, ConstantInt::get(T_int32, slot++)),
                            Align(sizeof(void*)));
                }
                ReplacementArgs.push_back(nframeargs == 0 ?
                    (llvm::Value*)ConstantPointerNull::get(T_pprjlvalue) :
                    (llvm::Value*)Frame);
                ReplacementArgs.push_back(ConstantInt::get(T_int32, nframeargs));
                if (CC == JLCALL_F2_CC) {
                    // move trailing arg to the end now
                    Value *front = ReplacementArgs.front();
                    ReplacementArgs.erase(ReplacementArgs.begin());
                    ReplacementArgs.push_back(front);
                }
                FunctionType *FTy;
                if  (CC == JLCALL_F_CC) // jl_fptr_args
                    FTy = FunctionType::get(T_prjlvalue, {T_prjlvalue, T_pprjlvalue, T_int32}, false);
                else // CC == JLCALL_F2_CC // jl_invoke
                    FTy = FunctionType::get(T_prjlvalue, {T_prjlvalue, T_pprjlvalue, T_int32, T_prjlvalue}, false);
                Value *newFptr = Builder.CreateBitCast(callee, FTy->getPointerTo());
                CallInst *NewCall = CallInst::Create(FTy, newFptr, ReplacementArgs, "", CI);
                NewCall->setTailCallKind(CI->getTailCallKind());
                auto old_attrs = CI->getAttributes();
                NewCall->setAttributes(AttributeList::get(CI->getContext(),
                                                          old_attrs.getFnAttributes(),
                                                          old_attrs.getRetAttributes(), {}));
                NewCall->copyMetadata(*CI);
                CI->replaceAllUsesWith(NewCall);
                UpdatePtrNumbering(CI, NewCall, S);
            } else if (CI->getNumArgOperands() == CI->getNumOperands()) {
                /* No operand bundle to lower */
                ++it;
                continue;
            } else {
                CallInst *NewCall = CallInst::Create(CI, None, CI);
                NewCall->takeName(CI);
                NewCall->copyMetadata(*CI);
                CI->replaceAllUsesWith(NewCall);
                UpdatePtrNumbering(CI, NewCall, S);
            }
            if (!CI->use_empty()) {
                CI->replaceAllUsesWith(UndefValue::get(CI->getType()));
                UpdatePtrNumbering(CI, nullptr, S);
            }
            it = CI->eraseFromParent();
            ChangesMade = true;
        }
    }
    for (auto CI : write_barriers) {
        auto parent = CI->getArgOperand(0);
        if (std::all_of(CI->op_begin() + 1, CI->op_end(),
                    [parent, &S](Value *child) { return parent == child || IsPermRooted(child, S); })) {
            CI->eraseFromParent();
            continue;
        }
        IRBuilder<> builder(CI);
        builder.SetCurrentDebugLocation(CI->getDebugLoc());
        auto parBits = builder.CreateAnd(EmitLoadTag(builder, parent), 3);
        auto parOldMarked = builder.CreateICmpEQ(parBits, ConstantInt::get(T_size, 3));
        auto mayTrigTerm = SplitBlockAndInsertIfThen(parOldMarked, CI, false);
        builder.SetInsertPoint(mayTrigTerm);
        Value *anyChldNotMarked = NULL;
        for (unsigned i = 1; i < CI->getNumArgOperands(); i++) {
            Value *child = CI->getArgOperand(i);
            Value *chldBit = builder.CreateAnd(EmitLoadTag(builder, child), 1);
            Value *chldNotMarked = builder.CreateICmpEQ(chldBit, ConstantInt::get(T_size, 0));
            anyChldNotMarked = anyChldNotMarked ? builder.CreateOr(anyChldNotMarked, chldNotMarked) : chldNotMarked;
        }
        assert(anyChldNotMarked); // handled by all_of test above
        MDBuilder MDB(parent->getContext());
        SmallVector<uint32_t, 2> Weights{1, 9};
        auto trigTerm = SplitBlockAndInsertIfThen(anyChldNotMarked, mayTrigTerm, false,
                                                  MDB.createBranchWeights(Weights));
        builder.SetInsertPoint(trigTerm);
        builder.CreateCall(getOrDeclare(jl_intrinsics::queueGCRoot), parent);
        CI->eraseFromParent();
    }
    if (maxframeargs == 0 && Frame) {
        Frame->eraseFromParent();
    }
    else if (Frame) {
        Frame->setOperand(0, ConstantInt::get(T_int32, maxframeargs));
    }
    return ChangesMade;
}

static void AddInPredLiveOuts(BasicBlock *BB, BitVector &LiveIn, State &S)
{
    bool First = true;
    std::set<BasicBlock *> Visited;
    std::vector<BasicBlock *> WorkList;
    WorkList.push_back(BB);
    while (!WorkList.empty()) {
        BB = &*WorkList.back();
        WorkList.pop_back();
        // Nothing is live at function entry
        if (BB == &S.F->getEntryBlock()) {
            LiveIn.reset();
            return;
        }
        for (BasicBlock *Pred : predecessors(BB)) {
            if (!Visited.insert(Pred).second)
                continue;
            if (!S.BBStates[Pred].HasSafepoint) {
                WorkList.push_back(Pred);
                continue;
            } else {
                int LastSP = S.BBStates[Pred].Safepoints.front();
                if (First) {
                    LiveIn |= S.LiveSets[LastSP];
                    First = false;
                } else {
                    LiveIn &= S.LiveSets[LastSP];
                }
                if (LiveIn.empty()) // Just a compiler performance optimization
                    return;
            }
        }
    }
}

void LateLowerGCFrame::PlaceGCFrameStore(State &S, unsigned R, unsigned MinColorRoot,
                                         const std::vector<int> &Colors, Value *GCFrame,
                                         Instruction *InsertBefore) {
    // Get the slot address.
    auto slotAddress = CallInst::Create(
        getOrDeclare(jl_intrinsics::getGCFrameSlot),
        {GCFrame, ConstantInt::get(T_int32, Colors[R] + MinColorRoot)},
        "", InsertBefore);

    Value *Val = GetPtrForNumber(S, R, InsertBefore);
    // Pointee types don't have semantics, so the optimizer is
    // free to rewrite them if convenient. We need to change
    // it back here for the store.
    if (Val->getType() != T_prjlvalue)
        Val = new BitCastInst(Val, T_prjlvalue, "", InsertBefore);
    new StoreInst(Val, slotAddress, InsertBefore);
}

void LateLowerGCFrame::PlaceGCFrameStores(State &S, unsigned MinColorRoot,
                                          const std::vector<int> &Colors, Value *GCFrame)
{
    for (auto &BB : *S.F) {
        const BBState &BBS = S.BBStates[&BB];
        if (!BBS.HasSafepoint) {
            continue;
        }
        BitVector LiveIn;
        AddInPredLiveOuts(&BB, LiveIn, S);
        const BitVector *LastLive = &LiveIn;
        for(auto rit = BBS.Safepoints.rbegin();
              rit != BBS.Safepoints.rend(); ++rit ) {
            const BitVector &NowLive = S.LiveSets[*rit];
            for (int Idx = NowLive.find_first(); Idx >= 0; Idx = NowLive.find_next(Idx)) {
                if (!HasBitSet(*LastLive, Idx)) {
                    PlaceGCFrameStore(S, Idx, MinColorRoot, Colors, GCFrame,
                      S.ReverseSafepointNumbering[*rit]);
                }
            }
            LastLive = &NowLive;
        }
    }
}

void LateLowerGCFrame::PlaceRootsAndUpdateCalls(std::vector<int> &Colors, State &S, std::map<Value *, std::pair<int, int>>) {
    auto F = S.F;
    int MaxColor = -1;
    for (auto C : Colors)
        if (C > MaxColor)
            MaxColor = C;

    // Insert instructions for the actual gc frame
    if (MaxColor != -1 || !S.Allocas.empty() || !S.ArrayAllocas.empty() || !S.TrackedStores.empty()) {
        // Create and push a GC frame.
        auto gcframe = CallInst::Create(
            getOrDeclare(jl_intrinsics::newGCFrame),
            {ConstantInt::get(T_int32, 0)},
            "gcframe");
        gcframe->insertBefore(&*F->getEntryBlock().begin());

        auto pushGcframe = CallInst::Create(
            getOrDeclare(jl_intrinsics::pushGCFrame),
            {gcframe, ConstantInt::get(T_int32, 0)});
        pushGcframe->insertAfter(ptlsStates);

        // Replace Allocas
        unsigned AllocaSlot = 2; // first two words are metadata
        auto replace_alloca = [this, gcframe, &AllocaSlot](AllocaInst *&AI) {
            // Pick a slot for the alloca.
            unsigned align = AI->getAlignment() / sizeof(void*); // TODO: use DataLayout pointer size
            assert(align <= 16 / sizeof(void*) && "Alignment exceeds llvm-final-gc-lowering abilities");
            if (align > 1)
                AllocaSlot = LLT_ALIGN(AllocaSlot, align);
            Instruction *slotAddress = CallInst::Create(
                getOrDeclare(jl_intrinsics::getGCFrameSlot),
                {gcframe, ConstantInt::get(T_int32, AllocaSlot - 2)});
            slotAddress->insertAfter(gcframe);
            slotAddress->takeName(AI);

            // Check for lifetime intrinsics on this alloca, we can't keep them
            // because we're changing the semantics
            std::vector<CallInst*> ToDelete;
            RecursivelyVisit<IntrinsicInst>([&](Use &VU) {
                IntrinsicInst *II = cast<IntrinsicInst>(VU.getUser());
                if ((II->getIntrinsicID() != Intrinsic::lifetime_start &&
                            II->getIntrinsicID() != Intrinsic::lifetime_end))
                    return;
                ToDelete.push_back(II);
            }, AI);
            for (CallInst *II : ToDelete) {
                II->eraseFromParent();
            }
            if (slotAddress->getType() != AI->getType()) {
                // If we're replacing an ArrayAlloca, the pointer element type may need to be fixed up
                auto BCI  = new BitCastInst(slotAddress, AI->getType());
                BCI->insertAfter(slotAddress);
                slotAddress = BCI;
            }
            AI->replaceAllUsesWith(slotAddress);
            AI->eraseFromParent();
            AI = NULL;
        };
        for (AllocaInst *AI : S.Allocas) {
            auto ns = cast<ConstantInt>(AI->getArraySize())->getZExtValue();
            replace_alloca(AI);
            AllocaSlot += ns;
        }
        for (auto AI : S.ArrayAllocas) {
            replace_alloca(AI.first);
            AllocaSlot += AI.second;
        }
        for (auto Store : S.TrackedStores) {
            auto SI = Store.first;
            auto Base = SI->getValueOperand();
            //auto Tracked = TrackCompositeType(Base->getType());
            for (unsigned i = 0; i < Store.second; ++i) {
                auto slotAddress = CallInst::Create(
                    getOrDeclare(jl_intrinsics::getGCFrameSlot),
                    {gcframe, ConstantInt::get(T_int32, AllocaSlot - 2)});
                slotAddress->insertAfter(gcframe);
                auto ValExpr = std::make_pair(Base, isa<PointerType>(Base->getType()) ? -1 : i);
                auto Elem = MaybeExtractScalar(S, ValExpr, SI);
                if (Elem->getType() != T_prjlvalue)
                    Elem = new BitCastInst(Elem, T_prjlvalue, "", SI);
                //auto Idxs = makeArrayRef(Tracked[i]);
                //Value *Elem = ExtractScalar(Base, true, Idxs, SI);
                Value *shadowStore = new StoreInst(Elem, slotAddress, SI);
                (void)shadowStore;
                // TODO: shadowStore->setMetadata(LLVMContext::MD_tbaa, tbaa_gcframe);
                AllocaSlot++;
            }
        }
        auto NRoots = ConstantInt::get(T_int32, MaxColor + 1 + AllocaSlot - 2);
        gcframe->setArgOperand(0, NRoots);
        pushGcframe->setArgOperand(1, NRoots);

        // Insert GC frame stores
        PlaceGCFrameStores(S, AllocaSlot - 2, Colors, gcframe);
        // Insert GCFrame pops
        for(Function::iterator I = F->begin(), E = F->end(); I != E; ++I) {
            if (isa<ReturnInst>(I->getTerminator())) {
                auto popGcframe = CallInst::Create(
                    getOrDeclare(jl_intrinsics::popGCFrame),
                    {gcframe});
                popGcframe->insertBefore(I->getTerminator());
            }
        }
    }
}

bool LateLowerGCFrame::doInitialization(Module &M) {
    // Initialize platform-agnostic references.
    initAll(M);
    return true;
}

bool LateLowerGCFrame::runOnFunction(Function &F) {
    LLVM_DEBUG(dbgs() << "GC ROOT PLACEMENT: Processing function " << F.getName() << "\n");
    // Check availability of functions again since they might have been deleted.
    initFunctions(*F.getParent());
    if (!ptls_getter)
        return CleanupIR(F);

    ptlsStates = getPtls(F);
    if (!ptlsStates)
        return CleanupIR(F);

    State S = LocalScan(F);
    ComputeLiveness(S);
    std::vector<int> Colors = ColorRoots(S);
    std::map<Value *, std::pair<int, int>> CallFrames; // = OptimizeCallFrames(S, Ordering);
    PlaceRootsAndUpdateCalls(Colors, S, CallFrames);
    CleanupIR(F, &S);
    return true;
}

char LateLowerGCFrame::ID = 0;
static RegisterPass<LateLowerGCFrame> X("LateLowerGCFrame", "Late Lower GCFrame Pass", false, false);

Pass *createLateLowerGCFramePass() {
    return new LateLowerGCFrame();
}

extern "C" JL_DLLEXPORT void LLVMExtraAddLateLowerGCFramePass(LLVMPassManagerRef PM)
{
    unwrap(PM)->add(createLateLowerGCFramePass());
}

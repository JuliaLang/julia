// This file is a part of Julia. License is MIT: https://julialang.org/license

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
#include <llvm/IR/CallSite.h>
#include <llvm/IR/MDBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Pass.h>
#include <llvm/Support/Debug.h>
#include <llvm/Transforms/Utils/BasicBlockUtils.h>
#include <llvm/Transforms/Utils/ModuleUtils.h>

#include "llvm-version.h"
#include "codegen_shared.h"
#include "julia.h"
#include "julia_internal.h"
#include "julia_assert.h"

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
    // These do not get updated after local analysis
    BitVector Defs;
    BitVector PhiOuts;
    //// Upward exposed uses that do not have a preceding safepoint
    BitVector UpExposedUsesUnrooted;
    //// All other uses
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
    std::map<Value *, std::vector<int>> AllVectorNumbering;
    // Numbering of pointers. This only includes Defs
    std::map<Value *, int> PtrNumbering;
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
    State(Function &F) : F(&F), DT(nullptr), MaxPtrNumber(-1), MaxSafepointNumber(-1) {}
};

namespace llvm {
    void initializeLateLowerGCFramePass(PassRegistry &Registry);
}

extern std::pair<MDNode*,MDNode*> tbaa_make_child(const char *name, MDNode *parent=nullptr, bool isConstant=false);
struct LateLowerGCFrame: public FunctionPass {
    static char ID;
    LateLowerGCFrame() : FunctionPass(ID)
    {
        llvm::initializeDominatorTreeWrapperPassPass(*PassRegistry::getPassRegistry());
        tbaa_gcframe = tbaa_make_child("jtbaa_gcframe").first;
        MDNode *tbaa_data;
        MDNode *tbaa_data_scalar;
        std::tie(tbaa_data, tbaa_data_scalar) = tbaa_make_child("jtbaa_data");
        tbaa_tag = tbaa_make_child("jtbaa_tag", tbaa_data_scalar).first;
    }

protected:
    void getAnalysisUsage(AnalysisUsage &AU) const override {
        FunctionPass::getAnalysisUsage(AU);
        AU.addRequired<DominatorTreeWrapperPass>();
        AU.addPreserved<DominatorTreeWrapperPass>();
        AU.setPreservesCFG();
    }

private:
    Type *T_prjlvalue;
    Type *T_ppjlvalue;
    Type *T_size;
    Type *T_int8;
    Type *T_int32;
    Type *T_pint8;
    Type *T_pjlvalue;
    Type *T_pjlvalue_der;
    Type *T_ppjlvalue_der;
    MDNode *tbaa_gcframe;
    MDNode *tbaa_tag;
    Function *ptls_getter;
    Function *gc_flush_func;
    Function *gc_preserve_begin_func;
    Function *gc_preserve_end_func;
    Function *pointer_from_objref_func;
    Function *alloc_obj_func;
    Function *typeof_func;
    Function *write_barrier_func;
    Function *queueroot_func;
    Function *pool_alloc_func;
    Function *big_alloc_func;
    CallInst *ptlsStates;

    void MaybeNoteDef(State &S, BBState &BBS, Value *Def, const std::vector<int> &SafepointsSoFar, SmallVector<int, 1> &&RefinedPtr = SmallVector<int, 1>());
    void NoteUse(State &S, BBState &BBS, Value *V, BitVector &Uses);
    void NoteUse(State &S, BBState &BBS, Value *V) {
        NoteUse(S, BBS, V, BBS.UpExposedUses);
    }
    Value *MaybeExtractUnion(std::pair<Value*,int> Val, Instruction *InsertBefore);
    void LiftPhi(State &S, PHINode *Phi, SmallVector<int, 16> &PHINumbers);
    bool LiftSelect(State &S, SelectInst *SI);
    int Number(State &S, Value *V);
    std::vector<int> NumberVector(State &S, Value *Vec);
    int NumberBase(State &S, Value *V, Value *Base);
    std::vector<int> NumberVectorBase(State &S, Value *Base);
    void NoteOperandUses(State &S, BBState &BBS, User &UI, BitVector &Uses);
    void NoteOperandUses(State &S, BBState &BBS, User &UI){
        NoteOperandUses(S, BBS, UI, BBS.UpExposedUses);
    }
    State LocalScan(Function &F);
    void ComputeLiveness(State &S);
    void ComputeLiveSets(State &S);
    void PushGCFrame(AllocaInst *gcframe, unsigned NRoots, Instruction *InsertAfter);
    void PopGCFrame(AllocaInst *gcframe, Instruction *InsertBefore);
    std::vector<int> ColorRoots(const State &S);
    void PlaceGCFrameStore(State &S, unsigned R, unsigned MinColorRoot, const std::vector<int> &Colors, Value *GCFrame, Instruction *InsertionPoint);
    void PlaceGCFrameStores(State &S, unsigned MinColorRoot, const std::vector<int> &Colors, Value *GCFrame);
    void PlaceRootsAndUpdateCalls(std::vector<int> &Colors, State &S, std::map<Value *, std::pair<int, int>>);
    bool doInitialization(Module &M) override;
    void reinitFunctions(Module &M);
    bool doFinalization(Module &) override;
    bool runOnFunction(Function &F) override;
    Instruction *get_pgcstack(Instruction *ptlsStates);
    bool CleanupIR(Function &F, State *S=nullptr);
    void NoteUseChain(State &S, BBState &BBS, User *TheUser);
    SmallVector<int, 1> GetPHIRefinements(PHINode *phi, State &S);
    void FixUpRefinements(ArrayRef<int> PHINumbers, State &S);
    void RefineLiveSet(BitVector &LS, State &S);
    Value *EmitTagPtr(IRBuilder<> &builder, Type *T, Value *V);
    Value *EmitLoadTag(IRBuilder<> &builder, Value *V);
};

static unsigned getValueAddrSpace(Value *V) {
    Type *Ty = V->getType();
    if (isa<VectorType>(Ty))
        Ty = cast<VectorType>(V->getType())->getElementType();
    return cast<PointerType>(Ty)->getAddressSpace();
}

static bool isSpecialPtr(Type *Ty) {
    PointerType *PTy = dyn_cast<PointerType>(Ty);
    if (!PTy)
        return false;
    unsigned AS = PTy->getAddressSpace();
    return AddressSpace::FirstSpecial <= AS && AS <= AddressSpace::LastSpecial;
}

static bool isSpecialPtrVec(Type *Ty) {
    auto *VTy = dyn_cast<VectorType>(Ty);
    if (!VTy)
        return false;
    return isSpecialPtr(VTy->getElementType());
}

static bool isUnionRep(Type *Ty) {
    return Ty->isStructTy() && cast<StructType>(Ty)->getNumElements() == 2 &&
        isSpecialPtr(cast<StructType>(Ty)->getTypeAtIndex((unsigned)0));
}

// If the input value is a scalar (pointer), we may return a vector value as base
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
                auto it = S.AllVectorNumbering.find(CurrentV);
                if (it != S.AllVectorNumbering.end())
                    return std::make_pair(CurrentV, fld_idx);
            }
        }
        if (isa<BitCastInst>(CurrentV))
            CurrentV = cast<BitCastInst>(CurrentV)->getOperand(0);
        else if (isa<AddrSpaceCastInst>(CurrentV)) {
            Value *NewV = cast<AddrSpaceCastInst>(CurrentV)->getOperand(0);
            if (getValueAddrSpace(NewV) == 0)
                break;
            CurrentV = NewV;
        }
        else if (isa<GetElementPtrInst>(CurrentV)) {
            CurrentV = cast<GetElementPtrInst>(CurrentV)->getOperand(0);
            // GEP can make vectors from a single base pointer
            if (fld_idx != -1 && !isSpecialPtrVec(CurrentV->getType())) {
                fld_idx = -1;
            }
        } else if (isa<ExtractValueInst>(CurrentV)) {
            Value *Operand = cast<ExtractValueInst>(CurrentV)->getOperand(0);
            if (!isUnionRep(Operand->getType()))
                break;
            CurrentV = Operand;
        }
        else if (isa<InsertValueInst>(CurrentV)) {
            if (!isUnionRep(CurrentV->getType()))
                break;
            InsertValueInst *IVI = cast<InsertValueInst>(CurrentV);
            assert(IVI->getNumIndices() == 1);
            unsigned idx = IVI->getIndices()[0];
            if (idx == 0) {
                // Updating the pointer in the union. Follow the pointer.
                CurrentV = IVI->getOperand(1);
            } else {
                // Updating which tindex is active. Follow the union.
                assert(idx == 1);
                CurrentV = IVI->getOperand(0);
            }
        }
        else if (auto EEI = dyn_cast<ExtractElementInst>(CurrentV)) {
            assert(CurrentV->getType()->isPointerTy() && fld_idx == -1);
            // For now, only support constant index.
            auto IdxOp = cast<ConstantInt>(EEI->getIndexOperand());
            fld_idx = IdxOp->getLimitedValue(INT_MAX);
            CurrentV = EEI->getVectorOperand();
        }
        else if (auto LI = dyn_cast<LoadInst>(CurrentV)) {
            if (auto PtrT = dyn_cast<PointerType>(LI->getType())) {
                if (PtrT->getAddressSpace() == AddressSpace::Loaded) {
                    CurrentV = LI->getPointerOperand();
                    if (!isSpecialPtr(CurrentV->getType())) {
                        // Special case to bypass the check below.
                        // This could really be anything, but it's not loaded
                        // from a tracked pointer, so it doesn't matter what
                        // it is.
                        return std::make_pair(CurrentV, fld_idx);
                    }
                    continue;
                }
            }
            // In general a load terminates a walk
            break;
        }
        else {
            break;
        }
    }
    assert(isa<LoadInst>(CurrentV) || isa<CallInst>(CurrentV) ||
           isa<Argument>(CurrentV) || isa<SelectInst>(CurrentV) ||
           isa<PHINode>(CurrentV) || isa<AddrSpaceCastInst>(CurrentV) ||
           isa<Constant>(CurrentV) || isa<AllocaInst>(CurrentV) ||
           isa<ExtractValueInst>(CurrentV) ||
           isa<InsertElementInst>(CurrentV) ||
           isa<ShuffleVectorInst>(CurrentV));
    return std::make_pair(CurrentV, fld_idx);
}

Value *LateLowerGCFrame::MaybeExtractUnion(std::pair<Value*,int> Val, Instruction *InsertBefore) {
    if (isUnionRep(Val.first->getType())) {
        assert(Val.second == -1);
        return ExtractValueInst::Create(Val.first, {(unsigned)0}, "", InsertBefore);
    }
    else if (Val.second != -1) {
        return ExtractElementInst::Create(Val.first, ConstantInt::get(T_int32, Val.second),
                                          "", InsertBefore);
    }
    return Val.first;
}

static Value *GetPtrForNumber(State &S, unsigned Num, Instruction *InsertionPoint)
{
    Value *Val = S.ReversePtrNumbering[Num];
    if (isSpecialPtrVec(Val->getType())) {
        const std::vector<int> &AllNums = S.AllVectorNumbering[Val];
        unsigned Idx = 0;
        for (; Idx < AllNums.size(); ++Idx) {
            if ((unsigned)AllNums[Idx] == Num)
                break;
        }
        Val = ExtractElementInst::Create(Val, ConstantInt::get(
            Type::getInt32Ty(Val->getContext()), Idx), "", InsertionPoint);
    }
    return Val;
}

bool LateLowerGCFrame::LiftSelect(State &S, SelectInst *SI) {
    if (isSpecialPtrVec(SI->getType())) {
        VectorType *VT = cast<VectorType>(SI->getType());
        std::vector<int> TrueNumbers = NumberVector(S, SI->getTrueValue());
        std::vector<int> FalseNumbers = NumberVector(S, SI->getFalseValue());
        std::vector<int> Numbers;
        for (unsigned i = 0; i < VT->getNumElements(); ++i) {
            SelectInst *LSI = SelectInst::Create(SI->getCondition(),
              TrueNumbers[i] < 0 ?
                ConstantPointerNull::get(cast<PointerType>(T_prjlvalue)) :
                GetPtrForNumber(S, TrueNumbers[i], SI),
              FalseNumbers[i] < 0 ?
                ConstantPointerNull::get(cast<PointerType>(T_prjlvalue)) :
                GetPtrForNumber(S, FalseNumbers[i], SI),
              "gclift", SI);
            int Number = ++S.MaxPtrNumber;
            Numbers.push_back(Number);
            S.PtrNumbering[LSI] = S.AllPtrNumbering[LSI] = Number;
            S.ReversePtrNumbering[Number] = LSI;
        }
        S.AllVectorNumbering[SI] = Numbers;
    } else {
        Value *TrueBase = MaybeExtractUnion(FindBaseValue(S, SI->getTrueValue(), false), SI);
        Value *FalseBase = MaybeExtractUnion(FindBaseValue(S, SI->getFalseValue(), false), SI);
        if (getValueAddrSpace(TrueBase) != AddressSpace::Tracked)
            TrueBase = ConstantPointerNull::get(cast<PointerType>(FalseBase->getType()));
        if (getValueAddrSpace(FalseBase) != AddressSpace::Tracked)
            FalseBase = ConstantPointerNull::get(cast<PointerType>(TrueBase->getType()));
        if (getValueAddrSpace(TrueBase) != AddressSpace::Tracked)
            return false;
        Value *SelectBase = SelectInst::Create(SI->getCondition(),
            TrueBase, FalseBase, "gclift", SI);
        int Number = ++S.MaxPtrNumber;
        S.PtrNumbering[SelectBase] = S.AllPtrNumbering[SelectBase] =
            S.AllPtrNumbering[SI] = Number;
        S.ReversePtrNumbering[Number] = SelectBase;
    }
    return true;
}

void LateLowerGCFrame::LiftPhi(State &S, PHINode *Phi, SmallVector<int, 16> &PHINumbers)
{
    if (isSpecialPtrVec(Phi->getType())) {
        VectorType *VT = cast<VectorType>(Phi->getType());
        std::vector<PHINode *> lifted;
        for (unsigned i = 0; i < VT->getNumElements(); ++i) {
            lifted.push_back(PHINode::Create(T_prjlvalue, Phi->getNumIncomingValues(), "gclift", Phi));
        }
        for (unsigned i = 0; i < Phi->getNumIncomingValues(); ++i) {
            std::vector<int> Numbers = NumberVector(S, Phi->getIncomingValue(i));
            BasicBlock *IncomingBB = Phi->getIncomingBlock(i);
            Instruction *Terminator = IncomingBB->getTerminator();
            for (unsigned i = 0; i < VT->getNumElements(); ++i) {
                if (Numbers[i] < 0)
                    lifted[i]->addIncoming(ConstantPointerNull::get(cast<PointerType>(T_prjlvalue)), IncomingBB);
                else
                    lifted[i]->addIncoming(GetPtrForNumber(S, Numbers[i], Terminator), IncomingBB);
            }
        }
        std::vector<int> Numbers;
        for (unsigned i = 0; i < VT->getNumElements(); ++i) {
            int Number = ++S.MaxPtrNumber;
            PHINumbers.push_back(Number);
            Numbers.push_back(Number);
            S.PtrNumbering[lifted[i]] = S.AllPtrNumbering[lifted[i]] = Number;
            S.ReversePtrNumbering[Number] = lifted[i];
        }
        S.AllVectorNumbering[Phi] = Numbers;
    } else {
        PHINode *lift = PHINode::Create(T_prjlvalue, Phi->getNumIncomingValues(), "gclift", Phi);
        for (unsigned i = 0; i < Phi->getNumIncomingValues(); ++i) {
            Value *Incoming = Phi->getIncomingValue(i);
            Value *Base = MaybeExtractUnion(FindBaseValue(S, Incoming, false),
                                            Phi->getIncomingBlock(i)->getTerminator());
            if (getValueAddrSpace(Base) != AddressSpace::Tracked)
                Base = ConstantPointerNull::get(cast<PointerType>(T_prjlvalue));
            if (Base->getType() != T_prjlvalue)
                Base = new BitCastInst(Base, T_prjlvalue, "", Phi->getIncomingBlock(i)->getTerminator());
            lift->addIncoming(Base, Phi->getIncomingBlock(i));
        }
        int Number = ++S.MaxPtrNumber;
        PHINumbers.push_back(Number);
        S.PtrNumbering[lift] = S.AllPtrNumbering[lift] =
            S.AllPtrNumbering[Phi] = Number;
        S.ReversePtrNumbering[Number] = lift;
    }
}

int LateLowerGCFrame::NumberBase(State &S, Value *V, Value *CurrentV)
{
    auto it = S.AllPtrNumbering.find(CurrentV);
    if (it != S.AllPtrNumbering.end())
        return it->second;
    int Number;
    bool isUnion = isUnionRep(CurrentV->getType());
    if (isa<Constant>(CurrentV)) {
        // Perm rooted
        Number = -2;
    } else if (isa<Argument>(CurrentV) ||
               ((isa<AllocaInst>(CurrentV) || isa<AddrSpaceCastInst>(CurrentV)) &&
                getValueAddrSpace(CurrentV) != AddressSpace::Tracked)) {
        // We know this is rooted in the parent
        Number = -1;
    } else if (!isSpecialPtr(CurrentV->getType()) && !isUnion) {
        // Externally rooted somehow hopefully (otherwise there's a bug in the
        // input IR)
        Number = -1;
    } else if (isa<SelectInst>(CurrentV) && !isUnion && getValueAddrSpace(CurrentV) != AddressSpace::Tracked) {
        Number = -1;
        if (LiftSelect(S, cast<SelectInst>(CurrentV)))
            Number = S.AllPtrNumbering[V] = S.AllPtrNumbering.at(CurrentV);
        return Number;
    } else if (isa<PHINode>(CurrentV) && !isUnion && getValueAddrSpace(CurrentV) != AddressSpace::Tracked) {
        SmallVector<int, 16> PHINumbers;
        LiftPhi(S, cast<PHINode>(CurrentV), PHINumbers);
        Number = S.AllPtrNumbering[V] = S.AllPtrNumbering.at(CurrentV);
        return Number;
    } else if (isa<ExtractValueInst>(CurrentV) && !isUnion) {
        assert(false && "TODO: Extract");
        abort();
    } else {
        assert(
            (CurrentV->getType()->isPointerTy() &&
                getValueAddrSpace(CurrentV) == AddressSpace::Tracked) ||
            isUnion);
        Number = ++S.MaxPtrNumber;
        S.ReversePtrNumbering[Number] = CurrentV;
    }
    S.PtrNumbering[CurrentV] = S.AllPtrNumbering[CurrentV] = S.AllPtrNumbering[V] = Number;
    return Number;
}

int LateLowerGCFrame::Number(State &S, Value *V) {
    assert(isSpecialPtr(V->getType()) || isUnionRep(V->getType()));
    auto CurrentV = FindBaseValue(S, V);
    if (CurrentV.second == -1)
        return NumberBase(S, V, CurrentV.first);
    auto Numbers = NumberVectorBase(S, CurrentV.first);
    auto Number = Numbers.size() == 0 ? -1 : Numbers[CurrentV.second];
    S.AllPtrNumbering[V] = Number;
    return Number;
}

std::vector<int> LateLowerGCFrame::NumberVectorBase(State &S, Value *CurrentV) {
    auto it = S.AllVectorNumbering.find(CurrentV);
    if (it != S.AllVectorNumbering.end())
        return it->second;
    std::vector<int> Numbers{};
    if (isa<Constant>(CurrentV) ||
        ((isa<Argument>(CurrentV) || isa<AllocaInst>(CurrentV) ||
         isa<AddrSpaceCastInst>(CurrentV)) &&
         getValueAddrSpace(CurrentV) != AddressSpace::Tracked)) {
        Numbers.resize(cast<VectorType>(CurrentV->getType())->getNumElements(), -1);
    }
    /* We (the frontend) don't insert either of these, but it would be legal -
       though a bit strange, considering they're pointers - for the optimizer to
       do so. All that's needed here is to NumberVector the previous vector/value
       and lift the operation */
    else if (auto *SVI = dyn_cast<ShuffleVectorInst>(CurrentV)) {
        std::vector<int> Numbers1 = NumberVectorBase(S, SVI->getOperand(0));
        std::vector<int> Numbers2 = NumberVectorBase(S, SVI->getOperand(1));
        auto Mask = SVI->getShuffleMask();
        for (unsigned idx : Mask) {
            if (idx < Numbers1.size()) {
                Numbers.push_back(Numbers1[idx]);
            } else {
                Numbers.push_back(Numbers2[idx - Numbers1.size()]);
            }
        }
    } else if (auto *IEI = dyn_cast<InsertElementInst>(CurrentV)) {
        unsigned idx = cast<ConstantInt>(IEI->getOperand(2))->getZExtValue();
        Numbers = NumberVectorBase(S, IEI->getOperand(0));
        int ElNumber = Number(S, IEI->getOperand(1));
        Numbers[idx] = ElNumber;
    } else if (isa<SelectInst>(CurrentV) && getValueAddrSpace(CurrentV) != AddressSpace::Tracked) {
        LiftSelect(S, cast<SelectInst>(CurrentV));
        Numbers = S.AllVectorNumbering[CurrentV];
    } else if (isa<PHINode>(CurrentV) && getValueAddrSpace(CurrentV) != AddressSpace::Tracked) {
        SmallVector<int, 16> PHINumbers;
        LiftPhi(S, cast<PHINode>(CurrentV), PHINumbers);
        Numbers = S.AllVectorNumbering[CurrentV];
    } else if (isa<LoadInst>(CurrentV) || isa<CallInst>(CurrentV) || isa<PHINode>(CurrentV) ||
               isa<SelectInst>(CurrentV)) {
        // This is simple, we can just number them sequentially
        for (unsigned i = 0; i < cast<VectorType>(CurrentV->getType())->getNumElements(); ++i) {
            int Num = ++S.MaxPtrNumber;
            Numbers.push_back(Num);
            S.ReversePtrNumbering[Num] = CurrentV;
        }
    } else {
        assert(false && "Unexpected vector generating operation");
    }
    S.AllVectorNumbering[CurrentV] = Numbers;
    return Numbers;
}

std::vector<int> LateLowerGCFrame::NumberVector(State &S, Value *V) {
    auto it = S.AllVectorNumbering.find(V);
    if (it != S.AllVectorNumbering.end())
        return it->second;
    auto CurrentV = FindBaseValue(S, V);
    assert(CurrentV.second == -1);
    // E.g. if this is a gep, it's possible for the base to be a single ptr
    if (isSpecialPtrVec(CurrentV.first->getType())) {
        auto Numbers = NumberVectorBase(S, CurrentV.first);
        S.AllVectorNumbering[V] = Numbers;
        return Numbers;
    } else {
        std::vector<int> Numbers{};
        Numbers.resize(cast<VectorType>(V->getType())->getNumElements(),
            NumberBase(S, V, CurrentV.first));
        return Numbers;
    }
}

static void MaybeResize(BBState &BBS, unsigned Idx) {
    if (BBS.Defs.size() <= Idx) {
        BBS.Defs.resize(Idx + 1);
        BBS.UpExposedUses.resize(Idx + 1);
        BBS.UpExposedUsesUnrooted.resize(Idx + 1);
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
    BBS.UpExposedUsesUnrooted[Num] = 0;
    // This value could potentially be live at any following safe point
    // if it ends up live out, so add it to the LiveIfLiveOut lists for all
    // following safepoints.
    for (int Safepoint : SafepointsSoFar) {
        S.LiveIfLiveOut[Safepoint].push_back(Num);
    }
}

void LateLowerGCFrame::MaybeNoteDef(State &S, BBState &BBS, Value *Def, const std::vector<int> &SafepointsSoFar, SmallVector<int, 1> &&RefinedPtr) {
    int Num = -1;
    Type *RT = Def->getType();
    if (isSpecialPtr(RT)) {
        assert(getValueAddrSpace(Def) == AddressSpace::Tracked &&
            "Returned value of GC interest, but not tracked?");
        Num = Number(S, Def);
    }
    else if (isUnionRep(RT)) {
        // Probably a union return. Find the extractvalue
        Num = Number(S, Def);
    }
    else if (isSpecialPtrVec(RT)) {
        std::vector<int> Nums = NumberVector(S, Def);
        for (int Num : Nums) {
            NoteDef(S, BBS, Num, SafepointsSoFar);
            if (!RefinedPtr.empty())
                S.Refinements[Num] = RefinedPtr;
        }
        return;
    }
    else {
        return;
    }
    NoteDef(S, BBS, Num, SafepointsSoFar);
    if (!RefinedPtr.empty())
        S.Refinements[Num] = std::move(RefinedPtr);
}

static int NoteSafepoint(State &S, BBState &BBS, CallInst *CI) {
    int Number = ++S.MaxSafepointNumber;
    S.SafepointNumbering[CI] = Number;
    S.ReverseSafepointNumbering.push_back(CI);
    // Note which pointers are upward exposed live here. They need to be
    // considered live at this safepoint even when they have a def earlier
    // in this BB (i.e. even when they don't participate in the dataflow
    // computation)
    BBS.UpExposedUses |= BBS.UpExposedUsesUnrooted;
    BBS.UpExposedUsesUnrooted.reset();
    S.LiveSets.push_back(BBS.UpExposedUses);
    S.LiveIfLiveOut.push_back(std::vector<int>{});
    return Number;
}

void LateLowerGCFrame::NoteUse(State &S, BBState &BBS, Value *V, BitVector &Uses) {
    // Short circuit to avoid having to deal with vectors of constants, etc.
    if (isa<Constant>(V))
        return;
    else if (isSpecialPtrVec(V->getType())) {
        std::vector<int> Nums = NumberVector(S, V);
        for (int Num : Nums) {
            MaybeResize(BBS, Num);
            if (Num < 0)
                continue;
            Uses[Num] = 1;
        }
    }
    else {
        int Num = Number(S, V);
        if (Num < 0)
            return;
        MaybeResize(BBS, Num);
        Uses[Num] = 1;
    }
}

void LateLowerGCFrame::NoteOperandUses(State &S, BBState &BBS, User &UI, BitVector &Uses) {
    for (Use &U : UI.operands()) {
        Value *V = U;
        if (!isSpecialPtr(V->getType()) && !isSpecialPtrVec(V->getType()))
            continue;
        NoteUse(S, BBS, V, Uses);
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
        dbgs() << "\n\tUpExposedUsesUnrooted: ";
        dumpBitVectorValues(S, S.BBStates[&BB].UpExposedUsesUnrooted);
        dbgs() << "\n\tUpExposedUses: ";
        dumpBitVectorValues(S, S.BBStates[&BB].UpExposedUses);
        dbgs() << "\n\tLiveIn: ";
        dumpBitVectorValues(S, S.BBStates[&BB].LiveIn);
        dbgs() << "\n\tLiveOut: ";
        dumpBitVectorValues(S, S.BBStates[&BB].LiveOut);
        dbgs() << "\n";
    }
}

// Check if this is a load from an immutable value. The easiest
// way to do so is to look at the tbaa and see if it derives from
// jtbaa_immut.
static bool isLoadFromImmut(LoadInst *LI)
{
    if (LI->getMetadata(LLVMContext::MD_invariant_load))
        return true;
    MDNode *TBAA = LI->getMetadata(LLVMContext::MD_tbaa);
    if (!TBAA)
        return false;
    while (TBAA->getNumOperands() > 1) {
        TBAA = cast<MDNode>(TBAA->getOperand(1).get());
        auto str = cast<MDString>(TBAA->getOperand(0))->getString();
        if (str == "jtbaa_immut" || str == "jtbaa_const") {
            return true;
        }
    }
    return false;
}

// Check if this is a load from an constant global.
static bool isLoadFromConstGV(LoadInst *LI)
{
    // We only emit single slot GV in codegen
    // but LLVM global merging can change the pointer operands to GEPs/bitcasts
    if (!isa<GlobalVariable>(LI->getPointerOperand()->stripInBoundsOffsets()))
        return false;
    MDNode *TBAA = LI->getMetadata(LLVMContext::MD_tbaa);
    if (!TBAA)
        return false;
    while (TBAA->getNumOperands() > 1) {
        TBAA = cast<MDNode>(TBAA->getOperand(1).get());
        if (cast<MDString>(TBAA->getOperand(0))->getString() == "jtbaa_const") {
            return true;
        }
    }
    return false;
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
        jl_safe_printf("Refinements for %d  --  ", Num);
        auto V = S->ReversePtrNumbering[Num];
        llvm_dump(V);
        for (auto refine: kv.second) {
            if (refine < 0) {
                jl_safe_printf("  %d\n", refine);
                continue;
            }
            jl_safe_printf("  %d: ", refine);
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
    // While we recursively relax the refinement, we need to keep track of the the values we've
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
    SmallVector<int, 16> PHINumbers;
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
                }
                auto callee = CI->getCalledFunction();
                if (callee && callee == typeof_func) {
                    MaybeNoteDef(S, BBS, CI, BBS.Safepoints, SmallVector<int, 1>{-2});
                }
                else {
                    MaybeNoteDef(S, BBS, CI, BBS.Safepoints);
                }
                NoteOperandUses(S, BBS, I, BBS.UpExposedUses);
                for (Use &U : CI->operands()) {
                    Value *V = U;
                    if (isUnionRep(V->getType())) {
                        NoteUse(S, BBS, V);
                        continue;
                    }
                }
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
                            int Num = Number(S, V);
                            if (Num >= 0)
                                args.push_back(Num);
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
                if (isLoadFromImmut(LI) && isSpecialPtr(LI->getPointerOperand()->getType())) {
                    RefinedPtr.push_back(Number(S, LI->getPointerOperand()));
                } else if (LI->getType()->isPointerTy() &&
                    isSpecialPtr(LI->getType()) &&
                    LooksLikeFrameRef(LI->getPointerOperand())) {
                    // Loads from a jlcall argument array
                    RefinedPtr.push_back(-1);
                }
                else if (isLoadFromConstGV(LI)) {
                    // If this is a const load from a global,
                    // we know that the object is a constant as well and doesn't need rooting.
                    RefinedPtr.push_back(-2);
                }
                if (!LI->getType()->isPointerTy() ||
                    cast<PointerType>(LI->getType())->getAddressSpace() != AddressSpace::Loaded) {
                    MaybeNoteDef(S, BBS, LI, BBS.Safepoints, std::move(RefinedPtr));
                }
                NoteOperandUses(S, BBS, I, BBS.UpExposedUsesUnrooted);
            } else if (SelectInst *SI = dyn_cast<SelectInst>(&I)) {
                // We need to insert an extra select for the GC root
                if (!isSpecialPtr(SI->getType()) && !isSpecialPtrVec(SI->getType()) &&
                    !isUnionRep(SI->getType()))
                    continue;
                if (!isUnionRep(SI->getType()) && getValueAddrSpace(SI) != AddressSpace::Tracked) {
                    if (isSpecialPtrVec(SI->getType()) ?
                        S.AllVectorNumbering.find(SI) != S.AllVectorNumbering.end() :
                        S.AllPtrNumbering.find(SI) != S.AllPtrNumbering.end())
                        continue;
                    if (!LiftSelect(S, SI))
                        continue;
                    if (!isSpecialPtrVec(SI->getType())) {
                        // TODO: Refinements for vector select
                        int Num = S.AllPtrNumbering[SI];
                        if (Num < 0)
                            continue;
                        auto SelectBase = cast<SelectInst>(S.ReversePtrNumbering[Num]);
                        SmallVector<int, 2> RefinedPtr{Number(S, SelectBase->getTrueValue()),
                                Number(S, SelectBase->getFalseValue())};
                        S.Refinements[Num] = std::move(RefinedPtr);
                    }
                } else {
                    SmallVector<int, 2> RefinedPtr;
                    if (!isSpecialPtrVec(SI->getType())) {
                        RefinedPtr = {
                            Number(S, SI->getTrueValue()),
                            Number(S, SI->getFalseValue())
                        };
                    }
                    MaybeNoteDef(S, BBS, SI, BBS.Safepoints, std::move(RefinedPtr));
                    NoteOperandUses(S, BBS, I, BBS.UpExposedUsesUnrooted);
                }
            } else if (PHINode *Phi = dyn_cast<PHINode>(&I)) {
                if (!isSpecialPtr(Phi->getType()) && !isSpecialPtrVec(Phi->getType()) &&
                    !isUnionRep(Phi->getType())) {
                    continue;
                }
                auto nIncoming = Phi->getNumIncomingValues();
                // We need to insert an extra phi for the GC root
                if (!isUnionRep(Phi->getType()) && getValueAddrSpace(Phi) != AddressSpace::Tracked) {
                    if (isSpecialPtrVec(Phi->getType()) ?
                        S.AllVectorNumbering.find(Phi) != S.AllVectorNumbering.end() :
                        S.AllPtrNumbering.find(Phi) != S.AllPtrNumbering.end())
                        continue;
                    LiftPhi(S, Phi, PHINumbers);
                } else {
                    SmallVector<int, 1> PHIRefinements;
                    if (!isSpecialPtrVec(Phi->getType()))
                        PHIRefinements = GetPHIRefinements(Phi, S);
                    MaybeNoteDef(S, BBS, Phi, BBS.Safepoints, std::move(PHIRefinements));
                    if (isSpecialPtrVec(Phi->getType())) {
                        // TODO: Vector refinements
                        std::vector<int> Nums = NumberVector(S, Phi);
                        for (int Num : Nums)
                            PHINumbers.push_back(Num);
                    } else {
                        PHINumbers.push_back(Number(S, Phi));
                    }
                    for (unsigned i = 0; i < nIncoming; ++i) {
                        BBState &IncomingBBS = S.BBStates[Phi->getIncomingBlock(i)];
                        NoteUse(S, IncomingBBS, Phi->getIncomingValue(i), IncomingBBS.PhiOuts);
                    }
                }
            } else if (isa<StoreInst>(&I) || isa<ReturnInst>(&I)) {
                NoteOperandUses(S, BBS, I, BBS.UpExposedUsesUnrooted);
            } else if (auto *ASCI = dyn_cast<AddrSpaceCastInst>(&I)) {
                if (getValueAddrSpace(ASCI) == AddressSpace::Tracked) {
                    SmallVector<int, 1> RefinedPtr{};
                    auto origin = ASCI->getPointerOperand()->stripPointerCasts();
                    if (auto LI = dyn_cast<LoadInst>(origin)) {
                        if (isLoadFromConstGV(LI)) {
                            RefinedPtr.push_back(-2);
                        }
                    }
                    MaybeNoteDef(S, BBS, ASCI, BBS.Safepoints, std::move(RefinedPtr));
                }
            } else if (auto *AI = dyn_cast<AllocaInst>(&I)) {
                if (isSpecialPtr(AI->getAllocatedType()) && !AI->isArrayAllocation() &&
                    cast<PointerType>(AI->getAllocatedType())->getAddressSpace() == AddressSpace::Tracked)
                {
                    S.Allocas.push_back(AI);
                }
            }
        }
        // Pre-seed the dataflow variables;
        BBS.LiveIn = BBS.UpExposedUses;
        BBS.LiveIn |= BBS.UpExposedUsesUnrooted;
        BBS.Done = true;
    }
    FixUpRefinements(PHINumbers, S);
    return S;
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
            NewLiveIn |= BBS.UpExposedUsesUnrooted;
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

Instruction *LateLowerGCFrame::get_pgcstack(Instruction *ptlsStates)
{
    Constant *offset = ConstantInt::getSigned(T_int32, offsetof(jl_tls_states_t, pgcstack) / sizeof(void*));
    return GetElementPtrInst::Create(nullptr,
                                     ptlsStates,
                                     ArrayRef<Value*>(offset),
                                     "jl_pgcstack");
}

void LateLowerGCFrame::PushGCFrame(AllocaInst *gcframe, unsigned NRoots, Instruction *InsertAfter) {
    IRBuilder<> builder(gcframe->getContext());
    builder.SetInsertPoint(&*(++BasicBlock::iterator(InsertAfter)));
    Instruction *inst =
        builder.CreateStore(ConstantInt::get(T_size, NRoots << 1),
                          builder.CreateBitCast(builder.CreateConstGEP1_32(gcframe, 0), T_size->getPointerTo()));
    inst->setMetadata(llvm::LLVMContext::MD_tbaa, tbaa_gcframe);
    Value *pgcstack = builder.Insert(get_pgcstack(ptlsStates));
    inst = builder.CreateStore(builder.CreateLoad(pgcstack),
                               builder.CreatePointerCast(builder.CreateConstGEP1_32(gcframe, 1), PointerType::get(T_ppjlvalue,0)));
    inst->setMetadata(llvm::LLVMContext::MD_tbaa, tbaa_gcframe);
    builder.CreateStore(gcframe, builder.CreateBitCast(pgcstack,
        PointerType::get(PointerType::get(T_prjlvalue, 0), 0)));
}

void LateLowerGCFrame::PopGCFrame(AllocaInst *gcframe, Instruction *InsertBefore) {
    IRBuilder<> builder(InsertBefore->getContext());
    builder.SetInsertPoint(InsertBefore); // set insert *before* Ret
    Instruction *gcpop =
        (Instruction*)builder.CreateConstGEP1_32(gcframe, 1);
    Instruction *inst = builder.CreateLoad(gcpop);
    inst->setMetadata(llvm::LLVMContext::MD_tbaa, tbaa_gcframe);
    inst = builder.CreateStore(inst,
                               builder.CreateBitCast(
                                 builder.Insert(get_pgcstack(ptlsStates)),
                                 PointerType::get(T_prjlvalue, 0)));
    inst->setMetadata(llvm::LLVMContext::MD_tbaa, tbaa_gcframe);
}

// Size of T is assumed to be `sizeof(void*)`
Value *LateLowerGCFrame::EmitTagPtr(IRBuilder<> &builder, Type *T, Value *V)
{
    assert(T == T_size || isa<PointerType>(T));
    auto TV = cast<PointerType>(V->getType());
    auto cast = builder.CreateBitCast(V, T->getPointerTo(TV->getAddressSpace()));
    return builder.CreateGEP(T, cast, ConstantInt::get(T_size, -1));
}

Value *LateLowerGCFrame::EmitLoadTag(IRBuilder<> &builder, Value *V)
{
    auto addr = EmitTagPtr(builder, T_size, V);
    auto load = builder.CreateLoad(T_size, addr);
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
    SmallVector<CallInst*, 16> write_barriers;
    for (BasicBlock &BB : F) {
        for (auto it = BB.begin(); it != BB.end();) {
            auto *CI = dyn_cast<CallInst>(&*it);
            if (!CI) {
                ++it;
                continue;
            }
            CallingConv::ID CC = CI->getCallingConv();
            auto callee = CI->getCalledValue();
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
                auto sz = (size_t)cast<ConstantInt>(CI->getArgOperand(1))->getZExtValue();
                // This is strongly architecture and OS dependent
                int osize;
                int offset = jl_gc_classify_pools(sz, &osize);
                IRBuilder<> builder(CI);
                builder.SetCurrentDebugLocation(CI->getDebugLoc());
                auto ptls = CI->getArgOperand(0);
                CallInst *newI;
                if (offset < 0) {
                    newI = builder.CreateCall(big_alloc_func,
                                              {ptls, ConstantInt::get(T_size,
                                                                      sz + sizeof(void*))});
                }
                else {
                    auto pool_offs = ConstantInt::get(T_int32, offset);
                    auto pool_osize = ConstantInt::get(T_int32, osize);
                    newI = builder.CreateCall(pool_alloc_func, {ptls, pool_offs, pool_osize});
                }
                newI->setAttributes(newI->getCalledFunction()->getAttributes());
                newI->takeName(CI);
                auto store = builder.CreateStore(CI->getArgOperand(2),
                                                 EmitTagPtr(builder, T_prjlvalue, newI));
                store->setMetadata(LLVMContext::MD_tbaa, tbaa_tag);
                CI->replaceAllUsesWith(newI);
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
                assert(CI->getNumArgOperands() == 2);
                write_barriers.push_back(CI);
                ChangesMade = true;
                ++it;
                continue;
            } else if (CC == JLCALL_CC ||
                       CC == JLCALL_F_CC) {
                assert(T_prjlvalue);
                size_t nargs = CI->getNumArgOperands();
                size_t nframeargs = nargs - (CC == JLCALL_F_CC);
                SmallVector<Value *, 3> ReplacementArgs;
                auto it = CI->arg_begin();
                if (CC == JLCALL_F_CC)
                    ReplacementArgs.push_back(*(it++));
                maxframeargs = std::max(maxframeargs, nframeargs);
                int slot = 0;
                IRBuilder<> Builder (CI);
                for (; it != CI->arg_end(); ++it) {
                    Builder.CreateStore(*it, Builder.CreateGEP(T_prjlvalue, Frame,
                        ConstantInt::get(T_int32, slot++)));
                }
                ReplacementArgs.push_back(nframeargs == 0 ?
                    (llvm::Value*)ConstantPointerNull::get(T_pprjlvalue) :
                    (llvm::Value*)Frame);
                ReplacementArgs.push_back(ConstantInt::get(T_int32, nframeargs));
                FunctionType *FTy = CC == JLCALL_F_CC ?
                    FunctionType::get(T_prjlvalue, {T_prjlvalue,
                        T_pprjlvalue, T_int32}, false) :
                    FunctionType::get(T_prjlvalue,
                        {T_pprjlvalue, T_int32}, false);
                Value *newFptr = Builder.CreateBitCast(callee, FTy->getPointerTo());
                CallInst *NewCall = CallInst::Create(newFptr, ReplacementArgs, "", CI);
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
    for (auto CI: write_barriers) {
        auto parent = CI->getArgOperand(0);
        auto child = CI->getArgOperand(1);
        if (parent == child || IsPermRooted(child, S)) {
            CI->eraseFromParent();
            continue;
        }
        IRBuilder<> builder(CI);
        builder.SetCurrentDebugLocation(CI->getDebugLoc());
        auto parBits = builder.CreateAnd(EmitLoadTag(builder, parent), 3);
        auto parOldMarked = builder.CreateICmpEQ(parBits, ConstantInt::get(T_size, 3));
        auto mayTrigTerm = SplitBlockAndInsertIfThen(parOldMarked, CI, false);
        builder.SetInsertPoint(mayTrigTerm);
        auto chldBit = builder.CreateAnd(EmitLoadTag(builder, child), 1);
        auto chldNotMarked = builder.CreateICmpEQ(chldBit, ConstantInt::get(T_size, 0));
        MDBuilder MDB(parent->getContext());
        SmallVector<uint32_t, 2> Weights{1, 9};
        auto trigTerm = SplitBlockAndInsertIfThen(chldNotMarked, mayTrigTerm, false,
                                                  MDB.createBranchWeights(Weights));
        builder.SetInsertPoint(trigTerm);
        builder.CreateCall(queueroot_func, parent);
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
                                         Instruction *InsertionPoint) {
    Value *Val = GetPtrForNumber(S, R, InsertionPoint);
    Value *args[1] = {
        ConstantInt::get(T_int32, Colors[R]+MinColorRoot)
    };
    GetElementPtrInst *gep = GetElementPtrInst::Create(T_prjlvalue, GCFrame, makeArrayRef(args));
    gep->insertBefore(InsertionPoint);
    Val = MaybeExtractUnion(std::make_pair(Val, -1), InsertionPoint);
    // Pointee types don't have semantics, so the optimizer is
    // free to rewrite them if convenient. We need to change
    // it back here for the store.
    if (Val->getType() != T_prjlvalue)
        Val = new BitCastInst(Val, T_prjlvalue, "", InsertionPoint);
    new StoreInst(Val, gep, InsertionPoint);
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
    if (MaxColor != -1 || S.Allocas.size() != 0) {
        unsigned NRoots = MaxColor + 1 + S.Allocas.size();
        // Create GC Frame
        AllocaInst *gcframe = new AllocaInst(T_prjlvalue, 0,
            ConstantInt::get(T_int32, NRoots + 2), "gcframe");
        gcframe->insertBefore(&*F->getEntryBlock().begin());
        // Zero out gcframe
        BitCastInst *tempSlot_i8 = new BitCastInst(gcframe, Type::getInt8PtrTy(F->getContext()), "");
        tempSlot_i8->insertAfter(gcframe);
        Type *argsT[2] = {tempSlot_i8->getType(), T_int32};
        Function *memset = Intrinsic::getDeclaration(F->getParent(), Intrinsic::memset, makeArrayRef(argsT));
        Value *args[5] = {
            tempSlot_i8, // dest
            ConstantInt::get(Type::getInt8Ty(F->getContext()), 0), // val
            ConstantInt::get(T_int32, sizeof(jl_value_t*)*(NRoots+2)), // len
            ConstantInt::get(T_int32, 0), // align
            ConstantInt::get(Type::getInt1Ty(F->getContext()), 0)}; // volatile
        CallInst *zeroing = CallInst::Create(memset, makeArrayRef(args));
        zeroing->setMetadata(llvm::LLVMContext::MD_tbaa, tbaa_gcframe);
        zeroing->insertAfter(tempSlot_i8);
        // Push GC Frame
        PushGCFrame(gcframe, NRoots, ptlsStates);
        // Replace Allocas
        unsigned AllocaSlot = 2;
        for (AllocaInst *AI : S.Allocas) {
            Value *args[1] = {
                ConstantInt::get(T_int32, AllocaSlot++)
            };
            GetElementPtrInst *gep = GetElementPtrInst::Create(T_prjlvalue, gcframe, makeArrayRef(args));
            gep->insertAfter(gcframe);
            gep->takeName(AI);
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
            for (CallInst *II : ToDelete)
                II->eraseFromParent();
            AI->replaceAllUsesWith(gep);
            AI->eraseFromParent();
        }
        unsigned MinColorRoot = AllocaSlot;
        // Insert GC frame stores
        PlaceGCFrameStores(S, MinColorRoot, Colors, gcframe);
        // Insert GCFrame pops
        for(Function::iterator I = F->begin(), E = F->end(); I != E; ++I) {
            if (isa<ReturnInst>(I->getTerminator())) {
                PopGCFrame(gcframe, I->getTerminator());
            }
        }
    }
}

void LateLowerGCFrame::reinitFunctions(Module &M) {
    ptls_getter = M.getFunction("julia.ptls_states");
    gc_flush_func = M.getFunction("julia.gcroot_flush");
    gc_preserve_begin_func = M.getFunction("llvm.julia.gc_preserve_begin");
    gc_preserve_end_func = M.getFunction("llvm.julia.gc_preserve_end");
    pointer_from_objref_func = M.getFunction("julia.pointer_from_objref");
    typeof_func = M.getFunction("julia.typeof");
    write_barrier_func = M.getFunction("julia.write_barrier");
    alloc_obj_func = M.getFunction("julia.gc_alloc_obj");
}

bool LateLowerGCFrame::doInitialization(Module &M) {
    ptls_getter = M.getFunction("julia.ptls_states");
    auto &ctx = M.getContext();
    T_size = M.getDataLayout().getIntPtrType(ctx);
    T_int8 = Type::getInt8Ty(ctx);
    T_pint8 = PointerType::get(T_int8, 0);
    T_int32 = Type::getInt32Ty(ctx);
    if ((write_barrier_func = M.getFunction("julia.write_barrier"))) {
        T_prjlvalue = write_barrier_func->getFunctionType()->getParamType(0);
        if (!(queueroot_func = M.getFunction("jl_gc_queue_root"))) {
            queueroot_func = Function::Create(FunctionType::get(Type::getVoidTy(ctx),
                                                                {T_prjlvalue}, false),
                                              Function::ExternalLinkage, "jl_gc_queue_root", &M);
            queueroot_func->addFnAttr(Attribute::InaccessibleMemOrArgMemOnly);
        }
    }
    else {
        queueroot_func = nullptr;
    }
    pool_alloc_func = nullptr;
    big_alloc_func = nullptr;
    if ((alloc_obj_func = M.getFunction("julia.gc_alloc_obj"))) {
        T_prjlvalue = alloc_obj_func->getReturnType();
        if (!(pool_alloc_func = M.getFunction("jl_gc_pool_alloc"))) {
            std::vector<Type*> args(0);
            args.push_back(T_pint8);
            args.push_back(T_int32);
            args.push_back(T_int32);
            pool_alloc_func = Function::Create(FunctionType::get(T_prjlvalue, args, false),
                                               Function::ExternalLinkage, "jl_gc_pool_alloc", &M);
            pool_alloc_func->setAttributes(AttributeList::get(M.getContext(),
                alloc_obj_func->getAttributes().getFnAttributes(),
                alloc_obj_func->getAttributes().getRetAttributes(),
                None));
        }
        if (!(big_alloc_func = M.getFunction("jl_gc_big_alloc"))) {
            std::vector<Type*> args(0);
            args.push_back(T_pint8);
            args.push_back(T_size);
            big_alloc_func = Function::Create(FunctionType::get(T_prjlvalue, args, false),
                                         Function::ExternalLinkage, "jl_gc_big_alloc", &M);
            big_alloc_func->setAttributes(AttributeList::get(M.getContext(),
                alloc_obj_func->getAttributes().getFnAttributes(),
                alloc_obj_func->getAttributes().getRetAttributes(),
                None));
        }
        auto T_jlvalue = cast<PointerType>(T_prjlvalue)->getElementType();
        T_pjlvalue = PointerType::get(T_jlvalue, 0);
        T_ppjlvalue = PointerType::get(T_pjlvalue, 0);
        T_pjlvalue_der = PointerType::get(T_jlvalue, AddressSpace::Derived);
        T_ppjlvalue_der = PointerType::get(T_prjlvalue, AddressSpace::Derived);
    }
    else if (ptls_getter) {
        auto functype = ptls_getter->getFunctionType();
        T_ppjlvalue = cast<PointerType>(functype->getReturnType())->getElementType();
        T_pjlvalue = cast<PointerType>(T_ppjlvalue)->getElementType();
        auto T_jlvalue = cast<PointerType>(T_pjlvalue)->getElementType();
        T_prjlvalue = PointerType::get(T_jlvalue, AddressSpace::Tracked);
        T_pjlvalue_der = PointerType::get(T_jlvalue, AddressSpace::Derived);
        T_ppjlvalue_der = PointerType::get(T_prjlvalue, AddressSpace::Derived);
    }
    else {
        T_ppjlvalue = nullptr;
        T_prjlvalue = nullptr;
        T_pjlvalue = nullptr;
        T_pjlvalue_der = nullptr;
        T_ppjlvalue_der = nullptr;
    }
    GlobalValue *function_list[] = {queueroot_func, pool_alloc_func, big_alloc_func};
    unsigned j = 0;
    for (unsigned i = 0; i < sizeof(function_list) / sizeof(void*); i++) {
        if (!function_list[i])
            continue;
        if (i != j)
            function_list[j] = function_list[i];
        j++;
    }
    if (j != 0)
        appendToCompilerUsed(M, ArrayRef<GlobalValue*>(function_list, j));
    return true;
}

bool LateLowerGCFrame::doFinalization(Module &M)
{
    auto used = M.getGlobalVariable("llvm.compiler.used");
    if (!used)
        return false;
    GlobalValue *function_list[] = {queueroot_func, pool_alloc_func, big_alloc_func};
    SmallPtrSet<Constant*, 16> InitAsSet(function_list,
                                         function_list + sizeof(function_list) / sizeof(void*));
    bool changed = false;
    SmallVector<Constant*, 16> Init;
    ConstantArray *CA = dyn_cast<ConstantArray>(used->getInitializer());
    for (auto &Op : CA->operands()) {
        Constant *C = cast_or_null<Constant>(Op);
        if (InitAsSet.count(C->stripPointerCasts())) {
            changed = true;
            continue;
        }
        Init.push_back(C);
    }
    if (!changed)
        return false;
    used->eraseFromParent();
    if (Init.empty())
        return true;
    ArrayType *ATy = ArrayType::get(T_pint8, Init.size());
    used = new llvm::GlobalVariable(M, ATy, false, GlobalValue::AppendingLinkage,
                                    ConstantArray::get(ATy, Init), "llvm.compiler.used");
    used->setSection("llvm.metadata");
    return true;
}

bool LateLowerGCFrame::runOnFunction(Function &F) {
    DEBUG(dbgs() << "GC ROOT PLACEMENT: Processing function " << F.getName() << "\n");
    // Check availability of functions again since they might have been deleted.
    reinitFunctions(*F.getParent());
    if (!ptls_getter)
        return CleanupIR(F);
    ptlsStates = nullptr;
    for (auto I = F.getEntryBlock().begin(), E = F.getEntryBlock().end();
         ptls_getter && I != E; ++I) {
        if (CallInst *callInst = dyn_cast<CallInst>(&*I)) {
            if (callInst->getCalledValue() == ptls_getter) {
                ptlsStates = callInst;
                break;
            }
        }
    }
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

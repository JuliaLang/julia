// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-gc-interface-passes.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/Support/Casting.h"

#define DEBUG_TYPE "late_lower_gcroot"

static unsigned getValueAddrSpace(Value *V) {
    return V->getType()->getPointerAddressSpace();
}

static bool isTrackedValue(Value *V) {
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
CountTrackedPointers::CountTrackedPointers(Type *T, bool ignore_loaded) {
    if (isa<PointerType>(T)) {
        if (isSpecialPtr(T)) {
            if (ignore_loaded && T->getPointerAddressSpace() == AddressSpace::Loaded)
                return;
            count++;
            if (T->getPointerAddressSpace() != AddressSpace::Tracked)
                derived = true;
        }
    } else if (isa<StructType>(T) || isa<ArrayType>(T) || isa<VectorType>(T)) {
        for (Type *ElT : T->subtypes()) {
            auto sub = CountTrackedPointers(ElT, ignore_loaded);
            count += sub.count;
            all &= sub.all;
            derived |= sub.derived;
        }
        if (isa<ArrayType>(T))
            count *= cast<ArrayType>(T)->getNumElements();
        else if (isa<VectorType>(T)) {
            ElementCount EC = cast<VectorType>(T)->getElementCount();
            count *= EC.getKnownMinValue();
        }
    }
    if (count == 0)
        all = false;
}

bool hasLoadedTy(Type *T) {
    if (isa<PointerType>(T)) {
        if (T->getPointerAddressSpace() == AddressSpace::Loaded)
            return true;
    } else if (isa<StructType>(T) || isa<ArrayType>(T) || isa<VectorType>(T)) {
        for (Type *ElT : T->subtypes()) {
            if (hasLoadedTy(ElT))
                return true;
        }
    }
    return false;
}


unsigned getCompositeNumElements(Type *T) {
    if (auto *ST = dyn_cast<StructType>(T))
        return ST->getNumElements();
    else if (auto *AT = dyn_cast<ArrayType>(T))
        return AT->getNumElements();
    else {
        ElementCount EC = cast<VectorType>(T)->getElementCount();
        return EC.getKnownMinValue();
    }
}

// Walk through a Type, and record the element path to every tracked value inside
void TrackCompositeType(Type *T, SmallVector<unsigned, 0> &Idxs, SmallVector<SmallVector<unsigned, 0>, 0> &Numberings) {
    if (isa<PointerType>(T)) {
        if (isSpecialPtr(T))
            Numberings.push_back(Idxs);
    }
    else if (isa<StructType>(T) || isa<ArrayType>(T) || isa<VectorType>(T)) {
        unsigned Idx, NumEl = getCompositeNumElements(T);
        for (Idx = 0; Idx < NumEl; Idx++) {
            Idxs.push_back(Idx);
            Type *ElT = GetElementPtrInst::getTypeAtIndex(T, Idx);
            TrackCompositeType(ElT, Idxs, Numberings);
            Idxs.pop_back();
        }
    }
}

SmallVector<SmallVector<unsigned, 0>, 0> TrackCompositeType(Type *T) {
    SmallVector<unsigned, 0> Idxs;
    SmallVector<SmallVector<unsigned, 0>, 0> Numberings;
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
        } else if (auto *Freeze = dyn_cast<FreezeInst>(CurrentV)) {
            CurrentV = Freeze->getOperand(0); // Can be formed by optimizations, treat as a no-op
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
            if (hasLoadedTy(LI->getType())) {
                // This is the old (now deprecated) implementation for loaded.
                // New code should use the gc_loaded intrinsic to ensure that
                // the load is paired with the correct Tracked value.
                CurrentV = LI->getPointerOperand();
                fld_idx = -1;
                if (!isSpecialPtr(CurrentV->getType())) {
                    // This could really be anything, but it's not loaded
                    // from a tracked pointer, so it doesn't matter what
                    // it is--just pick something simple.
                    CurrentV = ConstantPointerNull::get(PointerType::get(V->getContext(), 0));
                }
                continue;
            }
            // In general a load terminates a walk
            break;
        }
        else if (auto LI = dyn_cast<AtomicCmpXchgInst>(CurrentV)) {
            // In general a load terminates a walk
            (void)LI;
            break;
        }
        else if (auto LI = dyn_cast<AtomicRMWInst>(CurrentV)) {
            // In general a load terminates a walk
            (void)LI;
            break;
        }
        else if (auto *II = dyn_cast<IntrinsicInst>(CurrentV)) {
            if (II->getIntrinsicID() == Intrinsic::masked_load ||
                II->getIntrinsicID() == Intrinsic::masked_gather) {
                // Some intrinsics behave like LoadInst followed by a SelectInst
                // This should never happen in a derived addrspace (since those cannot be stored to memory)
                // so we don't need to lift these operations, but we do need to check if it's loaded and continue walking the base pointer
                if (auto VTy = dyn_cast<VectorType>(II->getType())) {
                    if (hasLoadedTy(VTy->getElementType())) {
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
                                CurrentV = ConstantPointerNull::get(PointerType::get(V->getContext(), 0));
                            }
                        } else {
                            if (auto VTy2 = dyn_cast<VectorType>(CurrentV->getType())) {
                                if (!isSpecialPtr(VTy2->getElementType())) {
                                    CurrentV = ConstantPointerNull::get(PointerType::get(V->getContext(), 0));
                                    fld_idx = -1;
                                }
                            }
                        }
                        continue;
                    }
                }
                // In general a load terminates a walk
                break;
            }
            else if (II->getIntrinsicID() == Intrinsic::vector_extract) {
                if (auto VTy = dyn_cast<VectorType>(II->getType())) {
                    if (hasLoadedTy(VTy->getElementType())) {
                        Value *Idx = II->getOperand(1);
                        if (!isa<ConstantInt>(Idx)) {
                            assert(isa<UndefValue>(Idx) && "unimplemented");
                            (void)Idx;
                        }
                        CurrentV = II->getOperand(0);
                        fld_idx = -1;
                        continue;
                    }
                }
                break;
            } else {
                // Unknown Intrinsic
                break;
            }
        }
        else if (auto CI = dyn_cast<CallInst>(CurrentV)) {
            auto callee = CI->getCalledFunction();
            if (callee && callee->getName() == "julia.gc_loaded") {
                CurrentV = CI->getArgOperand(0);
                continue;
            }
            // Unknown Call
            break;
        }
        else {
            // Unknown Instruction
            break;
        }
    }
    assert(isa<LoadInst>(CurrentV) || isa<CallInst>(CurrentV) ||
           isa<AtomicCmpXchgInst>(CurrentV) || isa<AtomicRMWInst>(CurrentV) ||
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
        auto Idxs = ArrayRef<unsigned>(Tracked[ValExpr.second]);
        auto IdxsNotVec = Idxs.slice(0, Idxs.size() - 1);
        Type *FinalT = ExtractValueInst::getIndexedType(V->getType(), IdxsNotVec);
        bool IsVector = isa<VectorType>(FinalT);
        PointerType *T = cast<PointerType>(
            GetElementPtrInst::getTypeAtIndex(FinalT, Idxs.back()));
        if (T->getAddressSpace() != AddressSpace::Tracked) {
            // if V isn't tracked, get the shadow def
            auto Numbers = NumberAllBase(S, V);
            int BaseNumber = Numbers[ValExpr.second];
            if (BaseNumber >= 0)
                V = GetPtrForNumber(S, BaseNumber, InsertBefore);
            else
                V = ConstantPointerNull::get(cast<PointerType>(T_prjlvalue));
            return V;
        }
        IRBuilder<InstSimplifyFolder> foldbuilder(InsertBefore->getContext(), InstSimplifyFolder(InsertBefore->getModule()->getDataLayout()));
        foldbuilder.SetInsertPoint(InsertBefore);
        if (Idxs.size() > IsVector)
            V = foldbuilder.CreateExtractValue(V, IsVector ? IdxsNotVec : Idxs);
        if (IsVector)
            V = foldbuilder.CreateExtractElement(V, ConstantInt::get(Type::getInt32Ty(V->getContext()), Idxs.back()));
    }
    return V;
}

SmallVector<Value*, 0> LateLowerGCFrame::MaybeExtractVector(State &S, Value *BaseVec, Instruction *InsertBefore) {
    auto Numbers = NumberAllBase(S, BaseVec);
    SmallVector<Value*, 0> V{Numbers.size()};
    Value *V_rnull = ConstantPointerNull::get(cast<PointerType>(T_prjlvalue));
    for (unsigned i = 0; i < V.size(); ++i) {
        if (Numbers[i] >= 0) // ignores undef and poison values
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
        const SmallVector<int, 0> &AllNums = S.AllCompositeNumbering[Val];
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
    assert(!isTrackedValue(SI));
    SmallVector<int, 0> Numbers;
    unsigned NumRoots = 1;
    Type *STy = SI->getType();
    if (!isa<PointerType>(STy))
        Numbers.resize(CountTrackedPointers(STy).count, -1);
    // find the base root for the arguments
    Value *TrueBase = MaybeExtractScalar(S, FindBaseValue(S, SI->getTrueValue(), false), SI);
    Value *FalseBase = MaybeExtractScalar(S, FindBaseValue(S, SI->getFalseValue(), false), SI);
    SmallVector<Value*, 0> TrueBases;
    SmallVector<Value*, 0> FalseBases;
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
#if JL_LLVM_VERSION >= 200000
                    "", SI->getIterator());
#else
                    "", SI);
#endif
        }
        assert(FalseElem->getType() == TrueElem->getType());
#if JL_LLVM_VERSION >= 200000
        SelectInst *SelectBase = SelectInst::Create(Cond, TrueElem, FalseElem, "gclift", SI->getIterator());
#else
        SelectInst *SelectBase = SelectInst::Create(Cond, TrueElem, FalseElem, "gclift", SI);
#endif
        int Number = ++S.MaxPtrNumber;
        S.AllPtrNumbering[SelectBase] = Number;
        S.ReversePtrNumbering[Number] = SelectBase;
        if (isa<PointerType>(SI->getType()))
            S.AllPtrNumbering[SI] = Number;
        else
            Numbers[i] = Number;
    }
    if (auto VTy = dyn_cast<FixedVectorType>(SI->getType())) {
        if (NumRoots != Numbers.size()) {
            // broadcast the scalar root number to fill the vector
            assert(NumRoots == 1);
            int Number = Numbers[0];
            Numbers.resize(0);
            ElementCount EC = VTy->getElementCount();
            Numbers.resize(EC.getKnownMinValue(), Number);
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
    SmallVector<int, 0> Numbers;
    unsigned NumRoots = 1;
    Type *PTy = Phi->getType();
    if (!isa<PointerType>(PTy)) {
        NumRoots = CountTrackedPointers(PTy).count;
        Numbers.resize(NumRoots);
    }
    for (unsigned i = 0; i < NumRoots; ++i) {
#if JL_LLVM_VERSION >= 200000
        PHINode *lift = PHINode::Create(T_prjlvalue, Phi->getNumIncomingValues(), "gclift", Phi->getIterator());
#else
        PHINode *lift = PHINode::Create(T_prjlvalue, Phi->getNumIncomingValues(), "gclift", Phi);
#endif
        int Number = ++S.MaxPtrNumber;
        S.AllPtrNumbering[lift] = Number;
        S.ReversePtrNumbering[Number] = lift;
        if (isa<PointerType>(PTy))
            S.AllPtrNumbering[Phi] = Number;
        else
            Numbers[i] = Number;
        lifted.push_back(lift);
    }
    if (!isa<PointerType>(Phi->getType()))
        S.AllCompositeNumbering[Phi] = Numbers;
    SmallVector<DenseMap<Value*, Value*>, 4> CastedRoots(NumRoots);
    for (unsigned i = 0; i < Phi->getNumIncomingValues(); ++i) {
        Value *Incoming = Phi->getIncomingValue(i);
        BasicBlock *IncomingBB = Phi->getIncomingBlock(i);
        Instruction *Terminator = IncomingBB->getTerminator();
        Value *Base = MaybeExtractScalar(S, FindBaseValue(S, Incoming, false), Terminator);
        SmallVector<Value*, 0> IncomingBases;
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
            assert(BaseElem->getType() == T_prjlvalue);
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
        Number = S.AllPtrNumbering[CurrentV];
        return Number;
    } else if (isa<PHINode>(CurrentV) && !isTrackedValue(CurrentV)) {
        LiftPhi(S, cast<PHINode>(CurrentV));
        Number = S.AllPtrNumbering[CurrentV];
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
        Number = Numbers[CurrentV.second];
    }
    if (V != CurrentV.first)
        S.AllPtrNumbering[V] = Number;
    return Number;
}

// assign pointer numbers to a def instruction
SmallVector<int, 0> LateLowerGCFrame::NumberAllBase(State &S, Value *CurrentV) {
    if (isa<PointerType>(CurrentV->getType())) {
        auto it = S.AllPtrNumbering.find(CurrentV);
        if (it != S.AllPtrNumbering.end())
            return SmallVector<int, 0>({it->second});
    } else {
        auto it = S.AllCompositeNumbering.find(CurrentV);
        if (it != S.AllCompositeNumbering.end())
            return it->second;
    }

    SmallVector<int, 0> Numbers;
    auto tracked = CountTrackedPointers(CurrentV->getType());
    if (tracked.count == 0)
        return Numbers;
    if (isa<Constant>(CurrentV) || isa<AllocaInst>(CurrentV) || isa<Argument>(CurrentV) ||
            (isa<AddrSpaceCastInst>(CurrentV) && !isTrackedValue(CurrentV))) {
        Numbers.resize(tracked.count, -1);
    }
    else if (auto *SVI = dyn_cast<ShuffleVectorInst>(CurrentV)) {
        SmallVector<int, 0> Numbers1 = NumberAll(S, SVI->getOperand(0));
        SmallVector<int, 0> Numbers2 = NumberAll(S, SVI->getOperand(1));
        auto Mask = SVI->getShuffleMask();
        for (auto idx : Mask) {
            if (idx == -1) {
                Numbers.push_back(-1);
            } else if ((unsigned)idx < Numbers1.size()) {
                Numbers.push_back(Numbers1[idx]);
            } else {
                Numbers.push_back(Numbers2[idx - Numbers1.size()]);
            }
        }
    } else if (auto *IEI = dyn_cast<InsertElementInst>(CurrentV)) {
        // TODO: handle non-constant: LiftInsertElement(S, IEI)
        unsigned idx = cast<ConstantInt>(IEI->getOperand(2))->getZExtValue();
        Numbers = NumberAll(S, IEI->getOperand(0));
        int ElNumber = Number(S, IEI->getOperand(1));
        Numbers[idx] = ElNumber;
    // C++17
    // } else if (auto *II = dyn_cast<IntrinsicInst>(CurrentV); II && II->getIntrinsicID() == Intrinsic::vector_insert) {
    } else if (isa<IntrinsicInst>(CurrentV) && cast<IntrinsicInst>(CurrentV)->getIntrinsicID() == Intrinsic::vector_insert) {
        auto *II = dyn_cast<IntrinsicInst>(CurrentV);
        // Vector insert is a bit like a shuffle so use the same approach
        SmallVector<int, 0> Numbers1 = NumberAll(S, II->getOperand(0));
        SmallVector<int, 0> Numbers2 = NumberAll(S, II->getOperand(1));
        unsigned first_idx = cast<ConstantInt>(II->getOperand(2))->getZExtValue();
        for (unsigned i = 0; i < Numbers1.size(); ++i) {
            if (i < first_idx)
                Numbers.push_back(Numbers1[i]);
            else if (i - first_idx < Numbers2.size())
                Numbers.push_back(Numbers2[i - first_idx]);
            else
                Numbers.push_back(Numbers1[i]);
        }
    } else if (auto *IVI = dyn_cast<InsertValueInst>(CurrentV)) {
        Numbers = NumberAll(S, IVI->getAggregateOperand());
        auto Tracked = TrackCompositeType(IVI->getType());
        assert(Tracked.size() == Numbers.size());
        SmallVector<int, 0> InsertNumbers = NumberAll(S, IVI->getInsertedValueOperand());
        auto Idxs = IVI->getIndices();
        unsigned j = 0;
        for (unsigned i = 0; i < Tracked.size(); ++i) {
            auto Elem = ArrayRef<unsigned>(Tracked[i]);
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
            auto Elem = ArrayRef<unsigned>(Tracked[i]);
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
            auto Number = S.AllPtrNumbering[CurrentV];
            Numbers.resize(1, Number);
        } else {
            Numbers = S.AllCompositeNumbering[CurrentV];
        }
    } else {
        assert((isa<LoadInst>(CurrentV) || isa<CallInst>(CurrentV) || isa<PHINode>(CurrentV) || isa<SelectInst>(CurrentV) ||
                isa<AtomicCmpXchgInst>(CurrentV) || isa<AtomicRMWInst>(CurrentV))
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
SmallVector<int, 0> LateLowerGCFrame::NumberAll(State &S, Value *V) {
    if (isa<PointerType>(V->getType())) {
        auto it = S.AllPtrNumbering.find(V);
        if (it != S.AllPtrNumbering.end())
            return SmallVector<int, 0>({it->second});
    } else {
        auto it = S.AllCompositeNumbering.find(V);
        if (it != S.AllCompositeNumbering.end())
            return it->second;
    }
    SmallVector<int, 0> Numbers;
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
        else {
            assert(!isa<PointerType>(V->getType()));
        }
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


static bool HasBitSet(const LargeSparseBitVector &BV, unsigned Bit) {
    return BV.test(Bit);
}

static bool HasBitSet(const BitVector &BV, unsigned Bit) {
    return Bit < BV.size() && BV[Bit];
}

static void NoteDef(State &S, BBState &BBS, int Num) {
    assert(Num >= 0);
    assert(!BBS.Defs.test(Num) && "SSA Violation or misnumbering?");
    BBS.Defs.set(Num);
    BBS.UpExposedUses.reset(Num);
    // This value could potentially be live at any following safe point
    // if it ends up live out, so add it to the LiveIfLiveOut lists for all
    // following safepoints.
    if (BBS.HasSafepoint)
        for (int Safepoint = BBS.FirstSafepoint; Safepoint >= BBS.LastSafepoint; --Safepoint)
            S.LiveIfLiveOut[Safepoint].push_back(Num);
}

bool LateLowerGCFrame::MaybeNoteDef(State &S, BBState &BBS, Value *Def,
                                    SmallVector<int, 1> &&RefinedPtr) {
    Type *RT = Def->getType();
    if (isa<PointerType>(RT)) {
        if (!isSpecialPtr(RT))
            return false;
        assert(isTrackedValue(Def) && "Returned value of GC interest, but not tracked?");
        int Num = Number(S, Def);
        NoteDef(S, BBS, Num);
        if (!RefinedPtr.empty())
            S.Refinements[Num] = std::move(RefinedPtr);
        return true;
    }
    else {
        SmallVector<int, 0> Nums = NumberAll(S, Def);
        for (int Num : Nums) {
            NoteDef(S, BBS, Num);
            if (!RefinedPtr.empty())
                S.Refinements[Num] = RefinedPtr;
        }
        return !Nums.empty();
    }
}

static int NoteSafepoint(State &S, BBState &BBS, CallInst *CI, SmallVectorImpl<int> &CalleeRoots) {
    assert(BBS.FirstSafepoint == -1 || BBS.FirstSafepoint == S.MaxSafepointNumber);
    int Number = ++S.MaxSafepointNumber;
    S.SafepointNumbering.push_back(CI);
    // Note which pointers are upward exposed live here. They need to be
    // considered live at this safepoint even when they have a def earlier
    // in this BB (i.e. even when they don't participate in the dataflow
    // computation)
    S.LiveSets.push_back(BBS.UpExposedUses);
    S.LiveIfLiveOut.push_back(SmallVector<int, 0>{});
    S.CalleeRoots.push_back(std::move(CalleeRoots));
    BBS.HasSafepoint = true;
    if (BBS.LastSafepoint == -1)
        BBS.LastSafepoint = Number;
    BBS.FirstSafepoint = Number;
    return Number;
}

void LateLowerGCFrame::NoteUse(State &S, BBState &BBS, Value *V, LargeSparseBitVector &Uses, Function &F) {
    // Short circuit to avoid having to deal with vectors of constants, etc.
//#ifndef NDEBUG
//    if (isa<PointerType>(V->getType())) {
//        if (isSpecialPtr(V->getType()))
//            if (isa<UndefValue>(V) && !isa<PoisonValue>(V))
//                F.dump();
//    }
//#endif
    if (isa<Constant>(V))
        return;
    if (isa<PointerType>(V->getType())) {
        if (isSpecialPtr(V->getType())) {
            int Num = Number(S, V);
            if (Num < 0)
                return;
            Uses.set(Num);
        }
    } else {
        SmallVector<int, 0> Nums = NumberAll(S, V);
        for (int Num : Nums) {
            if (Num < 0)
                continue;
            Uses.set(Num);
        }
    }
}

void LateLowerGCFrame::NoteOperandUses(State &S, BBState &BBS, Instruction &UI) {
    for (Use &U : UI.operands()) {
        NoteUse(S, BBS, U, *UI.getFunction());
    }
}

template <typename VisitInst, typename callback>
void RecursivelyVisit(callback f, Value *V) {
    for (Use &VU : V->uses()) {
        User *TheUser = VU.getUser();
        if (isa<VisitInst>(TheUser))
            f(VU);
        if (isa<CallInst>(TheUser) || isa<LoadInst>(TheUser) ||
            isa<SelectInst>(TheUser) || isa<PHINode>(TheUser) || // TODO: should these be removed from this list?
            isa<StoreInst>(TheUser) || isa<PtrToIntInst>(TheUser) ||
            isa<ICmpInst>(TheUser) || isa<InsertElementInst>(TheUser)|| // ICmpEQ/ICmpNE can be used with ptr types
            isa<AtomicCmpXchgInst>(TheUser) || isa<AtomicRMWInst>(TheUser))
            continue;
        if (isa<GetElementPtrInst>(TheUser) || isa<BitCastInst>(TheUser) || isa<AddrSpaceCastInst>(TheUser)) {
            RecursivelyVisit<VisitInst, callback>(f, TheUser);
            continue;
        }
        llvm_dump(V);
        llvm_dump(TheUser);
        errs() << "Unexpected instruction\n";
        abort();
    }
}

static void dumpBitVectorValues(State &S, LargeSparseBitVector &BV, ModuleSlotTracker &MST) {
    bool first = true;
    for (auto Idx : BV) {
        if (!first)
            dbgs() << ", ";
        first = false;
        S.ReversePtrNumbering[Idx]->printAsOperand(dbgs(), false, MST);
    }
}

static void dumpBBState(const BasicBlock &BB, State &S, ModuleSlotTracker &MST)
{
    dbgs() << "Liveness analysis for BB " << BB.getName();
    dbgs() << "\n\tDefs: ";
    dumpBitVectorValues(S, S.BBStates[&BB].Defs, MST);
    dbgs() << "\n\tPhiOuts: ";
    dumpBitVectorValues(S, S.BBStates[&BB].PhiOuts, MST);
    dbgs() << "\n\tUpExposedUses: ";
    dumpBitVectorValues(S, S.BBStates[&BB].UpExposedUses, MST);
    dbgs() << "\n\tLiveIn: ";
    dumpBitVectorValues(S, S.BBStates[&BB].LiveIn, MST);
    dbgs() << "\n\tLiveOut: ";
    dumpBitVectorValues(S, S.BBStates[&BB].LiveOut, MST);
    dbgs() << "\n";
}

JL_USED_FUNC static void dumpBBState(const BasicBlock &BB, State &S)
{
    ModuleSlotTracker MST(BB.getParent()->getParent());
    dumpBBState(BB, S, MST);
}


/* Debugging utility to dump liveness information */
JL_USED_FUNC static void dumpLivenessState(Function &F, State &S) {
    ModuleSlotTracker MST(F.getParent());
    for (auto &BB : F) {
        return dumpBBState(BB, S, MST);
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
    if (isTBAA(TBAA, {"jtbaa_immut", "jtbaa_const", "jtbaa_datatype", "jtbaa_memoryptr", "jtbaa_memorylen", "jtbaa_memoryown"}))
        return true;
    return false;
}

static bool isConstGV(GlobalVariable *gv)
{
    return gv->isConstant() || gv->getMetadata("julia.constgv");
}

typedef llvm::SmallPtrSet<PHINode*, 1> PhiSet;

static bool isLoadFromConstGV(LoadInst *LI, bool &task_local, PhiSet *seen = nullptr);
static bool isLoadFromConstGV(Value *v, bool &task_local, PhiSet *seen = nullptr)
{
    v = v->stripInBoundsOffsets();
    if (auto LI = dyn_cast<LoadInst>(v))
        return isLoadFromConstGV(LI, task_local, seen);
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
        return (isLoadFromConstGV(SL->getTrueValue(), task_local, seen) &&
                isLoadFromConstGV(SL->getFalseValue(), task_local, seen));
    if (auto Phi = dyn_cast<PHINode>(v)) {
        PhiSet ThisSet(&Phi, &Phi);
        if (!seen)
            seen = &ThisSet;
        else if (seen->count(Phi))
            return true;
        else
            seen->insert(Phi);
        auto n = Phi->getNumIncomingValues();
        for (unsigned i = 0; i < n; ++i) {
            if (!isLoadFromConstGV(Phi->getIncomingValue(i), task_local, seen)) {
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
        if (callee && callee->getName() == "julia.get_pgcstack") {
            task_local = true;
            return true;
        }
        if (callee && callee->getName() == "julia.gc_loaded") {
            return isLoadFromConstGV(call->getArgOperand(0), task_local, seen) &&
                   isLoadFromConstGV(call->getArgOperand(1), task_local, seen);
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
static bool isLoadFromConstGV(LoadInst *LI, bool &task_local, PhiSet *seen)
{
    // We only emit single slot GV in codegen
    // but LLVM global merging can change the pointer operands to GEPs/bitcasts
    auto load_base = LI->getPointerOperand()->stripInBoundsOffsets();
    assert(load_base); // Static analyzer
    auto gv = dyn_cast<GlobalVariable>(load_base);
    if (isLoadFromImmut(LI)) {
        if (gv)
            return true;
        return isLoadFromConstGV(load_base, task_local, seen);
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
    //     - Has no refinements (all obviously externally rooted values are annotated by -1/-2 in the
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
                    S.DT = &GetDT();
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
        } else {
            S.Refinements.erase(Num);
        }
        visited.reset();
    }
}

// Look through instructions to find all possible allocas that might become the sret argument
static SmallSetVector<AllocaInst *, 1> FindSretAllocas(Value* SRetArg) {
    SmallSetVector<AllocaInst *, 1> allocas;
    if (AllocaInst *OneSRet = dyn_cast<AllocaInst>(SRetArg)) {
        allocas.insert(OneSRet); // Found it directly
    }
    else {
        SmallSetVector<Value *, 8> worklist;
        worklist.insert(SRetArg);
        while (!worklist.empty()) {
            Value *V = worklist.pop_back_val();
            if (AllocaInst *Alloca = dyn_cast<AllocaInst>(V->stripInBoundsOffsets())) {
                allocas.insert(Alloca); // Found a candidate
            }
            else if (PHINode *Phi = dyn_cast<PHINode>(V)) {
                for (Value *Incoming : Phi->incoming_values()) {
                    worklist.insert(Incoming);
                }
            }
            else if (SelectInst *SI = dyn_cast<SelectInst>(V)) {
                auto TrueBranch = SI->getTrueValue();
                auto FalseBranch = SI->getFalseValue();
                if (TrueBranch && FalseBranch) {
                    worklist.insert(TrueBranch);
                    worklist.insert(FalseBranch);
                }
                else {
                    llvm_dump(SI);
                    dbgs() << "Malformed Select\n";
                    allocas.clear();
                    return allocas;
                }
            }
            else {
                llvm_dump(V);
                dbgs() << "Unexpected SRet argument\n";
                allocas.clear();
                return allocas;
            }
        }
    }
    assert(std::all_of(allocas.begin(), allocas.end(), [&] (AllocaInst* SRetAlloca) JL_NOTSAFEPOINT {
            return (SRetAlloca->getArraySize() == allocas[0]->getArraySize() &&
                SRetAlloca->getAllocatedType() == allocas[0]->getAllocatedType());
        }
    ));
    return allocas;
}

State LateLowerGCFrame::LocalScan(Function &F) {
    State S(F);
    SmallVector<int, 8> PHINumbers;
    for (BasicBlock &BB : F) {
        BBState &BBS = S.BBStates[&BB];
        // Avoid tracking safepoints until we reach the first instruction the defines a value.
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
                            if (CountTrackedPointers(VTy->getElementType()).count) {
                                // LLVM sometimes tries to materialize these operations with undefined pointers in our non-integral address space.
                                // Hopefully LLVM didn't already propagate that information and poison our users. Set those to NULL now.
                                Value *passthru = II->getArgOperand(3);
                                if (isa<UndefValue>(passthru)) {
                                    II->setArgOperand(3, Constant::getNullValue(passthru->getType()));
                                }
                            }
                            if (hasLoadedTy(VTy->getElementType())) {
                                // These are not real defs
                                continue;
                            }
                        }
                    }
                    if (II->getIntrinsicID() == Intrinsic::vector_extract || II->getIntrinsicID() == Intrinsic::vector_insert) {
                        // These are not real defs
                        continue;
                    }
                }
                auto callee = CI->getCalledFunction();
                if (callee && callee == typeof_func) {
                    MaybeNoteDef(S, BBS, CI, SmallVector<int, 1>{-2});
                }
                else if (callee && callee->getName() == "julia.gc_loaded") {
                    continue;
                }
                else {
                    if (MaybeNoteDef(S, BBS, CI))
                        BBS.FirstSafepointAfterFirstDef = BBS.FirstSafepoint;
                }
                bool HasDefBefore = false;
                // Loop over all arguments to find those with "julia.return_roots" attribute
                AttributeList Attrs = CI->getAttributes();
                for (unsigned i = 0; i < CI->arg_size(); ++i) {
                    Attribute RetRootsAttr = Attrs.getParamAttr(i, "julia.return_roots");
                    if (RetRootsAttr.isValid()) {
                        size_t return_roots = atol(RetRootsAttr.getValueAsString().data());
                        assert(return_roots);
                        auto gc_allocas = FindSretAllocas(CI->getArgOperand(i)->stripInBoundsOffsets());
                        // We know that with the right optimizations we can forward a sret directly from an argument
                        // This hasn't been seen without adding IPO effects to julia functions but it's possible we need to handle that too
                        if (gc_allocas.size() == 0) {
                            llvm_dump(&F);
                            abort();
                        }
                        for (AllocaInst *SRet_gc : gc_allocas) {
                            Type *ElT = SRet_gc->getAllocatedType();
                            if (!(SRet_gc->isStaticAlloca() && isa<PointerType>(ElT) && ElT->getPointerAddressSpace() == AddressSpace::Tracked)) {
                                // this is a umax operation since we sometimes see the calls cloned, although we don't see these reused for different sizes
                                auto &live_roots = S.ArrayAllocas[SRet_gc];
                                if (live_roots < return_roots)
                                    live_roots = return_roots;
                            }
                        }
                    }
                }
                NoteOperandUses(S, BBS, I);
                if (!CI->canReturnTwice()) {
                    if (callee) {
                        if (callee == gc_preserve_begin_func) {
                            SmallVector<int, 0> args;
                            for (Use &U : CI->args()) {
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
                                    SmallVector<int, 0> Nums = NumberAll(S, V);
                                    for (int Num : Nums) {
                                        if (Num < 0)
                                            continue;
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
                            callee == pgcstack_getter || callee->getName() == XSTR(jl_egal__unboxed) ||
                            callee->getName() == XSTR(jl_lock_value) || callee->getName() == XSTR(jl_unlock_value) ||
                            callee->getName() == XSTR(jl_lock_field) || callee->getName() == XSTR(jl_unlock_field) ||
                            callee == write_barrier_func || callee == gc_loaded_func || callee == pop_handler_noexcept_func ||
                            callee->getName() == "memcmp") {
                            continue;
                        }
                        if (callee->getMemoryEffects().onlyReadsMemory() ||
                            callee->getMemoryEffects().onlyAccessesArgPointees()) {
                            continue;
                        }
                    }
                    if (isa<IntrinsicInst>(CI))
                        // Intrinsics are never safepoints.
                        continue;
                    auto effects = CI->getMemoryEffects();
                    if (effects.onlyAccessesArgPointees() || effects.onlyReadsMemory())
                        // Readonly functions and functions that cannot change GC state (which is inaccessiblemem) are not safepoints
                        continue;
                }
                SmallVector<int, 0> CalleeRoots;
                for (Use &U : CI->args()) {
                    // Find all callee rooted arguments.
                    // Record them instead of simply remove them from live values here
                    // since they can be useful during refinement
                    // (e.g. to remove roots of objects that are refined to these)
                    Value *V = U;
                    if (isa<Constant>(V) || !isa<PointerType>(V->getType()) ||
                        getValueAddrSpace(V) != AddressSpace::CalleeRooted)
                        continue;
                    V = V->stripPointerCasts();
                    if (!isTrackedValue(V))
                        continue;
                    auto Num = Number(S, V);
                    if (Num < 0)
                        continue;
                    CalleeRoots.push_back(Num);
                }
                int SafepointNumber = NoteSafepoint(S, BBS, CI, CalleeRoots);
                if (CI->canReturnTwice()) {
                    S.ReturnsTwice.push_back(SafepointNumber);
                    HasDefBefore = true;
                }
                if (HasDefBefore) // With sret, the Def happens before the instruction instead of after
                    BBS.FirstSafepointAfterFirstDef = SafepointNumber;
                continue;
            }
            if (LoadInst *LI = dyn_cast<LoadInst>(&I)) {
                // If this is a load from an immutable, we know that
                // this object will always be rooted as long as the
                // object we're loading from is, so we can refine uses
                // of this object to uses of the object we're loading
                // from.
                SmallVector<int, 1> RefinedPtr{};
                Type *Ty = LI->getType()->getScalarType();
                bool refined_globally = false;
                bool task_local = false;
                if (isLoadFromImmut(LI) && isSpecialPtr(LI->getPointerOperand()->getType())) {
                    RefinedPtr.push_back(Number(S, LI->getPointerOperand()));
                }
                else if (isLoadFromConstGV(LI, task_local)) {
                    // If this is a const load from a global,
                    // we know that the object is a constant as well and doesn't need rooting.
                    // If this is a task local constant, we don't need to root it within the
                    // task but we do need to issue write barriers for when the current task dies.
                    RefinedPtr.push_back(task_local ? -1 : -2);
                    refined_globally = true;
                }
                if (!hasLoadedTy(Ty))
                    if (MaybeNoteDef(S, BBS, LI, std::move(RefinedPtr)))
                        if (!refined_globally)
                            BBS.FirstSafepointAfterFirstDef = BBS.FirstSafepoint;
                NoteOperandUses(S, BBS, I);
            } else if (auto *LI = dyn_cast<AtomicCmpXchgInst>(&I)) {
                Type *Ty = LI->getNewValOperand()->getType()->getScalarType();
                if (!Ty->isPointerTy() || Ty->getPointerAddressSpace() != AddressSpace::Loaded) {
                    if (MaybeNoteDef(S, BBS, LI))
                        BBS.FirstSafepointAfterFirstDef = BBS.FirstSafepoint;
                }
                NoteOperandUses(S, BBS, I);
                // TODO: do we need MaybeTrackStore(S, LI);
            } else if (auto *LI = dyn_cast<AtomicRMWInst>(&I)) {
                Type *Ty = LI->getType()->getScalarType();
                if (!Ty->isPointerTy() || Ty->getPointerAddressSpace() != AddressSpace::Loaded) {
                    if (MaybeNoteDef(S, BBS, LI))
                        BBS.FirstSafepointAfterFirstDef = BBS.FirstSafepoint;
                }
                NoteOperandUses(S, BBS, I);
                // TODO: do we need MaybeTrackStore(S, LI);
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
                    MaybeNoteDef(S, BBS, SI, std::move(RefinedPtr));
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
                    MaybeNoteDef(S, BBS, Phi, std::move(PHIRefinements));
                    if (isa<PointerType>(Phi->getType())) {
                        PHINumbers.push_back(Number(S, Phi));
                    } else {
                        SmallVector<int, 0> Nums = NumberAll(S, Phi);
                        for (int Num : Nums)
                            PHINumbers.push_back(Num);
                    }
                    unsigned nIncoming = Phi->getNumIncomingValues();
                    for (unsigned i = 0; i < nIncoming; ++i) {
                        BBState &IncomingBBS = S.BBStates[Phi->getIncomingBlock(i)];
                        NoteUse(S, IncomingBBS, Phi->getIncomingValue(i), IncomingBBS.PhiOuts, F);
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
                    MaybeNoteDef(S, BBS, ASCI, std::move(RefinedPtr));
                }
            } else if (auto *AI = dyn_cast<AllocaInst>(&I)) {
                Type *ElT = AI->getAllocatedType();
                if (AI->isStaticAlloca() && isa<PointerType>(ElT) && ElT->getPointerAddressSpace() == AddressSpace::Tracked) {
                    S.ArrayAllocas[AI] = cast<ConstantInt>(AI->getArraySize())->getZExtValue();
                }
            }
        }
        // Pre-seed the dataflow variables;
        BBS.LiveIn = BBS.UpExposedUses;
    }
    FixUpRefinements(PHINumbers, S);
    return S;
}



static Value *ExtractScalar(Value *V, Type *VTy, bool isptr, ArrayRef<unsigned> Idxs, IRBuilder<> &irbuilder) {
    Type *T_int32 = Type::getInt32Ty(V->getContext());
    if (isptr) {
        SmallVector<Value*, 0> IdxList{Idxs.size() + 1};
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
        IRBuilder<InstSimplifyFolder> foldbuilder(irbuilder.getContext(), InstSimplifyFolder(irbuilder.GetInsertBlock()->getModule()->getDataLayout()));
        foldbuilder.restoreIP(irbuilder.saveIP());
        foldbuilder.SetCurrentDebugLocation(irbuilder.getCurrentDebugLocation());
        if (Idxs.size() > IsVector)
            V = foldbuilder.CreateExtractValue(V, IsVector ? IdxsNotVec : Idxs);
        if (IsVector)
            V = foldbuilder.CreateExtractElement(V, ConstantInt::get(Type::getInt32Ty(V->getContext()), Idxs.back()));
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

SmallVector<Value*, 0> ExtractTrackedValues(Value *Src, Type *STy, bool isptr, IRBuilder<> &irbuilder, ArrayRef<unsigned> perm_offsets) {
    auto Tracked = TrackCompositeType(STy);
    SmallVector<Value*, 0> Ptrs;
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
        auto Idxs = ArrayRef<unsigned>(Tracked[i]);
        if (ignore_field(Idxs))
            continue;
        Value *Elem = ExtractScalar(Src, STy, isptr, Idxs, irbuilder);
        if (isTrackedValue(Elem)) // ignore addrspace Loaded when it appears
            Ptrs.push_back(Elem);
    }
    return Ptrs;
}

//static unsigned TrackWithShadow(Value *Src, Type *STy, bool isptr, Value *Dst, IRBuilder<> &irbuilder) {
//    auto Ptrs = ExtractTrackedValues(Src, STy, isptr, irbuilder);
//    for (unsigned i = 0; i < Ptrs.size(); ++i) {
//        Value *Elem = Ptrs[i];
//        Value *Slot = irbuilder.CreateConstInBoundsGEP1_32(irbuilder.getInt8Ty(), Dst, i * sizeof(void*));
//        StoreInst *shadowStore = irbuilder.CreateAlignedStore(Elem, Slot, Align(sizeof(void*)));
//        shadowStore->setOrdering(AtomicOrdering::NotAtomic);
//        // TODO: shadowStore->setMetadata(LLVMContext::MD_tbaa, tbaa_gcframe);
//    }
//    return Ptrs.size();
//}

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
    //    Shadow = new AllocaInst(ArrayType::get(T_prjlvalue, tracked.count), 0, "", MI);
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
    /* Liveness is a reverse problem, so post-order is a good way to perform this iteration. */
    LargeSparseBitVector NewLive;
    while (!Converged) {
        bool AnyChanged = false;
        for (BasicBlock *BB : post_order(S.F)) {
            // This could all be done more efficiently, by only updating what
            // changed - Let's get it working first though.
            BBState &BBS = S.BBStates[BB];
            NewLive = BBS.PhiOuts;
            for (BasicBlock *Succ : successors(BB)) {
                NewLive |= S.BBStates[Succ].LiveIn;
            }
            if (NewLive != BBS.LiveOut) {
                AnyChanged = true;
                BBS.LiveOut = NewLive;
            }
            NewLive.intersectWithComplement(BBS.Defs);
            NewLive |= BBS.UpExposedUses;
            if (NewLive != BBS.LiveIn) {
                AnyChanged = true;
                std::swap(BBS.LiveIn, NewLive);
            }
        }
        Converged = !AnyChanged;
    }
    ComputeLiveSets(S);
}

// For debugging
JL_USED_FUNC static void dumpSafepointsForBBName(Function &F, State &S, const char *BBName) {
    for (Instruction *&it : S.SafepointNumbering) {
        if (it->getParent()->getName() == BBName) {
            int idx = &it - S.SafepointNumbering.begin();
            dbgs() << "Live at " << idx << "\n";
            LargeSparseBitVector &LS = S.LiveSets[idx];
            for (auto Idx : LS) {
                dbgs() << "\t";
                S.ReversePtrNumbering[Idx]->printAsOperand(dbgs());
                dbgs() << "\n";
            }
        }
    }
}

static bool IsIndirectlyRooted(const State &S, LargeSparseBitVector &Visited, LargeSparseBitVector &IndirectlyRootedLS, const LargeSparseBitVector &LS, int RefPtr) {
    if (HasBitSet(IndirectlyRootedLS, RefPtr))
        return true;
    if (HasBitSet(Visited, RefPtr))
        return false;
    const auto it = S.Refinements.find(RefPtr);
    if (it == S.Refinements.end()) {
        Visited.set(RefPtr);
        return false;
    }
    const auto &RefinedPtr = it->second;
    assert(!RefinedPtr.empty());
    bool rooted = true;
    for (auto NRefPtr: RefinedPtr) {
        if (NRefPtr < 0 || IsIndirectlyRooted(S, Visited, IndirectlyRootedLS, LS, NRefPtr)) {
            continue;
        }
        // Not indirectly rooted, but in LS - can be used to establish a root
        if (HasBitSet(LS, NRefPtr))
            continue;
        rooted = false;
        break;
    }
    if (rooted)
        IndirectlyRootedLS.set(RefPtr);
    Visited.set(RefPtr);
    return rooted;
}

void LateLowerGCFrame::RefineLiveSet(LargeSparseBitVector &LS, State &S, ArrayRef<int> CalleeRoots)
{
    // It is possible that a value is not directly rooted by the refinements in the live set, but rather
    // indirectly by following the edges of the refinement graph to all the values that root it.
    // For example, suppose we have:
    // LS: 1 4 5
    // Refinements: 1 -> {2,3}
    //              2 -> 4
    //              3 -> 5
    // Even though {2,3} is not in the LiveSet, we can still refine, because we can follow the edges to
    // the roots {4, 5} which are in the live set. The two bit vectors here cache the lookup for efficiency.
    LargeSparseBitVector Visited;
    LargeSparseBitVector IndirectlyRootedLS;
    for (auto Num: CalleeRoots) {
        // For callee rooted values, they are all kept alive at the safepoint.
        // Make sure they are marked (even though they probably are already)
        // so that other values can be refined to them.
        IndirectlyRootedLS.set(Num);
        // Now unmark all values that are rooted by the callee after
        // refining other values to them.
        LS.reset(Num);
    }

    // Now remove all values from the LiveSet that's kept alive by other objects
    // This loop only mutate `LS` which isn't read from in the loop body so
    // a single pass is enough.
    auto it = LS.begin();
    while (it != LS.end()) {
        int Idx = *it;
        bool rooted = IsIndirectlyRooted(S, Visited, IndirectlyRootedLS, LS, Idx);
        ++it;
        if (rooted) {
            LS.reset(Idx);
        }
    }
}

void LateLowerGCFrame::ComputeLiveSets(State &S) {
    // Iterate over all safe points. Add to live sets all those variables that
    // are now live across their parent block.
    for (Instruction *&Safepoint : S.SafepointNumbering) {
        int idx = &Safepoint - S.SafepointNumbering.begin();
        BasicBlock *BB = Safepoint->getParent();
        BBState &BBS = S.BBStates[BB];
        LargeSparseBitVector LiveAcross = BBS.LiveIn;
        LiveAcross &= BBS.LiveOut;
        LargeSparseBitVector &LS = S.LiveSets[idx];
        LS |= LiveAcross;
        for (int Live : S.LiveIfLiveOut[idx]) {
            if (HasBitSet(BBS.LiveOut, Live))
                LS.set(Live);
        }
        RefineLiveSet(LS, S, S.CalleeRoots[idx]);
        // If the function has GC preserves, figure out whether we need to
        // add in any extra live values.
        if (!S.GCPreserves.empty()) {
            if (!S.DT) {
                S.DT = &GetDT();
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
                    LS.set(Num);
                }
            }
        }
    }
    // Compute the interference graph
    S.Neighbors.resize(S.MaxPtrNumber+1);
    for (Instruction *&Safepoint : S.SafepointNumbering) {
        int idx = &Safepoint - S.SafepointNumbering.begin();
        const LargeSparseBitVector &LS = S.LiveSets[idx];
        for (int idx : LS) {
            S.Neighbors[idx] |= LS;
        }
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
    SmallVector<Element, 0> Elements;
    SmallVector<SmallVector<int, 0>> Levels;
    const SmallVector<LargeSparseBitVector, 0> &Neighbors;
    PEOIterator(const SmallVector<LargeSparseBitVector, 0> &Neighbors) : Neighbors(Neighbors) {
        // Initialize State
        SmallVector<int, 0> FirstLevel;
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
            SmallVector<int, 0> &LastLevel = Levels.back();
            while (NextElement == -1 && !LastLevel.empty()) {
                NextElement = LastLevel.back();
                LastLevel.pop_back();
            }
            if (LastLevel.empty())
                Levels.pop_back();
        }
        if (NextElement == -1)
            return NextElement;
        // Make sure not to try to reuse this later.
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
                Levels.push_back(SmallVector<int, 0>{});
            Levels[NElement.weight].push_back(Neighbor);
            NElement.pos = Levels[NElement.weight].size()-1;
        }
        // As an enhancement, we might want to periodically compactify the whole
        // data structure. This could be done here.
        return NextElement;
    }
};

JL_USED_FUNC static void dumpColorAssignments(const State &S, const ArrayRef<int> &Colors)
{
    for (unsigned i = 0; i < Colors.size(); ++i) {
        if (Colors[i] == -1)
            continue;
        dbgs() << "\tValue ";
        S.ReversePtrNumbering.at(i)->printAsOperand(dbgs());
        dbgs() << " assigned color " << Colors[i] << "\n";
    }
}

std::pair<SmallVector<int, 0>, int> LateLowerGCFrame::ColorRoots(const State &S) {
    SmallVector<int, 0> Colors;
    Colors.resize(S.MaxPtrNumber + 1, -1);
    PEOIterator Ordering(S.Neighbors);
    int PreAssignedColors = 0;
    /* First assign permanent slots to things that need them due
       to returns_twice */
    for (int Num : S.ReturnsTwice) {
        const LargeSparseBitVector &LS = S.LiveSets[Num];
        for (int Idx : LS) {
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
    return {Colors, PreAssignedColors};
}

// Size of T is assumed to be `sizeof(void*)`
Value *LateLowerGCFrame::EmitTagPtr(IRBuilder<> &builder, Type *T, Type *T_size, Value *V)
{
    assert(T == T_size || isa<PointerType>(T));
    return builder.CreateInBoundsGEP(T, V, ConstantInt::get(T_size, -1), V->getName() + ".tag_addr");
}

Value *LateLowerGCFrame::EmitLoadTag(IRBuilder<> &builder, Type *T_size, Value *V)
{
    auto addr = EmitTagPtr(builder, T_size, T_size, V);
    auto &M = *builder.GetInsertBlock()->getModule();
    LoadInst *load = builder.CreateAlignedLoad(T_size, addr, M.getDataLayout().getPointerABIAlignment(0), V->getName() + ".tag");
    load->setOrdering(AtomicOrdering::Unordered);
    // Mark as volatile to prevent optimizers from treating GC tag loads as constants
    // since GC mark bits can change during runtime (issue #59547)
    load->setVolatile(true);
    load->setMetadata(LLVMContext::MD_tbaa, tbaa_tag);
    MDBuilder MDB(load->getContext());
    auto *NullInt = ConstantInt::get(T_size, 0);
    // We can be sure that the tag is at least 16 (1<<4)
    // Hopefully this is enough to convince LLVM that the value is still not NULL
    // after masking off the tag bits
    auto *NonNullInt = ConstantExpr::getAdd(NullInt, ConstantInt::get(T_size, 16));
    load->setMetadata(LLVMContext::MD_range, MDB.createRange(NonNullInt, NullInt));
    return load;
}

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

void LateLowerGCFrame::CleanupWriteBarriers(Function &F, State *S, const SmallVector<CallInst*, 0> &WriteBarriers, bool *CFGModified) {
    auto T_size = F.getParent()->getDataLayout().getIntPtrType(F.getContext());
    for (auto CI : WriteBarriers) {
        auto parent = CI->getArgOperand(0);
        if (std::all_of(CI->op_begin() + 1, CI->op_end(),
                    [parent, &S](Value *child) { return parent == child || IsPermRooted(child, S); })) {
            CI->eraseFromParent();
            continue;
        }
        if (CFGModified) {
            *CFGModified = true;
        }

        IRBuilder<> builder(CI);
        builder.SetCurrentDebugLocation(CI->getDebugLoc());
        auto parBits = builder.CreateAnd(EmitLoadTag(builder, T_size, parent), GC_OLD_MARKED, "parent_bits");
        auto parOldMarked = builder.CreateICmpEQ(parBits, ConstantInt::get(T_size, GC_OLD_MARKED), "parent_old_marked");
        auto mayTrigTerm = SplitBlockAndInsertIfThen(parOldMarked, CI, false);
        builder.SetInsertPoint(mayTrigTerm);
        mayTrigTerm->getParent()->setName("may_trigger_wb");
        Value *anyChldNotMarked = NULL;
        for (unsigned i = 1; i < CI->arg_size(); i++) {
            Value *child = CI->getArgOperand(i);
            Value *chldBit = builder.CreateAnd(EmitLoadTag(builder, T_size, child), GC_MARKED, "child_bit");
            Value *chldNotMarked = builder.CreateICmpEQ(chldBit, ConstantInt::get(T_size, 0), "child_not_marked");
            anyChldNotMarked = anyChldNotMarked ? builder.CreateOr(anyChldNotMarked, chldNotMarked) : chldNotMarked;
        }
        assert(anyChldNotMarked); // handled by all_of test above
        MDBuilder MDB(parent->getContext());
        SmallVector<uint32_t, 2> Weights{1, 9};
        auto trigTerm = SplitBlockAndInsertIfThen(anyChldNotMarked, mayTrigTerm, false,
                                                  MDB.createBranchWeights(Weights));
        trigTerm->getParent()->setName("trigger_wb");
        builder.SetInsertPoint(trigTerm);
        if (CI->getCalledOperand() == write_barrier_func) {
            builder.CreateCall(getOrDeclare(jl_intrinsics::queueGCRoot), parent);
        }
        else {
            assert(false);
        }
        CI->eraseFromParent();
    }
}

bool LateLowerGCFrame::CleanupIR(Function &F, State *S, bool *CFGModified) {
    auto T_int32 = Type::getInt32Ty(F.getContext());
    auto T_size = F.getParent()->getDataLayout().getIntPtrType(F.getContext());
    bool ChangesMade = false;
    // We create one alloca for all the jlcall frames that haven't been processed
    // yet. LLVM would merge them anyway later, so might as well save it a bit
    // of work
    size_t maxframeargs = 0;
    Instruction *StartOff = &*(F.getEntryBlock().begin());
    PointerType *T_pprjlvalue = nullptr;
    AllocaInst *Frame = nullptr;
    unsigned allocaAddressSpace = F.getParent()->getDataLayout().getAllocaAddrSpace();
    if (T_prjlvalue) {
        T_pprjlvalue = PointerType::getUnqual(T_prjlvalue->getContext());
        Frame = new AllocaInst(T_prjlvalue, allocaAddressSpace,ConstantInt::get(T_int32, maxframeargs), "jlcallframe",
#if JL_LLVM_VERSION >= 200000
            StartOff->getIterator()
#else
            StartOff
#endif
        );
    }
    SmallVector<CallInst*, 0> write_barriers;
    for (BasicBlock &BB : F) {
        for (auto it = BB.begin(); it != BB.end();) {
            Instruction *I = &*it;
            // strip all constant alias information, as it might depend on the gc having
            // preserved a gc root, which stops being true after this pass (#32215)
            // similar to RewriteStatepointsForGC::stripNonValidData, but less aggressive
            if (auto *LI = dyn_cast<LoadInst>(I)){
                if (isSpecialPtr(LI->getPointerOperand()->getType()) && LI->getMetadata(LLVMContext::MD_invariant_load))
                    LI->setMetadata(LLVMContext::MD_invariant_load, NULL);
            }
            if (MDNode *TBAA = I->getMetadata(LLVMContext::MD_tbaa)) {
                if (TBAA->getNumOperands() == 4 && isTBAA(TBAA, {"jtbaa_const", "jtbaa_memoryptr", "jtbaa_memorylen", "tbaa_memoryown"})) {
                    MDNode *MutableTBAA = createMutableTBAAAccessTag(TBAA);
                    if (MutableTBAA != TBAA)
                        I->setMetadata(LLVMContext::MD_tbaa, MutableTBAA);
                }
            }
            // FCA chains created by SROA start with an undef value
            // if the type contains an tracked pointer that can lead to a partial
            // initialisation and LateLower might have inserted an extractvalue
            // of an undef field. Fix this by changing it to start with an zero-init
            if (auto *IV = dyn_cast<InsertValueInst>(*&it)) {
                Value *SourceAggregate = IV->getAggregateOperand();
                if (isa<UndefValue>(SourceAggregate)) {
                    IV->setOperand(IV->getAggregateOperandIndex(), ConstantAggregateZero::get(IV->getType()));
                    ChangesMade = true;
                }
            }

            auto *CI = dyn_cast<CallInst>(&*it);
            if (!CI) {
                ++it;
                continue;
            }
            Value *callee = CI->getCalledOperand();
            if (callee && (callee == gc_flush_func || callee == gc_preserve_begin_func
                        || callee == gc_preserve_end_func)) {
                /* No replacement */
            } else if (pointer_from_objref_func != nullptr && callee == pointer_from_objref_func) {
                auto *obj = CI->getOperand(0);
#if JL_LLVM_VERSION >= 200000
                auto *ASCI = new AddrSpaceCastInst(obj, CI->getType(), "", CI->getIterator());
#else
                auto *ASCI = new AddrSpaceCastInst(obj, CI->getType(), "", CI);
#endif
                ASCI->takeName(CI);
                CI->replaceAllUsesWith(ASCI);
                UpdatePtrNumbering(CI, ASCI, S);
            } else if (gc_loaded_func != nullptr && callee == gc_loaded_func) {
                auto *obj = CI->getOperand(1);
#if JL_LLVM_VERSION >= 200000
                auto *ASCI = new AddrSpaceCastInst(obj, CI->getType(), "", CI->getIterator());
#else
                auto *ASCI = new AddrSpaceCastInst(obj, CI->getType(), "", CI);
#endif
                ASCI->takeName(CI);
                CI->replaceAllUsesWith(ASCI);
                UpdatePtrNumbering(CI, ASCI, S);
            } else if (alloc_obj_func && callee == alloc_obj_func) {
                assert(CI->arg_size() == 3);

                // Initialize an IR builder.
                IRBuilder<> builder(CI);
                builder.SetCurrentDebugLocation(CI->getDebugLoc());

                // LLVM alignment/bit check is not happy about addrspacecast and refuse
                // to remove write barrier because of it.
                // We pretty much only load using `T_size` so try our best to strip
                // as many cast as possible.
                auto tag = CI->getArgOperand(2)->stripPointerCastsAndAliases();
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
                        LI->setMetadata(LLVMContext::MD_align, MDNode::get(LI->getContext(), { op }));
                    }
                }
                // As a last resort, if we didn't manage to strip down the tag
                // for LLVM, emit an alignment assumption.
                auto tag_type = tag->getType();
                if (tag_type->isPointerTy()) {
                    auto &DL = CI->getModule()->getDataLayout();
                    auto align = tag->getPointerAlignment(DL).value();
                    if (align < 16) {
                        // On 5 <= LLVM < 12, it is illegal to call this on
                        // non-integral pointer. This relies on stripping the
                        // non-integralness from datalayout before this pass
                        builder.CreateAlignmentAssumption(DL, tag, 16);
                    }
                }

                // Create a call to the `julia.gc_alloc_bytes` intrinsic, which is like
                // `julia.gc_alloc_obj` except it specializes the call based on the constant
                // size of the object to allocate, to save one indirection, and doesn't set
                // the type tag. (Note that if the size is not a constant, it will call
                // gc_alloc_obj, and will redundantly set the tag.)
                auto allocBytesIntrinsic = getOrDeclare(jl_intrinsics::GCAllocBytes);
                auto ptls = get_current_ptls_from_task(builder, CI->getArgOperand(0), tbaa_gcframe);
                auto newI = builder.CreateCall(
                    allocBytesIntrinsic,
                    {
                        ptls,
                        builder.CreateIntCast(
                            CI->getArgOperand(1),
                            allocBytesIntrinsic->getFunctionType()->getParamType(1),
                            false),
                        builder.CreatePtrToInt(tag, T_size),
                    });
                newI->setAttributes(allocBytesIntrinsic->getAttributes());
                newI->addDereferenceableRetAttr(CI->getRetDereferenceableBytes());
                newI->takeName(CI);
                // Now, finally, set the tag. We do this in IR instead of in the C alloc
                // function, to provide possible optimization opportunities. (I think? TBH
                // the most recent editor of this code is not entirely clear on why we
                // prefer to set the tag in the generated code. Providing optimization
                // opportunities is the most likely reason; the tradeoff is slightly
                // larger code size and increased compilation time, compiling this
                // instruction at every allocation site, rather than once in the C alloc
                // function.)
                auto &M = *builder.GetInsertBlock()->getModule();
                StoreInst *store = builder.CreateAlignedStore(
                    tag, EmitTagPtr(builder, tag_type, T_size, newI), M.getDataLayout().getPointerABIAlignment(0));
                store->setOrdering(AtomicOrdering::Unordered);
                store->setMetadata(LLVMContext::MD_tbaa, tbaa_tag);

                // Replace uses of the call to `julia.gc_alloc_obj` with the call to
                // `julia.gc_alloc_bytes`.
                CI->replaceAllUsesWith(newI);

                // Update the pointer numbering.
                UpdatePtrNumbering(CI, newI, S);
            } else if (typeof_func && callee == typeof_func) {
                assert(CI->arg_size() == 1);
                IRBuilder<> builder(CI);
                builder.SetCurrentDebugLocation(CI->getDebugLoc());
                auto tag = EmitLoadTag(builder, T_size, CI->getArgOperand(0));
                auto masked = builder.CreateAnd(tag, ConstantInt::get(T_size, ~(uintptr_t)15));
                auto typ = builder.CreateAddrSpaceCast(builder.CreateIntToPtr(masked, JuliaType::get_pjlvalue_ty(masked->getContext())),
                                                       T_prjlvalue);
                typ->takeName(CI);
                CI->replaceAllUsesWith(typ);
                UpdatePtrNumbering(CI, typ, S);
            } else if (write_barrier_func && callee == write_barrier_func) {
                // The replacement for this requires creating new BasicBlocks
                // which messes up the loop. Queue all of them to be replaced later.
                assert(CI->arg_size() >= 1);
                write_barriers.push_back(CI);
                ChangesMade = true;
                ++it;
                continue;
            } else if ((call_func && callee == call_func) ||
                       (call2_func && callee == call2_func) ||
                       (call3_func && callee == call3_func)) {
                assert(T_prjlvalue);
                size_t nargs = CI->arg_size();
                size_t nframeargs = nargs-1;
                if (callee == call2_func)
                    nframeargs -= 2;
                else
                    nframeargs -= 1;
                SmallVector<Value*, 4> ReplacementArgs;
                auto arg_it = CI->arg_begin();
                assert(arg_it != CI->arg_end());
                Value *new_callee = *(arg_it++);
                assert(arg_it != CI->arg_end());
                ReplacementArgs.push_back(*(arg_it++));
                if (callee == call2_func) {
                    assert(arg_it != CI->arg_end());
                    ReplacementArgs.push_back(*(arg_it++));
                }
                maxframeargs = std::max(maxframeargs, nframeargs);
                int slot = 0;
                IRBuilder<> Builder (CI);
                for (; arg_it != CI->arg_end(); ++arg_it) {
                    // Julia emits IR with proper pointer types here, but because
                    // the julia.call signature is varargs, the optimizer is allowed
                    // to rewrite pointee types. It'll go away with opaque pointer
                    // types anyway.
                    Builder.CreateAlignedStore(*arg_it,
                            Builder.CreateInBoundsGEP(T_prjlvalue, Frame, ConstantInt::get(T_int32, slot++)),
                            Align(sizeof(void*)));
                }
                ReplacementArgs.push_back(nframeargs == 0 ?
                    (llvm::Value*)ConstantPointerNull::get(T_pprjlvalue) :
                    Builder.CreateAddrSpaceCast(Frame, PointerType::getUnqual(T_prjlvalue->getContext())));
                ReplacementArgs.push_back(ConstantInt::get(T_int32, nframeargs));
                if (callee == call2_func) {
                    // move trailing arg to the end now
                    Value *front = ReplacementArgs.front();
                    ReplacementArgs.erase(ReplacementArgs.begin());
                    ReplacementArgs.push_back(front);
                }
                FunctionType *FTy = callee == call3_func ? JuliaType::get_jlfunc3_ty(CI->getContext()) :
                                    callee == call2_func ? JuliaType::get_jlfunc2_ty(CI->getContext()) :
                                                           JuliaType::get_jlfunc_ty(CI->getContext());
#if JL_LLVM_VERSION >= 200000
                CallInst *NewCall = CallInst::Create(FTy, new_callee, ReplacementArgs, "", CI->getIterator());
#else
                CallInst *NewCall = CallInst::Create(FTy, new_callee, ReplacementArgs, "", CI);
#endif
                NewCall->setTailCallKind(CI->getTailCallKind());
                auto callattrs = CI->getAttributes();
                callattrs = AttributeList::get(CI->getContext(), getFnAttrs(callattrs), getRetAttrs(callattrs), {});
                if (auto new_callee = CI->getCalledFunction()) // get the parameter attributes from the function target (if possible)
                    callattrs = AttributeList::get(CI->getContext(), {callattrs, new_callee->getAttributes()});
                NewCall->setAttributes(callattrs);
                NewCall->takeName(CI);
                NewCall->copyMetadata(*CI);
                CI->replaceAllUsesWith(NewCall);
                UpdatePtrNumbering(CI, NewCall, S);
            } else {
                SmallVector<OperandBundleDef,2> bundles;
                CI->getOperandBundlesAsDefs(bundles);
                bool gc_transition = false;
                Value *ptls = nullptr;
                for (auto &bundle: bundles)
                    if (bundle.getTag() == "gc-transition") {
                        gc_transition = true;
                        ptls = bundle.inputs()[0];
                    }

                // In theory LLVM wants us to lower this using RewriteStatepointsForGC
                if (gc_transition) {
                    // Insert the operations to switch to gc_safe if necessary.
                    IRBuilder<> builder(CI);
                    assert(ptls);
                    // We dont use emit_state_set here because safepoints are unconditional for any code that reaches this
                    // We are basically guaranteed to go from gc_unsafe to gc_safe and back, and both transitions need a safepoint
                    // We also can't add any BBs here, so just avoiding the branches is good
                    unsigned offset = offsetof(jl_tls_states_t, gc_state);
                    Value *gc_state = builder.CreateConstInBoundsGEP1_32(Type::getInt8Ty(builder.getContext()), ptls, offset, "gc_state");
                    LoadInst *last_gc_state = builder.CreateAlignedLoad(Type::getInt8Ty(builder.getContext()), gc_state, Align(sizeof(void*)));
                    last_gc_state->setOrdering(AtomicOrdering::Monotonic);
                    builder.CreateAlignedStore(builder.getInt8(JL_GC_STATE_SAFE), gc_state, Align(sizeof(void*)))->setOrdering(AtomicOrdering::Release);
                    MDNode *tbaa = get_tbaa_const(builder.getContext());
                    emit_gc_safepoint(builder, T_size, ptls, tbaa, false);
                    builder.SetInsertPoint(CI->getNextNode());
                    builder.CreateAlignedStore(last_gc_state, gc_state, Align(sizeof(void*)))->setOrdering(AtomicOrdering::Release);
                    emit_gc_safepoint(builder, T_size, ptls, tbaa, false);
                }
                if (CI->arg_size() == CI->getNumOperands()) {
                    /* No operand bundle to lower */
                    ++it;
                    continue;
                } else {
                    // remove all operand bundles
#if JL_LLVM_VERSION >= 200000
                    CallInst *NewCall = CallInst::Create(CI, None, CI->getIterator());
#else
                    CallInst *NewCall = CallInst::Create(CI, None, CI);
#endif
                    NewCall->takeName(CI);
                    NewCall->copyMetadata(*CI);
                    CI->replaceAllUsesWith(NewCall);
                    UpdatePtrNumbering(CI, NewCall, S);
                }
            }
            if (!CI->use_empty()) {
                CI->replaceAllUsesWith(UndefValue::get(CI->getType()));
                UpdatePtrNumbering(CI, nullptr, S);
            }
            it = CI->eraseFromParent();
            ChangesMade = true;
        }
    }
    CleanupWriteBarriers(F, S, write_barriers, CFGModified);
    if (maxframeargs == 0 && Frame) {
        Frame->eraseFromParent();
    }
    else if (Frame) {
        Frame->setOperand(0, ConstantInt::get(T_int32, maxframeargs));
    }
    return ChangesMade;
}

// Compute the set of all objects that are live in from all predecessors
// TODO: reset any slots that contain values which are only live from some predecessors
static void AddInPredecessorLiveOuts(BasicBlock *BB, LargeSparseBitVector &LiveIn, State &S)
{
    bool First = true;
    std::set<BasicBlock *> Visited;
    SmallVector<BasicBlock *, 0> WorkList;
    WorkList.push_back(BB);
    while (!WorkList.empty()) {
        BB = &*WorkList.back();
        WorkList.pop_back();
        // Nothing is live at function entry
        if (BB == &S.F->getEntryBlock()) {
            LiveIn.clear();
            return;
        }
        for (BasicBlock *Pred : predecessors(BB)) {
            if (!Visited.insert(Pred).second)
                continue;
            if (!S.BBStates[Pred].HasSafepoint) {
                WorkList.push_back(Pred);
                continue;
            } else {
                int LastSP = S.BBStates[Pred].LastSafepoint;
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
                                         ArrayRef<int> Colors, Value *GCFrame,
                                         Instruction *InsertBefore) {
    // Get the slot address.
    auto slotAddress = CallInst::Create(
        getOrDeclare(jl_intrinsics::getGCFrameSlot),
        {GCFrame, ConstantInt::get(Type::getInt32Ty(InsertBefore->getContext()), Colors[R] + MinColorRoot)},
#if JL_LLVM_VERSION >= 200000
        "gc_slot_addr_" + StringRef(std::to_string(Colors[R] + MinColorRoot)), InsertBefore->getIterator());
#else
        "gc_slot_addr_" + StringRef(std::to_string(Colors[R] + MinColorRoot)), InsertBefore);
#endif

    Value *Val = GetPtrForNumber(S, R, InsertBefore);
    // Pointee types don't have semantics, so the optimizer is
    // free to rewrite them if convenient. We need to change
    // it back here for the store.
    assert(Val->getType() == T_prjlvalue);
#if JL_LLVM_VERSION >= 200000
    new StoreInst(Val, slotAddress, InsertBefore->getIterator());
#else
    new StoreInst(Val, slotAddress, InsertBefore);
#endif
}

void LateLowerGCFrame::PlaceGCFrameReset(State &S, unsigned R, unsigned MinColorRoot,
                                         ArrayRef<int> Colors, Value *GCFrame,
                                         Instruction *InsertBefore) {
    // Get the slot address.
    auto slotAddress = CallInst::Create(
        getOrDeclare(jl_intrinsics::getGCFrameSlot),
        {GCFrame, ConstantInt::get(Type::getInt32Ty(InsertBefore->getContext()), Colors[R] + MinColorRoot)},
#if JL_LLVM_VERSION >= 200000
        "gc_slot_addr_" + StringRef(std::to_string(Colors[R] + MinColorRoot)), InsertBefore->getIterator());
#else
        "gc_slot_addr_" + StringRef(std::to_string(Colors[R] + MinColorRoot)), InsertBefore);
#endif
    // Reset the slot to NULL.
    Value *Val = ConstantPointerNull::get(T_prjlvalue);
#if JL_LLVM_VERSION >= 200000
    new StoreInst(Val, slotAddress, InsertBefore->getIterator());
#else
    new StoreInst(Val, slotAddress, InsertBefore);
#endif
}

void LateLowerGCFrame::PlaceGCFrameStores(State &S, unsigned MinColorRoot,
                                          ArrayRef<int> Colors, int PreAssignedColors, Value *GCFrame)
{
    for (auto &BB : *S.F) {
        const BBState &BBS = S.BBStates[&BB];
        if (!BBS.HasSafepoint)
            continue;
        LargeSparseBitVector LiveIn;
        AddInPredecessorLiveOuts(&BB, LiveIn, S);
        const LargeSparseBitVector *LastLive = &LiveIn;
        for (int Safepoint = BBS.FirstSafepoint; Safepoint >= BBS.LastSafepoint; --Safepoint) {
            const LargeSparseBitVector &NowLive = S.LiveSets[Safepoint];
            // reset slots which are no longer alive
            for (int Idx : *LastLive) {
                if (Colors[Idx] >= PreAssignedColors && !HasBitSet(NowLive, Idx)) {
                    PlaceGCFrameReset(S, Idx, MinColorRoot, Colors, GCFrame,
                        S.SafepointNumbering[Safepoint]);
                }
            }
            // store values which are alive in this safepoint but
            // haven't been stored in the GC frame before
            for (int Idx : NowLive) {
                if (!HasBitSet(*LastLive, Idx)) {
                    PlaceGCFrameStore(S, Idx, MinColorRoot, Colors, GCFrame,
                      S.SafepointNumbering[Safepoint]);
                }
            }
            LastLive = &NowLive;
        }
    }
}

void LateLowerGCFrame::PlaceRootsAndUpdateCalls(ArrayRef<int> Colors, int PreAssignedColors, State &S,
                                                std::map<Value *, std::pair<int, int>>) {
    auto F = S.F;
    auto T_int32 = Type::getInt32Ty(F->getContext());
    int MaxColor = -1;
    for (auto C : Colors)
        if (C > MaxColor)
            MaxColor = C;

    // Insert instructions for the actual gc frame
    if (MaxColor != -1 || !S.ArrayAllocas.empty() || !S.TrackedStores.empty()) {
        // Create and push a GC frame.
        auto gcframe = CallInst::Create(
            getOrDeclare(jl_intrinsics::newGCFrame),
            {ConstantInt::get(T_int32, 0)},
            "gcframe");
        gcframe->insertBefore(F->getEntryBlock().begin());

        auto pushGcframe = CallInst::Create(
            getOrDeclare(jl_intrinsics::pushGCFrame),
            {gcframe, ConstantInt::get(T_int32, 0)});
        if (isa<Argument>(pgcstack))
             pushGcframe->insertAfter(gcframe);
         else
             pushGcframe->insertAfter(cast<Instruction>(pgcstack));

        // we don't run memsetopt after this, so run a basic approximation of it
        // that removes any redundant memset calls in the prologue since getGCFrameSlot already includes the null store
        Instruction *toerase = nullptr;
        for (auto &I : F->getEntryBlock()) {
            if (toerase)
                toerase->eraseFromParent();
            toerase = nullptr;
            Value *ptr;
            Value *value;
            bool isvolatile;
            if (auto *SI = dyn_cast<StoreInst>(&I)) {
                ptr = SI->getPointerOperand();
                value = SI->getValueOperand();
                isvolatile = SI->isVolatile();
            }
            else if (auto *MSI = dyn_cast<MemSetInst>(&I)) {
                ptr = MSI->getDest();
                value = MSI->getValue();
                isvolatile = MSI->isVolatile();
            }
            else {
                continue;
            }
            ptr = ptr->stripInBoundsOffsets();
            AllocaInst *AI = dyn_cast<AllocaInst>(ptr);
            if (isa<GetElementPtrInst>(ptr))
                break;
            if (!S.ArrayAllocas.count(AI))
                continue;
            if (isvolatile || !isa<Constant>(value) || !cast<Constant>(value)->isNullValue())
                break; // stop once we reach a pointer operation that couldn't be analyzed or isn't a null store
            toerase = &I;
        }
        if (toerase)
            toerase->eraseFromParent();
        toerase = nullptr;

        // Replace Allocas
        unsigned AllocaSlot = 2; // first two words are metadata
        auto replace_alloca = [this, gcframe, &AllocaSlot, T_int32](AllocaInst *&AI) {
            // Pick a slot for the alloca.
            AI->getAlign();
            unsigned align = AI->getAlign().value() / sizeof(void*); // TODO: use DataLayout pointer size
            assert(align <= 16 / sizeof(void*) && "Alignment exceeds llvm-final-gc-lowering abilities");
            if (align > 1)
                AllocaSlot = LLT_ALIGN(AllocaSlot, align);
            Instruction *slotAddress = CallInst::Create(
                getOrDeclare(jl_intrinsics::getGCFrameSlot),
                {gcframe, ConstantInt::get(T_int32, AllocaSlot - 2)}, "gc_slot_addr" + StringRef(std::to_string(AllocaSlot - 2)));
            slotAddress->insertAfter(gcframe);
            slotAddress->takeName(AI);

            // Check for lifetime intrinsics on this alloca, we can't keep them
            // because we're changing the semantics
            SmallVector<CallInst*, 0> ToDelete;
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
            assert(slotAddress->getType() == AI->getType());
            AI->replaceAllUsesWith(slotAddress);
            AI->eraseFromParent();
            AI = NULL;
        };
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
                    {gcframe, ConstantInt::get(T_int32, AllocaSlot - 2)}, "gc_slot_addr" + StringRef(std::to_string(AllocaSlot - 2)));
                slotAddress->insertAfter(gcframe);
                auto ValExpr = std::make_pair(Base, isa<PointerType>(Base->getType()) ? -1 : i);
                auto Elem = MaybeExtractScalar(S, ValExpr, SI);
                assert(Elem->getType() == T_prjlvalue);
                //auto Idxs = ArrayRef<unsigned>(Tracked[i]);
                //Value *Elem = ExtractScalar(Base, true, Idxs, SI);
#if JL_LLVM_VERSION >= 200000
                Value *shadowStore = new StoreInst(Elem, slotAddress, SI->getIterator());
#else
                Value *shadowStore = new StoreInst(Elem, slotAddress, SI);
#endif
                (void)shadowStore;
                // TODO: shadowStore->setMetadata(LLVMContext::MD_tbaa, tbaa_gcframe);
                AllocaSlot++;
            }
        }
        auto NRoots = ConstantInt::get(T_int32, MaxColor + 1 + AllocaSlot - 2);
        gcframe->setArgOperand(0, NRoots);
        pushGcframe->setArgOperand(1, NRoots);

        // Insert GC frame stores
        PlaceGCFrameStores(S, AllocaSlot - 2, Colors, PreAssignedColors, gcframe);
        // Insert GCFrame pops
        for (auto &BB : *F) {
            if (isa<ReturnInst>(BB.getTerminator())) {
                auto popGcframe = CallInst::Create(
                    getOrDeclare(jl_intrinsics::popGCFrame),
                    {gcframe});
#if JL_LLVM_VERSION >= 200000
                popGcframe->insertBefore(BB.getTerminator()->getIterator());
#else
                popGcframe->insertBefore(BB.getTerminator());
#endif
            }
        }
    }
}

bool LateLowerGCFrame::runOnFunction(Function &F, bool *CFGModified) {
    initAll(*F.getParent());
    smallAllocFunc = getOrDeclare(jl_well_known::GCSmallAlloc);
    LLVM_DEBUG(dbgs() << "GC ROOT PLACEMENT: Processing function " << F.getName() << "\n");

    pgcstack = getPGCstack(F);
    if (pgcstack) {
      State S = LocalScan(F);
      // If there is no safepoint after the first reachable def, then we don't need any roots (even those for allocas)
      if (std::any_of(S.BBStates.begin(), S.BBStates.end(),
                  [&F](auto BBS) {
                      if (BBS.first == &F.getEntryBlock())
                          return BBS.second.FirstSafepointAfterFirstDef != -1;
                      return BBS.second.HasSafepoint;
                  })) {
        ComputeLiveness(S);
        auto Colors = ColorRoots(S);
        std::map<Value *, std::pair<int, int>> CallFrames; // = OptimizeCallFrames(S, Ordering);
        PlaceRootsAndUpdateCalls(Colors.first, Colors.second, S, CallFrames);
      }
      CleanupIR(F, &S, CFGModified);
    }
    else {
      CleanupIR(F, nullptr, CFGModified);
    }

    // We lower the julia.gc_alloc_bytes intrinsic in this pass to insert slowpath/fastpath blocks for MMTk
    // For now, we do nothing for the Stock GC
    auto GCAllocBytes = getOrNull(jl_intrinsics::GCAllocBytes);

    if (GCAllocBytes) {
        for (auto it = GCAllocBytes->user_begin(); it != GCAllocBytes->user_end(); ) {
            if (auto *CI = dyn_cast<CallInst>(*it)) {
                *CFGModified = true;

                assert(CI->getCalledOperand() == GCAllocBytes);

                auto newI = lowerGCAllocBytesLate(CI, F);
                if (newI != CI) {
                    ++it;
                    CI->replaceAllUsesWith(newI);
                    CI->eraseFromParent();
                    continue;
                }
            }
            ++it;
        }
    }

    return true;
}

PreservedAnalyses LateLowerGCPass::run(Function &F, FunctionAnalysisManager &AM)
{
    auto GetDT = [&AM, &F]() -> DominatorTree & {
        return AM.getResult<DominatorTreeAnalysis>(F);
    };
    auto lateLowerGCFrame = LateLowerGCFrame(GetDT);
    bool CFGModified = false;
    bool modified = lateLowerGCFrame.runOnFunction(F, &CFGModified);
#ifdef JL_VERIFY_PASSES
    assert(!verifyLLVMIR(F));
#endif
    if (modified) {
        if (CFGModified) {
            return PreservedAnalyses::none();
        } else {
            return PreservedAnalyses::allInSet<CFGAnalyses>();
        }
    }
    return PreservedAnalyses::all();
}

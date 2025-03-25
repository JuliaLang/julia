// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-version.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

#include <llvm/ADT/SmallPtrSet.h>
#include <llvm/Analysis/CFG.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/ValueMap.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/InstVisitor.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Pass.h>
#include <llvm/Support/Debug.h>

#include "passes.h"
#include "llvm-codegen-shared.h"

#define DEBUG_TYPE "propagate_julia_addrspaces"

using namespace llvm;

/* This pass performs propagation of addrspace information that is legal from
   the frontend definition, but illegal by general IR semantics. In particular,
   this includes:
      - Changing the address space of a load/store if the base pointer is
        in an untracked address space
      - Commuting GEPs and addrspace casts

    This is most useful for removing superfluous casts that can inhibit LLVM
    optimizations.
*/

struct PropagateJuliaAddrspacesVisitor : public InstVisitor<PropagateJuliaAddrspacesVisitor> {
    DenseMap<Value *, Value *> LiftingMap;
    SmallPtrSet<Value *, 4> Visited;
    SmallVector<Instruction *, 0> ToDelete;
    SmallVector<std::pair<Instruction *, Instruction *>, 0> ToInsert;

public:
    Value *LiftPointer(Module *M, Value *V, Instruction *InsertPt=nullptr);
    void visitMemop(Instruction &I, Type *T, unsigned OpIndex);
    void visitLoadInst(LoadInst &LI);
    void visitStoreInst(StoreInst &SI);
    void visitAtomicCmpXchgInst(AtomicCmpXchgInst &SI);
    void visitAtomicRMWInst(AtomicRMWInst &SI);
    void visitMemSetInst(MemSetInst &MI);
    void visitMemTransferInst(MemTransferInst &MTI);

private:
    void PoisonValues(SmallVectorImpl<Value *> &Worklist);
};

static unsigned getValueAddrSpace(Value *V) {
    return V->getType()->getPointerAddressSpace();
}

static bool isSpecialAS(unsigned AS) {
    return AddressSpace::FirstSpecial <= AS && AS <= AddressSpace::LastSpecial;
}

void PropagateJuliaAddrspacesVisitor::PoisonValues(SmallVectorImpl<Value *> &Worklist) {
    while (!Worklist.empty()) {
        Value *CurrentV = Worklist.back();
        Worklist.pop_back();
        for (Value *User : CurrentV->users()) {
            if (Visited.count(User))
                continue;
            Visited.insert(CurrentV);
            Worklist.push_back(User);
        }
    }
}

Value *PropagateJuliaAddrspacesVisitor::LiftPointer(Module *M, Value *V, Instruction *InsertPt) {
    SmallVector<Value *, 4> Stack;
    SmallVector<Value *, 0> Worklist;
    std::set<Value *> LocalVisited;
    unsigned allocaAddressSpace = M->getDataLayout().getAllocaAddrSpace();
    Worklist.push_back(V);
    // Follow pointer casts back, see if we're based on a pointer in
    // an untracked address space, in which case we're allowed to drop
    // intermediate addrspace casts.
    while (!Worklist.empty()) {
        Value *CurrentV = Worklist.back();
        Worklist.pop_back();
        if (LocalVisited.count(CurrentV)) {
            continue;
        }
        while (true) {
            if (auto *BCI = dyn_cast<BitCastInst>(CurrentV))
                CurrentV = BCI->getOperand(0);
            else if (auto *ACI = dyn_cast<AddrSpaceCastInst>(CurrentV)) {
                CurrentV = ACI->getOperand(0);
                if (!isSpecialAS(getValueAddrSpace(ACI)))
                    break;
            }
            else if (auto *GEP = dyn_cast<GetElementPtrInst>(CurrentV)) {
                if (LiftingMap.count(GEP)) {
                    break;
                } else if (Visited.count(GEP)) {
                    return nullptr;
                }
                Stack.push_back(GEP);
                LocalVisited.insert(GEP);
                CurrentV = GEP->getOperand(0);
            } else if (auto *Phi = dyn_cast<PHINode>(CurrentV)) {
                if (LiftingMap.count(Phi)) {
                    break;
                }
                for (Value *Incoming : Phi->incoming_values()) {
                    Worklist.push_back(Incoming);
                }
                Stack.push_back(Phi);
                LocalVisited.insert(Phi);
                break;
            } else if (auto *Select = dyn_cast<SelectInst>(CurrentV)) {
                if (LiftingMap.count(Select)) {
                    break;
                } else if (Visited.count(Select)) {
                    return nullptr;
                }
                // Push one of the branches onto the worklist, continue with the other one
                // directly
                Worklist.push_back(Select->getOperand(2));
                Stack.push_back(Select);
                LocalVisited.insert(Select);
                CurrentV = Select->getOperand(1);
            } else if (isa<ConstantPointerNull>(CurrentV)) {
                // It's always legal to lift null pointers into any address space
                break;
            } else {
                // Ok, we've reached a leaf - check if it is eligible for lifting
                if (!CurrentV->getType()->isPtrOrPtrVectorTy() ||
                    isSpecialAS(getValueAddrSpace(CurrentV))) {
                    // If not, poison all (recursive) users of this value, to prevent
                    // looking at them again in future iterations.
                    Worklist.clear();
                    Worklist.push_back(CurrentV);
                    Visited.insert(CurrentV);
                    PoisonValues(Worklist);
                    return nullptr;
                }
                break;
            }
        }
    }

    // Go through and insert lifted versions of all instructions on the list.
    SmallVector<Value *, 0> ToRevisit;
    for (Value *V : Stack) {
        if (LiftingMap.count(V))
            continue;
        if (isa<GetElementPtrInst>(V) || isa<PHINode>(V) || isa<SelectInst>(V)) {
            Instruction *InstV = cast<Instruction>(V);
            Instruction *NewV = InstV->clone();
            ToInsert.push_back(std::make_pair(NewV, InstV));
            Type *NewRetTy = PointerType::get(InstV->getType(), allocaAddressSpace);
            NewV->mutateType(NewRetTy);
            LiftingMap[InstV] = NewV;
            ToRevisit.push_back(NewV);
        }
    }
    auto CollapseCastsAndLift = [&](Value *CurrentV, Instruction *InsertPt) -> Value * {
        PointerType *TargetType = PointerType::get(CurrentV->getType(), allocaAddressSpace);
        while (!LiftingMap.count(CurrentV)) {
            if (isa<BitCastInst>(CurrentV))
                CurrentV = cast<BitCastInst>(CurrentV)->getOperand(0);
            else if (isa<AddrSpaceCastInst>(CurrentV))
                CurrentV = cast<AddrSpaceCastInst>(CurrentV)->getOperand(0);
            else
                break;
        }
        if (isa<ConstantPointerNull>(CurrentV)) {
            return ConstantPointerNull::get(TargetType);
        }
        if (LiftingMap.count(CurrentV))
            CurrentV = LiftingMap[CurrentV];
        assert(CurrentV->getType() == TargetType);
        return CurrentV;
    };

    // Now go through and update the operands
    for (Value *V : ToRevisit) {
        if (GetElementPtrInst *NewGEP = dyn_cast<GetElementPtrInst>(V)) {
            NewGEP->setOperand(GetElementPtrInst::getPointerOperandIndex(),
                CollapseCastsAndLift(NewGEP->getOperand(GetElementPtrInst::getPointerOperandIndex()),
                NewGEP));
        } else if (PHINode *NewPhi = dyn_cast<PHINode>(V)) {
            for (size_t i = 0; i < NewPhi->getNumIncomingValues(); ++i) {
                NewPhi->setIncomingValue(i, CollapseCastsAndLift(NewPhi->getIncomingValue(i),
                    NewPhi->getIncomingBlock(i)->getTerminator()));
            }
        } else if (SelectInst *NewSelect = dyn_cast<SelectInst>(V)) {
            NewSelect->setOperand(1, CollapseCastsAndLift(NewSelect->getOperand(1), NewSelect));
            NewSelect->setOperand(2, CollapseCastsAndLift(NewSelect->getOperand(2), NewSelect));
        } else {
            assert(false && "Shouldn't have reached here");
        }
    }

    return CollapseCastsAndLift(V, InsertPt);
}

void PropagateJuliaAddrspacesVisitor::visitMemop(Instruction &I, Type *T, unsigned OpIndex) {
    Value *Original = I.getOperand(OpIndex);
    unsigned AS = Original->getType()->getPointerAddressSpace();
    if (!isSpecialAS(AS))
        return;
    Value *Replacement = LiftPointer(I.getModule(), Original, &I);
    if (!Replacement)
        return;
    I.setOperand(OpIndex, Replacement);
}

void PropagateJuliaAddrspacesVisitor::visitLoadInst(LoadInst &LI) {
    visitMemop(LI, LI.getType(), LoadInst::getPointerOperandIndex());
}

void PropagateJuliaAddrspacesVisitor::visitStoreInst(StoreInst &SI) {
    visitMemop(SI, SI.getValueOperand()->getType(), StoreInst::getPointerOperandIndex());
}

void PropagateJuliaAddrspacesVisitor::visitAtomicCmpXchgInst(AtomicCmpXchgInst &SI) {
    visitMemop(SI, SI.getNewValOperand()->getType(), AtomicCmpXchgInst::getPointerOperandIndex());
}

void PropagateJuliaAddrspacesVisitor::visitAtomicRMWInst(AtomicRMWInst &SI) {
    visitMemop(SI, SI.getType(), AtomicRMWInst::getPointerOperandIndex());
}

void PropagateJuliaAddrspacesVisitor::visitMemSetInst(MemSetInst &MI) {
    unsigned AS = MI.getDestAddressSpace();
    if (!isSpecialAS(AS))
        return;
    Value *Replacement = LiftPointer(MI.getModule(), MI.getRawDest());
    if (!Replacement)
        return;
#if JL_LLVM_VERSION >= 200000
    Function *TheFn = Intrinsic::getOrInsertDeclaration(MI.getModule(), Intrinsic::memset,
#else
    Function *TheFn = Intrinsic::getDeclaration(MI.getModule(), Intrinsic::memset,
#endif
        {Replacement->getType(), MI.getOperand(1)->getType()});
    MI.setCalledFunction(TheFn);
    MI.setArgOperand(0, Replacement);
}

void PropagateJuliaAddrspacesVisitor::visitMemTransferInst(MemTransferInst &MTI) {
    unsigned DestAS = MTI.getDestAddressSpace();
    unsigned SrcAS = MTI.getSourceAddressSpace();
    if (!isSpecialAS(DestAS) && !isSpecialAS(SrcAS))
        return;
    Value *Dest = MTI.getRawDest();
    if (isSpecialAS(DestAS)) {
        Value *Replacement = LiftPointer(MTI.getModule(), Dest, &MTI);
        if (Replacement)
            Dest = Replacement;
    }
    Value *Src = MTI.getRawSource();
    if (isSpecialAS(SrcAS)) {
        Value *Replacement = LiftPointer(MTI.getModule(), Src, &MTI);
        if (Replacement)
            Src = Replacement;
    }
    if (Dest == MTI.getRawDest() && Src == MTI.getRawSource())
        return;
#if JL_LLVM_VERSION >= 200000
    Function *TheFn = Intrinsic::getOrInsertDeclaration(MTI.getModule(), MTI.getIntrinsicID(),
#else
    Function *TheFn = Intrinsic::getDeclaration(MTI.getModule(), MTI.getIntrinsicID(),
#endif
        {Dest->getType(), Src->getType(),
         MTI.getOperand(2)->getType()});
    MTI.setCalledFunction(TheFn);
    MTI.setArgOperand(0, Dest);
    MTI.setArgOperand(1, Src);
}

bool propagateJuliaAddrspaces(Function &F) {
    PropagateJuliaAddrspacesVisitor visitor;
    visitor.visit(F);
    for (auto it : visitor.ToInsert)
        it.first->insertBefore(it.second);
    for (Instruction *I : visitor.ToDelete)
        I->eraseFromParent();
    visitor.ToInsert.clear();
    visitor.ToDelete.clear();
    visitor.LiftingMap.clear();
    visitor.Visited.clear();
    return true;
}
PreservedAnalyses PropagateJuliaAddrspacesPass::run(Function &F, FunctionAnalysisManager &AM) {
    bool modified = propagateJuliaAddrspaces(F);

#ifdef JL_VERIFY_PASSES
    assert(!verifyLLVMIR(F));
#endif
    if (modified) {
        return PreservedAnalyses::allInSet<CFGAnalyses>();
    } else {
        return PreservedAnalyses::all();
    }
}

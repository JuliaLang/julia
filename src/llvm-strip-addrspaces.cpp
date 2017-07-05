// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/Instructions.h>
#include <llvm/Support/Debug.h>

#include "llvm-version.h"
#include "codegen_shared.h"
#include "julia.h"

#define DEBUG_TYPE "strip_julia_addrspaces"

using namespace llvm;

template<typename T>
void unsafeReplaceAllUsesWith(T *from, T *to) {
  while (!from->use_empty()) {
    auto &U = *from->use_begin();
    U.set(to);
  }
}

unsigned remapAS(unsigned AS) {
    if (isSpecialAS(AS))
        return AddressSpace::Generic;
    else
        return AS;
}


//
// Type remapping
//

Type *remapType(Type *SrcTy);

PointerType* remapPointerType(PointerType* SrcTy) {
    Type* ElTy = remapType(SrcTy->getElementType());
    bool Changed = (ElTy != SrcTy->getElementType());
    unsigned AS = remapAS(SrcTy->getAddressSpace());
    Changed |= (AS != SrcTy->getAddressSpace());
    return Changed ? PointerType::get(ElTy, remapAS(SrcTy->getAddressSpace())) : SrcTy;
}

FunctionType* remapFunctionType(FunctionType *SrcTy) {
    auto *RetTy = remapType(SrcTy->getReturnType());

    auto NumParams = SrcTy->getNumParams();
    SmallVector<Type*,4> Params(NumParams);
    bool Changed = false;
    for (unsigned i = 0; i < NumParams; ++i) {
        Params[i] = remapType(SrcTy->getParamType(i));
        Changed |= (Params[i] != SrcTy->getParamType(i));
    }

    return Changed ? FunctionType::get(RetTy, Params, SrcTy->isVarArg()) : SrcTy;
}

VectorType* remapVectorType(VectorType* SrcTy) {
    Type* ElTy = remapType(SrcTy->getElementType());
    bool Changed = (ElTy != SrcTy->getElementType());
    return Changed ? VectorType::get(ElTy, SrcTy->getNumElements()) : SrcTy;
}

ArrayType* remapArrayType(ArrayType* SrcTy) {
    Type* ElTy = remapType(SrcTy->getElementType());
    bool Changed = (ElTy != SrcTy->getElementType());
    return Changed ? ArrayType::get(ElTy, SrcTy->getNumElements()) : SrcTy;
}

StructType* remapStructType(StructType* SrcTy) {
    auto Els = SrcTy->getNumElements();
    SmallVector<Type*,4> NewElTys(Els);
    bool Changed = false;
    for (unsigned i = 0; i < Els; ++i) {
        NewElTys[i] = remapType(SrcTy->getElementType(i));
        Changed |= (NewElTys[i] != SrcTy->getElementType(i));
    }

    return Changed ? StructType::create(SrcTy->getContext(), NewElTys, SrcTy->getName(), SrcTy->isPacked()) : SrcTy;
}

Type *remapType(Type *SrcTy)
{
    // process all descendants from llvm::Type that contain other types
    if (auto Ty = dyn_cast<PointerType>(SrcTy))
        return remapPointerType(Ty);
    if (auto Ty = dyn_cast<FunctionType>(SrcTy))
        return remapFunctionType(Ty);
    if (auto Ty = dyn_cast<VectorType>(SrcTy))
        return remapVectorType(Ty);
    if (auto Ty = dyn_cast<ArrayType>(SrcTy))
        return remapArrayType(Ty);
    if (auto Ty = dyn_cast<StructType>(SrcTy))
        return remapStructType(Ty);
    else if (isa<CompositeType>(SrcTy))
        dbgs() << "WARNING: unhandled type " << SrcTy->getTypeID() << ": " << *SrcTy << "\n";
    return SrcTy;
}

bool isSafeType(Type *SrcTy) {
    auto *DstTy = remapType(SrcTy);
    return (SrcTy == DstTy);
}


//
// Instruction rewriting
//

Instruction *rewriteInst(Instruction *SrcI);

AllocaInst *rewriteAllocaInst(AllocaInst *SrcI) {
    auto Ty = remapType(SrcI->getAllocatedType());
    auto I = new AllocaInst(Ty, SrcI->getArraySize());
    I->setAlignment(SrcI->getAlignment());
    return I;
}

GetElementPtrInst *rewriteGetElementPtrInst(GetElementPtrInst *SrcI) {
    auto Ty = remapType(SrcI->getSourceElementType());
    SmallVector<Value *, 4> Operands(SrcI->idx_begin(), SrcI->idx_end());
    if (SrcI->isInBounds())
        return GetElementPtrInst::CreateInBounds(Ty, SrcI->getPointerOperand(), Operands);
    else
        return GetElementPtrInst::Create(Ty, SrcI->getPointerOperand(), Operands);
}

BitCastInst *rewriteBitCastInst(BitCastInst *SrcI) {
    assert(isSafeType(SrcI->getOperand(0)->getType()));
    auto Ty = remapType(SrcI->getDestTy());
    dbgs() << "New bitcast will target " << *Ty << "\n";
    return new BitCastInst(SrcI->getOperand(0), Ty);
}

Instruction *rewriteInst(Instruction *SrcI)
{
    auto Ty = SrcI->getType();
    auto NewTy = remapType(Ty);
    if (Ty != NewTy) {
        SrcI->dump();
        if (auto I = dyn_cast<AllocaInst>(SrcI))
            return rewriteAllocaInst(I);
        else if (auto I = dyn_cast<GetElementPtrInst>(SrcI))
            return rewriteGetElementPtrInst(I);
        else if (auto I = dyn_cast<BitCastInst>(SrcI))
            return rewriteBitCastInst(I);
        else {
            dbgs() << "ERROR: unhandled instruction " << *SrcI << " produces value " << *Ty << " that needs rewriting\n";
            return nullptr;
        }
    } else {
        return nullptr;
    }
}

bool isSafeInst(Instruction *SrcI) {
    return isSafeType(SrcI->getType());
}


//
// Actual pass
//

struct StripJuliaAddrspaces : public ModulePass {
    static char ID;
    StripJuliaAddrspaces() : ModulePass(ID) {};

public:
    bool runOnModule(Module &M) override;

private:
    Function* redefine(Function* F);
    bool rewrite(Function* F);
};

Function* StripJuliaAddrspaces::redefine(Function* F) {
    auto *FTy = F->getFunctionType();
    auto *NFTy = remapFunctionType(FTy);
    if (FTy != NFTy) {
        dbgs() << "Redefining function " << F->getName() << " " << *FTy << " as " << *NFTy << "\n";

        // Create the new function...
        Function *NF = Function::Create(NFTy, F->getLinkage(), F->getName(), F->getParent());
        NF->copyAttributesFrom(F);

        // splice in basic blocks
        NF->getBasicBlockList().splice(NF->begin(), F->getBasicBlockList());

        // update arguments
        for (auto I = F->arg_begin(), E = F->arg_end(), I2 = NF->arg_begin(); I != E; ++I) {
            unsafeReplaceAllUsesWith(&*I, &*I2);
            I2->takeName(&*I);
            ++I2;
        }

        unsafeReplaceAllUsesWith(F, NF);
        NF->takeName(F);

        return NF;
    } else {
        return nullptr;
    }
}

bool StripJuliaAddrspaces::rewrite(Function *F) {
    SmallVector<Instruction*,4> Removals;
    bool Changed = false;

    for (auto &BB: *F) {
        for (auto &I: BB) {
            Instruction *NI = rewriteInst(&I);
            if (NI) {
                assert(isSafeInst(NI));
                NI->setDebugLoc(I.getDebugLoc());
                unsafeReplaceAllUsesWith(&I, NI);
                Removals.push_back(&I);
                NI->insertBefore(&I);
                NI->takeName(&I);
                Changed = true;
            }
        }
    }

    for (auto &I: Removals)
        I->eraseFromParent();

    return Changed;
}

bool StripJuliaAddrspaces::runOnModule(Module &M) {
    SmallVector<Function*,4> Removals;
    bool Changed = false;

    for (auto &F: M) {
        if (Function *NF = redefine(&F)) {
            Removals.push_back(&F);
            Changed = true;
            rewrite(NF);
            NF->dump();
        } else {
            Changed |= rewrite(&F);
            F.dump();
        }
    }

    for (auto &F: Removals)
        F->eraseFromParent();

    return Changed;
}

char StripJuliaAddrspaces::ID = 0;
static RegisterPass<StripJuliaAddrspaces> X("StripJuliaAddrspaces", "Strip (non-)rootedness information", false, false);

Pass *createStripJuliaAddrspaces() {
    return new StripJuliaAddrspaces();
}

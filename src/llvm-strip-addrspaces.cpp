// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/Constants.h>
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

template <typename T>
T* remapSequentialType(T* SrcTy) {
    Type* ElTy = remapType(SrcTy->getElementType());
    bool Changed = (ElTy != SrcTy->getElementType());
    return Changed ? T::get(ElTy, SrcTy->getNumElements()) : SrcTy;
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
    if (auto Ty = dyn_cast<PointerType>(SrcTy))
        return remapPointerType(Ty);
    else if (auto Ty = dyn_cast<FunctionType>(SrcTy))
        return remapFunctionType(Ty);
    else if (auto Ty = dyn_cast<VectorType>(SrcTy))
        return remapSequentialType(Ty);
    else if (auto Ty = dyn_cast<ArrayType>(SrcTy))
        return remapSequentialType(Ty);
    else if (auto Ty = dyn_cast<StructType>(SrcTy))
        return remapStructType(Ty);
    else if (isa<CompositeType>(SrcTy)) {
        SrcTy->dump();
        report_fatal_error("FATAL: unhandled composite type");
    }
    return SrcTy;
}

bool isSafeType(Type *SrcTy) {
    auto *DstTy = remapType(SrcTy);
    return (SrcTy == DstTy);
}

bool isSafeValue(Value *V) {
    return isSafeType(V->getType());
}



//
// Value rewriting
//

Constant *remapConst(Constant *SrcC);
Instruction *remapInst(Instruction *SrcI);

Value* remapValue(Value *SrcV)
{
    if (isSafeValue(SrcV))
        return SrcV;
    else if (auto V = dyn_cast<Instruction>(SrcV))
        return remapInst(V);
    else if (auto V = dyn_cast<Constant>(SrcV))
        return remapConst(V);
    else {
        SrcV->dump();
        report_fatal_error("FATAL: unhandled value that needs rewriting");
    }
}


// Constants

Constant *remapConstantExpr(ConstantExpr *SrcC)
{
    auto *Inst = remapInst(SrcC->getAsInstruction());
    assert(isSafeValue(Inst));
    // TODO: is there an easy way to reconstruct a ConstExpr from its equivalent Instr?
    if (auto I = dyn_cast<IntToPtrInst>(Inst))
        return ConstantExpr::getIntToPtr(cast<Constant>(I->getOperand(0)), I->getDestTy());
    else if (auto I = dyn_cast<AddrSpaceCastInst>(Inst))
        return ConstantExpr::getAddrSpaceCast(cast<Constant>(I->getOperand(0)), I->getDestTy());
    else if (auto I = dyn_cast<BitCastInst>(Inst))
        return ConstantExpr::getBitCast(cast<Constant>(I->getOperand(0)), I->getDestTy());
    else {
        Inst->dump();
        report_fatal_error("FATAL: unhandled constant expression type");
    }
}

Constant *remapConst(Constant *SrcC)
{
    if (isSafeValue(SrcC))
        return nullptr;
    else if (auto C = dyn_cast<ConstantExpr>(SrcC))
        return remapConstantExpr(C);
    else {
        SrcC->dump();
        report_fatal_error("FATAL: unhandled constant containing value that needs rewriting");
    }
}


// Instructions

AllocaInst *remapAllocaInst(AllocaInst *SrcI) {
    auto Ty = remapType(SrcI->getAllocatedType());
    auto I = new AllocaInst(Ty, SrcI->getArraySize());
    I->setAlignment(SrcI->getAlignment());
    return I;
}

GetElementPtrInst *remapGetElementPtrInst(GetElementPtrInst *SrcI) {
    auto Ty = remapType(SrcI->getSourceElementType());
    SmallVector<Value *, 4> Idxs(SrcI->idx_begin(), SrcI->idx_end());
    if (SrcI->isInBounds())
        return GetElementPtrInst::CreateInBounds(Ty, SrcI->getPointerOperand(), Idxs);
    else
        return GetElementPtrInst::Create(Ty, SrcI->getPointerOperand(), Idxs);
}

LoadInst *remapLoadInst(LoadInst *SrcI) {
    auto Op = SrcI->getOperand(0);
    assert(isSafeValue(Op));
    auto Ty = remapType(SrcI->getType());
    return new LoadInst(Ty, Op, "", SrcI->isVolatile(), SrcI->getAlignment(), SrcI->getOrdering(), SrcI->getSynchScope());
}

template <typename T>
T *remapCastInst(T *SrcI) {
    auto Op = SrcI->getOperand(0);
    assert(isSafeValue(Op));
    auto DstTy = remapType(SrcI->getDestTy());
    return new T(Op, DstTy);
}

CastInst *remapAddrSpaceCastInst(AddrSpaceCastInst *SrcI) {
    auto Op = SrcI->getOperand(0);
    auto SrcTy = Op->getType();
    assert(isSafeType(SrcTy));
    auto DstTy = remapType(SrcI->getDestTy());
    PointerType *SrcPtrTy = cast<PointerType>(SrcTy->getScalarType());
    PointerType *DstPtrTy = cast<PointerType>(DstTy->getScalarType());
    if (SrcPtrTy->getAddressSpace() == DstPtrTy->getAddressSpace())
        return new BitCastInst(Op, DstTy);
    else
        return new AddrSpaceCastInst(Op, DstTy);
}

CallInst *remapCallInst(CallInst *SrcI) {
    auto F = remapValue(SrcI->getCalledValue());
    auto FT = remapFunctionType(SrcI->getFunctionType());
    unsigned NumArgs = SrcI->getNumArgOperands();
    SmallVector<Value *, 4> Args(NumArgs);
    for (unsigned i = 0; i < NumArgs; ++i)
        Args[i] = remapValue(SrcI->getArgOperand(i));
    return CallInst::Create(FT, F, Args);   // TODO: OperandBundle?
}

Instruction *remapInst(Instruction *SrcI)
{
    if (isSafeValue(SrcI))
        return SrcI;
    else if (auto I = dyn_cast<AllocaInst>(SrcI))
        return remapAllocaInst(I);
    else if (auto I = dyn_cast<GetElementPtrInst>(SrcI))
        return remapGetElementPtrInst(I);
    else if (auto I = dyn_cast<LoadInst>(SrcI))
        return remapLoadInst(I);
    else if (auto I = dyn_cast<BitCastInst>(SrcI))
        return remapCastInst(I);
    else if (auto I = dyn_cast<AddrSpaceCastInst>(SrcI))
        return remapAddrSpaceCastInst(I);
    else if (auto I = dyn_cast<IntToPtrInst>(SrcI))
        return remapCastInst(I);
    else if (auto I = dyn_cast<CallInst>(SrcI))
        return remapCallInst(I);
    else {
        SrcI->dump();
        report_fatal_error("FATAL: unhandled instruction producing value that needs rewriting");
    }
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
        dbgs() << "Redefining " << F->getName() << " " << *FTy << " as " << *NFTy << "\n";

        // create the new function
        Function *NF = Function::Create(NFTy, F->getLinkage(), F->getName(), F->getParent());
        NF->copyAttributesFrom(F);
        if (F->hasPersonalityFn())
            NF->setPersonalityFn(F->getPersonalityFn());

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
        dbgs() << "Rewriting " << F->getName() << "\n";
        for (auto &I: BB) {
            I.dump();
            Instruction *NI = remapInst(&I);
            if (NI != &I) {
                assert(isSafeValue(NI));
                NI->setDebugLoc(I.getDebugLoc());
                unsafeReplaceAllUsesWith(&I, NI);
                Removals.push_back(&I);
                NI->insertBefore(&I);
                NI->takeName(&I);
                dbgs() << "  ->" << *NI << "\n";
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

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

struct StripJuliaAddrspaces : public ModulePass {
    static char ID;
    StripJuliaAddrspaces() : ModulePass(ID) {};

public:
    bool runOnFunction(Function &F);
    bool runOnModule(Module &M) override;
};

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

class TypeStripper
{
public:
    Type *remapType(Type *SrcTy)
    {
        // process all descendants from llvm::Type that contain other types
        if (auto Ty = dyn_cast<PointerType>(SrcTy))
            return remapType(Ty);
        if (auto Ty = dyn_cast<FunctionType>(SrcTy))
            return remapType(Ty);
        if (auto Ty = dyn_cast<VectorType>(SrcTy))
            return remapType(Ty);
        if (auto Ty = dyn_cast<ArrayType>(SrcTy))
            return remapType(Ty);
        if (auto Ty = dyn_cast<StructType>(SrcTy))
            return remapType(Ty);
        else if (isa<CompositeType>(SrcTy))
            dbgs() << "WARNING: unhandled type " << SrcTy->getTypeID() << ": " << *SrcTy << "\n";
        return SrcTy;
    }

    bool isSafeType(Type *SrcTy) {
        auto *DstTy = remapType(SrcTy);
        return (SrcTy == DstTy);
    }

    PointerType* remapType(PointerType* SrcTy) {
        Type* ElTy = remapType(SrcTy->getElementType());
        bool Changed = (ElTy != SrcTy->getElementType());
        unsigned AS = remapAS(SrcTy->getAddressSpace());
        Changed |= (AS != SrcTy->getAddressSpace());
        return Changed ? PointerType::get(ElTy, remapAS(SrcTy->getAddressSpace())) : SrcTy;
    }

    FunctionType* remapType(FunctionType *SrcTy) {
        auto *RetTy = remapType(SrcTy->getReturnType());

        auto Params = SrcTy->getNumParams();
        SmallVector<Type*,4> ParamTys(Params);
        bool Changed = false;
        for (unsigned i = 0; i < Params; ++i) {
            ParamTys[i] = remapType(SrcTy->getParamType(i));
            Changed |= (ParamTys[i] != SrcTy->getParamType(i));
        }

        return Changed ? FunctionType::get(RetTy, ParamTys, SrcTy->isVarArg()) : SrcTy;
    }

    VectorType* remapType(VectorType* SrcTy) {
        Type* ElTy = remapType(SrcTy->getElementType());
        bool Changed = (ElTy != SrcTy->getElementType());
        return Changed ? VectorType::get(ElTy, SrcTy->getNumElements()) : SrcTy;
    }

    ArrayType* remapType(ArrayType* SrcTy) {
        Type* ElTy = remapType(SrcTy->getElementType());
        bool Changed = (ElTy != SrcTy->getElementType());
        return Changed ? ArrayType::get(ElTy, SrcTy->getNumElements()) : SrcTy;
    }

    StructType* remapType(StructType* SrcTy) {
        auto Els = SrcTy->getNumElements();
        SmallVector<Type*,4> NewElTys(Els);
        bool Changed = false;
        for (unsigned i = 0; i < Els; ++i) {
            NewElTys[i] = remapType(SrcTy->getElementType(i));
            Changed |= (NewElTys[i] != SrcTy->getElementType(i));
        }

        return Changed ? StructType::create(SrcTy->getContext(), NewElTys, SrcTy->getName(), SrcTy->isPacked()) : SrcTy;
    }
};

class InstrStripper
{
public:
    // returns bool indicating whether the instruction has been remapped,
    // either returning through DstI, or setting it to null and modifying SrcI in-place
    Instruction *remapInstr(Instruction *SrcI)
    {
        auto Ty = SrcI->getType();
        auto NewTy = TypeMapper.remapType(Ty);
        if (Ty != NewTy) {
            SrcI->dump();
            if (auto I = dyn_cast<AllocaInst>(SrcI))
                return remapInstr(I);
            else if (auto I = dyn_cast<GetElementPtrInst>(SrcI))
                return remapInstr(I);
            else if (auto I = dyn_cast<BitCastInst>(SrcI))
                return remapInstr(I);
            else {
                dbgs() << "ERROR: unhandled instruction " << *SrcI << " produces value " << *Ty << " that needs rewriting\n";
                return nullptr;
            }
        } else {
            return nullptr;
        }
    }

    bool isSafeInstr(Instruction *SrcI) {
        return TypeMapper.isSafeType(SrcI->getType());
    }

private:
    AllocaInst *remapInstr(AllocaInst *SrcI) {
        auto Ty = TypeMapper.remapType(SrcI->getAllocatedType());
        auto I = new AllocaInst(Ty, SrcI->getArraySize());
        I->setAlignment(SrcI->getAlignment());
        return I;
    }

    GetElementPtrInst *remapInstr(GetElementPtrInst *SrcI) {
        auto Ty = TypeMapper.remapType(SrcI->getSourceElementType());
        SmallVector<Value *, 4> Operands(SrcI->idx_begin(), SrcI->idx_end());
        if (SrcI->isInBounds())
            return GetElementPtrInst::CreateInBounds(Ty, SrcI->getPointerOperand(), Operands);
        else
            return GetElementPtrInst::Create(Ty, SrcI->getPointerOperand(), Operands);
    }

    BitCastInst *remapInstr(BitCastInst *SrcI) {
        assert(TypeMapper.isSafeType(SrcI->getOperand(0)->getType()));
        auto Ty = TypeMapper.remapType(SrcI->getDestTy());
        dbgs() << "New bitcast will target " << *Ty << "\n";
        return new BitCastInst(SrcI->getOperand(0), Ty);
    }

    TypeStripper TypeMapper;
};

bool StripJuliaAddrspaces::runOnFunction(Function &F) {
    bool Changed = false;
    F.dump();

    SmallVector<std::pair<Instruction*,Instruction*>,4> Replacements;

    for (auto &BB: F) {
        for (auto &I: BB) {
            InstrStripper InstrMapper;
            Instruction *NewI = InstrMapper.remapInstr(&I);
            if (NewI) {
                assert(InstrMapper.isSafeInstr(NewI));
                dbgs() << "Rewriting instruction " << I << " with " << *NewI << "\n";

                unsafeReplaceAllUsesWith(&I, NewI);
                NewI->setDebugLoc(I.getDebugLoc());
                Changed = true;

                Replacements.push_back({&I, NewI});
            }
        }
    }

    // final modifications (to avoid iterator invalidation)
    for (auto &Is: Replacements) {
        auto *I = Is.first;
        auto *NewI = Is.second;

        std::string NewIName = I->getName();
        NewI->insertBefore(I);
        I->eraseFromParent();
        NewI->setName(NewIName);
    }

    return Changed;
}

bool StripJuliaAddrspaces::runOnModule(Module &M) {
    SmallVector<std::pair<Function*,Function*>,4> Replacements;

    bool Changed = false;

    for (auto &F: M) {
        TypeStripper TypeMapper;
        Function *NewF = &F;

        // replace function definitions
        auto *FTy = F.getFunctionType();
        auto *NewFTy = TypeMapper.remapType(FTy);
        if (FTy != NewFTy) {
            dbgs() << "Redefining function " << F.getName() << " " << *FTy << " as " << *NewFTy << "\n";

            // Create the new function...
            NewF = Function::Create(NewFTy, F.getLinkage(), F.getName(), F.getParent());

            // splice in basic blocks
            NewF->getBasicBlockList().splice(NewF->begin(), F.getBasicBlockList());

            // update arguments
            auto NewArg = NewF->arg_begin();
            for (auto Arg = F.arg_begin(), E = F.arg_end(); Arg != E; ++Arg) {
                unsafeReplaceAllUsesWith(&*Arg, &*(NewArg++));
            }

            unsafeReplaceAllUsesWith(&F, NewF);
            Changed = true;
        }

        // rewrite function IR
        Changed |= runOnFunction(*NewF);

        if (Changed)
            Replacements.push_back({&F, NewF});
    }

    // final modifications (to avoid iterator invalidation)
    for (auto &Fs: Replacements) {
        auto *F = Fs.first;
        auto *NewF = Fs.second;

        if (F != NewF) {
            std::string NewFName = F->getName();
            F->setName("");
            NewF->setName(NewFName);
            F->eraseFromParent();
        }

        // DEBUG
        NewF->dump();
        verifyFunction(*NewF);
    }

    return Changed;
}

char StripJuliaAddrspaces::ID = 0;
static RegisterPass<StripJuliaAddrspaces> X("StripJuliaAddrspaces", "Strip (non-)rootedness information", false, false);

Pass *createStripJuliaAddrspaces() {
    return new StripJuliaAddrspaces();
}

// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <llvm/IR/Module.h>
#include <llvm/Support/Debug.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/Transforms/Utils/ValueMapper.h>

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

class TypeStripper: public ValueMapTypeRemapper
{
public:
    Type *remapType(Type *SrcTy) override
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

    PointerType* remapType(PointerType* SrcTy) {
        Type* ElTy = remapType(SrcTy->getElementType());
        return PointerType::get(ElTy, remapAS(SrcTy->getAddressSpace()));
    }

    FunctionType* remapType(FunctionType *SrcTy) {
        auto *RetTy = remapType(SrcTy->getReturnType());

        auto Params = SrcTy->getNumParams();
        SmallVector<Type*,4> ParamTys(Params);
        for (unsigned i = 0; i < Params; ++i)
            ParamTys[i] = remapType(SrcTy->getParamType(i));

        return FunctionType::get(RetTy, ParamTys, SrcTy->isVarArg());
    }

    VectorType* remapType(VectorType* SrcTy) {
        Type* ElTy = remapType(SrcTy->getElementType());
        return VectorType::get(ElTy, SrcTy->getNumElements());
    }

    ArrayType* remapType(ArrayType* SrcTy) {
        Type* ElTy = remapType(SrcTy->getElementType());
        return ArrayType::get(ElTy, SrcTy->getNumElements());
    }

    StructType* remapType(StructType* SrcTy) {
        auto Els = SrcTy->getNumElements();
        SmallVector<Type*,4> NewElTys(Els);
        for (unsigned i = 0; i < Els; ++i)
            NewElTys[i] = remapType(SrcTy->getElementType(i));

        return StructType::create(SrcTy->getContext(), NewElTys, SrcTy->getName(), SrcTy->isPacked());
    }
};

class InstrStripper
{
public:
    Instruction *remapInstr(Instruction *SrcI)
    {
        auto Ty = SrcI->getType();
        auto NewTy = TypeMapper.remapType(Ty);
        if (Ty != NewTy) {
            if (auto I = dyn_cast<AllocaInst>(SrcI))
                return remapInstr(I);
            else {
                dbgs() << "ERROR: unhandled instruction " << *SrcI << " produces value " << *Ty << " that needs rewriting\n";
                return nullptr;
            }
        } else {
            return nullptr;
        }
    }

private:
    AllocaInst *remapInstr(AllocaInst *SrcI) {
        auto I = new AllocaInst(TypeMapper.remapType(SrcI->getType()), SrcI->getArraySize(), SrcI->getName());
        I->setAlignment(SrcI->getAlignment());
        return I;
    }

    TypeStripper TypeMapper;
};

bool StripJuliaAddrspaces::runOnFunction(Function &F) {
    bool Changed = false;

    SmallVector<std::pair<Instruction*,Instruction*>,4> Replacements;

    for (auto &BB: F) {
        for (auto &I: BB) {
            InstrStripper InstrMapper;
            auto *NewI = InstrMapper.remapInstr(&I);
            if (NewI) {
                Replacements.push_back({&I, NewI});
                dbgs() << "Rewriting instruction " << I << " with " << *NewI << "\n";
            }
        }
    }

    for (auto &Is: Replacements) {
        auto *I = Is.first;
        auto *NewI = Is.second;

        NewI->insertBefore(I);
        unsafeReplaceAllUsesWith(I, NewI);
        I->eraseFromParent();

        Changed = true;
    }

    return Changed;
}

bool StripJuliaAddrspaces::runOnModule(Module &M) {
    SmallVector<std::pair<Function*,Function*>,4> Replacements;

    bool Changed = false;

    for (auto &F: M) {
        TypeStripper TypeMapper;

        // rewrite the function IR
        Changed |= runOnFunction(F);
        if (Changed)
            F.dump();

        // replace the function definition
        auto *FTy = F.getFunctionType();
        auto *NewFTy = TypeMapper.remapType(FTy);
        if (FTy != NewFTy) {
            dbgs() << "Changing function " << F.getName() << " from " << *FTy << " to " << *NewFTy << "\n";
            ValueToValueMapTy VMap;

            // Create the new function...
            auto *NewF = Function::Create(NewFTy, F.getLinkage(), F.getName(), F.getParent());

            // Loop over the arguments, copying the names of the mapped arguments over...
            Function::arg_iterator DestI = NewF->arg_begin();
            for (Function::const_arg_iterator I = F.arg_begin(), E = F.arg_end(); I != E; ++I) {
                DestI->setName(I->getName());   // Copy the name over...
                VMap[&*I] = &*(DestI++);        // Add mapping to VMap
            }

            SmallVector<ReturnInst*, 8> Returns;  // Ignore returns cloned.
            CloneFunctionInto(NewF, &F, VMap, /*ModuleLevelChanges=*/false, Returns,
                              "", NULL, &TypeMapper);

            Replacements.push_back({&F, NewF});
        }
    }

    for (auto &Fs: Replacements) {
        auto *F = Fs.first;
        auto *NewF = Fs.second;

        std::string NewFName = F->getName();
        F->setName("");
        NewF->setName(NewFName);
        F->replaceAllUsesWith(NewF);
        F->eraseFromParent();

        Changed = true;
    }

    return Changed;
}

char StripJuliaAddrspaces::ID = 0;
static RegisterPass<StripJuliaAddrspaces> X("StripJuliaAddrspaces", "Strip (non-)rootedness information", false, false);

Pass *createStripJuliaAddrspaces() {
    return new StripJuliaAddrspaces();
}

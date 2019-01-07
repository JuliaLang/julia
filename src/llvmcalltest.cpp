// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "../src/support/platform.h"
#include "../src/support/dtypes.h"

#include "llvm/Config/llvm-config.h"
#include "llvm/IR/IRBuilder.h"

#include "codegen_shared.h"

using namespace llvm;

extern "C" {

JL_DLLEXPORT llvm::Function *MakeIdentityFunction(llvm::PointerType *AnyTy) {
    Type *TrackedTy = PointerType::get(AnyTy->getElementType(), AddressSpace::Tracked);
    Module *M = new llvm::Module("shadow", AnyTy->getContext());
    Function *F = Function::Create(
        FunctionType::get(
            TrackedTy, {TrackedTy}, false),
        llvm::GlobalValue::ExternalLinkage,
        "identity",
        M
    );

    IRBuilder<> Builder(BasicBlock::Create(AnyTy->getContext(), "top", F));
    Builder.CreateRet(&*F->arg_begin());

    return F;
}

}

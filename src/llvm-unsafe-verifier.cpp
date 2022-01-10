// This file is a part of Julia. License is MIT: https://julialang.org/license

// This LLVM pass verifies invariants required for correct GC root placement.
// See the devdocs for a description of these invariants.

#include "llvm-version.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

#include <llvm/IR/Value.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Pass.h>
#include <llvm/Support/Debug.h>

#include "codegen_shared.h"
#include "julia.h"
#include "julia_assert.h"
#include "llvm-pass-helpers.h"

#define DEBUG_TYPE "verify_unsafe"
#undef DEBUG

using namespace llvm;

struct UnsafeVerifier : public FunctionPass, private JuliaPassContext {
    static char ID;
    UnsafeVerifier() : FunctionPass(ID) {}

public:
    void getAnalysisUsage(AnalysisUsage &AU) const override {
        FunctionPass::getAnalysisUsage(AU);
        AU.setPreservesAll();
    }

    bool runOnFunction(Function &F) override;
};

bool UnsafeVerifier::runOnFunction(Function &F) {
    initFunctions(*F.getParent());
    // pgcstack_getter doesn't exist in this module -> safe
    if (!pgcstack_getter)
        return false;

    // pgcstack doesn't exist in this function -> safe
    llvm::Instruction* pgcstack = getPGCstack(F);
    if (!pgcstack)
        return false;
    
    // Check metadata
    MDNode *MD = F.getMetadata("julia.unsafe");
    if (!MD)
        return false;

    assert(MD->getNumOperands() == 1);
    auto* V = cast<ConstantAsMetadata>(MD->getOperand(0));
    auto C = cast<ConstantInt>(V->getValue())->getZExtValue();
    if (C == 0)
        return false;

    dbgs() << "Verifying unsafe function " << F.getName() << " failed\n";
    abort();

    return false;
}

char UnsafeVerifier::ID = 0;
static RegisterPass<UnsafeVerifier> X("UnsafeVerifier", "Julia Unsafe Verification Pass", false, false);

Pass *createUnsafeVerifierPass() {
    return new UnsafeVerifier();
}

extern "C" JL_DLLEXPORT void LLVMExtraAddUnsafeVerifierPass_impl(LLVMPassManagerRef PM)
{
    unwrap(PM)->add(createUnsafeVerifierPass());
}

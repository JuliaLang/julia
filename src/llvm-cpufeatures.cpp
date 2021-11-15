// This file is a part of Julia. License is MIT: https://julialang.org/license
//
// Lower intrinsics that expose subtarget information to the language. This makes it
// possible to write code that changes behavior based on, e.g., the availability of
// specific CPU features.
//
// The following intrinsics are supported:
// - julia.cpu.have_fma: returns 1 if the platform supports hardware-accelerated FMA
//
// XXX: can / do we want to make this a codegen pass to enable querying TargetPassConfig?

#include "llvm-version.h"

#include <llvm/IR/Module.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Support/Debug.h>

#include "julia.h"

#define DEBUG_TYPE "cpufeatures"

using namespace llvm;

extern TargetMachine *jl_TargetMachine;

namespace {

static void lowerHaveFMA(Function &F, Instruction *I) {
    Triple TheTriple = Triple(jl_TargetMachine->getTargetTriple());

    Attribute CPUAttr = F.getFnAttribute("target-cpu");
    Attribute FSAttr = F.getFnAttribute("target-features");

    StringRef CPU =
        CPUAttr.isValid() ? CPUAttr.getValueAsString() : jl_TargetMachine->getTargetCPU();
    StringRef FS =
        FSAttr.isValid() ? FSAttr.getValueAsString() : jl_TargetMachine->getTargetFeatureString();

    if (TheTriple.getArch() == Triple::x86_64 && FS.find("+fma") != StringRef::npos)
        I->replaceAllUsesWith(ConstantInt::get(I->getType(), 1));
    else
        I->replaceAllUsesWith(ConstantInt::get(I->getType(), 0));

    return;
}

static bool lowerCPUFeatures(Module &M)
{
    SmallVector<Instruction*,6> Materialized;
    if (auto have_fma = M.getFunction("julia.cpu.have_fma")) {
        for (Use &U: have_fma->uses()) {
            User *RU = U.getUser();
            Instruction *I = cast<Instruction>(RU);
            lowerHaveFMA(*I->getParent()->getParent(), I);
            Materialized.push_back(I);
        }
    }

    if (!Materialized.empty()) {
        for (auto I: Materialized) {
            I->eraseFromParent();
        }
        return true;
    } else {
        return false;
    }
}
}

struct CPUFeatures : PassInfoMixin<CPUFeatures> {
    PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM);
};

PreservedAnalyses CPUFeatures::run(Module &M, ModuleAnalysisManager &AM)
{
    lowerCPUFeatures(M);
    return PreservedAnalyses::all();
}

namespace {
struct CPUFeaturesLegacy : public ModulePass {
    static char ID;
    CPUFeaturesLegacy() : ModulePass(ID) {};

    bool runOnModule(Module &M)
    {
        return lowerCPUFeatures(M);
    }
};

char CPUFeaturesLegacy::ID = 0;
static RegisterPass<CPUFeaturesLegacy>
        Y("CPUFeatures",
          "Lower calls to CPU feature testing intrinsics.",
          false,
          false);
}

Pass *createCPUFeaturesPass()
{
    return new CPUFeaturesLegacy();
}

extern "C" JL_DLLEXPORT void LLVMExtraAddCPUFeaturesPass_impl(LLVMPassManagerRef PM)
{
    unwrap(PM)->add(createCPUFeaturesPass());
}

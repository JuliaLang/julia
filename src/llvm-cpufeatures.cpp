// This file is a part of Julia. License is MIT: https://julialang.org/license

// Lower intrinsics that expose subtarget information to the language. This makes it
// possible to write code that changes behavior based on, e.g., the availability of
// specific CPU features.
//
// The following intrinsics are supported:
// - julia.cpu.have_fma.$typ: returns 1 if the platform supports hardware-accelerated FMA.
//
// Some of these intrinsics are overloaded, i.e., they are suffixed with a type name.
// To extend support, make sure codegen (in intrinsics.cpp) knows how to emit them.
//
// XXX: can / do we want to make this a codegen pass to enable querying TargetPassConfig
//      instead of using the global target machine?

#include "llvm-version.h"
#include "passes.h"

#include <llvm/ADT/Statistic.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Support/Debug.h>

#include "julia.h"
#include "jitlayers.h"

#define DEBUG_TYPE "cpufeatures"

using namespace llvm;

STATISTIC(LoweredWithFMA, "Number of have_fma's that were lowered to true");
STATISTIC(LoweredWithoutFMA, "Number of have_fma's that were lowered to false");

extern JuliaOJIT *jl_ExecutionEngine;

// whether this platform unconditionally (i.e. without needing multiversioning) supports FMA
Optional<bool> always_have_fma(Function &intr) {
    auto intr_name = intr.getName();
    auto typ = intr_name.substr(strlen("julia.cpu.have_fma."));

#if defined(_CPU_AARCH64_)
    return typ == "f32" || typ == "f64";
#else
    (void)typ;
    return {};
#endif
}

bool have_fma(Function &intr, Function &caller) {
    auto unconditional = always_have_fma(intr);
    if (unconditional.hasValue())
        return unconditional.getValue();

    auto intr_name = intr.getName();
    auto typ = intr_name.substr(strlen("julia.cpu.have_fma."));

    Attribute FSAttr = caller.getFnAttribute("target-features");
    StringRef FS =
        FSAttr.isValid() ? FSAttr.getValueAsString() : jl_ExecutionEngine->getTargetFeatureString();

    SmallVector<StringRef, 6> Features;
    FS.split(Features, ',');
    for (StringRef Feature : Features)
#if defined _CPU_ARM_
      if (Feature == "+vfp4")
        return typ == "f32" || typ == "f64";
      else if (Feature == "+vfp4sp")
        return typ == "f32";
#else
      if (Feature == "+fma" || Feature == "+fma4")
        return typ == "f32" || typ == "f64";
#endif

    return false;
}

void lowerHaveFMA(Function &intr, Function &caller, CallInst *I) {
    if (have_fma(intr, caller)) {
        ++LoweredWithFMA;
        I->replaceAllUsesWith(ConstantInt::get(I->getType(), 1));
    } else {
        ++LoweredWithoutFMA;
        I->replaceAllUsesWith(ConstantInt::get(I->getType(), 0));
    }
    return;
}

bool lowerCPUFeatures(Module &M)
{
    SmallVector<Instruction*,6> Materialized;

    for (auto &F: M.functions()) {
        auto FN = F.getName();

        if (FN.startswith("julia.cpu.have_fma.")) {
            for (Use &U: F.uses()) {
                User *RU = U.getUser();
                CallInst *I = cast<CallInst>(RU);
                lowerHaveFMA(F, *I->getParent()->getParent(), I);
                Materialized.push_back(I);
            }
        }
    }

    if (!Materialized.empty()) {
        for (auto I: Materialized) {
            I->eraseFromParent();
        }
        assert(!verifyModule(M));
        return true;
    } else {
        return false;
    }
}

PreservedAnalyses CPUFeatures::run(Module &M, ModuleAnalysisManager &AM)
{
    if (lowerCPUFeatures(M)) {
        return PreservedAnalyses::allInSet<CFGAnalyses>();
    }
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

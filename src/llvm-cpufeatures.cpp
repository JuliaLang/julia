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

#include <llvm/CodeGen/MachineModuleInfo.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Support/Debug.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Passes/PassPlugin.h>


#include "julia.h"
extern "C" int32_t (*jl_sysimg_cpuflags[3])(void);

#define DEBUG_TYPE "cpufeatures"

using namespace llvm;

extern TargetMachine *jl_TargetMachine;

// whether this platform unconditionally (i.e. without needing multiversioning) supports FMA
Optional<bool> always_have_fma(Function &intr) {
    auto intr_name = intr.getName();
    auto typ = intr_name.substr(strlen("julia.cpu.have_fma."));

    // if we are using a sysimage, return that constant
    if (typ == "f16" && jl_sysimg_cpuflags[0] != NULL)
        return jl_sysimg_cpuflags[0]();
    if (typ == "f32" && jl_sysimg_cpuflags[1] != NULL)
        return jl_sysimg_cpuflags[1]();
    if (typ == "f64" && jl_sysimg_cpuflags[2] != NULL)
        return jl_sysimg_cpuflags[2]();

#if defined(_CPU_AARCH64_)
    return typ == "f32" || typ == "f64";
#else
    (void)typ;
    return {};
#endif
}

bool have_fma(const TargetMachine &TM, Function &intr, Function &caller) {
    auto unconditional = always_have_fma(intr);
    if (unconditional.hasValue())
        return unconditional.getValue();

    auto intr_name = intr.getName();
    auto typ = intr_name.substr(strlen("julia.cpu.have_fma."));

    // otherwise, examine the target-features of the compile unit (JIT or AOT)
    Attribute FSAttr = caller.getFnAttribute("target-features");
    StringRef FS =
        FSAttr.isValid() ? FSAttr.getValueAsString() : TM.getTargetFeatureString();

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

void lowerHaveFMA(const TargetMachine &TM, Function &intr, Function &caller, CallInst *I) {
    if (have_fma(TM, intr, caller))
        I->replaceAllUsesWith(ConstantInt::get(I->getType(), 1));
    else
        I->replaceAllUsesWith(ConstantInt::get(I->getType(), 0));

    return;
}

bool lowerCPUFeatures(const TargetMachine &TM, Module &M)
{
    SmallVector<Instruction*,6> Materialized;

    for (auto &F: M.functions()) {
        auto FN = F.getName();

        if (FN.startswith("julia.cpu.have_fma.")) {
            for (Use &U: F.uses()) {
                User *RU = U.getUser();
                CallInst *I = cast<CallInst>(RU);
                lowerHaveFMA(TM, F, *I->getParent()->getParent(), I);
                Materialized.push_back(I);
            }
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

struct CPUFeaturesPass : public PassInfoMixin<CPUFeaturesPass> {
  static void registerCallbacks(PassBuilder &PB) {
    PB.registerPipelineParsingCallback(
        [](StringRef Name, ModulePassManager &PM,
           ArrayRef<PassBuilder::PipelineElement> InnerPipeline) {
          if (Name == "CPUFeatures") {
            PM.addPass(CPUFeaturesPass());
            return true;
          }
          return false;
        });
  }

  PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM);
};


PreservedAnalyses CPUFeaturesPass::run(Module &M, ModuleAnalysisManager &AM)
{
    auto &MMI = AM.getResult<MachineModuleAnalysis>(M);
    auto &TM = MMI.getTarget();
    lowerCPUFeatures(TM, M);
    return PreservedAnalyses::all();
}

extern "C" JL_DLLEXPORT ::llvm::PassPluginLibraryInfo
llvmGetPassPluginInfo() {
  return {LLVM_PLUGIN_API_VERSION, "CPUFeatures", "1",
          CPUFeaturesPass::registerCallbacks};
}


namespace {
struct CPUFeaturesLegacy : public ModulePass {
    static char ID;
    CPUFeaturesLegacy() : ModulePass(ID) {};

    bool runOnModule(Module &M)
    {
        auto &MMI = getAnalysis<MachineModuleInfoWrapperPass>().getMMI();
        auto &TM = MMI.getTarget();
        return lowerCPUFeatures(TM, M);
    }

    void getAnalysisUsage(AnalysisUsage &AU) const override {
        AU.addRequired<MachineModuleInfoWrapperPass>();
        AU.setPreservesAll();
        ModulePass::getAnalysisUsage(AU);
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

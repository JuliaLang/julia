// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_PASSES_H
#define JL_PASSES_H

#include <llvm/IR/PassManager.h>

using namespace llvm;

// Function Passes
struct DemoteFloat16 : PassInfoMixin<DemoteFloat16> {
    PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM);
};

struct CombineMulAdd : PassInfoMixin<CombineMulAdd> {
    PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM);
};

struct LateLowerGC : PassInfoMixin<LateLowerGC> {
    PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM);
};

// Module Passes
struct CPUFeatures : PassInfoMixin<CPUFeatures> {
    PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM);
};

struct RemoveNI : PassInfoMixin<RemoveNI> {
    PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM);
};

struct LowerSIMDLoop : PassInfoMixin<LowerSIMDLoop> {
    PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM);
};

#endif

// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_PASSES_H
#define JL_PASSES_H

#include "analyzer_annotations.h"
#include <llvm/IR/PassManager.h>
#include <llvm/Transforms/Scalar/LoopPassManager.h>

using namespace llvm;

// Function Passes
struct DemoteFloat16Pass : PassInfoMixin<DemoteFloat16Pass> {
    PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM) JL_NOTSAFEPOINT;
    static bool isRequired() { return true; }
};

struct CombineMulAddPass : PassInfoMixin<CombineMulAddPass> {
    PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM) JL_NOTSAFEPOINT;
};

struct LateLowerGCPass : PassInfoMixin<LateLowerGCPass> {
    PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM) JL_NOTSAFEPOINT;
    static bool isRequired() { return true; }
};

struct AllocOptPass : PassInfoMixin<AllocOptPass> {
    PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM) JL_NOTSAFEPOINT;
};

struct PropagateJuliaAddrspacesPass : PassInfoMixin<PropagateJuliaAddrspacesPass> {
    PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM) JL_NOTSAFEPOINT;
    static bool isRequired() { return true; }
};

struct LowerExcHandlersPass : PassInfoMixin<LowerExcHandlersPass> {
    PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM) JL_NOTSAFEPOINT;
    static bool isRequired() { return true; }
};

struct GCInvariantVerifierPass : PassInfoMixin<GCInvariantVerifierPass> {
    bool Strong;
    GCInvariantVerifierPass(bool Strong = false) JL_NOTSAFEPOINT : Strong(Strong) {}

    PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM) JL_NOTSAFEPOINT;
    static bool isRequired() { return true; }
};

// Module Passes
struct CPUFeaturesPass : PassInfoMixin<CPUFeaturesPass> {
    PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM) JL_NOTSAFEPOINT;
    static bool isRequired() { return true; }
};

struct RemoveNIPass : PassInfoMixin<RemoveNIPass> {
    PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM) JL_NOTSAFEPOINT;
    static bool isRequired() { return true; }
};

struct LowerSIMDLoopPass : PassInfoMixin<LowerSIMDLoopPass> {
    PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM) JL_NOTSAFEPOINT;
    static bool isRequired() { return true; }
};

struct FinalLowerGCPass : PassInfoMixin<FinalLowerGCPass> {
    PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM) JL_NOTSAFEPOINT;
    static bool isRequired() { return true; }
};

struct MultiVersioningPass : PassInfoMixin<MultiVersioningPass> {
    bool external_use;
    MultiVersioningPass(bool external_use = false) : external_use(external_use) {}
    PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM) JL_NOTSAFEPOINT;
    static bool isRequired() { return true; }
};

struct RemoveJuliaAddrspacesPass : PassInfoMixin<RemoveJuliaAddrspacesPass> {
    PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM) JL_NOTSAFEPOINT;
    static bool isRequired() { return true; }
};

struct RemoveAddrspacesPass : PassInfoMixin<RemoveAddrspacesPass> {
    std::function<unsigned(unsigned)> ASRemapper;
    RemoveAddrspacesPass() JL_NOTSAFEPOINT;
    RemoveAddrspacesPass(std::function<unsigned(unsigned)> ASRemapper) JL_NOTSAFEPOINT : ASRemapper(std::move(ASRemapper)) {}
    ~RemoveAddrspacesPass() JL_NOTSAFEPOINT = default;

    PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM) JL_NOTSAFEPOINT;
    static bool isRequired() { return true; }
};

struct LowerPTLSPass : PassInfoMixin<LowerPTLSPass> {
    bool imaging_mode;
    LowerPTLSPass(bool imaging_mode=false) JL_NOTSAFEPOINT : imaging_mode(imaging_mode) {}

    PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM) JL_NOTSAFEPOINT;
    static bool isRequired() { return true; }
};

// Loop Passes
struct JuliaLICMPass : PassInfoMixin<JuliaLICMPass> {
    PreservedAnalyses run(Loop &L, LoopAnalysisManager &AM,
                          LoopStandardAnalysisResults &AR, LPMUpdater &U) JL_NOTSAFEPOINT;
};

#endif

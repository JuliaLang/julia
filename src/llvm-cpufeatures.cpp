// This file is a part of Julia. License is MIT: https://julialang.org/license

// Lower intrinsics that expose subtarget information to the language so
// that Julia code can dispatch on CPU features at compile time.
//
// Intrinsics:
// - julia.cpu.have_fma.$typ:  hardware FMA support for $typ (f32/f64).
// - julia.cpu.supports.$feat: LLVM target feature $feat (Base.@cpu_supports).
//
// Codegen (intrinsics.cpp) emits these as placeholder calls; this pass
// folds each using the call site's effective MCSubtargetInfo.

#include "llvm-version.h"
#include "passes.h"

#include <llvm/ADT/Statistic.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Verifier.h>
#include <llvm/ADT/StringMap.h>
#include <llvm/CodeGen/TargetSubtargetInfo.h>
#include <llvm/MC/MCSubtargetInfo.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Support/Debug.h>

#define DEBUG_TYPE "cpufeatures"

using namespace llvm;

STATISTIC(LoweredWithFMA, "Number of have_fma's that were lowered to true");
STATISTIC(LoweredWithoutFMA, "Number of have_fma's that were lowered to false");
STATISTIC(LoweredCPUSupports, "Number of cpu_supports calls lowered to a constant");

// True iff the triple guarantees FMA without needing multiversioning;
// used by MultiVersioning to skip clone emission for have_fma callers.
std::optional<bool> always_have_fma(Function &intr, const Triple &TT) JL_NOTSAFEPOINT
{
    if (TT.isAArch64()) {
        StringRef typ = intr.getName().substr(strlen("julia.cpu.have_fma."));
        return typ == "f32" || typ == "f64";
    }
    return std::nullopt;
}

static bool have_fma(StringRef typ, const MCSubtargetInfo &STI, const Triple &TT) JL_NOTSAFEPOINT
{
    if (TT.isAArch64())
        return typ == "f32" || typ == "f64";
    if (TT.isARM()) {
        // ARM (32-bit): VFPv4 provides FMA on supported types
        if (STI.checkFeatures("+vfp4"))
            return typ == "f32" || typ == "f64";
        if (STI.checkFeatures("+vfp4sp"))
            return typ == "f32";
        return false;
    }
    if (TT.isX86()) {
        if (STI.checkFeatures("+fma") || STI.checkFeatures("+fma4"))
            return typ == "f32" || typ == "f64";
        return false;
    }
    return false;
}

// Per-(CPU, features) MCSubtargetInfo cache, used when the pass runs
// without a TargetMachine (e.g. `opt -passes=CPUFeatures`).
namespace {
struct STIBuilder {
    const Triple &TT;
    const Target *T = nullptr;
    StringMap<std::unique_ptr<MCSubtargetInfo>> cache;

    explicit STIBuilder(const Triple &TT) : TT(TT) {
        std::string Err;
        T = TargetRegistry::lookupTarget(TT.str(), Err);
    }

    const MCSubtargetInfo *get(const Function &F) {
        if (!T)
            return nullptr;
        StringRef CPU = F.getFnAttribute("target-cpu").getValueAsString();
        StringRef FS = F.getFnAttribute("target-features").getValueAsString();
        SmallString<256> key(CPU);
        key += '\0';
        key += FS;
        auto &slot = cache[key];
        if (!slot)
            slot.reset(T->createMCSubtargetInfo(TT.str(), CPU, FS));
        return slot.get();
    }
};
}

static const MCSubtargetInfo *getFunctionSubtarget(
        const Function &F,
        const TargetMachine *TM,
        STIBuilder &fallback) JL_NOTSAFEPOINT
{
    if (TM)
        return TM->getSubtargetImpl(F);
    return fallback.get(F);
}

bool lowerCPUFeatures(Module &M, const TargetMachine *TM) JL_NOTSAFEPOINT
{
    Triple TT = Triple(M.getTargetTriple());
    SmallVector<Instruction*, 6> Materialized;
    STIBuilder fallback(TT);

    for (auto &F : M.functions()) {
        StringRef FN = F.getName();

        if (FN.starts_with("julia.cpu.have_fma.")) {
            StringRef typ = FN.substr(strlen("julia.cpu.have_fma."));
            for (Use &U : F.uses()) {
                CallInst *I = cast<CallInst>(U.getUser());
                Function *caller = I->getFunction();
                const MCSubtargetInfo *STI = getFunctionSubtarget(*caller, TM, fallback);
                bool result = STI && have_fma(typ, *STI, TT);
                if (result)
                    ++LoweredWithFMA;
                else
                    ++LoweredWithoutFMA;
                I->replaceAllUsesWith(ConstantInt::get(I->getType(), result ? 1 : 0));
                Materialized.push_back(I);
            }
        }
        else if (FN.starts_with("julia.cpu.supports.")) {
            StringRef feat = FN.substr(strlen("julia.cpu.supports."));
            SmallString<64> query("+");
            query += feat;
            for (Use &U : F.uses()) {
                CallInst *I = cast<CallInst>(U.getUser());
                Function *caller = I->getFunction();
                const MCSubtargetInfo *STI = getFunctionSubtarget(*caller, TM, fallback);
                // checkFeatures matches unknown names vacuously; guard via the table.
                bool known = false;
                if (STI) {
                    for (const auto &kv : STI->getAllProcessorFeatures()) {
                        if (kv.Key == feat) { known = true; break; }
                    }
                }
                bool result = known && STI->checkFeatures(query);
                ++LoweredCPUSupports;
                I->replaceAllUsesWith(ConstantInt::get(I->getType(), result ? 1 : 0));
                Materialized.push_back(I);
            }
        }
    }

    if (Materialized.empty())
        return false;
    for (auto I : Materialized)
        I->eraseFromParent();
#ifdef JL_VERIFY_PASSES
    assert(!verifyLLVMIR(M));
#endif
    return true;
}

PreservedAnalyses CPUFeaturesPass::run(Module &M, ModuleAnalysisManager &AM)
{
    if (lowerCPUFeatures(M, TM))
        return PreservedAnalyses::allInSet<CFGAnalyses>();
    return PreservedAnalyses::all();
}

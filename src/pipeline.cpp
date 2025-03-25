// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <llvm-version.h>
#include "platform.h"

//We don't care about uninitialized variables in LLVM; that's LLVM's problem
#ifdef _COMPILER_GCC_
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmaybe-uninitialized"
#endif

// analysis passes
#include <llvm/Analysis/Passes.h>
#include <llvm/Analysis/BasicAliasAnalysis.h>
#include <llvm/Analysis/GlobalsModRef.h>
#include <llvm/Analysis/TargetTransformInfo.h>
#include <llvm/Analysis/TypeBasedAliasAnalysis.h>
#include <llvm/Analysis/ScopedNoAliasAA.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Transforms/IPO/InferFunctionAttrs.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Passes/PassPlugin.h>

// NewPM needs to manually include all the pass headers
#include <llvm/Transforms/AggressiveInstCombine/AggressiveInstCombine.h>
#include <llvm/Transforms/IPO/AlwaysInliner.h>
#include <llvm/Transforms/IPO/Annotation2Metadata.h>
#include <llvm/Transforms/IPO/ConstantMerge.h>
#include <llvm/Transforms/IPO/ForceFunctionAttrs.h>
#include <llvm/Transforms/IPO/GlobalDCE.h>
#include <llvm/Transforms/IPO/GlobalOpt.h>
#include <llvm/Transforms/IPO/StripDeadPrototypes.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Instrumentation/AddressSanitizer.h>
#include <llvm/Transforms/Instrumentation/MemorySanitizer.h>
#include <llvm/Transforms/Instrumentation/ThreadSanitizer.h>
#include <llvm/Transforms/Scalar/ADCE.h>
#include <llvm/Transforms/Scalar/AnnotationRemarks.h>
#include <llvm/Transforms/Scalar/BDCE.h>
#include "llvm/Transforms/Scalar/ConstraintElimination.h"
#include <llvm/Transforms/Scalar/CorrelatedValuePropagation.h>
#include <llvm/Transforms/Scalar/DCE.h>
#include <llvm/Transforms/Scalar/DeadStoreElimination.h>
#include <llvm/Transforms/Scalar/DivRemPairs.h>
#include <llvm/Transforms/Scalar/EarlyCSE.h>
#include <llvm/Transforms/Scalar/Float2Int.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Scalar/IndVarSimplify.h>
#include <llvm/Transforms/Scalar/InductiveRangeCheckElimination.h>
#include <llvm/Transforms/Scalar/InstSimplifyPass.h>
#include <llvm/Transforms/Scalar/JumpThreading.h>
#include <llvm/Transforms/Scalar/LICM.h>
#include <llvm/Transforms/Scalar/LoopDeletion.h>
#include <llvm/Transforms/Scalar/LoopDistribute.h>
#include <llvm/Transforms/Scalar/LoopIdiomRecognize.h>
#include <llvm/Transforms/Scalar/LoopInstSimplify.h>
#include <llvm/Transforms/Scalar/LoopLoadElimination.h>
#include <llvm/Transforms/Scalar/LoopRotation.h>
#include <llvm/Transforms/Scalar/LoopSimplifyCFG.h>
#include <llvm/Transforms/Scalar/LoopUnrollPass.h>
#include <llvm/Transforms/Scalar/LowerConstantIntrinsics.h>
#include <llvm/Transforms/Scalar/LowerExpectIntrinsic.h>
#include <llvm/Transforms/Scalar/MemCpyOptimizer.h>
#include <llvm/Transforms/Scalar/MergedLoadStoreMotion.h>
#include <llvm/Transforms/Scalar/Reassociate.h>
#include <llvm/Transforms/Scalar/SCCP.h>
#include <llvm/Transforms/Scalar/SROA.h>
#include <llvm/Transforms/Scalar/SimpleLoopUnswitch.h>
#include <llvm/Transforms/Scalar/SimplifyCFG.h>
#include <llvm/Transforms/Scalar/WarnMissedTransforms.h>
#include <llvm/Transforms/Utils/LibCallsShrinkWrap.h>
#include <llvm/Transforms/Utils/InjectTLIMappings.h>
#include <llvm/Transforms/Utils/Mem2Reg.h>
#include <llvm/Transforms/Utils/RelLookupTableConverter.h>
#include <llvm/Transforms/Utils/ModuleUtils.h>
#include <llvm/Transforms/Utils/SimplifyCFGOptions.h>
#include <llvm/Transforms/Vectorize/LoopVectorize.h>
#include <llvm/Transforms/Vectorize/SLPVectorizer.h>
#include <llvm/Transforms/Vectorize/VectorCombine.h>
#ifdef _COMPILER_GCC_
#pragma GCC diagnostic pop
#endif

#include <llvm/Target/TargetMachine.h>

#include "julia.h"
#include "julia_internal.h"
#include "jitlayers.h"
#include "julia_assert.h"
#include "passes.h"

using namespace llvm;

namespace {
    //Shamelessly stolen from Clang's approach to sanitizers
    //TODO do we want to enable other sanitizers?
    static void addSanitizerPasses(ModulePassManager &MPM, OptimizationLevel O) JL_NOTSAFEPOINT {
        // Coverage sanitizer
        // if (CodeGenOpts.hasSanitizeCoverage()) {
        //   auto SancovOpts = getSancovOptsFromCGOpts(CodeGenOpts);
        //   MPM.addPass(ModuleSanitizerCoveragePass(
        //       SancovOpts, CodeGenOpts.SanitizeCoverageAllowlistFiles,
        //       CodeGenOpts.SanitizeCoverageIgnorelistFiles));
        // }

    #ifdef _COMPILER_MSAN_ENABLED_
        auto MSanPass = [&](/*SanitizerMask Mask, */bool CompileKernel) JL_NOTSAFEPOINT {
        // if (LangOpts.Sanitize.has(Mask)) {
            // int TrackOrigins = CodeGenOpts.SanitizeMemoryTrackOrigins;
            // bool Recover = CodeGenOpts.SanitizeRecover.has(Mask);

            // MemorySanitizerOptions options(TrackOrigins, Recover, CompileKernel,{
            //                             CodeGenOpts.SanitizeMemoryParamRetval);
            MemorySanitizerOptions options;
            MPM.addPass(ModuleMemorySanitizerPass(options));
            FunctionPassManager FPM;
            FPM.addPass(MemorySanitizerPass(options));
            if (O != OptimizationLevel::O0) {
            // MemorySanitizer inserts complex instrumentation that mostly
            // follows the logic of the original code, but operates on
            // "shadow" values. It can benefit from re-running some
            // general purpose optimization passes.
            FPM.addPass(EarlyCSEPass());
            // TODO: Consider add more passes like in
            // addGeneralOptsForMemorySanitizer. EarlyCSEPass makes visible
            // difference on size. It's not clear if the rest is still
            // useful. InstCombinePass breaks
            // compiler-rt/test/msan/select_origin.cpp.
            }
            MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
        // }
        };
        MSanPass(/*SanitizerKind::Memory, */false);
        // MSanPass(SanitizerKind::KernelMemory, true);
    #endif

    #ifdef _COMPILER_TSAN_ENABLED_
        // if (LangOpts.Sanitize.has(SanitizerKind::Thread)) {
        MPM.addPass(ModuleThreadSanitizerPass());
        MPM.addPass(createModuleToFunctionPassAdaptor(ThreadSanitizerPass()));
        // }
    #endif


    #ifdef _COMPILER_ASAN_ENABLED_
        auto ASanPass = [&](/*SanitizerMask Mask, */bool CompileKernel) JL_NOTSAFEPOINT {
        //   if (LangOpts.Sanitize.has(Mask)) {
            // bool UseGlobalGC = asanUseGlobalsGC(TargetTriple, CodeGenOpts);
            // bool UseOdrIndicator = CodeGenOpts.SanitizeAddressUseOdrIndicator;
            // llvm::AsanDtorKind DestructorKind =
            //     CodeGenOpts.getSanitizeAddressDtor();
            // AddressSanitizerOptions Opts;
            // Opts.CompileKernel = CompileKernel;
            // Opts.Recover = CodeGenOpts.SanitizeRecover.has(Mask);
            // Opts.UseAfterScope = CodeGenOpts.SanitizeAddressUseAfterScope;
            // Opts.UseAfterReturn = CodeGenOpts.getSanitizeAddressUseAfterReturn();
            // MPM.addPass(RequireAnalysisPass<ASanGlobalsMetadataAnalysis, Module>());
            //Let's assume the defaults are actually fine for our purposes
            // MPM.addPass(AddressSanitizerPass(
            //     Opts, UseGlobalGC, UseOdrIndicator, DestructorKind));
            MPM.addPass(AddressSanitizerPass(AddressSanitizerOptions(), true, false));
        //   }
        };
        ASanPass(/*SanitizerKind::Address, */false);
        // ASanPass(SanitizerKind::KernelAddress, true);
    #endif

        // auto HWASanPass = [&](SanitizerMask Mask, bool CompileKernel) {
        //   if (LangOpts.Sanitize.has(Mask)) {
        //     bool Recover = CodeGenOpts.SanitizeRecover.has(Mask);
        //     MPM.addPass(HWAddressSanitizerPass(
        //         {CompileKernel, Recover,
        //          /*DisableOptimization=*/CodeGenOpts.OptimizationLevel == 0}));
        //   }
        // };
        // HWASanPass(/*SanitizerKind::HWAddress, */false);
        // // HWASanPass(SanitizerKind::KernelHWAddress, true);

        // if (LangOpts.Sanitize.has(SanitizerKind::DataFlow)) {
        //   MPM.addPass(DataFlowSanitizerPass(LangOpts.NoSanitizeFiles));
        // }
    }

#ifdef JL_VERIFY_PASSES
    static inline void addVerificationPasses(ModulePassManager &MPM, bool llvm_only) JL_NOTSAFEPOINT {
        if (!llvm_only){
            MPM.addPass(llvm::createModuleToFunctionPassAdaptor(GCInvariantVerifierPass(true)));
        }
        MPM.addPass(VerifierPass());
    }
#endif

    auto basicSimplifyCFGOptions() JL_NOTSAFEPOINT {
        return SimplifyCFGOptions()
            .convertSwitchRangeToICmp(true)
            .convertSwitchToLookupTable(true)
            .forwardSwitchCondToPhi(true);
    }

    auto aggressiveSimplifyCFGOptions() JL_NOTSAFEPOINT {
        return SimplifyCFGOptions()
            .convertSwitchRangeToICmp(true)
            .convertSwitchToLookupTable(true)
            .forwardSwitchCondToPhi(true)
            .needCanonicalLoops(false)
            .hoistCommonInsts(true)
            .sinkCommonInsts(true)
            ;
    }

// At any given time exactly one of each pair of overloads is strictly unused
#ifdef _COMPILER_GCC_
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function"
#endif

#ifdef _COMPILER_CLANG_
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-function"
#endif

    // Version check for our patch to allow invoking pipeline callbacks
    // won't work if built with our LLVM but linked with system LLVM
    template<typename PB> std::true_type hasInvokeCallbacks_helper(decltype(&PB::invokePipelineStartEPCallbacks)) JL_NOTSAFEPOINT;
    std::false_type hasInvokeCallbacks_helper(...) JL_NOTSAFEPOINT;

    // static constexpr bool hasInvokeCallbacks = decltype(hasInvokeCallbacks_helper<PassBuilder>(nullptr))::value;

    //If PB is a nullptr, don't invoke anything (this happens when running julia from opt)
    template<typename PB_t>
    std::enable_if_t<decltype(hasInvokeCallbacks_helper<PB_t>(nullptr))::value, void> invokePipelineStartCallbacks(ModulePassManager &MPM, PB_t *PB, OptimizationLevel O) JL_NOTSAFEPOINT {
        static_assert(std::is_same<PassBuilder, PB_t>::value, "Expected PassBuilder as second argument!");
        if (!PB) return;
        PB->invokePipelineStartEPCallbacks(MPM, O);
    }
    template<typename PB_t>
    std::enable_if_t<decltype(hasInvokeCallbacks_helper<PB_t>(nullptr))::value, void> invokePeepholeEPCallbacks(FunctionPassManager &FPM, PB_t *PB, OptimizationLevel O) JL_NOTSAFEPOINT {
        static_assert(std::is_same<PassBuilder, PB_t>::value, "Expected PassBuilder as second argument!");
        if (!PB) return;
        PB->invokePeepholeEPCallbacks(FPM, O);
    }
    template<typename PB_t>
    std::enable_if_t<decltype(hasInvokeCallbacks_helper<PB_t>(nullptr))::value, void> invokeEarlySimplificationCallbacks(ModulePassManager &MPM, PB_t *PB, OptimizationLevel O) JL_NOTSAFEPOINT {
        static_assert(std::is_same<PassBuilder, PB_t>::value, "Expected PassBuilder as second argument!");
        if (!PB) return;
#if JL_LLVM_VERSION >= 200000
        PB->invokePipelineEarlySimplificationEPCallbacks(MPM, O, ThinOrFullLTOPhase::None);
#else
        PB->invokePipelineEarlySimplificationEPCallbacks(MPM, O);
#endif
    }
    template<typename PB_t>
    std::enable_if_t<decltype(hasInvokeCallbacks_helper<PB_t>(nullptr))::value, void> invokeCGSCCCallbacks(CGSCCPassManager &CGPM, PB_t *PB, OptimizationLevel O) JL_NOTSAFEPOINT {
        static_assert(std::is_same<PassBuilder, PB_t>::value, "Expected PassBuilder as second argument!");
        if (!PB) return;
        PB->invokeCGSCCOptimizerLateEPCallbacks(CGPM, O);
    }
    template<typename PB_t>
    std::enable_if_t<decltype(hasInvokeCallbacks_helper<PB_t>(nullptr))::value, void> invokeOptimizerEarlyCallbacks(ModulePassManager &MPM, PB_t *PB, OptimizationLevel O) JL_NOTSAFEPOINT {
        static_assert(std::is_same<PassBuilder, PB_t>::value, "Expected PassBuilder as second argument!");
        if (!PB) return;
#if JL_LLVM_VERSION >= 200000
        PB->invokeOptimizerEarlyEPCallbacks(MPM, O, ThinOrFullLTOPhase::None);
#else
        PB->invokeOptimizerEarlyEPCallbacks(MPM, O);
#endif
    }
    template<typename PB_t>
    std::enable_if_t<decltype(hasInvokeCallbacks_helper<PB_t>(nullptr))::value, void> invokeLateLoopOptimizationCallbacks(LoopPassManager &LPM, PB_t *PB, OptimizationLevel O) JL_NOTSAFEPOINT {
        static_assert(std::is_same<PassBuilder, PB_t>::value, "Expected PassBuilder as second argument!");
        if (!PB) return;
        PB->invokeLateLoopOptimizationsEPCallbacks(LPM, O);
    }
    template<typename PB_t>
    std::enable_if_t<decltype(hasInvokeCallbacks_helper<PB_t>(nullptr))::value, void> invokeLoopOptimizerEndCallbacks(LoopPassManager &LPM, PB_t *PB, OptimizationLevel O) JL_NOTSAFEPOINT {
        static_assert(std::is_same<PassBuilder, PB_t>::value, "Expected PassBuilder as second argument!");
        if (!PB) return;
        PB->invokeLoopOptimizerEndEPCallbacks(LPM, O);
    }
    template<typename PB_t>
    std::enable_if_t<decltype(hasInvokeCallbacks_helper<PB_t>(nullptr))::value, void> invokeScalarOptimizerCallbacks(FunctionPassManager &FPM, PB_t *PB, OptimizationLevel O) JL_NOTSAFEPOINT {
        static_assert(std::is_same<PassBuilder, PB_t>::value, "Expected PassBuilder as second argument!");
        if (!PB) return;
        PB->invokeScalarOptimizerLateEPCallbacks(FPM, O);
    }
    template<typename PB_t>
    std::enable_if_t<decltype(hasInvokeCallbacks_helper<PB_t>(nullptr))::value, void> invokeVectorizerCallbacks(FunctionPassManager &FPM, PB_t *PB, OptimizationLevel O) JL_NOTSAFEPOINT {
        static_assert(std::is_same<PassBuilder, PB_t>::value, "Expected PassBuilder as second argument!");
        if (!PB) return;
        PB->invokeVectorizerStartEPCallbacks(FPM, O);
    }
    template<typename PB_t>
    std::enable_if_t<decltype(hasInvokeCallbacks_helper<PB_t>(nullptr))::value, void> invokeOptimizerLastCallbacks(ModulePassManager &MPM, PB_t *PB, OptimizationLevel O) JL_NOTSAFEPOINT {
        static_assert(std::is_same<PassBuilder, PB_t>::value, "Expected PassBuilder as second argument!");
        if (!PB) return;
#if JL_LLVM_VERSION >= 200000
        PB->invokeOptimizerLastEPCallbacks(MPM, O, ThinOrFullLTOPhase::None);
#else
        PB->invokeOptimizerLastEPCallbacks(MPM, O);
#endif
    }

    // Fallbacks
    void invokePipelineStartCallbacks(...) {}
    void invokePeepholeEPCallbacks(...) {}
    void invokeEarlySimplificationCallbacks(...) {}
    void invokeCGSCCCallbacks(...) {}
    void invokeOptimizerEarlyCallbacks(...) {}
    void invokeLateLoopOptimizationCallbacks(...) {}
    void invokeLoopOptimizerEndCallbacks(...) {}
    void invokeScalarOptimizerCallbacks(...) {}
    void invokeVectorizerCallbacks(...) {}
    void invokeOptimizerLastCallbacks(...) {}

#ifdef _COMPILER_CLANG_
#pragma clang diagnostic pop
#endif

#ifdef _COMPILER_GCC_
#pragma GCC diagnostic pop
#endif
}

//The actual pipelines
//TODO Things we might want to consider:
//* For vectorization
//? loop unroll/jam after loop vectorization
//? optimization remarks pass
//? cse/cvp/instcombine/bdce/sccp/licm/unswitch after loop vectorization (
// cleanup as much as possible before trying to slp vectorize)
//* For optimization
//? loop sink pass
//? hot-cold splitting pass

#define JULIA_PASS(ADD_PASS) if (!options.llvm_only) { ADD_PASS; } else do { } while (0)

static void buildEarlySimplificationPipeline(ModulePassManager &MPM, PassBuilder *PB, OptimizationLevel O, const OptimizationOptions &options) JL_NOTSAFEPOINT {
    MPM.addPass(BeforeEarlySimplificationMarkerPass());
#ifdef JL_VERIFY_PASSES
    addVerificationPasses(MPM, options.llvm_only);
#endif
    if (options.enable_early_simplifications) {
      // Place after verification in case we want to force it anyways
      MPM.addPass(ForceFunctionAttrsPass());
      invokePipelineStartCallbacks(MPM, PB, O);
      MPM.addPass(Annotation2MetadataPass());
      MPM.addPass(InferFunctionAttrsPass());
      MPM.addPass(ConstantMergePass());
      {
          FunctionPassManager FPM;
          FPM.addPass(LowerExpectIntrinsicPass());
          if (O.getSpeedupLevel() >= 2) {
              JULIA_PASS(FPM.addPass(PropagateJuliaAddrspacesPass()));
          }
          // DCE must come before simplifycfg
          // codegen can generate unused statements when generating builtin calls,
          // and those dead statements can alter how simplifycfg optimizes the CFG
          FPM.addPass(DCEPass());
          FPM.addPass(SimplifyCFGPass(basicSimplifyCFGOptions()));
          if (O.getSpeedupLevel() >= 1) {
              FPM.addPass(SROAPass(SROAOptions::ModifyCFG));
              FPM.addPass(EarlyCSEPass());
          }
          MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
          if (O.getSpeedupLevel() >= 1) {
            FunctionPassManager GlobalFPM;
            MPM.addPass(GlobalOptPass());
            GlobalFPM.addPass(PromotePass());
            GlobalFPM.addPass(InstCombinePass());
        }
      }
      invokeEarlySimplificationCallbacks(MPM, PB, O);
    }
    MPM.addPass(AfterEarlySimplificationMarkerPass());
}

static void buildEarlyOptimizerPipeline(ModulePassManager &MPM, PassBuilder *PB, OptimizationLevel O, const OptimizationOptions &options) JL_NOTSAFEPOINT {
    MPM.addPass(BeforeEarlyOptimizationMarkerPass());
    if (options.enable_early_optimizations) {
      invokeOptimizerEarlyCallbacks(MPM, PB, O);
      {
          CGSCCPassManager CGPM;
          invokeCGSCCCallbacks(CGPM, PB, O);
          if (O.getSpeedupLevel() >= 2) {
              FunctionPassManager FPM;
              JULIA_PASS(FPM.addPass(AllocOptPass()));
              FPM.addPass(Float2IntPass());
              FPM.addPass(LowerConstantIntrinsicsPass());
              CGPM.addPass(createCGSCCToFunctionPassAdaptor(std::move(FPM)));
          }
          MPM.addPass(createModuleToPostOrderCGSCCPassAdaptor(std::move(CGPM)));
      }
      if (O.getSpeedupLevel() >= 2) {
          MPM.addPass(RequireAnalysisPass<GlobalsAA, Module>());
      }
      // MPM.addPass(createModuleToFunctionPassAdaptor(InvalidateAnalysisPass<AAManager>()));
      if (options.dump_native) {
          MPM.addPass(StripDeadPrototypesPass());
          JULIA_PASS(MPM.addPass(MultiVersioningPass(options.external_use)));
      }
      JULIA_PASS(MPM.addPass(CPUFeaturesPass()));
      if (O.getSpeedupLevel() >= 1) {
          FunctionPassManager FPM;
          if (O.getSpeedupLevel() >= 2) {
            FPM.addPass(SROAPass(SROAOptions::ModifyCFG));
            FPM.addPass(EarlyCSEPass(true));
            FPM.addPass(InstCombinePass());
            FPM.addPass(AggressiveInstCombinePass());
            FPM.addPass(JumpThreadingPass());
            FPM.addPass(CorrelatedValuePropagationPass());
            FPM.addPass(LibCallsShrinkWrapPass());
            FPM.addPass(ReassociatePass());
            FPM.addPass(ConstraintEliminationPass());
            JULIA_PASS(FPM.addPass(AllocOptPass()));
        } else { // if (O.getSpeedupLevel() >= 1) (exactly)
            FPM.addPass(EarlyCSEPass());
            FPM.addPass(InstCombinePass());
        }
        invokePeepholeEPCallbacks(FPM, PB, O);
        MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM), /*UseMemorySSA = */true));
      }
      MPM.addPass(GlobalOptPass());
      MPM.addPass(GlobalDCEPass());
    }
    MPM.addPass(AfterEarlyOptimizationMarkerPass());
}

static void buildLoopOptimizerPipeline(FunctionPassManager &FPM, PassBuilder *PB, OptimizationLevel O, const OptimizationOptions &options) JL_NOTSAFEPOINT {
    FPM.addPass(BeforeLoopOptimizationMarkerPass());
    if (options.enable_loop_optimizations) {
        {
            LoopPassManager LPM;
            LPM.addPass(LowerSIMDLoopPass());
            if (O.getSpeedupLevel() >= 2) {
                LPM.addPass(LoopInstSimplifyPass());
                LPM.addPass(LoopSimplifyCFGPass());
                LPM.addPass(BeforeLICMMarkerPass());
                auto opts = LICMOptions();
                opts.AllowSpeculation = false;
                LPM.addPass(LICMPass(opts));
                LPM.addPass(JuliaLICMPass());
                LPM.addPass(LoopRotatePass(true, false));
                LPM.addPass(LICMPass(LICMOptions()));
                LPM.addPass(JuliaLICMPass());
                LPM.addPass(AfterLICMMarkerPass());
                LPM.addPass(SimpleLoopUnswitchPass(/*NonTrivial*/true, true));
            }
            invokeLateLoopOptimizationCallbacks(LPM, PB, O);
            //We don't know if the loop callbacks support MSSA
            FPM.addPass(createFunctionToLoopPassAdaptor(std::move(LPM), /*UseMemorySSA = */true));
        }
        if (O.getSpeedupLevel() >= 2)
            FPM.addPass(IRCEPass());
        {
            LoopPassManager LPM;
            LPM.addPass(BeforeLoopSimplificationMarkerPass());
            if (O.getSpeedupLevel() >= 2) {
                LPM.addPass(LoopIdiomRecognizePass());
                LPM.addPass(IndVarSimplifyPass());
                LPM.addPass(SimpleLoopUnswitchPass(/*NonTrivial*/true, true));
                LPM.addPass(LoopDeletionPass());
                // This unroll will only unroll loops when the trip count is known and small,
                // so that no loop remains
                LPM.addPass(LoopFullUnrollPass());
            }
            invokeLoopOptimizerEndCallbacks(LPM, PB, O);
            LPM.addPass(AfterLoopSimplificationMarkerPass());
            FPM.addPass(SimplifyCFGPass(basicSimplifyCFGOptions()));
            FPM.addPass(InstCombinePass());
            //We don't know if the loop end callbacks support MSSA
            FPM.addPass(createFunctionToLoopPassAdaptor(std::move(LPM), /*UseMemorySSA = */false));
        }
    }
    FPM.addPass(AfterLoopOptimizationMarkerPass());
}

static void buildScalarOptimizerPipeline(FunctionPassManager &FPM, PassBuilder *PB, OptimizationLevel O, const OptimizationOptions &options) JL_NOTSAFEPOINT {
    FPM.addPass(BeforeScalarOptimizationMarkerPass());
    if (options.enable_scalar_optimizations) {
        if (O.getSpeedupLevel() >= 2) {
            JULIA_PASS(FPM.addPass(AllocOptPass()));
            FPM.addPass(SROAPass(SROAOptions::ModifyCFG));
            FPM.addPass(VectorCombinePass(/*TryEarlyFoldsOnly=*/true));
            FPM.addPass(MergedLoadStoreMotionPass());
            FPM.addPass(GVNPass());
            FPM.addPass(SCCPPass());
            FPM.addPass(BDCEPass());
            FPM.addPass(InstCombinePass());
            FPM.addPass(CorrelatedValuePropagationPass());
            FPM.addPass(ADCEPass());
            FPM.addPass(MemCpyOptPass());
            FPM.addPass(DSEPass());
            FPM.addPass(IRCEPass());
            FPM.addPass(JumpThreadingPass());
            FPM.addPass(ConstraintEliminationPass());
        } else if (O.getSpeedupLevel() >= 1) {
            JULIA_PASS(FPM.addPass(AllocOptPass()));
            FPM.addPass(SROAPass(SROAOptions::ModifyCFG));
            FPM.addPass(MemCpyOptPass());
            FPM.addPass(SCCPPass());
            FPM.addPass(BDCEPass());
            FPM.addPass(InstCombinePass());
            FPM.addPass(ADCEPass());
        }
        if (O.getSpeedupLevel() >= 3) {
            FPM.addPass(GVNPass());
        }
        if (O.getSpeedupLevel() >= 2) {
            FPM.addPass(DSEPass());
            invokePeepholeEPCallbacks(FPM, PB, O);
            FPM.addPass(SimplifyCFGPass(aggressiveSimplifyCFGOptions()));
            JULIA_PASS(FPM.addPass(AllocOptPass()));
            {
                LoopPassManager LPM;
                LPM.addPass(LICMPass(LICMOptions()));
                LPM.addPass(JuliaLICMPass());
                FPM.addPass(createFunctionToLoopPassAdaptor(std::move(LPM), /*UseMemorySSA = */true));
            }
            FPM.addPass(SimplifyCFGPass(aggressiveSimplifyCFGOptions()));
            FPM.addPass(InstCombinePass());
        } else if (O.getSpeedupLevel() >= 1)
            FPM.addPass(SimplifyCFGPass(aggressiveSimplifyCFGOptions()));

        invokeScalarOptimizerCallbacks(FPM, PB, O);
    }
    FPM.addPass(AfterScalarOptimizationMarkerPass());
}

static void buildVectorPipeline(FunctionPassManager &FPM, PassBuilder *PB, OptimizationLevel O, const OptimizationOptions &options) JL_NOTSAFEPOINT {
    FPM.addPass(BeforeVectorizationMarkerPass());
    if (options.enable_vector_pipeline) {
        //TODO look into loop vectorize options
        // Rerotate loops that might have been unrotated in the simplification
        LoopPassManager LPM;
        LPM.addPass(LoopRotatePass());
        LPM.addPass(LoopDeletionPass());
        FPM.addPass(createFunctionToLoopPassAdaptor(std::move(LPM), /*UseMemorySSA=*/false, /*UseBlockFrequencyInfo=*/false));
        FPM.addPass(LoopDistributePass());
        FPM.addPass(InjectTLIMappings());
        FPM.addPass(LoopVectorizePass());
        FPM.addPass(LoopLoadEliminationPass());
        FPM.addPass(SimplifyCFGPass(aggressiveSimplifyCFGOptions()));
        FPM.addPass(createFunctionToLoopPassAdaptor(LICMPass(LICMOptions()), /*UseMemorySSA=*/true, /*UseBlockFrequencyInfo=*/false));
        FPM.addPass(EarlyCSEPass());
        FPM.addPass(CorrelatedValuePropagationPass());
        FPM.addPass(InstCombinePass());
        FPM.addPass(SLPVectorizerPass());
        FPM.addPass(VectorCombinePass());
        invokeVectorizerCallbacks(FPM, PB, O);
        FPM.addPass(LoopUnrollPass(LoopUnrollOptions(O.getSpeedupLevel(), /*OnlyWhenForced = */ false, /*ForgetSCEV = */false)));
        FPM.addPass(SROAPass(SROAOptions::PreserveCFG));
        FPM.addPass(InstSimplifyPass());
        FPM.addPass(AfterVectorizationMarkerPass());
    }
    FPM.addPass(AfterVectorizationMarkerPass());
}

static void buildIntrinsicLoweringPipeline(ModulePassManager &MPM, PassBuilder *PB, OptimizationLevel O, const OptimizationOptions &options) JL_NOTSAFEPOINT {
    MPM.addPass(BeforeIntrinsicLoweringMarkerPass());
    if (options.lower_intrinsics) {
        //TODO barrier pass?
        {
            FunctionPassManager FPM;
            JULIA_PASS(FPM.addPass(GCInvariantVerifierPass(false)));
            MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
        }
        // Needed **before** LateLowerGCFrame on LLVM < 12
        // due to bug in `CreateAlignmentAssumption`.
        assert(options.remove_ni);
        JULIA_PASS(MPM.addPass(RemoveNIPass()));
        {
            FunctionPassManager FPM;
            JULIA_PASS(FPM.addPass(LateLowerGCPass()));
            JULIA_PASS(FPM.addPass(FinalLowerGCPass()));
            MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
        }
        JULIA_PASS(MPM.addPass(LowerPTLSPass(options.dump_native)));
        MPM.addPass(RemoveJuliaAddrspacesPass()); //TODO: Make this conditional on arches (GlobalISel doesn't like our addrsspaces)
        if (O.getSpeedupLevel() >= 1) {
            FunctionPassManager FPM;
            if (O.getSpeedupLevel() >= 2) {
                FPM.addPass(DSEPass());
                FPM.addPass(GVNPass());
                FPM.addPass(SCCPPass());
                FPM.addPass(DCEPass());
            }
            FPM.addPass(InstCombinePass());
            FPM.addPass(SimplifyCFGPass(aggressiveSimplifyCFGOptions()));
            MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
        }
    } else if (!options.remove_ni) {
        JULIA_PASS(MPM.addPass(RemoveNIPass()));
    }
    MPM.addPass(AfterIntrinsicLoweringMarkerPass());
}

static void buildCleanupPipeline(ModulePassManager &MPM, PassBuilder *PB, OptimizationLevel O, const OptimizationOptions &options) JL_NOTSAFEPOINT {
    MPM.addPass(BeforeCleanupMarkerPass());
    if (options.cleanup) {
        if (O.getSpeedupLevel() >= 2) {
            FunctionPassManager FPM;
            FPM.addPass(DivRemPairsPass());
            MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
        }
        invokeOptimizerLastCallbacks(MPM, PB, O);
        MPM.addPass(createModuleToFunctionPassAdaptor(AnnotationRemarksPass()));
        addSanitizerPasses(MPM, O);
        {
            FunctionPassManager FPM;
            JULIA_PASS(FPM.addPass(DemoteFloat16Pass()));
            if (O.getSpeedupLevel() >= 2) {
                FPM.addPass(GVNPass());
            }
            MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
        }
    }
    MPM.addPass(AfterCleanupMarkerPass());
}

static void buildPipeline(ModulePassManager &MPM, PassBuilder *PB, OptimizationLevel O, const OptimizationOptions &options) JL_NOTSAFEPOINT {
    MPM.addPass(BeforeOptimizationMarkerPass());
    buildEarlySimplificationPipeline(MPM, PB, O, options);
    if (options.always_inline)
        MPM.addPass(AlwaysInlinerPass());
    buildEarlyOptimizerPipeline(MPM, PB, O, options);
    {
        FunctionPassManager FPM;
        buildLoopOptimizerPipeline(FPM, PB, O, options);
        buildScalarOptimizerPipeline(FPM, PB, O, options);
        if (O.getSpeedupLevel() >= 2) {
            buildVectorPipeline(FPM, PB, O, options);
        }
        if (options.warn_missed_transformations)
            FPM.addPass(WarnMissedTransformationsPass());
        MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
    }
    buildIntrinsicLoweringPipeline(MPM, PB, O, options);
    buildCleanupPipeline(MPM, PB, O, options);
    MPM.addPass(AfterOptimizationMarkerPass());
}


#undef JULIA_PASS

namespace {

    void adjustPIC(PassInstrumentationCallbacks &PIC) JL_NOTSAFEPOINT {
//Borrowed from LLVM PassBuilder.cpp:386
#define MODULE_PASS(NAME, CREATE_PASS)                                         \
PIC.addClassToPassName(decltype(CREATE_PASS)::name(), NAME);
#define MODULE_PASS_WITH_PARAMS(NAME, CREATE_PASS, PARSER, PARAMS)      \
PIC.addClassToPassName(CLASS, NAME);
#define MODULE_ANALYSIS(NAME, CREATE_PASS)                                     \
PIC.addClassToPassName(decltype(CREATE_PASS)::name(), NAME);
#define FUNCTION_PASS(NAME, CREATE_PASS)                                       \
PIC.addClassToPassName(decltype(CREATE_PASS)::name(), NAME);
#define FUNCTION_PASS_WITH_PARAMS(NAME, CREATE_PASS, PARSER, PARAMS)    \
PIC.addClassToPassName(CLASS, NAME);
#define FUNCTION_ANALYSIS(NAME, CREATE_PASS)                                   \
PIC.addClassToPassName(decltype(CREATE_PASS)::name(), NAME);
#define LOOPNEST_PASS(NAME, CREATE_PASS)                                       \
PIC.addClassToPassName(decltype(CREATE_PASS)::name(), NAME);
#define LOOP_PASS(NAME, CREATE_PASS)                                           \
PIC.addClassToPassName(decltype(CREATE_PASS)::name(), NAME);
#define LOOP_PASS_WITH_PARAMS(NAME, CREATE_PASS, PARSER, PARAMS)        \
PIC.addClassToPassName(CLASS, NAME);
#define LOOP_ANALYSIS(NAME, CREATE_PASS)                                       \
PIC.addClassToPassName(decltype(CREATE_PASS)::name(), NAME);
#define CGSCC_PASS(NAME, CREATE_PASS)                                          \
PIC.addClassToPassName(decltype(CREATE_PASS)::name(), NAME);
#define CGSCC_PASS_WITH_PARAMS(NAME, CREATE_PASS, PARSER, PARAMS)       \
PIC.addClassToPassName(CLASS, NAME);
#define CGSCC_ANALYSIS(NAME, CREATE_PASS)                                      \
PIC.addClassToPassName(decltype(CREATE_PASS)::name(), NAME);

#include "llvm-julia-passes.inc"

#undef MODULE_PASS
#undef MODULE_PASS_WITH_PARAMS
#undef MODULE_ANALYSIS
#undef FUNCTION_PASS
#undef FUNCTION_PASS_WITH_PARAMS
#undef FUNCTION_ANALYSIS
#undef LOOPNEST_PASS
#undef LOOP_PASS
#undef LOOP_PASS_WITH_PARAMS
#undef LOOP_ANALYSIS
#undef CGSCC_PASS
#undef CGSCC_PASS_WITH_PARAMS
#undef CGSCC_ANALYSIS
        // Marker passes are set separately so that we don't export them by accident
        PIC.addClassToPassName("BeforeOptimizationMarkerPass", "BeforeOptimization");
        PIC.addClassToPassName("BeforeEarlySimplificationMarkerPass", "BeforeEarlySimplification");
        PIC.addClassToPassName("AfterEarlySimplificationMarkerPass", "AfterEarlySimplification");
        PIC.addClassToPassName("BeforeEarlyOptimizationMarkerPass", "BeforeEarlyOptimization");
        PIC.addClassToPassName("AfterEarlyOptimizationMarkerPass", "AfterEarlyOptimization");
        PIC.addClassToPassName("BeforeLoopOptimizationMarkerPass", "BeforeLoopOptimization");
        PIC.addClassToPassName("BeforeLICMMarkerPass", "BeforeLICM");
        PIC.addClassToPassName("AfterLICMMarkerPass", "AfterLICM");
        PIC.addClassToPassName("BeforeLoopSimplificationMarkerPass", "BeforeLoopSimplification");
        PIC.addClassToPassName("AfterLoopSimplificationMarkerPass", "AfterLoopSimplification");
        PIC.addClassToPassName("AfterLoopOptimizationMarkerPass", "AfterLoopOptimization");
        PIC.addClassToPassName("BeforeScalarOptimizationMarkerPass", "BeforeScalarOptimization");
        PIC.addClassToPassName("AfterScalarOptimizationMarkerPass", "AfterScalarOptimization");
        PIC.addClassToPassName("BeforeVectorizationMarkerPass", "BeforeVectorization");
        PIC.addClassToPassName("AfterVectorizationMarkerPass", "AfterVectorization");
        PIC.addClassToPassName("BeforeIntrinsicLoweringMarkerPass", "BeforeIntrinsicLowering");
        PIC.addClassToPassName("AfterIntrinsicLoweringMarkerPass", "AfterIntrinsicLowering");
        PIC.addClassToPassName("BeforeCleanupMarkerPass", "BeforeCleanup");
        PIC.addClassToPassName("AfterCleanupMarkerPass", "AfterCleanup");
        PIC.addClassToPassName("AfterOptimizationMarkerPass", "AfterOptimization");
    }

    FunctionAnalysisManager createFAM(OptimizationLevel O, TargetMachine &TM) JL_NOTSAFEPOINT {

        FunctionAnalysisManager FAM;
        // Register the AA manager first so that our version is the one used.
        FAM.registerPass([&] JL_NOTSAFEPOINT {
            AAManager AA;
            if (O.getSpeedupLevel() >= 2) {
                AA.registerFunctionAnalysis<BasicAA>();
                AA.registerFunctionAnalysis<ScopedNoAliasAA>();
                AA.registerFunctionAnalysis<TypeBasedAA>();
            }
            TM.registerDefaultAliasAnalyses(AA);
            return AA;
        });
        // Register our TargetLibraryInfoImpl.
        FAM.registerPass([&] JL_NOTSAFEPOINT { return llvm::TargetIRAnalysis(TM.getTargetIRAnalysis()); });
        FAM.registerPass([&] JL_NOTSAFEPOINT { return llvm::TargetLibraryAnalysis(llvm::TargetLibraryInfoImpl(TM.getTargetTriple())); });
        return FAM;
    }

    ModulePassManager createMPM(PassBuilder &PB, OptimizationLevel O, OptimizationOptions options) JL_NOTSAFEPOINT {
        ModulePassManager MPM;
        buildPipeline(MPM, &PB, O, options);
        return MPM;
    }
}

NewPM::NewPM(std::unique_ptr<TargetMachine> TM, OptimizationLevel O, OptimizationOptions options) :
    TM(std::move(TM)), O(O), options(options), TimePasses() {}


NewPM::~NewPM() = default;

AnalysisManagers::AnalysisManagers(TargetMachine &TM, PassBuilder &PB, OptimizationLevel O) : LAM(), FAM(createFAM(O, TM)), CGAM(), MAM() {
    PB.registerLoopAnalyses(LAM);
    PB.registerFunctionAnalyses(FAM);
    PB.registerCGSCCAnalyses(CGAM);
    PB.registerModuleAnalyses(MAM);
    PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);
}

AnalysisManagers::AnalysisManagers(PassBuilder &PB) : LAM(), FAM(), CGAM(), MAM() {
    PB.registerLoopAnalyses(LAM);
    PB.registerFunctionAnalyses(FAM);
    PB.registerCGSCCAnalyses(CGAM);
    PB.registerModuleAnalyses(MAM);
    PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);
}

AnalysisManagers::~AnalysisManagers() = default;

void NewPM::run(Module &M) {
    //We must recreate the analysis managers every time
    //so that analyses from previous runs of the pass manager
    //do not hang around for the next run
    StandardInstrumentations SI(M.getContext(),false);
    PassInstrumentationCallbacks PIC;
    adjustPIC(PIC);
    TimePasses.registerCallbacks(PIC);
    FunctionAnalysisManager FAM(createFAM(O, *TM.get()));
    LoopAnalysisManager LAM;
    CGSCCAnalysisManager CGAM;
    ModuleAnalysisManager MAM;
    SI.registerCallbacks(PIC, &MAM);
    SI.getTimePasses().setOutStream(nulls()); //TODO: figure out a better way of doing this
    PassBuilder PB(TM.get(), PipelineTuningOptions(), None, &PIC);
    PB.registerLoopAnalyses(LAM);
    PB.registerFunctionAnalyses(FAM);
    PB.registerCGSCCAnalyses(CGAM);
    PB.registerModuleAnalyses(MAM);
    PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);
    ModulePassManager MPM = createMPM(PB, O, options);
#ifndef __clang_gcanalyzer__ /* the analyzer cannot prove we have not added instrumentation callbacks with safepoints */
    MPM.run(M, MAM);
#endif
}

void NewPM::printTimers() {
    TimePasses.print();
}

OptimizationLevel getOptLevel(int optlevel) {
    switch (std::min(std::max(optlevel, 0), 3)) {
        case 0:
            return OptimizationLevel::O0;
        case 1:
            return OptimizationLevel::O1;
        case 2:
            return OptimizationLevel::O2;
        case 3:
            return OptimizationLevel::O3;
    }
    llvm_unreachable("cannot get here!");
}

//This part is also basically stolen from LLVM's PassBuilder.cpp file
static std::optional<std::pair<OptimizationLevel, OptimizationOptions>> parseJuliaPipelineOptions(StringRef name) {
    if (name.consume_front("julia")) {
        auto O = OptimizationLevel::O2;
        auto options = OptimizationOptions::defaults();
        if (!name.empty() && (!name.consume_front("<") || !name.consume_back(">"))) {
            assert(false && "Expected pass options to be enclosed in <>!");
        }
        std::map<StringRef, bool*> option_pointers = {
#define OPTION(name) {#name, &options.name}
            OPTION(lower_intrinsics),
            OPTION(dump_native),
            OPTION(external_use),
            OPTION(llvm_only),
            OPTION(always_inline),
            OPTION(enable_early_simplifications),
            OPTION(enable_early_optimizations),
            OPTION(enable_scalar_optimizations),
            OPTION(enable_loop_optimizations),
            OPTION(enable_vector_pipeline),
            OPTION(remove_ni),
            OPTION(cleanup),
            OPTION(warn_missed_transformations)
#undef OPTION
        };
        while (!name.empty()) {
            StringRef option;
            std::tie(option, name) = name.split(';');
            bool enable = !option.consume_front("no_");
            auto it = option_pointers.find(option);
            if (it == option_pointers.end()) {
                if (option.consume_front("level=")) {
                    int level = 2;
                    if (option.getAsInteger(0, level)) {
                        assert(false && "Non-integer passed to julia level!");
                    }
                    switch (std::min(std::max(level, 0), 3)) {
                        case 0:
                            O = OptimizationLevel::O0;
                            break;
                        case 1:
                            O = OptimizationLevel::O1;
                            break;
                        case 2:
                            O = OptimizationLevel::O2;
                            break;
                        case 3:
                            O = OptimizationLevel::O3;
                            break;
                    }
                } else {
                    errs() << "Unable to find julia option '" << option << "'!";
                    assert(false && "Invalid option passed to julia pass!");
                }
            } else {
                *it->second = enable;
            }
        }
        return {{O, options}};
    }
    return None;
}

bool verifyLLVMIR(const Module &M) JL_NOTSAFEPOINT {
    JL_TIMING(VERIFY_IR, VERIFY_Module);
    if (verifyModule(M, &errs())) {
        errs() << "Failed to verify module '" << M.getModuleIdentifier() << "', dumping entire module!\n\n";
        errs() << M << "\n";
        return true;
    }
    return false;
}

bool verifyLLVMIR(const Function &F) JL_NOTSAFEPOINT {
    JL_TIMING(VERIFY_IR, VERIFY_Function);
    if (verifyFunction(F, &errs())) {
        errs() << "Failed to verify function '" << F.getName() << "', dumping entire module!\n\n";
        errs() << *F.getParent() << "\n";
        return true;
    }
    return false;
}

bool verifyLLVMIR(const Loop &L) JL_NOTSAFEPOINT {
    JL_TIMING(VERIFY_IR, VERIFY_Loop);
    if (verifyFunction(*L.getHeader()->getParent(), &errs())) {
        errs() << "Failed to verify loop '" << L << "', dumping entire module!\n\n";
        errs() << *L.getHeader()->getModule() << "\n";
        return true;
    }
    return false;
}

// new pass manager plugin

// NOTE: Instead of exporting all the constructors in passes.h we could
// forward the callbacks to the respective passes. LLVM seems to prefer this,
// and when we add the full pass builder having them directly will be helpful.
static void registerCallbacks(PassBuilder &PB) JL_NOTSAFEPOINT {
    auto PIC = PB.getPassInstrumentationCallbacks();
    if (PIC) {
        adjustPIC(*PIC);
    }
    PB.registerPipelineParsingCallback(
        [](StringRef Name, FunctionPassManager &PM,
           ArrayRef<PassBuilder::PipelineElement> InnerPipeline) {
#define FUNCTION_PASS(NAME, CREATE_PASS) if (Name == NAME) { PM.addPass(CREATE_PASS); return true; }
#include "llvm-julia-passes.inc"
#undef FUNCTION_PASS
            if (Name.consume_front("GCInvariantVerifier")) {
                if (Name.consume_front("<") && Name.consume_back(">")) {
                    bool strong = true;
                    if (Name.consume_front("no-")) {
                        strong = false;
                    }
                    if (Name == "strong") {
                        PM.addPass(GCInvariantVerifierPass(strong));
                        return true;
                    }
                }
                return false;
            }
            return false;
        });

    PB.registerPipelineParsingCallback(
        [](StringRef Name, ModulePassManager &PM,
           ArrayRef<PassBuilder::PipelineElement> InnerPipeline) {
#define MODULE_PASS(NAME, CREATE_PASS) if (Name == NAME) { PM.addPass(CREATE_PASS); return true; }
#include "llvm-julia-passes.inc"
#undef MODULE_PASS
            if (Name.consume_front("LowerPTLSPass")) {
                if (Name.consume_front("<") && Name.consume_back(">")) {
                    bool imaging_mode = true;
                    if (Name.consume_front("no-")) {
                        imaging_mode = false;
                    }
                    if (Name == "imaging") {
                        PM.addPass(LowerPTLSPass(imaging_mode));
                        return true;
                    }
                }
                return false;
            }
            if (Name.consume_front("JuliaMultiVersioning")) {
                if (Name.consume_front("<") && Name.consume_back(">")) {
                    bool external_use = true;
                    if (Name.consume_front("no-")) {
                        external_use = false;
                    }
                    if (Name == "external") {
                        PM.addPass(MultiVersioningPass(external_use));
                        return true;
                    }
                }
                return false;
            }
            //Add full pipelines here
            auto julia_options = parseJuliaPipelineOptions(Name);
            if (julia_options) {
                ModulePassManager pipeline;
                buildPipeline(pipeline, nullptr, julia_options->first, julia_options->second);
                PM.addPass(std::move(pipeline));
                return true;
            }
            return false;
        });

    PB.registerPipelineParsingCallback(
        [](StringRef Name, LoopPassManager &PM,
           ArrayRef<PassBuilder::PipelineElement> InnerPipeline) {
#define LOOP_PASS(NAME, CREATE_PASS) if (Name == NAME) { PM.addPass(CREATE_PASS); return true; }
#include "llvm-julia-passes.inc"
#undef LOOP_PASS
            return false;
        });
}

extern "C" JL_DLLEXPORT_CODEGEN
void jl_register_passbuilder_callbacks_impl(void *PB) JL_NOTSAFEPOINT {
    registerCallbacks(*static_cast<PassBuilder*>(PB));
}

extern "C" JL_DLLEXPORT_CODEGEN
::llvm::PassPluginLibraryInfo llvmGetPassPluginInfo() JL_NOTSAFEPOINT {
      return {LLVM_PLUGIN_API_VERSION, "Julia", "1", registerCallbacks};
}

void addTargetPasses(legacy::PassManagerBase *PM, const Triple &triple, TargetIRAnalysis analysis)
{
    PM->add(new TargetLibraryInfoWrapperPass(triple));
    PM->add(createTargetTransformInfoWrapperPass(std::move(analysis)));
}

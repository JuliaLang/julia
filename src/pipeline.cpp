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
#include <llvm/Analysis/TypeBasedAliasAnalysis.h>
#include <llvm/Analysis/ScopedNoAliasAA.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Vectorize.h>
#include <llvm/Transforms/Instrumentation/AddressSanitizer.h>
#include <llvm/Transforms/Instrumentation/ThreadSanitizer.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/IPO/AlwaysInliner.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar/InstSimplifyPass.h>
#include <llvm/Transforms/Utils/SimplifyCFGOptions.h>
#include <llvm/Transforms/Utils/ModuleUtils.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Passes/PassPlugin.h>

// NewPM needs to manually include all the pass headers
#include <llvm/Transforms/IPO/AlwaysInliner.h>
#include <llvm/Transforms/IPO/ConstantMerge.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Instrumentation/AddressSanitizer.h>
#include <llvm/Transforms/Instrumentation/MemorySanitizer.h>
#include <llvm/Transforms/Instrumentation/ThreadSanitizer.h>
#include <llvm/Transforms/Scalar/ADCE.h>
#include <llvm/Transforms/Scalar/CorrelatedValuePropagation.h>
#include <llvm/Transforms/Scalar/DCE.h>
#include <llvm/Transforms/Scalar/DeadStoreElimination.h>
#include <llvm/Transforms/Scalar/DivRemPairs.h>
#include <llvm/Transforms/Scalar/EarlyCSE.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Scalar/IndVarSimplify.h>
#include <llvm/Transforms/Scalar/InductiveRangeCheckElimination.h>
#include <llvm/Transforms/Scalar/InstSimplifyPass.h>
#include <llvm/Transforms/Scalar/JumpThreading.h>
#include <llvm/Transforms/Scalar/LICM.h>
#include <llvm/Transforms/Scalar/LoopDeletion.h>
#include <llvm/Transforms/Scalar/LoopIdiomRecognize.h>
#include <llvm/Transforms/Scalar/LoopInstSimplify.h>
#include <llvm/Transforms/Scalar/LoopLoadElimination.h>
#include <llvm/Transforms/Scalar/LoopRotation.h>
#include <llvm/Transforms/Scalar/LoopSimplifyCFG.h>
#include <llvm/Transforms/Scalar/LoopUnrollPass.h>
#include <llvm/Transforms/Scalar/MemCpyOptimizer.h>
#include <llvm/Transforms/Scalar/Reassociate.h>
#include <llvm/Transforms/Scalar/SCCP.h>
#include <llvm/Transforms/Scalar/SROA.h>
#include <llvm/Transforms/Scalar/SimpleLoopUnswitch.h>
#include <llvm/Transforms/Scalar/SimplifyCFG.h>
#include <llvm/Transforms/Vectorize/LoopVectorize.h>
#include <llvm/Transforms/Vectorize/SLPVectorizer.h>
#include <llvm/Transforms/Vectorize/VectorCombine.h>

#ifdef _COMPILER_GCC_
#pragma GCC diagnostic pop
#endif

#include "passes.h"

#include <llvm/Target/TargetMachine.h>

#include "julia.h"
#include "julia_internal.h"
#include "jitlayers.h"
#include "julia_assert.h"

using namespace llvm;

namespace {
    //Shamelessly stolen from Clang's approach to sanitizers
    //TODO do we want to enable other sanitizers?
    static void addSanitizerPasses(ModulePassManager &MPM, OptimizationLevel O) {
        // Coverage sanitizer
        // if (CodeGenOpts.hasSanitizeCoverage()) {
        //   auto SancovOpts = getSancovOptsFromCGOpts(CodeGenOpts);
        //   MPM.addPass(ModuleSanitizerCoveragePass(
        //       SancovOpts, CodeGenOpts.SanitizeCoverageAllowlistFiles,
        //       CodeGenOpts.SanitizeCoverageIgnorelistFiles));
        // }

    #ifdef _COMPILER_MSAN_ENABLED_
        auto MSanPass = [&](/*SanitizerMask Mask, */bool CompileKernel) {
        // if (LangOpts.Sanitize.has(Mask)) {
            // int TrackOrigins = CodeGenOpts.SanitizeMemoryTrackOrigins;
            // bool Recover = CodeGenOpts.SanitizeRecover.has(Mask);

            // MemorySanitizerOptions options(TrackOrigins, Recover, CompileKernel,
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
            // usefull. InstCombinePass breakes
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
        auto ASanPass = [&](/*SanitizerMask Mask, */bool CompileKernel) {
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
            MPM.addPass(RequireAnalysisPass<ASanGlobalsMetadataAnalysis, Module>());
            // MPM.addPass(ModuleAddressSanitizerPass(
            //     Opts, UseGlobalGC, UseOdrIndicator, DestructorKind));
            //Let's assume the defaults are actually fine for our purposes
            MPM.addPass(ModuleAddressSanitizerPass(AddressSanitizerOptions()));
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

    void addVerificationPasses(ModulePassManager &MPM) {
        FunctionPassManager FPM;
        FPM.addPass(GCInvariantVerifierPass());
        FPM.addPass(VerifierPass());
        MPM.addPass(llvm::createModuleToFunctionPassAdaptor(std::move(FPM)));
    }

    auto basicSimplifyCFGOptions() {
        return SimplifyCFGOptions()
            .convertSwitchRangeToICmp(true)
            .convertSwitchToLookupTable(true)
            .forwardSwitchCondToPhi(true);
    }

    auto aggressiveSimplifyCFGOptions() {
        return SimplifyCFGOptions()
            .convertSwitchRangeToICmp(true)
            .convertSwitchToLookupTable(true)
            .forwardSwitchCondToPhi(true)
            //These mess with loop rotation, so only do them after that
            .hoistCommonInsts(true)
            // Causes an SRET assertion error in late-gc-lowering
            // .sinkCommonInsts(true)
            ;
    }

    // TODO(vchuravy/maleadt):
    // Since we are not using the PassBuilder fully and instead rolling our own, we are missing out on
    // TargetMachine::registerPassBuilderCallbacks. We need to find a solution either in working with upstream
    // or adapting PassBuilder (or subclassing it) to suite our needs. This is in particular important for
    // BPF, NVPTX, and AMDGPU.
    //TODO implement these once LLVM exposes
    //the PassBuilder extension point callbacks
    //For now we'll maintain the insertion points even though they don't do anything
    //for the sake of documentation
    void invokePipelineStartCallbacks(ModulePassManager &MPM, PassBuilder &PB, OptimizationLevel O) {}
    void invokePeepholeEPCallbacks(FunctionPassManager &MPM, PassBuilder &PB, OptimizationLevel O) {}
    void invokeEarlySimplificationCallbacks(ModulePassManager &MPM, PassBuilder &PB, OptimizationLevel O) {}
    void invokeCGSCCCallbacks(CGSCCPassManager &MPM, PassBuilder &PB, OptimizationLevel O) {}
    void invokeOptimizerEarlyCallbacks(ModulePassManager &MPM, PassBuilder &PB, OptimizationLevel O) {}
    void invokeLateLoopOptimizationCallbacks(LoopPassManager &MPM, PassBuilder &PB, OptimizationLevel O) {}
    void invokeLoopOptimizerEndCallbacks(LoopPassManager &MPM, PassBuilder &PB, OptimizationLevel O) {}
    void invokeScalarOptimizerCallbacks(FunctionPassManager &MPM, PassBuilder &PB, OptimizationLevel O) {}
    void invokeVectorizerCallbacks(FunctionPassManager &MPM, PassBuilder &PB, OptimizationLevel O) {}
    void invokeOptimizerLastCallbacks(ModulePassManager &MPM, PassBuilder &PB, OptimizationLevel O) {}
}

//The actual pipelines
//TODO Things we might want to consider:
//? annotation2metadata pass
//? force function attributes pass
//? annotation remarks pass
//? infer function attributes pass
//? lower expect intrinsic pass
//? warn missed transformations pass
//* For vectorization
//? loop unroll/jam after loop vectorization
//? optimization remarks pass
//? cse/cvp/instcombine/bdce/sccp/licm/unswitch after loop vectorization (
// cleanup as much as possible before trying to slp vectorize)
//? vectorcombine pass
//* For optimization
//? float2int pass
//? lower constant intrinsics pass
//? loop sink pass
//? hot-cold splitting pass

//Use for O1 and below
void buildBasicPipeline(ModulePassManager &MPM, PassBuilder &PB, OptimizationLevel O, OptimizationOptions options) {
// #ifdef JL_DEBUG_BUILD
    addVerificationPasses(MPM);
// #endif
    invokePipelineStartCallbacks(MPM, PB, O);
    MPM.addPass(ConstantMergePass());
    if (!options.dump_native) {
        MPM.addPass(CPUFeatures());
        if (O.getSpeedupLevel() > 0) {
            MPM.addPass(createModuleToFunctionPassAdaptor(InstSimplifyPass()));
        }
    }
    {
        FunctionPassManager FPM;
        FPM.addPass(SimplifyCFGPass(basicSimplifyCFGOptions()));
        if (O.getSpeedupLevel() > 0) {
            FPM.addPass(SROAPass());
            FPM.addPass(InstCombinePass());
            FPM.addPass(EarlyCSEPass());
        }
        FPM.addPass(MemCpyOptPass());
        MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
    }
    invokeEarlySimplificationCallbacks(MPM, PB, O);
    MPM.addPass(AlwaysInlinerPass());
    {
        CGSCCPassManager CGPM;
        invokeCGSCCCallbacks(CGPM, PB, O);
        MPM.addPass(createModuleToPostOrderCGSCCPassAdaptor(std::move(CGPM)));
    }
    invokeOptimizerEarlyCallbacks(MPM, PB, O);
    MPM.addPass(LowerSIMDLoop());
    {
        FunctionPassManager FPM;
        {
            LoopPassManager LPM;
            invokeLateLoopOptimizationCallbacks(LPM, PB, O);
            invokeLoopOptimizerEndCallbacks(LPM, PB, O);
            FPM.addPass(createFunctionToLoopPassAdaptor(std::move(LPM)));
        }
        invokeScalarOptimizerCallbacks(FPM, PB, O);
        invokeVectorizerCallbacks(FPM, PB, O);
        MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
    }
    if (options.lower_intrinsics) {
        //TODO no barrier pass?
        {
            FunctionPassManager FPM;
            FPM.addPass(LowerExcHandlers());
            FPM.addPass(GCInvariantVerifierPass(false));
            MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
        }
        MPM.addPass(RemoveNI());
        MPM.addPass(createModuleToFunctionPassAdaptor(LateLowerGC()));
        MPM.addPass(FinalLowerGCPass());
        MPM.addPass(LowerPTLSPass(options.dump_native));
    } else {
        MPM.addPass(RemoveNI());
    }
    MPM.addPass(LowerSIMDLoop()); // TODO why do we do this twice
    if (options.dump_native) {
        MPM.addPass(MultiVersioning(options.external_use));
        MPM.addPass(CPUFeatures());
        if (O.getSpeedupLevel() > 0) {
            FunctionPassManager FPM;
            FPM.addPass(InstSimplifyPass());
            FPM.addPass(SimplifyCFGPass(basicSimplifyCFGOptions()));
            MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
        }
    }
    invokeOptimizerLastCallbacks(MPM, PB, O);
    addSanitizerPasses(MPM, O);
    MPM.addPass(createModuleToFunctionPassAdaptor(DemoteFloat16()));
}

//Use for O2 and above
void buildFullPipeline(ModulePassManager &MPM, PassBuilder &PB, OptimizationLevel O, OptimizationOptions options) {
// #ifdef JL_DEBUG_BUILD
    addVerificationPasses(MPM);
// #endif
    invokePipelineStartCallbacks(MPM, PB, O);
    MPM.addPass(ConstantMergePass());
    {
        FunctionPassManager FPM;
        FPM.addPass(PropagateJuliaAddrspacesPass());
        //TODO consider not using even basic simplification
        //options here, and adding a run of CVP to take advantage
        //of the unsimplified codegen information (e.g. known
        //zeros or ones)
        FPM.addPass(SimplifyCFGPass(basicSimplifyCFGOptions()));
        FPM.addPass(DCEPass());
        FPM.addPass(SROAPass());
        MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
    }
    invokeEarlySimplificationCallbacks(MPM, PB, O);
    MPM.addPass(AlwaysInlinerPass());
    invokeOptimizerEarlyCallbacks(MPM, PB, O);
    {
        CGSCCPassManager CGPM;
        invokeCGSCCCallbacks(CGPM, PB, O);
        {
            FunctionPassManager FPM;
            FPM.addPass(AllocOptPass());
            FPM.addPass(InstCombinePass());
            FPM.addPass(SimplifyCFGPass(basicSimplifyCFGOptions()));
            CGPM.addPass(createCGSCCToFunctionPassAdaptor(std::move(FPM)));
        }
        MPM.addPass(createModuleToPostOrderCGSCCPassAdaptor(std::move(CGPM)));
    }
    if (options.dump_native) {
        MPM.addPass(MultiVersioning(options.external_use));
    }
    MPM.addPass(CPUFeatures());
    {
        FunctionPassManager FPM;
        FPM.addPass(SROAPass());
        FPM.addPass(InstSimplifyPass());
        FPM.addPass(JumpThreadingPass());
        FPM.addPass(CorrelatedValuePropagationPass());
        FPM.addPass(ReassociatePass());
        FPM.addPass(EarlyCSEPass());
        FPM.addPass(AllocOptPass());
        invokePeepholeEPCallbacks(FPM, PB, O);
        MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
    }
    MPM.addPass(LowerSIMDLoop());
    {
        FunctionPassManager FPM;
        {
            LoopPassManager LPM1, LPM2;
            LPM1.addPass(LoopRotatePass());
            invokeLateLoopOptimizationCallbacks(LPM1, PB, O);
            //We don't know if the loop callbacks support MSSA
            FPM.addPass(createFunctionToLoopPassAdaptor(std::move(LPM1), /*UseMemorySSA = */false));
            LPM2.addPass(LICMPass());
            LPM2.addPass(JuliaLICMPass());
            LPM2.addPass(SimpleLoopUnswitchPass());
            LPM2.addPass(LICMPass());
            LPM2.addPass(JuliaLICMPass());
            //LICM needs MemorySSA now, so we must use it
            FPM.addPass(createFunctionToLoopPassAdaptor(std::move(LPM2), /*UseMemorySSA = */true));
        }
        FPM.addPass(IRCEPass());
        {
            LoopPassManager LPM;
            LPM.addPass(LoopInstSimplifyPass());
            LPM.addPass(LoopIdiomRecognizePass());
            LPM.addPass(IndVarSimplifyPass());
            LPM.addPass(LoopDeletionPass());
            invokeLoopOptimizerEndCallbacks(LPM, PB, O);
            //We don't know if the loop end callbacks support MSSA
            FPM.addPass(createFunctionToLoopPassAdaptor(std::move(LPM), /*UseMemorySSA = */false));
        }
        FPM.addPass(LoopUnrollPass());
        FPM.addPass(AllocOptPass());
        FPM.addPass(SROAPass());
        FPM.addPass(InstSimplifyPass());
        FPM.addPass(GVNPass());
        FPM.addPass(MemCpyOptPass());
        FPM.addPass(SCCPPass());
        FPM.addPass(CorrelatedValuePropagationPass());
        FPM.addPass(DCEPass());
        FPM.addPass(IRCEPass());
        FPM.addPass(InstCombinePass());
        FPM.addPass(JumpThreadingPass());
        if (O.getSpeedupLevel() >= 3) {
            FPM.addPass(GVNPass());
        }
        FPM.addPass(DSEPass());
        invokePeepholeEPCallbacks(FPM, PB, O);
        FPM.addPass(SimplifyCFGPass(aggressiveSimplifyCFGOptions()));
        FPM.addPass(AllocOptPass());
        {
            LoopPassManager LPM;
            LPM.addPass(LoopDeletionPass());
            LPM.addPass(LoopInstSimplifyPass());
            FPM.addPass(createFunctionToLoopPassAdaptor(std::move(LPM)));
        }
        invokeScalarOptimizerCallbacks(FPM, PB, O);
        //TODO look into loop vectorize options
        FPM.addPass(LoopVectorizePass());
        FPM.addPass(LoopLoadEliminationPass());
        FPM.addPass(InstCombinePass());
        FPM.addPass(SimplifyCFGPass(aggressiveSimplifyCFGOptions()));
        FPM.addPass(SLPVectorizerPass());
        invokeVectorizerCallbacks(FPM, PB, O);
        FPM.addPass(ADCEPass());
        //TODO add BDCEPass here?
        MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
    }
    if (options.lower_intrinsics) {
        //TODO barrier pass?
        {
            FunctionPassManager FPM;
            FPM.addPass(LowerExcHandlers());
            FPM.addPass(GCInvariantVerifierPass(false));
            MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
        }
        // Needed **before** LateLowerGCFrame on LLVM < 12
        // due to bug in `CreateAlignmentAssumption`.
        MPM.addPass(RemoveNI());
        MPM.addPass(createModuleToFunctionPassAdaptor(LateLowerGC()));
        MPM.addPass(FinalLowerGCPass());
        {
            FunctionPassManager FPM;
            FPM.addPass(GVNPass());
            FPM.addPass(SCCPPass());
            FPM.addPass(DCEPass());
            MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
        }
        MPM.addPass(LowerPTLSPass(options.dump_native));
        {
            FunctionPassManager FPM;
            FPM.addPass(InstCombinePass());
            FPM.addPass(SimplifyCFGPass(aggressiveSimplifyCFGOptions()));
            MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
        }
    } else {
        MPM.addPass(RemoveNI());
    }
    {
        FunctionPassManager FPM;
        FPM.addPass(CombineMulAdd());
        FPM.addPass(DivRemPairsPass());
        MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
    }
    invokeOptimizerLastCallbacks(MPM, PB, O);
    addSanitizerPasses(MPM, O);
    {
        FunctionPassManager FPM;
        FPM.addPass(DemoteFloat16());
        FPM.addPass(GVNPass());
        MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
    }
}

namespace {
    auto createPIC(StandardInstrumentations &SI) {
        auto PIC = std::make_unique<PassInstrumentationCallbacks>();
//Borrowed from LLVM PassBuilder.cpp:386
#define MODULE_PASS(NAME, CREATE_PASS)                                         \
PIC->addClassToPassName(decltype(CREATE_PASS)::name(), NAME);
#define MODULE_PASS_WITH_PARAMS(NAME, CLASS, CREATE_PASS, PARSER, PARAMS)      \
PIC->addClassToPassName(CLASS, NAME);
#define MODULE_ANALYSIS(NAME, CREATE_PASS)                                     \
PIC->addClassToPassName(decltype(CREATE_PASS)::name(), NAME);
#define FUNCTION_PASS(NAME, CREATE_PASS)                                       \
PIC->addClassToPassName(decltype(CREATE_PASS)::name(), NAME);
#define FUNCTION_PASS_WITH_PARAMS(NAME, CLASS, CREATE_PASS, PARSER, PARAMS)    \
PIC->addClassToPassName(CLASS, NAME);
#define FUNCTION_ANALYSIS(NAME, CREATE_PASS)                                   \
PIC->addClassToPassName(decltype(CREATE_PASS)::name(), NAME);
#define LOOPNEST_PASS(NAME, CREATE_PASS)                                       \
PIC->addClassToPassName(decltype(CREATE_PASS)::name(), NAME);
#define LOOP_PASS(NAME, CREATE_PASS)                                           \
PIC->addClassToPassName(decltype(CREATE_PASS)::name(), NAME);
#define LOOP_PASS_WITH_PARAMS(NAME, CLASS, CREATE_PASS, PARSER, PARAMS)        \
PIC->addClassToPassName(CLASS, NAME);
#define LOOP_ANALYSIS(NAME, CREATE_PASS)                                       \
PIC->addClassToPassName(decltype(CREATE_PASS)::name(), NAME);
#define CGSCC_PASS(NAME, CREATE_PASS)                                          \
PIC->addClassToPassName(decltype(CREATE_PASS)::name(), NAME);
#define CGSCC_PASS_WITH_PARAMS(NAME, CLASS, CREATE_PASS, PARSER, PARAMS)       \
PIC->addClassToPassName(CLASS, NAME);
#define CGSCC_ANALYSIS(NAME, CREATE_PASS)                                      \
PIC->addClassToPassName(decltype(CREATE_PASS)::name(), NAME);

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

        SI.registerCallbacks(*PIC);
        return PIC;
    }

    FunctionAnalysisManager createFAM(OptimizationLevel O, TargetIRAnalysis analysis, const Triple &triple) {

        FunctionAnalysisManager FAM;
        // Register the AA manager first so that our version is the one used.
        FAM.registerPass([&] {
            AAManager AA;
            // TODO: Why are we only doing this for -O3?
            if (O.getSpeedupLevel() >= 3) {
                AA.registerFunctionAnalysis<BasicAA>();
            }
            if (O.getSpeedupLevel() >= 2) {
                AA.registerFunctionAnalysis<ScopedNoAliasAA>();
                AA.registerFunctionAnalysis<TypeBasedAA>();
            }
            // TM->registerDefaultAliasAnalyses(AA);
            return AA;
        });
        // Register our TargetLibraryInfoImpl.
        FAM.registerPass([&] { return llvm::TargetIRAnalysis(analysis); });
        FAM.registerPass([&] { return llvm::TargetLibraryAnalysis(llvm::TargetLibraryInfoImpl(triple)); });
        return FAM;
    }

    ModulePassManager createMPM(PassBuilder &PB, OptimizationLevel O, OptimizationOptions options) {
        ModulePassManager MPM;
        if (O.getSpeedupLevel() < 2)
            buildBasicPipeline(MPM, PB, O, options);
        else
            buildFullPipeline(MPM, PB, O, options);
        return MPM;
    }
}

NewPM::NewPM(std::unique_ptr<TargetMachine> TM, OptimizationLevel O, OptimizationOptions options) :
    TM(std::move(TM)), SI(false), PIC(createPIC(SI)),
    PB(this->TM.get(), PipelineTuningOptions(), None, PIC.get()),
    MPM(createMPM(PB, O, options)), O(O) {}

AnalysisManagers::AnalysisManagers(TargetMachine &TM, PassBuilder &PB, OptimizationLevel O) : LAM(), FAM(createFAM(O, TM.getTargetIRAnalysis(), TM.getTargetTriple())), CGAM(), MAM() {
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

void NewPM::run(Module &M) {
    //We must recreate the analysis managers every time
    //so that analyses from previous runs of the pass manager
    //do not hang around for the next run
    AnalysisManagers AM{*TM, PB, O};
    MPM.run(M, AM.MAM);
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

#include "dtypes.h"
#include "passes.h"

typedef struct LLVMOpaqueModulePassManager *LLVMModulePassManagerRef;
typedef struct LLVMOpaqueFunctionPassManager *LLVMFunctionPassManagerRef;
typedef struct LLVMOpaqueLoopPassManager *LLVMLoopPassManagerRef;

DEFINE_SIMPLE_CONVERSION_FUNCTIONS(llvm::ModulePassManager, LLVMModulePassManagerRef);
DEFINE_SIMPLE_CONVERSION_FUNCTIONS(llvm::FunctionPassManager, LLVMFunctionPassManagerRef);
DEFINE_SIMPLE_CONVERSION_FUNCTIONS(llvm::LoopPassManager, LLVMLoopPassManagerRef);

#define MODULE_PASS(NAME, CLASS, CREATE_PASS) \
    extern "C" JL_DLLEXPORT_CODEGEN void LLVMExtraNewPMMPMAdd##CLASS##_impl(LLVMModulePassManagerRef PM) \
    { \
        unwrap(PM)->addPass(CREATE_PASS); \
    }
#define FUNCTION_PASS(NAME, CLASS, CREATE_PASS) \
    extern "C" JL_DLLEXPORT_CODEGEN void LLVMExtraNewPMFPMAdd##CLASS##_impl(LLVMFunctionPassManagerRef PM) \
    { \
        unwrap(PM)->addPass(CREATE_PASS); \
    }
#define LOOP_PASS(NAME, CLASS, CREATE_PASS) \
    extern "C" JL_DLLEXPORT_CODEGEN void LLVMExtraNewPMLPMAdd##CLASS##_impl(LLVMLoopPassManagerRef PM) \
    { \
        unwrap(PM)->addPass(CREATE_PASS); \
    }

#include "llvm-julia-passes.inc"

#undef MODULE_PASS
#undef CGSCC_PASS
#undef FUNCTION_PASS
#undef LOOP_PASS
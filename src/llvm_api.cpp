// This file is a part of Julia. License is MIT: https://julialang.org/license

#undef DEBUG
#include "llvm-version.h"
#include "platform.h"

#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS
#endif

#include "jitlayers.h"
#include "passes.h"

#include <llvm-c/Core.h>
#include <llvm-c/Error.h>
#include <llvm-c/Orc.h>
#include <llvm-c/OrcEE.h>
#include <llvm-c/TargetMachine.h>
#include <llvm-c/Types.h>
#include <llvm/Support/CBindingWrapping.h>
#include <llvm/Support/MemoryBuffer.h>

#if JL_LLVM_VERSION < 180000
namespace llvm {
namespace orc {
class OrcV2CAPIHelper {
public:
    using PoolEntry = orc::SymbolStringPtr::PoolEntry;
    using PoolEntryPtr = orc::SymbolStringPtr::PoolEntryPtr;

    // Move from SymbolStringPtr to PoolEntryPtr (no change in ref count).
    static PoolEntryPtr moveFromSymbolStringPtr(SymbolStringPtr S)
    {
        PoolEntryPtr Result = nullptr;
        std::swap(Result, S.S);
        return Result;
    }
};
} // namespace orc
} // namespace llvm
#endif

typedef struct JLOpaqueJuliaOJIT *JuliaOJITRef;
typedef struct LLVMOrcOpaqueIRCompileLayer *LLVMOrcIRCompileLayerRef;

DEFINE_SIMPLE_CONVERSION_FUNCTIONS(JuliaOJIT, JuliaOJITRef)
DEFINE_SIMPLE_CONVERSION_FUNCTIONS(orc::JITDylib, LLVMOrcJITDylibRef)
DEFINE_SIMPLE_CONVERSION_FUNCTIONS(orc::ExecutionSession, LLVMOrcExecutionSessionRef)
#if JL_LLVM_VERSION >= 180000
DEFINE_SIMPLE_CONVERSION_FUNCTIONS(orc::SymbolStringPoolEntryUnsafe::PoolEntry,
                                   LLVMOrcSymbolStringPoolEntryRef)
#else
DEFINE_SIMPLE_CONVERSION_FUNCTIONS(orc::OrcV2CAPIHelper::PoolEntry,
                                   LLVMOrcSymbolStringPoolEntryRef)
#endif
DEFINE_SIMPLE_CONVERSION_FUNCTIONS(orc::IRCompileLayer, LLVMOrcIRCompileLayerRef)
DEFINE_SIMPLE_CONVERSION_FUNCTIONS(orc::MaterializationResponsibility,
                                   LLVMOrcMaterializationResponsibilityRef)

typedef struct LLVMOpaqueModulePassManager *LLVMModulePassManagerRef;
typedef struct LLVMOpaqueFunctionPassManager *LLVMFunctionPassManagerRef;
typedef struct LLVMOpaqueLoopPassManager *LLVMLoopPassManagerRef;

DEFINE_SIMPLE_CONVERSION_FUNCTIONS(llvm::ModulePassManager, LLVMModulePassManagerRef)
DEFINE_SIMPLE_CONVERSION_FUNCTIONS(llvm::FunctionPassManager, LLVMFunctionPassManagerRef)
DEFINE_SIMPLE_CONVERSION_FUNCTIONS(llvm::LoopPassManager, LLVMLoopPassManagerRef)

extern "C" {

JL_DLLEXPORT_CODEGEN JuliaOJITRef JLJITGetJuliaOJIT_impl(void)
{
    return wrap(jl_ExecutionEngine);
}

JL_DLLEXPORT_CODEGEN LLVMOrcExecutionSessionRef
JLJITGetLLVMOrcExecutionSession_impl(JuliaOJITRef JIT)
{
    return wrap(&unwrap(JIT)->getExecutionSession());
}

JL_DLLEXPORT_CODEGEN LLVMOrcJITDylibRef
JLJITGetExternalJITDylib_impl(JuliaOJITRef JIT)
{
    return wrap(&unwrap(JIT)->getExternalJITDylib());
}

JL_DLLEXPORT_CODEGEN LLVMErrorRef JLJITAddObjectFile_impl(
    JuliaOJITRef JIT, LLVMOrcJITDylibRef JD, LLVMMemoryBufferRef ObjBuffer)
{
    return wrap(unwrap(JIT)->addObjectFile(
        *unwrap(JD), std::unique_ptr<MemoryBuffer>(unwrap(ObjBuffer))));
}

JL_DLLEXPORT_CODEGEN LLVMErrorRef JLJITAddLLVMIRModule_impl(
    JuliaOJITRef JIT, LLVMOrcJITDylibRef JD, LLVMOrcThreadSafeModuleRef TSM)
{
    std::unique_ptr<orc::ThreadSafeModule> TmpTSM(unwrap(TSM));
    return wrap(unwrap(JIT)->addExternalModule(*unwrap(JD), std::move(*TmpTSM)));
}

JL_DLLEXPORT_CODEGEN LLVMErrorRef
JLJITLookup_impl(JuliaOJITRef JIT, LLVMOrcExecutorAddress *Result,
                                   const char *Name, int ExternalJDOnly)
{
    auto Sym = unwrap(JIT)->findExternalJDSymbol(Name, ExternalJDOnly);
    if (Sym) {
        auto addr = Sym->getAddress();
        *Result = orc::ExecutorAddr(addr).getValue();
        return LLVMErrorSuccess;
    }
    else {
        *Result = 0;
        return wrap(Sym.takeError());
    }
}

JL_DLLEXPORT_CODEGEN LLVMOrcSymbolStringPoolEntryRef
JLJITMangleAndIntern_impl(JuliaOJITRef JIT,
                                            const char *Name)
{
#if JL_LLVM_VERSION >= 180000
    return wrap(orc::SymbolStringPoolEntryUnsafe::take(unwrap(JIT)->mangle(Name)).rawPtr());
#else
    return wrap(orc::OrcV2CAPIHelper::moveFromSymbolStringPtr(unwrap(JIT)->mangle(Name)));
#endif
}

JL_DLLEXPORT_CODEGEN const char *
JLJITGetTripleString_impl(JuliaOJITRef JIT)
{
    return unwrap(JIT)->getTargetTriple().str().c_str();
}

JL_DLLEXPORT_CODEGEN const char
JLJITGetGlobalPrefix_impl(JuliaOJITRef JIT)
{
    return unwrap(JIT)->getDataLayout().getGlobalPrefix();
}

JL_DLLEXPORT_CODEGEN const char *
JLJITGetDataLayoutString_impl(JuliaOJITRef JIT)
{
    return unwrap(JIT)->getDataLayout().getStringRepresentation().c_str();
}

JL_DLLEXPORT_CODEGEN LLVMOrcIRCompileLayerRef
JLJITGetIRCompileLayer_impl(JuliaOJITRef JIT)
{
    return wrap(&unwrap(JIT)->getIRCompileLayer());
}

#define MODULE_PASS(NAME, CLASS, CREATE_PASS) \
    JL_DLLEXPORT_CODEGEN void LLVMExtraMPMAdd##CLASS##_impl(LLVMModulePassManagerRef PM) \
    { \
        unwrap(PM)->addPass(CREATE_PASS); \
    }
#define FUNCTION_PASS(NAME, CLASS, CREATE_PASS) \
    JL_DLLEXPORT_CODEGEN void LLVMExtraFPMAdd##CLASS##_impl(LLVMFunctionPassManagerRef PM) \
    { \
        unwrap(PM)->addPass(CREATE_PASS); \
    }
#define LOOP_PASS(NAME, CLASS, CREATE_PASS) \
    JL_DLLEXPORT_CODEGEN void LLVMExtraLPMAdd##CLASS##_impl(LLVMLoopPassManagerRef PM) \
    { \
        unwrap(PM)->addPass(CREATE_PASS); \
    }

#include "llvm-julia-passes.inc"

#undef MODULE_PASS
#undef CGSCC_PASS
#undef FUNCTION_PASS
#undef LOOP_PASS

} // extern "C"

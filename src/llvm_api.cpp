// This file is a part of Julia. License is MIT: https://julialang.org/license

#undef DEBUG
#include "llvm-version.h"
#include "platform.h"

#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS
#endif

#include <jitlayers.h>

#include <llvm-c/Core.h>
#include <llvm-c/Error.h>
#include <llvm-c/Orc.h>
#include <llvm-c/OrcEE.h>
#include <llvm-c/TargetMachine.h>
#include <llvm-c/Types.h>
#include <llvm/Support/CBindingWrapping.h>
#include <llvm/Support/MemoryBuffer.h>

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


typedef struct JLOpaqueJuliaOJIT *JuliaOJITRef;
typedef struct LLVMOrcOpaqueIRCompileLayer *LLVMOrcIRCompileLayerRef;

DEFINE_SIMPLE_CONVERSION_FUNCTIONS(JuliaOJIT, JuliaOJITRef)
DEFINE_SIMPLE_CONVERSION_FUNCTIONS(orc::JITDylib, LLVMOrcJITDylibRef)
DEFINE_SIMPLE_CONVERSION_FUNCTIONS(orc::ExecutionSession, LLVMOrcExecutionSessionRef)
DEFINE_SIMPLE_CONVERSION_FUNCTIONS(orc::OrcV2CAPIHelper::PoolEntry,
                                   LLVMOrcSymbolStringPoolEntryRef)
DEFINE_SIMPLE_CONVERSION_FUNCTIONS(orc::IRCompileLayer, LLVMOrcIRCompileLayerRef)
DEFINE_SIMPLE_CONVERSION_FUNCTIONS(orc::MaterializationResponsibility,
                                   LLVMOrcMaterializationResponsibilityRef)
extern "C" {

JL_DLLEXPORT_CODEGEN JuliaOJITRef LLVMExtraJLJITGetJuliaOJIT_impl(void)
    JL_NOTSAFEPOINT
{
    return wrap(jl_ExecutionEngine);
}

JL_DLLEXPORT_CODEGEN LLVMOrcExecutionSessionRef
LLVMExtraJLJITGetLLVMOrcExecutionSession_impl(JuliaOJITRef JIT) JL_NOTSAFEPOINT
{
    return wrap(&unwrap(JIT)->getExecutionSession());
}

JL_DLLEXPORT_CODEGEN LLVMOrcJITDylibRef
LLVMExtraJLJITGetExternalJITDylib_impl(JuliaOJITRef JIT) JL_NOTSAFEPOINT
{
    return wrap(&unwrap(JIT)->getExternalJITDylib());
}

JL_DLLEXPORT_CODEGEN LLVMErrorRef LLVMExtraJLJITAddObjectFile_impl(
    JuliaOJITRef JIT, LLVMOrcJITDylibRef JD, LLVMMemoryBufferRef ObjBuffer) JL_NOTSAFEPOINT
{
    return wrap(unwrap(JIT)->addObjectFile(
        *unwrap(JD), std::unique_ptr<MemoryBuffer>(unwrap(ObjBuffer))));
}

JL_DLLEXPORT_CODEGEN LLVMErrorRef LLVMExtraJLJITAddLLVMIRModule_impl(
    JuliaOJITRef JIT, LLVMOrcJITDylibRef JD, LLVMOrcThreadSafeModuleRef TSM) JL_NOTSAFEPOINT
{
    std::unique_ptr<orc::ThreadSafeModule> TmpTSM(unwrap(TSM));
    return wrap(unwrap(JIT)->addExternalModule(*unwrap(JD), std::move(*TmpTSM)));
}

JL_DLLEXPORT_CODEGEN LLVMErrorRef
LLVMExtraJLJITLookup_impl(JuliaOJITRef JIT, LLVMOrcExecutorAddress *Result,
                                   const char *Name, int ExternalJDOnly) JL_NOTSAFEPOINT
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
LLVMExtraJLJITMangleAndIntern_impl(JuliaOJITRef JIT,
                                            const char *Name) JL_NOTSAFEPOINT
{
    return wrap(orc::OrcV2CAPIHelper::moveFromSymbolStringPtr(unwrap(JIT)->mangle(Name)));
}

JL_DLLEXPORT_CODEGEN const char *
LLVMExtraJLJITGetTripleString_impl(JuliaOJITRef JIT)
{
    return unwrap(JIT)->getTargetTriple().str().c_str();
}

JL_DLLEXPORT_CODEGEN const char
LLVMExtraJLJITGetGlobalPrefix_impl(JuliaOJITRef JIT)
{
    return unwrap(JIT)->getDataLayout().getGlobalPrefix();
}

JL_DLLEXPORT_CODEGEN const char *
LLVMExtraJLJITGetDataLayoutString_impl(JuliaOJITRef JIT)
{
    return unwrap(JIT)->getDataLayout().getStringRepresentation().c_str();
}

JL_DLLEXPORT_CODEGEN LLVMOrcIRCompileLayerRef
LLVMExtraJLJITGetIRCompileLayer_impl(JuliaOJITRef JIT)
{
    return wrap(&unwrap(JIT)->getIRCompileLayer());
}

} // extern "C"

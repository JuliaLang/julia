// This file is a part of Julia. License is MIT: https://julialang.org/license

// Extensions of the LLVM C API for LLVM.jl
//
// These are part of the Julia repository as they need to be
// built with the same C++ toolchain Julia & LLVM are built with
//
// They are not to be considered a stable API, and will be removed
// when better package build systems are available

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

#include <llvm/ADT/Triple.h>
#include <llvm/Analysis/TargetLibraryInfo.h>
#include <llvm/IR/Attributes.h>
#include <llvm/IR/CallSite.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Transforms/IPO.h>

#include "julia.h"
#include "llvm-version.h"

using namespace llvm::legacy;

namespace llvm {


// Initialization functions
//
// The LLVMInitialize* functions and friends are defined `static inline`

extern "C" JL_DLLEXPORT void LLVMExtraInitializeAllTargetInfos()
{
    InitializeAllTargetInfos();
}

extern "C" JL_DLLEXPORT void LLVMExtraInitializeAllTargets()
{
    InitializeAllTargets();
}

extern "C" JL_DLLEXPORT void LLVMExtraInitializeAllTargetMCs()
{
    InitializeAllTargetMCs();
}

extern "C" JL_DLLEXPORT void LLVMExtraInitializeAllAsmPrinters()
{
    InitializeAllAsmPrinters();
}

extern "C" JL_DLLEXPORT void LLVMExtraInitializeAllAsmParsers()
{
    InitializeAllAsmParsers();
}

extern "C" JL_DLLEXPORT void LLVMExtraInitializeAllDisassemblers()
{
    InitializeAllDisassemblers();
}

extern "C" JL_DLLEXPORT LLVMBool LLVMExtraInitializeNativeTarget()
{
    return InitializeNativeTarget();
}

extern "C" JL_DLLEXPORT LLVMBool LLVMExtraInitializeNativeAsmParser()
{
    return InitializeNativeTargetAsmParser();
}

extern "C" JL_DLLEXPORT LLVMBool LLVMExtraInitializeNativeAsmPrinter()
{
    return InitializeNativeTargetAsmPrinter();
}

extern "C" JL_DLLEXPORT LLVMBool LLVMExtraInitializeNativeDisassembler()
{
    return InitializeNativeTargetDisassembler();
}


// Infrastructure for writing LLVM passes in Julia

typedef struct LLVMOpaquePass *LLVMPassRef;
DEFINE_STDCXX_CONVERSION_FUNCTIONS(Pass, LLVMPassRef)

extern "C" JL_DLLEXPORT void
LLVMExtraAddPass(LLVMPassManagerRef PM, LLVMPassRef P)
{
    unwrap(PM)->add(unwrap(P));
}

StringMap<char *> PassIDs;
char &CreatePassID(const char *Name)
{
    std::string NameStr(Name);
    if (PassIDs.find(NameStr) != PassIDs.end())
        return *PassIDs[NameStr];
    else
        return *(PassIDs[NameStr] = new char);
}

class JuliaModulePass : public ModulePass {
public:
    JuliaModulePass(const char *Name, jl_value_t *Callback)
        : ModulePass(CreatePassID(Name)), Callback(Callback)
    {
    }

    bool runOnModule(Module &M)
    {
        jl_value_t **argv;
        JL_GC_PUSHARGS(argv, 2);
        argv[0] = Callback;
        argv[1] = jl_box_voidpointer(wrap(&M));

        jl_value_t *ret = jl_apply(argv, 2);
        bool changed = jl_unbox_bool(ret);

        JL_GC_POP();
        return changed;
    }

private:
    jl_value_t *Callback;
};

extern "C" JL_DLLEXPORT LLVMPassRef
LLVMExtraCreateModulePass(const char *Name, jl_value_t *Callback)
{
    return wrap(new JuliaModulePass(Name, Callback));
}

class JuliaFunctionPass : public FunctionPass {
public:
    JuliaFunctionPass(const char *Name, jl_value_t *Callback)
        : FunctionPass(CreatePassID(Name)), Callback(Callback)
    {
    }

    bool runOnFunction(Function &Fn)
    {
        jl_value_t **argv;
        JL_GC_PUSHARGS(argv, 2);
        argv[0] = Callback;
        argv[1] = jl_box_voidpointer(wrap(&Fn));

        jl_value_t *ret = jl_apply(argv, 2);
        bool changed = jl_unbox_bool(ret);

        JL_GC_POP();
        return changed;
    }

private:
    jl_value_t *Callback;
};

extern "C" JL_DLLEXPORT LLVMPassRef
LLVMExtraCreateFunctionPass(const char *Name, jl_value_t *Callback)
{
    return wrap(new JuliaFunctionPass(Name, Callback));
}

class JuliaBasicBlockPass : public BasicBlockPass {
public:
    JuliaBasicBlockPass(const char *Name, jl_value_t *Callback)
        : BasicBlockPass(CreatePassID(Name)), Callback(Callback)
    {
    }

    bool runOnBasicBlock(BasicBlock &BB)
    {
        jl_value_t **argv;
        JL_GC_PUSHARGS(argv, 2);
        argv[0] = Callback;
        argv[1] = jl_box_voidpointer(wrap(&BB));

        jl_value_t *ret = jl_apply(argv, 2);
        bool changed = jl_unbox_bool(ret);

        JL_GC_POP();
        return changed;
    }

private:
    jl_value_t *Callback;
};

extern "C" JL_DLLEXPORT LLVMPassRef
LLVMExtraCreateBasicBlockPass(const char *Name, jl_value_t *Callback)
{
    return wrap(new JuliaBasicBlockPass(Name, Callback));
}


// Various missing functions

extern "C" JL_DLLEXPORT unsigned int LLVMExtraGetDebugMDVersion()
{
    return DEBUG_METADATA_VERSION;
}

extern "C" JL_DLLEXPORT LLVMContextRef LLVMExtraGetValueContext(LLVMValueRef V)
{
    return wrap(&unwrap(V)->getContext());
}

extern ModulePass *createNVVMReflectPass();
extern "C" JL_DLLEXPORT void LLVMExtraAddMVVMReflectPass(LLVMPassManagerRef PM)
{
    createNVVMReflectPass();
}

extern "C" JL_DLLEXPORT void
LLVMExtraAddTargetLibraryInfoByTiple(const char *T, LLVMPassManagerRef PM)
{
    unwrap(PM)->add(new TargetLibraryInfoWrapperPass(Triple(T)));
}

extern "C" JL_DLLEXPORT void LLVMExtraAddInternalizePassWithExportList(
        LLVMPassManagerRef PM, const char **ExportList, size_t Length)
{
    auto PreserveFobj = [=](const GlobalValue &GV) {
        for (size_t i = 0; i < Length; i++) {
            if (strcmp(ExportList[i], GV.getName().data()) == 0)
                return true;
        }
        return false;
    };
    unwrap(PM)->add(createInternalizePass(PreserveFobj));
}

} // namespace llvm

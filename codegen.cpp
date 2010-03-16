#include "llvm/DerivedTypes.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/Interpreter.h"
#include "llvm/ExecutionEngine/JIT.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/ModuleProvider.h"
#include "llvm/PassManager.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Target/TargetSelect.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Support/IRBuilder.h"
#include <cstdio>
#include <string>
#include <map>
#include <vector>
using namespace llvm;

extern "C" {
#ifndef NO_BOEHM_GC
#include <gc.h>
#endif
#include "llt.h"
#include "julia.h"
}

static LLVMContext &jl_LLVMContext = getGlobalContext();
static IRBuilder<> builder(getGlobalContext());
static Module *jl_Module;
static ExecutionEngine *jl_ExecutionEngine;

extern "C" void jl_compile(jl_lambda_info_t *li)
{
}

extern "C" void hello_func()
{
    ios_printf(ios_stdout, "hello, llvm.\n");
}

static Function *emit_lambda()
{
    FunctionType *ft =
        FunctionType::get(Type::getVoidTy(jl_LLVMContext),
                          new std::vector<const Type*>(0));
    Function *f = Function::Create(ft, Function::ExternalLinkage, "foobar",
                                   jl_Module);
    Function *hello = Function::Create(ft, Function::ExternalLinkage,
                                       "hello_func", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(hello, (void*)&hello_func);
    if (f==NULL || ft == NULL || hello == NULL)
        jl_error("something went wrong.");
    
    BasicBlock *bb = BasicBlock::Create(jl_LLVMContext, "entry", f);
    builder.SetInsertPoint(bb);
    std::vector<Value*> argsv(0);
    builder.CreateCall(hello, argsv.begin(), argsv.end());
    builder.CreateRetVoid();
    verifyFunction(*f);
    return f;
}

extern "C" void jl_init_codegen()
{
    InitializeNativeTarget();
    jl_Module = new Module("julia", jl_LLVMContext);
    jl_ExecutionEngine = EngineBuilder(jl_Module).create();

    // test
    Function *f = emit_lambda();
    void *fptr = jl_ExecutionEngine->getPointerToFunction(f);
    void (*fp)() = (void (*)())fptr;
    fp();
}

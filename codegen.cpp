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
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Bitcode/ReaderWriter.h"
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
static const Type *jl_value_llvmt;
static const FunctionType *jl_func_sig;
static GlobalVariable *jltrue_var;
static GlobalVariable *jlfalse_var;
static GlobalVariable *jlnull_var;

extern "C" void jl_compile(jl_lambda_info_t *li)
{
}

extern "C" void hello_func()
{
    ios_printf(ios_stdout, "hello, llvm.\n");
}

static GlobalVariable *global_to_llvm(const std::string &cname, void *addr)
{
    GlobalVariable *gv;
    gv = new GlobalVariable(*jl_Module, jl_value_llvmt,
                            true, GlobalVariable::ExternalLinkage,
                            NULL, cname);
    jl_ExecutionEngine->addGlobalMapping(gv, addr);
    return gv;
}

static void init_julia_llvm_env(Module *m)
{
    // add needed base definitions to our LLVM environment
    MemoryBuffer *deffile = MemoryBuffer::getFile("julia-defs.s.bc");
    Module *jdefs = ParseBitcodeFile(deffile, getGlobalContext());
    delete deffile;

    jl_value_llvmt = jdefs->getTypeByName("struct._jl_value_t");
    jl_func_sig = dynamic_cast<const FunctionType*>(jdefs->getTypeByName("jl_callable_t"));

    jltrue_var = global_to_llvm("jl_true", (void*)&jl_true);
    jlfalse_var = global_to_llvm("jl_false", (void*)&jl_false);
    jlnull_var = global_to_llvm("jl_null", (void*)&jl_null);
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

    init_julia_llvm_env(jl_Module);

    // test
    Function *f = emit_lambda();
    void *fptr = jl_ExecutionEngine->getPointerToFunction(f);
    void (*fp)() = (void (*)())fptr;
    fp();
}

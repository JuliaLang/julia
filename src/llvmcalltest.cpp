// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-version.h"
#include "support/platform.h"
#include "support/dtypes.h"

#include "llvm/Config/llvm-config.h"
#include "llvm/IR/IRBuilder.h"
#include <llvm/Support/raw_ostream.h>

#include "julia.h"
#include "codegen_shared.h"

using namespace llvm;

// Borrow definition from `support/dtypes.h`
#ifdef _OS_WINDOWS_
#  define DLLEXPORT __declspec(dllexport)
#else
# if defined(_OS_LINUX_)
#  define DLLEXPORT __attribute__ ((visibility("protected")))
# else
#  define DLLEXPORT __attribute__ ((visibility("default")))
# endif
#endif

extern "C" {

DLLEXPORT const char *MakeIdentityFunction(jl_value_t* jl_AnyTy) {
    LLVMContext Ctx;
    // FIXME: get TrackedTy via jl_type_to_llvm(Ctx, jl_AnyTy)
    Type *TrackedTy = PointerType::get(StructType::get(Ctx), AddressSpace::Tracked);
    Module *M = new llvm::Module("shadow", Ctx);
    Function *F = Function::Create(
        FunctionType::get(
            TrackedTy, {TrackedTy}, false),
        llvm::GlobalValue::ExternalLinkage,
        "identity",
        M
    );

    IRBuilder<> Builder(BasicBlock::Create(Ctx, "top", F));
    Builder.CreateRet(&*F->arg_begin());

    std::string buf;
    raw_string_ostream os(buf);
    M->print(os, NULL);
    os.flush();
    return strdup(buf.c_str());
}

DLLEXPORT const char *MakeLoadGlobalFunction() {
    LLVMContext Ctx;

    auto M = new Module("shadow", Ctx);
    auto intType = Type::getInt32Ty(Ctx);
    auto G = new GlobalVariable(
        *M,
        intType,
        true,
        GlobalValue::InternalLinkage,
        Constant::getNullValue(intType),
        "test_global_var");

    auto resultType = Type::getInt64Ty(Ctx);
    auto F = Function::Create(
        FunctionType::get(resultType, {}, false),
        GlobalValue::ExternalLinkage,
        "load_global_var",
        M);

    IRBuilder<> Builder(BasicBlock::Create(Ctx, "top", F));
    Builder.CreateRet(Builder.CreatePtrToInt(G, resultType));

    std::string buf;
    raw_string_ostream os(buf);
    M->print(os, NULL);
    os.flush();
    return strdup(buf.c_str());
}

}

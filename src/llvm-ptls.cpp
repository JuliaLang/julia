// This file is a part of Julia. License is MIT: http://julialang.org/license

#define DEBUG_TYPE "lower_ptls"
#undef DEBUG

// LLVM pass to optimize TLS access and remove references to julia intrinsics

#include "llvm-version.h"
#include "support/dtypes.h"

#include <llvm/Pass.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/LLVMContext.h>

#include "julia.h"
#include "julia_internal.h"

#if defined(LLVM37) && defined(JULIA_ENABLE_THREADING)
#include <llvm/IR/InlineAsm.h>
#endif

using namespace llvm;

namespace {

struct LowerPTLS : public ModulePass {
    static char ID;
    LowerPTLS(bool _imaging_mode = false, MDNode *_tbaa_const = nullptr)
        : ModulePass(ID), imaging_mode(_imaging_mode), tbaa_const(_tbaa_const)
    {
    }

private:
    bool    imaging_mode;
    MDNode *tbaa_const; // One `LLVMContext` only
    bool runOnModule(Module &M) override;
    void runOnFunction(LLVMContext &ctx, Module &M, Function *F, Function *ptls_getter,
                       Type *T_ppjlvalue);
};

static void ensure_global(const char *name, Type *t, Module &M, bool dllimport = false)
{
    if (M.getNamedValue(name))
        return;
    GlobalVariable *proto =
        new GlobalVariable(M, t, false, GlobalVariable::ExternalLinkage, NULL, name);
#ifdef _OS_WINDOWS_
    // setting JL_DLLEXPORT correctly only matters when building a binary
    // (global_proto will strip this from the JIT)
    if (dllimport) {
#ifdef LLVM35
        // add the __declspec(dllimport) attribute
        proto->setDLLStorageClass(GlobalValue::DLLImportStorageClass);
#else
        proto->setLinkage(GlobalValue::DLLImportLinkage);
#endif
    }
#else  // _OS_WINDOWS_
    (void)proto;
#endif // _OS_WINDOWS_
}

void LowerPTLS::runOnFunction(LLVMContext &ctx, Module &M, Function *F,
                              Function *ptls_getter, Type *T_ppjlvalue)
{
    CallInst *ptlsStates = NULL;
    for (auto I = F->getEntryBlock().begin(), E = F->getEntryBlock().end(); I != E; ++I) {
        if (CallInst *callInst = dyn_cast<CallInst>(&*I)) {
            if (callInst->getCalledValue() == ptls_getter) {
                ptlsStates = callInst;
                break;
            }
        }
    }
    if (!ptlsStates)
        return;

    if (ptlsStates->use_empty()) {
        ptlsStates->eraseFromParent();
        return;
    }

#ifdef JULIA_ENABLE_THREADING
    if (imaging_mode) {
        GlobalVariable *GV =
            cast<GlobalVariable>(M.getNamedValue("jl_get_ptls_states.ptr"));
        LoadInst *getter = new LoadInst(GV, "", ptlsStates);
        getter->setMetadata(llvm::LLVMContext::MD_tbaa, tbaa_const);
        ptlsStates->setCalledFunction(getter);
        ptlsStates->addAttribute(AttributeSet::FunctionIndex, Attribute::ReadNone);
        ptlsStates->addAttribute(AttributeSet::FunctionIndex, Attribute::NoUnwind);
    }
    else if (jl_tls_offset != -1) {
#ifdef LLVM37
        auto T_int8  = Type::getInt8Ty(ctx);
        auto T_pint8 = PointerType::get(T_int8, 0);
        auto T_size = (sizeof(size_t) == 8 ? Type::getInt64Ty(ctx) : Type::getInt32Ty(ctx));
        // Replace the function call with inline assembly if we know
        // how to generate it.
        const char *asm_str = nullptr;
#if defined(_CPU_X86_64_)
        asm_str = "movq %fs:0, $0";
#elif defined(_CPU_X86_)
        asm_str = "movl %gs:0, $0";
#elif defined(_CPU_AARCH64_)
        asm_str = "mrs $0, tpidr_el0";
#endif
        assert(asm_str && "Cannot emit thread pointer for this architecture.");
        auto   offset = ConstantInt::getSigned(T_size, jl_tls_offset);
        auto   tp = InlineAsm::get(FunctionType::get(T_pint8, false), asm_str, "=r", false);
        Value *tls = CallInst::Create(tp, "thread_ptr", ptlsStates);
        tls = GetElementPtrInst::Create(T_int8, tls, {offset}, "ptls_i8", ptlsStates);
        tls = new BitCastInst(tls, PointerType::get(T_ppjlvalue, 0), "ptls", ptlsStates);
        ptlsStates->replaceAllUsesWith(tls);
        ptlsStates->eraseFromParent();
#endif
    }
    else {
        ptlsStates->addAttribute(AttributeSet::FunctionIndex, Attribute::ReadNone);
        ptlsStates->addAttribute(AttributeSet::FunctionIndex, Attribute::NoUnwind);
    }
#else
    ptlsStates->replaceAllUsesWith(M.getNamedValue("jl_tls_states"));
    ptlsStates->eraseFromParent();
#endif
}

static void eraseFunction(Module &M, const char *name)
{
    if (Function *f = M.getFunction(name)) {
        f->eraseFromParent();
    }
}

bool LowerPTLS::runOnModule(Module &M)
{
    // Cleanup for GC frame lowering.
    eraseFunction(M, "julia.gc_root_decl");
    eraseFunction(M, "julia.gc_root_kill");
    eraseFunction(M, "julia.jlcall_frame_decl");
    eraseFunction(M, "julia.gcroot_flush");

    Function *ptls_getter = M.getFunction("jl_get_ptls_states");
    if (!ptls_getter)
        return true;
    LLVMContext & ctx      = M.getContext();
    FunctionType *functype = ptls_getter->getFunctionType();
    auto T_ppjlvalue       = cast<PointerType>(functype->getReturnType())->getElementType();
#ifdef JULIA_ENABLE_THREADING
    if (imaging_mode)
        ensure_global("jl_get_ptls_states.ptr", functype->getPointerTo(), M);
#else
    ensure_global("jl_tls_states", T_ppjlvalue, M, imaging_mode);
#endif
    for (auto F = M.begin(), E = M.end(); F != E; ++F) {
        if (F->isDeclaration())
            continue;
        runOnFunction(ctx, M, &*F, ptls_getter, T_ppjlvalue);
    }
#ifndef JULIA_ENABLE_THREADING
    ptls_getter->eraseFromParent();
#endif
    return true;
}

char LowerPTLS::ID = 0;

static RegisterPass<LowerPTLS> X("LowerPTLS", "LowerPTLS Pass",
                                 false /* Only looks at CFG */, false /* Analysis Pass */);

} // anonymous namespace

Pass *createLowerPTLSPass(bool imaging_mode, MDNode *tbaa_const)
{
    assert(tbaa_const);
    return new LowerPTLS(imaging_mode, tbaa_const);
}

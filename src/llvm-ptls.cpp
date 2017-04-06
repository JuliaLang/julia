// This file is a part of Julia. License is MIT: http://julialang.org/license

#define DEBUG_TYPE "lower_ptls"
#undef DEBUG

// LLVM pass to optimize TLS access and remove references to julia intrinsics

#include "llvm-version.h"
#include "support/dtypes.h"
#include <sstream>

#include <llvm/Pass.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/LLVMContext.h>
#if JL_LLVM_VERSION >= 30700
#include <llvm/IR/LegacyPassManager.h>
#else
#include <llvm/PassManager.h>
#endif
#include <llvm/IR/MDBuilder.h>

#include "julia.h"
#include "julia_internal.h"

#if JL_LLVM_VERSION >= 30700 && defined(JULIA_ENABLE_THREADING)
#  include <llvm/IR/InlineAsm.h>
#endif

using namespace llvm;

extern std::pair<MDNode*,MDNode*> tbaa_make_child(const char *name, MDNode *parent=nullptr, bool isConstant=false);

namespace {

struct LowerPTLS: public ModulePass {
    static char ID;
    LowerPTLS(bool _imaging_mode=false)
        : ModulePass(ID),
          imaging_mode(_imaging_mode)
    {}

private:
    bool imaging_mode;
    bool runOnModule(Module &M) override;
    void runOnFunction(LLVMContext &ctx, Module &M, Function *F,
                       Function *ptls_getter, Type *T_ppjlvalue, MDNode *tbaa_const);
};

static void ensure_global(const char *name, Type *t, Module &M,
                          bool dllimport=false)
{
    if (M.getNamedValue(name))
        return;
    GlobalVariable *proto = new GlobalVariable(M, t, false,
                                               GlobalVariable::ExternalLinkage,
                                               NULL, name);
#ifdef _OS_WINDOWS_
    // setting JL_DLLEXPORT correctly only matters when building a binary
    // (global_proto will strip this from the JIT)
    if (dllimport) {
#if JL_LLVM_VERSION >= 30500
        // add the __declspec(dllimport) attribute
        proto->setDLLStorageClass(GlobalValue::DLLImportStorageClass);
#else
        proto->setLinkage(GlobalValue::DLLImportLinkage);
#endif
    }
#else // _OS_WINDOWS_
    (void)proto;
#endif // _OS_WINDOWS_
}

void LowerPTLS::runOnFunction(LLVMContext &ctx, Module &M, Function *F,
                              Function *ptls_getter, Type *T_ppjlvalue, MDNode *tbaa_const)
{
    CallInst *ptlsStates = NULL;
    for (auto I = F->getEntryBlock().begin(), E = F->getEntryBlock().end();
         I != E; ++I) {
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
        GlobalVariable *GV = cast<GlobalVariable>(
            M.getNamedValue("jl_get_ptls_states.ptr"));
        LoadInst *getter = new LoadInst(GV, "", ptlsStates);
        getter->setMetadata(llvm::LLVMContext::MD_tbaa, tbaa_const);
        ptlsStates->setCalledFunction(getter);
        ptlsStates->addAttribute(AttributeSet::FunctionIndex,
                                 Attribute::ReadNone);
        ptlsStates->addAttribute(AttributeSet::FunctionIndex,
                                 Attribute::NoUnwind);
    }
#if JL_LLVM_VERSION >= 30700
    else if (jl_tls_offset != -1) {
        auto T_int8 = Type::getInt8Ty(ctx);
        auto T_pint8 = PointerType::get(T_int8, 0);
        // Replace the function call with inline assembly if we know
        // how to generate it.
#  if defined(_CPU_X86_64_) || defined(_CPU_X86_)
        // Workaround LLVM bug by hiding the offset computation
        // (and therefore the optimization opportunity) from LLVM.
        static const std::string asm_str = [&] () {
            std::stringstream stm;
#  if defined(_CPU_X86_64_)
            stm << "movq %fs:0, $0;\naddq $$" << jl_tls_offset << ", $0";
#  else
            stm << "movl %gs:0, $0;\naddl $$" << jl_tls_offset << ", $0";
#  endif
            return stm.str();
        }();
        // The add instruction clobbers flags
        auto tp = InlineAsm::get(FunctionType::get(T_pint8, false),
                                 asm_str.c_str(),
                                 "=r,~{dirflag},~{fpsr},~{flags}", false);
        Value *tls = CallInst::Create(tp, "ptls_i8", ptlsStates);
        tls = new BitCastInst(tls, PointerType::get(T_ppjlvalue, 0),
                              "ptls", ptlsStates);
#  elif defined(_CPU_AARCH64_)
        // AArch64 doesn't seem to have this issue.
        // (Possibly because there are many more registers and the offset is
        // positive and small)
        // It's also harder to emit the offset in a generic way on AArch64
        // (need to generate one or two `add` with shift) so let llvm emit
        // the add for now.
        auto T_size = (sizeof(size_t) == 8 ? Type::getInt64Ty(ctx) :
                       Type::getInt32Ty(ctx));
        const char *asm_str = "mrs $0, tpidr_el0";
        auto offset = ConstantInt::getSigned(T_size, jl_tls_offset);
        auto tp = InlineAsm::get(FunctionType::get(T_pint8, false),
                                 asm_str, "=r", false);
        Value *tls = CallInst::Create(tp, "thread_ptr", ptlsStates);
        tls = GetElementPtrInst::Create(T_int8, tls, {offset},
                                        "ptls_i8", ptlsStates);
        tls = new BitCastInst(tls, PointerType::get(T_ppjlvalue, 0),
                              "ptls", ptlsStates);
#  else
        Value *tls = nullptr;
        assert(0 && "Cannot emit thread pointer for this architecture.");
#  endif
        (void)T_pint8;
        ptlsStates->replaceAllUsesWith(tls);
        ptlsStates->eraseFromParent();
    }
#endif
    else {
        ptlsStates->addAttribute(AttributeSet::FunctionIndex,
                                 Attribute::ReadNone);
        ptlsStates->addAttribute(AttributeSet::FunctionIndex,
                                 Attribute::NoUnwind);
    }
#else
    ptlsStates->replaceAllUsesWith(M.getNamedValue("jl_tls_states"));
    ptlsStates->eraseFromParent();
#endif
}

bool LowerPTLS::runOnModule(Module &M)
{
    MDNode *tbaa_const = tbaa_make_child("jtbaa_const", nullptr, true).first;

    Function *ptls_getter = M.getFunction("jl_get_ptls_states");
    if (!ptls_getter)
        return true;
    LLVMContext &ctx = M.getContext();
    FunctionType *functype = ptls_getter->getFunctionType();
    auto T_ppjlvalue =
        cast<PointerType>(functype->getReturnType())->getElementType();
#ifdef JULIA_ENABLE_THREADING
    if (imaging_mode)
        ensure_global("jl_get_ptls_states.ptr", functype->getPointerTo(), M);
#else
    ensure_global("jl_tls_states", T_ppjlvalue, M, imaging_mode);
#endif
    for (auto F = M.begin(), E = M.end(); F != E; ++F) {
        if (F->isDeclaration())
            continue;
        runOnFunction(ctx, M, &*F, ptls_getter, T_ppjlvalue, tbaa_const);
    }
#ifndef JULIA_ENABLE_THREADING
    ptls_getter->eraseFromParent();
#endif
    return true;
}

char LowerPTLS::ID = 0;

static RegisterPass<LowerPTLS> X("LowerPTLS", "LowerPTLS Pass",
                                 false /* Only looks at CFG */,
                                 false /* Analysis Pass */);

} // anonymous namespace

Pass *createLowerPTLSPass(bool imaging_mode)
{
    return new LowerPTLS(imaging_mode);
}

extern "C" JL_DLLEXPORT
void LLVMAddLowerPTLSPass(LLVMPassManagerRef PM, int imaging_mode) {
    unwrap(PM)->add(createLowerPTLSPass(imaging_mode != 0));
}

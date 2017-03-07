// This file is a part of Julia. License is MIT: http://julialang.org/license

#define DEBUG_TYPE "add_safepoint"
#undef DEBUG

// LLVM pass to optimize TLS access and remove references to julia intrinsics

#include "llvm-version.h"
#include "support/dtypes.h"
#include <sstream>

#include <llvm/Analysis/LoopPass.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/LLVMContext.h>
#if JL_LLVM_VERSION >= 30700
#  include <llvm/IR/LegacyPassManager.h>
#else
#  include <llvm/PassManager.h>
#endif
#include <llvm/IR/MDBuilder.h>

#if JL_LLVM_VERSION >= 30700 && defined(JULIA_ENABLE_THREADING)
#  include <llvm/IR/InlineAsm.h>
#endif

#include "julia.h"
#include "julia_internal.h"

using namespace llvm;

static Instruction *emit_signal_fence(LLVMContext &ctx, Instruction *ins)
{
#if JL_LLVM_VERSION >= 30900
    return new FenceInst(ctx, AtomicOrdering::SequentiallyConsistent, SingleThread, ins);
#else
    return new FenceInst(ctx, SequentiallyConsistent, SingleThread, ins);
#endif
}

namespace {

struct AddSafepoint: public LoopPass {
    static char ID;
    AddSafepoint()
        : LoopPass(ID)
    {}

private:
    bool runOnLoop(Loop *L, LPPassManager &mgr) override;
};

bool AddSafepoint::runOnLoop(Loop *L, LPPassManager &mgr)
{
    BasicBlock *header = L->getHeader();
    Function *F = header->getParent();
    Module *M = F->getParent();
    Function *ptls_getter = M->getFunction("jl_get_ptls_states");
    LLVMContext &ctx = M->getContext();
    FunctionType *functype = ptls_getter->getFunctionType();
    auto T_ppjlvalue =
        cast<PointerType>(functype->getReturnType())->getElementType();
    if (!ptls_getter)
        return true;

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
        return true;

    Instruction *ins = header->getFirstNonPHI();
    int nthfield = offsetof(jl_tls_states_t, safepoint) / sizeof(void*);
    Instruction *ptr = new BitCastInst(ptlsStates, T_ppjlvalue, "", ins);
    auto idx = ConstantInt::get(Type::getInt32Ty(ctx), nthfield);
    ptr = GetElementPtrInst::Create(nullptr, ptr, {idx}, "", ins);
    ptr = new BitCastInst(ptr, T_ppjlvalue, "", ins);
    ptr = new LoadInst(ptr, "", ins);
    ptr = new BitCastInst(ptr, T_ppjlvalue, "", ins);
    emit_signal_fence(ctx, ins);
    new LoadInst(ptr, "", true, ins);
    emit_signal_fence(ctx, ins);
    return true;
}

char AddSafepoint::ID = 0;

static RegisterPass<AddSafepoint> X("AddSafepoint", "AddSafepoint Pass",
                                    false /* Only looks at CFG */,
                                    false /* Analysis Pass */);

} // anonymous namespace

Pass *createAddSafepointPass()
{
    return new AddSafepoint();
}

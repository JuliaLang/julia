// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-version.h"
#include "passes.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

#include <llvm/ADT/DepthFirstIterator.h>
#include <llvm/Analysis/CFG.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Pass.h>
#include <llvm/Support/Debug.h>
#include <llvm/Transforms/Utils/BasicBlockUtils.h>

#include "julia.h"
#include "julia_assert.h"
#include "codegen_shared.h"
#include <map>

#define DEBUG_TYPE "lower_handlers"
#undef DEBUG

using namespace llvm;

/* Lowers Julia Exception Handlers and colors EH frames.
 *
 *  Our task is to lower:
 * call void @julia.except_enter()
 * <...>
 * call void jl_pop_handler(1)
 *
 * to
 *
 * call void @jl_enter_handler(jl_handler *%buff)
 * <...>
 * call void jl_pop_handler(1)
 *
 * Where buff is an appropriate stack slot handler.
 *
 * We make the following assumptions:
 *  - All EH frames are completely nested.
 *  - The exception nestedness of a BB is not dynamic. I.e. we don't allow
 *    the following:
 *                br i1 %cond, %left, %right
 *                  /                \
 *           except.enter         br mid
 *           br mid                  |
 *                 \                 /
 *              br i1 %cond, %left2, %right2
 *                 /                 \
 *           jl_pop_hander          ret
 *           ret
 *
 *    The frontend doesn't emit structures like this. However, the optimizer
 *    could easily introduce them, so this pass should run early after IRGen.
 *
 * Because of these assumptions, the algorithm is very simple. We simply label
 * the handler depth at every basic block using a DFS search. For each enter
 * we encounter, we record the current depth and then allocate an exception
 * handler frame for every level.
 *
 * As an additional optimization, we also insert lifetime intrinsics for the
 * handler structures to tell LLVM that it is free to re-use the stack slot
 * while the handler is not being used.
 */

namespace {
/*
 * If the module doesn't have declarations for the jl_enter_handler and setjmp
 * functions, insert them.
 */
static void ensure_enter_function(Module &M)
{
    auto T_int8  = Type::getInt8Ty(M.getContext());
    auto T_pint8 = PointerType::get(T_int8, 0);
    auto T_void = Type::getVoidTy(M.getContext());
    auto T_int32 = Type::getInt32Ty(M.getContext());
    if (!M.getNamedValue(XSTR(jl_enter_handler))) {
        std::vector<Type*> ehargs(0);
        ehargs.push_back(T_pint8);
        Function::Create(FunctionType::get(T_void, ehargs, false),
                         Function::ExternalLinkage, XSTR(jl_enter_handler), &M);
    }
    if (!M.getNamedValue(jl_setjmp_name)) {
        std::vector<Type*> args2(0);
        args2.push_back(T_pint8);
#ifndef _OS_WINDOWS_
        args2.push_back(T_int32);
#endif
        Function::Create(FunctionType::get(T_int32, args2, false),
                         Function::ExternalLinkage, jl_setjmp_name, &M)
            ->addFnAttr(Attribute::ReturnsTwice);
    }
}

static bool lowerExcHandlers(Function &F) {
    Module &M = *F.getParent();
    Function *except_enter_func = M.getFunction("julia.except_enter");
    if (!except_enter_func)
        return false; // No EH frames in this module
    ensure_enter_function(M);
    Function *leave_func = M.getFunction(XSTR(jl_pop_handler));
    Function *jlenter_func = M.getFunction(XSTR(jl_enter_handler));
    Function *setjmp_func = M.getFunction(jl_setjmp_name);

    auto T_pint8 = Type::getInt8PtrTy(M.getContext(), 0);
    Function *lifetime_start = Intrinsic::getDeclaration(&M, Intrinsic::lifetime_start, { T_pint8 });
    Function *lifetime_end = Intrinsic::getDeclaration(&M, Intrinsic::lifetime_end, { T_pint8 });

    /* Step 1: EH Depth Numbering */
    std::map<llvm::CallInst *, int> EnterDepth;
    std::map<llvm::CallInst *, int> LeaveDepth;
    std::map<BasicBlock *, int> ExitDepth;
    int MaxDepth = 0;
    // Compute EH Depth at each basic block using a DFS traversal.
    for (df_iterator<BasicBlock *> I = df_begin(&F.getEntryBlock()),
            E = df_end(&F.getEntryBlock()); I != E; ++I) {
        auto *BB = *I;
        int Depth = 0;
        /* Here we use the assumption that all incoming edges have the same
         * EH depth.
         */
        for (auto *Pred : predecessors(BB)) {
            auto it = ExitDepth.find(Pred);
            if (it != ExitDepth.end()) {
                Depth = it->second;
                break;
            }
        }
        /* Compute the depth within the basic block */
        for (auto &I : *BB) {
            auto *CI = dyn_cast<CallInst>(&I);
            if (!CI)
                continue;
            Function *Callee = CI->getCalledFunction();
            if (!Callee)
                continue;
            if (Callee == except_enter_func)
                EnterDepth[CI] = Depth++;
            else if (Callee == leave_func) {
                LeaveDepth[CI] = Depth;
                Depth -= cast<ConstantInt>(CI->getArgOperand(0))->getLimitedValue();
            }
            assert(Depth >= 0);
            if (Depth > MaxDepth)
                MaxDepth = Depth;
        }
        /* Remember the depth at the BB boundary */
        ExitDepth[BB] = Depth;
    }

    /* Step 2: EH Frame lowering */
    // Allocate stack space for each handler. We allocate these as separate
    // allocas so the optimizer can later merge and rearrange them if it wants
    // to.
    Value *handler_sz = ConstantInt::get(Type::getInt32Ty(F.getContext()),
                                         sizeof(jl_handler_t));
    Value *handler_sz64 = ConstantInt::get(Type::getInt64Ty(F.getContext()),
                                           sizeof(jl_handler_t));
    Instruction *firstInst = &F.getEntryBlock().front();
    std::vector<Instruction *> buffs;
    unsigned allocaAddressSpace = F.getParent()->getDataLayout().getAllocaAddrSpace();
    for (int i = 0; i < MaxDepth; ++i) {
        auto *buff = new AllocaInst(Type::getInt8Ty(F.getContext()), allocaAddressSpace,
                handler_sz, Align(16), "", firstInst);
        if (allocaAddressSpace) {
            AddrSpaceCastInst *buff_casted = new AddrSpaceCastInst(buff, Type::getInt8PtrTy(F.getContext(), AddressSpace::Generic));
            buff_casted->insertAfter(buff);
            buffs.push_back(buff_casted);
        } else {
            buffs.push_back(buff);
        }
    }

    // Lower enter funcs
    for (auto it : EnterDepth) {
        assert(it.second >= 0);
        Instruction *buff = buffs[it.second];
        CallInst *enter = it.first;
        auto new_enter = CallInst::Create(jlenter_func, buff, "", enter);
        Value *lifetime_args[] = {
            handler_sz64,
            buff
        };
        CallInst::Create(lifetime_start, lifetime_args, "", new_enter);
#ifndef _OS_WINDOWS_
        // For LLVM 3.3 compatibility
        Value *args[] = {buff,
                         ConstantInt::get(Type::getInt32Ty(F.getContext()), 0)};
        auto sj = CallInst::Create(setjmp_func, args, "", enter);
#else
        auto sj = CallInst::Create(setjmp_func, buff, "", enter);
#endif
        // We need to mark this on the call site as well. See issue #6757
        sj->setCanReturnTwice();
        if (auto dbg = enter->getMetadata(LLVMContext::MD_dbg)) {
            new_enter->setMetadata(LLVMContext::MD_dbg, dbg);
            sj->setMetadata(LLVMContext::MD_dbg, dbg);
        }
        enter->replaceAllUsesWith(sj);
        enter->eraseFromParent();
    }
    // Insert lifetime end intrinsics after every leave.
    for (auto it : LeaveDepth) {
        int StartDepth = it.second - 1;
        int npops = cast<ConstantInt>(it.first->getArgOperand(0))->getLimitedValue();
        for (int i = 0; i < npops; ++i) {
            assert(StartDepth-i >= 0);
            Value *lifetime_args[] = {
                handler_sz64,
                buffs[StartDepth-i]
            };
            auto LifetimeEnd = CallInst::Create(lifetime_end, lifetime_args);
            LifetimeEnd->insertAfter(it.first);
        }
    }
    return true;
}

} // anonymous namespace

PreservedAnalyses LowerExcHandlers::run(Function &F, FunctionAnalysisManager &AM)
{
    if (lowerExcHandlers(F)) {
        return PreservedAnalyses::allInSet<CFGAnalyses>();
    }
    return PreservedAnalyses::all();
}


struct LowerExcHandlersLegacy : public FunctionPass {
    static char ID;
    LowerExcHandlersLegacy() : FunctionPass(ID)
    {}
    bool runOnFunction(Function &F) {
        return lowerExcHandlers(F);
    }
};

char LowerExcHandlersLegacy::ID = 0;
static RegisterPass<LowerExcHandlersLegacy> X("LowerExcHandlers", "Lower Julia Exception Handlers",
                                         false /* Only looks at CFG */,
                                         false /* Analysis Pass */);

Pass *createLowerExcHandlersPass()
{
    return new LowerExcHandlersLegacy();
}

extern "C" JL_DLLEXPORT void LLVMExtraAddLowerExcHandlersPass_impl(LLVMPassManagerRef PM)
{
    unwrap(PM)->add(createLowerExcHandlersPass());
}

// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

#include <llvm/ADT/DepthFirstIterator.h>
#include <llvm/Analysis/CFG.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Module.h>
#include <llvm/Pass.h>
#include <llvm/Support/Debug.h>

#include "llvm-version.h"
#include "julia.h"
#include "julia_assert.h"

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
struct LowerExcHandlers : public FunctionPass {
    static char ID;
    LowerExcHandlers() : FunctionPass(ID)
    {}

private:
    Function *except_enter_func;
    Function *leave_func;
    Function *jlenter_func;
    Function *setjmp_func;
    Function *lifetime_start;
    Function *lifetime_end;

    bool doInitialization(Module &M) override;
    bool runOnFunction(Function &F) override;
};

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
    if (!M.getNamedValue("jl_enter_handler")) {
        std::vector<Type*> ehargs(0);
        ehargs.push_back(T_pint8);
        Function::Create(FunctionType::get(T_void, ehargs, false),
                         Function::ExternalLinkage, "jl_enter_handler", &M);
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

bool LowerExcHandlers::doInitialization(Module &M) {
    except_enter_func = M.getFunction("julia.except_enter");
    if (!except_enter_func)
        return false;
    ensure_enter_function(M);
    leave_func = M.getFunction("jl_pop_handler");
    jlenter_func = M.getFunction("jl_enter_handler");
    setjmp_func = M.getFunction(jl_setjmp_name);

    auto T_pint8 = Type::getInt8PtrTy(M.getContext(), 0);
    lifetime_start = Intrinsic::getDeclaration(&M, Intrinsic::lifetime_start, { T_pint8 });
    lifetime_end = Intrinsic::getDeclaration(&M, Intrinsic::lifetime_end, { T_pint8 });
    return true;
}

bool LowerExcHandlers::runOnFunction(Function &F) {
    if (!except_enter_func)
        return false; // No EH frames in this module

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
    // allocas so the optimizer can later merge and reaarange them if it wants
    // to.
    Value *handler_sz = ConstantInt::get(Type::getInt32Ty(F.getContext()),
                                         sizeof(jl_handler_t));
    Value *handler_sz64 = ConstantInt::get(Type::getInt64Ty(F.getContext()),
                                           sizeof(jl_handler_t));
    Instruction *firstInst = &F.getEntryBlock().front();
    std::vector<AllocaInst *> buffs;
    for (int i = 0; i < MaxDepth; ++i) {
        auto *buff = new AllocaInst(Type::getInt8Ty(F.getContext()),
                                       0,
                                       handler_sz, "", firstInst);
        buff->setAlignment(Align(16));
        buffs.push_back(buff);
    }

    // Lower enter funcs
    for (auto it : EnterDepth) {
        assert(it.second >= 0);
        AllocaInst *buff = buffs[it.second];
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

char LowerExcHandlers::ID = 0;
static RegisterPass<LowerExcHandlers> X("LowerExcHandlers", "Lower Julia Exception Handlers",
                                         false /* Only looks at CFG */,
                                         false /* Analysis Pass */);

Pass *createLowerExcHandlersPass()
{
    return new LowerExcHandlers();
}

extern "C" JL_DLLEXPORT void LLVMExtraAddLowerExcHandlersPass(LLVMPassManagerRef PM)
{
    unwrap(PM)->add(createLowerExcHandlersPass());
}

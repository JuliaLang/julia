// This file is a part of Julia. License is MIT: https://julialang.org/license

#define DEBUG_TYPE "eh_outlining"

// LLVM pass creating outlined body for exception handling.

// LLVM has really poor handling of return twice functions
// and it is pretty hard for our pass (alloc-opt) too.
// This pass outlines the exception handling body to a separate function so that return twice
// functions are never visible to LLVM and we can just include a few special purpose passes
// to handle the valid optimization across eh frames.
// This also allow us to remove all the volatile on stack addresses.
//
// The pass needs to be run early in the pipeline so that it's easier to do pattern matching
// and to avoid buggy LLVM. So far it seems that `mem2reg` is OK as long as the stack slots
// are properly marked with `volatile` before removed in this pass.
// We also need to run this early so that there isn't any phi node on derived object pointers
// which can mess up GC roots of output values.

#include "llvm-version.h"
#include "support/dtypes.h"

#include <llvm/ADT/SetVector.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/SmallSet.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DebugInfoMetadata.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/Pass.h>
#include <llvm/Transforms/Utils/BasicBlockUtils.h>
#include <llvm/Transforms/Utils/ValueMapper.h>

#include <set>
#include <map>

#include "julia.h"
#include "julia_internal.h"
#include "codegen_shared.h"
#include "julia_assert.h"

// TODO:
// * Mark output slots with invariant group
// * Optimize dead use across eh frame
// * Update alloc-opt pass
// * Mark arg attributes
// * Identify base value of inputs and outputs
// * Special case printing of backtrace involving eh frame

using namespace llvm;

namespace {

static bool isFromAlloca(Value *V)
{
    return isa<AllocaInst>(V->stripInBoundsOffsets());
}

struct EHOutlining: public ModulePass {
    static char ID;
    EHOutlining()
        : ModulePass(ID)
    {}

private:
    bool runOnModule(Module &M) override;
};

struct EHContext {
    Module &M;
    LLVMContext &ctx;

    IntegerType *T_int32;
    Type *T_void;
    ConstantInt *V_false;
    ConstantInt *V0_int32;

    Function *ptls_getter;
    Function *except_enter;
    Function *except_leave;
    // Catcher wrapper to hide the control flow from LLVM and to make it easier to find
    // exception catcher during optimization and lowering.
    // The first argument is the exception handling callback.
    // When the return type of the callback is `void` it handles the common exception handling
    // pattern:
    //
    //       ...
    //       %set = call i32 @julia.except_enter()
    //       %cond = icmp eq i32 %set, 0
    //       br i1 %cond, label %try, label %catch
    //
    //     try:
    //       ...
    //
    //     catch:
    //       call void @julia.except_leave()
    //       ...
    //
    // and is expected to be used most of the time.
    // The callback will only contains the try block and will only be run once per catch.
    //
    // When the return type of the callback is `i32` it handles the generic use of eh intrinsics
    // when we failed to match the pattern.
    // The callback may be executed multiple times, once per return of `setjmp`.
    Function *catcher;

    SetVector<Instruction*> insts{};
    SmallVector<CallInst*,4> leaves{};
    std::map<CallInst*,uint32_t> leave_id{};
    // This does not contain the entry BB
    SetVector<BasicBlock*> bbs{};

    EHContext(Module &M, Function *except_enter)
        : M(M),
          ctx(M.getContext()),
          T_int32(Type::getInt32Ty(ctx)),
          T_void(Type::getVoidTy(ctx)),
          V_false(ConstantInt::getFalse(ctx)),
          V0_int32(ConstantInt::get(T_int32, 0)),
          ptls_getter(M.getFunction("julia.ptls_states")),
          except_enter(except_enter),
          except_leave(M.getFunction("julia.except_leave")),
          catcher(Function::Create(FunctionType::get(T_int32, {}, true),
                                   Function::ExternalLinkage, "julia.catch_exception", &M))
    {
    }

    void reset()
    {
        insts.clear();
        leaves.clear();
        leave_id.clear();
        bbs.clear();
    }
    void outlineAll();
    void handleEnter(CallInst *enter);
    void deleteFrame(CallInst *enter);
    bool matchEHPattern(CallInst *enter);
    void outlineFrame(CallInst *enter);
    void stripVolatile(Function *F);
};

void EHContext::outlineAll()
{
    while (!except_enter->use_empty()) {
        assert(ptls_getter);
        reset();
        handleEnter(cast<CallInst>(*except_enter->user_begin()));
    }
    except_enter->eraseFromParent();
    // There could be use in unreachable code.
    while (!except_leave->use_empty())
        cast<CallInst>(*except_leave->user_begin())->eraseFromParent();
    except_leave->eraseFromParent();
}

void EHContext::handleEnter(CallInst *enter)
{
    bool isempty = true;
    SmallVector<std::pair<Instruction*,uint32_t>,4> frontier;
    std::pair<Instruction*,uint32_t> cur(enter, 0);
    auto pop = [&] {
        while (true) {
            if (frontier.empty()) {
                cur = {nullptr, 0};
                return;
            }
            cur = frontier.pop_back_val();
            if (insts.count(cur.first) == 0) {
                return;
            }
        }
    };
    auto next = [&] {
        cur.first = &*++BasicBlock::iterator(cur.first);
    };
    auto push = [&] (Instruction *inst) {
        if (insts.count(inst) != 0)
            return;
        frontier.emplace_back(inst, cur.second);
    };
    while (cur.first) {
        insts.insert(cur.first);
        bool may_throw = cur.first->mayWriteToMemory();
        if (auto call = dyn_cast<CallInst>(cur.first)) {
            if (call->getCalledValue() == except_enter) {
                may_throw = false;
                cur.second += 1;
            }
            else if (call->getCalledValue() == except_leave) {
                assert(cur.second > 0);
                if (cur.second <= 1) {
                    uint32_t id = leaves.size();
                    leave_id.emplace(call, id);
                    leaves.push_back(call);
                    pop();
                    continue;
                }
                may_throw = false;
                cur.second--;
            }
        }
        else if (!may_throw && isa<LoadInst>(cur.first)) {
            auto load = cast<LoadInst>(cur.first);
            if (load->isVolatile() && !isFromAlloca(load->getPointerOperand())) {
                may_throw = true;
            }
        }
        else if (may_throw && isa<StoreInst>(cur.first)) {
            auto store = cast<StoreInst>(cur.first);
            if (!store->isAtomic() && isFromAlloca(store->getPointerOperand())) {
                may_throw = false;
            }
        }
        if (may_throw)
            isempty = false;
        if (auto term = dyn_cast<TerminatorInst>(cur.first)) {
            for (auto *succ: term->successors()) {
                bbs.insert(succ);
                push(&*succ->begin());
            }
            pop();
            continue;
        }
        next();
    }
#ifndef JL_NDEBUG
    for (auto bb: bbs) {
        for (auto pred: predecessors(bb)) {
            assert(pred == enter->getParent() || bbs.count(pred) != 0);
        }
    }
#endif
    if (isempty) {
        deleteFrame(enter);
    }
    else {
        outlineFrame(enter);
    }
}

void EHContext::deleteFrame(CallInst *enter)
{
    enter->replaceAllUsesWith(ConstantInt::get(T_int32, 0));
    enter->eraseFromParent();
    for (auto leave: leaves) {
        leave->eraseFromParent();
    }
}

bool EHContext::matchEHPattern(CallInst *enter)
{
    auto nleaves = leaves.size();
    if (nleaves != 1 && nleaves != 2)
        return false;
    BasicBlock::iterator it(enter);
    ++it;
    auto cmp = dyn_cast<CmpInst>(&*it);
    if (!cmp)
        return false;
    if (cmp->getPredicate() != CmpInst::ICMP_EQ)
        return false;
    if (!((cmp->getOperand(0) == enter && cmp->getOperand(1) == V0_int32) ||
          (cmp->getOperand(1) == enter && cmp->getOperand(0) == V0_int32)))
        return false;
    ++it;
    auto br = dyn_cast<BranchInst>(&*it);
    if (!br)
        return false;
    if (br->getCondition() != cmp)
        return false;
    auto throw_bb = br->getSuccessor(1);
    // With only one reachable leave the return value of the catcher function
    // will be wrong (it will always be 1 but the expected value is 0)
    // The more specific version of the function can still be used by patching the
    // return value to 0. See the other `nleaves == 1` branch below when replacing the
    // return ID.
    if (nleaves == 1)
        return &*throw_bb->begin() == leaves[0];
    if (&*throw_bb->begin() == leaves[1])
        return true;
    if (&*throw_bb->begin() == leaves[0]) {
        std::swap(leaves[0], leaves[1]);
        return true;
    }
    return false;
}

void EHContext::outlineFrame(CallInst *enter)
{
    auto F = enter->getFunction();
    auto nleaves = leaves.size();
    std::map<Value*,SmallVector<std::pair<Instruction*,uint32_t>,2>> input_infos;
    SmallVector<Value*,16> inputs;
    std::map<Instruction*,SmallVector<std::pair<Instruction*,uint32_t>,2>> output_infos;
    SmallVector<Instruction*,4> outputs;
    for (auto inst: insts) {
        for (auto &opuse: inst->operands()) {
            auto op = opuse.get();
            if (isa<Instruction>(op)) {
                if (insts.count(cast<Instruction>(op)) != 0) {
                    continue;
                }
            }
            else if (!isa<Argument>(op)) {
                continue;
            }
            auto &uses = input_infos[op];
            if (uses.empty())
                inputs.push_back(op);
            uses.emplace_back(inst, opuse.getOperandNo());
        }
        for (auto &use: inst->uses()) {
            auto user = cast<Instruction>(use.getUser());
            if (insts.count(user) != 0)
                continue;
            auto &uses = output_infos[inst];
            if (uses.empty())
                outputs.push_back(inst);
            uses.emplace_back(user, use.getOperandNo());
        }
    }
    // * Pattern patching and fix up exit point numbering
    bool is_generic = !matchEHPattern(enter);

    // * Fix up and split exit blocks
    BasicBlock *dispatchBB = BasicBlock::Create(ctx, "after_eh", F);
    BasicBlock *unreachBB = BasicBlock::Create(ctx, "unreachable", F);
    IRBuilder<> builder(dispatchBB);
    auto ret_id = builder.CreatePHI(T_int32, nleaves);
    auto dispatch = builder.CreateSwitch(ret_id, unreachBB, nleaves);
    for (uint32_t id = 0; id < nleaves; id++) {
        auto leave = leaves[id];
        auto oldBB = leave->getParent();
        auto newBB = SplitBlock(oldBB, &*++BasicBlock::iterator(leave));
        dispatch->addCase(ConstantInt::get(T_int32, id), newBB);
    }
    builder.SetInsertPoint(unreachBB);
    builder.CreateUnreachable();

    // * Split input blocks
    auto old_enter_bb = enter->getParent();
    auto new_enter_bb = SplitBlock(old_enter_bb, enter);

    // * Fix up output values
    IRBuilder<> prolog_builder(&F->getEntryBlock().front());
    builder.SetInsertPoint(dispatch);
    SmallVector<AllocaInst*,4> output_slots;
    for (uint32_t i = 0; i < outputs.size(); i++) {
        auto val = outputs[i];
        auto val_ty = val->getType();
        Value *newval;
        // This seems to be the only case where a non-tracked special pointer can be live
        // across the leave at this point in the pipeline.
        if (val_ty->isPointerTy() &&
            cast<PointerType>(val_ty)->getAddressSpace() == AddressSpace::Derived) {
            auto select = cast<SelectInst>(val);
            auto c_val = select->getCondition();
            auto t_val = cast<AddrSpaceCastInst>(select->getTrueValue())->getPointerOperand();
            auto f_val = cast<AddrSpaceCastInst>(select->getFalseValue())->getPointerOperand();
            auto t_ty = cast<PointerType>(t_val->getType());
            auto f_ty = cast<PointerType>(f_val->getType());

            outputs.erase(outputs.begin() + i);

            Value *c_newval = c_val;
            Value *t_newval = t_val;
            Value *f_newval = f_val;

            if (isa<Instruction>(c_val) && insts.count(cast<Instruction>(c_val))) {
                auto c_slot = prolog_builder.CreateAlloca(c_val->getType());
                output_slots.push_back(c_slot);
                outputs.insert(outputs.begin() + i, cast<Instruction>(c_val));
                i++;
                c_newval = builder.CreateLoad(c_val->getType(), c_slot);
            }
            if (isa<Instruction>(t_val) && insts.count(cast<Instruction>(t_val))) {
                auto t_slot = prolog_builder.CreateAlloca(t_ty);
                output_slots.push_back(t_slot);
                outputs.insert(outputs.begin() + i, cast<Instruction>(t_val));
                i++;
                t_newval = builder.CreateLoad(t_ty, t_slot);
            }
            t_newval = builder.CreateAddrSpaceCast(t_newval, val_ty);
            if (isa<Instruction>(f_val) && insts.count(cast<Instruction>(f_val))) {
                auto f_slot = prolog_builder.CreateAlloca(f_ty);
                output_slots.push_back(f_slot);
                outputs.insert(outputs.begin() + i, cast<Instruction>(f_val));
                i++;
                f_newval = builder.CreateLoad(f_ty, f_slot);
            }
            f_newval = builder.CreateAddrSpaceCast(f_newval, val_ty);
            newval = builder.CreateSelect(c_newval, t_newval, f_newval);
        }
        else {
            auto slot = prolog_builder.CreateAlloca(val_ty);
            output_slots.push_back(slot);
            newval = builder.CreateLoad(slot);
        }
        for (auto &use: output_infos[val]) {
            use.first->setOperand(use.second, newval);
        }
    }

    // * Split function
    SmallVector<Type*,16> arg_tys;
    SmallVector<Value*,8> call_args;
    for (auto input: inputs) {
        if (auto call = dyn_cast<CallInst>(input)) {
            if (call->getCalledValue() == ptls_getter) {
                continue;
            }
        }
        call_args.push_back(input);
        arg_tys.push_back(input->getType());
    }
    for (auto slot: output_slots) {
        call_args.push_back(slot);
        arg_tys.push_back(slot->getType());
    }
    if (is_generic)
        arg_tys.insert(arg_tys.begin(), T_int32);
    FunctionType *func_ty = FunctionType::get(is_generic ? T_int32 : T_void, arg_tys, false);
    Function *eh_func = Function::Create(func_ty, GlobalValue::InternalLinkage,
                                         F->getName() + ".eh", &M);
    eh_func->setDoesNotThrow();
    eh_func->setHasUWTable();
#if JL_LLVM_VERSION >= 50000
    AttrBuilder AB(F->getAttributes().getFnAttributes());
    for (const auto &attr : AB.td_attrs())
        eh_func->addFnAttr(attr.first, attr.second);
#endif

    // Make sure the debug info is valid after outlining.
    // We need to make sure that the same DISubprogram is not attached to multiple functions
    // but the DICompileUnit should still be shared
    // (copying the compile unit and add all of them to the module should also be OK).
    // We also need to update the debug info on all instructions to point to the new subprogram.
    // This might give strange printing in the backtrace since the inlining may be printed
    // multiple times.
    ValueToValueMapTy VMap;
    auto &MD = VMap.MD();
    for (auto cu: F->getParent()->debug_compile_units())
        MD[cu].reset(cu);
    if (auto subprogram = F->getSubprogram())
        eh_func->setSubprogram(cast<DISubprogram>(MapMetadata(subprogram, VMap)));

    auto &old_bbs = F->getBasicBlockList();
    auto &new_bbs = eh_func->getBasicBlockList();

    auto move_bb = [&] (BasicBlock *bb) {
        old_bbs.remove(bb);
        new_bbs.push_back(bb);
        for (auto &I: *bb) {
            if (auto dbgloc = I.getDebugLoc()) {
                I.setDebugLoc(cast<DILocation>(MapMetadata(dbgloc.get(), VMap)));
            }
            if (auto dbginst = dyn_cast<DbgInfoIntrinsic>(&I)) {
                uint32_t nargs = dbginst->getNumOperands();
                for (uint32_t i = 0; i < nargs; i++) {
                    dbginst->setOperand(i, MapValue(dbginst->getOperand(i), VMap));
                }
            }
        }
    };

    move_bb(new_enter_bb);
    for (auto bb: bbs)
        move_bb(bb);

    builder.SetInsertPoint(&*new_enter_bb->begin());
    auto v_ptls = builder.CreateCall(ptls_getter);

    auto arg_it = eh_func->arg_begin();
    if (is_generic) {
        enter->replaceAllUsesWith(&*arg_it);
        ++arg_it;
    }
    else {
        enter->replaceAllUsesWith(V0_int32);
    }
    enter->eraseFromParent();
    for (auto input: inputs) {
        Value *replace = nullptr;
        if (auto call = dyn_cast<CallInst>(input)) {
            if (call->getCalledValue() == ptls_getter) {
                replace = v_ptls;
            }
        }
        if (!replace) {
            replace = &*arg_it;
            ++arg_it;
        }
        for (auto &use: input_infos[input]) {
            use.first->setOperand(use.second, replace);
        }
    }
    for (auto val: outputs) {
        auto slot = &*arg_it;
        ++arg_it;
        auto it = ++BasicBlock::iterator(val);
        while (isa<PHINode>(*it))
            ++it;
        builder.SetInsertPoint(&*it);
        builder.CreateStore(val, slot);
    }
    for (uint32_t id = 0; id < nleaves; id++) {
        auto leave = leaves[id];
        auto bb = leave->getParent();
        auto old_term = bb->getTerminator();
        builder.SetInsertPoint(old_term);
        if (is_generic) {
            builder.CreateRet(ConstantInt::get(T_int32, id));
        }
        else {
            builder.CreateRetVoid();
        }
        old_term->eraseFromParent();
        leave->eraseFromParent();
    }

    auto enter_br = cast<BranchInst>(old_enter_bb->getTerminator());
    enter_br->setSuccessor(0, dispatchBB);
    builder.SetInsertPoint(enter_br);
    call_args.insert(call_args.begin(), eh_func);
    auto eh_call = builder.CreateCall(catcher, call_args);
    if (nleaves == 1) {
        // This is not just an optimization but is also needed for correctness since
        // pattern matching above can match single reachable leave in error branch
        // in which case the return value of the function can only be 1 but the
        // dispatch expects 0.
        ret_id->replaceAllUsesWith(V0_int32);
    }
    else {
        ret_id->replaceAllUsesWith(eh_call);
    }
    ret_id->eraseFromParent();
}

void EHContext::stripVolatile(Function *F)
{
    for (auto &inst: instructions(F)) {
        if (auto load = dyn_cast<LoadInst>(&inst)) {
            if (load->isVolatile() && isFromAlloca(load->getPointerOperand())) {
                load->setVolatile(false);
            }
        }
        else if (auto store = dyn_cast<StoreInst>(&inst)) {
            if (store->isVolatile() && isFromAlloca(store->getPointerOperand())) {
                store->setVolatile(false);
            }
        }
        else if (auto cmpxchg = dyn_cast<AtomicCmpXchgInst>(&inst)) {
            if (cmpxchg->isVolatile() && isFromAlloca(cmpxchg->getPointerOperand())) {
                cmpxchg->setVolatile(false);
            }
        }
        else if (auto rmw = dyn_cast<AtomicRMWInst>(&inst)) {
            if (rmw->isVolatile() && isFromAlloca(rmw->getPointerOperand())) {
                rmw->setVolatile(false);
            }
        }
        else if (auto intrinsic = dyn_cast<IntrinsicInst>(&inst)) {
            auto id = intrinsic->getIntrinsicID();
            if (id == Intrinsic::memcpy || id == Intrinsic::memmove) {
                if (intrinsic->getArgOperand(4) != V_false &&
                    (isFromAlloca(intrinsic->getArgOperand(0)) ||
                     isFromAlloca(intrinsic->getArgOperand(1)))) {
                    intrinsic->setArgOperand(4, V_false);
                }
            }
            else if (id == Intrinsic::memset) {
                if (intrinsic->getArgOperand(4) != V_false &&
                    isFromAlloca(intrinsic->getArgOperand(0))) {
                    intrinsic->setArgOperand(4, V_false);
                }
            }
        }
    }
}

bool EHOutlining::runOnModule(Module &M)
{
    auto except_enter = M.getFunction("julia.except_enter");
    if (!except_enter)
        return false;
    EHContext eh(M, except_enter);
    for (auto &F: M) {
        if (!F.empty()) {
            eh.stripVolatile(&F);
        }
    }
    eh.outlineAll();
    return true;
}

char EHOutlining::ID = 0;

static RegisterPass<EHOutlining> X("EHOutlining", "EHOutlining Pass",
                                   false /* Only looks at CFG */,
                                   false /* Analysis Pass */);

} // anonymous namespace

Pass *createEHOutliningPass()
{
    return new EHOutlining();
}

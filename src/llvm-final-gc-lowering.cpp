// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-version.h"

#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/Pass.h>
#include <llvm/Support/Debug.h>
#include <llvm/Transforms/Utils/ModuleUtils.h>

#include "codegen_shared.h"
#include "julia.h"
#include "julia_internal.h"
#include "llvm-pass-helpers.h"

#define DEBUG_TYPE "final_gc_lowering"

using namespace llvm;

// The final GC lowering pass. This pass lowers platform-agnostic GC
// intrinsics to platform-dependent instruction sequences. The
// intrinsics it targets are those produced by the late GC frame
// lowering pass.
//
// This pass targets typical back-ends for which the standard Julia
// runtime library is available. Atypical back-ends should supply
// their own lowering pass.
struct FinalLowerGC: public FunctionPass, private JuliaPassContext {
    static char ID;
    FinalLowerGC() : FunctionPass(ID)
    { }

private:
    Function *queueRootFunc;
    Function *poolAllocFunc;
    Function *bigAllocFunc;
    Instruction *pgcstack;

    bool doInitialization(Module &M) override;
    bool doFinalization(Module &M) override;
    bool runOnFunction(Function &F) override;

    // Lowers a `julia.new_gc_frame` intrinsic.
    Value *lowerNewGCFrame(CallInst *target, Function &F);

    // Lowers a `julia.push_gc_frame` intrinsic.
    void lowerPushGCFrame(CallInst *target, Function &F);

    // Lowers a `julia.pop_gc_frame` intrinsic.
    void lowerPopGCFrame(CallInst *target, Function &F);

    // Lowers a `julia.get_gc_frame_slot` intrinsic.
    Value *lowerGetGCFrameSlot(CallInst *target, Function &F);

    // Lowers a `julia.gc_alloc_bytes` intrinsic.
    Value *lowerGCAllocBytes(CallInst *target, Function &F);

    // Lowers a `julia.queue_gc_root` intrinsic.
    Value *lowerQueueGCRoot(CallInst *target, Function &F);
};

Value *FinalLowerGC::lowerNewGCFrame(CallInst *target, Function &F)
{
    assert(target->arg_size() == 1);
    unsigned nRoots = cast<ConstantInt>(target->getArgOperand(0))->getLimitedValue(INT_MAX);

    // Create the GC frame.
    AllocaInst *gcframe = new AllocaInst(
        T_prjlvalue,
        0,
        ConstantInt::get(T_int32, nRoots + 2),
        Align(16));
    gcframe->insertAfter(target);
    gcframe->takeName(target);

    // Zero out the GC frame.
    BitCastInst *tempSlot_i8 = new BitCastInst(gcframe, Type::getInt8PtrTy(F.getContext()), "");
    tempSlot_i8->insertAfter(gcframe);
    Type *argsT[2] = {tempSlot_i8->getType(), T_int32};
    Function *memset = Intrinsic::getDeclaration(F.getParent(), Intrinsic::memset, makeArrayRef(argsT));
    Value *args[4] = {
        tempSlot_i8, // dest
        ConstantInt::get(Type::getInt8Ty(F.getContext()), 0), // val
        ConstantInt::get(T_int32, sizeof(jl_value_t*) * (nRoots + 2)), // len
        ConstantInt::get(Type::getInt1Ty(F.getContext()), 0)}; // volatile
    CallInst *zeroing = CallInst::Create(memset, makeArrayRef(args));
    cast<MemSetInst>(zeroing)->setDestAlignment(16);
    zeroing->setMetadata(LLVMContext::MD_tbaa, tbaa_gcframe);
    zeroing->insertAfter(tempSlot_i8);

    return gcframe;
}

void FinalLowerGC::lowerPushGCFrame(CallInst *target, Function &F)
{
    assert(target->arg_size() == 2);
    auto gcframe = target->getArgOperand(0);
    unsigned nRoots = cast<ConstantInt>(target->getArgOperand(1))->getLimitedValue(INT_MAX);

    IRBuilder<> builder(target->getContext());
    builder.SetInsertPoint(&*(++BasicBlock::iterator(target)));
    StoreInst *inst = builder.CreateAlignedStore(
                ConstantInt::get(T_size, JL_GC_ENCODE_PUSHARGS(nRoots)),
                builder.CreateBitCast(
                        builder.CreateConstInBoundsGEP1_32(T_prjlvalue, gcframe, 0),
                        T_size->getPointerTo()),
                Align(sizeof(void*)));
    inst->setMetadata(LLVMContext::MD_tbaa, tbaa_gcframe);
    inst = builder.CreateAlignedStore(
            builder.CreateAlignedLoad(T_ppjlvalue, pgcstack, Align(sizeof(void*))),
            builder.CreatePointerCast(
                    builder.CreateConstInBoundsGEP1_32(T_prjlvalue, gcframe, 1),
                    PointerType::get(T_ppjlvalue, 0)),
            Align(sizeof(void*)));
    inst->setMetadata(LLVMContext::MD_tbaa, tbaa_gcframe);
    inst = builder.CreateAlignedStore(
            gcframe,
            builder.CreateBitCast(pgcstack, PointerType::get(PointerType::get(T_prjlvalue, 0), 0)),
            Align(sizeof(void*)));
}

void FinalLowerGC::lowerPopGCFrame(CallInst *target, Function &F)
{
    assert(target->arg_size() == 1);
    auto gcframe = target->getArgOperand(0);

    IRBuilder<> builder(target->getContext());
    builder.SetInsertPoint(target);
    Instruction *gcpop =
        cast<Instruction>(builder.CreateConstInBoundsGEP1_32(T_prjlvalue, gcframe, 1));
    Instruction *inst = builder.CreateAlignedLoad(T_prjlvalue, gcpop, Align(sizeof(void*)));
    inst->setMetadata(LLVMContext::MD_tbaa, tbaa_gcframe);
    inst = builder.CreateAlignedStore(
        inst,
        builder.CreateBitCast(pgcstack,
            PointerType::get(T_prjlvalue, 0)),
        Align(sizeof(void*)));
    inst->setMetadata(LLVMContext::MD_tbaa, tbaa_gcframe);
}

Value *FinalLowerGC::lowerGetGCFrameSlot(CallInst *target, Function &F)
{
    assert(target->arg_size() == 2);
    auto gcframe = target->getArgOperand(0);
    auto index = target->getArgOperand(1);

    // Initialize an IR builder.
    IRBuilder<> builder(target->getContext());
    builder.SetInsertPoint(target);

    // The first two slots are reserved, so we'll add two to the index.
    index = builder.CreateAdd(index, ConstantInt::get(T_int32, 2));

    // Lower the intrinsic as a GEP.
    auto gep = builder.CreateInBoundsGEP(T_prjlvalue, gcframe, index);
    gep->takeName(target);
    return gep;
}

Value *FinalLowerGC::lowerQueueGCRoot(CallInst *target, Function &F)
{
    assert(target->arg_size() == 1);
    target->setCalledFunction(queueRootFunc);
    return target;
}

Value *FinalLowerGC::lowerGCAllocBytes(CallInst *target, Function &F)
{
    assert(target->arg_size() == 2);
    auto sz = (size_t)cast<ConstantInt>(target->getArgOperand(1))->getZExtValue();
    // This is strongly architecture and OS dependent
    int osize;
    int offset = jl_gc_classify_pools(sz, &osize);
    IRBuilder<> builder(target);
    builder.SetCurrentDebugLocation(target->getDebugLoc());
    auto ptls = target->getArgOperand(0);
    CallInst *newI;
    if (offset < 0) {
        newI = builder.CreateCall(
            bigAllocFunc,
            { ptls, ConstantInt::get(T_size, sz + sizeof(void*)) });
    }
    else {
#ifndef MMTKHEAP
        auto pool_offs = ConstantInt::get(Type::getInt32Ty(F.getContext()), offset);
        auto pool_osize = ConstantInt::get(Type::getInt32Ty(F.getContext()), osize);
        newI = builder.CreateCall(poolAllocFunc, { ptls, pool_offs, pool_osize });
#else
        auto pool_osize = ConstantInt::get(Type::getInt64Ty(F.getContext()), osize);
        auto cursor_pos = ConstantInt::get(Type::getInt64Ty(target->getContext()), offsetof(jl_tls_states_t, cursor));
        auto limit_pos = ConstantInt::get(Type::getInt64Ty(target->getContext()),  offsetof(jl_tls_states_t, limit));

        auto cursor_tls_i8 = builder.CreateGEP(Type::getInt8Ty(target->getContext()), ptls, cursor_pos);
        auto cursor_ptr = builder.CreateBitCast(cursor_tls_i8, PointerType::get(Type::getInt64Ty(target->getContext()), 0), "cursor_ptr");
        auto cursor = builder.CreateLoad(Type::getInt64Ty(target->getContext()), cursor_ptr, "cursor");


        auto delta_offset = builder.CreateNSWSub(ConstantInt::get(Type::getInt64Ty(target->getContext()), 0), ConstantInt::get(Type::getInt64Ty(target->getContext()), 8));
        auto delta_cursor = builder.CreateNSWSub(ConstantInt::get(Type::getInt64Ty(target->getContext()), 0), cursor);
        auto delta_op = builder.CreateNSWAdd(delta_offset, delta_cursor);
        auto delta = builder.CreateAnd(delta_op, ConstantInt::get(Type::getInt64Ty(target->getContext()), 15), "delta");
        auto result = builder.CreateNSWAdd(cursor, delta, "result");

        auto new_cursor = builder.CreateNSWAdd(result, pool_osize);

        auto limit_tls_i8 = builder.CreateGEP(Type::getInt8Ty(target->getContext()), ptls, limit_pos);
        auto limit_ptr = builder.CreateBitCast(limit_tls_i8, PointerType::get(Type::getInt64Ty(target->getContext()), 0), "limit_ptr");
        auto limit = builder.CreateLoad(Type::getInt64Ty(target->getContext()), limit_ptr, "limit");

        auto gt_limit = builder.CreateICmpSGT(new_cursor, limit);

        auto current_block = target->getParent();
        builder.SetInsertPoint(target->getNextNode());
        auto phiNode = builder.CreatePHI(poolAllocFunc->getReturnType(), 2, "phi_fast_slow");
        auto top_cont = current_block->splitBasicBlock(target->getNextNode(), "top_cont");

        auto slowpath = BasicBlock::Create(target->getContext(), "slowpath", target->getFunction());
        auto fastpath = BasicBlock::Create(target->getContext(), "fastpath", target->getFunction(), top_cont);

        auto next_br = current_block->getTerminator();
        next_br->eraseFromParent();
        builder.SetInsertPoint(current_block);
        builder.CreateCondBr(gt_limit, slowpath, fastpath);

        // slowpath
        builder.SetInsertPoint(slowpath);
        auto pool_offs = ConstantInt::get(Type::getInt32Ty(F.getContext()), 1);
        auto new_call = builder.CreateCall(poolAllocFunc, { pool_offs, pool_osize });
        new_call->setAttributes(new_call->getCalledFunction()->getAttributes());
        builder.CreateBr(top_cont);

        // // fastpath
        builder.SetInsertPoint(fastpath);
        builder.CreateStore(new_cursor, cursor_ptr);

        // ptls->gc_num.allocd += osize;
        auto pool_alloc_pos = ConstantInt::get(Type::getInt64Ty(target->getContext()), offsetof(jl_tls_states_t, gc_num));
        auto pool_alloc_i8 = builder.CreateGEP(Type::getInt8Ty(target->getContext()), ptls, pool_alloc_pos);
        auto pool_alloc_tls = builder.CreateBitCast(pool_alloc_i8, PointerType::get(Type::getInt64Ty(target->getContext()), 0), "pool_alloc");
        auto pool_allocd = builder.CreateLoad(Type::getInt64Ty(target->getContext()), pool_alloc_tls);
        auto pool_allocd_total = builder.CreateAdd(pool_allocd, pool_osize);
        builder.CreateStore(pool_allocd_total, pool_alloc_tls);

        auto v_raw = builder.CreateNSWAdd(result, ConstantInt::get(Type::getInt64Ty(target->getContext()), sizeof(jl_taggedvalue_t)));
        auto v_as_ptr = builder.CreateIntToPtr(v_raw, poolAllocFunc->getReturnType());
        builder.CreateBr(top_cont);

        phiNode->addIncoming(new_call, slowpath);
        phiNode->addIncoming(v_as_ptr, fastpath);
        phiNode->takeName(target);

        return phiNode;
#endif
    }
    newI->setAttributes(newI->getCalledFunction()->getAttributes());
    newI->takeName(target);
    return newI;
}

bool FinalLowerGC::doInitialization(Module &M) {
    // Initialize platform-agnostic references.
    initAll(M);

    // Initialize platform-specific references.
    queueRootFunc = getOrDeclare(jl_well_known::GCQueueRoot);
    poolAllocFunc = getOrDeclare(jl_well_known::GCPoolAlloc);
    bigAllocFunc = getOrDeclare(jl_well_known::GCBigAlloc);

    GlobalValue *functionList[] = {queueRootFunc, poolAllocFunc, bigAllocFunc};
    unsigned j = 0;
    for (unsigned i = 0; i < sizeof(functionList) / sizeof(void*); i++) {
        if (!functionList[i])
            continue;
        if (i != j)
            functionList[j] = functionList[i];
        j++;
    }
    if (j != 0)
        appendToCompilerUsed(M, ArrayRef<GlobalValue*>(functionList, j));
    return true;
}

bool FinalLowerGC::doFinalization(Module &M)
{
    GlobalValue *functionList[] = {queueRootFunc, poolAllocFunc, bigAllocFunc};
    queueRootFunc = poolAllocFunc = bigAllocFunc = nullptr;
    auto used = M.getGlobalVariable("llvm.compiler.used");
    if (!used)
        return false;
    SmallPtrSet<Constant*, 16> InitAsSet(
        functionList,
        functionList + sizeof(functionList) / sizeof(void*));
    bool changed = false;
    SmallVector<Constant*, 16> init;
    ConstantArray *CA = cast<ConstantArray>(used->getInitializer());
    for (auto &Op : CA->operands()) {
        Constant *C = cast_or_null<Constant>(Op);
        if (InitAsSet.count(C->stripPointerCasts())) {
            changed = true;
            continue;
        }
        init.push_back(C);
    }
    if (!changed)
        return false;
    used->eraseFromParent();
    if (init.empty())
        return true;
    ArrayType *ATy = ArrayType::get(T_pint8, init.size());
    used = new GlobalVariable(M, ATy, false, GlobalValue::AppendingLinkage,
                                    ConstantArray::get(ATy, init), "llvm.compiler.used");
    used->setSection("llvm.metadata");
    return true;
}

template<typename TIterator>
static void replaceInstruction(
    Instruction *oldInstruction,
    Value *newInstruction,
    TIterator &it)
{
    if (newInstruction != oldInstruction) {
        oldInstruction->replaceAllUsesWith(newInstruction);
        it = oldInstruction->eraseFromParent();
    }
    else {
        ++it;
    }
}

bool FinalLowerGC::runOnFunction(Function &F)
{
    LLVM_DEBUG(dbgs() << "FINAL GC LOWERING: Processing function " << F.getName() << "\n");
    // Check availability of functions again since they might have been deleted.
    initFunctions(*F.getParent());
    if (!pgcstack_getter)
        return false;

    // Look for a call to 'julia.get_pgcstack'.
    pgcstack = getPGCstack(F);
    if (!pgcstack)
        return false;

    // Acquire intrinsic functions.
    auto newGCFrameFunc = getOrNull(jl_intrinsics::newGCFrame);
    auto pushGCFrameFunc = getOrNull(jl_intrinsics::pushGCFrame);
    auto popGCFrameFunc = getOrNull(jl_intrinsics::popGCFrame);
    auto getGCFrameSlotFunc = getOrNull(jl_intrinsics::getGCFrameSlot);
    auto GCAllocBytesFunc = getOrNull(jl_intrinsics::GCAllocBytes);
    auto queueGCRootFunc = getOrNull(jl_intrinsics::queueGCRoot);

    // Lower all calls to supported intrinsics.
    for (BasicBlock &BB : F) {
        for (auto it = BB.begin(); it != BB.end();) {
            auto *CI = dyn_cast<CallInst>(&*it);
            if (!CI) {
                ++it;
                continue;
            }

            Value *callee = CI->getCalledOperand();

            if (callee == newGCFrameFunc) {
                replaceInstruction(CI, lowerNewGCFrame(CI, F), it);
            }
            else if (callee == pushGCFrameFunc) {
                lowerPushGCFrame(CI, F);
                it = CI->eraseFromParent();
            }
            else if (callee == popGCFrameFunc) {
                lowerPopGCFrame(CI, F);
                it = CI->eraseFromParent();
            }
            else if (callee == getGCFrameSlotFunc) {
                replaceInstruction(CI, lowerGetGCFrameSlot(CI, F), it);
            }
            else if (callee == GCAllocBytesFunc) {
                replaceInstruction(CI, lowerGCAllocBytes(CI, F), it);
            }
            else if (callee == queueGCRootFunc) {
                replaceInstruction(CI, lowerQueueGCRoot(CI, F), it);
            }
            else {
                ++it;
            }
        }
    }

    return true;
}

char FinalLowerGC::ID = 0;
static RegisterPass<FinalLowerGC> X("FinalLowerGC", "Final GC intrinsic lowering pass", false, false);

Pass *createFinalLowerGCPass()
{
    return new FinalLowerGC();
}

extern "C" JL_DLLEXPORT void LLVMExtraAddFinalLowerGCPass_impl(LLVMPassManagerRef PM)
{
    unwrap(PM)->add(createFinalLowerGCPass());
}

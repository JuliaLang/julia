// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-version.h"
#include "passes.h"

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

struct FinalLowerGC: private JuliaPassContext {
    bool runOnFunction(Function &F);
    bool doInitialization(Module &M);
    bool doFinalization(Module &M);

private:
    Function *queueRootFunc;
    Function *queueBindingFunc;
    Function *poolAllocFunc;
    Function *bigAllocFunc;
    Instruction *pgcstack;

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

    // Lowers a `julia.queue_gc_binding` intrinsic.
    Value *lowerQueueGCBinding(CallInst *target, Function &F);
};

Value *FinalLowerGC::lowerNewGCFrame(CallInst *target, Function &F)
{
    assert(target->arg_size() == 1);
    unsigned nRoots = cast<ConstantInt>(target->getArgOperand(0))->getLimitedValue(INT_MAX);

    // Create the GC frame.
    AllocaInst *gcframe = new AllocaInst(
        T_prjlvalue,
        0,
        ConstantInt::get(Type::getInt32Ty(F.getContext()), nRoots + 2),
        Align(16));
    gcframe->insertAfter(target);
    gcframe->takeName(target);

    // Zero out the GC frame.
    BitCastInst *tempSlot_i8 = new BitCastInst(gcframe, Type::getInt8PtrTy(F.getContext()), "");
    tempSlot_i8->insertAfter(gcframe);
    Type *argsT[2] = {tempSlot_i8->getType(), Type::getInt32Ty(F.getContext())};
    Function *memset = Intrinsic::getDeclaration(F.getParent(), Intrinsic::memset, makeArrayRef(argsT));
    Value *args[4] = {
        tempSlot_i8, // dest
        ConstantInt::get(Type::getInt8Ty(F.getContext()), 0), // val
        ConstantInt::get(Type::getInt32Ty(F.getContext()), sizeof(jl_value_t*) * (nRoots + 2)), // len
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
                ConstantInt::get(getSizeTy(F.getContext()), JL_GC_ENCODE_PUSHARGS(nRoots)),
                builder.CreateBitCast(
                        builder.CreateConstInBoundsGEP1_32(T_prjlvalue, gcframe, 0),
                        getSizeTy(F.getContext())->getPointerTo()),
                Align(sizeof(void*)));
    inst->setMetadata(LLVMContext::MD_tbaa, tbaa_gcframe);
    auto T_ppjlvalue = JuliaType::get_ppjlvalue_ty(F.getContext());
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
    index = builder.CreateAdd(index, ConstantInt::get(Type::getInt32Ty(F.getContext()), 2));

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

Value *FinalLowerGC::lowerQueueGCBinding(CallInst *target, Function &F)
{
    assert(target->arg_size() == 1);
    target->setCalledFunction(queueBindingFunc);
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
            { ptls, ConstantInt::get(getSizeTy(F.getContext()), sz + sizeof(void*)) });
    }
    else {
        auto pool_offs = ConstantInt::get(Type::getInt32Ty(F.getContext()), offset);
        auto pool_osize = ConstantInt::get(Type::getInt32Ty(F.getContext()), osize);
        newI = builder.CreateCall(poolAllocFunc, { ptls, pool_offs, pool_osize });
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
    queueBindingFunc = getOrDeclare(jl_well_known::GCQueueBinding);
    poolAllocFunc = getOrDeclare(jl_well_known::GCPoolAlloc);
    bigAllocFunc = getOrDeclare(jl_well_known::GCBigAlloc);

    GlobalValue *functionList[] = {queueRootFunc, queueBindingFunc, poolAllocFunc, bigAllocFunc};
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
    GlobalValue *functionList[] = {queueRootFunc, queueBindingFunc, poolAllocFunc, bigAllocFunc};
    queueRootFunc = queueBindingFunc = poolAllocFunc = bigAllocFunc = nullptr;
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
    ArrayType *ATy = ArrayType::get(Type::getInt8PtrTy(M.getContext()), init.size());
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
    auto queueGCBindingFunc = getOrNull(jl_intrinsics::queueGCBinding);

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
            else if (callee == queueGCBindingFunc) {
                replaceInstruction(CI, lowerQueueGCBinding(CI, F), it);
            }
            else {
                ++it;
            }
        }
    }

    return true;
}

struct FinalLowerGCLegacy: public FunctionPass {
    static char ID;
    FinalLowerGCLegacy() : FunctionPass(ID), finalLowerGC(FinalLowerGC()) {}

protected:
    void getAnalysisUsage(AnalysisUsage &AU) const override {
        FunctionPass::getAnalysisUsage(AU);
    }

private:
    bool runOnFunction(Function &F) override;
    bool doInitialization(Module &M) override;
    bool doFinalization(Module &M) override;

    FinalLowerGC finalLowerGC;
};

bool FinalLowerGCLegacy::runOnFunction(Function &F) {
    return finalLowerGC.runOnFunction(F);
}

bool FinalLowerGCLegacy::doInitialization(Module &M) {
    return finalLowerGC.doInitialization(M);
}

bool FinalLowerGCLegacy::doFinalization(Module &M) {
    return finalLowerGC.doFinalization(M);
}


PreservedAnalyses FinalLowerGCPass::run(Module &M, ModuleAnalysisManager &AM)
{
    auto finalLowerGC = FinalLowerGC();
    bool modified = false;
    modified |= finalLowerGC.doInitialization(M);
    for (auto &F : M.functions()) {
        if (F.isDeclaration())
            continue;
        modified |= finalLowerGC.runOnFunction(F);
    }
    modified |= finalLowerGC.doFinalization(M);
    if (modified) {
        return PreservedAnalyses::allInSet<CFGAnalyses>();
    }
    return PreservedAnalyses::all();
}

char FinalLowerGCLegacy::ID = 0;
static RegisterPass<FinalLowerGCLegacy> X("FinalLowerGC", "Final GC intrinsic lowering pass", false, false);

Pass *createFinalLowerGCPass()
{
    return new FinalLowerGCLegacy();
}

extern "C" JL_DLLEXPORT void LLVMExtraAddFinalLowerGCPass_impl(LLVMPassManagerRef PM)
{
    unwrap(PM)->add(createFinalLowerGCPass());
}

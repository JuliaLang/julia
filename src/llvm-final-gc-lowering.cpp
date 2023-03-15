// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-version.h"
#include "passes.h"

#include <llvm/ADT/Statistic.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Pass.h>
#include <llvm/Support/Debug.h>
#include <llvm/Transforms/Utils/ModuleUtils.h>

#include "llvm-codegen-shared.h"
#include "julia.h"
#include "julia_internal.h"
#include "llvm-pass-helpers.h"

#define DEBUG_TYPE "final_gc_lowering"
STATISTIC(NewGCFrameCount, "Number of lowered newGCFrameFunc intrinsics");
STATISTIC(PushGCFrameCount, "Number of lowered pushGCFrameFunc intrinsics");
STATISTIC(PopGCFrameCount, "Number of lowered popGCFrameFunc intrinsics");
STATISTIC(GetGCFrameSlotCount, "Number of lowered getGCFrameSlotFunc intrinsics");
STATISTIC(GCAllocBytesCount, "Number of lowered GCAllocBytesFunc intrinsics");
STATISTIC(QueueGCRootCount, "Number of lowered queueGCRootFunc intrinsics");
STATISTIC(SafepointCount, "Number of lowered safepoint intrinsics");

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
    Function *poolAllocFunc;
    Function *bigAllocFunc;
    Function *allocTypedFunc;
    Instruction *pgcstack;
    Type *T_size;

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

    // Lowers a `julia.safepoint` intrinsic.
    Value *lowerSafepoint(CallInst *target, Function &F);
};

Value *FinalLowerGC::lowerNewGCFrame(CallInst *target, Function &F)
{
    ++NewGCFrameCount;
    assert(target->arg_size() == 1);
    unsigned nRoots = cast<ConstantInt>(target->getArgOperand(0))->getLimitedValue(INT_MAX);

    // Create the GC frame.
    unsigned allocaAddressSpace = F.getParent()->getDataLayout().getAllocaAddrSpace();
    AllocaInst *gcframe_alloca = new AllocaInst(
        T_prjlvalue,
        allocaAddressSpace,
        ConstantInt::get(Type::getInt32Ty(F.getContext()), nRoots + 2),
        Align(16));
    gcframe_alloca->insertAfter(target);
    Instruction *gcframe;
    if (allocaAddressSpace) {
        // addrspacecast as needed for non-0 alloca addrspace
        gcframe = new AddrSpaceCastInst(gcframe_alloca, T_prjlvalue->getPointerTo(0));
        gcframe->insertAfter(gcframe_alloca);
    } else {
        gcframe = gcframe_alloca;
    }
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
    cast<MemSetInst>(zeroing)->setDestAlignment(Align(16));
    zeroing->setMetadata(LLVMContext::MD_tbaa, tbaa_gcframe);
    zeroing->insertAfter(tempSlot_i8);

    return gcframe;
}

void FinalLowerGC::lowerPushGCFrame(CallInst *target, Function &F)
{
    ++PushGCFrameCount;
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
    ++PopGCFrameCount;
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
    ++GetGCFrameSlotCount;
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
    ++QueueGCRootCount;
    assert(target->arg_size() == 1);
    target->setCalledFunction(queueRootFunc);
    return target;
}

Value *FinalLowerGC::lowerSafepoint(CallInst *target, Function &F)
{
    ++SafepointCount;
    assert(target->arg_size() == 1);
    IRBuilder<> builder(target->getContext());
    builder.SetInsertPoint(target);
    Value* signal_page = target->getOperand(0);
    Value* load = builder.CreateLoad(T_size, signal_page, true);
    return load;
}

Value *FinalLowerGC::lowerGCAllocBytes(CallInst *target, Function &F)
{
    ++GCAllocBytesCount;
    assert(target->arg_size() == 2);
    CallInst *newI;

    IRBuilder<> builder(target);
    builder.SetCurrentDebugLocation(target->getDebugLoc());
    auto ptls = target->getArgOperand(0);
    Attribute derefAttr;

    if (auto CI = dyn_cast<ConstantInt>(target->getArgOperand(1))) {
        size_t sz = (size_t)CI->getZExtValue();
        // This is strongly architecture and OS dependent
        int osize;
        int offset = jl_gc_classify_pools(sz, &osize);
        if (offset < 0) {
            newI = builder.CreateCall(
                bigAllocFunc,
                { ptls, ConstantInt::get(T_size, sz + sizeof(void*)) });
            derefAttr = Attribute::getWithDereferenceableBytes(F.getContext(), sz + sizeof(void*));
        }
        else {
            auto pool_offs = ConstantInt::get(Type::getInt32Ty(F.getContext()), offset);
            auto pool_osize = ConstantInt::get(Type::getInt32Ty(F.getContext()), osize);
            newI = builder.CreateCall(poolAllocFunc, { ptls, pool_offs, pool_osize });
            derefAttr = Attribute::getWithDereferenceableBytes(F.getContext(), osize);
        }
    } else {
        auto size = builder.CreateZExtOrTrunc(target->getArgOperand(1), T_size);
        size = builder.CreateAdd(size, ConstantInt::get(T_size, sizeof(void*)));
        newI = builder.CreateCall(allocTypedFunc, { ptls, size, ConstantPointerNull::get(Type::getInt8PtrTy(F.getContext())) });
        derefAttr = Attribute::getWithDereferenceableBytes(F.getContext(), sizeof(void*));
    }
    newI->setAttributes(newI->getCalledFunction()->getAttributes());
    newI->addRetAttr(derefAttr);
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
    allocTypedFunc = getOrDeclare(jl_well_known::GCAllocTyped);
    T_size = M.getDataLayout().getIntPtrType(M.getContext());

    GlobalValue *functionList[] = {queueRootFunc, poolAllocFunc, bigAllocFunc, allocTypedFunc};
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
    GlobalValue *functionList[] = {queueRootFunc, poolAllocFunc, bigAllocFunc, allocTypedFunc};
    queueRootFunc = poolAllocFunc = bigAllocFunc = allocTypedFunc = nullptr;
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
    // Check availability of functions again since they might have been deleted.
    initFunctions(*F.getParent());
    if (!pgcstack_getter && !adoptthread_func) {
        LLVM_DEBUG(dbgs() << "FINAL GC LOWERING: Skipping function " << F.getName() << "\n");
        return false;
    }

    // Look for a call to 'julia.get_pgcstack'.
    pgcstack = getPGCstack(F);
    if (!pgcstack) {
        LLVM_DEBUG(dbgs() << "FINAL GC LOWERING: Skipping function " << F.getName() << " no pgcstack\n");
        return false;
    }
    LLVM_DEBUG(dbgs() << "FINAL GC LOWERING: Processing function " << F.getName() << "\n");

    // Acquire intrinsic functions.
    auto newGCFrameFunc = getOrNull(jl_intrinsics::newGCFrame);
    auto pushGCFrameFunc = getOrNull(jl_intrinsics::pushGCFrame);
    auto popGCFrameFunc = getOrNull(jl_intrinsics::popGCFrame);
    auto getGCFrameSlotFunc = getOrNull(jl_intrinsics::getGCFrameSlot);
    auto GCAllocBytesFunc = getOrNull(jl_intrinsics::GCAllocBytes);
    auto queueGCRootFunc = getOrNull(jl_intrinsics::queueGCRoot);
    auto safepointFunc = getOrNull(jl_intrinsics::safepoint);

    // Lower all calls to supported intrinsics.
    for (BasicBlock &BB : F) {
        for (auto it = BB.begin(); it != BB.end();) {
            auto *CI = dyn_cast<CallInst>(&*it);
            if (!CI) {
                ++it;
                continue;
            }

            Value *callee = CI->getCalledOperand();
            assert(callee);

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
            else if (callee == safepointFunc) {
                lowerSafepoint(CI, F);
                it = CI->eraseFromParent();
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
    auto ret = finalLowerGC.doFinalization(M);
#ifdef JL_VERIFY_PASSES
    assert(!verifyModule(M, &errs()));
#endif
    return ret;
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
#ifdef JL_VERIFY_PASSES
    assert(!verifyModule(M, &errs()));
#endif
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

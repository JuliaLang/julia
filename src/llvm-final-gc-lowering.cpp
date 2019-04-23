// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/Pass.h>
#include <llvm/Support/Debug.h>

#include "llvm-version.h"
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
    CallInst *ptlsStates;

    bool doInitialization(Module &M) override;
    bool runOnFunction(Function &F) override;

    // Lowers a `julia.new_gc_frame` intrinsic.
    Value *lowerNewGCFrame(CallInst *target, Function &F);

    // Lowers a `julia.push_gc_frame` intrinsic.
    void lowerPushGCFrame(CallInst *target, Function &F);

    // Lowers a `julia.pop_gc_frame` intrinsic.
    void lowerPopGCFrame(CallInst *target, Function &F);

    // Lowers a `julia.get_gc_frame_slot` intrinsic.
    Value *lowerGetGCFrameSlot(CallInst *target, Function &F);

    Instruction *getPgcstack(Instruction *ptlsStates);
};

Value *FinalLowerGC::lowerNewGCFrame(CallInst *target, Function &F)
{
    assert(target->getNumArgOperands() == 1);
    unsigned nRoots = cast<ConstantInt>(target->getArgOperand(0))->getLimitedValue(INT_MAX);

    // Create the GC frame.
    AllocaInst *gcframe = new AllocaInst(
        T_prjlvalue,
        0,
        ConstantInt::get(T_int32, nRoots + 2));
    gcframe->insertAfter(target);
    gcframe->takeName(target);

    // Zero out the GC frame.
    BitCastInst *tempSlot_i8 = new BitCastInst(gcframe, Type::getInt8PtrTy(F.getContext()), "");
    tempSlot_i8->insertAfter(gcframe);
    Type *argsT[2] = {tempSlot_i8->getType(), T_int32};
    Function *memset = Intrinsic::getDeclaration(F.getParent(), Intrinsic::memset, makeArrayRef(argsT));
#if JL_LLVM_VERSION >= 70000
        Value *args[4] = {
            tempSlot_i8, // dest
            ConstantInt::get(Type::getInt8Ty(F.getContext()), 0), // val
            ConstantInt::get(T_int32, sizeof(jl_value_t*)*(nRoots+2)), // len
            ConstantInt::get(Type::getInt1Ty(F.getContext()), 0)}; // volatile
#else
        Value *args[5] = {
            tempSlot_i8, // dest
            ConstantInt::get(Type::getInt8Ty(F.getContext()), 0), // val
            ConstantInt::get(T_int32, sizeof(jl_value_t*)*(nRoots+2)), // len
            ConstantInt::get(T_int32, 0), // align
            ConstantInt::get(Type::getInt1Ty(F.getContext()), 0)}; // volatile
#endif
    CallInst *zeroing = CallInst::Create(memset, makeArrayRef(args));
    zeroing->setMetadata(LLVMContext::MD_tbaa, tbaa_gcframe);
    zeroing->insertAfter(tempSlot_i8);

    return gcframe;
}

void FinalLowerGC::lowerPushGCFrame(CallInst *target, Function &F)
{
    assert(target->getNumArgOperands() == 2);
    auto gcframe = target->getArgOperand(0);
    unsigned nRoots = cast<ConstantInt>(target->getArgOperand(1))->getLimitedValue(INT_MAX);

    IRBuilder<> builder(target->getContext());
    builder.SetInsertPoint(&*(++BasicBlock::iterator(target)));
    Instruction *inst =
        builder.CreateStore(
            ConstantInt::get(T_size, nRoots << 1),
            builder.CreateBitCast(
                builder.CreateConstGEP1_32(gcframe, 0),
                T_size->getPointerTo()));
    inst->setMetadata(LLVMContext::MD_tbaa, tbaa_gcframe);
    Value *pgcstack = builder.Insert(getPgcstack(ptlsStates));
    inst = builder.CreateStore(
        builder.CreateLoad(pgcstack),
        builder.CreatePointerCast(
            builder.CreateConstGEP1_32(gcframe, 1),
            PointerType::get(T_ppjlvalue, 0)));
    inst->setMetadata(LLVMContext::MD_tbaa, tbaa_gcframe);
    builder.CreateStore(gcframe, builder.CreateBitCast(pgcstack,
        PointerType::get(PointerType::get(T_prjlvalue, 0), 0)));
}

void FinalLowerGC::lowerPopGCFrame(CallInst *target, Function &F)
{
    assert(target->getNumArgOperands() == 1);
    auto gcframe = target->getArgOperand(0);

    IRBuilder<> builder(target->getContext());
    builder.SetInsertPoint(target);
    Instruction *gcpop =
        cast<Instruction>(builder.CreateConstGEP1_32(gcframe, 1));
    Instruction *inst = builder.CreateLoad(gcpop);
    inst->setMetadata(LLVMContext::MD_tbaa, tbaa_gcframe);
    inst = builder.CreateStore(
        inst,
        builder.CreateBitCast(
            builder.Insert(getPgcstack(ptlsStates)),
            PointerType::get(T_prjlvalue, 0)));
    inst->setMetadata(LLVMContext::MD_tbaa, tbaa_gcframe);
}

Value *FinalLowerGC::lowerGetGCFrameSlot(CallInst *target, Function &F)
{
    assert(target->getNumArgOperands() == 3);
    auto gcframe = target->getArgOperand(0);
    auto index = target->getArgOperand(1);

    // Initialize an IR builder.
    IRBuilder<> builder(target->getContext());
    builder.SetInsertPoint(target);

    // The first two slots are reserved, so we'll add two to the index.
    index = builder.CreateAdd(index, ConstantInt::get(T_int32, 2));

    // Lower the intrinsic as a GEP.
    auto gep = builder.CreateGEP(gcframe, index);
    gep->takeName(target);
    return gep;
}

Instruction *FinalLowerGC::getPgcstack(Instruction *ptlsStates)
{
    Constant *offset = ConstantInt::getSigned(T_int32, offsetof(jl_tls_states_t, pgcstack) / sizeof(void*));
    return GetElementPtrInst::Create(
        nullptr,
        ptlsStates,
        ArrayRef<Value*>(offset),
        "jl_pgcstack");
}

bool FinalLowerGC::doInitialization(Module &M)
{
    initAll(M);
    return true;
}

bool FinalLowerGC::runOnFunction(Function &F)
{
    DEBUG(dbgs() << "FINAL GC LOWERING: Processing function " << F.getName() << "\n");
    // Check availability of functions again since they might have been deleted.
    initFunctions(*F.getParent());
    if (!ptls_getter)
        return true;

    // Look for a call to 'julia.ptls_states'.
    ptlsStates = getPtls(F);
    if (!ptlsStates)
        return true;

    // Acquire intrinsic functions.
    auto newGCFrameFunc = getOrNull(jl_intrinsics::newGCFrame);
    auto pushGCFrameFunc = getOrNull(jl_intrinsics::pushGCFrame);
    auto popGCFrameFunc = getOrNull(jl_intrinsics::popGCFrame);
    auto getGCFrameSlotFunc = getOrNull(jl_intrinsics::getGCFrameSlot);

    // Lower all calls to supported intrinsics.
    for (BasicBlock &BB : F) {
        for (auto it = BB.begin(); it != BB.end();) {
            auto *CI = dyn_cast<CallInst>(&*it);
            if (!CI) {
                ++it;
                continue;
            }

            auto callee = CI->getCalledValue();

            if (callee == newGCFrameFunc) {
                CI->replaceAllUsesWith(lowerNewGCFrame(CI, F));
                it = CI->eraseFromParent();
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
                CI->replaceAllUsesWith(lowerGetGCFrameSlot(CI, F));
                it = CI->eraseFromParent();
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

extern "C" JL_DLLEXPORT void LLVMExtraAddFinalLowerGCPass(LLVMPassManagerRef PM)
{
    unwrap(PM)->add(createFinalLowerGCPass());
}

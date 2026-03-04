// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-gc-interface-passes.h"

#define DEBUG_TYPE "final_gc_lowering"
STATISTIC(GCAllocBytesCount, "Number of lowered GCAllocBytesFunc intrinsics");

Value* FinalLowerGC::lowerGCAllocBytes(CallInst *target, Function &F)
{
    ++GCAllocBytesCount;
    CallInst *newI;

    IRBuilder<> builder(target);
    auto ptls = target->getArgOperand(0);
    auto type = target->getArgOperand(2);
    uint64_t derefBytes = 0;
    if (auto CI = dyn_cast<ConstantInt>(target->getArgOperand(1))) {
        size_t sz = (size_t)CI->getZExtValue();
        // This is strongly architecture and OS dependent
        int osize;
        int offset = jl_gc_classify_pools(sz, &osize);
        if (offset < 0) {
            newI = builder.CreateCall(
                bigAllocFunc,
                { ptls, ConstantInt::get(T_size, sz + sizeof(void*)), type });
            if (sz > 0)
                derefBytes = sz;
        }
        else {
            auto pool_offs = ConstantInt::get(Type::getInt32Ty(F.getContext()), offset);
            auto pool_osize = ConstantInt::get(Type::getInt32Ty(F.getContext()), osize);
            newI = builder.CreateCall(smallAllocFunc, { ptls, pool_offs, pool_osize, type });
            if (sz > 0)
                derefBytes = sz;
        }
    } else {
        auto size = builder.CreateZExtOrTrunc(target->getArgOperand(1), T_size);
        // allocTypedFunc does not include the type tag in the allocation size!
        newI = builder.CreateCall(allocTypedFunc, { ptls, size, type });
        derefBytes = sizeof(void*);
    }
    newI->setAttributes(newI->getCalledFunction()->getAttributes());
    unsigned align = std::max((unsigned)target->getRetAlign().valueOrOne().value(), (unsigned)sizeof(void*));
    newI->addRetAttr(Attribute::getWithAlignment(F.getContext(), Align(align)));
    if (derefBytes > 0)
        newI->addDereferenceableRetAttr(derefBytes);
    newI->takeName(target);
    return newI;
}

void FinalLowerGC::lowerWriteBarrier(CallInst *target, Function &F) {
    auto parent = target->getArgOperand(0);
    IRBuilder<> builder(target);
    builder.SetCurrentDebugLocation(target->getDebugLoc());
    auto parBits = builder.CreateAnd(EmitLoadTag(builder, T_size, parent, tbaa_tag), GC_OLD_MARKED, "parent_bits");
    auto parOldMarked = builder.CreateICmpEQ(parBits, ConstantInt::get(T_size, GC_OLD_MARKED), "parent_old_marked");
    auto mayTrigTerm = SplitBlockAndInsertIfThen(parOldMarked, target, false);
    builder.SetInsertPoint(mayTrigTerm);
    mayTrigTerm->getParent()->setName("may_trigger_wb");
    Value *anyChldNotMarked = NULL;
    for (unsigned i = 1; i < target->arg_size(); i++) {
        Value *child = target->getArgOperand(i);
        Value *chldBit = builder.CreateAnd(EmitLoadTag(builder, T_size, child, tbaa_tag), GC_MARKED, "child_bit");
        Value *chldNotMarked = builder.CreateICmpEQ(chldBit, ConstantInt::get(T_size, 0), "child_not_marked");
        anyChldNotMarked = anyChldNotMarked ? builder.CreateOr(anyChldNotMarked, chldNotMarked) : chldNotMarked;
    }
    assert(anyChldNotMarked); // handled by all_of test above
    MDBuilder MDB(parent->getContext());
    SmallVector<uint32_t, 2> Weights{1, 9};
    auto trigTerm = SplitBlockAndInsertIfThen(anyChldNotMarked, mayTrigTerm, false,
                                                MDB.createBranchWeights(Weights));
    trigTerm->getParent()->setName("trigger_wb");
    builder.SetInsertPoint(trigTerm);
    if (target->getCalledOperand() == write_barrier_func) {
        builder.CreateCall(getOrDeclare(jl_intrinsics::queueGCRoot), parent);
    }
    else {
        assert(false);
    }
}

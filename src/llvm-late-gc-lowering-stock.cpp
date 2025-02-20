// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-gc-interface-passes.h"

Value* LateLowerGCFrame::lowerGCAllocBytesLate(CallInst *target, Function &F)
{
    // Do nothing for the stock GC
    return target;
}

void LateLowerGCFrame::CleanupWriteBarriers(Function &F, State *S, const SmallVector<CallInst*, 0> &WriteBarriers, bool *CFGModified) {
    auto T_size = F.getParent()->getDataLayout().getIntPtrType(F.getContext());
    for (auto CI : WriteBarriers) {
        auto parent = CI->getArgOperand(0);
        if (std::all_of(CI->op_begin() + 1, CI->op_end(),
                    [parent, &S](Value *child) { return parent == child || IsPermRooted(child, S); })) {
            CI->eraseFromParent();
            continue;
        }
        if (CFGModified) {
            *CFGModified = true;
        }

        IRBuilder<> builder(CI);
        builder.SetCurrentDebugLocation(CI->getDebugLoc());
        auto parBits = builder.CreateAnd(EmitLoadTag(builder, T_size, parent), GC_OLD_MARKED, "parent_bits");
        auto parOldMarked = builder.CreateICmpEQ(parBits, ConstantInt::get(T_size, GC_OLD_MARKED), "parent_old_marked");
        auto mayTrigTerm = SplitBlockAndInsertIfThen(parOldMarked, CI, false);
        builder.SetInsertPoint(mayTrigTerm);
        mayTrigTerm->getParent()->setName("may_trigger_wb");
        Value *anyChldNotMarked = NULL;
        for (unsigned i = 1; i < CI->arg_size(); i++) {
            Value *child = CI->getArgOperand(i);
            Value *chldBit = builder.CreateAnd(EmitLoadTag(builder, T_size, child), GC_MARKED, "child_bit");
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
        if (CI->getCalledOperand() == write_barrier_func) {
            builder.CreateCall(getOrDeclare(jl_intrinsics::queueGCRoot), parent);
        }
        else {
            assert(false);
        }
        CI->eraseFromParent();
    }
}

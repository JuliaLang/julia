// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-gc-interface-passes.h"

#define DEBUG_TYPE "mmtk_final_gc_lowering"
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
            // In this case instead of lowering julia.gc_alloc_bytes to jl_gc_small_alloc
            // We do a slowpath/fastpath check and lower it only on the slowpath, returning
            // the cursor and updating it in the fastpath.
            auto pool_osize_i32 = ConstantInt::get(Type::getInt32Ty(F.getContext()), osize);
            auto pool_osize = ConstantInt::get(Type::getInt64Ty(F.getContext()), osize);

            // Should we generate fastpath allocation sequence here? We should always generate fastpath here for MMTk.
            // Setting this to false will increase allocation overhead a lot, and should only be used for debugging.
            const bool INLINE_FASTPATH_ALLOCATION = true;

            if (INLINE_FASTPATH_ALLOCATION) {
                // Assuming we use the first immix allocator.
                // FIXME: We should get the allocator index and type from MMTk.
                auto allocator_offset = offsetof(jl_tls_states_t, gc_tls) + offsetof(jl_gc_tls_states_t, mmtk_mutator) + offsetof(MMTkMutatorContext, allocators) + offsetof(Allocators, immix);

                auto cursor_pos = ConstantInt::get(Type::getInt64Ty(target->getContext()), allocator_offset + offsetof(ImmixAllocator, cursor));
                auto limit_pos = ConstantInt::get(Type::getInt64Ty(target->getContext()),  allocator_offset + offsetof(ImmixAllocator, limit));

                auto cursor_ptr = builder.CreateInBoundsGEP(Type::getInt8Ty(target->getContext()), ptls, cursor_pos);
                auto cursor = builder.CreateAlignedLoad(Type::getInt64Ty(target->getContext()), cursor_ptr, Align(sizeof(void *)), "cursor");

                // offset = 8
                auto delta_offset = builder.CreateNSWSub(ConstantInt::get(Type::getInt64Ty(target->getContext()), 0), ConstantInt::get(Type::getInt64Ty(target->getContext()), 8));
                auto delta_cursor = builder.CreateNSWSub(ConstantInt::get(Type::getInt64Ty(target->getContext()), 0), cursor);
                auto delta_op = builder.CreateNSWAdd(delta_offset, delta_cursor);
                // alignment 16 (15 = 16 - 1)
                auto delta = builder.CreateAnd(delta_op, ConstantInt::get(Type::getInt64Ty(target->getContext()), 15), "delta");
                auto result = builder.CreateNSWAdd(cursor, delta, "result");

                auto new_cursor = builder.CreateNSWAdd(result, pool_osize);

                auto limit_ptr = builder.CreateInBoundsGEP(Type::getInt8Ty(target->getContext()), ptls, limit_pos);
                auto limit = builder.CreateAlignedLoad(Type::getInt64Ty(target->getContext()), limit_ptr, Align(sizeof(void *)), "limit");

                auto gt_limit = builder.CreateICmpSGT(new_cursor, limit);

                auto slowpath = BasicBlock::Create(target->getContext(), "slowpath", target->getFunction());
                auto fastpath = BasicBlock::Create(target->getContext(), "fastpath", target->getFunction());

                auto next_instr = target->getNextNode();
                SmallVector<uint32_t, 2> Weights{1, 9};

                MDBuilder MDB(F.getContext());
                SplitBlockAndInsertIfThenElse(gt_limit, next_instr, &slowpath, &fastpath, false, false, MDB.createBranchWeights(Weights));

                builder.SetInsertPoint(next_instr);
                auto phiNode = builder.CreatePHI(target->getCalledFunction()->getReturnType(), 2, "phi_fast_slow");

                // slowpath
                builder.SetInsertPoint(slowpath);
                auto pool_offs = ConstantInt::get(Type::getInt32Ty(F.getContext()), 1);
                auto new_call = builder.CreateCall(smallAllocFunc, { ptls, pool_offs, pool_osize_i32, type });
                new_call->setAttributes(new_call->getCalledFunction()->getAttributes());
                builder.CreateBr(next_instr->getParent());

                // fastpath
                builder.SetInsertPoint(fastpath);
                builder.CreateStore(new_cursor, cursor_ptr);

                // ptls->gc_tls.gc_num.allocd += osize;
                auto pool_alloc_pos = ConstantInt::get(Type::getInt64Ty(target->getContext()), offsetof(jl_tls_states_t, gc_tls_common) + offsetof(jl_gc_tls_states_common_t, gc_num));
                auto pool_alloc_tls = builder.CreateInBoundsGEP(Type::getInt8Ty(target->getContext()), ptls, pool_alloc_pos);
                auto pool_allocd = builder.CreateAlignedLoad(Type::getInt64Ty(target->getContext()), pool_alloc_tls, Align(sizeof(void *)));
                auto pool_allocd_total = builder.CreateAdd(pool_allocd, pool_osize);
                builder.CreateStore(pool_allocd_total, pool_alloc_tls);

                auto v_raw = builder.CreateNSWAdd(result, ConstantInt::get(Type::getInt64Ty(target->getContext()), sizeof(jl_taggedvalue_t)));
                auto v_as_ptr = builder.CreateIntToPtr(v_raw, smallAllocFunc->getReturnType());
                builder.CreateBr(next_instr->getParent());

                phiNode->addIncoming(new_call, slowpath);
                phiNode->addIncoming(v_as_ptr, fastpath);
                phiNode->takeName(target);
                return phiNode;
            }
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
    State S(F);
    auto parent = target->getArgOperand(0);
    if (std::all_of(target->op_begin() + 1, target->op_end(),
                [parent, &S](Value *child) { return parent == child || IsPermRooted(child, &S); })) {
        return;
    }

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


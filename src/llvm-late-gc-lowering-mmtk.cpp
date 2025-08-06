// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-gc-interface-passes.h"

Value* LateLowerGCFrame::lowerGCAllocBytesLate(CallInst *target, Function &F)
{
    assert(target->arg_size() == 3);

    IRBuilder<> builder(target);
    auto ptls = target->getArgOperand(0);
    auto type = target->getArgOperand(2);
    if (auto CI = dyn_cast<ConstantInt>(target->getArgOperand(1))) {
        size_t sz = (size_t)CI->getZExtValue();
        // This is strongly architecture and OS dependent
        int osize;
        int offset = jl_gc_classify_pools(sz, &osize);
        if (offset >= 0) {
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
    }
    return target;
}

// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-gc-interface-passes.h"

Value* LateLowerGCFrame::lowerGCAllocBytesLate(CallInst *target, Function &F)
{
    // Do nothing for the stock GC
    return target;
}

void LateLowerGCFrame::CleanupGCPreserve(Function &F, CallInst *CI, Value *callee, Type *T_size) {
    // Do nothing for the stock GC
}

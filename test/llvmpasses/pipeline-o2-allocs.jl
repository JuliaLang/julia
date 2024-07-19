# This file is a part of Julia. License is MIT: https://julialang.org/license

# RUN: julia --startup-file=no -O2 --check-bounds=yes %s %t -O && llvm-link -S %t/* | FileCheck %s
# RUN: julia --startup-file=no -O3 --check-bounds=yes %s %t -O && llvm-link -S %t/* | FileCheck %s

include(joinpath("..", "testhelpers", "llvmpasses.jl"))

# COM: This tests that simplifycfg is still hoisting allocations in different basic blocks
# COM: into the parent basic block, and deduplicating them in the process
# CHECK-LABEL: @julia_split
# CHECK: alloc
# CHECK-NOT: alloc
# CHECK: ret
function split(maybe)
    if maybe
        Ref(1)
    else
        Ref(2)
    end
end

# COM: This tests that irrespective of the condition outside the loop
# COM: allocations inside the loop are hoisted and the loop is deleted
# CHECK-LABEL: @julia_loop_alloc
# CHECK: phi
# CHECK-NOT: phi
function loop_alloc(N)
    ref = Ref(zero(typeof(N)))
    N <= zero(typeof(N)) && return ref
    for i in one(typeof(N)):N
        ref = Ref(i)
    end
    ref
end

# COM: This tests that even with the allocation LLVM will recognize
# COM: that the loop is meaningless and delete it
# CHECK-LABEL: @julia_loop_const
# CHECK-NOT: br
function loop_const()
    ref = Ref(0)
    for i in 1:1000
        ref = Ref(0)
    end
    ref
end

# COM: This tests that the GC.@preserve macro is being ignored since ref
# COM: is not used anywhere else
# CHECK-LABEL: @julia_nopreserve
# CHECK-NOT: alloc
# CHECK-NOT: julia.gc_preserve_begin
# CHECK-NOT: julia.gc_preserve_end
function nopreserve()
    ref = Ref(0)
    GC.@preserve ref begin end
end

# COM: this cordons off the attributes/function declarations from the actual
# COM: IR that we really want to check
# CHECK: attributes

emit(split, Bool)
emit(loop_alloc, Int64)
emit(loop_const)
emit(nopreserve)

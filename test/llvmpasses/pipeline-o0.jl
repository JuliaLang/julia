# This file is a part of Julia. License is MIT: https://julialang.org/license

# RUN: julia --startup-file=no -O0 --check-bounds=yes %s %t -O && llvm-link -S %t/* | FileCheck %s
# RUN: julia --startup-file=no -O1 --check-bounds=yes %s %t -O && llvm-link -S %t/* | FileCheck %s
# RUN: julia --startup-file=no -O2 --check-bounds=yes %s %t -O && llvm-link -S %t/* | FileCheck %s
# RUN: julia --startup-file=no -O3 --check-bounds=yes %s %t -O && llvm-link -S %t/* | FileCheck %s

include(joinpath("..", "testhelpers", "llvmpasses.jl"))

# CHECK-LABEL: @julia_simple
# CHECK-NOT: julia.get_pgcstack
# CHECK-NOT: julia.gc_alloc_obj
# CHECK: ijl_gc_small_alloc
# COM: we want something vaguely along the lines of asm load from the fs register -> allocate bytes
function simple()
    Ref(0)
end

# CHECK-LABEL: @julia_buildarray
# CHECK-NOT: write_barrier
# CHECK: gc_queue_root
function buildarray()
    out = []
    for i in 1:100
        push!(out, Ref(0))
    end
    out
end

# COM: the reference from unsafe_pointer_to_objref is in a GC root before the next safepoint
# CHECK-LABEL: @julia_objref_root
# CHECK: [[OBJ:%.*]] = call ptr @{{.*}}jl_call1
# CHECK: store ptr [[OBJ]], ptr
# CHECK: call void @{{.*}}jl_gc_collect
function objref_root(r::Base.RefValue{Any}, c::Bool)
    x = GC.@preserve r unsafe_pointer_to_objref(ccall(:jl_call1, Ptr{Cvoid}, (Any, Any), getindex, r))
    GC.gc()
    c && return x
    return nothing
end

emit(simple)
emit(buildarray)
emit(objref_root, Base.RefValue{Any}, Bool)

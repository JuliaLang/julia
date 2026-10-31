# This file is a part of Julia. License is MIT: https://julialang.org/license
# RUN: julia --startup-file=no --check-bounds=yes %s %t -O
# RUN: cat %t/* | FileCheck %s

include(joinpath("..", "testhelpers", "llvmpasses.jl"))

# Test for GenericMemoryRef address space bug
# Issue: stores incorrectly use addrspace(10) instead of addrspace(11)
# in bounds checking code, causing LLVM assertion failures
#
# The `boundscheck` argument of `memoryrefnew` is passed explicitly here.
# Writing `x[i]` would not do: `Base.getindex` validates the index with its own
# `@boundscheck` and then passes `false`, so the intrinsic emits no check of its
# own and this bounds-checking path would not be reached.
function bf(i, x)
    ref = Core.memoryrefnew(getfield(x, :ref), i, true)
    v = Core.memoryrefget(ref, :not_atomic, false)
    Core.memoryrefset!(ref, v * v, :not_atomic, false)
    nothing
end

# CHECK-LABEL: @julia_bf
# CHECK: oob:
# CHECK: store ptr {{.*}}, ptr %"box::GenericMemoryRef"
# CHECK-NOT: store {{.*}} addrspace(10) {{.*}}GenericMemoryRef
# CHECK: call void @ijl_bounds_error_int

emit(bf, Int, Vector{Float64})

# Known alignment must survive converting Julia references to ordinary pointers.
function aligned_ref_load(x::Base.RefValue{UInt})
    unsafe_load(Ptr{UInt}(pointer_from_objref(x)))
end

# CHECK-LABEL: @julia_aligned_ref_load
# CHECK: %pointerref = load i{{32|64}}, ptr {{.*}}, align {{4|8}}
emit(aligned_ref_load, Base.RefValue{UInt})

function aligned_ref_store(x::Base.RefValue{UInt}, value::UInt)
    unsafe_store!(Ptr{UInt}(pointer_from_objref(x)), value)
    nothing
end

# CHECK-LABEL: @julia_aligned_ref_store
# CHECK: store i{{32|64}} {{.*}}, ptr {{.*}}, align {{4|8}}
emit(aligned_ref_store, Base.RefValue{UInt}, UInt)

function aligned_array_load(x::Vector{UInt}, i::Int)
    GC.@preserve x unsafe_load(pointer(x), i)
end

# CHECK-LABEL: @julia_aligned_array_load
# CHECK: %pointerref = load i{{32|64}}, ptr {{.*}}, align {{4|8}}
emit(aligned_array_load, Vector{UInt}, Int)

function aligned_memory_load(x::Memory{UInt}, i::Int)
    GC.@preserve x unsafe_load(pointer(x), i)
end

# CHECK-LABEL: @julia_aligned_memory_load
# CHECK: %pointerref = load i{{32|64}}, ptr {{.*}}, align {{4|8}}
emit(aligned_memory_load, Memory{UInt}, Int)

function byte_array_word_load(x::Vector{UInt8}, i::Int)
    GC.@preserve x unsafe_load(Ptr{UInt}(pointer(x, i)))
end

# CHECK-LABEL: @julia_byte_array_word_load
# CHECK: %pointerref = load i{{32|64}}, ptr {{.*}}, align 1
emit(byte_array_word_load, Vector{UInt8}, Int)

function unknown_alignment_load(x::Ptr{UInt})
    unsafe_load(x)
end

# CHECK-LABEL: @julia_unknown_alignment_load
# CHECK: %pointerref = load i{{32|64}}, ptr {{.*}}, align 1
emit(unknown_alignment_load, Ptr{UInt})

# Union and zero-size memory references store offsets, rather than data pointers.
function memoryref_offset(x)
    Core.memoryrefoffset(getfield(x, :ref))
end

# CHECK-LABEL: @julia_memoryref_offset
# CHECK: ret i{{32|64}}
emit(memoryref_offset, Vector{Union{Int,Float64}})

# CHECK-LABEL: @julia_memoryref_offset
# CHECK: ret i{{32|64}}
emit(memoryref_offset, Vector{Nothing})

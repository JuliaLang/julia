# This file is a part of Julia. License is MIT: https://julialang.org/license

# RUNx: julia --startup-file=no -O2 --check-bounds=yes %s %t -O && llvm-link -S %t/* | FileCheck %s --check-prefixes=ALL
# RUNx: julia --startup-file=no -O3 --check-bounds=yes %s %t -O && llvm-link -S %t/* | FileCheck %s --check-prefixes=ALL

# RUN: julia --startup-file=no -O2 --check-bounds=no %s %t -O && llvm-link -S %t/* | FileCheck %s --check-prefixes=ALL,BC_OFF
# RUN: julia --startup-file=no -O3 --check-bounds=no %s %t -O && llvm-link -S %t/* | FileCheck %s --check-prefixes=ALL,BC_OFF

# RUN: julia --startup-file=no -O2 --check-bounds=auto %s %t -O && llvm-link -S %t/* | FileCheck %s --check-prefixes=ALL,BC_AUTO
# RUN: julia --startup-file=no -O3 --check-bounds=auto %s %t -O && llvm-link -S %t/* | FileCheck %s --check-prefixes=ALL,BC_AUTO

include(joinpath("..", "testhelpers", "llvmpasses.jl"))

# COM: Ensure safe iteration over one array is not boundschecked and is vectorized

# ALL-LABEL: @julia_iterate_read
# ALL-NOT: bounds_error
# ALL: vector.body
function iterate_read(arr)
    total = zero(eltype(arr))
    for i in eachindex(arr)
        total += arr[i]
    end
    total
end

# ALL-LABEL: @julia_iterate_write
# ALL-NOT: bounds_error
# ALL: vector.body
function iterate_write(arr, out)
    for i in eachindex(arr, out)
        out[i] = arr[i]
    end
end

# ALL-LABEL: @"julia_iterate_write!
# ALL-NOT: bounds_error
# ALL: vector.body
function iterate_write!(arr)
    for i in eachindex(arr)
        arr[i] *= 2
    end
end

# COM: Ensure safe iteration over multiple arrays is not boundschecked and is vectorized

# ALL-LABEL: @julia_multiiterate_read
# ALL-NOT: bounds_error
# ALL: vector.body
function multiiterate_read(arr1, arr2)
    total = zero(eltype(arr1))
    for i in eachindex(arr1, arr2)
        total += arr1[i]
        total += arr2[i]
    end
    total
end

# ALL-LABEL: @japi1_multiiterate_write
# ALL-NOT: bounds_error
# ALL: vector.body
function multiiterate_write(arr1, arr2, arr3)
    for i in eachindex(arr1, arr2, arr3)
        arr3[i] += arr1[i]
        arr3[i] += arr2[i]
    end
end

# ALL-LABEL: @"julia_multiiterate_write!
# ALL-NOT: bounds_error
# ALL: vector.body
function multiiterate_write!(arr1, arr2)
    for i in eachindex(arr1, arr2)
        arr1[i] += arr2[i]
    end
end

# COM: memset checks

# COM: INT64
# ALL: define {{.*}} @julia_zeros
# ALL-NOT: bounds_error
# COM: memset is not used with bounds checks on (too late in the pipeline)
# BC_OFF: llvm.memset
# BC_AUTO: llvm.memset

# COM: INT32
# ALL: define {{.*}} @julia_zeros
# ALL-NOT: bounds_error
# COM: memset is not used with bounds checks on (too late in the pipeline)
# BC_OFF: llvm.memset
# BC_AUTO: llvm.memset

# COM: INT16
# ALL: define {{.*}} @julia_zeros
# ALL-NOT: bounds_error
# COM: memset is not used with bounds checks on (too late in the pipeline)
# BC_OFF: llvm.memset
# BC_AUTO: llvm.memset

# COM: check reductive indvars/vectorization

# ALL-LABEL: @julia_sumloop
# ALL: mul
function sumloop(N)
    total = zero(typeof(N))
    for i in one(typeof(N)):N
        total += i
    end
    total
end
# ALL-LABEL: @julia_simd_sumloop
# ALL: vector.body
function simd_sumloop(N)
    total = zero(typeof(N))
    @simd for i in one(typeof(N)):N
        total += i
    end
    total
end

# COM: check hoisting and loop deletion functionality

# ALL-LABEL: @julia_loopedlength
# ALL-NOT: br
# ALL: ret
function loopedlength(arr)
    len = length(arr)
    for i in 1:length(arr)
        len = length(arr)
    end
    len
end
# COM: Vector
# ALL-LABEL: @julia_memset_like
# ALL: vector.body

# COM: Memory
# ALL-LABEL: @julia_memset_like
# ALL: vector.body
function memset_like(mem)
    for idx in eachindex(mem)
        mem[idx] = 1.0
    end
end

emit(iterate_read, Vector{Int64})
emit(iterate_write, Vector{Int64}, Vector{Int64})
emit(iterate_write!, Vector{Int64})

emit(multiiterate_read, Vector{Int64}, Vector{Int64})
emit(multiiterate_write, Vector{Int64}, Vector{Int64}, Vector{Int64})
emit(multiiterate_write!, Vector{Int64}, Vector{Int64})

emit(zeros, Type{Int64}, Int64)
emit(zeros, Type{Int32}, Int64)
emit(zeros, Type{Int16}, Int64)
# COM: Int8 is hardcoded to memset anyways

emit(sumloop, Int64)
# COM: Float64 doesn't vectorize for some reason
emit(simd_sumloop, Float32)

emit(loopedlength, Vector{Int64})

emit(memset_like, Vector{Float64})
emit(memset_like, Memory{Float64})

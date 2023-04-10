# RUN: julia --startup-file=no -O2 --check-bounds=auto %s %t -O && llvm-link -S %t/* | FileCheck %s
# RUN: julia --startup-file=no -O3 --check-bounds=auto %s %t -O && llvm-link -S %t/* | FileCheck %s

include(joinpath("..", "testhelpers", "llvmpasses.jl"))

# COM: Check broadcasted outer product is vectorized

# COM: Float32
# CHECK: @japi1_outer_product
# CHECK: load <[[VEC_FACTOR:[0-9]+]] x float>
# CHECK: fmul <[[VEC_FACTOR]] x float>
# CHECK: store <[[VEC_FACTOR]] x float>

# COM: Float64
# CHECK: @japi1_outer_product
# CHECK: load <[[VEC_FACTOR:[0-9]+]] x double>
# CHECK: fmul <[[VEC_FACTOR]] x double>
# CHECK: store <[[VEC_FACTOR]] x double>

# COM: Int32
# CHECK: @japi1_outer_product
# CHECK: load <[[VEC_FACTOR:[0-9]+]] x i32>
# CHECK: mul <[[VEC_FACTOR]] x i32>
# CHECK: store <[[VEC_FACTOR]] x i32>

# COM: Int64
# CHECK: @japi1_outer_product
# CHECK: load <[[VEC_FACTOR:[0-9]+]] x i64>
# CHECK: mul <[[VEC_FACTOR]] x i64>
# CHECK: store <[[VEC_FACTOR]] x i64>

function outer_product(R, x, y)
    R .= x .* y'
end

# COM: Check broadcasted inner product is vectorized

# COM: Float32
# CHECK: @japi1_inner_product
# CHECK: load <[[VEC_FACTOR:[0-9]+]] x float>
# CHECK: fmul <[[VEC_FACTOR]] x float>
# CHECK: store <[[VEC_FACTOR]] x float>

# COM: Float64
# CHECK: @japi1_inner_product
# CHECK: load <[[VEC_FACTOR:[0-9]+]] x double>
# CHECK: fmul <[[VEC_FACTOR]] x double>
# CHECK: store <[[VEC_FACTOR]] x double>

# COM: Int32
# CHECK: @japi1_inner_product
# CHECK: load <[[VEC_FACTOR:[0-9]+]] x i32>
# CHECK: mul <[[VEC_FACTOR]] x i32>
# CHECK: store <[[VEC_FACTOR]] x i32>

# COM: Int64
# CHECK: @japi1_inner_product
# CHECK: load <[[VEC_FACTOR:[0-9]+]] x i64>
# CHECK: mul <[[VEC_FACTOR]] x i64>
# CHECK: store <[[VEC_FACTOR]] x i64>

function inner_product(R, x, y)
    R .= x' .* y
end

# COM: Check broadcasted multiplications are vectorized

# COM: Float32
# CHECK: @japi1_squash
# CHECK: load <[[VEC_FACTOR:[0-9]+]] x float>
# CHECK: fmul <[[VEC_FACTOR]] x float>
# CHECK: store <[[VEC_FACTOR]] x float>

# COM: Float64
# CHECK: @japi1_squash
# CHECK: load <[[VEC_FACTOR:[0-9]+]] x double>
# CHECK: fmul <[[VEC_FACTOR]] x double>
# CHECK: store <[[VEC_FACTOR]] x double>

# COM: Int32
# CHECK: @japi1_squash
# CHECK: load <[[VEC_FACTOR:[0-9]+]] x i32>
# CHECK: mul <[[VEC_FACTOR]] x i32>
# CHECK: store <[[VEC_FACTOR]] x i32>

# COM: Int64
# CHECK: @japi1_squash
# CHECK: load <[[VEC_FACTOR:[0-9]+]] x i64>
# CHECK: mul <[[VEC_FACTOR]] x i64>
# CHECK: store <[[VEC_FACTOR]] x i64>

function squash(R, x, M, y)
    R .= x .* M .* y'
end

emit(outer_product, Matrix{Float32}, Vector{Float32}, Vector{Float32})
emit(outer_product, Matrix{Float64}, Vector{Float64}, Vector{Float64})
emit(outer_product, Matrix{Int32}, Vector{Int32}, Vector{Int32})
emit(outer_product, Matrix{Int64}, Vector{Int64}, Vector{Int64})

emit(inner_product, Matrix{Float32}, Vector{Float32}, Vector{Float32})
emit(inner_product, Matrix{Float64}, Vector{Float64}, Vector{Float64})
emit(inner_product, Matrix{Int32}, Vector{Int32}, Vector{Int32})
emit(inner_product, Matrix{Int64}, Vector{Int64}, Vector{Int64})

emit(squash, Matrix{Float32}, Vector{Float32}, Matrix{Float32}, Vector{Float32})
emit(squash, Matrix{Float64}, Vector{Float64}, Matrix{Float64}, Vector{Float64})
emit(squash, Matrix{Int32}, Vector{Int32}, Matrix{Int32}, Vector{Int32})
emit(squash, Matrix{Int64}, Vector{Int64}, Matrix{Int64}, Vector{Int64})

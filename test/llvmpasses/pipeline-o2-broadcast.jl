# RUN: julia --startup-file=no -O2 --check-bounds=auto %s %t -O && llvm-link -S %t/* | FileCheck %s
# RUN: julia --startup-file=no -O3 --check-bounds=auto %s %t -O && llvm-link -S %t/* | FileCheck %s

include(joinpath("..", "testhelpers", "llvmpasses.jl"))

# COM: Check broadcasted outer product is vectorized

# COM: Float32
# CHECK: @japi1_prod_v_vT
# COM: load <[[VSCALE:(vscale x )?]][[VEC_FACTOR:[0-9]+]] x float>
# CHECK: fmul <[[VSCALE]][[VEC_FACTOR]] x float>
# CHECK: store <[[VSCALE]][[VEC_FACTOR]] x float>

# COM: Float64
# CHECK: @japi1_prod_v_vT
# COM: load <[[VSCALE:(vscale x )?]][[VEC_FACTOR:[0-9]+]] x double>
# CHECK: fmul <[[VSCALE]][[VEC_FACTOR]] x double>
# CHECK: store <[[VSCALE]][[VEC_FACTOR]] x double>

# COM: Int32
# CHECK: @japi1_prod_v_vT
# COM: load <[[VSCALE:(vscale x )?]][[VEC_FACTOR:[0-9]+]] x i32>
# CHECK: mul <[[VSCALE]][[VEC_FACTOR]] x i32>
# CHECK: store <[[VSCALE]][[VEC_FACTOR]] x i32>

# COM: Int64
# CHECK: @japi1_prod_v_vT
# COM: load <[[VSCALE:(vscale x )?]][[VEC_FACTOR:[0-9]+]] x i64>
# CHECK: mul <[[VSCALE]][[VEC_FACTOR]] x i64>
# CHECK: store <[[VSCALE]][[VEC_FACTOR]] x i64>

function prod_v_vT(R, x, y)
    R .= x .* y'
end

# COM: Check broadcasted inner product is vectorized

# COM: Float32
# CHECK: @japi1_prod_vT_v
# COM: load <[[VSCALE:(vscale x )?]][[VEC_FACTOR:[0-9]+]] x float>
# CHECK: fmul <[[VSCALE]][[VEC_FACTOR]] x float>
# CHECK: store <[[VSCALE]][[VEC_FACTOR]] x float>

# COM: Float64
# CHECK: @japi1_prod_vT_v
# COM: load <[[VSCALE:(vscale x )?]][[VEC_FACTOR:[0-9]+]] x double>
# CHECK: fmul <[[VSCALE]][[VEC_FACTOR]] x double>
# CHECK: store <[[VSCALE]][[VEC_FACTOR]] x double>

# COM: Int32
# CHECK: @japi1_prod_vT_v
# COM: load <[[VSCALE:(vscale x )?]][[VEC_FACTOR:[0-9]+]] x i32>
# CHECK: mul <[[VSCALE]][[VEC_FACTOR]] x i32>
# CHECK: store <[[VSCALE]][[VEC_FACTOR]] x i32>

# COM: Int64
# CHECK: @japi1_prod_vT_v
# COM: load <[[VSCALE:(vscale x )?]][[VEC_FACTOR:[0-9]+]] x i64>
# CHECK: mul <[[VSCALE]][[VEC_FACTOR]] x i64>
# CHECK: store <[[VSCALE]][[VEC_FACTOR]] x i64>

function prod_vT_v(R, x, y)
    R .= x' .* y
end

# COM: Check broadcasted multiplications are vectorized

# COM: Float32
# CHECK: @japi1_prod_v_M_vT
# COM: load <[[VSCALE:(vscale x )?]][[VEC_FACTOR:[0-9]+]] x float>
# CHECK: fmul <[[VSCALE]][[VEC_FACTOR]] x float>
# CHECK: store <[[VSCALE]][[VEC_FACTOR]] x float>

# COM: Float64
# CHECK: @japi1_prod_v_M_vT
# COM: load <[[VSCALE:(vscale x )?]][[VEC_FACTOR:[0-9]+]] x double>
# CHECK: fmul <[[VSCALE]][[VEC_FACTOR]] x double>
# CHECK: store <[[VSCALE]][[VEC_FACTOR]] x double>

# COM: Int32
# CHECK: @japi1_prod_v_M_vT
# COM: load <[[VSCALE:(vscale x )?]][[VEC_FACTOR:[0-9]+]] x i32>
# CHECK: mul <[[VSCALE]][[VEC_FACTOR]] x i32>
# CHECK: store <[[VSCALE]][[VEC_FACTOR]] x i32>

# COM: Int64
# CHECK: @japi1_prod_v_M_vT
# COM: load <[[VSCALE:(vscale x )?]][[VEC_FACTOR:[0-9]+]] x i64>
# CHECK: mul <[[VSCALE]][[VEC_FACTOR]] x i64>
# CHECK: store <[[VSCALE]][[VEC_FACTOR]] x i64>

function prod_v_M_vT(R, x, M, y)
    R .= x .* M .* y'
end

emit(prod_v_vT, Matrix{Float32}, Vector{Float32}, Vector{Float32})
emit(prod_v_vT, Matrix{Float64}, Vector{Float64}, Vector{Float64})
emit(prod_v_vT, Matrix{Int32}, Vector{Int32}, Vector{Int32})
emit(prod_v_vT, Matrix{Int64}, Vector{Int64}, Vector{Int64})

emit(prod_vT_v, Matrix{Float32}, Vector{Float32}, Vector{Float32})
emit(prod_vT_v, Matrix{Float64}, Vector{Float64}, Vector{Float64})
emit(prod_vT_v, Matrix{Int32}, Vector{Int32}, Vector{Int32})
emit(prod_vT_v, Matrix{Int64}, Vector{Int64}, Vector{Int64})

emit(prod_v_M_vT, Matrix{Float32}, Vector{Float32}, Matrix{Float32}, Vector{Float32})
emit(prod_v_M_vT, Matrix{Float64}, Vector{Float64}, Matrix{Float64}, Vector{Float64})
emit(prod_v_M_vT, Matrix{Int32}, Vector{Int32}, Matrix{Int32}, Vector{Int32})
emit(prod_v_M_vT, Matrix{Int64}, Vector{Int64}, Matrix{Int64}, Vector{Int64})

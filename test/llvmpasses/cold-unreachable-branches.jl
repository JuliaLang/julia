# This file is a part of Julia. License is MIT: https://julialang.org/license

# RUN: julia --startup-file=no -O2 --check-bounds=yes %s %t -O && llvm-link -S %t/* | FileCheck %s

include(joinpath("..", "testhelpers", "llvmpasses.jl"))

# COM: Branches to blocks ending in unreachable should have branch weight metadata

# CHECK-LABEL: @julia_boundscheck_1d
# CHECK: br i1 %{{[^,]+}}, label %{{[^,]+}}, label %{{[^,]+}}, {{.*}}!prof
function boundscheck_1d(a::Vector{Float64}, i::Int)
    a[i]
end

# CHECK-LABEL: @julia_boundscheck_2d
# CHECK: br i1 %{{[^,]+}}, label %{{[^,]+}}, label %{{[^,]+}}, {{.*}}!prof
# CHECK: br i1 %{{[^,]+}}, label %{{[^,]+}}, label %{{[^,]+}}, {{.*}}!prof
function boundscheck_2d(a::Matrix{Float64}, i::Int, j::Int)
    a[i, j]
end

# CHECK-LABEL: @julia_throw_when_nonpositive
# CHECK: br i1 %{{[^,]+}}, label %{{[^,]+}}, label %{{[^,]+}}, {{.*}}!prof
function throw_when_nonpositive(x::Int, y::Int)
    if x > 0
        return y
    else
        throw(DomainError(x))
    end
end

# CHECK-DAG: !{!"branch_weights", i32 2000, i32 1}
# CHECK-DAG: !{!"branch_weights", i32 1, i32 2000}

emit(boundscheck_1d, Vector{Float64}, Int)
emit(boundscheck_2d, Matrix{Float64}, Int, Int)
emit(throw_when_nonpositive, Int, Int)

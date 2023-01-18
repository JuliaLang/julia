# This file is a part of Julia. License is MIT: https://julialang.org/license

# RUN: julia --startup-file=no %s %t && llvm-link -S %t/* -o %t/module.ll
# RUN: cat %t/module.ll | FileCheck %s

## Notes:
# This script uses the `emit` function (defined llvmpasses.jl) to emit either
# optimized or unoptimized LLVM IR. Each function is emitted individually and
# `llvm-link` is used to create a single module that can be passed to opt.
# The order in which files are emitted and linked is important since `lit` will
# process the test cases in order.

include(joinpath("..", "testhelpers", "llvmpasses.jl"))

import Base.Experimental: Const, @aliasscope

# CHECK-LABEL: @julia_simple
function simple(A, B)
    @aliasscope @inbounds for I in eachindex(A, B)
        A[I] = Const(B)[I]
# CHECK: load double, {{.*}} !alias.scope [[SCOPE_LD:![0-9]+]]
# CHECK: store double {{.*}} !noalias [[SCOPE_ST:![0-9]+]]
    end
    return 0 # return nothing causes japi1
end

# CHECK-LABEL: @julia_constargs
function constargs(A, B::Const)
    @aliasscope @inbounds for I in eachindex(A, B)
        A[I] = B[I]
# CHECK: load double, {{.*}} !alias.scope [[SCOPE2_LD:![0-9]+]]
# CHECK: store double {{.*}} !noalias [[SCOPE2_ST:![0-9]+]]
    end
    return 0
end

# CHECK-LABEL: @"julia_micro_ker!
function micro_ker!(AB, Ac, Bc, kc, offSetA, offSetB)
    MR = 8; NR = 6;
    @inbounds @aliasscope for k in 1:kc
        for j in 1:NR, i in 1:MR
            AB[i+(j-1)*MR] = muladd(Const(Ac)[offSetA+i], Const(Bc)[offSetB+j], Const(AB)[i+(j-1)*MR])
# CHECK: load double, {{.*}} !alias.scope [[SCOPE3_LD:![0-9]+]]
# CHECK: load double, {{.*}} !alias.scope [[SCOPE3_LD]]
# CHECK: load double, {{.*}} !alias.scope [[SCOPE3_LD]]
# CHECK: store double {{.*}} !noalias [[SCOPE3_ST:![0-9]+]]
        end
        offSetA += MR
        offSetB += NR
    end
    return
end

# CHECK-DAG: [[SCOPE_LD]] = !{[[ALIASSCOPE:![0-9]+]]
# CHECK-DAG: [[SCOPE_ST]] = !{[[ALIASSCOPE]]
# CHECK-DAG: [[SCOPE2_LD]] = !{[[ALIASSCOPE2:![0-9]+]]
# CHECK-DAG: [[SCOPE2_ST]] = !{[[ALIASSCOPE2]]
# CHECK-DAG: [[SCOPE3_LD]] = !{[[ALIASSCOPE3:![0-9]+]]
# CHECK-DAG: [[SCOPE3_ST]] = !{[[ALIASSCOPE3]]
# CHECK-DAG: [[ALIASSCOPE]] = !{!"aliasscope", [[MDNODE:![0-9]+]]}
# CHECK-DAG: [[MDNODE]] = !{!"simple"}

emit(simple, Vector{Float64}, Vector{Float64})
emit(constargs, Vector{Float64}, Const{Float64, 1})
emit(micro_ker!, Matrix{Float64}, Vector{Float64}, Vector{Float64}, Int64, Int64, Int64)

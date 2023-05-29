# This file is a part of Julia. License is MIT: https://julialang.org/license

# REQUIRES: x86_64
# RUN: julia --startup-file=no %s %t -O && llvm-link -S %t/* -o %t/module.ll
# RUN: cat %t/module.ll | llc - -mtriple=x86_64-- -mattr=fma | FileCheck %s

## Notes:
# This script uses the `emit` function (defined llvmpasses.jl) to emit either
# optimized or unoptimized LLVM IR. Each function is emitted individually and
# `llvm-link` is used to create a single module that can be passed to opt.
# The order in which files are emitted and linked is important since `lit` will
# process the test cases in order.

include(joinpath("..", "testhelpers", "llvmpasses.jl"))

# CHECK-LABEL: julia_dotf_
function dotf(a, b)
    s = 0.0
    @inbounds @simd for i ∈ eachindex(a)
# CHECK:    vfmadd231pd
# CHECK-NEXT:    vfmadd231pd
# CHECK-NEXT:    vfmadd231pd
# CHECK-NEXT:    vfmadd231pd
        s += a[i] * b[i]
    end
    s
end

emit(dotf, Vector{Float64}, Vector{Float64})

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

# CHECK-LABEL: @julia_minimum_nonans
@nonans function minimum_nonans(itr)
    return reduce(itr) do a, b ifelse(a < b, a, b) end
end

# CHECK: attributes #{{[0-9]+}} = {{{[a-z "=]*}}"no-nans-fp-math"="true"{{[a-z "=]*}}}
emit(minimum_nonans, Vector{Float64})

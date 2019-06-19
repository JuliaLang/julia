# This file is a part of Julia. License is MIT: https://julialang.org/license

# RUN: julia -g0 --startup-file=no %s %t && llvm-link -S %t/* -o %t/module.ll
# RUN: cat %t/module.ll | FileCheck %s

## Notes:
# This script uses the `emit` function (defined llvmpasses.jl) to emit either
# optimized or unoptimized LLVM IR. Each function is emitted individually and
# `llvm-link` is used to create a single module that can be passed to opt.
# The order in which files are emitted and linked is important since `lit` will
# process the test cases in order.

include(joinpath("..", "testhelpers", "llvmpasses.jl"))

@noinline function simple_noinline(A, B)
    return A + B
end

@noinline Base.@pure function simple_pure_helper(A, B)
    return A + B
end
function simple_pure(A, B)
    return simple_pure_helper(A, B)
end

# CHECK: define double @julia_simple_noinline_{{[0-9]+}}(double, double) #[[NOINLINE:[0-9]+]] {
emit(simple_noinline, Float64, Float64)
# CHECK-LABEL: @julia_simple_pure
# CHECK: call double @julia_simple_pure_helper_{{[0-9]+}}(double %0, double %1) #[[PURE:[0-9]+]]
# CHECK: declare double @julia_simple_pure_helper_{{[0-9]+}}(double, double) #[[PURE]]
emit(simple_pure, Float64, Float64)
# CHECK-LABEL @japi1_simple_pure
# CHECK: call cc37 {{.+}} @japi1_simple_pure_helper_{{.+}} #[[PURE]]
# CHECK: declare nonnull %jl_value_t addrspace(10)* @japi1_simple_pure_helper_{{[0-9]+}}(%jl_value_t addrspace(10)*, %jl_value_t addrspace(10)**, i32) #[[PURE:[0-9]+]]
emit(simple_pure, BigFloat, BigFloat)

# CHECK: attributes #[[NOINLINE]] = {{{([a-z]+ )*}} noinline {{([a-z]+ )*}}}
# CHECK: attributes #[[PURE]] = { nounwind readnone "thunk" }

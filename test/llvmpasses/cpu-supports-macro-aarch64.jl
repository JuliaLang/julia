# This file is a part of Julia. License is MIT: https://julialang.org/license

# RUN: julia --startup-file=no %s %t -O && llvm-link -S %t/* -o %t/module.ll
# RUN: cat %t/module.ll | FileCheck %s
# REQUIRES: aarch64

# Same coverage as cpu-supports-macro.jl, but for AArch64.

include(joinpath("..", "testhelpers", "llvmpasses.jl"))

# CHECK-LABEL: @julia_query_neon
# CHECK-NOT: julia.cpu.supports
# CHECK-NOT: ijl_cpu_supports
# CHECK: ret i8 {{[01]}}
query_neon() = Base.@cpu_supports aarch64 neon
emit(query_neon)

# Multiple features ANDed.
# CHECK-LABEL: @julia_query_multi
# CHECK-NOT: julia.cpu.supports
# CHECK-NOT: ijl_cpu_supports
# CHECK: ret i8 {{[01]}}
query_multi() = Base.@cpu_supports aarch64 neon "fp-armv8"
emit(query_multi)

# Hyphenated name via String literal.
# CHECK-LABEL: @julia_query_v82a
# CHECK-NOT: julia.cpu.supports
# CHECK-NOT: ijl_cpu_supports
# CHECK: ret i8 {{[01]}}
query_v82a() = Base.@cpu_supports aarch64 "v8.2a"
emit(query_v82a)

# CPU-model expansion.
# CHECK-LABEL: @julia_query_a78
# CHECK-NOT: julia.cpu.supports
# CHECK-NOT: ijl_cpu_supports
# CHECK: ret i8 {{[01]}}
query_a78() = Base.@cpu_supports aarch64 "cortex-a78"
emit(query_a78)

# Cross-arch prefix: x86_64 on aarch64 host folds to false at parse time.
# CHECK-LABEL: @julia_query_crossarch
# CHECK-NOT: julia.cpu.supports
# CHECK-NOT: ijl_cpu_supports
# CHECK: ret i8 0
query_crossarch() = Base.@cpu_supports x86_64 avx2
emit(query_crossarch)

# Branch dispatch: dead arm eliminated.
# CHECK-LABEL: @julia_dispatch
# CHECK-NOT: julia.cpu.supports
# CHECK-NOT: ijl_cpu_supports
# CHECK-NOT: br i1
@inline dispatch(x) = (Base.@cpu_supports aarch64 neon) ? x * 2 : x + 1
emit(dispatch, Int)

# Assert true: folds to no-op.
# CHECK-LABEL: @julia_assert_holds
# CHECK-NOT: julia.cpu.supports
# CHECK-NOT: ijl_cpu_supports
# CHECK-NOT: AssertionError
# CHECK: ret i64
function assert_holds(x::Int)
    @assert Base.@cpu_supports aarch64 neon
    return x * 2
end
emit(assert_holds, Int)

# Assert false via the __never__ sentinel (arch-independent).
# CHECK-LABEL: @julia_assert_fails
# CHECK-NOT: julia.cpu.supports
# CHECK-NOT: ijl_cpu_supports
# CHECK-NOT: 10000
# CHECK: AssertionError
function assert_fails(x::Int)
    @assert Base.@cpu_supports __never__
    return x * 10000
end
emit(assert_fails, Int)

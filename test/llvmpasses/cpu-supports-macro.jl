# This file is a part of Julia. License is MIT: https://julialang.org/license

# RUN: julia --startup-file=no %s %t -O && llvm-link -S %t/* -o %t/module.ll
# RUN: cat %t/module.ll | FileCheck %s
# REQUIRES: x86_64

# Verify that Base.@cpu_supports folds to constants and dead branches
# are eliminated by the CPUFeatures pass + cleanup.

include(joinpath("..", "testhelpers", "llvmpasses.jl"))

# CHECK-LABEL: @julia_query_avx2
# CHECK-NOT: julia.cpu.supports
# CHECK-NOT: ijl_cpu_supports
# CHECK: ret i8 {{[01]}}
query_avx2() = Base.@cpu_supports x86_64 avx2
emit(query_avx2)

# Multiple features ANDed.
# CHECK-LABEL: @julia_query_multi
# CHECK-NOT: julia.cpu.supports
# CHECK-NOT: ijl_cpu_supports
# CHECK: ret i8 {{[01]}}
query_multi() = Base.@cpu_supports x86_64 avx2 fma bmi2
emit(query_multi)

# Dotted name via String literal.
# CHECK-LABEL: @julia_query_sse42
# CHECK-NOT: julia.cpu.supports
# CHECK-NOT: ijl_cpu_supports
# CHECK: ret i8 {{[01]}}
query_sse42() = Base.@cpu_supports x86_64 "sse4.2"
emit(query_sse42)

# CPU-model expansion: all ~30 haswell feature queries collapse to one constant.
# CHECK-LABEL: @julia_query_haswell
# CHECK-NOT: julia.cpu.supports
# CHECK-NOT: ijl_cpu_supports
# CHECK: ret i8 {{[01]}}
query_haswell() = Base.@cpu_supports x86_64 haswell
emit(query_haswell)

# psABI level via the CPU table.
# CHECK-LABEL: @julia_query_v3
# CHECK-NOT: julia.cpu.supports
# CHECK-NOT: ijl_cpu_supports
# CHECK: ret i8 {{[01]}}
query_v3() = Base.@cpu_supports x86_64 "x86-64-v3"
emit(query_v3)

# Mixed feature + CPU model.
# CHECK-LABEL: @julia_query_mixed
# CHECK-NOT: julia.cpu.supports
# CHECK-NOT: ijl_cpu_supports
# CHECK: ret i8 {{[01]}}
query_mixed() = Base.@cpu_supports x86_64 avx2 haswell
emit(query_mixed)

# Cross-arch prefix: aarch64 on x86 host folds to false at parse time.
# CHECK-LABEL: @julia_query_crossarch
# CHECK-NOT: julia.cpu.supports
# CHECK-NOT: ijl_cpu_supports
# CHECK: ret i8 0
query_crossarch() = Base.@cpu_supports aarch64 neon
emit(query_crossarch)

# Branch dispatch: dead arm eliminated.
# CHECK-LABEL: @julia_dispatch
# CHECK-NOT: julia.cpu.supports
# CHECK-NOT: ijl_cpu_supports
# CHECK-NOT: br i1
@inline dispatch(x) = (Base.@cpu_supports x86_64 avx2) ? x * 2 : x + 1
emit(dispatch, Int)

# Assert true: folds to no-op (no comparison or throw).
# CHECK-LABEL: @julia_assert_holds
# CHECK-NOT: julia.cpu.supports
# CHECK-NOT: ijl_cpu_supports
# CHECK-NOT: AssertionError
# CHECK: ret i64
function assert_holds(x::Int)
    @assert Base.@cpu_supports x86_64 avx2
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

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

import Base.FastMath

# CHECK: call fast float @llvm.sqrt.f32(float %0)
emit(FastMath.sqrt_fast, Float32)


# Float16 operations should be performed as Float32, unless @fastmath is specified
# TODO: this is not true for platforms that natively support Float16

foo(x::T,y::T) where T = x-y == zero(T)
# LOWER: fsub half %0, %1
# FINAL: %2 = fpext half %0 to float
# FINAL: %3 = fpext half %1 to float
# FINAL: fsub half %2, %3
emit(foo, Float16, Float16)

@fastmath foo(x::T,y::T) where T = x-y == zero(T)
# LOWER: fsub fast half %0, %1
# FINAL: fsub fast half %0, %1
emit(foo, Float16, Float16)

# This file is a part of Julia. License is MIT: https://julialang.org/license

# RUN: julia --startup-file=no %s %t -O && llvm-link -S %t/* -o %t/module.ll
# RUN: cat %t/module.ll | FileCheck %s

## Notes:
# This script uses the `emit` function (defined llvmpasses.jl) to emit either
# optimized or unoptimized LLVM IR. Each function is emitted individually and
# `llvm-link` is used to create a single module that can be passed to opt.
# The order in which files are emitted and linked is important since `lit` will
# process the test cases in order.

include(joinpath("..", "testhelpers", "llvmpasses.jl"))

import Base.FastMath

# CHECK: call fast float @llvm.sqrt.f32(float %"x::Float32")
emit(FastMath.sqrt_fast, Float32)


# Float16 operations should be performed as Float32, unless @fastmath is specified
# TODO: this is not true for platforms that natively support Float16

foo(x::T,y::T) where T = x-y == zero(T)
# CHECK: define {{(swiftcc )?}}i8 @julia_foo_{{[0-9]+}}({{.*}}half %[[X:"x::Float16"]], half %[[Y:"y::Float16"]]) {{.*}}{
# CHECK-DAG: %[[XEXT:[0-9]+]] = fpext half %[[X]] to float
# CHECK-DAG: %[[YEXT:[0-9]+]] = fpext half %[[Y]] to float
# CHECK: %[[DIFF:[0-9]+]] = fsub float %[[XEXT]], %[[YEXT]]
# CHECK: %[[TRUNC:[0-9]+]] = fptrunc float %[[DIFF]] to half
# CHECK: %[[DIFFEXT:[0-9]+]] = fpext half %[[TRUNC]] to float
# CHECK: %[[CMP:[0-9]+]] = fcmp oeq float %[[DIFFEXT]], 0.000000e+00
# CHECK: %[[ZEXT:[0-9]+]] = zext i1 %[[CMP]] to i8
# CHECK: ret i8 %[[ZEXT]]
# CHECK: }
emit(foo, Float16, Float16)

@fastmath foo(x::T,y::T) where T = x-y == zero(T)
# CHECK: define {{(swiftcc )?}}i8 @julia_foo_{{[0-9]+}}({{.*}}half %[[X:"x::Float16"]], half %[[Y:"y::Float16"]]) {{.*}}{
# CHECK: %[[DIFF:[0-9]+]] = fsub fast half %[[X]], %[[Y]]
# CHECK: %[[CMP:[0-9]+]] = fcmp fast oeq half %[[DIFF]], 0xH0000
# CHECK: %[[ZEXT:[0-9]+]] = zext i1 %[[CMP]] to i8
# CHECK: ret i8 %[[ZEXT]]
# CHECK: }
emit(foo, Float16, Float16)

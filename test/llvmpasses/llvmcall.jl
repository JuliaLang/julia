# This file is a part of Julia. License is MIT: https://julialang.org/license

# RUN: export JULIA_LLVM_ARGS=""

# RUN: julia --startup-file=no %s %t
# RUN: cat %t/* | FileCheck %s --check-prefixes=CHECK,OPAQUE

include(joinpath("..", "testhelpers", "llvmpasses.jl"))

struct Foo
    x::Int32
    y::Int32
end

@generated foo(x) = :(ccall("extern foo", llvmcall, $x, ($x,), x))
bar(x) = ntuple(i -> VecElement{Float16}(x[i]), 2)

# CHECK: define
# CHECK-SAME: half @julia_foo
# CHECK-SAME: {
# CHECK-NOT: define
# CHECK: [[FOO_RET:%.*]] = call half @foo(half [[FOO_ARG:%.*]])
# CHECK-NOT: define
# CHECK: ret half
# CHECK-NOT: define
# CHECK: }
emit(foo, Float16)

# COM: Make sure that we don't miss a function by accident (helps localize errors)
# CHECK-NOT: {
# CHECK-NOT: }
# CHECK: define
# TYPED-SAME: nonnull {} addrspace(10)* @jfptr
# OPAQUE-SAME: nonnull ptr addrspace(10) @jfptr
# CHECK-SAME: {

# CHECK: define
# CHECK-SAME: [2 x half] @julia_foo
# CHECK-SAME: {
# CHECK-NOT: define
# CHECK: [[FOO_RET:%.*]] = call [2 x half] @foo([2 x half] [[FOO_ARG:%.*]])
# CHECK-NOT: define
# CHECK: ret [2 x half]
# CHECK-NOT: define
# CHECK: }
emit(foo, NTuple{2,Float16})

# COM: Make sure that we don't miss a function by accident (helps localize errors)
# CHECK-NOT: {
# CHECK-NOT: }
# CHECK: define
# TYPED-SAME: nonnull {} addrspace(10)* @jfptr
# OPAQUE-SAME: nonnull ptr addrspace(10) @jfptr
# CHECK-SAME: {

# CHECK: define
# CHECK-SAME: <2 x half> @julia_foo
# CHECK-SAME: {
# CHECK-NOT: define
# CHECK: [[FOO_RET:%.*]] call <2 x half> @foo(<2 x half> [[FOO_ARG:%.*]])
# CHECK-NOT: define
# CHECK: ret <2 x half>
# CHECK-NOT: define
# CHECK: }
emit(foo, NTuple{2,VecElement{Float16}})

# COM: Make sure that we don't miss a function by accident (helps localize errors)
# CHECK-NOT: {
# CHECK-NOT: }
# CHECK: define
# TYPED-SAME: nonnull {} addrspace(10)* @jfptr
# OPAQUE-SAME: nonnull ptr addrspace(10) @jfptr
# CHECK-SAME: {

# CHECK: define
# TYPED-SAME: i8 addrspace(3)* @julia_foo
# OPAQUE-SAME: ptr addrspace(3) @julia_foo
# CHECK-SAME: {
# CHECK-NOT: define
# TYPED: [[FOO_RET:%.*]] call i8 addrspace(3)* @foo(i8 addrspace(3)* [[FOO_ARG:%.*]])
# OPAQUE: [[FOO_RET:%.*]] call ptr addrspace(3) @foo(ptr addrspace(3) [[FOO_ARG:%.*]])
# CHECK-NOT: define
# TYPED: ret i8 addrspace(3)*
# OPAQUE: ret ptr addrspace(3)
# CHECK-NOT: define
# CHECK: }
emit(foo, Core.LLVMPtr{Float32,3})

# COM: Make sure that we don't miss a function by accident (helps localize errors)
# CHECK-NOT: {
# CHECK-NOT: }
# CHECK: define
# TYPED-SAME: nonnull {} addrspace(10)* @jfptr
# OPAQUE-SAME: nonnull ptr addrspace(10) @jfptr
# CHECK-SAME: {

# CHECK: define
# CHECK-SAME: [2 x i32] @julia_foo
# CHECK-SAME: {
# CHECK-NOT: define
# CHECK: [[FOO_RET:%.*]] call { i32, i32 } @foo({ i32, i32 } [[FOO_ARG:%.*]])
# CHECK-NOT: define
# CHECK: ret [2 x i32]
# CHECK-NOT: define
# CHECK: }
emit(foo, Foo)

# COM: Make sure that we don't miss a function by accident (helps localize errors)
# CHECK-NOT: {
# CHECK-NOT: }
# CHECK: define
# TYPED-SAME: nonnull {} addrspace(10)* @jfptr
# OPAQUE-SAME: nonnull ptr addrspace(10) @jfptr
# CHECK-SAME: {

# CHECK: define
# CHECK-SAME: <2 x half> @julia_bar
# TYPED-SAME: [2 x half]
# OPAQUE-SAME: ptr
# CHECK-SAME: {
# CHECK-NOT: define
# CHECK: ret <2 x half>
# CHECK-NOT: define
# CHECK: }
emit(bar, NTuple{2,Float16})

# COM: Make sure that we don't miss a function by accident (helps localize errors)
# CHECK-NOT: {
# CHECK-NOT: }
# CHECK: define
# TYPED-SAME: nonnull {} addrspace(10)* @jfptr
# OPAQUE-SAME: nonnull ptr addrspace(10) @jfptr
# CHECK-SAME: {

# COM: pointer-free arguments loaded from memory are not GC roots of the call
fmuladd(x, y, z) = ccall("llvm.fmuladd.f32", llvmcall, Float32, (Float32, Float32, Float32), x, y, z)
cmuladd(a::Complex{Float32}, b::Complex{Float32}) = fmuladd(real(a), imag(b), imag(a))
cmuladd_ref(r::Base.RefValue{Complex{Float32}}) = (c = r[]; fmuladd(real(c), imag(c), real(c)))
clen(a::Vector{UInt8}) = ccall(:strlen, Csize_t, (Ptr{UInt8},), a)

# CHECK: define
# CHECK-SAME: float @julia_cmuladd
# CHECK-SAME: {
# CHECK-NOT: jl_roots
# CHECK: call float @llvm.fmuladd.f32
# CHECK-NOT: jl_roots
# CHECK: ret float
# CHECK-NOT: define
# CHECK: }
emit(cmuladd, Complex{Float32}, Complex{Float32})

# COM: Make sure that we don't miss a function by accident (helps localize errors)
# CHECK-NOT: {
# CHECK-NOT: }
# CHECK: define
# TYPED-SAME: nonnull {} addrspace(10)* @jfptr
# OPAQUE-SAME: nonnull ptr addrspace(10) @jfptr
# CHECK-SAME: {

# CHECK: define
# CHECK-SAME: float @julia_cmuladd_ref
# CHECK-SAME: {
# CHECK-NOT: jl_roots
# CHECK: call float @llvm.fmuladd.f32
# CHECK-NOT: jl_roots
# CHECK: ret float
# CHECK-NOT: define
# CHECK: }
emit(cmuladd_ref, Base.RefValue{Complex{Float32}})

# COM: Make sure that we don't miss a function by accident (helps localize errors)
# CHECK-NOT: {
# CHECK-NOT: }
# CHECK: define
# TYPED-SAME: nonnull {} addrspace(10)* @jfptr
# OPAQUE-SAME: nonnull ptr addrspace(10) @jfptr
# CHECK-SAME: {

# COM: but the memory a pointer argument points into still is
# CHECK: define
# CHECK-SAME: @julia_clen
# CHECK-SAME: {
# CHECK-NOT: define
# CHECK: call i{{32|64}} @strlen_{{[0-9]+}}(
# CHECK-SAME: "jl_roots"
# CHECK-NOT: define
# CHECK: }
emit(clen, Vector{UInt8})

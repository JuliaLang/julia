# This file is a part of Julia. License is MIT: https://julialang.org/license

# RUN: julia --startup-file=no %s | opt -load libjulia.so -AllocOpt -S - | FileCheck %s

isz = sizeof(UInt) == 8 ? "i64" : "i32"

println("""
%jl_value_t = type opaque
@tag = external addrspace(10) global %jl_value_t
""")

# Test that the gc_preserve intrinsics are deleted directly.

# CHECK-LABEL: @preserve_branches
# CHECK: alloca i64
# CHECK: call %jl_value_t*** @julia.ptls_states()
# CHECK: L1:
# CHECK-NEXT: call void @llvm.lifetime.start{{.*}}(i64 8,
# CHECK-NEXT: @external_function()
# CHECK-NEXT: br i1 %b2, label %L2, label %L3

# CHECK: L2:
# CHECK-NOT: call void @llvm.lifetime.end{{.*}}(i64 8,
# CHECK: @external_function()
# CHECK-NEXT: br label %L3

# CHECK: L3:
# CHECK-NEXT: call void @llvm.lifetime.end{{.*}}(i64 8,
println("""
define void @preserve_branches(i8* %fptr, i1 %b, i1 %b2) {
  %ptls = call %jl_value_t*** @julia.ptls_states()
  %ptls_i8 = bitcast %jl_value_t*** %ptls to i8*
  br i1 %b, label %L1, label %L3

L1:
  %v = call noalias %jl_value_t addrspace(10)* @julia.gc_alloc_obj(i8* %ptls_i8, $isz 8, %jl_value_t addrspace(10)* @tag)
  %tok = call token (...) @llvm.julia.gc_preserve_begin(%jl_value_t addrspace(10)* %v)
  call void @external_function()
  br i1 %b2, label %L2, label %L3

L2:
  call void @external_function()
  br label %L3

L3:
  ret void
}
""")
# CHECK-LABEL: }

# CHECK-LABEL: @preserve_branches2
# CHECK: alloca i64
# CHECK: call %jl_value_t*** @julia.ptls_states()
# CHECK: L1:
# CHECK-NEXT: call void @llvm.lifetime.start{{.*}}(i64 8,
# CHECK-NEXT: @llvm.julia.gc_preserve_begin{{.*}}%jl_value_t addrspace(10)* %v2
# CHECK-NEXT: @external_function()
# CHECK-NEXT: br i1 %b2, label %L2, label %L3

# CHECK: L2:
# CHECK-NOT: call void @llvm.lifetime.end{{.*}}(i64 8,
# CHECK: @external_function()
# CHECK-NEXT: br label %L3

# CHECK: L3:
# CHECK-NEXT: call void @llvm.lifetime.end{{.*}}(i64 8,
println("""
define void @preserve_branches2(i8* %fptr, i1 %b, i1 %b2) {
  %ptls = call %jl_value_t*** @julia.ptls_states()
  %ptls_i8 = bitcast %jl_value_t*** %ptls to i8*
  %v2 = call %jl_value_t addrspace(10)* @external_function2()
  br i1 %b, label %L1, label %L3

L1:
  %v = call noalias %jl_value_t addrspace(10)* @julia.gc_alloc_obj(i8* %ptls_i8, $isz 8, %jl_value_t addrspace(10)* @tag)
  %tok = call token (...) @llvm.julia.gc_preserve_begin(%jl_value_t addrspace(10)* %v, %jl_value_t addrspace(10)* %v2)
  call void @external_function()
  br i1 %b2, label %L2, label %L3

L2:
  call void @external_function()
  br label %L3

L3:
  ret void
}
""")
# CHECK-LABEL: }

println("""
declare void @external_function()
declare %jl_value_t addrspace(10)* @external_function2()
declare %jl_value_t*** @julia.ptls_states()
declare noalias %jl_value_t addrspace(10)* @julia.gc_alloc_obj(i8*, $isz, %jl_value_t addrspace(10)*)
declare i64 @julia.pointer_from_objref(%jl_value_t addrspace(11)*)
declare void @llvm.memcpy.p11i8.p0i8.i64(i8 addrspace(11)* nocapture writeonly, i8* nocapture readonly, i64, i32, i1)
declare token @llvm.julia.gc_preserve_begin(...)
declare void @llvm.julia.gc_preserve_end(token)
""")

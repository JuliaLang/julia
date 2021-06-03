# This file is a part of Julia. License is MIT: https://julialang.org/license

# RUN: julia --startup-file=no %s | opt -load libjulia-internal%shlibext -AllocOpt -S - | FileCheck %s

isz = sizeof(UInt) == 8 ? "i64" : "i32"

println("""
@tag = external addrspace(10) global {}
""")

# Test that the gc_preserve intrinsics are deleted directly.

# CHECK-LABEL: @preserve_branches
# CHECK: call {}*** @julia.ptls_states()
# CHECK: L1:
# CHECK-NOT: @llvm.julia.gc_preserve_begin
# CHECK-NEXT: @external_function()
# CHECK-NEXT: br i1 %b2, label %L2, label %L3

# CHECK: L2:
# CHECK: @external_function()
# CHECK-NEXT: br label %L3

# CHECK: L3:
println("""
define void @preserve_branches(i8* %fptr, i1 %b, i1 %b2) {
  %pgcstack = call {}*** @julia.get_pgcstack()
  %ptls = call {}*** @julia.ptls_states()
  %ptls_i8 = bitcast {}*** %ptls to i8*
  br i1 %b, label %L1, label %L3

L1:
  %v = call noalias {} addrspace(10)* @julia.gc_alloc_obj(i8* %ptls_i8, $isz 8, {} addrspace(10)* @tag)
  %tok = call token (...) @llvm.julia.gc_preserve_begin({} addrspace(10)* %v)
  call void @external_function()
  br i1 %b2, label %L2, label %L3

L2:
  call void @external_function()
  br label %L3

L3:
  ret void
}
""")
# CHECK-LABEL: }{{$}}

# CHECK-LABEL: @preserve_branches2
# CHECK: call {}*** @julia.ptls_states()
# CHECK: L1:
# CHECK-NEXT: @llvm.julia.gc_preserve_begin{{.*}}{} addrspace(10)* %v2
# CHECK-NEXT: @external_function()
# CHECK-NEXT: br i1 %b2, label %L2, label %L3

# CHECK: L2:
# CHECK: @external_function()
# CHECK-NEXT: br label %L3

# CHECK: L3:
println("""
define void @preserve_branches2(i8* %fptr, i1 %b, i1 %b2) {
  %pgcstack = call {}*** @julia.get_pgcstack()
  %ptls = call {}*** @julia.ptls_states()
  %ptls_i8 = bitcast {}*** %ptls to i8*
  %v2 = call {} addrspace(10)* @external_function2()
  br i1 %b, label %L1, label %L3

L1:
  %v = call noalias {} addrspace(10)* @julia.gc_alloc_obj(i8* %ptls_i8, $isz 8, {} addrspace(10)* @tag)
  %tok = call token (...) @llvm.julia.gc_preserve_begin({} addrspace(10)* %v, {} addrspace(10)* %v2)
  call void @external_function()
  br i1 %b2, label %L2, label %L3

L2:
  call void @external_function()
  br label %L3

L3:
  ret void
}
""")
# CHECK-LABEL: }{{$}}

# CHECK-LABEL: @legal_int_types
# CHECK: alloca [12 x i8]
# CHECK-NOT: alloca i96
# CHECK: ret void
println("""
define void @legal_int_types() {
  %pgcstack = call {}*** @julia.get_pgcstack()
  %ptls = call {}*** @julia.ptls_states()
  %ptls_i8 = bitcast {}*** %ptls to i8*
  %var1 = call {} addrspace(10)* @julia.gc_alloc_obj(i8* %ptls_i8, $isz 12, {} addrspace(10)* @tag)
  %var2 = addrspacecast {} addrspace(10)* %var1 to {} addrspace(11)*
  %var3 = call {}* @julia.pointer_from_objref({} addrspace(11)* %var2)
  ret void
}
""")
# CHECK-LABEL: }{{$}}



println("""
declare void @external_function()
declare {} addrspace(10)* @external_function2()
declare {}*** @julia.ptls_states()
declare {}*** @julia.get_pgcstack()
declare noalias {} addrspace(10)* @julia.gc_alloc_obj(i8*, $isz, {} addrspace(10)*)
declare {}* @julia.pointer_from_objref({} addrspace(11)*)
declare void @llvm.memcpy.p11i8.p0i8.i64(i8 addrspace(11)* nocapture writeonly, i8* nocapture readonly, i64, i32, i1)
declare token @llvm.julia.gc_preserve_begin(...)
declare void @llvm.julia.gc_preserve_end(token)
""")

# CHECK-LABEL: @memref_collision
# CHECK: call {}*** @julia.ptls_states()
# CHECK-NOT: store {}
# CHECK: store i
# CHECK-NOT: store {}
# CHECK: L1:
# CHECK: load {}
# CHECK: L2:
# CHECK: load i
println("""
define void @memref_collision($isz %x) {
  %pgcstack = call {}*** @julia.get_pgcstack()
  %ptls = call {}*** @julia.ptls_states()
  %ptls_i8 = bitcast {}*** %ptls to i8*
  %v = call noalias {} addrspace(10)* @julia.gc_alloc_obj(i8* %ptls_i8, $isz 8, {} addrspace(10)* @tag)
  %v_p = bitcast {} addrspace(10)* %v to $isz addrspace(10)*
  store $isz %x, $isz addrspace(10)* %v_p
  br i1 0, label %L1, label %L2

L1:
  %v1 = bitcast {} addrspace(10)* %v to {} addrspace(10)* addrspace(10)*
  %v1_x = load {} addrspace(10)*, {} addrspace(10)* addrspace(10)* %v1
  ret void

L2:
  %v2 = bitcast {} addrspace(10)* %v to $isz addrspace(10)*
  %v2_x = load i64, i64 addrspace(10)* %v2
  ret void
}
""")
# CHECK-LABEL: }{{$}}

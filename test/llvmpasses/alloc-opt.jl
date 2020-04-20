# This file is a part of Julia. License is MIT: https://julialang.org/license

# RUN: julia --startup-file=no %s | opt -load libjulia%shlibext -AllocOpt -LateLowerGCFrame -FinalLowerGC -S - | FileCheck %s

isz = sizeof(UInt) == 8 ? "i64" : "i32"

println("""
%jl_value_t = type opaque
@tag = external addrspace(10) global %jl_value_t
""")

# CHECK-LABEL: @return_obj
# CHECK-NOT: @julia.gc_alloc_obj
# CHECK: %v = call noalias nonnull %jl_value_t addrspace(10)* @jl_gc_pool_alloc
# CHECK: store atomic %jl_value_t addrspace(10)* @tag, %jl_value_t addrspace(10)* addrspace(10)* {{.*}} unordered, align 8, !tbaa !0
println("""
define %jl_value_t addrspace(10)* @return_obj() {
  %ptls = call %jl_value_t*** @julia.ptls_states()
  %ptls_i8 = bitcast %jl_value_t*** %ptls to i8*
  %v = call noalias %jl_value_t addrspace(10)* @julia.gc_alloc_obj(i8* %ptls_i8, $isz 8, %jl_value_t addrspace(10)* @tag)
  ret %jl_value_t addrspace(10)* %v
}
""")
# CHECK-LABEL: }

# CHECK-LABEL: @return_load
# CHECK: alloca i64
# CHECK-NOT: @julia.gc_alloc_obj
# CHECK-NOT: @jl_gc_pool_alloc
# CHECK: call void @llvm.lifetime.start{{.*}}(i64 8, i8*
# CHECK-NOT: @tag
# CHECK-NOT: @llvm.lifetime.end
println("""
define i64 @return_load(i64 %i) {
  %ptls = call %jl_value_t*** @julia.ptls_states()
  %ptls_i8 = bitcast %jl_value_t*** %ptls to i8*
  %v = call noalias %jl_value_t addrspace(10)* @julia.gc_alloc_obj(i8* %ptls_i8, $isz 8, %jl_value_t addrspace(10)* @tag)
  %v64 = bitcast %jl_value_t addrspace(10)* %v to i64 addrspace(10)*
  %v64a11 = addrspacecast i64 addrspace(10)* %v64 to i64 addrspace(11)*
  store i64 %i, i64 addrspace(11)* %v64a11, align 16, !tbaa !4
  call void @external_function()
  %l = load i64, i64 addrspace(11)* %v64a11, align 16, !tbaa !4
  ret i64 %l
}
""")
# CHECK-LABEL: }

# CHECK-LABEL: @ccall_obj
# CHECK: call %jl_value_t*** @julia.ptls_states()
# CHECK-NOT: @julia.gc_alloc_obj
# CHECK: @jl_gc_pool_alloc
# CHECK: store atomic %jl_value_t addrspace(10)* @tag, %jl_value_t addrspace(10)* addrspace(10)* {{.*}} unordered, align 8, !tbaa !0
println("""
define void @ccall_obj(i8* %fptr) {
  %ptls = call %jl_value_t*** @julia.ptls_states()
  %ptls_i8 = bitcast %jl_value_t*** %ptls to i8*
  %v = call noalias %jl_value_t addrspace(10)* @julia.gc_alloc_obj(i8* %ptls_i8, $isz 8, %jl_value_t addrspace(10)* @tag)
  %f = bitcast i8* %fptr to void (%jl_value_t addrspace(10)*)*
  call void %f(%jl_value_t addrspace(10)* %v)
  ret void
}
""")
# CHECK-LABEL: }

# CHECK-LABEL: @ccall_ptr
# CHECK: alloca i64
# CHECK: call %jl_value_t*** @julia.ptls_states()
# CHECK-NOT: @julia.gc_alloc_obj
# CHECK-NOT: @jl_gc_pool_alloc
# CHECK: call void @llvm.lifetime.start{{.*}}(i64 8, i8*
# CHECK: %f = bitcast i8* %fptr to void (i8*)*
# Currently the GC frame lowering pass strips away all operand bundles
# CHECK-NEXT: call void %f(i8*
# CHECK-NEXT: ret void
println("""
define void @ccall_ptr(i8* %fptr) {
  %ptls = call %jl_value_t*** @julia.ptls_states()
  %ptls_i8 = bitcast %jl_value_t*** %ptls to i8*
  %v = call noalias %jl_value_t addrspace(10)* @julia.gc_alloc_obj(i8* %ptls_i8, $isz 8, %jl_value_t addrspace(10)* @tag)
  %va = addrspacecast %jl_value_t addrspace(10)* %v to %jl_value_t addrspace(11)*
  %ptrj = call %jl_value_t* @julia.pointer_from_objref(%jl_value_t addrspace(11)* %va)
  %ptr = bitcast %jl_value_t* %ptrj to i8*
  %f = bitcast i8* %fptr to void (i8*)*
  call void %f(i8* %ptr) [ "jl_roots"(%jl_value_t addrspace(10)* %v), "unknown_bundle"(i8* %ptr) ]
  ret void
}
""")
# CHECK-LABEL: }

# CHECK-LABEL: @ccall_unknown_bundle
# CHECK: call %jl_value_t*** @julia.ptls_states()
# CHECK-NOT: @julia.gc_alloc_obj
# CHECK: @jl_gc_pool_alloc
# CHECK: store atomic %jl_value_t addrspace(10)* @tag, %jl_value_t addrspace(10)* addrspace(10)* {{.*}} unordered, align 8, !tbaa !0
println("""
define void @ccall_unknown_bundle(i8* %fptr) {
  %ptls = call %jl_value_t*** @julia.ptls_states()
  %ptls_i8 = bitcast %jl_value_t*** %ptls to i8*
  %v = call noalias %jl_value_t addrspace(10)* @julia.gc_alloc_obj(i8* %ptls_i8, $isz 8, %jl_value_t addrspace(10)* @tag)
  %va = addrspacecast %jl_value_t addrspace(10)* %v to %jl_value_t addrspace(11)*
  %ptrj = call %jl_value_t* @julia.pointer_from_objref(%jl_value_t addrspace(11)* %va)
  %ptr = bitcast %jl_value_t* %ptrj to i8*
  %f = bitcast i8* %fptr to void (i8*)*
  call void %f(i8* %ptr) [ "jl_not_jl_roots"(%jl_value_t addrspace(10)* %v) ]
  ret void
}
""")
# CHECK-LABEL: }

# CHECK-LABEL: @lifetime_branches
# CHECK: alloca i64
# CHECK: call %jl_value_t*** @julia.ptls_states()
# CHECK: L1:
# CHECK-NEXT: call void @llvm.lifetime.start{{.*}}(i64 8,
# CHECK: %f = bitcast i8* %fptr to void (i8*)*
# CHECK-NEXT: call void %f(i8*
# CHECK-NEXT: br i1 %b2, label %L2, label %L3

# CHECK: L2:
# CHECK-NEXT: %f2 = bitcast i8* %fptr to void (%jl_value_t*)*
# CHECK-NEXT: call void @llvm.lifetime.end{{.*}}(i64 8,
# CHECK-NEXT: call void %f2(%jl_value_t* null)

# CHECK: L3:
# CHECK-NEXT: call void @llvm.lifetime.end{{.*}}(i64 8,
println("""
define void @lifetime_branches(i8* %fptr, i1 %b, i1 %b2) {
  %ptls = call %jl_value_t*** @julia.ptls_states()
  %ptls_i8 = bitcast %jl_value_t*** %ptls to i8*
  br i1 %b, label %L1, label %L3

L1:
  %v = call noalias %jl_value_t addrspace(10)* @julia.gc_alloc_obj(i8* %ptls_i8, $isz 8, %jl_value_t addrspace(10)* @tag)
  %va = addrspacecast %jl_value_t addrspace(10)* %v to %jl_value_t addrspace(11)*
  %ptrj = call %jl_value_t* @julia.pointer_from_objref(%jl_value_t addrspace(11)* %va)
  %ptr = bitcast %jl_value_t* %ptrj to i8*
  %f = bitcast i8* %fptr to void (i8*)*
  call void %f(i8* %ptr) [ "jl_roots"(%jl_value_t addrspace(10)* %v) ]
  br i1 %b2, label %L2, label %L3

L2:
  %f2 = bitcast i8* %fptr to void (%jl_value_t*)*
  call void %f2(%jl_value_t* null)
  br label %L3

L3:
  ret void
}
""")
# CHECK-LABEL: }

# CHECK-LABEL: @object_field
# CHECK: call %jl_value_t*** @julia.ptls_states()
# CHECK-NOT: @julia.gc_alloc_obj
# CHECK-NOT: @jl_gc_pool_alloc
# CHECK-NOT: store %jl_value_t addrspace(10)* @tag, %jl_value_t addrspace(10)* addrspace(10)* {{.*}}, align 8, !tbaa !0
println("""
define void @object_field(%jl_value_t addrspace(10)* %field) {
  %ptls = call %jl_value_t*** @julia.ptls_states()
  %ptls_i8 = bitcast %jl_value_t*** %ptls to i8*
  %v = call noalias %jl_value_t addrspace(10)* @julia.gc_alloc_obj(i8* %ptls_i8, $isz 8, %jl_value_t addrspace(10)* @tag)
  %va = addrspacecast %jl_value_t addrspace(10)* %v to %jl_value_t addrspace(11)*
  %vab = bitcast %jl_value_t addrspace(11)* %va to %jl_value_t addrspace(10)* addrspace(11)*
  store %jl_value_t addrspace(10)* %field, %jl_value_t addrspace(10)* addrspace(11)* %vab, align 8
  ret void
}
""")
# CHECK-LABEL: }

# CHECK-LABEL: @memcpy_opt
# CHECK: alloca i128, align 16
# CHECK: call %jl_value_t*** @julia.ptls_states()
# CHECK-NOT: @julia.gc_alloc_obj
# CHECK-NOT: @jl_gc_pool_alloc
# CHECK: call void @llvm.memcpy.p0i8.p0i8.i64
println("""
define void @memcpy_opt(i8* %v22) {
top:
  %v6 = call %jl_value_t*** @julia.ptls_states()
  %v18 = bitcast %jl_value_t*** %v6 to i8*
  %v19 = call noalias %jl_value_t addrspace(10)* @julia.gc_alloc_obj(i8* %v18, $isz 16, %jl_value_t addrspace(10)* @tag)
  %v20 = bitcast %jl_value_t addrspace(10)* %v19 to i8 addrspace(10)*
  %v21 = addrspacecast i8 addrspace(10)* %v20 to i8 addrspace(11)*
  call void @llvm.memcpy.p11i8.p0i8.i64(i8 addrspace(11)* %v21, i8* %v22, i64 16, i32 8, i1 false)
  ret void
}
""")
# CHECK-LABEL: }

# CHECK-LABEL: @preserve_opt
# CHECK: call %jl_value_t*** @julia.ptls_states()
# CHECK-NOT: @julia.gc_alloc_obj
# CHECK-NOT: @jl_gc_pool_alloc
# CHECK-NOT: @llvm.lifetime.end
# CHECK: @external_function
println("""
define void @preserve_opt(i8* %v22) {
top:
  %v6 = call %jl_value_t*** @julia.ptls_states()
  %v18 = bitcast %jl_value_t*** %v6 to i8*
  %v19 = call noalias %jl_value_t addrspace(10)* @julia.gc_alloc_obj(i8* %v18, $isz 16, %jl_value_t addrspace(10)* @tag)
  %v20 = bitcast %jl_value_t addrspace(10)* %v19 to i8 addrspace(10)*
  %v21 = addrspacecast i8 addrspace(10)* %v20 to i8 addrspace(11)*
  %tok = call token (...) @llvm.julia.gc_preserve_begin(%jl_value_t addrspace(10)* %v19)
  call void @external_function()
  call void @llvm.julia.gc_preserve_end(token %tok)
  call void @external_function()
  ret void
}
""")
# CHECK-LABEL: }

# CHECK-LABEL: @preserve_branches
# CHECK: call %jl_value_t*** @julia.ptls_states()
# CHECK: L1:
# CHECK-NEXT: @external_function()
# CHECK-NEXT: br i1 %b2, label %L2, label %L3

# CHECK: L2:
# CHECK: @external_function()
# CHECK-NEXT: br label %L3

# CHECK: L3:
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

# CHECK: declare noalias nonnull %jl_value_t addrspace(10)* @jl_gc_pool_alloc(i8*,
# CHECK: declare noalias nonnull %jl_value_t addrspace(10)* @jl_gc_big_alloc(i8*,
println("""
declare void @external_function()
declare %jl_value_t*** @julia.ptls_states()
declare noalias nonnull %jl_value_t addrspace(10)* @julia.gc_alloc_obj(i8*, $isz, %jl_value_t addrspace(10)*)
declare %jl_value_t* @julia.pointer_from_objref(%jl_value_t addrspace(11)*)
declare void @llvm.memcpy.p11i8.p0i8.i64(i8 addrspace(11)* nocapture writeonly, i8* nocapture readonly, i64, i32, i1)
declare token @llvm.julia.gc_preserve_begin(...)
declare void @llvm.julia.gc_preserve_end(token)

!0 = !{!1, !1, i64 0}
!1 = !{!"jtbaa_tag", !2, i64 0}
!2 = !{!"jtbaa_data", !3, i64 0}
!3 = !{!"jtbaa"}
!4 = !{!5, !5, i64 0}
!5 = !{!"jtbaa_mutab", !6, i64 0}
!6 = !{!"jtbaa_value", !2, i64 0}
""")

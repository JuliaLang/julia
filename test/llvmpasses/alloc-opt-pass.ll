; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='function(AllocOpt)' -S %s | FileCheck %s --check-prefixes=CHECK,OPAQUE

@tag = external addrspace(10) global {}

; Test that the gc_preserve intrinsics are deleted directly.

; CHECK-LABEL: @preserve_branches
; OPAQUE: call ptr @julia.ptls_states()
; CHECK: L1:
; CHECK-NOT: @llvm.julia.gc_preserve_begin
; CHECK-NEXT: @external_function()
; CHECK-NEXT: br i1 %b2, label %L2, label %L3

; CHECK: L2:
; CHECK: @external_function()
; CHECK-NEXT: br label %L3

; CHECK: L3:
define void @preserve_branches(ptr %fptr, i1 %b, i1 %b2) {
  %pgcstack = call ptr @julia.get_pgcstack()
  %ptls = call ptr @julia.ptls_states()
  %ptls_i8 = bitcast ptr %ptls to ptr
  br i1 %b, label %L1, label %L3

L1:                                               ; preds = %0
  %v = call noalias ptr addrspace(10) @julia.gc_alloc_obj(ptr %ptls_i8, i64 8, ptr addrspace(10) @tag)
  %tok = call token (...) @llvm.julia.gc_preserve_begin(ptr addrspace(10) nonnull %v)
  call void @external_function()
  br i1 %b2, label %L2, label %L3

L2:                                               ; preds = %L1
  call void @external_function()
  br label %L3

L3:                                               ; preds = %L2, %L1, %0
  ret void
}
; CHECK-LABEL: }{{$}}

; CHECK-LABEL: @preserve_branches2
; OPAQUE: call ptr @julia.ptls_states()
; CHECK: L1:
; OPAQUE-NEXT: @llvm.julia.gc_preserve_begin{{.*}}ptr addrspace(10) %v2
; CHECK-NEXT: @external_function()
; CHECK-NEXT: br i1 %b2, label %L2, label %L3

; CHECK: L2:
; CHECK: @external_function()
; CHECK-NEXT: br label %L3

; CHECK: L3:
define void @preserve_branches2(ptr %fptr, i1 %b, i1 %b2) {
  %pgcstack = call ptr @julia.get_pgcstack()
  %ptls = call ptr @julia.ptls_states()
  %ptls_i8 = bitcast ptr %ptls to ptr
  %v2 = call ptr addrspace(10) @external_function2()
  br i1 %b, label %L1, label %L3

L1:                                               ; preds = %0
  %v = call noalias ptr addrspace(10) @julia.gc_alloc_obj(ptr %ptls_i8, i64 8, ptr addrspace(10) @tag)
  %tok = call token (...) @llvm.julia.gc_preserve_begin(ptr addrspace(10) %v, ptr addrspace(10) nonnull %v2)
  call void @external_function()
  br i1 %b2, label %L2, label %L3

L2:                                               ; preds = %L1
  call void @external_function()
  br label %L3

L3:                                               ; preds = %L2, %L1, %0
  ret void
}
; CHECK-LABEL: }{{$}}

declare void @external_function()

declare ptr addrspace(10) @external_function2()


; CHECK-LABEL: @legal_int_types
; CHECK: alloca [12 x i8]
; CHECK-NOT: alloca i96
; CHECK: call void @llvm.memset.p0.i64(ptr align 16 %var1,
; CHECK: ret void
define void @legal_int_types() {
  %pgcstack = call ptr @julia.get_pgcstack()
  %ptls = call ptr @julia.ptls_states()
  %ptls_i8 = bitcast ptr %ptls to ptr
  %var1 = call ptr addrspace(10) @julia.gc_alloc_obj(ptr %ptls_i8, i64 12, ptr addrspace(10) @tag)
  %var2 = addrspacecast ptr addrspace(10) %var1 to ptr addrspace(11)
  %var3 = call ptr @julia.pointer_from_objref(ptr addrspace(11) %var2)
  ret void
}
; CHECK-LABEL: }{{$}}


; CHECK-LABEL: @memref_collision
; OPAQUE: call ptr @julia.ptls_states()
; OPAQUE-NOT: store ptr
; CHECK: store i
; OPAQUE-NOT: store ptr
; CHECK: L1:
; OPAQUE: load ptr
; CHECK: L2:
; CHECK: load i
define void @memref_collision(i64 %x) {
  %pgcstack = call ptr @julia.get_pgcstack()
  %ptls = call ptr @julia.ptls_states()
  %ptls_i8 = bitcast ptr %ptls to ptr
  %v = call noalias ptr addrspace(10) @julia.gc_alloc_obj(ptr %ptls_i8, i64 8, ptr addrspace(10) @tag)
  %v_p = bitcast ptr addrspace(10) %v to ptr addrspace(10)
  store i64 %x, ptr addrspace(10) %v_p, align 4
  br i1 false, label %L1, label %L2

L1:                                               ; preds = %0
  %v1 = bitcast ptr addrspace(10) %v to ptr addrspace(10)
  %v1_x = load ptr addrspace(10), ptr addrspace(10) %v1, align 8
  ret void

L2:                                               ; preds = %0
  %v2 = bitcast ptr addrspace(10) %v to ptr addrspace(10)
  %v2_x = load i64, ptr addrspace(10) %v2, align 4
  ret void
}

; CHECK-LABEL: }{{$}}

; CHECK-LABEL: @lifetime_no_preserve_end
; CHECK: alloca
; CHECK-NOT: call token(...) @llvm.julia.gc_preserve_begin
; CHECK: call void @llvm.lifetime.start
; CHECK: call void @llvm.memset.p0.i64(ptr align 16 %v,
; CHECK-NOT: call void @llvm.lifetime.end
define void @lifetime_no_preserve_end(ptr noalias nocapture noundef nonnull sret({}) %0) {
  %pgcstack = call ptr @julia.get_pgcstack()
  %ptls = call ptr @julia.ptls_states()
  %ptls_i8 = bitcast ptr %ptls to ptr
  %v = call noalias ptr addrspace(10) @julia.gc_alloc_obj(ptr %ptls_i8, i64 8, ptr addrspace(10) @tag)
  %token = call token (...) @llvm.julia.gc_preserve_begin(ptr addrspace(10) %v)
  %v_derived = addrspacecast ptr addrspace(10) %v to ptr addrspace(11)
  %ptr = call nonnull ptr @julia.pointer_from_objref(ptr addrspace(11) %v_derived)
  %ptr_raw = bitcast ptr %ptr to ptr
  call void @external_function()
  %ret_raw = bitcast ptr %0 to ptr
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %ret_raw, ptr align 8 %ptr_raw, i64 0, i1 false)
  %ret_raw2 = bitcast ptr %0 to ptr
  ret void
}
; CHECK-LABEL: }{{$}}


; CHECK-LABEL: @initializers
; CHECK: alloca [1 x i8]
; CHECK-DAG: alloca [2 x i8]
; CHECK-DAG: alloca [3 x i8]
; CHECK-DAG: call void @llvm.memset.p0.i64(ptr align 1 %var1,
; CHECK-DAG: call void @llvm.memset.p0.i64(ptr align 4 %var7,
; CHECK: ret void
define void @initializers() {
  %pgcstack = call ptr @julia.get_pgcstack()
  %ptls = call ptr @julia.ptls_states()
  %ptls_i8 = bitcast ptr %ptls to ptr
  %var1 = call ptr addrspace(10) @julia.gc_alloc_obj(ptr %ptls_i8, i64 1, ptr addrspace(10) @tag) #4
  %var2 = addrspacecast ptr addrspace(10) %var1 to ptr addrspace(11)
  %var3 = call ptr @julia.pointer_from_objref(ptr addrspace(11) %var2)
  %var4 = call ptr addrspace(10) @julia.gc_alloc_obj(ptr %ptls_i8, i64 2, ptr addrspace(10) @tag) #7
  %var5 = addrspacecast ptr addrspace(10) %var4 to ptr addrspace(11)
  %var6 = call ptr @julia.pointer_from_objref(ptr addrspace(11) %var5)
  %var7 = call ptr addrspace(10) @julia.gc_alloc_obj(ptr %ptls_i8, i64 3, ptr addrspace(10) @tag) #1
  %var8 = addrspacecast ptr addrspace(10) %var7 to ptr addrspace(11)
  %var9 = call ptr @julia.pointer_from_objref(ptr addrspace(11) %var8)
  ret void
}
; CHECK-LABEL: }{{$}}

; Test that the pass handles dead basic blocks with references to the allocation
; CHECK-LABEL: @nopreds
; CHECK: alloca i8, i64 0, align 1
; CHECK: call void @llvm.lifetime.start
define swiftcc { ptr addrspace(10), i8 } @nopreds() {
top:
  %0 = call ptr addrspace(10) @julia.gc_alloc_obj(ptr null, i64 0, ptr addrspace(10) null)
  %1 = addrspacecast ptr addrspace(10) %0 to ptr addrspace(11)
  br label %common.ret

common.ret:                                       ; preds = %union_move9, %top
  ret { ptr addrspace(10), i8 } zeroinitializer

union_move9:                                      ; No predecessors!
  call void @llvm.memcpy.p0.p11.i64(ptr null, ptr addrspace(11) %1, i64 0, i1 false)
  br label %common.ret
}
; CHECK-LABEL: }{{$}}

@0 = private unnamed_addr constant ptr inttoptr (i64 4373799056 to ptr), !julia.constgv !0
@1 = private unnamed_addr constant i64 0, align 8

; CHECK-LABEL: @cmpxchg
; CHECK: alloca 
; CHECK: alloca
; CHECK:  %20 = cmpxchg ptr %2,
define swiftcc i64 @"cmpxchg"(ptr nonnull swiftself %0) #0 {
  %2 = alloca i64, align 16
  %3 = call ptr @julia.get_pgcstack()
  %4 = getelementptr inbounds i8, ptr %3, i32 -152
  %5 = getelementptr inbounds i8, ptr %4, i32 168
  %6 = load ptr, ptr %5, align 8, !tbaa !4
  %7 = getelementptr inbounds i8, ptr %6, i32 16
  %8 = load ptr, ptr %7, align 8, !tbaa !8, !invariant.load !0
  fence syncscope("singlethread") seq_cst
  call void @julia.safepoint(ptr %8)
  fence syncscope("singlethread") seq_cst
  %9 = load ptr, ptr @0, align 8, !tbaa !8, !invariant.load !0, !alias.scope !10, !noalias !13, !nonnull !0, !dereferenceable !18, !align !19
  %10 = ptrtoint ptr %9 to i64
  %11 = inttoptr i64 %10 to ptr
  %12 = getelementptr inbounds i8, ptr %3, i32 -152
  %13 = addrspacecast ptr %11 to ptr addrspace(10)
  call void @llvm.lifetime.start.p0(i64 8, ptr %2)
  %14 = call noalias nonnull align 8 dereferenceable(8) ptr addrspace(10) @julia.gc_alloc_obj(ptr %12, i64 8, ptr addrspace(10) %13) #7
  %15 = addrspacecast ptr addrspace(10) %14 to ptr addrspace(11)
  call void @llvm.memcpy.p11.p0.i64(ptr addrspace(11) align 8 %15, ptr align 8 @1, i64 8, i1 false), !tbaa !20, !alias.scope !23, !noalias !24
  %16 = addrspacecast ptr addrspace(10) %14 to ptr addrspace(11)
  %17 = load atomic i64, ptr addrspace(11) %16 monotonic, align 8, !tbaa !25, !alias.scope !23, !noalias !24
  br label %19

18:                                               ; preds = %19
  ret i64 %21

19:                                               ; preds = %19, %1
  %20 = phi i64 [ %17, %1 ], [ %23, %19 ]
  %21 = call swiftcc i64 @"jlsys_+_47"(ptr nonnull swiftself %3, i64 signext %20, i64 signext 1)
  %22 = cmpxchg ptr addrspace(11) %16, i64 %20, i64 %21 seq_cst monotonic, align 8, !tbaa !25, !alias.scope !23, !noalias !24
  %23 = extractvalue { i64, i1 } %22, 0
  %24 = extractvalue { i64, i1 } %22, 1
  br i1 %24, label %18, label %19
}

; CHECK-LABEL: }{{$}}
; CHECK-LABEL: @atomicrmw
; CHECK: alloca
; CHECK: alloca
; CHECK: atomicrmw xchg ptr %2,
define swiftcc i64 @"atomicrmw"(ptr nonnull swiftself %0) #0 {
  %2 = alloca i64, align 16
  %3 = call ptr @julia.get_pgcstack()
  %4 = getelementptr inbounds i8, ptr %3, i32 -152
  %5 = getelementptr inbounds i8, ptr %4, i32 168
  %6 = load ptr, ptr %5, align 8, !tbaa !4
  %7 = getelementptr inbounds i8, ptr %6, i32 16
  %8 = load ptr, ptr %7, align 8, !tbaa !8, !invariant.load !0
  fence syncscope("singlethread") seq_cst
  call void @julia.safepoint(ptr %8)
  fence syncscope("singlethread") seq_cst
  %9 = load ptr, ptr @0, align 8, !tbaa !8, !invariant.load !0, !alias.scope !10, !noalias !13, !nonnull !0, !dereferenceable !18, !align !19
  %10 = ptrtoint ptr %9 to i64
  %11 = inttoptr i64 %10 to ptr
  %12 = getelementptr inbounds i8, ptr %3, i32 -152
  %13 = addrspacecast ptr %11 to ptr addrspace(10)
  call void @llvm.lifetime.start.p0(i64 8, ptr %2)
  %14 = call noalias nonnull align 8 dereferenceable(8) ptr addrspace(10) @julia.gc_alloc_obj(ptr %12, i64 8, ptr addrspace(10) %13) #7
  %15 = addrspacecast ptr addrspace(10) %14 to ptr addrspace(11)
  call void @llvm.memcpy.p11.p0.i64(ptr addrspace(11) align 8 %15, ptr align 8 @1, i64 8, i1 false), !tbaa !20, !alias.scope !23, !noalias !24
  %16 = addrspacecast ptr addrspace(10) %14 to ptr addrspace(11)
  %17 = load atomic i64, ptr addrspace(11) %16 monotonic, align 8, !tbaa !25, !alias.scope !23, !noalias !24
  %18 = call swiftcc i64 @"jlsys_+_47"(ptr nonnull swiftself %3, i64 signext %17, i64 signext 1)
  %19 = atomicrmw xchg ptr addrspace(11) %16, i64 %18 seq_cst, align 8, !tbaa !25, !alias.scope !23, !noalias !24                                    ; preds = %19
  ret i64 %19
}

declare ptr @julia.ptls_states()

declare ptr @julia.pointer_from_objref(ptr addrspace(11))

declare token @llvm.julia.gc_preserve_begin(...)

declare void @llvm.julia.gc_preserve_end(token)

declare ptr @julia.get_pgcstack()

; Function Attrs: mustprogress nounwind willreturn memory(inaccessiblemem: readwrite)
declare nonnull align 8 dereferenceable(8) ptr addrspace(10) @ijl_box_int64(i64 signext) #2

; Function Attrs: memory(argmem: readwrite, inaccessiblemem: readwrite)
declare void @julia.safepoint(ptr) #3

; Function Attrs: mustprogress nounwind willreturn allockind("alloc") allocsize(1) memory(argmem: read, inaccessiblemem: readwrite)
declare noalias nonnull ptr addrspace(10) @julia.gc_alloc_obj(ptr, i64, ptr addrspace(10)) #4

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p11.p0.i64(ptr addrspace(11) noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #5

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p11.i64(ptr noalias nocapture writeonly, ptr addrspace(11) noalias nocapture readonly, i64, i1 immarg) #5

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #5

declare swiftcc i64 @"jlsys_+_47"(ptr nonnull swiftself, i64 signext, i64 signext) #0

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.start.p0(i64 immarg, ptr nocapture) #6

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.end.p0(i64 immarg, ptr nocapture) #6

attributes #0 = { "probe-stack"="inline-asm" }
attributes #1 = { nounwind willreturn allockind("alloc,zeroed") allocsize(1) memory(argmem: read, inaccessiblemem: readwrite) }
attributes #2 = { mustprogress nounwind willreturn memory(inaccessiblemem: readwrite) }
attributes #3 = { memory(argmem: readwrite, inaccessiblemem: readwrite) }
attributes #4 = { mustprogress nounwind willreturn allockind("alloc") allocsize(1) memory(argmem: read, inaccessiblemem: readwrite) }
attributes #5 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }
attributes #6 = { nocallback nofree nosync nounwind willreturn memory(argmem: readwrite) }
attributes #7 = { nounwind willreturn allockind("alloc,uninitialized") allocsize(1) memory(argmem: read, inaccessiblemem: readwrite) }
attributes #8 = { nounwind willreturn memory(inaccessiblemem: readwrite) }

!llvm.module.flags = !{!1, !2, !3}

!0 = !{}
!1 = !{i32 2, !"Dwarf Version", i32 4}
!2 = !{i32 2, !"Debug Info Version", i32 3}
!3 = !{i32 2, !"julia.optlevel", i32 2}
!4 = !{!5, !5, i64 0}
!5 = !{!"jtbaa_gcframe", !6, i64 0}
!6 = !{!"jtbaa", !7, i64 0}
!7 = !{!"jtbaa"}
!8 = !{!9, !9, i64 0, i64 1}
!9 = !{!"jtbaa_const", !6, i64 0}
!10 = !{!11}
!11 = !{!"jnoalias_const", !12}
!12 = !{!"jnoalias"}
!13 = !{!14, !15, !16, !17}
!14 = !{!"jnoalias_gcframe", !12}
!15 = !{!"jnoalias_stack", !12}
!16 = !{!"jnoalias_data", !12}
!17 = !{!"jnoalias_typemd", !12}
!18 = !{i64 56}
!19 = !{i64 16}
!20 = !{!21, !21, i64 0}
!21 = !{!"jtbaa_value", !22, i64 0}
!22 = !{!"jtbaa_data", !6, i64 0}
!23 = !{!16}
!24 = !{!14, !15, !17, !11}
!25 = !{!26, !26, i64 0}
!26 = !{!"jtbaa_mutab", !21, i64 0}


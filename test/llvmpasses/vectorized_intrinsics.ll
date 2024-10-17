; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='loop-vectorize' -force-vector-width=2 -S %s | FileCheck %s --check-prefixes=CHECKV2
; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='loop-vectorize' -force-vector-width=4 -S %s | FileCheck %s --check-prefixes=CHECKV4
; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='loop-vectorize' -force-vector-width=8 -S %s | FileCheck %s --check-prefixes=CHECKV8
; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='loop-vectorize' -force-vector-width=16 -S %s | FileCheck %s --check-prefixes=CHECKV16

source_filename = "vectorized_intrinsics.ll"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128-ni:10:11:12:13"
target triple = "x86_64-unknown-linux-gnu"

@jl_nothing = external constant ptr
@llvm.compiler.used = appending global [4 x ptr] [ptr @julia.gc_loaded.v2, ptr @julia.gc_loaded.v4, ptr @julia.gc_loaded.v8, ptr @julia.gc_loaded.v16], section "llvm.metadata"

define void @"julia_#5_1643"(ptr addrspace(11) nocapture noundef nonnull readonly align 8 dereferenceable(16) %"v::FixedSizeArray", ptr nocapture readonly %.roots.v) #0 {
top:
  %pgcstack = call ptr @julia.get_pgcstack()
  %memoryref_mem = load ptr addrspace(10), ptr %.roots.v, align 8, !tbaa !3, !alias.scope !7, !noalias !10
  %0 = getelementptr inbounds i8, ptr addrspace(11) %"v::FixedSizeArray", i64 8
  %.unbox = load i64, ptr addrspace(11) %0, align 8, !tbaa !15, !invariant.load !17, !alias.scope !18, !noalias !19
  %1 = icmp slt i64 %.unbox, 1
  br i1 %1, label %L48, label %L13.preheader15

L13.preheader15:                                  ; preds = %top
  %2 = addrspacecast ptr addrspace(10) %memoryref_mem to ptr addrspace(11)
  %memory_data_ptr = getelementptr inbounds { i64, ptr }, ptr addrspace(11) %2, i64 0, i32 1
  br label %L30

L30:                                              ; preds = %L30, %L13.preheader15
  %value_phi3 = phi i64 [ %5, %L30 ], [ 1, %L13.preheader15 ]
  %memoryref_data = load ptr, ptr addrspace(11) %memory_data_ptr, align 8, !tbaa !15, !invariant.load !17, !alias.scope !18, !noalias !19, !nonnull !17
  %memoryref_offset = shl i64 %value_phi3, 3
  %3 = call ptr addrspace(13) @julia.gc_loaded(ptr addrspace(10) %memoryref_mem, ptr %memoryref_data)
  ; CHECKV2: call <2 x ptr addrspace(13)> @julia.gc_loaded.v2(ptr addrspace(10) %memoryref_mem, <2 x ptr>
  ; CHECKV4: call <4 x ptr addrspace(13)> @julia.gc_loaded.v4(ptr addrspace(10) %memoryref_mem, <4 x ptr>
  ; CHECKV8: call <8 x ptr addrspace(13)> @julia.gc_loaded.v8(ptr addrspace(10) %memoryref_mem, <8 x ptr>
  ; CHECKV16: call <16 x ptr addrspace(13)> @julia.gc_loaded.v16(ptr addrspace(10) %memoryref_mem, <16 x ptr>
  %4 = getelementptr i8, ptr addrspace(13) %3, i64 %memoryref_offset
  %memoryref_data6 = getelementptr i8, ptr addrspace(13) %4, i64 -8
  store i64 4607182418800017408, ptr addrspace(13) %memoryref_data6, align 8, !tbaa !20, !alias.scope !22, !noalias !23
  %5 = add nuw i64 %value_phi3, 1
  %6 = icmp ult i64 %value_phi3, %.unbox
  br i1 %6, label %L30, label %L48.loopexit

L48.loopexit:                                     ; preds = %L30
  br label %L48

L48:                                              ; preds = %L48.loopexit, %top
  ret void
}

declare ptr @julia.get_pgcstack()

; Function Attrs: norecurse nosync nounwind speculatable willreturn memory(none)
declare noundef nonnull ptr addrspace(13) @julia.gc_loaded(ptr addrspace(10) nocapture noundef nonnull readnone, ptr noundef nonnull readnone) #1

; Function Attrs: norecurse nosync nounwind speculatable willreturn memory(none)
declare noundef <2 x ptr addrspace(13)> @julia.gc_loaded.v2(ptr addrspace(10) nocapture noundef nonnull readnone, <2 x ptr> noundef) #2

; Function Attrs: norecurse nosync nounwind speculatable willreturn memory(none)
declare noundef <4 x ptr addrspace(13)> @julia.gc_loaded.v4(ptr addrspace(10) nocapture noundef nonnull readnone, <4 x ptr> noundef) #2

; Function Attrs: norecurse nosync nounwind speculatable willreturn memory(none)
declare noundef <8 x ptr addrspace(13)> @julia.gc_loaded.v8(ptr addrspace(10) nocapture noundef nonnull readnone, <8 x ptr> noundef) #2

; Function Attrs: norecurse nosync nounwind speculatable willreturn memory(none)
declare noundef <16 x ptr addrspace(13)> @julia.gc_loaded.v16(ptr addrspace(10) nocapture noundef nonnull readnone, <16 x ptr> noundef) #2

attributes #0 = { "frame-pointer"="all" "julia.fsig"="var\22#5\22(FixedSizeArrays.FixedSizeArray{Float64, 1, Memory{Float64}})" "probe-stack"="inline-asm" }
attributes #1 = { norecurse nosync nounwind speculatable willreturn memory(none) "vector-function-abi-variant"="_ZGV_LLVM_N2uv_julia.gc_loaded(julia.gc_loaded.v2),_ZGV_LLVM_N4uv_julia.gc_loaded(julia.gc_loaded.v4),_ZGV_LLVM_N8uv_julia.gc_loaded(julia.gc_loaded.v8),_ZGV_LLVM_N16uv_julia.gc_loaded(julia.gc_loaded.v16)" }
attributes #2 = { norecurse nosync nounwind speculatable willreturn memory(none) }

!llvm.module.flags = !{!0, !1, !2}

!0 = !{i32 2, !"Dwarf Version", i32 4}
!1 = !{i32 2, !"Debug Info Version", i32 3}
!2 = !{i32 2, !"julia.debug_level", i32 2}
!3 = !{!4, !4, i64 0}
!4 = !{!"jtbaa_gcframe", !5, i64 0}
!5 = !{!"jtbaa", !6, i64 0}
!6 = !{!"jtbaa"}
!7 = !{!8}
!8 = !{!"jnoalias_gcframe", !9}
!9 = !{!"jnoalias"}
!10 = !{!11, !12, !13, !14}
!11 = !{!"jnoalias_stack", !9}
!12 = !{!"jnoalias_data", !9}
!13 = !{!"jnoalias_typemd", !9}
!14 = !{!"jnoalias_const", !9}
!15 = !{!16, !16, i64 0, i64 1}
!16 = !{!"jtbaa_const", !5, i64 0}
!17 = !{}
!18 = !{!14}
!19 = !{!8, !11, !12, !13}
!20 = !{!21, !21, i64 0}
!21 = !{!"jtbaa_data", !5, i64 0}
!22 = !{!12}
!23 = !{!8, !11, !13, !14}

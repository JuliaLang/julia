; RUN: opt -enable-new-pm=0 -load libjulia-codegen%shlibext -ArrayOpt -S %s | FileCheck %s

; ModuleID = 'f'
source_filename = "f"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128-ni:10:11:12:13"
target triple = "x86_64-unknown-linux-gnu"

@llvm.compiler.used = appending global [3 x i8*] [i8* bitcast (void ({} addrspace(10)*)* @ijl_gc_queue_root to i8*), i8* bitcast ({} addrspace(10)* (i8*, i32, i32)* @ijl_gc_pool_alloc to i8*), i8* bitcast ({} addrspace(10)* (i8*, i64)* @ijl_gc_big_alloc to i8*)], section "llvm.metadata"

define i64 @test_1d_arraylen() #0 {
top:
  %0 = call {}*** @julia.get_pgcstack()
  %1 = call noalias nonnull {} addrspace(10)* inttoptr (i64 140617067693952 to {} addrspace(10)* ({} addrspace(10)*, i64)*)({} addrspace(10)* addrspacecast ({}* inttoptr (i64 140616737874944 to {}*) to {} addrspace(10)*), i64 100), !julia.array !17
  %2 = addrspacecast {} addrspace(10)* %1 to {} addrspace(11)*
  %3 = bitcast {} addrspace(11)* %2 to { i8 addrspace(13)*, i64, i16, i16, i32 } addrspace(11)*
  %4 = getelementptr inbounds { i8 addrspace(13)*, i64, i16, i16, i32 }, { i8 addrspace(13)*, i64, i16, i16, i32 } addrspace(11)* %3, i32 0, i32 1
  %5 = load i64, i64 addrspace(11)* %4, align 8, !tbaa !30, !range !35
  %6 = icmp slt i64 %5, 0
  ; CHECK: !julia.array
  ; COM: Check that arraylen load was removed
  ; CHECK-NOT: load
  ; COM: Check that signed->unsigned conversion worked
  ; CHECK: icmp ult i64 100, 0
  %7 = zext i1 %6 to i8
  %8 = trunc i8 %7 to i1
  %9 = xor i1 %8, true
  ; CHECK: select i1 %8, i64 100, i64 0
  %10 = select i1 %9, i64 %5, i64 0
  %11 = icmp slt i64 %10, 1
  %12 = zext i1 %11 to i8
  %13 = trunc i8 %12 to i1
  %14 = xor i1 %13, true
  %. = select i1 %14, i8 0, i8 1
  %.8 = select i1 %14, i64 1, i64 undef
  %15 = xor i8 %., 1
  %16 = trunc i8 %15 to i1
  %17 = xor i1 %16, true
  br i1 %17, label %L35, label %L16

L16:                                              ; preds = %L16, %top
  %value_phi3 = phi i64 [ %.8, %top ], [ %value_phi5, %L16 ]
  %18 = sub i64 %value_phi3, 1
  %19 = mul i64 %18, 1
  %20 = add i64 0, %19
  %21 = addrspacecast {} addrspace(10)* %1 to {} addrspace(11)*
  %22 = bitcast {} addrspace(11)* %21 to { i8 addrspace(13)*, i64, i16, i16, i32 } addrspace(11)*
  %23 = getelementptr inbounds { i8 addrspace(13)*, i64, i16, i16, i32 }, { i8 addrspace(13)*, i64, i16, i16, i32 } addrspace(11)* %22, i32 0, i32 0
  %24 = load i8 addrspace(13)*, i8 addrspace(13)* addrspace(11)* %23, align 8, !tbaa !65, !nonnull !4
  %25 = bitcast i8 addrspace(13)* %24 to double addrspace(13)*
  %26 = getelementptr inbounds double, double addrspace(13)* %25, i64 %20
  store double 0.000000e+00, double addrspace(13)* %26, align 8, !tbaa !67
  %27 = icmp eq i64 %value_phi3, %10
  %28 = zext i1 %27 to i8
  %29 = trunc i8 %28 to i1
  %30 = xor i1 %29, true
  %31 = add i64 %value_phi3, 1
  %value_phi5 = select i1 %30, i64 %31, i64 undef
  %value_phi7 = select i1 %30, i8 0, i8 1
  %32 = xor i8 %value_phi7, 1
  %33 = trunc i8 %32 to i1
  %34 = xor i1 %33, true
  br i1 %34, label %L35, label %L16

L35:                                              ; preds = %L16, %top
  %35 = addrspacecast {} addrspace(10)* %1 to {} addrspace(11)*
  %36 = bitcast {} addrspace(11)* %35 to { i8 addrspace(13)*, i64, i16, i16, i32 } addrspace(11)*
  %37 = getelementptr inbounds { i8 addrspace(13)*, i64, i16, i16, i32 }, { i8 addrspace(13)*, i64, i16, i16, i32 } addrspace(11)* %36, i32 0, i32 1
  %38 = load i64, i64 addrspace(11)* %37, align 8, !tbaa !30, !range !35
  ret i64 %38
}

define i8 @test_2d_arraycmp(i64 signext %0, i64 signext %1) #0 {
top:
  %2 = call {}*** @julia.get_pgcstack()
  %3 = mul nuw nsw i64 %1, %0
  %4 = call noalias nonnull {} addrspace(10)* inttoptr (i64 140069975219312 to {} addrspace(10)* ({} addrspace(10)*, i64, i64)*)({} addrspace(10)* addrspacecast ({}* inttoptr (i64 140069832560720 to {}*) to {} addrspace(10)*), i64 %0, i64 %1), !julia.array !17
  ; CHECK: !julia.array
  ; CHECK-NEXT: icmp ult i64 %3, 0
  %5 = icmp slt i64 %3, 0
  %6 = zext i1 %5 to i8
  %7 = trunc i8 %6 to i1
  %8 = xor i1 %7, true
  %9 = select i1 %8, i64 %3, i64 0
  %10 = icmp slt i64 %9, 1
  %11 = zext i1 %10 to i8
  %12 = trunc i8 %11 to i1
  %13 = xor i1 %12, true
  %. = select i1 %13, i8 0, i8 1
  %.8 = select i1 %13, i64 1, i64 undef
  %14 = xor i8 %., 1
  %15 = trunc i8 %14 to i1
  %16 = xor i1 %15, true
  br i1 %16, label %L35, label %L16

L16:                                              ; preds = %L16, %top
  %value_phi3 = phi i64 [ %.8, %top ], [ %value_phi5, %L16 ]
  %17 = sub i64 %value_phi3, 1
  %18 = mul i64 %17, 1
  %19 = add i64 0, %18
  %20 = addrspacecast {} addrspace(10)* %4 to {} addrspace(11)*
  %21 = bitcast {} addrspace(11)* %20 to { i8 addrspace(13)*, i64, i16, i16, i32 } addrspace(11)*
  %22 = getelementptr inbounds { i8 addrspace(13)*, i64, i16, i16, i32 }, { i8 addrspace(13)*, i64, i16, i16, i32 } addrspace(11)* %21, i32 0, i32 0
  %23 = load i8 addrspace(13)*, i8 addrspace(13)* addrspace(11)* %22, align 8, !tbaa !65, !invariant.load !4, !nonnull !4
  %24 = bitcast i8 addrspace(13)* %23 to double addrspace(13)*
  %25 = getelementptr inbounds double, double addrspace(13)* %24, i64 %19
  store double 0.000000e+00, double addrspace(13)* %25, align 8, !tbaa !67
  %26 = icmp eq i64 %value_phi3, %9
  %27 = zext i1 %26 to i8
  %28 = trunc i8 %27 to i1
  %29 = xor i1 %28, true
  %30 = add i64 %value_phi3, 1
  %value_phi5 = select i1 %29, i64 %30, i64 undef
  %value_phi7 = select i1 %29, i8 0, i8 1
  %31 = xor i8 %value_phi7, 1
  %32 = trunc i8 %31 to i1
  %33 = xor i1 %32, true
  br i1 %33, label %L35, label %L16

L35:                                              ; preds = %L16, %top
  ; CHECK: icmp ult i64 %0, 0
  %34 = icmp slt i64 %0, 0
  %35 = zext i1 %34 to i8
  ret i8 %35
}

declare {}*** @julia.get_pgcstack()

declare nonnull {} addrspace(10)* @ijl_box_int64(i64 signext)

; Function Attrs: inaccessiblemem_or_argmemonly
declare void @ijl_gc_queue_root({} addrspace(10)*) #2

; Function Attrs: allocsize(1)
declare noalias nonnull {} addrspace(10)* @ijl_gc_pool_alloc(i8*, i32, i32) #3

; Function Attrs: allocsize(1)
declare noalias nonnull {} addrspace(10)* @ijl_gc_big_alloc(i8*, i64) #3

attributes #0 = { "probe-stack"="inline-asm" }
attributes #1 = { "probe-stack"="inline-asm" "thunk" }
attributes #2 = { inaccessiblemem_or_argmemonly }
attributes #3 = { allocsize(1) }

!llvm.module.flags = !{!0, !1}

!0 = !{i32 2, !"Dwarf Version", i32 4}
!1 = !{i32 2, !"Debug Info Version", i32 3}
!4 = !{}
!17 = !{!"allocation"}
!30 = !{!31, !31, i64 0}
!31 = !{!"jtbaa_arraylen", !32, i64 0}
!32 = !{!"jtbaa_array", !33, i64 0}
!33 = !{!"jtbaa", !34, i64 0}
!34 = !{!"jtbaa"}
!35 = !{i64 0, i64 9223372036854775807}
!65 = !{!66, !66, i64 0}
!66 = !{!"jtbaa_arrayptr", !32, i64 0}
!67 = !{!68, !68, i64 0}
!68 = !{!"jtbaa_arraybuf", !69, i64 0}
!69 = !{!"jtbaa_data", !33, i64 0}

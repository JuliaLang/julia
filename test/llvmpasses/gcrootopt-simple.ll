; RUN: opt -load libjulia.so -OptimizeGCRoots -globaldce -S %s | FileCheck %s

source_filename = "f"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%jl_array_t = type { i8*, i64, i16 }

; CHECK: julia_f_68566
define void @julia_f_68566(i8**) #0 !dbg !5 {
top:
  %1 = alloca i64
  %2 = alloca i64
  %3 = alloca i64
  %4 = alloca i64
  %x = alloca i8**
  %a = alloca i8**
; These are all redundant. LLVM should get rid of them
; CHECK-NOT: @julia.gc_root_decl
  %5 = call i8*** @julia.gc_root_decl()
  %6 = call i8*** @julia.gc_root_decl()
  %7 = call i8*** @julia.gc_root_decl()
  %8 = call i8**** @jl_get_ptls_states()
  %9 = bitcast i8**** %8 to i8***
  %10 = getelementptr i8**, i8*** %9, i64 3
  %11 = bitcast i8*** %10 to i64**
  %12 = load i64*, i64** %11, !tbaa !7
  %i = alloca i64
  %"#temp#" = alloca i64
  store i8** %0, i8*** %x
  %13 = load i8**, i8*** %x, !dbg !10
  %14 = bitcast i8** %13 to i8*, !dbg !10
  %15 = getelementptr i8, i8* %14, i64 0, !dbg !10
  %16 = bitcast i8* %15 to i8***, !dbg !10
  %17 = load i8**, i8*** %16, !dbg !10, !tbaa !11
  store i8** %17, i8*** %5, !dbg !10
  store i8** %17, i8*** %a, !dbg !10
  store i64 100, i64* %4, !dbg !10
  store i64 1, i64* %"#temp#", !dbg !10, !tbaa !15
  br label %L4, !dbg !10

L4:                                               ; preds = %L15, %top
  %18 = load i64, i64* %4, !dbg !10, !tbaa !15
  %19 = add i64 %18, 1, !dbg !10
  %20 = load i64, i64* %"#temp#", !dbg !10, !tbaa !15
  %21 = icmp eq i64 %20, %19, !dbg !10
  %22 = zext i1 %21 to i8, !dbg !10
  %23 = xor i8 %22, 1, !dbg !10
  %24 = trunc i8 %23 to i1, !dbg !10
  %25 = xor i1 %24, true, !dbg !10
  br i1 %25, label %L17, label %if, !dbg !10

if:                                               ; preds = %L4
  %26 = bitcast i64* %3 to i8*, !dbg !10
  %27 = bitcast i64* %"#temp#" to i8*, !dbg !10
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %26, i8* %27, i64 8, i32 0, i1 false), !dbg !10, !tbaa_src !15, !tbaa_dest !15
  %28 = load i64, i64* %"#temp#", !dbg !10, !tbaa !15
  %29 = add i64 %28, 1, !dbg !10
  store i64 %29, i64* %2, !dbg !10
  %30 = bitcast i64* %3 to i8*, !dbg !10
  %31 = bitcast i64* %i to i8*, !dbg !10
  call void @llvm.memcpy.p0i8.p0i8.i32(i8* %31, i8* %30, i32 8, i32 8, i1 false), !dbg !10, !tbaa_src !15, !tbaa_dest !15
  %32 = bitcast i64* %2 to i8*, !dbg !10
  %33 = bitcast i64* %"#temp#" to i8*, !dbg !10
  call void @llvm.memcpy.p0i8.p0i8.i32(i8* %33, i8* %32, i32 8, i32 8, i1 false), !dbg !10, !tbaa_src !15, !tbaa_dest !15
  %34 = load i8**, i8*** %a, !dbg !10
  store i8** %34, i8*** %6, !dbg !10
  %35 = load i64, i64* %i, !dbg !10, !tbaa !15
  %36 = sub i64 %35, 1, !dbg !10
  %37 = mul i64 %36, 1, !dbg !10
  %38 = add i64 0, %37, !dbg !10
  %39 = bitcast i8** %34 to %jl_array_t*, !dbg !10
  %40 = getelementptr inbounds %jl_array_t, %jl_array_t* %39, i32 0, i32 0, !dbg !10
  %41 = load i8*, i8** %40, !dbg !10, !tbaa !17
  %42 = bitcast i8* %41 to i64*, !dbg !10
  %43 = getelementptr i64, i64* %42, i64 %38, !dbg !10
  %44 = load i64, i64* %43, !dbg !10, !tbaa !20
  %45 = add i64 %44, 1, !dbg !10
  store i64 %45, i64* %1, !dbg !10
  %46 = load i8**, i8*** %a, !dbg !10
  store i8** %46, i8*** %7, !dbg !10
  %47 = load i64, i64* %i, !dbg !10, !tbaa !15
  %48 = sub i64 %47, 1, !dbg !10
  %49 = mul i64 %48, 1, !dbg !10
  %50 = add i64 0, %49, !dbg !10
  %51 = bitcast i8** %46 to %jl_array_t*, !dbg !10
  %52 = getelementptr inbounds %jl_array_t, %jl_array_t* %51, i32 0, i32 0, !dbg !10
  %53 = load i8*, i8** %52, !dbg !10, !tbaa !17
  %54 = load i64, i64* %1, !dbg !10, !tbaa !15
  %55 = bitcast i8* %53 to i64*, !dbg !10
  %56 = getelementptr i64, i64* %55, i64 %50, !dbg !10
  store i64 %54, i64* %56, !dbg !10, !tbaa !20
  br label %L15, !dbg !10

L17:                                              ; preds = %L4
  ret void, !dbg !10

L15:                                              ; preds = %if
  br label %L4, !dbg !10
}

declare i8**** @jl_get_ptls_states()

declare i8*** @julia.gc_root_decl()

; Function Attrs: argmemonly nounwind
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* nocapture writeonly, i8* nocapture readonly, i64, i32, i1) #1

; Function Attrs: argmemonly nounwind
declare void @llvm.memcpy.p0i8.p0i8.i32(i8* nocapture writeonly, i8* nocapture readonly, i32, i32, i1) #1

attributes #0 = { "no-frame-pointer-elim"="true" }
attributes #1 = { argmemonly nounwind }

!llvm.module.flags = !{!0, !1}
!llvm.dbg.cu = !{!2}

!0 = !{i32 2, !"Dwarf Version", i32 4}
!1 = !{i32 1, !"Debug Info Version", i32 3}
!2 = distinct !DICompileUnit(language: DW_LANG_C89, file: !3, producer: "julia", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug, enums: !4)
!3 = !DIFile(filename: "REPL[2]", directory: ".")
!4 = !{}
!5 = distinct !DISubprogram(name: "f", linkageName: "julia_f_68566", scope: null, file: !3, type: !6, isLocal: false, isDefinition: true, isOptimized: true, unit: !2, variables: !4)
!6 = !DISubroutineType(types: !4)
!7 = !{!8, !8, i64 0, i64 1}
!8 = !{!"jtbaa_const", !9, i64 0}
!9 = !{!"jtbaa"}
!10 = !DILocation(line: 1, scope: !5)
!11 = !{!12, !12, i64 0}
!12 = !{!"jtbaa_mutab", !13, i64 0}
!13 = !{!"jtbaa_value", !14, i64 0}
!14 = !{!"jtbaa_data", !9, i64 0}
!15 = !{!16, !16, i64 0}
!16 = !{!"jtbaa_stack", !9, i64 0}
!17 = !{!18, !18, i64 0}
!18 = !{!"jtbaa_arrayptr", !19, i64 0}
!19 = !{!"jtbaa_array", !9, i64 0}
!20 = !{!21, !21, i64 0}
!21 = !{!"jtbaa_arraybuf", !14, i64 0}

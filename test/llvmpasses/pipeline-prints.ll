; COM: This is a newpm-only test, no legacypm command
; COM: we run all the prefixes even though some don't have tests because we want to make sure they don't crash
; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='julia' --print-before=BeforeOptimization -o /dev/null %s 2>&1 | FileCheck %s --check-prefixes=BEFOREOPTIMIZATION
; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='julia' --print-before=BeforeEarlySimplification -o /dev/null %s 2>&1 | FileCheck %s --check-prefixes=BEFOREEARLYSIMPLIFICATION
; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='julia' --print-before=AfterEarlySimplification -o /dev/null %s 2>&1 | FileCheck %s --check-prefixes=AFTEREARLYSIMPLIFICATION
; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='julia' --print-before=BeforeEarlyOptimization -o /dev/null %s 2>&1 | FileCheck %s --check-prefixes=BEFOREEARLYOPTIMIZATION
; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='julia' --print-before=AfterEarlyOptimization -o /dev/null %s 2>&1 | FileCheck %s --check-prefixes=AFTEREARLYOPTIMIZATION
; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='julia' --print-before=BeforeLoopOptimization -o /dev/null %s 2>&1 | FileCheck %s --check-prefixes=BEFORELOOPOPTIMIZATION
; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='julia' --print-before=BeforeLICM -o /dev/null %s 2>&1 | FileCheck %s --check-prefixes=BEFORELICM
; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='julia' --print-before=AfterLICM -o /dev/null %s 2>&1 | FileCheck %s --check-prefixes=AFTERLICM
; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='julia' --print-before=BeforeLoopSimplification -o /dev/null %s 2>&1 | FileCheck %s --check-prefixes=BEFORELOOPSIMPLIFICATION
; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='julia' --print-before=AfterLoopSimplification -o /dev/null %s 2>&1 | FileCheck %s --check-prefixes=AFTERLOOPSIMPLIFICATION
; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='julia' --print-before=AfterLoopOptimization -o /dev/null %s 2>&1 | FileCheck %s --check-prefixes=AFTERLOOPOPTIMIZATION
; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='julia' --print-before=BeforeScalarOptimization -o /dev/null %s 2>&1 | FileCheck %s --check-prefixes=BEFORESCALAROPTIMIZATION
; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='julia' --print-before=AfterScalarOptimization -o /dev/null %s 2>&1 | FileCheck %s --check-prefixes=AFTERSCALAROPTIMIZATION
; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='julia' --print-before=BeforeVectorization -o /dev/null %s 2>&1 | FileCheck %s --check-prefixes=BEFOREVECTORIZATION
; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='julia' --print-before=AfterVectorization -force-vector-width=2 -o /dev/null %s 2>&1 | FileCheck %s --check-prefixes=AFTERVECTORIZATION
; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='julia' --print-before=BeforeIntrinsicLowering -o /dev/null %s 2>&1 | FileCheck %s --check-prefixes=BEFOREINTRINSICLOWERING
; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='julia' --print-before=AfterIntrinsicLowering -o /dev/null %s 2>&1 | FileCheck %s --check-prefixes=AFTERINTRINSICLOWERING
; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='julia' --print-before=BeforeCleanup -o /dev/null %s 2>&1 | FileCheck %s --check-prefixes=BEFORECLEANUP
; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='julia' --print-before=AfterCleanup -o /dev/null %s 2>&1 | FileCheck %s --check-prefixes=AFTERCLEANUP
; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='julia' --print-before=AfterOptimization -o /dev/null %s 2>&1 | FileCheck %s --check-prefixes=AFTEROPTIMIZATION

; ModuleID = 'f'
source_filename = "f"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128-ni:10:11:12:13"
target triple = "x86_64-unknown-linux-gnu"

define i64 @julia_f_199({} addrspace(10)* noundef nonnull align 16 dereferenceable(40) %0) #0 !dbg !4 {
top:
  %x = alloca {} addrspace(10)*, align 8
  %1 = call {}*** @julia.get_pgcstack()
  store {} addrspace(10)* null, {} addrspace(10)** %x, align 8
  %2 = bitcast {}*** %1 to {}**
  %current_task = getelementptr inbounds {}*, {}** %2, i64 -14
  %3 = bitcast {}** %current_task to i64*
  %world_age = getelementptr inbounds i64, i64* %3, i64 15
  store {} addrspace(10)* %0, {} addrspace(10)** %x, align 8
  %4 = bitcast {}*** %1 to {}**
  %current_task1 = getelementptr inbounds {}*, {}** %4, i64 -14
  %ptls_field = getelementptr inbounds {}*, {}** %current_task1, i64 16
  %ptls_load = load {}*, {}** %ptls_field, align 8, !tbaa !8
  %ptls = bitcast {}* %ptls_load to {}**
  %5 = bitcast {}** %ptls to i64**
  %6 = getelementptr inbounds i64*, i64** %5, i64 2
  %safepoint = load i64*, i64** %6, align 8, !tbaa !12, !invariant.load !7
  fence syncscope("singlethread") seq_cst
  call void @julia.safepoint(i64* %safepoint), !dbg !14
  fence syncscope("singlethread") seq_cst
  %7 = load {} addrspace(10)*, {} addrspace(10)** %x, align 8, !dbg !15, !nonnull !7, !dereferenceable !23, !align !24
  %8 = addrspacecast {} addrspace(10)* %7 to {} addrspace(11)*, !dbg !15
  %9 = bitcast {} addrspace(11)* %8 to { i8 addrspace(13)*, i64, i16, i16, i32 } addrspace(11)*, !dbg !15
  %10 = getelementptr inbounds { i8 addrspace(13)*, i64, i16, i16, i32 }, { i8 addrspace(13)*, i64, i16, i16, i32 } addrspace(11)* %9, i32 0, i32 1, !dbg !15
  %11 = load i64, i64 addrspace(11)* %10, align 8, !dbg !15, !tbaa !12, !range !25, !invariant.load !7, !alias.scope !26, !noalias !29
  %12 = icmp sle i64 0, %11, !dbg !34
  %13 = icmp ult i64 0, %11, !dbg !42
  %14 = and i1 %12, %13, !dbg !43
  %15 = zext i1 %14 to i8, !dbg !18
  %16 = trunc i8 %15 to i1, !dbg !18
  %17 = xor i1 %16, true, !dbg !18
  br i1 %17, label %L12, label %L9, !dbg !18

L9:                                               ; preds = %top
  %18 = load {} addrspace(10)*, {} addrspace(10)** %x, align 8, !dbg !46, !nonnull !7, !dereferenceable !23, !align !24
  %19 = addrspacecast {} addrspace(10)* %18 to {} addrspace(11)*, !dbg !46
  %20 = bitcast {} addrspace(11)* %19 to { i8 addrspace(13)*, i64, i16, i16, i32 } addrspace(11)*, !dbg !46
  %21 = getelementptr inbounds { i8 addrspace(13)*, i64, i16, i16, i32 }, { i8 addrspace(13)*, i64, i16, i16, i32 } addrspace(11)* %20, i32 0, i32 0, !dbg !46
  %22 = load i8 addrspace(13)*, i8 addrspace(13)* addrspace(11)* %21, align 8, !dbg !46, !tbaa !12, !invariant.load !7, !alias.scope !26, !noalias !29, !nonnull !7
  %23 = bitcast i8 addrspace(13)* %22 to i64 addrspace(13)*, !dbg !46
  %24 = getelementptr inbounds i64, i64 addrspace(13)* %23, i64 0, !dbg !46
  %25 = load i64, i64 addrspace(13)* %24, align 8, !dbg !46, !tbaa !48, !alias.scope !51, !noalias !52
  br label %L13, !dbg !18

L12:                                              ; preds = %top
  br label %L13, !dbg !18

L13:                                              ; preds = %L12, %L9
  %value_phi = phi i8 [ 0, %L9 ], [ 1, %L12 ]
  %value_phi2 = phi i64 [ %25, %L9 ], [ undef, %L12 ]
  %value_phi3 = phi i64 [ 2, %L9 ], [ undef, %L12 ]
  br label %L17, !dbg !21

L17:                                              ; preds = %L13
  %26 = trunc i8 %value_phi to i1, !dbg !22
  %27 = xor i1 %26, true, !dbg !22
  %28 = zext i1 %27 to i8, !dbg !22
  %29 = trunc i8 %28 to i1, !dbg !22
  %30 = xor i1 %29, true, !dbg !22
  br i1 %30, label %L17.L41_crit_edge, label %L17.L19_crit_edge, !dbg !22

L17.L41_crit_edge:                                ; preds = %L17
  br label %L41, !dbg !53

L17.L19_crit_edge:                                ; preds = %L17
  br label %L19, !dbg !18

L19:                                              ; preds = %L17.L19_crit_edge, %L40
  %value_phi4 = phi i64 [ %value_phi2, %L17.L19_crit_edge ], [ %value_phi7, %L40 ]
  %value_phi5 = phi i64 [ %value_phi3, %L17.L19_crit_edge ], [ %value_phi8, %L40 ]
  %value_phi6 = phi i64 [ 0, %L17.L19_crit_edge ], [ %31, %L40 ]
  %31 = add i64 %value_phi6, %value_phi4, !dbg !55
  %32 = sub i64 %value_phi5, 1, !dbg !58
  %33 = load {} addrspace(10)*, {} addrspace(10)** %x, align 8, !dbg !61, !nonnull !7, !dereferenceable !23, !align !24
  %34 = addrspacecast {} addrspace(10)* %33 to {} addrspace(11)*, !dbg !61
  %35 = bitcast {} addrspace(11)* %34 to { i8 addrspace(13)*, i64, i16, i16, i32 } addrspace(11)*, !dbg !61
  %36 = getelementptr inbounds { i8 addrspace(13)*, i64, i16, i16, i32 }, { i8 addrspace(13)*, i64, i16, i16, i32 } addrspace(11)* %35, i32 0, i32 1, !dbg !61
  %37 = load i64, i64 addrspace(11)* %36, align 8, !dbg !61, !tbaa !12, !range !25, !invariant.load !7, !alias.scope !26, !noalias !29
  %38 = icmp sle i64 0, %37, !dbg !62
  %39 = icmp ult i64 %32, %37, !dbg !65
  %40 = and i1 %38, %39, !dbg !66
  %41 = zext i1 %40 to i8, !dbg !53
  %42 = trunc i8 %41 to i1, !dbg !53
  %43 = xor i1 %42, true, !dbg !53
  br i1 %43, label %L34, label %L31, !dbg !53

L31:                                              ; preds = %L19
  %44 = load {} addrspace(10)*, {} addrspace(10)** %x, align 8, !dbg !67, !nonnull !7, !dereferenceable !23, !align !24
  %45 = sub i64 %value_phi5, 1, !dbg !67
  %46 = mul i64 %45, 1, !dbg !67
  %47 = add i64 0, %46, !dbg !67
  %48 = addrspacecast {} addrspace(10)* %44 to {} addrspace(11)*, !dbg !67
  %49 = bitcast {} addrspace(11)* %48 to { i8 addrspace(13)*, i64, i16, i16, i32 } addrspace(11)*, !dbg !67
  %50 = getelementptr inbounds { i8 addrspace(13)*, i64, i16, i16, i32 }, { i8 addrspace(13)*, i64, i16, i16, i32 } addrspace(11)* %49, i32 0, i32 0, !dbg !67
  %51 = load i8 addrspace(13)*, i8 addrspace(13)* addrspace(11)* %50, align 8, !dbg !67, !tbaa !12, !invariant.load !7, !alias.scope !26, !noalias !29, !nonnull !7
  %52 = bitcast i8 addrspace(13)* %51 to i64 addrspace(13)*, !dbg !67
  %53 = getelementptr inbounds i64, i64 addrspace(13)* %52, i64 %47, !dbg !67
  %54 = load i64, i64 addrspace(13)* %53, align 8, !dbg !67, !tbaa !48, !alias.scope !51, !noalias !52
  %55 = add i64 %value_phi5, 1, !dbg !68
  br label %L35, !dbg !53

L34:                                              ; preds = %L19
  br label %L35, !dbg !53

L35:                                              ; preds = %L34, %L31
  %value_phi7 = phi i64 [ %54, %L31 ], [ undef, %L34 ]
  %value_phi8 = phi i64 [ %55, %L31 ], [ undef, %L34 ]
  %value_phi9 = phi i8 [ 0, %L31 ], [ 1, %L34 ]
  %56 = trunc i8 %value_phi9 to i1, !dbg !54
  %57 = xor i1 %56, true, !dbg !54
  %58 = zext i1 %57 to i8, !dbg !54
  %59 = trunc i8 %58 to i1, !dbg !54
  %60 = xor i1 %59, true, !dbg !54
  br i1 %60, label %L35.L41_crit_edge, label %L40, !dbg !54

L35.L41_crit_edge:                                ; preds = %L35
  br label %L41, !dbg !53

L40:                                              ; preds = %L35
  br label %L19, !dbg !18

L41:                                              ; preds = %L17.L41_crit_edge, %L35.L41_crit_edge
  %value_phi10 = phi i64 [ %31, %L35.L41_crit_edge ], [ 0, %L17.L41_crit_edge ]
  ret i64 %value_phi10, !dbg !69
}

; Function Attrs: noinline optnone
define nonnull {} addrspace(10)* @jfptr_f_200({} addrspace(10)* %0, {} addrspace(10)** noalias nocapture noundef readonly %1, i32 %2) #1 {
top:
  %3 = call {}*** @julia.get_pgcstack()
  %4 = getelementptr inbounds {} addrspace(10)*, {} addrspace(10)** %1, i32 0
  %5 = load {} addrspace(10)*, {} addrspace(10)** %4, align 8, !tbaa !12, !invariant.load !7, !alias.scope !26, !noalias !29, !nonnull !7, !dereferenceable !23, !align !24
  %6 = call i64 @julia_f_199({} addrspace(10)* %5)
  %7 = call nonnull {} addrspace(10)* @ijl_box_int64(i64 signext %6)
  ret {} addrspace(10)* %7
}

declare {}*** @julia.get_pgcstack()

declare nonnull {} addrspace(10)* @ijl_box_int64(i64 signext)

; Function Attrs: inaccessiblemem_or_argmemonly
declare void @julia.safepoint(i64*) #2

attributes #0 = { "frame-pointer"="all" "probe-stack"="inline-asm" }
attributes #1 = { noinline optnone "frame-pointer"="all" "probe-stack"="inline-asm" }
attributes #2 = { inaccessiblemem_or_argmemonly }

!llvm.module.flags = !{!0, !1}
!llvm.dbg.cu = !{!2}

!0 = !{i32 2, !"Dwarf Version", i32 4}
!1 = !{i32 2, !"Debug Info Version", i32 3}
!2 = distinct !DICompileUnit(language: DW_LANG_Julia, file: !3, producer: "julia", isOptimized: true, runtimeVersion: 0, emissionKind: NoDebug, nameTableKind: GNU)
!3 = !DIFile(filename: "julia", directory: ".")
!4 = distinct !DISubprogram(name: "f", linkageName: "julia_f_199", scope: null, file: !5, line: 1, type: !6, scopeLine: 1, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !7)
!5 = !DIFile(filename: "REPL[2]", directory: ".")
!6 = !DISubroutineType(types: !7)
!7 = !{}
!8 = !{!9, !9, i64 0}
!9 = !{!"jtbaa_gcframe", !10, i64 0}
!10 = !{!"jtbaa", !11, i64 0}
!11 = !{!"jtbaa"}
!12 = !{!13, !13, i64 0, i64 1}
!13 = !{!"jtbaa_const", !10, i64 0}
!14 = !DILocation(line: 1, scope: !4)
!15 = !DILocation(line: 10, scope: !16, inlinedAt: !18)
!16 = distinct !DISubprogram(name: "length;", linkageName: "length", scope: !17, file: !17, type: !6, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !7)
!17 = !DIFile(filename: "essentials.jl", directory: ".")
!18 = !DILocation(line: 943, scope: !19, inlinedAt: !21)
!19 = distinct !DISubprogram(name: "iterate;", linkageName: "iterate", scope: !20, file: !20, type: !6, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !7)
!20 = !DIFile(filename: "array.jl", directory: ".")
!21 = !DILocation(line: 943, scope: !19, inlinedAt: !22)
!22 = !DILocation(line: 3, scope: !4)
!23 = !{i64 40}
!24 = !{i64 16}
!25 = !{i64 0, i64 9223372036854775807}
!26 = !{!27}
!27 = !{!"jnoalias_const", !28}
!28 = !{!"jnoalias"}
!29 = !{!30, !31, !32, !33}
!30 = !{!"jnoalias_gcframe", !28}
!31 = !{!"jnoalias_stack", !28}
!32 = !{!"jnoalias_data", !28}
!33 = !{!"jnoalias_typemd", !28}
!34 = !DILocation(line: 514, scope: !35, inlinedAt: !37)
!35 = distinct !DISubprogram(name: "<=;", linkageName: "<=", scope: !36, file: !36, type: !6, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !7)
!36 = !DIFile(filename: "int.jl", directory: ".")
!37 = !DILocation(line: 423, scope: !38, inlinedAt: !40)
!38 = distinct !DISubprogram(name: ">=;", linkageName: ">=", scope: !39, file: !39, type: !6, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !7)
!39 = !DIFile(filename: "operators.jl", directory: ".")
!40 = !DILocation(line: 520, scope: !41, inlinedAt: !18)
!41 = distinct !DISubprogram(name: "<;", linkageName: "<", scope: !36, file: !36, type: !6, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !7)
!42 = !DILocation(line: 513, scope: !41, inlinedAt: !40)
!43 = !DILocation(line: 38, scope: !44, inlinedAt: !40)
!44 = distinct !DISubprogram(name: "&;", linkageName: "&", scope: !45, file: !45, type: !6, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !7)
!45 = !DIFile(filename: "bool.jl", directory: ".")
!46 = !DILocation(line: 13, scope: !47, inlinedAt: !18)
!47 = distinct !DISubprogram(name: "getindex;", linkageName: "getindex", scope: !17, file: !17, type: !6, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !7)
!48 = !{!49, !49, i64 0}
!49 = !{!"jtbaa_arraybuf", !50, i64 0}
!50 = !{!"jtbaa_data", !10, i64 0}
!51 = !{!32}
!52 = !{!30, !31, !33, !27}
!53 = !DILocation(line: 943, scope: !19, inlinedAt: !54)
!54 = !DILocation(line: 5, scope: !4)
!55 = !DILocation(line: 87, scope: !56, inlinedAt: !57)
!56 = distinct !DISubprogram(name: "+;", linkageName: "+", scope: !36, file: !36, type: !6, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !7)
!57 = !DILocation(line: 4, scope: !4)
!58 = !DILocation(line: 86, scope: !59, inlinedAt: !60)
!59 = distinct !DISubprogram(name: "-;", linkageName: "-", scope: !36, file: !36, type: !6, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !7)
!60 = !DILocation(line: 1068, scope: !59, inlinedAt: !53)
!61 = !DILocation(line: 10, scope: !16, inlinedAt: !53)
!62 = !DILocation(line: 514, scope: !35, inlinedAt: !63)
!63 = !DILocation(line: 423, scope: !38, inlinedAt: !64)
!64 = !DILocation(line: 520, scope: !41, inlinedAt: !53)
!65 = !DILocation(line: 513, scope: !41, inlinedAt: !64)
!66 = !DILocation(line: 38, scope: !44, inlinedAt: !64)
!67 = !DILocation(line: 13, scope: !47, inlinedAt: !53)
!68 = !DILocation(line: 87, scope: !56, inlinedAt: !53)
!69 = !DILocation(line: 6, scope: !4)

; BEFOREEARLYSIMPLIFICATION: IR Dump Before BeforeEarlySimplification
; AFTEREARLYSIMPLIFICATION: IR Dump Before AfterEarlySimplification
; BEFOREEARLYOPTIMIZATION: IR Dump Before BeforeEarlyOptimization
; AFTEREARLYOPTIMIZATION: IR Dump Before AfterEarlyOptimization
; BEFORELOOPOPTIMIZATION: IR Dump Before BeforeLoopOptimization
; BEFORELICM: IR Dump Before BeforeLICM
; AFTERLICM: IR Dump Before AfterLICM
; BEFORELOOPSIMPLIFICATION: IR Dump Before BeforeLoopSimplification
; AFTERLOOPSIMPLIFICATION: IR Dump Before AfterLoopSimplification
; AFTERLOOPOPTIMIZATION: IR Dump Before AfterLoopOptimization
; BEFORESCALAROPTIMIZATION: IR Dump Before BeforeScalarOptimization
; AFTERSCALAROPTIMIZATION: IR Dump Before AfterScalarOptimization
; BEFOREVECTORIZATION: IR Dump Before BeforeVectorization
; AFTERVECTORIZATION: IR Dump Before AfterVectorization
; BEFOREINTRINSICLOWERING: IR Dump Before BeforeIntrinsicLowering
; AFTERINTRINSICLOWERING: IR Dump Before AfterIntrinsicLowering
; BEFORECLEANUP: IR Dump Before BeforeCleanup
; AFTERCLEANUP: IR Dump Before AfterCleanup
; AFTEROPTIMIZATION: IR Dump Before AfterOptimization

; COM: simplifycfg should have killed this block
; BEFOREOPTIMIZATION: L17.L41_crit_edge:                                ; preds = %L17
; BEFOREOPTIMIZATION-NEXT: br label %L41, !dbg !53

; BEFOREEARLYSIMPLIFICATION: L17.L41_crit_edge:                          ; preds = %L17
; BEFOREEARLYSIMPLIFICATION-NEXT: br label %L41, !dbg !53

; AFTEREARLYSIMPLIFICATION-NOT: L17.L41_crit_edge:                           ; preds = %L17
; AFTEREARLYSIMPLIFICATION-NOT: br label %L41, !dbg !53

; BEFOREEARLYOPTIMIZATION-NOT: L17.L41_crit_edge:                           ; preds = %L17
; BEFOREEARLYOPTIMIZATION-NOT: br label %L41, !dbg !53


; COM: InstSimplify/InstCombine should kill this zext-trunc pair
; AFTEREARLYSIMPLIFICATION: [[ZEXT:%.*]] = zext i1 {{%.*}} to i8

; AFTEREARLYOPTIMIZATION-NOT: zext i1 {{%.*}} to i8

; BEFORELOOPOPTIMIZATION-NOT: zext i1 {{%.*}} to i8

; COM: Loop simplification makes the exit condition obvious
; AFTERLOOPSIMPLIFICATION: L35.lr.ph:
; AFTERLOOPSIMPLIFICATION: add nuw nsw

; COM: Scalar optimization removes the preheader
; AFTERSCALAROPTIMIZATION: L17:
; AFTERSCALAROPTIMIZATION: icmp eq i64 {{%.*}}, 1,

; COM: Vectorization does stuff
; AFTERVECTORIZATION: vector.body
; AFTERVECTORIZATION: llvm.vector.reduce.add

; COM: Intrinsics are lowered and cleaned up by the time optimization is finished
; AFTEROPTIMIZATION-NOT: call void @julia.safepoint
; AFTEROPTIMIZATION: load volatile i64{{.*}}%safepoint

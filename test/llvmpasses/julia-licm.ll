; RUN: opt -load libjulia-internal%shlibext -JuliaLICM -S %s | FileCheck %s

@tag = external addrspace(10) global {}, align 16

declare {}*** @julia.get_pgcstack()

define nonnull {} addrspace(10)* @julia_allocation_hoist(i64 signext %0) #0 !dbg !5 {
top:
  %1 = call {}*** @julia.get_pgcstack()
  %2 = icmp sgt i64 %0, 0, !dbg !7
  br i1 %2, label %L4, label %L3, !dbg !10

L3.loopexit:                                      ; preds = %L22
  %.lcssa = phi {} addrspace(10)* [ %3, %L22 ], !dbg !11
  br label %L3, !dbg !10

L3:                                               ; preds = %L3.loopexit, %top
  %merge = phi {} addrspace(10)* [ addrspacecast ({}* inttoptr (i64 139952239804424 to {}*) to {} addrspace(10)*), %top ], [ %.lcssa, %L3.loopexit ]
  ret {} addrspace(10)* %merge, !dbg !10

L4:                                               ; preds = %top
  %current_task112 = getelementptr inbounds {}**, {}*** %1, i64 -12, !dbg !19
  %current_task1 = bitcast {}*** %current_task112 to {}**, !dbg !19
  ; CHECK: %3 = call noalias nonnull {} addrspace(10)* @julia.gc_alloc_obj({}** nonnull %current_task1, i64 8, {} addrspace(10)* @tag)
  ; CHECK-NEXT: br label %L22
  br label %L22, !dbg !23

L22:                                              ; preds = %L4, %L22
  %value_phi5 = phi i64 [ 1, %L4 ], [ %5, %L22 ]
  ; CHECK: %value_phi5 = phi i64 [ 1, %L4 ], [ %5, %L22 ]
  ; CHECK-NEXT %4 = bitcast {} addrspace(10)* %3 to i64 addrspace(10)*
  %3 = call noalias nonnull {} addrspace(10)* @julia.gc_alloc_obj({}** nonnull %current_task1, i64 8, {} addrspace(10)* @tag) #2, !dbg !11
  %4 = bitcast {} addrspace(10)* %3 to i64 addrspace(10)*, !dbg !11
  store i64 %value_phi5, i64 addrspace(10)* %4, align 8, !dbg !11, !tbaa !24
  %.not = icmp eq i64 %value_phi5, %0, !dbg !30
  %5 = add i64 %value_phi5, 1, !dbg !33
  br i1 %.not, label %L3.loopexit, label %L22, !dbg !36
}

; Function Attrs: allocsize(1)
declare noalias nonnull {} addrspace(10)* @julia.gc_alloc_obj({}**, i64, {} addrspace(10)*) #2

; Function Attrs: argmemonly nofree nosync nounwind willreturn
declare void @llvm.lifetime.start.p0i8(i64 immarg, i8* nocapture) #3

; Function Attrs: argmemonly nofree nosync nounwind willreturn
declare void @llvm.lifetime.end.p0i8(i64 immarg, i8* nocapture) #3

; Function Attrs: inaccessiblemem_or_argmemonly
declare void @ijl_gc_queue_root({} addrspace(10)*) #4

; Function Attrs: allocsize(1)
declare noalias nonnull {} addrspace(10)* @ijl_gc_pool_alloc(i8*, i32, i32) #2

; Function Attrs: allocsize(1)
declare noalias nonnull {} addrspace(10)* @ijl_gc_big_alloc(i8*, i64) #2

attributes #0 = { "probe-stack"="inline-asm" }
attributes #1 = { "probe-stack"="inline-asm" "thunk" }
attributes #2 = { allocsize(1) }
attributes #3 = { argmemonly nofree nosync nounwind willreturn }
attributes #4 = { inaccessiblemem_or_argmemonly }

!llvm.module.flags = !{!0, !1}
!llvm.dbg.cu = !{!2}

!0 = !{i32 2, !"Dwarf Version", i32 4}
!1 = !{i32 2, !"Debug Info Version", i32 3}
!2 = distinct !DICompileUnit(language: DW_LANG_Julia, file: !3, producer: "julia", isOptimized: true, runtimeVersion: 0, emissionKind: NoDebug, enums: !4, nameTableKind: GNU)
!3 = !DIFile(filename: "REPL[1]", directory: ".")
!4 = !{}
!5 = distinct !DISubprogram(name: "f", linkageName: "julia_f_139", scope: null, file: !3, line: 1, type: !6, scopeLine: 1, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !4)
!6 = !DISubroutineType(types: !4)
!7 = !DILocation(line: 477, scope: !8, inlinedAt: !10)
!8 = distinct !DISubprogram(name: "<=;", linkageName: "<=", scope: !9, file: !9, type: !6, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !4)
!9 = !DIFile(filename: "int.jl", directory: ".")
!10 = !DILocation(line: 2, scope: !5)
!11 = !DILocation(line: 8, scope: !12, inlinedAt: !14)
!12 = distinct !DISubprogram(name: "RefValue;", linkageName: "RefValue", scope: !13, file: !13, type: !6, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !4)
!13 = !DIFile(filename: "refvalue.jl", directory: ".")
!14 = !DILocation(line: 10, scope: !12, inlinedAt: !15)
!15 = !DILocation(line: 134, scope: !16, inlinedAt: !18)
!16 = distinct !DISubprogram(name: "Ref;", linkageName: "Ref", scope: !17, file: !17, type: !6, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !4)
!17 = !DIFile(filename: "refpointer.jl", directory: ".")
!18 = !DILocation(line: 5, scope: !5)
!19 = !DILocation(line: 8, scope: !12, inlinedAt: !20)
!20 = !DILocation(line: 10, scope: !12, inlinedAt: !21)
!21 = !DILocation(line: 134, scope: !16, inlinedAt: !22)
!22 = !DILocation(line: 3, scope: !5)
!23 = !DILocation(line: 4, scope: !5)
!24 = !{!25, !25, i64 0}
!25 = !{!"jtbaa_mutab", !26, i64 0}
!26 = !{!"jtbaa_value", !27, i64 0}
!27 = !{!"jtbaa_data", !28, i64 0}
!28 = !{!"jtbaa", !29, i64 0}
!29 = !{!"jtbaa"}
!30 = !DILocation(line: 473, scope: !31, inlinedAt: !33)
!31 = distinct !DISubprogram(name: "==;", linkageName: "==", scope: !32, file: !32, type: !6, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !4)
!32 = !DIFile(filename: "promotion.jl", directory: ".")
!33 = !DILocation(line: 881, scope: !34, inlinedAt: !36)
!34 = distinct !DISubprogram(name: "iterate;", linkageName: "iterate", scope: !35, file: !35, type: !6, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !4)
!35 = !DIFile(filename: "range.jl", directory: ".")
!36 = !DILocation(line: 6, scope: !5)
!37 = !{i64 8}
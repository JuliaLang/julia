; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%{shlibext} -passes='function(LateLowerGCFrame,FinalLowerGC),RemoveJuliaAddrspaces,verify' -S %s | FileCheck %s
; RUN: opt --load-pass-plugin=libjulia-codegen%{shlibext} -passes='function(LateLowerGCFrame,FinalLowerGC),RemoveJuliaAddrspaces,verify' %s | llc -O0 -filetype=obj -o - | llvm-dwarfdump - | FileCheck %s --check-prefix=DWARF
; RUN: opt --load-pass-plugin=libjulia-codegen%{shlibext} -passes='function(LateLowerGCFrame,FinalLowerGC),RemoveJuliaAddrspaces,verify' %s | llc -O2 -filetype=obj -o - | llvm-dwarfdump - | FileCheck %s --check-prefix=DWARF

declare ptr @julia.get_pgcstack()
declare void @llvm.dbg.declare(metadata, metadata, metadata)
declare void @use_ptr(ptr)

define void @boxed_local_sunk_dbg(i1 %cond, ptr addrspace(10) %val) !dbg !5 {
top:
; CHECK-LABEL: @boxed_local_sunk_dbg
; CHECK: %gcframe = alloca ptr, i32 3, align 16
; CHECK: dbg{{.*}}declare({{(metadata )?}}ptr %box
; CHECK: br i1 %cond, label %use_bb, label %exit
  %pgcstack = call ptr @julia.get_pgcstack()
  %box = alloca ptr addrspace(10), align 8
  call void @llvm.dbg.declare(metadata ptr %box, metadata !10, metadata !DIExpression(DW_OP_deref)), !dbg !12
  br i1 %cond, label %use_bb, label %exit

use_bb:
; CHECK: use_bb:
; CHECK-NEXT: %box = getelementptr inbounds {{(nuw )?}}ptr, ptr %gcframe, i32 2
  store ptr addrspace(10) %val, ptr %box, align 8, !dbg !12
  call void @use_ptr(ptr %box), !dbg !12
  br label %exit

exit:
  ret void, !dbg !12
}

define void @boxed_local_diamond_dbg(i1 %c1, i1 %c2, ptr addrspace(10) %val) !dbg !13 {
top:
; CHECK-LABEL: @boxed_local_diamond_dbg
; CHECK: %gcframe = alloca ptr, i32 3, align 16
; CHECK: dbg{{.*}}declare({{(metadata )?}}ptr %box
; CHECK: br i1 %c1, label %left, label %right
  %pgcstack = call ptr @julia.get_pgcstack()
  %box = alloca ptr addrspace(10), align 8
  call void @llvm.dbg.declare(metadata ptr %box, metadata !14, metadata !DIExpression(DW_OP_deref)), !dbg !15
  br i1 %c1, label %left, label %right

left:
; CHECK: left:
; CHECK-NEXT: %box = getelementptr inbounds {{(nuw )?}}ptr, ptr %gcframe, i32 2
  store ptr addrspace(10) %val, ptr %box, align 8, !dbg !15
  call void @use_ptr(ptr %box), !dbg !15
  br label %exit

right:
; CHECK: right:
; CHECK-NEXT: %box{{[0-9]+}} = getelementptr inbounds {{(nuw )?}}ptr, ptr %gcframe, i32 2
  store ptr addrspace(10) %val, ptr %box, align 8, !dbg !15
  call void @use_ptr(ptr %box), !dbg !15
  br label %exit

exit:
  ret void, !dbg !15
}

; DWARF: DW_TAG_subprogram
; DWARF: DW_AT_name{{.*}}("boxed_local_sunk_dbg")
; DWARF: DW_TAG_variable
; DWARF-NEXT: DW_AT_location{{.*}}({{.*}}DW_OP_deref)
; DWARF-NEXT: DW_AT_name{{.*}}("x")
; DWARF: DW_TAG_subprogram
; DWARF: DW_AT_name{{.*}}("boxed_local_diamond_dbg")
; DWARF: DW_TAG_variable
; DWARF-NEXT: DW_AT_location{{.*}}({{.*}}DW_OP_deref)
; DWARF-NEXT: DW_AT_name{{.*}}("y")

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!3, !4}
!0 = distinct !DICompileUnit(language: DW_LANG_Julia, file: !1, producer: "julia", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug)
!1 = !DIFile(filename: "test.jl", directory: ".")
!2 = !{}
!3 = !{i32 2, !"Dwarf Version", i32 4}
!4 = !{i32 2, !"Debug Info Version", i32 3}
!5 = distinct !DISubprogram(name: "boxed_local_sunk_dbg", scope: !1, file: !1, line: 1, type: !6, scopeLine: 1, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!6 = !DISubroutineType(types: !2)
!10 = !DILocalVariable(name: "x", scope: !5, file: !1, line: 2, type: !11)
!11 = !DIBasicType(name: "Int64", size: 64, encoding: DW_ATE_signed)
!12 = !DILocation(line: 2, column: 1, scope: !5)
!13 = distinct !DISubprogram(name: "boxed_local_diamond_dbg", scope: !1, file: !1, line: 5, type: !6, scopeLine: 5, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!14 = !DILocalVariable(name: "y", scope: !13, file: !1, line: 6, type: !11)
!15 = !DILocation(line: 6, column: 1, scope: !13)

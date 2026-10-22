; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%{shlibext} -passes='VersionUnalignedLoads' -S %s | FileCheck %s
; RUN: opt --load-pass-plugin=libjulia-codegen%{shlibext} -passes='VersionUnalignedLoads' -mattr=+allows-misaligned-mem-access -S %s | FileCheck %s --check-prefix=FAST

; BPF stands in for targets without fast misaligned access (e.g. RISC-V without
; unaligned-scalar-mem): it has a subtarget feature that makes misaligned accesses fast.

; Targets with fast misaligned access keep all loads as is.
; FAST-NOT: is_aligned
target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "bpfel"

; CHECK-LABEL: @single
; CHECK: %[[ADDR:.+]] = ptrtoint ptr %p to i64
; CHECK: %[[LOW:.+]] = and i64 %[[ADDR]], 7
; CHECK: %is_aligned = icmp eq i64 %[[LOW]], 0
; CHECK: br i1 %is_aligned, label %[[ALIGNED:.+]], label %[[UNALIGNED:.+]]
; CHECK: [[ALIGNED]]:
; CHECK-NEXT: %[[A:.+]] = load i64, ptr %p, align 8
; CHECK: [[UNALIGNED]]:
; CHECK-NEXT: %[[U:.+]] = load i64, ptr %p, align 1
; CHECK: %x = phi i64 [ %[[A]], %[[ALIGNED]] ], [ %[[U]], %[[UNALIGNED]] ]
; CHECK-NEXT: ret i64 %x
define i64 @single(ptr %p) {
  %x = load i64, ptr %p, align 1
  ret i64 %x
}

; Loads whose addresses differ by multiples of their size share one check, with the address
; computations in between moved before it.
; CHECK-LABEL: @run
; CHECK: %q = getelementptr i8, ptr %p, i64 16
; CHECK: %is_aligned = icmp
; CHECK-NOT: icmp
; CHECK: load i64, ptr %p, align 8
; CHECK-NEXT: load i64, ptr %q, align 8
; CHECK: load i64, ptr %p, align 1
; CHECK-NEXT: load i64, ptr %q, align 1
; CHECK: ret i64
define i64 @run(ptr %p) {
  %x = load i64, ptr %p, align 1
  %q = getelementptr i8, ptr %p, i64 16
  %y = load i64, ptr %q, align 1
  %s = add i64 %x, %y
  ret i64 %s
}

; Different residues need different checks.
; CHECK-LABEL: @different_residues
; CHECK: %is_aligned = icmp
; CHECK: %is_aligned{{[0-9]+}} = icmp
define i64 @different_residues(ptr %p) {
  %x = load i64, ptr %p, align 1
  %q = getelementptr i8, ptr %p, i64 4
  %y = load i64, ptr %q, align 1
  %s = add i64 %x, %y
  ret i64 %s
}

; A run cannot extend across a store, and loads of another size start their own run.
; CHECK-LABEL: @barrier
; CHECK: %is_aligned = icmp
; CHECK: store i8 0, ptr %r
; CHECK: %is_aligned{{[0-9]+}} = icmp
; CHECK: %is_aligned{{[0-9]+}} = icmp
; CHECK-NOT: icmp
; CHECK: ret
define i64 @barrier(ptr %p, ptr %r) {
  %x = load i64, ptr %p, align 1
  store i8 0, ptr %r
  %q = getelementptr i8, ptr %p, i64 8
  %y = load i64, ptr %q, align 1
  %z = load i32, ptr %q, align 1
  %z64 = zext i32 %z to i64
  %s = add i64 %x, %y
  %t = add i64 %s, %z64
  ret i64 %t
}

; Interleaved runs of different sizes stay disjoint.
; CHECK-LABEL: @interleaved
; CHECK: %is_aligned = icmp
; CHECK: %is_aligned{{[0-9]+}} = icmp
; CHECK-NOT: icmp
; CHECK: ret
define i64 @interleaved(ptr %p) {
  %a = load i32, ptr %p, align 1
  %q = getelementptr i8, ptr %p, i64 4
  %b = load i64, ptr %q, align 1
  %r = getelementptr i8, ptr %p, i64 8
  %c = load i32, ptr %r, align 1
  %a64 = zext i32 %a to i64
  %c64 = zext i32 %c to i64
  %s = add i64 %a64, %b
  %t = add i64 %s, %c64
  ret i64 %t
}

; Both loads of a run are tied to their own phi.
; CHECK-LABEL: @run_phis
; CHECK: [[A1:%.+]] = load i64, ptr %p, align 8
; CHECK-NEXT: [[A2:%.+]] = load i64, ptr %q, align 8
; CHECK: [[U1:%.+]] = load i64, ptr %p, align 1
; CHECK-NEXT: [[U2:%.+]] = load i64, ptr %q, align 1
; CHECK-DAG: %x = phi i64 [ [[A1]], %{{.+}} ], [ [[U1]], %{{.+}} ]
; CHECK-DAG: %y = phi i64 [ [[A2]], %{{.+}} ], [ [[U2]], %{{.+}} ]
define i64 @run_phis(ptr %p) {
  %x = load i64, ptr %p, align 1
  %q = getelementptr i8, ptr %p, i64 8
  %y = load i64, ptr %q, align 1
  %s = sub i64 %x, %y
  ret i64 %s
}

; Calls, atomics and volatile accesses end a run.
; CHECK-LABEL: @barriers
; CHECK-COUNT-4: %is_aligned{{[0-9]*}} = icmp
; CHECK-NOT: icmp
; CHECK: ret
define i64 @barriers(ptr %p, ptr %r) {
  %a = load i64, ptr %p, align 1
  call void @f()
  %q1 = getelementptr i8, ptr %p, i64 8
  %b = load i64, ptr %q1, align 1
  %v = load atomic i8, ptr %r acquire, align 1
  %q2 = getelementptr i8, ptr %p, i64 16
  %c = load i64, ptr %q2, align 1
  %w = load volatile i8, ptr %r, align 1
  %q3 = getelementptr i8, ptr %p, i64 24
  %d = load i64, ptr %q3, align 1
  %s1 = add i64 %a, %b
  %s2 = add i64 %s1, %c
  %s3 = add i64 %s2, %d
  ret i64 %s3
}

; A load whose address computation cannot be moved before the first load of a run gets its own
; check rather than none.
; CHECK-LABEL: @unhoistable
; CHECK: %is_aligned = icmp
; CHECK: load i64, ptr %p0, align 8
; CHECK: %d2 = udiv i64 %n, %den
; CHECK: %is_aligned{{[0-9]+}} = icmp
; CHECK: load i64, ptr %q, align 8
define i64 @unhoistable(ptr %p, i64 %n, i64 %den) {
  %d = udiv i64 %n, %den
  %p0 = getelementptr i8, ptr %p, i64 %d
  %x = load i64, ptr %p0, align 1
  %d2 = udiv i64 %n, %den
  %q0 = getelementptr i8, ptr %p, i64 %d2
  %q = getelementptr i8, ptr %q0, i64 8
  %y = load i64, ptr %q, align 1
  %s = add i64 %x, %y
  ret i64 %s
}

; Floating-point loads are versioned like integer loads.
; CHECK-LABEL: @float
; CHECK: %is_aligned = icmp
; CHECK: load double, ptr %p, align 8
; CHECK: load double, ptr %p, align 1
define double @float(ptr %p) {
  %x = load double, ptr %p, align 1
  ret double %x
}

; Functions optimized for size are left alone.
; CHECK-LABEL: @optsize
; CHECK-NOT: is_aligned
; CHECK: ret
define i64 @optsize(ptr %p) optsize {
  %x = load i64, ptr %p, align 1
  ret i64 %x
}

; Interleaved runs sharing an address computation: the first run moves it out of the block of
; the second.
; CHECK-LABEL: @interleaved_shared
; CHECK: %r = getelementptr i8, ptr %p, i64 8
; CHECK: %is_aligned = icmp
; CHECK: load i32, ptr %r, align 4
; CHECK: %s = getelementptr i8, ptr %r, i64 8
; CHECK: %is_aligned{{[0-9]+}} = icmp
; CHECK: load i64, ptr %s, align 8
define i64 @interleaved_shared(ptr %p) {
  %a = load i32, ptr %p, align 1
  %b = load i64, ptr %p, align 1
  %r = getelementptr i8, ptr %p, i64 8
  %c = load i32, ptr %r, align 1
  %s = getelementptr i8, ptr %r, i64 8
  %d = load i64, ptr %s, align 1
  %a64 = zext i32 %a to i64
  %c64 = zext i32 %c to i64
  %t1 = add i64 %a64, %b
  %t2 = add i64 %t1, %c64
  %t3 = add i64 %t2, %d
  ret i64 %t3
}

; Known address bits decide statically.
; CHECK-LABEL: @known
; CHECK-NOT: is_aligned
; CHECK: load i64, ptr %aligned, align 8
; CHECK: load i64, ptr %misaligned, align 1
define i64 @known(ptr %p) {
  %aligned = call ptr @llvm.ptrmask.p0.i64(ptr %p, i64 -8)
  %x = load i64, ptr %aligned, align 1
  %misaligned = getelementptr i8, ptr %aligned, i64 3
  %y = load i64, ptr %misaligned, align 1
  %s = add i64 %x, %y
  ret i64 %s
}

; Atomic, volatile, byte-sized and sufficiently aligned loads are left alone.
; CHECK-LABEL: @ignored
; CHECK-NOT: is_aligned
; CHECK: ret
define i64 @ignored(ptr %p, ptr %q) {
  %a = load atomic i64, ptr %p unordered, align 1
  %b = load volatile i64, ptr %p, align 1
  %c = load i8, ptr %q, align 1
  %d = load i64, ptr %q, align 8
  %c64 = zext i8 %c to i64
  %s = add i64 %a, %b
  %t = add i64 %s, %c64
  %u = add i64 %t, %d
  ret i64 %u
}

declare void @f()

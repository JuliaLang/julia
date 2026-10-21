; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%{shlibext} -passes='CancellationLowering' -S %s | FileCheck %s

; Intrinsics are classified by their memory effects: read-only ones are never
; unsafe points, and ones that only write through their pointer arguments are
; unsafe where a store is (JuliaLang/julia#62856, #63092).

declare i32 @julia.cancellation_point()
declare ptr @julia.get_pgcstack()
declare void @some_unsafe_call()
declare token @llvm.julia.gc_preserve_begin(...)
declare void @llvm.julia.gc_preserve_end(token)
declare <8 x i64> @llvm.masked.load.v8i64.p0(ptr, i32 immarg, <8 x i1>, <8 x i64>)
declare <8 x i64> @llvm.ctpop.v8i64(<8 x i64>)
declare i64 @llvm.vector.reduce.add.v8i64(<8 x i64>)
declare i64 @llvm.smax.i64(i64, i64)
declare i64 @llvm.ctlz.i64(i64, i1 immarg)
declare void @llvm.memset.p0.i64(ptr, i8, i64, i1 immarg)
declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1 immarg)
declare double @llvm.fma.f64(double, double, double)
declare ptr @llvm.threadlocal.address.p0(ptr)
declare void @llvm.memmove.p0.p0.i64(ptr, ptr, i64, i1 immarg)
declare void @llvm.set.rounding(i32)

@tlsvar = thread_local global i64 0

; Inside a caller's region, where stores are not unsafe points: only the
; untagged call after the loop drops the region.
define i64 @test_pure_intrinsics_stay_spanned(ptr %chunks, ptr %lits, ptr %lits_inv, i64 %stride, i64 %LF, ptr %tmp, ptr addrspace(10) %root) "julia.ipo_reset_safe" {
entry:
; CHECK-LABEL: @test_pure_intrinsics_stay_spanned
; CHECK-NOT: setjmp
; CHECK: %reset_ctx_ptr = getelementptr i8, ptr %current_task
; CHECK-NOT: store atomic volatile ptr null, ptr %reset_ctx_ptr
; CHECK: call token (...) @llvm.julia.gc_preserve_begin
; CHECK-NOT: store atomic volatile ptr null, ptr %reset_ctx_ptr
; CHECK: call void @llvm.memcpy.p0.p0.i64
; CHECK-NOT: store atomic volatile ptr null, ptr %reset_ctx_ptr
; CHECK: loop:
; CHECK-NOT: store atomic volatile ptr null, ptr %reset_ctx_ptr
; CHECK: call <8 x i64> @llvm.masked.load.v8i64.p0
; CHECK-NOT: store atomic volatile ptr null, ptr %reset_ctx_ptr
; CHECK: call <8 x i64> @llvm.ctpop.v8i64
; CHECK-NOT: store atomic volatile ptr null, ptr %reset_ctx_ptr
; CHECK: call i64 @llvm.vector.reduce.add.v8i64
; CHECK-NOT: store atomic volatile ptr null, ptr %reset_ctx_ptr
; CHECK: call i64 @llvm.smax.i64
; CHECK-NOT: store atomic volatile ptr null, ptr %reset_ctx_ptr
; CHECK: exit:
; CHECK-NEXT: call void @llvm.julia.gc_preserve_end
; CHECK-NEXT: store atomic volatile ptr null, ptr %reset_ctx_ptr release
; CHECK-NEXT: fence syncscope("singlethread") seq_cst
; CHECK-NEXT: call void @some_unsafe_call()
; CHECK-NOT: store
; CHECK: ret i64
  %pgcstack = call ptr @julia.get_pgcstack()
  %tok = call token (...) @llvm.julia.gc_preserve_begin(ptr addrspace(10) %root)
  call void @llvm.memcpy.p0.p0.i64(ptr %tmp, ptr %chunks, i64 64, i1 false)
  %x = load <8 x i64>, ptr %tmp, align 8
  br label %loop

loop:
  %i = phi i64 [ 0, %entry ], [ %i.next, %loop ]
  %acc = phi i64 [ 0, %entry ], [ %acc.next, %loop ]
  %off = mul i64 %i, %stride
  %p = getelementptr inbounds i64, ptr %lits, i64 %off
  %q = getelementptr inbounds i64, ptr %lits_inv, i64 %off
  %lit = call <8 x i64> @llvm.masked.load.v8i64.p0(ptr %p, i32 8, <8 x i1> <i1 true, i1 true, i1 true, i1 true, i1 true, i1 false, i1 false, i1 false>, <8 x i64> poison)
  %inv = call <8 x i64> @llvm.masked.load.v8i64.p0(ptr %q, i32 8, <8 x i1> <i1 true, i1 true, i1 true, i1 true, i1 true, i1 false, i1 false, i1 false>, <8 x i64> poison)
  %t0 = xor <8 x i64> %lit, %inv
  %t1 = and <8 x i64> %t0, %x
  %t2 = xor <8 x i64> %t1, %lit
  %pc = call <8 x i64> @llvm.ctpop.v8i64(<8 x i64> %t2)
  %c = call i64 @llvm.vector.reduce.add.v8i64(<8 x i64> %pc)
  %d = sub i64 %LF, %c
  %m = call i64 @llvm.smax.i64(i64 %d, i64 0)
  %acc.next = add i64 %acc, %m
  %i.next = add nuw nsw i64 %i, 1
  %done = icmp eq i64 %i.next, 10
  br i1 %done, label %exit, label %loop

exit:
  call void @llvm.julia.gc_preserve_end(token %tok)
  call void @some_unsafe_call()
  ret i64 %acc.next
}

; With cancellation points of its own, where stores are unsafe points: only
; the memset drops the region.
define i64 @test_instrumented_intrinsics(ptr %buf, i64 %v, ptr addrspace(10) %root) {
entry:
; CHECK-LABEL: @test_instrumented_intrinsics
; CHECK: call i32 @{{.*}}setjmp
; CHECK-NEXT: store atomic volatile ptr %cancel_ucontext, ptr %reset_ctx_ptr release
; CHECK: cancel_pt_cont:
; CHECK-NOT: store atomic volatile ptr null, ptr %reset_ctx_ptr
; CHECK: %tok = call token (...) @llvm.julia.gc_preserve_begin
; CHECK-NOT: store atomic volatile ptr null, ptr %reset_ctx_ptr
; CHECK: %lz = call i64 @llvm.ctlz.i64
; CHECK-NOT: store atomic volatile ptr null, ptr %reset_ctx_ptr
; CHECK: %m = call i64 @llvm.smax.i64
; CHECK-NEXT: store atomic volatile ptr null, ptr %reset_ctx_ptr release
; CHECK-NEXT: fence syncscope("singlethread") seq_cst
; CHECK-NEXT: call void @llvm.memset.p0.i64
; CHECK-NEXT: call void @llvm.julia.gc_preserve_end
; CHECK-NEXT: store atomic volatile ptr null, ptr %reset_ctx_ptr release
; CHECK-NEXT: ret i64 %m
  %pgcstack = call ptr @julia.get_pgcstack()
  %result = call i32 @julia.cancellation_point()
  %tok = call token (...) @llvm.julia.gc_preserve_begin(ptr addrspace(10) %root)
  %lz = call i64 @llvm.ctlz.i64(i64 %v, i1 false)
  %m = call i64 @llvm.smax.i64(i64 %lz, i64 1)
  call void @llvm.memset.p0.i64(ptr %buf, i8 0, i64 %m, i1 false)
  call void @llvm.julia.gc_preserve_end(token %tok)
  ret i64 %m
}

; fma, threadlocal.address and memmove follow their effects; set.rounding
; writes inaccessible memory, so it drops the region like a call.
define double @test_effects_classify(double %a, double %b, double %c, ptr %p, i64 %n) "julia.ipo_reset_safe" {
entry:
; CHECK-LABEL: @test_effects_classify
; CHECK-NOT: store atomic volatile ptr null, ptr %reset_ctx_ptr
; CHECK: %f = call double @llvm.fma.f64
; CHECK-NEXT: %t = call ptr @llvm.threadlocal.address.p0
; CHECK-NEXT: %q = getelementptr
; CHECK-NEXT: call void @llvm.memmove.p0.p0.i64
; CHECK-NEXT: store atomic volatile ptr null, ptr %reset_ctx_ptr release
; CHECK-NEXT: fence syncscope("singlethread") seq_cst
; CHECK-NEXT: call void @llvm.set.rounding
  %pgcstack = call ptr @julia.get_pgcstack()
  %f = call double @llvm.fma.f64(double %a, double %b, double %c)
  %t = call ptr @llvm.threadlocal.address.p0(ptr @tlsvar)
  %q = getelementptr i8, ptr %p, i64 1
  call void @llvm.memmove.p0.p0.i64(ptr %q, ptr %p, i64 %n, i1 false)
  call void @llvm.set.rounding(i32 1)
  %v = load i64, ptr %t, align 8
  %vf = sitofp i64 %v to double
  %r = fadd double %f, %vf
  ret double %r
}

; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='CancellationLowering' -S %s | FileCheck %s

; NOTE: the CHECK lines assume a 64-bit non-Windows host (the jl_reset_ctx_t
; field offsets 8/16/24 and the two-argument setjmp form); the llvmpasses
; suite only runs in such environments.

declare i32 @julia.cancellation_point()
declare ptr @julia.get_pgcstack()
declare void @some_unsafe_call()
declare void @some_safe_call()
declare ptr @julia.gc_alloc_obj(ptr, i64, ptr)
declare ptr @ijl_box_int64(i64)
declare ptr @ijl_apply_generic(ptr, ptr, i32)
declare i32 @tail_target(ptr)
declare ptr @julia.pointer_from_objref(ptr addrspace(11))

; Test basic cancellation point lowering with reset_ctx cleared before return
define i32 @test_cancellation_point() {
entry:
; CHECK-LABEL: @test_cancellation_point
; CHECK: %cancel_ucontext = alloca
; CHECK: %pgcstack = call ptr @julia.get_pgcstack()
; CHECK: %current_task = getelementptr i8, ptr %pgcstack
; CHECK: %reset_ctx_ptr = getelementptr i8, ptr %current_task
; If this frame's region is still published (an earlier point of this frame
; established it and nothing tore it), the whole establishment - safepoint
; included - is skipped: every delivery throws, so landing at the earlier
; setjmp is equivalent.
; CHECK: %reset_ctx_prev = load atomic volatile ptr, ptr %reset_ctx_ptr monotonic
; CHECK-NEXT: %region_live = icmp eq ptr %reset_ctx_prev, %cancel_ucontext
; CHECK-NEXT: br i1 %region_live, label %cancel_pt_cont, label %cancel_establish
; The buffer may only be published while its contents are valid: unpublish,
; write it (setjmp), then publish it. All the bookkeeping stores are volatile
; (delivery observes them asynchronously), and the signal fence keeps the
; buffer writes from being reordered above the unpublish. A GC safepoint is
; emitted while the buffer is unpublished.
; CHECK: cancel_establish:
; CHECK: store atomic volatile ptr null, ptr %reset_ctx_ptr release
; CHECK-NEXT: fence syncscope("singlethread") seq_cst
; CHECK: call void @julia.safepoint(ptr %safepoint)
; The sp discriminator word is stamped first; then the task's gcstack and eh
; are saved (restored by a delivered reset); the jump buffer lives after them.
; CHECK: store atomic volatile i64 %{{.*}}, ptr %cancel_ucontext monotonic
; CHECK-NEXT: %saved_gcstack = load ptr, ptr %pgcstack
; CHECK-NEXT: %cancel_gcstack_slot = getelementptr i8, ptr %cancel_ucontext, i64 8
; CHECK-NEXT: store volatile ptr %saved_gcstack, ptr %cancel_gcstack_slot
; CHECK-NEXT: %saved_eh = load ptr, ptr %eh_field_ptr
; CHECK-NEXT: %cancel_eh_slot = getelementptr i8, ptr %cancel_ucontext, i64 16
; CHECK-NEXT: store volatile ptr %saved_eh, ptr %cancel_eh_slot
; CHECK-NEXT: %cancel_mctx = getelementptr i8, ptr %cancel_ucontext, i64 24
; CHECK-NEXT: %{{.*}} = call i32 @{{.*}}setjmp{{.*}}(ptr %cancel_mctx, i32 0)
; CHECK-NEXT: store atomic volatile ptr %cancel_ucontext, ptr %reset_ctx_ptr release
; The point's value merges 0 (region already live) with the setjmp return
; CHECK: cancel_pt_cont:
; CHECK-NEXT: %{{.*}} = phi i32 [ 0, %entry ], [ %{{.*}}, %cancel_establish ]
; CHECK-NOT: call i32 @julia.cancellation_point()
; CHECK: store atomic volatile ptr null, ptr %reset_ctx_ptr release
; CHECK-NEXT: ret i32
  %pgcstack = call ptr @julia.get_pgcstack()
  %result = call i32 @julia.cancellation_point()
  ret i32 %result
}

; Test that unsafe calls get reset_ctx = NULL inserted before them, separated
; from the call by a signal fence so no later pass can move the call above
; the clear
define void @test_unsafe_call() {
entry:
; CHECK-LABEL: @test_unsafe_call
; CHECK: %cancel_ucontext = alloca
; CHECK: %pgcstack = call ptr @julia.get_pgcstack()
; CHECK: %current_task = getelementptr i8, ptr %pgcstack
; CHECK: %reset_ctx_ptr = getelementptr i8, ptr %current_task
; CHECK: call i32 @{{.*}}setjmp
; CHECK-NEXT: store atomic volatile ptr %cancel_ucontext, ptr %reset_ctx_ptr release
; The unsafe call should have reset_ctx = NULL (plus the fence) before it
; CHECK: store atomic volatile ptr null, ptr %reset_ctx_ptr release
; CHECK-NEXT: fence syncscope("singlethread") seq_cst
; CHECK-NEXT: call void @some_unsafe_call()
; Also reset_ctx = NULL before the return (no fence needed: an early clear
; only drops a delivery)
; CHECK: store atomic volatile ptr null, ptr %reset_ctx_ptr release
; CHECK-NEXT: ret void
  %pgcstack = call ptr @julia.get_pgcstack()
  %result = call i32 @julia.cancellation_point()
  call void @some_unsafe_call()
  ret void
}

; Test that calls with reset_safe metadata don't get reset_ctx = NULL before them
; but still get reset_ctx = NULL before return
define void @test_safe_call() {
entry:
; CHECK-LABEL: @test_safe_call
; CHECK: %cancel_ucontext = alloca
; CHECK: %pgcstack = call ptr @julia.get_pgcstack()
; CHECK: %current_task = getelementptr i8, ptr %pgcstack
; CHECK: %reset_ctx_ptr = getelementptr i8, ptr %current_task
; CHECK: call i32 @{{.*}}setjmp
; The safe call should NOT have reset_ctx = NULL before it
; CHECK: call void @some_safe_call(), !julia.reset_safe
; But reset_ctx = NULL should be before the return
; CHECK: store atomic volatile ptr null, ptr %reset_ctx_ptr release
; CHECK-NEXT: ret void
  %pgcstack = call ptr @julia.get_pgcstack()
  %result = call i32 @julia.cancellation_point()
  call void @some_safe_call(), !julia.reset_safe !0
  ret void
}

; Test function without cancellation points is unchanged: it is not marked
; julia.ipo_reset_safe, so no reset region can be open while it runs and no
; instrumentation is needed - even around allocation.
define ptr @test_no_cancellation_point(i64 %v) {
entry:
; CHECK-LABEL: @test_no_cancellation_point
; CHECK-NOT: setjmp
; CHECK-NOT: reset_ctx
; CHECK: call void @some_unsafe_call()
; CHECK-NOT: reset_ctx
; CHECK: call ptr @julia.gc_alloc_obj
  %pgcstack = call ptr @julia.get_pgcstack()
  call void @some_unsafe_call()
  %obj = call ptr @julia.gc_alloc_obj(ptr %pgcstack, i64 16, ptr null)
  ret ptr %obj
}

; A function codegen marked julia.ipo_reset_safe (its own IPO effects are
; reset_safe) may be running inside a caller's reset region kept open across
; a reset_safe call, so machinery the runtime inserts implicitly - runtime
; library calls - and any call not itself tagged reset_safe eagerly drops
; the reset context. Tagged (non-implicit) calls stay spanned. Allocations
; and write barriers do not drop: sites that may execute with a region
; published are annotated with julia.reset_region instead, and FinalLowerGC
; lowers them to reset-safe runtime entry points that unpublish/republish
; the region themselves. A site on a path where the region was already
; dropped (here: after the untagged call) is not annotated. No region is
; established (no setjmp), and the region is NOT cleared at the return: it
; belongs to the caller.
define ptr @test_implicit_runtime_drop(i64 %v) "julia.ipo_reset_safe" {
entry:
; CHECK-LABEL: @test_implicit_runtime_drop
; CHECK-NOT: setjmp
; CHECK: %reset_ctx_ptr = getelementptr i8, ptr %current_task
; CHECK: %obj = call ptr @julia.gc_alloc_obj({{.*}}), !julia.reset_region
; CHECK-NEXT: call void @some_safe_call(), !julia.reset_safe
; CHECK-NEXT: store atomic volatile ptr null, ptr %reset_ctx_ptr release
; CHECK-NEXT: fence syncscope("singlethread") seq_cst
; CHECK-NEXT: %box = call ptr @ijl_box_int64(i64 %v)
; CHECK: %obj2 = call ptr @julia.gc_alloc_obj({{.*}}){{$}}
; CHECK-NOT: setjmp
; CHECK-NOT: store
; CHECK: ret ptr %obj
  %pgcstack = call ptr @julia.get_pgcstack()
  %obj = call ptr @julia.gc_alloc_obj(ptr %pgcstack, i64 16, ptr null)
  call void @some_safe_call(), !julia.reset_safe !0
  %box = call ptr @ijl_box_int64(i64 %v)
  %obj2 = call ptr @julia.gc_alloc_obj(ptr %pgcstack, i64 16, ptr null)
  ret ptr %obj
}

; In a function with cancellation points of its own, an allocation site
; between the region's publication and the next drop is annotated (not
; dropped): FinalLowerGC lowers it to a reset-safe entry point.
define ptr @test_alloc_in_region() {
entry:
; CHECK-LABEL: @test_alloc_in_region
; CHECK: call i32 @{{.*}}setjmp
; CHECK-NEXT: store atomic volatile ptr %cancel_ucontext, ptr %reset_ctx_ptr release
; CHECK: cancel_pt_cont:
; CHECK: %obj = call ptr @julia.gc_alloc_obj({{.*}}), !julia.reset_region
; CHECK: store atomic volatile ptr null, ptr %reset_ctx_ptr release
; CHECK-NEXT: ret ptr %obj
  %pgcstack = call ptr @julia.get_pgcstack()
  %result = call i32 @julia.cancellation_point()
  %obj = call ptr @julia.gc_alloc_obj(ptr %pgcstack, i64 16, ptr null)
  ret ptr %obj
}

; Even a reset_safe-tagged call is an unsafe point when it is
; implicitly-inserted runtime machinery: the tag describes the source
; statement's IPO contract, not the runtime frames implementing it (e.g. a
; dynamic dispatch inside a reset_safe statement).
define void @test_tagged_implicit_still_unsafe(ptr %x) {
entry:
; CHECK-LABEL: @test_tagged_implicit_still_unsafe
; CHECK: call i32 @{{.*}}setjmp
; CHECK-NEXT: store atomic volatile ptr %cancel_ucontext, ptr %reset_ctx_ptr release
; CHECK: store atomic volatile ptr null, ptr %reset_ctx_ptr release
; CHECK-NEXT: fence syncscope("singlethread") seq_cst
; CHECK-NEXT: %r = call ptr @ijl_apply_generic
  %pgcstack = call ptr @julia.get_pgcstack()
  %result = call i32 @julia.cancellation_point()
  %r = call ptr @ijl_apply_generic(ptr %x, ptr null, i32 0), !julia.reset_safe !0
  ret void
}

; Atomic stores are unsafe points like plain ones: a reset delivered around
; e.g. a lock-release store would leave the lock owned forever. Only the
; pass's own bookkeeping stores (which carry the reset_safe metadata) are
; exempt.
define void @test_atomic_store_unsafe(ptr %lock) {
entry:
; CHECK-LABEL: @test_atomic_store_unsafe
; CHECK: call i32 @{{.*}}setjmp
; CHECK-NEXT: store atomic volatile ptr %cancel_ucontext, ptr %reset_ctx_ptr release
; CHECK: store atomic volatile ptr null, ptr %reset_ctx_ptr release
; CHECK-NEXT: fence syncscope("singlethread") seq_cst
; CHECK-NEXT: store atomic i64 0, ptr %lock release
; CHECK: store atomic volatile ptr null, ptr %reset_ctx_ptr release
; CHECK-NEXT: ret void
  %pgcstack = call ptr @julia.get_pgcstack()
  %result = call i32 @julia.cancellation_point()
  store atomic i64 0, ptr %lock release, align 8
  ret void
}

; Atomic read-modify-write operations publish state a reset could tear
; (e.g. a cmpxchg acquiring a user spin lock).
define void @test_atomic_rmw_unsafe(ptr %lock, ptr %counter) {
entry:
; CHECK-LABEL: @test_atomic_rmw_unsafe
; CHECK: call i32 @{{.*}}setjmp
; CHECK-NEXT: store atomic volatile ptr %cancel_ucontext, ptr %reset_ctx_ptr release
; CHECK: store atomic volatile ptr null, ptr %reset_ctx_ptr release
; CHECK-NEXT: fence syncscope("singlethread") seq_cst
; CHECK-NEXT: cmpxchg ptr %lock, i64 0, i64 1 acq_rel monotonic
; CHECK: store atomic volatile ptr null, ptr %reset_ctx_ptr release
; CHECK-NEXT: fence syncscope("singlethread") seq_cst
; CHECK-NEXT: atomicrmw add ptr %counter, i64 1 seq_cst
; CHECK: store atomic volatile ptr null, ptr %reset_ctx_ptr release
; CHECK-NEXT: ret void
  %pgcstack = call ptr @julia.get_pgcstack()
  %result = call i32 @julia.cancellation_point()
  %acq = cmpxchg ptr %lock, i64 0, i64 1 acq_rel monotonic, align 8
  %old = atomicrmw add ptr %counter, i64 1 seq_cst, align 8
  ret void
}

; Volatile loads can have side effects (MMIO, read-to-clear hardware
; registers): a reset delivered after the load has taken effect must not
; silently abandon it.
define i64 @test_volatile_load_unsafe(ptr %mmio) {
entry:
; CHECK-LABEL: @test_volatile_load_unsafe
; CHECK: call i32 @{{.*}}setjmp
; CHECK-NEXT: store atomic volatile ptr %cancel_ucontext, ptr %reset_ctx_ptr release
; CHECK: store atomic volatile ptr null, ptr %reset_ctx_ptr release
; CHECK-NEXT: fence syncscope("singlethread") seq_cst
; CHECK-NEXT: %v = load volatile i64, ptr %mmio
; CHECK: store atomic volatile ptr null, ptr %reset_ctx_ptr release
; CHECK-NEXT: ret i64 %v
  %pgcstack = call ptr @julia.get_pgcstack()
  %result = call i32 @julia.cancellation_point()
  %v = load volatile i64, ptr %mmio, align 8
  ret i64 %v
}

; A musttail call must stay immediately adjacent to its return, so the
; return teardown is inserted in front of the call - unconditionally, even
; when the unsafe-point clear before the call already tore the region down
; (metadata is not a reliable proxy: exempt calls get no clear).
define i32 @test_musttail(ptr %arg) {
entry:
; CHECK-LABEL: @test_musttail
; CHECK: call i32 @{{.*}}setjmp
; CHECK: store atomic volatile ptr null, ptr %reset_ctx_ptr release
; CHECK-NEXT: fence syncscope("singlethread") seq_cst
; CHECK-NEXT: store atomic volatile ptr null, ptr %reset_ctx_ptr release
; CHECK-NEXT: %r = musttail call i32 @tail_target(ptr %arg)
; CHECK-NEXT: ret i32 %r
  %pgcstack = call ptr @julia.get_pgcstack()
  %result = call i32 @julia.cancellation_point()
  %r = musttail call i32 @tail_target(ptr %arg)
  ret i32 %r
}

; A reset_safe-tagged musttail call gets no unsafe-point clear, so the
; return teardown goes in front of the call instead (no fence needed: an
; early clear only drops a delivery).
define i32 @test_musttail_safe(ptr %arg) {
entry:
; CHECK-LABEL: @test_musttail_safe
; CHECK: call i32 @{{.*}}setjmp
; CHECK-NEXT: store atomic volatile ptr %cancel_ucontext, ptr %reset_ctx_ptr release
; CHECK: cancel_pt_cont:
; CHECK: store atomic volatile ptr null, ptr %reset_ctx_ptr release
; CHECK-NEXT: %r = musttail call i32 @tail_target(ptr %arg), !julia.reset_safe
; CHECK-NEXT: ret i32 %r
  %pgcstack = call ptr @julia.get_pgcstack()
  %result = call i32 @julia.cancellation_point()
  %r = musttail call i32 @tail_target(ptr %arg), !julia.reset_safe !0
  ret i32 %r
}

; Instructions not reachable from any cancellation point can never observe
; an open region: no clears are inserted for them (the store and call before
; the point below are untouched; only those after it are guarded).
define void @test_unreachable_before_point(ptr %p) {
entry:
; CHECK-LABEL: @test_unreachable_before_point
; CHECK: %reset_ctx_ptr = getelementptr i8, ptr %current_task
; CHECK-NOT: store atomic volatile
; CHECK: store i64 1, ptr %p
; CHECK-NEXT: call void @some_unsafe_call()
; CHECK: call i32 @{{.*}}setjmp
; CHECK: store atomic volatile ptr null, ptr %reset_ctx_ptr release
; CHECK-NEXT: fence syncscope("singlethread") seq_cst
; CHECK-NEXT: store i64 2, ptr %p
; CHECK: store atomic volatile ptr null, ptr %reset_ctx_ptr release
; CHECK-NEXT: ret void
  %pgcstack = call ptr @julia.get_pgcstack()
  store i64 1, ptr %p
  call void @some_unsafe_call()
  %result = call i32 @julia.cancellation_point()
  store i64 2, ptr %p
  ret void
}

; An exempt call (no unsafe-point clear, e.g. julia.pointer_from_objref)
; that is a musttail still must not leave the stack reset context published
; past the return: the teardown goes in front of the call.
define ptr @test_musttail_exempt(ptr addrspace(11) %obj) {
entry:
; CHECK-LABEL: @test_musttail_exempt
; CHECK: call i32 @{{.*}}setjmp
; CHECK-NEXT: store atomic volatile ptr %cancel_ucontext, ptr %reset_ctx_ptr release
; CHECK: cancel_pt_cont:
; CHECK: store atomic volatile ptr null, ptr %reset_ctx_ptr release
; CHECK-NEXT: %r = musttail call ptr @julia.pointer_from_objref(ptr addrspace(11) %obj)
; CHECK-NEXT: ret ptr %r
  %pgcstack = call ptr @julia.get_pgcstack()
  %result = call i32 @julia.cancellation_point()
  %r = musttail call ptr @julia.pointer_from_objref(ptr addrspace(11) %obj)
  ret ptr %r
}

!0 = !{}

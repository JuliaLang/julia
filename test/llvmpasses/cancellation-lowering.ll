; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='CancellationLowering' -S %s | FileCheck %s

declare i32 @julia.cancellation_point()
declare ptr @julia.get_pgcstack()
declare void @some_unsafe_call()
declare void @some_safe_call()

; Test basic cancellation point lowering with reset_ctx cleared before return
define i32 @test_cancellation_point() {
entry:
; CHECK-LABEL: @test_cancellation_point
; CHECK: %cancel_ucontext = alloca
; CHECK: %pgcstack = call ptr @julia.get_pgcstack()
; CHECK: %current_task = getelementptr i8, ptr %pgcstack
; CHECK: %reset_ctx_ptr = getelementptr i8, ptr %current_task
; CHECK: store atomic ptr %cancel_ucontext, ptr %reset_ctx_ptr release
; CHECK: %{{.*}} = call i32 @{{.*}}setjmp{{.*}}(ptr %cancel_ucontext)
; CHECK-NOT: call i32 @julia.cancellation_point()
; CHECK: store atomic ptr null, ptr %reset_ctx_ptr release
; CHECK-NEXT: ret i32
  %pgcstack = call ptr @julia.get_pgcstack()
  %result = call i32 @julia.cancellation_point()
  ret i32 %result
}

; Test that unsafe calls get reset_ctx = NULL inserted before them
define void @test_unsafe_call() {
entry:
; CHECK-LABEL: @test_unsafe_call
; CHECK: %cancel_ucontext = alloca
; CHECK: %pgcstack = call ptr @julia.get_pgcstack()
; CHECK: %current_task = getelementptr i8, ptr %pgcstack
; CHECK: %reset_ctx_ptr = getelementptr i8, ptr %current_task
; CHECK: store atomic ptr %cancel_ucontext, ptr %reset_ctx_ptr release
; CHECK: call i32 @{{.*}}setjmp
; The unsafe call should have reset_ctx = NULL before it
; CHECK: store atomic ptr null, ptr %reset_ctx_ptr release
; CHECK-NEXT: call void @some_unsafe_call()
; Also reset_ctx = NULL before the return
; CHECK: store atomic ptr null, ptr %reset_ctx_ptr release
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
; CHECK: store atomic ptr null, ptr %reset_ctx_ptr release
; CHECK-NEXT: ret void
  %pgcstack = call ptr @julia.get_pgcstack()
  %result = call i32 @julia.cancellation_point()
  call void @some_safe_call(), !julia.reset_safe !0
  ret void
}

; Test function without cancellation points is unchanged
define void @test_no_cancellation_point() {
entry:
; CHECK-LABEL: @test_no_cancellation_point
; CHECK-NOT: setjmp
; CHECK-NOT: reset_ctx
; CHECK: call void @some_unsafe_call()
  call void @some_unsafe_call()
  ret void
}

!0 = !{}

; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='function(LowerExcHandlers)' -S %s | FileCheck %s
; ModuleID = 'lower-handlers.ll'
; ModuleID = 'lower-handlers2.ll'
source_filename = "lower-handlers.ll"

declare ptr @julia.get_pgcstack()

declare i64 @ijl_excstack_state(ptr)

declare { i32, ptr } @julia.except_enter(ptr)

declare void @ijl_pop_handler(ptr, i32)

declare void @ijl_pop_handler_noexcept(ptr, i32)

declare void @ijl_restore_excstack(ptr, i64)

declare ptr @julia.ptls_states()

define void @simple() {
top:
  %pgcstack = call ptr @julia.get_pgcstack()
; CHECK: call void @llvm.lifetime.start
; CHECK: call void @ijl_enter_handler
; CHECK: setjmp
  %rb = call { i32, ptr } @julia.except_enter(ptr null)
  %r = extractvalue { i32, ptr } %rb, 0
  %b = extractvalue { i32, ptr } %rb, 1
  %cmp = icmp eq i32 %r, 0
  br i1 %cmp, label %try, label %catch

try:                                              ; preds = %top
  %lcssa = phi { i32, ptr } [ %rb, %top ]
  br label %after

catch:                                            ; preds = %top
  br label %after

after:                                            ; preds = %catch, %try
  call void @ijl_pop_handler(ptr null, i32 1)
; CHECK: llvm.lifetime.end
  ret void
}

define ptr addrspace(10) @julia_poll_fd2_1135(){
top:
; CHECK: %depth0 = alloca i8, i32 256, align 16
; CHECK: %depth1 = alloca i8, i32 256, align 16
  %pgcstack = call ptr @julia.get_pgcstack()
  %current_task7 = getelementptr inbounds i8, ptr %pgcstack, i64 -112
  %0 = call i64 @ijl_excstack_state(ptr nonnull %current_task7)
; CHECK: call void @llvm.lifetime.start
; CHECK: call void @ijl_enter_handler
; CHECK: setjmp
  %1 = call { i32, ptr } @julia.except_enter(ptr nonnull %current_task7)
  %2 = extractvalue { i32, ptr } %1, 0
  %.not2 = icmp eq i32 %2, 0
  br i1 %.not2, label %try, label %L50

L50:                                              ; preds = %top
  call void @ijl_pop_handler(ptr nonnull %current_task7, i32 1)
; CHECK: llvm.lifetime.end
  unreachable

try:                                              ; preds = %top
  %3 = call i64 @ijl_excstack_state(ptr nonnull %current_task7)
; CHECK: call void @llvm.lifetime.start
; CHECK: call void @ijl_enter_handler
; CHECK: setjmp
  %4 = call { i32, ptr } @julia.except_enter(ptr nonnull %current_task7)
  %5 = extractvalue { i32, ptr } %4, 0
  %6 = icmp eq i32 %5, 0
  br i1 %6, label %common.ret, label %catch_pop14

catch_pop14:                                      ; preds = %try
  call void @ijl_pop_handler(ptr nonnull %current_task7, i32 1)
; CHECK: llvm.lifetime.end
  call void @ijl_restore_excstack(ptr nonnull %current_task7, i64 %3)
  br label %common.ret

common.ret:                                       ; preds = %catch_pop14, %try
  %.sink = phi i32 [ 1, %catch_pop14 ], [ 2, %try ]
  call void @ijl_pop_handler_noexcept(ptr nonnull %current_task7, i32 %.sink)
; CHECK: llvm.lifetime.end
  ret ptr addrspace(10) null
}

; RUN: opt -load-pass-plugin=libjulia-codegen%shlibext -passes='NewSink' -S %s | FileCheck %s

; Tests for exception handling - should NOT sink into or across EH pads
; Based on LLVM Sink pass test patterns

declare void @may_throw()
declare i32 @__gxx_personality_v0(...)

; Test: Don't sink into landing pad blocks
define i64 @test_no_sink_into_landingpad(i64 %a, i64 %bound) personality ptr @__gxx_personality_v0 {
; CHECK-LABEL: @test_no_sink_into_landingpad
entry:
  %tuple = alloca i64, align 8
  ; This store should NOT be sunk into landing pad
  ; CHECK: entry:
  ; CHECK: store i64 %a, ptr %tuple
  store i64 %a, ptr %tuple, align 8
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %try, label %error

try:
  invoke void @may_throw()
    to label %ok unwind label %lpad

lpad:
  ; Landing pad is an EH pad - cannot sink into it
  %lp = landingpad { ptr, i32 }
          cleanup
  %v = load i64, ptr %tuple, align 8
  call void @use(i64 %v)
  resume { ptr, i32 } %lp

error:
  call void @throw_error(ptr %tuple)
  unreachable

ok:
  ret i64 %a
}

; Test: Don't sink EH pad instructions
define i64 @test_no_sink_ehpad_inst(i64 %a, i64 %bound) personality ptr @__gxx_personality_v0 {
; CHECK-LABEL: @test_no_sink_ehpad_inst
entry:
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %try, label %exit

try:
  invoke void @may_throw()
    to label %ok unwind label %lpad

lpad:
  ; EH pad instruction should never be moved
  ; CHECK: lpad:
  ; CHECK: %lp = landingpad
  %lp = landingpad { ptr, i32 }
          cleanup
  br label %cleanup

cleanup:
  call void @use(i64 %a)
  unreachable

ok:
  ret i64 %a

exit:
  ret i64 0
}

; Test: Store only used after invoke can still be sunk to normal destination
; but NOT into the unwind destination
define i64 @test_sink_past_invoke_to_normal(i64 %a, i64 %bound) personality ptr @__gxx_personality_v0 {
; CHECK-LABEL: @test_sink_past_invoke_to_normal
entry:
  %tuple = alloca i64, align 8
  ; Store used only in error (unwind) path
  ; CHECK: entry:
  ; Store should stay in entry because landing pad is involved
  store i64 %a, ptr %tuple, align 8
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %try, label %direct_error

try:
  invoke void @may_throw()
    to label %ok unwind label %lpad

lpad:
  %lp = landingpad { ptr, i32 }
          cleanup
  call void @throw_error(ptr %tuple)
  unreachable

direct_error:
  call void @throw_error(ptr %tuple)
  unreachable

ok:
  ret i64 %a
}

declare void @use(i64)
declare void @throw_error(ptr)

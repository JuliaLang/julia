; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='RemoveJuliaAddrspaces' -S %s | FileCheck %s --check-prefixes=CHECK,OPAQUE

; COM: check that the addrspace of the global itself is removed
; OPAQUE: @ejl_enz_runtime_exc = external global {}
@ejl_enz_runtime_exc = external addrspace(10) global {}

; COM: check that package image fptrs work
@pjlsys_BoundsError_32 = internal global {} addrspace(10)* ({}***, {} addrspace(10)*, [1 x i64] addrspace(11)*)* null
; CHECK: @pjlsys_BoundsError_32 = internal global
; OPAQUE-SAME: ptr null

define i64 @getindex({} addrspace(10)* nonnull align 16 dereferenceable(40)) {
; CHECK-LABEL: @getindex
top:
; CHECK-NOT: addrspace(10)
; CHECK-NOT: addrspacecast
   %1 = addrspacecast {} addrspace(10)* %0 to {} addrspace(11)*
; CHECK: bitcast
   %2 = bitcast {} addrspace(11)* %1 to i64 addrspace(13)* addrspace(11)*
   %3 = load i64 addrspace(13)*, i64 addrspace(13)* addrspace(11)* %2, align 8
   %4 = load i64, i64 addrspace(13)* %3, align 8
  ret i64 %4
}

define void @target_as({ [2 x i64], i64 } addrspace(11)* nocapture nonnull readonly dereferenceable(24)) {
; CHECK-LABEL: @target_as
top:
; CHECK-NOT: addrspace(11)
  %1 = getelementptr inbounds { [2 x i64], i64 }, { [2 x i64], i64 } addrspace(11)* %0, i64 0, i32 1
  %2 = bitcast i64 addrspace(11)* %1 to float* addrspace(11)*
  %3 = load float*, float* addrspace(11)* %2, align 8
; CHECK: addrspacecast
; CHECK: addrspace(1)
  %4 = addrspacecast float* %3 to float addrspace(1)*
  store float 1.000000e+00, float addrspace(1)* %4, align 4
  ret void
}

define nonnull {} addrspace(10)* @constexpr(i64) {
; CHECK-LABEL: @constexpr
top:
; OPAQUE: call ptr inttoptr (i64 139806640486784 to ptr)(ptr inttoptr (i64 139806425039920 to ptr), i64 1)
  %1 = call {} addrspace(10)* inttoptr (i64 139806640486784 to {} addrspace(10)* ({} addrspace(10)*, i64)*)({} addrspace(10)* addrspacecast ({}* inttoptr (i64 139806425039920 to {}*) to {} addrspace(10)*), i64 1)
; CHECK-NOT: addrspacecast
; CHECK-NOT: addrspace
  %2 = addrspacecast {} addrspace(10)* %1 to {} addrspace(11)*
  %3 = bitcast {} addrspace(11)* %2 to i64 addrspace(13)* addrspace(11)*
  %4 = load i64 addrspace(13)*, i64 addrspace(13)* addrspace(11)* %3, align 8
  store i64 %0, i64 addrspace(13)* %4, align 8
  ret {} addrspace(10)* %1
}

; COM: A type used for testing that remove-addrspaces can handle recursive types.
%list = type { i64, %list* }

; COM: There's nothing to remove in this function; but remove-addrspaces shouldn't crash.
define i64 @sum.linked.list() {
; CHECK-LABEL: @sum.linked.list
top:
  %a = alloca %list
  %b = alloca %list
  %c = alloca %list
  %a.car = getelementptr %list, %list* %a, i32 0, i32 0
  %a.cdr = getelementptr %list, %list* %a, i32 0, i32 1
  %b.car = getelementptr %list, %list* %b, i32 0, i32 0
  %b.cdr = getelementptr %list, %list* %b, i32 0, i32 1
  %c.car = getelementptr %list, %list* %c, i32 0, i32 0
  %c.cdr = getelementptr %list, %list* %c, i32 0, i32 1
; COM: Allow remove-addrspaces to rename the type but expect it to use the same prefix.
; CHECK: getelementptr %list
; OPAQUE-SAME: ptr %a
; CHECK: getelementptr %list
; OPAQUE-SAME: ptr %a
; CHECK: getelementptr %list
; OPAQUE-SAME: ptr %b
; CHECK: getelementptr %list
; OPAQUE-SAME: ptr %b
; CHECK: getelementptr %list
; OPAQUE-SAME: ptr %c
; CHECK: getelementptr %list
; OPAQUE-SAME: ptr %c
  store i64 111, i64* %a.car
  store i64 222, i64* %b.car
  store i64 333, i64* %c.car
  store %list* %b, %list** %a.cdr
  store %list* %c, %list** %b.cdr
  store %list* null, %list** %c.cdr
  br label %loop

loop:
  %x = phi %list* [ %a, %top ], [ %x.cdr.value, %loop ]
  %sum.prev = phi i64 [ 0, %top ], [ %sum, %loop ]
  %x.car = getelementptr %list, %list* %x, i32 0, i32 0
  %x.cdr = getelementptr %list, %list* %x, i32 0, i32 1
  %x.car.value = load i64, i64* %x.car
  %x.cdr.value = load %list*, %list** %x.cdr
  %sum = add i64 %sum.prev, %x.car.value
  %null.int = ptrtoint %list* null to i64
  %x.cdr.value.int = ptrtoint %list* %x.cdr.value to i64
  %cond = icmp eq i64 %x.cdr.value.int, %null.int
  br i1 %cond, label %exit, label %loop

exit:
  ret i64 %sum
}


; COM: check that address spaces in byval types are processed correctly
define void @byval_type([1 x {} addrspace(10)*] addrspace(11)* byval([1 x {} addrspace(10)*]) %0) {
; OPAQUE: define void @byval_type(ptr byval([1 x ptr]) %0)
  ret void
}


define private fastcc void @diffejulia__mapreduce_97() {
L6:
; OPAQUE: store atomic ptr @ejl_enz_runtime_exc, ptr null unordered
  store atomic {} addrspace(10)* @ejl_enz_runtime_exc, {} addrspace(10)* addrspace(10)* null unordered, align 8
  unreachable
}

; COM: check that function attributes are preserved on declarations too
declare void @convergent_function() #0
attributes #0 = { convergent }
; CHECK: attributes #0 = { convergent }

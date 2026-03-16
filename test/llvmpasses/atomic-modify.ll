; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='ExpandAtomicModify' -S %s | FileCheck %s

declare {i8, i8} @julia.atomicmodify.i8(ptr, ptr, i8, i8, ...)
declare {double, double} @julia.atomicmodify.f64(ptr, ptr, i8, i8, ...)
declare double @llvm.maxnum.f64(double %Val0, double %Val1)

define i8 @add.i8(i8 %x, i8 %y) {
    %z = add i8 %x, %y
    ret i8 %z
}

define i8 @sub.i8(i8 %x, i8 %y) {
    %z = sub i8 %x, %y
    ret i8 %z
}

define i8 @subx.i8(i8 %x, i8 %y) {
    %z = sub i8 %y, %x
    ret i8 %z
}

define i8 @add.i8.zext(i8 %x, i1 %y) {
    %y8 = zext i1 %y to i8
    %z = add i8 %x, %y8
    ret i8 %z
}

define i8 @and.i8(i8 %x, i8 %y) {
    %z = and i8 %x, %y
    ret i8 %z
}

define i8 @nand.i8(i8 %x, i8 %y) {
    %z = and i8 %x, %y
    %w = xor i8 %z, -1
    ret i8 %w
}

define i8 @nand.i8.zext(i8 %x, i1 %y) {
    %y8 = zext i1 %y to i8
    %z = and i8 %y8, %x
    %w = xor i8 %z, -1
    ret i8 %w
}

define i8 @xchg.i8(i8 %x, i8 %y) {
    ret i8 %y
}

define double @fadd.f64(double %x, double %y) {
    %z = fadd double %y, %x
    ret double %z
}

define double @fmax.f64(double %x, double %y) {
    %z = call double @llvm.maxnum.f64(double %y, double %x)
    ret double %z
}

define internal i8 @0(i8 %x, i8 %y) unnamed_addr {
    %z = call i8 @add.i8(i8 %x, i8 %y)
    ret i8 %z
}

define internal i8 @1(i8 %x, i8 %y) unnamed_addr {
    %z = call i8 @0(i8 %x, i8 %y)
    ret i8 %z
}

define internal i8 @2(i8 %x, i8 %y, ptr %f) unnamed_addr {
    %z = call i8 %f(i8 %x, i8 %y)
    ret i8 %z
}

define i8 @mod_i8_add(ptr %a, i8 %b) {
; CHECK-LABEL: @mod_i8_add
; CHECK: %0 = atomicrmw add ptr %a, i8 %b release, align 1
; CHECK: ret i8 %0
top:
  %oldnew = call {i8, i8} (ptr, ptr, i8, i8, ...) @julia.atomicmodify.i8(ptr align(1) %a, ptr @add.i8, i8 5, i8 1, i8 %b)
  %oldval = extractvalue {i8, i8} %oldnew, 0
  ret i8 %oldval
}

define i8 @mod_i8_add_new(ptr %a, i8 %b) {
; CHECK-LABEL: @mod_i8_add
; CHECK: %0 = atomicrmw add ptr %a, i8 %b release, align 1
; CHECK-NEXT: [[newval:%.*]] = add i8 %0, %b
; CHECK-NEXT: ret i8 [[newval]]
top:
  %oldnew = call {i8, i8} (ptr, ptr, i8, i8, ...) @julia.atomicmodify.i8(ptr align(1) %a, ptr @add.i8, i8 5, i8 1, i8 %b)
  %newval = extractvalue {i8, i8} %oldnew, 1
  ret i8 %newval
}

define i8 @mod_i8_addfence(ptr %a) {
; CHECK-LABEL: @mod_i8_addfence
; CHECK: %0 = atomicrmw or ptr %a, i8 0 release, align 1
; CHECK-NEXT: ret i8 %0
top:
  %oldnew = call {i8, i8} (ptr, ptr, i8, i8, ...) @julia.atomicmodify.i8(ptr align(1) %a, ptr @add.i8, i8 5, i8 1, i8 0)
  %oldval = extractvalue {i8, i8} %oldnew, 0
  ret i8 %oldval
}

define i8 @mod_i8_add_zext(ptr %a, i1 %b) {
; CHECK-LABEL: @mod_i8_add_zext
; CHECK: [[b8:%.*]] = zext i1 %b to i8
; CHECK: %0 = atomicrmw add ptr %a, i8 [[b8]] release, align 1
; CHECK: ret i8 %0
top:
  %oldnew = call {i8, i8} (ptr, ptr, i8, i8, ...) @julia.atomicmodify.i8(ptr align(1) %a, ptr @add.i8.zext, i8 5, i8 1, i1 %b)
  %oldval = extractvalue {i8, i8} %oldnew, 0
  ret i8 %oldval
}

define i8 @mod_i8_add_zext_new(ptr %a, i1 %b) {
; CHECK-LABEL: @mod_i8_add_zext
; CHECK: [[b8:%.*]] = zext i1 %b to i8
; CHECK-NEXT: %0 = atomicrmw add ptr %a, i8 [[b8]] release, align 1
; CHECK-NEXT: [[newval:%.*]] = add i8 %0, [[b8]]
; CHECK-NEXT: ret i8 [[newval]]
top:
  %oldnew = call {i8, i8} (ptr, ptr, i8, i8, ...) @julia.atomicmodify.i8(ptr align(1) %a, ptr @add.i8.zext, i8 5, i8 1, i1 %b)
  %newval = extractvalue {i8, i8} %oldnew, 1
  ret i8 %newval
}

define i8 @mod_i8_sub(ptr %a, i8 %b) {
; CHECK-LABEL: @mod_i8_sub
; CHECK: %0 = atomicrmw sub ptr %a, i8 %b release, align 1
; CHECK: ret i8 %0
top:
  %oldnew = call {i8, i8} (ptr, ptr, i8, i8, ...) @julia.atomicmodify.i8(ptr align(1) %a, ptr @sub.i8, i8 5, i8 1, i8 %b)
  %oldval = extractvalue {i8, i8} %oldnew, 0
  ret i8 %oldval
}

define i8 @mod_i8_subx(ptr %a, i8 %b) {
; CHECK-LABEL: @mod_i8_subx
; CHECK: [[newval:%.*]] = call i8 @subx.i8(i8 %loaded, i8 %b)
; CHECK: [[success:%.*]] = cmpxchg ptr %a, i8 %loaded, i8 [[newval]]
; CHECK: [[oldval:%.*]] = extractvalue { i8, i1 } [[success:%.*]], 0
; CHECK: ret i8 [[oldval]]
top:
  %oldnew = call {i8, i8} (ptr, ptr, i8, i8, ...) @julia.atomicmodify.i8(ptr align(1) %a, ptr @subx.i8, i8 5, i8 1, i8 %b)
  %oldval = extractvalue {i8, i8} %oldnew, 0
  ret i8 %oldval
}

define i8 @mod_i8_subx_new(ptr %a, i8 %b) {
; CHECK-LABEL: @mod_i8_subx_new
; CHECK: [[newval:%.*]] = call i8 @subx.i8(i8 %loaded, i8 %b)
; CHECK: [[oldval:%.*]] = cmpxchg ptr %a, i8 %loaded, i8 [[newval]]
; CHECK: ret i8 [[newval]]
top:
  %oldnew = call {i8, i8} (ptr, ptr, i8, i8, ...) @julia.atomicmodify.i8(ptr align(1) %a, ptr @subx.i8, i8 5, i8 1, i8 %b)
  %newval = extractvalue {i8, i8} %oldnew, 1
  ret i8 %newval
}

define i8 @mod_i8_nand(ptr %a, i8 %b) {
; CHECK-LABEL: @mod_i8_nand
; CHECK: %0 = atomicrmw nand ptr %a, i8 %b release, align 1
; CHECK: ret i8 %0
top:
  %oldnew = call {i8, i8} (ptr, ptr, i8, i8, ...) @julia.atomicmodify.i8(ptr align(1) %a, ptr @nand.i8, i8 5, i8 1, i8 %b)
  %oldval = extractvalue {i8, i8} %oldnew, 0
  ret i8 %oldval
}

define i8 @mod_i8_nand_new(ptr %a, i1 %b) {
; CHECK-LABEL: @mod_i8_nand_new
; CHECK: [[b8:%.*]] = zext i1 %b to i8
; CHECK: %0 = atomicrmw nand ptr %a, i8 [[b8]] release, align 1
; CHECK: [[newand:%.*]] = and i8 [[b8]], %0
; CHECK: [[newval:%.*]] = xor i8 [[newand:%.*]], -1
; CHECK: ret i8 [[newval]]
top:
  %oldnew = call {i8, i8} (ptr, ptr, i8, i8, ...) @julia.atomicmodify.i8(ptr align(1) %a, ptr @nand.i8.zext, i8 5, i8 1, i1 %b)
  %newval = extractvalue {i8, i8} %oldnew, 1
  ret i8 %newval
}

define i8 @mod_i8_andxchg(ptr %a) {
; CHECK-LABEL: @mod_i8_andxchg
; CHECK: %0 = atomicrmw xchg ptr %a, i8 0 release, align 1
; CHECK-NEXT: ret i8 %0
top:
  %oldnew = call {i8, i8} (ptr, ptr, i8, i8, ...) @julia.atomicmodify.i8(ptr align(1) %a, ptr @and.i8, i8 5, i8 1, i8 0)
  %oldval = extractvalue {i8, i8} %oldnew, 0
  ret i8 %oldval
}

define i8 @mod_i8_xchg(ptr %a, i8 %b) {
; CHECK-LABEL: @mod_i8_xchg
; CHECK: %0 = atomicrmw xchg ptr %a, i8 %b release, align 1
; CHECK-NEXT: ret i8 %0
top:
  %oldnew = call {i8, i8} (ptr, ptr, i8, i8, ...) @julia.atomicmodify.i8(ptr align(1) %a, ptr @xchg.i8, i8 5, i8 1, i8 %b)
  %oldval = extractvalue {i8, i8} %oldnew, 0
  ret i8 %oldval
}

define i8 @mod_i8_xchg_new(ptr %a, i8 %b) {
; CHECK-LABEL: @mod_i8_xchg_new
; CHECK: %0 = atomicrmw xchg ptr %a, i8 %b release, align 1
; CHECK-NEXT: ret i8 %b
top:
  %oldnew = call {i8, i8} (ptr, ptr, i8, i8, ...) @julia.atomicmodify.i8(ptr align(1) %a, ptr @xchg.i8, i8 5, i8 1, i8 %b)
  %newval = extractvalue {i8, i8} %oldnew, 1
  ret i8 %newval
}

define double @mod_i8_fadd(ptr %a, double %b) {
; CHECK-LABEL: @mod_i8_fadd
; CHECK: %0 = atomicrmw fadd ptr %a, double %b release, align 8
; CHECK: ret double %0
top:
  %oldnew = call {double, double} (ptr, ptr, i8, i8, ...) @julia.atomicmodify.f64(ptr align(8) %a, ptr @fadd.f64, i8 5, i8 1, double %b)
  %oldval = extractvalue {double, double} %oldnew, 0
  ret double %oldval
}

define double @mod_i8_fmax(ptr %a, double %b) {
; CHECK-LABEL: @mod_i8_fmax
; CHECK: %0 = atomicrmw fmax ptr %a, double %b release, align 8
; CHECK: ret double %0
top:
  %oldnew = call {double, double} (ptr, ptr, i8, i8, ...) @julia.atomicmodify.f64(ptr align(8) %a, ptr @fmax.f64, i8 5, i8 1, double %b)
  %oldval = extractvalue {double, double} %oldnew, 0
  ret double %oldval
}

define i8 @mod_i8_indirect0(ptr %a, i8 %b) {
; CHECK-LABEL: @mod_i8_indirect0
; CHECK: %0 = atomicrmw add ptr %a, i8 %b release, align 1
; CHECK: ret i8 %0
top:
  %oldnew = call {i8, i8} (ptr, ptr, i8, i8, ...) @julia.atomicmodify.i8(ptr align(1) %a, ptr @0, i8 5, i8 1, i8 %b)
  %oldval = extractvalue {i8, i8} %oldnew, 0
  ret i8 %oldval
}

define i8 @mod_i8_indirect1(ptr %a, i8 %b) {
; CHECK-LABEL: @mod_i8_indirect1
; CHECK: %0 = atomicrmw add ptr %a, i8 %b release, align 1
; CHECK: ret i8 %0
top:
  %oldnew = call {i8, i8} (ptr, ptr, i8, i8, ...) @julia.atomicmodify.i8(ptr align(1) %a, ptr @1, i8 5, i8 1, i8 %b)
  %oldval = extractvalue {i8, i8} %oldnew, 0
  ret i8 %oldval
}

define i8 @mod_i8_indirect2(ptr %a, i8 %b, ptr %f) {
; CHECK-LABEL: @mod_i8_indirect2
; CHECK: [[newval:%.*]] = call i8 %f(i8 %loaded, i8 %b)
; CHECK: [[success:%.*]] = cmpxchg ptr %a, i8 %loaded, i8 [[newval]]
; CHECK: [[oldval:%.*]] = extractvalue { i8, i1 } [[success:%.*]], 0
; CHECK: ret i8 [[oldval]]
top:
  %oldnew = call {i8, i8} (ptr, ptr, i8, i8, ...) @julia.atomicmodify.i8(ptr align(1) %a, ptr @2, i8 5, i8 1, i8 %b, ptr %f)
  %oldval = extractvalue {i8, i8} %oldnew, 0
  ret i8 %oldval
}

define i8 @mod_i8_indirect2_new(ptr %a, i8 %b, ptr %f) {
; CHECK-LABEL: @mod_i8_indirect2_new
; CHECK: [[newval:%.*]] = call i8 %f(i8 %loaded, i8 %b)
; CHECK: [[oldval:%.*]] = cmpxchg ptr %a, i8 %loaded, i8 [[newval]]
; CHECK: ret i8 [[newval]]
top:
  %oldnew = call {i8, i8} (ptr, ptr, i8, i8, ...) @julia.atomicmodify.i8(ptr align(1) %a, ptr @2, i8 5, i8 1, i8 %b, ptr %f)
  %newval = extractvalue {i8, i8} %oldnew, 1
  ret i8 %newval
}

define i8 @mod_i8_indirect3(ptr %a, i8 %b) {
; CHECK-LABEL: @mod_i8_indirect3
; CHECK: %0 = atomicrmw add ptr %a, i8 %b release, align 1
; CHECK: ret i8 %0
top:
  %oldnew = call {i8, i8} (ptr, ptr, i8, i8, ...) @julia.atomicmodify.i8(ptr align(1) %a, ptr @2, i8 5, i8 1, i8 %b, ptr @0)
  %oldval = extractvalue {i8, i8} %oldnew, 0
  ret i8 %oldval
}

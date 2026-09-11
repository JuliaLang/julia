; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='PropagateJuliaAddrspaces,dce' -S %s | FileCheck %s

target triple = "amdgcn-amd-amdhsa"
target datalayout = "e-p:64:64-p1:64:64-p2:32:32-p3:32:32-p4:64:64-p5:32:32-p6:32:32-i64:64-v16:16-v24:32-v32:32-v48:64-v96:128-v192:256-v256:256-v512:512-v1024:1024-v2048:2048-n32:64-S32-A5-G1-ni:7-ni:10:11:12:13"

define i64 @simple() {
; CHECK-LABEL: @simple
; CHECK-NOT: addrspace(11)
    %stack = alloca i64, addrspace(5)
    %casted = addrspacecast i64 addrspace(5)*%stack to i64 addrspace(11)*
    %loaded = load i64, i64 addrspace(11)* %casted
    ret i64 %loaded
}

define i64 @twogeps() {
; CHECK-LABEL: @twogeps
; CHECK-NOT: addrspace(11)
    %stack = alloca i64, addrspace(5)
    %casted = addrspacecast i64 addrspace(5)*%stack to i64 addrspace(11)*
    %gep1 = getelementptr i64, i64 addrspace(11)* %casted, i64 1
    %gep2 = getelementptr i64, i64 addrspace(11)* %gep1, i64 1
    %loaded = load i64, i64 addrspace(11)* %gep2
    ret i64 %loaded
}

define i64 @phi(i1 %cond) {
; CHECK-LABEL: @phi
; CHECK-NOT: addrspace(11)
top:
    %stack1 = alloca i64, addrspace(5)
    %stack2 = alloca i64, addrspace(5)
    %stack1_casted = addrspacecast i64 addrspace(5)*%stack1 to i64 addrspace(11)*
    %stack2_casted = addrspacecast i64 addrspace(5)*%stack2 to i64 addrspace(11)*
    br i1 %cond, label %A, label %B
A:
    br label %B
B:
    %phi = phi i64 addrspace(11)* [ %stack1_casted, %top ], [ %stack2_casted, %A ]
    %load = load i64, i64 addrspace(11)* %phi
    ret i64 %load
}


define i64 @select(i1 %cond) {
; CHECK-LABEL: @select
; CHECK-NOT: addrspace(11)
top:
    %stack1 = alloca i64, addrspace(5)
    %stack2 = alloca i64, addrspace(5)
    %stack1_casted = addrspacecast i64 addrspace(5)*%stack1 to i64 addrspace(11)*
    %stack2_casted = addrspacecast i64 addrspace(5)*%stack2 to i64 addrspace(11)*
    %select = select i1 %cond, i64 addrspace(11)* %stack1_casted, i64 addrspace(11)* %stack2_casted
    %load = load i64, i64 addrspace(11)* %select
    ret i64 %load
}

define i64 @nullptr() {
; CHECK-LABEL: @nullptr
; CHECK-NOT: addrspace(11)
    %casted = addrspacecast i64 addrspace(5)*null to i64 addrspace(11)*
    %load = load i64, i64 addrspace(11)* %casted
    ret i64 %load
}

@gbox = private unnamed_addr constant { i64, i64 } { i64 42, i64 42 }, align 16

; A phi between an alloca (addrspace 5) and a global constant (addrspace 0):
; there is no single address space the phi can be lifted into, so it must be
; left alone (lifting it used to produce a phi with mismatched operand types).
define i64 @phi_mixed_addrspace(i1 %cond) {
; CHECK-LABEL: @phi_mixed_addrspace
; CHECK: phi ptr addrspace(11)
; CHECK: load i64, ptr addrspace(11)
top:
    %stack = alloca i64, addrspace(5)
    %stack_casted = addrspacecast ptr addrspace(5) %stack to ptr addrspace(11)
    br i1 %cond, label %A, label %B
A:
    %global_casted = addrspacecast ptr getelementptr inbounds (i8, ptr @gbox, i64 8) to ptr addrspace(11)
    br label %B
B:
    %phi = phi ptr addrspace(11) [ %stack_casted, %top ], [ %global_casted, %A ]
    %load = load i64, ptr addrspace(11) %phi
    ret i64 %load
}

; A phi between a global constant (addrspace 0) and null: lifting is fine, but
; must lift into the address space of the global, not the alloca address space.
define i64 @phi_global_null(i1 %cond) {
; CHECK-LABEL: @phi_global_null
; CHECK: phi ptr [
; CHECK-NOT: addrspace(11)
top:
    br i1 %cond, label %A, label %B
A:
    %global_casted = addrspacecast ptr getelementptr inbounds (i8, ptr @gbox, i64 8) to ptr addrspace(11)
    br label %B
B:
    %phi = phi ptr addrspace(11) [ null, %top ], [ %global_casted, %A ]
    %load = load i64, ptr addrspace(11) %phi
    ret i64 %load
}

; RUN: opt -enable-new-pm=0 -load libjulia-codegen%shlibext -PropagateJuliaAddrspaces -dce -S %s | FileCheck %s
; RUN: opt -enable-new-pm=1 --load-pass-plugin=libjulia-codegen%shlibext -passes='PropagateJuliaAddrspaces,dce' -S %s | FileCheck %s

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

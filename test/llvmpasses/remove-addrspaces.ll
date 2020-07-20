; RUN: opt -load libjulia%shlibext -RemoveJuliaAddrspaces -S %s | FileCheck %s


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
; CHECK: call {}* inttoptr (i64 139806640486784 to {}* ({}*, i64)*)({}* inttoptr (i64 139806425039920 to {}*), i64 1)
  %1 = call {} addrspace(10)* inttoptr (i64 139806640486784 to {} addrspace(10)* ({} addrspace(10)*, i64)*)({} addrspace(10)* addrspacecast ({}* inttoptr (i64 139806425039920 to {}*) to {} addrspace(10)*), i64 1)
; CHECK-NOT: addrspacecast
; CHECK-NOT: addrspace
  %2 = addrspacecast {} addrspace(10)* %1 to {} addrspace(11)*
  %3 = bitcast {} addrspace(11)* %2 to i64 addrspace(13)* addrspace(11)*
  %4 = load i64 addrspace(13)*, i64 addrspace(13)* addrspace(11)* %3, align 8
  store i64 %0, i64 addrspace(13)* %4, align 8
  ret {} addrspace(10)* %1
}

; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='function(GCInvariantVerifier)' -S %s | FileCheck %s

; CHECK-LABEL: @vectorized_addrspacecast
define ptr addrspace(10) @vectorized_addrspacecast() {
top:
  ret ptr addrspace(10) null

vector.ph:
  %0 = addrspacecast <4 x ptr addrspace(10)> zeroinitializer to <4 x ptr addrspace(11)>
  unreachable
}

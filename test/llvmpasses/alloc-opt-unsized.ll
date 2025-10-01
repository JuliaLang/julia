; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext --passes='function(AllocOpt)' -S %s | FileCheck %s --check-prefixes=OPAQUE

source_filename = "text"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128-ni:10:11:12:13"
target triple = "x86_64-linux-gnu"

declare {}*** @julia.get_pgcstack()

declare {} addrspace(10)* @julia.gc_alloc_obj({}**, i64, {} addrspace(10)*)

declare void @julia.write_barrier({} addrspace(10)*, ...)

define void @diffejulia_objective__1864_inner_1wrap({} addrspace(10)* %arg, i64 %iv.i) {
entry:
  %i5 = call {}*** @julia.get_pgcstack()
  %i13 = bitcast {}*** %i5 to {}**
  %i14 = getelementptr inbounds {}*, {}** %i13, i64 -12
  %i18 = call noalias nonnull dereferenceable(8000) dereferenceable_or_null(8000) {} addrspace(10)* @julia.gc_alloc_obj({}** %i14, i64 8000, {} addrspace(10)* addrspacecast ({}* inttoptr (i64 139756155247504 to {}*) to {} addrspace(10)*))
  %_malloccache.i = bitcast {} addrspace(10)* %i18 to {} addrspace(10)* addrspace(10)*
  %i23 = getelementptr inbounds {} addrspace(10)*, {} addrspace(10)* addrspace(10)* %_malloccache.i, i64 %iv.i
  store {} addrspace(10)* %arg, {} addrspace(10)* addrspace(10)* %i23, align 8
  %i24 = bitcast {} addrspace(10)* addrspace(10)* %_malloccache.i to {} addrspace(10)*
  call void ({} addrspace(10)*, ...) @julia.write_barrier({} addrspace(10)* %i24, {} addrspace(10)* %arg)
  %l = load {} addrspace(10)*, {} addrspace(10)* addrspace(10)* %i23
  ret void
}

; OPAQUE:   %[[i0:.+]] = alloca ptr addrspace(10), i64 1000, align 16
; OPAQUE:   %i23 = getelementptr inbounds ptr addrspace(10), ptr %i18, i64 %iv.i
; OPAQUE:   store ptr addrspace(10) %arg, ptr %i23, align 8
; OPAQUE:   %l = load ptr addrspace(10), ptr %i23, align 8

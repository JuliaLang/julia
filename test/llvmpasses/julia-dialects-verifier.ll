; This file is a part of Julia. License is MIT: https://julialang.org/license

; Checks that the llvm-dialects verifier accepts well-formed uses of the
; Julia dialect ops (as specified in src/JuliaDialect.td).
; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='JuliaDialectsVerifier' -S %s -o /dev/null

declare ptr @julia.get_pgcstack()
declare ptr addrspace(13) @julia.gc_loaded(ptr addrspace(10), ptr)
declare ptr @julia.new_gc_frame(i32)
declare void @julia.push_gc_frame(ptr, i32)
declare ptr @julia.get_gc_frame_slot(ptr, i32)
declare void @julia.pop_gc_frame(ptr)
declare ptr addrspace(10) @julia.gc_alloc_bytes(ptr, i64, i64)
declare void @julia.queue_gc_root(ptr addrspace(10))
declare void @julia.safepoint(ptr)
declare ptr addrspace(10) @julia.typeof(ptr addrspace(10))
declare void @julia.write_barrier(ptr addrspace(10), ...)

define void @gcframe(ptr %ptls, ptr addrspace(10) %obj) {
top:
  %pgcstack = call ptr @julia.get_pgcstack()
  %frame = call ptr @julia.new_gc_frame(i32 2)
  call void @julia.push_gc_frame(ptr %frame, i32 2)
  %slot = call ptr @julia.get_gc_frame_slot(ptr %frame, i32 0)
  %alloc = call ptr addrspace(10) @julia.gc_alloc_bytes(ptr %ptls, i64 8, i64 0)
  store ptr addrspace(10) %alloc, ptr %slot
  %tag = call ptr addrspace(10) @julia.typeof(ptr addrspace(10) %alloc)
  call void (ptr addrspace(10), ...) @julia.write_barrier(ptr addrspace(10) %obj, ptr addrspace(10) %alloc)
  call void @julia.queue_gc_root(ptr addrspace(10) %obj)
  call void @julia.safepoint(ptr %ptls)
  call void @julia.pop_gc_frame(ptr %frame)
  ret void
}

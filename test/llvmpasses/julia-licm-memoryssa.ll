; COM: NewPM-only test, tests that memoryssa is preserved correctly

; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='function(loop-mssa(JuliaLICM),print<memoryssa>)' -S -o /dev/null %s 2>&1 | FileCheck %s --check-prefixes=CHECK,OPAQUE

@tag = external addrspace(10) global {}, align 16

declare void @julia.object_write_barrier({} addrspace(10)*, ...)

declare void @julia.field_write_barrier.p11({} addrspace(10)*, {} addrspace(11)*, {} addrspace(10)*, ...)

declare {}*** @julia.get_pgcstack()

declare token @llvm.julia.gc_preserve_begin(...)

declare void @llvm.julia.gc_preserve_end(token)

declare void @mssa_use({} addrspace(10)*)

declare noalias nonnull {} addrspace(10)* @julia.gc_alloc_obj({}**, i64, {} addrspace(10)*)

; COM: check basic preserve hoist/sink functionality
; CHECK-LABEL: MemorySSA for function: hoist_sink_preserves
; CHECK-LABEL: @hoist_sink_preserves
define void @hoist_sink_preserves({} addrspace(10)* %obj, i1 %ret) {
; CHECK: top:
top:
; CHECK-NEXT: [[PGCSTACK:[0-9]+]] = MemoryDef(liveOnEntry)
  %pgcstack = call {}*** @julia.get_pgcstack()
  %current_task = bitcast {}*** %pgcstack to {}**
; CHECK: br label %preheader
  br label %preheader
; CHECK: preheader:
preheader:
; CHECK-NEXT: [[PRESERVE_TOKEN:[0-9]+]] = MemoryDef([[PGCSTACK]])
; CHECK-NEXT: %preserve_token = call token (...) @llvm.julia.gc_preserve_begin
; CHECK-NEXT: br label %loop
  br label %loop
; CHECK: loop:
loop:
; CHECK-NOT: call token (...) @llvm.julia.gc_preserve_begin
  %preserve_token = call token (...) @llvm.julia.gc_preserve_begin({} addrspace(10)* %obj)
; CHECK-NOT: call void @llvm.julia.gc_preserve_end
  call void @llvm.julia.gc_preserve_end(token %preserve_token)
; CHECK-NEXT: [[MPHI:[0-9]+]] = MemoryPhi({preheader,[[PRESERVE_TOKEN]]},{loop,[[MPHI]]})
; CHECK-NEXT: br i1 %ret
  br i1 %ret, label %return, label %loop
; CHECK: return:
return:
; CHECK-NEXT: [[PRESERVE_END:[0-9]+]] = MemoryDef([[MPHI]])
; CHECK-NEXT: call void @llvm.julia.gc_preserve_end(token %preserve_token)
; CHECK-NEXT: [[MSSA_USE:[0-9]+]] = MemoryDef([[PRESERVE_END]])
; CHECK-NEXT: call void @mssa_use
  call void @mssa_use({} addrspace(10)* %obj)
; CHECK-NEXT: ret void
  ret void
}

; COM: check sink functionality when there are multiple loop exit blocks
; CHECK-LABEL: MemorySSA for function: hoist_multisink_preserves
; CHECK-LABEL: @hoist_multisink_preserves
define void @hoist_multisink_preserves({} addrspace(10)* %obj, i1 %ret) {
; CHECK: top:
top:
; CHECK-NEXT: [[PGCSTACK:[0-9]+]] = MemoryDef(liveOnEntry)
  %pgcstack = call {}*** @julia.get_pgcstack()
  %current_task = bitcast {}*** %pgcstack to {}**
; CHECK: br label %preheader
  br label %preheader
; CHECK: preheader:
preheader:
; CHECK-NEXT: [[PRESERVE_TOKEN:[0-9]+]] = MemoryDef([[PGCSTACK]])
; CHECK-NEXT: %preserve_token = call token (...) @llvm.julia.gc_preserve_begin
; CHECK-NEXT: br label %loop
  br label %loop
; CHECK: loop:
loop:
; CHECK-NOT: call token (...) @llvm.julia.gc_preserve_begin
  %preserve_token = call token (...) @llvm.julia.gc_preserve_begin({} addrspace(10)* %obj)
; CHECK-NOT: call void @llvm.julia.gc_preserve_end
  call void @llvm.julia.gc_preserve_end(token %preserve_token)
; CHECK-NEXT: [[MPHI:[0-9]+]] = MemoryPhi({preheader,[[PRESERVE_TOKEN]]},{loop2,[[MPHI]]})
; CHECK-NEXT: br i1 %ret
  br i1 %ret, label %return, label %loop2
; CHECK: loop2:
loop2:
; CHECK-NEXT: br i1 %ret
  br i1 %ret, label %return2, label %loop
; CHECK: return:
return:
; CHECK-NEXT: [[PRESERVE_END_1:[0-9]+]] = MemoryDef([[MPHI]])
; CHECK-NEXT: call void @llvm.julia.gc_preserve_end(token %preserve_token)
; CHECK-NEXT: [[MSSA_USE:[0-9]+]] = MemoryDef([[PRESERVE_END_1]])
; CHECK-NEXT: call void @mssa_use
  call void @mssa_use({} addrspace(10)* %obj)
; CHECK-NEXT: ret void
  ret void
; CHECK: return2:
return2:
; CHECK-NEXT: [[PRESERVE_END_2:[0-9]+]] = MemoryDef([[MPHI]])
; CHECK-NEXT: call void @llvm.julia.gc_preserve_end(token %preserve_token)
; CHECK-NEXT: [[MSSA_USE:[0-9]+]] = MemoryDef([[PRESERVE_END_2]])
; CHECK-NEXT: call void @mssa_use
  call void @mssa_use({} addrspace(10)* %obj)
; CHECK-NEXT: ret void
  ret void
}

define void @hoist_allocation({} addrspace(10)* %obj, i1 %ret) {
; CHECK: top:
top:
; CHECK-NEXT: [[PGCSTACK:[0-9]+]] = MemoryDef(liveOnEntry)
  %pgcstack = call {}*** @julia.get_pgcstack()
  %current_task = bitcast {}*** %pgcstack to {}**
  br label %preheader
; CHECK: preheader:
preheader:
; CHECK-NEXT: [[ALLOC:[0-9]+]] = MemoryDef([[PGCSTACK]])


; OPAQUE-NEXT: %alloc = call ptr addrspace(10) @julia.gc_alloc_obj(ptr %current_task, i64 0, ptr addrspace(10) @tag)

; CHECK-NEXT: [[MSET:[0-9]+]] = MemoryDef([[ALLOC]])
; CHECK-NEXT: call void @llvm.memset
; CHECK-NEXT: br label %loop
  br label %loop
; CHECK: loop:
loop:
; CHECK-NOT: %alloc
; CHECK-NOT: @julia.gc_alloc_obj
  %alloc = call {} addrspace(10)* @julia.gc_alloc_obj({}** %current_task, i64 0, {} addrspace(10)* @tag)
; CHECK-NEXT: [[MPHI:[0-9]+]] = MemoryPhi({preheader,[[MSET]]},{loop,[[MPHI]]})
  br i1 %ret, label %return, label %loop
; CHECK: return:
return:
; CHECK-NEXT: [[MSSA_USE:[0-9]+]] = MemoryDef([[MPHI]])
; CHECK-NEXT: call void @mssa_use
  call void @mssa_use({} addrspace(10)* %obj)
; CHECK-NEXT: ret void
  ret void
}

define void @hoist_write_barrier({} addrspace(10)* %obj, i1 %ret) {
; CHECK: top:
top:
; CHECK-NEXT: [[PGCSTACK:[0-9]+]] = MemoryDef(liveOnEntry)
  %pgcstack = call {}*** @julia.get_pgcstack()
  %current_task = bitcast {}*** %pgcstack to {}**
  br label %preheader
; CHECK: preheader:
preheader:
; CHECK-NEXT: [[WB:[0-9]+]] = MemoryDef([[PGCSTACK]])
; CHECK-NEXT: call void
; CHECK-SAME: @julia.object_write_barrier
; CHECK-NEXT: br label %loop
  br label %loop
; CHECK: loop:
loop:
; CHECK-NOT: write_barrier
  call void ({} addrspace(10)*, ...) @julia.object_write_barrier({} addrspace(10)* %obj, {} addrspace(10)* %obj)
; CHECK-NEXT: [[MPHI:[0-9]+]] = MemoryPhi({preheader,[[WB]]},{loop,[[MPHI]]})
  br i1 %ret, label %return, label %loop
; CHECK: return:
return:
; CHECK-NEXT: [[MSSA_USE:[0-9]+]] = MemoryDef([[MPHI]])
; CHECK-NEXT: call void @mssa_use
  call void @mssa_use({} addrspace(10)* %obj)
; CHECK-NEXT: ret void
  ret void
}

; COM: a field write barrier whose slot is loop-varying (but whose parent and child are
; COM: loop-invariant) is demoted to an object write barrier built in the preheader. Its
; COM: MemoryDef must be ordered after the preheader's pre-existing access, matching the
; COM: instruction's position -- placing it at the block start inverts the def ordering
; COM: and corrupts MemorySSA, which later loop passes (e.g. loop unswitch) then trip on.
; CHECK-LABEL: MemorySSA for function: demote_write_barrier_varying_slot
; CHECK-LABEL: @demote_write_barrier_varying_slot
define void @demote_write_barrier_varying_slot({} addrspace(10)* %parent, {} addrspace(10)* %child, i64 %n, i1 %c) {
; CHECK: top:
top:
; CHECK-NEXT: [[PGCSTACK:[0-9]+]] = MemoryDef(liveOnEntry)
  %pgcstack = call {}*** @julia.get_pgcstack()
  %current_task = bitcast {}*** %pgcstack to {}**
  br label %preheader
; CHECK: preheader:
preheader:
; COM: the pre-existing preheader access; the demoted barrier must be ordered after it
; CHECK: [[MU:[0-9]+]] = MemoryDef([[PGCSTACK]])
; CHECK-NEXT: call void @mssa_use
  call void @mssa_use({} addrspace(10)* %parent)
; CHECK: [[WB:[0-9]+]] = MemoryDef([[MU]])
; CHECK-NEXT: call void
; CHECK-SAME: @julia.object_write_barrier
; CHECK-NEXT: br label %loop
  br label %loop
loop:
  %iv = phi i64 [ 0, %preheader ], [ %iv.next, %latch ]
  br i1 %c, label %do_wb, label %latch
do_wb:
; CHECK-NOT: @julia.field_write_barrier
  %pd = addrspacecast {} addrspace(10)* %parent to {} addrspace(11)*
  %slot = getelementptr {}, {} addrspace(11)* %pd, i64 %iv
  call void ({} addrspace(10)*, {} addrspace(11)*, {} addrspace(10)*, ...) @julia.field_write_barrier.p11({} addrspace(10)* %parent, {} addrspace(11)* %slot, {} addrspace(10)* %child)
  br label %latch
latch:
  %iv.next = add i64 %iv, 1
  %cond = icmp slt i64 %iv.next, %n
  br i1 %cond, label %loop, label %exit
exit:
  ret void
}

; ModuleID = 'tapir.bc'
source_filename = "tapir"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.__rts_stack_frame = type { {} addrspace(10)*  }

declare void @__rts_enter_frame(%struct.__rts_stack_frame*)

declare void @__rts_spawn(%struct.__rts_stack_frame*, void (i8*)*, i8*, i64, i64)

declare void @__rts_leave_frame(%struct.__rts_stack_frame*)

declare void @__rts_sync(%struct.__rts_stack_frame*)

declare void @__rts_sync_nothrow(%struct.__rts_stack_frame*)

declare i8 @__rts_loop_grainsize_8(i8)

declare i16 @__rts_loop_grainsize_16(i16)

declare i32 @__rts_loop_grainsize_32(i32)

declare i64 @__rts_loop_grainsize_64(i64)

define i32 @__rts_get_num_workers()
{
  ret i32 0;
}

define i32 @__rts_get_worker_id()
{
  ret i32 -1;
}



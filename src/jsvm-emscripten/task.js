mergeInto(LibraryManager.library, {
  jl_set_fiber: function(ctx) {
    set_next_ctx(ctx, false);
    return ctx_switch(0)
  },
  jl_swap_fiber: function(lastt_ctx, ctx) {
    if (Asyncify.state === Asyncify.State.Rewinding) {
      // Stop a resume.
      finish_schedule_task();
      return;
    }
    set_next_ctx(ctx, false);
    return ctx_switch(lastt_ctx)
  },
  jl_start_fiber: function(lastt_ctx, ctx) {
    if (Asyncify.state === Asyncify.State.Rewinding) {
      // Stop a resume.
      finish_schedule_task();
      return;
    }
    set_next_ctx(ctx, true);
    return ctx_switch(lastt_ctx)
  }
});


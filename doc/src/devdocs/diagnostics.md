# Diagnostics used by the package ecosystem

This page documents "hooks" embedded in Julia that are primarily used by
external tools. Many of these tools are designed to perform analyses that are
too complicated to be made part of Julia proper.

## SnoopCompile

SnoopCompile "snoops" on Julia's compiler to extract information for analysis
about invalidations and type-inference. There are a few internals it uses for
different purposes:

- recording invalidations: `Base.StaticData.debug_method_invalidation` and
  `ccall(:jl_debug_method_invalidation, ...)`: these record different modes of
  invalidation. Users of SnoopCompile will transiently turn these on when, e.g.,
  loading packages. Each produces a standard log format; messing with the log
  format might require a complementary pull request to SnoopCompile.
  SnoopCompile will process these logs and generate trees of invalidated
  CodeInstances that are attributable to specific changes in the method tables
  or bindings.
- observing inference: `ccall(:jl_set_newly_inferred, ...)` and
  `ccall(:jl_set_inference_entrance_backtraces, ...)`: these are used to
  understand how inference gets triggered. The main purpose is to allow
  performance diagnostics to understand sources of TTFX. The second of these
  `ccall`s records a backtrace on every entrance to type-inference, so that
  SnoopCompile can determine the caller of a dynamically-dispatched call. This
  is needed to attribute "cause" for new type inference.

  The `jl_set_inference_entrance_backtraces` function accepts an array where
  inference entrance events will be recorded. Each inference event stores two
  consecutive array elements: first the `CodeInstance` object, then the
  backtrace representation. So for N inference events, the array will contain 2N
  elements arranged as: `[ci₁, bt₁, ci₂, bt₂, ..., ciₙ, btₙ]`.

  Note that the backtrace elements `btᵢ` contain raw backtrace data that
  typically needs to be processed using `stacktrace(Base._reformat_bt(btᵢ...))`.
  to convert them into a usable stack trace format for analysis.

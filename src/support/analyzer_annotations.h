// This file is a part of Julia. License is MIT: https://julialang.org/license

// This file defines the annotations Julia uses to drive its static
// safepoint/GC analyses. Each annotation is only meaningful when an analyzer is
// enabled; otherwise it compiles away. Three configurations are selected below:
//
//   __clang_gcanalyzer__  the Clang static analyzer plugin in src/clangsa that
//                         checks GC rooting (see doc/src/devdocs/gc-sa.md). The
//                         annotations expand to `annotate(...)` attributes it
//                         reads.
//   __clang_safetyanalysis__  Clang Thread Safety Analysis (-Wthread-safety
//                         -Wthread-safety-negative), which models safepoints as
//                         two "capability" tokens (see below). Enabled by
//                         compiling with -D__clang_safetyanalysis__.
//   (neither)             a normal compile: every annotation expands to nothing
//                         (prototype annotations) or to a no-op (function-like
//                         annotations), so code builds identically either way.
//
// The Thread Safety Analysis models the current thread's state with two token
// capabilities:
//
//   jl_notsafepoint    a REENTRANT capability, conceptually "held" whenever the
//                      thread is in a no-safepoint region -- between entering one
//                      (e.g. taking a no-gc lock) and leaving it. It is reentrant
//                      so that nested no-safepoint regions (e.g. nested no-gc
//                      locks) may acquire it more than once.
//   jl_gcunsaferegion  a (non-reentrant) capability, held whenever the thread is
//                      in a gc-unsafe region -- the state in which it is allowed
//                      to reach a safepoint.
//
// A safepoint is permissible only when the thread is gc-unsafe AND holds no
// no-gc lock. The tokens are pointers so the negative requirement
// `!jl_notsafepoint` is well-formed in C; jl_gcunsaferegion is not reentrant, so
// re-entering an already gc-unsafe region is a modeling error.
//
// -- Nullability --
//
//   JL_NONNULL  Mark a pointer (argument, field, or return value) that is never
//               NULL. On Clang with the nullability feature it expands to the
//               real `_Nonnull` qualifier (the analyzer then flags passing or
//               storing NULL where a non-null pointer is required); on every
//               other compiler `_Nonnull`, and hence JL_NONNULL, is nothing.
//
// -- Safepoint annotations (on function prototypes) --
//
//   JL_NOTSAFEPOINT   The function never reaches a GC safepoint, so callers may
//                     keep values unrooted across a call to it. Under the
//                     GCChecker this is the opt-in default (it carries no
//                     annotation: a function is assumed no-safepoint unless it
//                     opts in with JL_CANSAFEPOINT or a region transition).
//   JL_CANSAFEPOINT   The function may reach a GC safepoint (may trigger GC), so
//                     callers must keep live values rooted across the call.
//   JL_NOTSAFEPOINT_ENTER   The function enters a no-safepoint region (e.g.
//                     takes a no-gc lock).
//   JL_NOTSAFEPOINT_LEAVE   The function leaves a no-safepoint region (e.g.
//                     releases a no-gc lock).
//   JL_NOTSAFEPOINT_ENTER_CONDITIONAL(success)   A conditional enter (e.g. a
//                     no-gc trylock) that enters the no-safepoint region only
//                     when the function returns `success`.
//   JL_NOTSAFEPOINT_LEAVE_ENTER   The function leaves a no-safepoint region and
//                     then re-enters it (e.g. briefly drops a no-gc lock to
//                     permit a safepoint). This is unsafe, so use sparingly.
//   JL_CANSAFEPOINT_ENTER   The function enters a gc-unsafe region (becomes able
//                     to safepoint).
//   JL_CANSAFEPOINT_LEAVE   The function leaves a gc-unsafe region (becomes
//                     unable to safepoint).
//   JL_CANSAFEPOINT_ENTER_LEAVE   The function both enters and then leaves a
//                     gc-unsafe region within its own body. This permits the function body
//                     to be used as a callback from unknown contexts.
//   JL_NOTSAFEPOINT_LEAVE_WITH_CANSAFEPOINT   The function releases a no-gc lock
//                     and then will need to assert CANSAFEPOINT.
//   JL_CANCALLBACK    The function can call arbitrary user/foreign callback.
//                     This is a close dual of JL_CANSAFEPOINT_ENTER_LEAVE,
//                     when the body doesn't care about the incoming state of
//                     jl_gcunsaferegion.
//   JL_NO_SAFEPOINT_ANALYSIS   Opt this function's body out of the thread-safety analysis
//                     entirely, either because it is buggy or because it implements some of
//                     the primitives.
//
// -- Rooting annotations (on arguments, return values, or globals) --
//
//   JL_PROPAGATES_ROOT   On an accessor's argument: the GC root protecting that
//                     argument also protects the value the function returns
//                     (e.g. a field or element read out of a rooted container).
//   JL_PROPAGATES_ROOT_INDEXED(root, index)   Like JL_PROPAGATES_ROOT, but the
//                     returned value is loaded from a specific indexed child of
//                     argument `root` (with `index` the zero-based argument index
//                     giving the position), letting the analyzer track later
//                     overwrites of that child precisely.
//   JL_ROOTED_BY_ARG(n)   The annotated argument is rooted by argument `n` (the
//                     assignment counterpart of JL_PROPAGATES_ROOT); `n` is a
//                     zero-based argument index.
//   JL_ROOTED_BY_ARG_INDEXED(root, index)   The annotated argument is rooted by
//                     being stored into an indexed child of argument `root`,
//                     where `index` is the zero-based index of the argument
//                     giving the position.
//   JL_OUT_ROOTED_BY_ARG(n)   The value written through this out-argument is
//                     rooted by argument `n` (zero-based).
//   JL_ROOTED_BY_RETURN   The annotated argument is rooted by the function's
//                     return value.
//   JL_ROOTED_VARARGS   On a varargs function: its variadic arguments are rooted
//                     by the return value (individual varargs cannot be
//                     annotated).
//   JL_MAYBE_UNROOTED   On an argument: it may be passed even when unrooted,
//                     waiving the usual ABI rule that callers root arguments.
//   JL_GLOBALLY_ROOTED   The value is always globally rooted. On a global
//                     variable it applies to the variable's value(s); on a
//                     function, to its return value.
//   JL_ALWAYS_LEAFTYPE   Like JL_GLOBALLY_ROOTED, but specifically because the
//                     value is a leaftype (rooted via its TypeName cache). Kept
//                     as a separate name so the reason for the rooting is
//                     explicit and can be refined later.
//   JL_GC_DISABLED   The function is only ever called with the GC
//                     runtime-disabled; checked against the gc enable/disable
//                     calls, so the analyzer knows if you lie.
//   JL_REQUIRE_ROOTED_SLOT   The caller must pass a rooted slot -- values
//                     assigned through the annotated pointer argument are treated
//                     as rooted.
//
// -- Escape hatches (function-like annotations) --
//
//   JL_GC_PROMISE_ROOTED(v)   Treat `v` as rooted for the remainder of the
//                     current function. Use sparingly, in favor of improving the
//                     analyzer itself.
//   jl_may_leak(v)    Mark `v` as intentionally allowed to leak,
//                     suppressing the analyzer's leaked-value diagnostic for it.

#ifndef __has_feature
#define __has_feature(x) 0
#endif
#if !(defined(__clang__) && __has_feature(nullability))
#define _Nonnull
#endif
#define JL_NONNULL _Nonnull

#ifdef __clang_gcanalyzer__

#define JL_PROPAGATES_ROOT __attribute__((annotate("julia_propagates_root")))
#define JL_NOTSAFEPOINT __attribute__((annotate("julia_not_safepoint")))
#define JL_CANSAFEPOINT __attribute__((annotate("julia_can_safepoint")))
#define JL_CANSAFEPOINT_ENTER_LEAVE __attribute__((annotate("julia_notsafepoint_leave"),annotate("julia_notsafepoint_enter")))
#define JL_NOTSAFEPOINT_LEAVE_ENTER JL_NOTSAFEPOINT
#define JL_CANSAFEPOINT_ENTER __attribute__((annotate("julia_notsafepoint_leave")))
#define JL_CANSAFEPOINT_LEAVE __attribute__((annotate("julia_notsafepoint_enter")))
#define JL_NO_SAFEPOINT_ANALYSIS __attribute__((annotate("julia_no_safepoint_analysis")))
#define JL_NOTSAFEPOINT_ENTER __attribute__((annotate("julia_notsafepoint_enter")))
#define JL_NOTSAFEPOINT_ENTER_CONDITIONAL(success) __attribute__((annotate("julia_notsafepoint_enter_conditional:" #success)))
#define JL_NOTSAFEPOINT_LEAVE __attribute__((annotate("julia_notsafepoint_leave")))
#define JL_NOTSAFEPOINT_LEAVE_WITH_CANSAFEPOINT __attribute__((annotate("julia_notsafepoint_leave")))
#define JL_CANCALLBACK __attribute__((annotate("julia_can_safepoint")))
#define JL_MAYBE_UNROOTED __attribute__((annotate("julia_maybe_unrooted")))
#define JL_GLOBALLY_ROOTED __attribute__((annotate("julia_globally_rooted")))
#define JL_ROOTED_VARARGS __attribute__((annotate("julia_rooted_varargs")))
#define JL_ROOTED_BY_ARG(n) __attribute__((annotate("julia_rooted_by_arg:" #n)))
#define JL_ROOTED_BY_ARG_INDEXED(root, index) __attribute__((annotate("julia_rooted_by_arg_indexed:" #root ":" #index)))
#define JL_PROPAGATES_ROOT_INDEXED(root, index) __attribute__((annotate("julia_propagates_root_indexed:" #root ":" #index)))
#define JL_OUT_ROOTED_BY_ARG(n) __attribute__((annotate("julia_out_rooted_by_arg:" #n)))
#define JL_ROOTED_BY_RETURN __attribute__((annotate("julia_rooted_by_return")))
#define JL_GC_DISABLED __attribute__((annotate("julia_gc_disabled")))
#define JL_ALWAYS_LEAFTYPE JL_GLOBALLY_ROOTED
#define JL_REQUIRE_ROOTED_SLOT __attribute__((annotate("julia_require_rooted_slot")))
#ifdef __cplusplus
extern "C" {
#endif
  void JL_GC_PROMISE_ROOTED(const void *v) JL_NOTSAFEPOINT;
  void jl_may_leak(const void *v) JL_NOTSAFEPOINT;
#ifdef __cplusplus
}
#endif

#elif defined(__clang_safetyanalysis__)

#ifndef JL_NOTSAFEPOINT_TOKEN_DEFINED
#define JL_NOTSAFEPOINT_TOKEN_DEFINED
struct __attribute__((capability("notsafepoint"),reentrant_capability)) NOTSAFEPOINT {
  char cpp_compat;
};
struct __attribute__((capability("gcunsaferegion"))) GCUNSAFEREGION {
  char cpp_compat;
};
#ifdef __cplusplus
extern "C" {
#endif
extern struct NOTSAFEPOINT *jl_notsafepoint;
extern struct GCUNSAFEREGION *jl_gcunsaferegion;
#ifdef __cplusplus
}
#endif
#endif

#define JL_PROPAGATES_ROOT
#define JL_NOTSAFEPOINT
#define JL_CANSAFEPOINT __attribute__((requires_capability(jl_gcunsaferegion), requires_capability(!jl_notsafepoint)))
#define JL_CANSAFEPOINT_ENTER __attribute__((requires_capability(!jl_gcunsaferegion), acquire_capability(jl_gcunsaferegion), requires_capability(!jl_notsafepoint)))
#define JL_CANSAFEPOINT_LEAVE __attribute__((release_capability(jl_gcunsaferegion), requires_capability(!jl_notsafepoint)))
#define JL_CANSAFEPOINT_ENTER_LEAVE __attribute__((requires_capability(!jl_gcunsaferegion), requires_capability(!jl_notsafepoint)))
#define JL_NOTSAFEPOINT_LEAVE_ENTER __attribute__((requires_capability(jl_notsafepoint)))
#define JL_NO_SAFEPOINT_ANALYSIS __attribute__((no_thread_safety_analysis))
#define JL_NOTSAFEPOINT_ENTER __attribute__((acquire_capability(jl_notsafepoint)))
#define JL_NOTSAFEPOINT_ENTER_CONDITIONAL(success) __attribute__((try_acquire_capability(success, jl_notsafepoint)))
#define JL_NOTSAFEPOINT_LEAVE __attribute__((release_capability(jl_notsafepoint)))
#define JL_NOTSAFEPOINT_LEAVE_WITH_CANSAFEPOINT __attribute__((requires_capability(jl_gcunsaferegion),release_capability(jl_notsafepoint)))
#define JL_CANCALLBACK __attribute__((requires_capability(!jl_notsafepoint)))

#define JL_MAYBE_UNROOTED
#define JL_GLOBALLY_ROOTED
#define JL_ROOTED_VARARGS
#define JL_ROOTED_BY_ARG(n)
#define JL_ROOTED_BY_ARG_INDEXED(root, index)
#define JL_PROPAGATES_ROOT_INDEXED(root, index)
#define JL_OUT_ROOTED_BY_ARG(n)
#define JL_ROOTED_BY_RETURN
#define JL_GC_DISABLED
#define JL_ALWAYS_LEAFTYPE
#define JL_REQUIRE_ROOTED_SLOT
#define JL_GC_PROMISE_ROOTED(x) (void)(x)
#define jl_may_leak(x) (void)(x)

#else

#define JL_PROPAGATES_ROOT
#define JL_NOTSAFEPOINT
#define JL_CANSAFEPOINT
#define JL_CANSAFEPOINT_ENTER
#define JL_CANSAFEPOINT_LEAVE
#define JL_CANSAFEPOINT_ENTER_LEAVE
#define JL_NOTSAFEPOINT_LEAVE_ENTER
#define JL_NO_SAFEPOINT_ANALYSIS
#define JL_NOTSAFEPOINT_ENTER
#define JL_NOTSAFEPOINT_ENTER_CONDITIONAL(success)
#define JL_NOTSAFEPOINT_LEAVE
#define JL_NOTSAFEPOINT_LEAVE_WITH_CANSAFEPOINT
#define JL_CANCALLBACK
#define JL_MAYBE_UNROOTED
#define JL_GLOBALLY_ROOTED
#define JL_ROOTED_VARARGS
#define JL_ROOTED_BY_ARG(n)
#define JL_ROOTED_BY_ARG_INDEXED(root, index)
#define JL_PROPAGATES_ROOT_INDEXED(root, index)
#define JL_OUT_ROOTED_BY_ARG(n)
#define JL_ROOTED_BY_RETURN
#define JL_GC_DISABLED
#define JL_ALWAYS_LEAFTYPE
#define JL_REQUIRE_ROOTED_SLOT
#define JL_GC_PROMISE_ROOTED(x) (void)(x)
#define jl_may_leak(x) (void)(x)

#endif

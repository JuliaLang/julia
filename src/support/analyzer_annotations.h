// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef __has_feature
#define __has_feature(x) 0
#endif
#if !(defined(__clang__) && __has_feature(nullability))
#define _Nonnull
#endif
#define JL_NONNULL _Nonnull

#ifdef __clang_analyzer__

#define JL_PROPAGATES_ROOT __attribute__((annotate("julia_propagates_root")))
#define JL_NOTSAFEPOINT __attribute__((annotate("julia_not_safepoint")))
#define JL_MAYBE_UNROOTED __attribute__((annotate("julia_maybe_unrooted")))
#define JL_GLOBALLY_ROOTED __attribute__((annotate("julia_globally_rooted")))
#define JL_ROOTING_ARGUMENT __attribute__((annotate("julia_rooting_argument")))
#define JL_ROOTED_ARGUMENT __attribute__((annotate("julia_rooted_argument")))
#define JL_GC_DISABLED __attribute__((annotate("julia_gc_disabled")))
#define JL_ALWAYS_LEAFTYPE JL_GLOBALLY_ROOTED
#define JL_ROOTS_TEMPORARILY __attribute__((annotate("julia_temporarily_roots")))
#define JL_REQUIRE_ROOTED_SLOT __attribute__((annotate("julia_require_rooted_slot")))
#define JL_ROOTED_VALUE_COLLECTION __attribute__((annotate("julia_rooted_value_collection")))
#ifdef __cplusplus
extern "C" {
#endif
  void JL_GC_PROMISE_ROOTED(void *v) JL_NOTSAFEPOINT;
  void jl_may_leak(uintptr_t) JL_NOTSAFEPOINT;
#ifdef __cplusplus
}
#endif

#else

#define JL_PROPAGATES_ROOT
#define JL_NOTSAFEPOINT
#define JL_MAYBE_UNROOTED
#define JL_GLOBALLY_ROOTED
#define JL_ROOTING_ARGUMENT
#define JL_ROOTED_ARGUMENT
#define JL_GC_DISABLED
#define JL_ALWAYS_LEAFTYPE
#define JL_ROOTS_TEMPORARILY
#define JL_REQUIRE_ROOTED_SLOT
#define JL_ROOTED_VALUE_COLLECTION
#define JL_GC_PROMISE_ROOTED(x) (void)(x)
#define jl_may_leak(x) (void)(x)

#endif

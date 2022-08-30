"""
    effects::Effects

Represents computational effects of a method call.

The effects are a composition of different effect bits that represent some program property
of the method being analyzed. They are represented as `Bool` or `UInt8` bits with the
following meanings:
- `effects.consistent::UInt8`:
  * `ALWAYS_TRUE`: this method is guaranteed to return or terminate consistently.
  * `ALWAYS_FALSE`: this method may be not return or terminate consistently, and there is
    no need for further analysis with respect to this effect property as this conclusion
    will not be refined anyway.
  * `CONSISTENT_IF_NOTRETURNED`: the `:consistent`-cy of this method can later be refined to
    `ALWAYS_TRUE` in a case when the return value of this method never involves newly
    allocated mutable objects.
  * `CONSISTENT_IF_INACCESSIBLEMEMONLY`: the `:consistent`-cy of this method can later be
    refined to `ALWAYS_TRUE` in a case when `:inaccessiblememonly` is proven.
- `effect_free::UInt8`:
  * `ALWAYS_TRUE`: this method is free from externally semantically visible side effects.
  * `ALWAYS_FALSE`: this method may not be free from externally semantically visible side effects, and there is
    no need for further analysis with respect to this effect property as this conclusion
    will not be refined anyway.
  * `EFFECT_FREE_IF_INACCESSIBLEMEMONLY`: the `:effect-free`-ness of this method can later be
    refined to `ALWAYS_TRUE` in a case when `:inaccessiblememonly` is proven.
- `nothrow::Bool`: this method is guaranteed to not throw an exception.
- `terminates::Bool`: this method is guaranteed to terminate.
- `notaskstate::Bool`: this method does not access any state bound to the current
  task and may thus be moved to a different task without changing observable
  behavior. Note that this currently implies that `noyield` as well, since
  yielding modifies the state of the current task, though this may be split
  in the future.
- `inaccessiblememonly::UInt8`:
  * `ALWAYS_TRUE`: this method does not access or modify externally accessible mutable memory.
    This state corresponds to LLVM's `inaccessiblememonly` function attribute.
  * `ALWAYS_FALSE`: this method may access or modify externally accessible mutable memory.
  * `INACCESSIBLEMEM_OR_ARGMEMONLY`: this method does not access or modify externally accessible mutable memory,
    except that it may access or modify mutable memory pointed to by its call arguments.
    This may later be refined to `ALWAYS_TRUE` in a case when call arguments are known to be immutable.
    This state corresponds to LLVM's `inaccessiblemem_or_argmemonly` function attribute.
- `nonoverlayed::Bool`: indicates that any methods that may be called within this method
  are not defined in an [overlayed method table](@ref OverlayMethodTable).
- `noinbounds::Bool`: indicates this method can't be `:consistent` because of bounds checking.
  This effect is currently only set on `InferenceState` construction and used to taint
  `:consistent`-cy before caching. We may want to track it with more accuracy in the future.

Note that the representations above are just internal implementation details and thus likely
to change in the future. See [`Base.@assume_effects`](@ref) for more detailed explanation
on the definitions of these properties.

Along the abstract interpretation, `Effects` at each statement are analyzed locally and they
are merged into the single global `Effects` that represents the entire effects of the
analyzed method (see the implementation of `merge_effects!`). Each effect property is
initialized with `ALWAYS_TRUE`/`true` and then transitioned towards `ALWAYS_FALSE`/`false`.
Note that within the current flow-insensitive analysis design, effects detected by local
analysis on each statement usually taint the global conclusion conservatively.
"""
struct Effects
    consistent::UInt8
    effect_free::UInt8
    nothrow::Bool
    terminates::Bool
    notaskstate::Bool
    inaccessiblememonly::UInt8
    nonoverlayed::Bool
    noinbounds::Bool
    function Effects(
        consistent::UInt8,
        effect_free::UInt8,
        nothrow::Bool,
        terminates::Bool,
        notaskstate::Bool,
        inaccessiblememonly::UInt8,
        nonoverlayed::Bool,
        noinbounds::Bool = true)
        return new(
            consistent,
            effect_free,
            nothrow,
            terminates,
            notaskstate,
            inaccessiblememonly,
            nonoverlayed,
            noinbounds)
    end
end

const ALWAYS_TRUE  = 0x00
const ALWAYS_FALSE = 0x01

# :consistent-cy bits
const CONSISTENT_IF_NOTRETURNED         = 0x01 << 1
const CONSISTENT_IF_INACCESSIBLEMEMONLY = 0x01 << 2

# :effect_free-ness bits
const EFFECT_FREE_IF_INACCESSIBLEMEMONLY = 0x01 << 1

# :inaccessiblememonly bits
const INACCESSIBLEMEM_OR_ARGMEMONLY = 0x01 << 1

const EFFECTS_TOTAL    = Effects(ALWAYS_TRUE,  ALWAYS_TRUE,  true,  true,  true,  ALWAYS_TRUE,  true)
const EFFECTS_THROWS   = Effects(ALWAYS_TRUE,  ALWAYS_TRUE,  false, true,  true,  ALWAYS_TRUE,  true)
const EFFECTS_UNKNOWN  = Effects(ALWAYS_FALSE, ALWAYS_FALSE, false, false, false, ALWAYS_FALSE, true)  # unknown mostly, but it's not overlayed at least (e.g. it's not a call)
const EFFECTS_UNKNOWN′ = Effects(ALWAYS_FALSE, ALWAYS_FALSE, false, false, false, ALWAYS_FALSE, false) # unknown really

function Effects(e::Effects = EFFECTS_UNKNOWN′;
    consistent::UInt8 = e.consistent,
    effect_free::UInt8 = e.effect_free,
    nothrow::Bool = e.nothrow,
    terminates::Bool = e.terminates,
    notaskstate::Bool = e.notaskstate,
    inaccessiblememonly::UInt8 = e.inaccessiblememonly,
    nonoverlayed::Bool = e.nonoverlayed,
    noinbounds::Bool = e.noinbounds)
    return Effects(
        consistent,
        effect_free,
        nothrow,
        terminates,
        notaskstate,
        inaccessiblememonly,
        nonoverlayed,
        noinbounds)
end

function merge_effects(old::Effects, new::Effects)
    return Effects(
        merge_effectbits(old.consistent, new.consistent),
        merge_effectbits(old.effect_free, new.effect_free),
        merge_effectbits(old.nothrow, new.nothrow),
        merge_effectbits(old.terminates, new.terminates),
        merge_effectbits(old.notaskstate, new.notaskstate),
        merge_effectbits(old.inaccessiblememonly, new.inaccessiblememonly),
        merge_effectbits(old.nonoverlayed, new.nonoverlayed),
        merge_effectbits(old.noinbounds, new.noinbounds))
end

function merge_effectbits(old::UInt8, new::UInt8)
    if old === ALWAYS_FALSE || new === ALWAYS_FALSE
        return ALWAYS_FALSE
    end
    return old | new
end
merge_effectbits(old::Bool, new::Bool) = old & new

is_consistent(effects::Effects)          = effects.consistent === ALWAYS_TRUE
is_effect_free(effects::Effects)         = effects.effect_free === ALWAYS_TRUE
is_nothrow(effects::Effects)             = effects.nothrow
is_terminates(effects::Effects)          = effects.terminates
is_notaskstate(effects::Effects)         = effects.notaskstate
is_inaccessiblememonly(effects::Effects) = effects.inaccessiblememonly === ALWAYS_TRUE
is_nonoverlayed(effects::Effects)        = effects.nonoverlayed

# implies `is_notaskstate` & `is_inaccessiblememonly`, but not explicitly checked here
is_foldable(effects::Effects) =
    is_consistent(effects) &&
    is_effect_free(effects) &&
    is_terminates(effects)

is_total(effects::Effects) =
    is_foldable(effects) &&
    is_nothrow(effects)

is_removable_if_unused(effects::Effects) =
    is_effect_free(effects) &&
    is_terminates(effects) &&
    is_nothrow(effects)

is_consistent_if_notreturned(effects::Effects)         = !iszero(effects.consistent & CONSISTENT_IF_NOTRETURNED)
is_consistent_if_inaccessiblememonly(effects::Effects) = !iszero(effects.consistent & CONSISTENT_IF_INACCESSIBLEMEMONLY)

is_effect_free_if_inaccessiblememonly(effects::Effects) = !iszero(effects.effect_free & EFFECT_FREE_IF_INACCESSIBLEMEMONLY)

is_inaccessiblemem_or_argmemonly(effects::Effects) = effects.inaccessiblememonly === INACCESSIBLEMEM_OR_ARGMEMONLY

function encode_effects(e::Effects)
    return ((e.consistent          % UInt32) << 0) |
           ((e.effect_free         % UInt32) << 3) |
           ((e.nothrow             % UInt32) << 5) |
           ((e.terminates          % UInt32) << 6) |
           ((e.notaskstate         % UInt32) << 7) |
           ((e.inaccessiblememonly % UInt32) << 8) |
           ((e.nonoverlayed        % UInt32) << 10)
end

function decode_effects(e::UInt32)
    return Effects(
        UInt8((e >> 0) & 0x07),
        UInt8((e >> 3) & 0x03),
        _Bool((e >> 5) & 0x01),
        _Bool((e >> 6) & 0x01),
        _Bool((e >> 7) & 0x01),
        UInt8((e >> 8) & 0x03),
        _Bool((e >> 10) & 0x01))
end

struct EffectsOverride
    consistent::Bool
    effect_free::Bool
    nothrow::Bool
    terminates_globally::Bool
    terminates_locally::Bool
    notaskstate::Bool
    inaccessiblememonly::Bool
end

function encode_effects_override(eo::EffectsOverride)
    e = 0x00
    eo.consistent          && (e |= (0x01 << 0))
    eo.effect_free         && (e |= (0x01 << 1))
    eo.nothrow             && (e |= (0x01 << 2))
    eo.terminates_globally && (e |= (0x01 << 3))
    eo.terminates_locally  && (e |= (0x01 << 4))
    eo.notaskstate         && (e |= (0x01 << 5))
    eo.inaccessiblememonly && (e |= (0x01 << 6))
    return e
end

function decode_effects_override(e::UInt8)
    return EffectsOverride(
        (e & (0x01 << 0)) != 0x00,
        (e & (0x01 << 1)) != 0x00,
        (e & (0x01 << 2)) != 0x00,
        (e & (0x01 << 3)) != 0x00,
        (e & (0x01 << 4)) != 0x00,
        (e & (0x01 << 5)) != 0x00,
        (e & (0x01 << 6)) != 0x00)
end

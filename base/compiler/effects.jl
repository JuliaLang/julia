"""
    effects::Effects

Represents computational effects of a method call.

The effects are a composition of different effect bits that represent some program property
of the method being analyzed. They are represented as `Bool` or `UInt8` bits with the
following meanings:
- `consistent::UInt8`:
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
  If the execution of this method may raise `MethodError`s and similar exceptions, then
  the method is not considered as `:nothrow`.
  However, note that environment-dependent errors like `StackOverflowError` or `InterruptException`
  are not modeled by this effect and thus a method that may result in `StackOverflowError`
  does not necessarily need to taint `:nothrow` (although it should usually taint `:terminates` too).
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
- `noub::UInt8`: indicates that the method will not execute any undefined behavior (for any input).
  Note that undefined behavior may technically cause the method to violate any other effect
  assertions (such as `:consistent` or `:effect_free`) as well, but we do not model this,
  and they assume the absence of undefined behavior.
  * `ALWAYS_TRUE`: this method is guaranteed to not execute any undefined behavior.
  * `ALWAYS_FALSE`: this method may execute undefined behavior.
  * `NOUB_IF_NOINBOUNDS`: this method is guaranteed to not execute any undefined behavior
    if the caller does not set nor propagate the `@inbounds` context.
- `nonoverlayed::Bool`: indicates that any methods that may be called within this method
  are not defined in an [overlayed method table](@ref OverlayMethodTable).

Note that the representations above are just internal implementation details and thus likely
to change in the future. See [`Base.@assume_effects`](@ref) for more detailed explanation
on the definitions of these properties.

Along the abstract interpretation, `Effects` at each statement are analyzed locally and they
are merged into the single global `Effects` that represents the entire effects of the
analyzed method (see the implementation of `merge_effects!`). Each effect property is
initialized with `ALWAYS_TRUE`/`true` and then transitioned towards `ALWAYS_FALSE`/`false`.
Note that within the current flow-insensitive analysis design, effects detected by local
analysis on each statement usually taint the global conclusion conservatively.

## Key for `show` output of Effects:

The output represents the state of different effect properties in the following order:

1. `consistent` (`c`):
    - `+c` (green): `ALWAYS_TRUE`
    - `-c` (red): `ALWAYS_FALSE`
    - `?c` (yellow): `CONSISTENT_IF_NOTRETURNED` and/or `CONSISTENT_IF_INACCESSIBLEMEMONLY`
2. `effect_free` (`e`):
    - `+e` (green): `ALWAYS_TRUE`
    - `-e` (red): `ALWAYS_FALSE`
    - `?e` (yellow): `EFFECT_FREE_IF_INACCESSIBLEMEMONLY`
3. `nothrow` (`n`):
    - `+n` (green): `true`
    - `-n` (red): `false`
4. `terminates` (`t`):
    - `+t` (green): `true`
    - `-t` (red): `false`
5. `notaskstate` (`s`):
    - `+s` (green): `true`
    - `-s` (red): `false`
6. `inaccessiblememonly` (`m`):
    - `+m` (green): `ALWAYS_TRUE`
    - `-m` (red): `ALWAYS_FALSE`
    - `?m` (yellow): `INACCESSIBLEMEM_OR_ARGMEMONLY`
7. `noub` (`u`):
    - `+u` (green): `true`
    - `-u` (red): `false`
    - `?u` (yellow): `NOUB_IF_NOINBOUNDS`

Additionally, if the `nonoverlayed` property is false, a red prime symbol (′) is displayed after the tuple.
"""
struct Effects
    consistent::UInt8
    effect_free::UInt8
    nothrow::Bool
    terminates::Bool
    notaskstate::Bool
    inaccessiblememonly::UInt8
    noub::UInt8
    nonoverlayed::Bool
    function Effects(
        consistent::UInt8,
        effect_free::UInt8,
        nothrow::Bool,
        terminates::Bool,
        notaskstate::Bool,
        inaccessiblememonly::UInt8,
        noub::UInt8,
        nonoverlayed::Bool)
        return new(
            consistent,
            effect_free,
            nothrow,
            terminates,
            notaskstate,
            inaccessiblememonly,
            noub,
            nonoverlayed)
    end
end

const ALWAYS_TRUE  = 0x00
const ALWAYS_FALSE = 0x01

# :consistent-cy bits
const CONSISTENT_IF_NOTRETURNED         = 0x01 << 1
const CONSISTENT_IF_INACCESSIBLEMEMONLY = 0x01 << 2

# :effect_free-ness bits
const EFFECT_FREE_IF_INACCESSIBLEMEMONLY = 0x02

"""
`EFFECT_FREE_GLOBALLY` means that the statement is `:effect-free` and does not have a
caller-visible effect, but may not be removed from the function itself. This may e.g.
be used for effects that last only for the scope of the current function.
"""
const EFFECT_FREE_GLOBALLY = 0x03

# :inaccessiblememonly bits
const INACCESSIBLEMEM_OR_ARGMEMONLY = 0x01 << 1

# :noub bits
const NOUB_IF_NOINBOUNDS = 0x01 << 1

const EFFECTS_TOTAL    = Effects(ALWAYS_TRUE,  ALWAYS_TRUE,  true,  true,  true,  ALWAYS_TRUE,  ALWAYS_TRUE,  true)
const EFFECTS_THROWS   = Effects(ALWAYS_TRUE,  ALWAYS_TRUE,  false, true,  true,  ALWAYS_TRUE,  ALWAYS_TRUE,  true)
const EFFECTS_UNKNOWN  = Effects(ALWAYS_FALSE, ALWAYS_FALSE, false, false, false, ALWAYS_FALSE, ALWAYS_FALSE, true) # unknown mostly, but it's not overlayed at least (e.g. it's not a call)
const _EFFECTS_UNKNOWN = Effects(ALWAYS_FALSE, ALWAYS_FALSE, false, false, false, ALWAYS_FALSE, ALWAYS_FALSE, false) # unknown really

function Effects(effects::Effects = _EFFECTS_UNKNOWN;
    consistent::UInt8 = effects.consistent,
    effect_free::UInt8 = effects.effect_free,
    nothrow::Bool = effects.nothrow,
    terminates::Bool = effects.terminates,
    notaskstate::Bool = effects.notaskstate,
    inaccessiblememonly::UInt8 = effects.inaccessiblememonly,
    noub::UInt8 = effects.noub,
    nonoverlayed::Bool = effects.nonoverlayed)
    return Effects(
        consistent,
        effect_free,
        nothrow,
        terminates,
        notaskstate,
        inaccessiblememonly,
        noub,
        nonoverlayed)
end

function is_better_effects(new::Effects, old::Effects)
    any_improved = false
    if new.consistent == ALWAYS_TRUE
        any_improved |= old.consistent != ALWAYS_TRUE
    else
        if !iszero(new.consistent & CONSISTENT_IF_NOTRETURNED)
            old.consistent == ALWAYS_TRUE && return false
            any_improved |= iszero(old.consistent & CONSISTENT_IF_NOTRETURNED)
        elseif !iszero(new.consistent & CONSISTENT_IF_INACCESSIBLEMEMONLY)
            old.consistent == ALWAYS_TRUE && return false
            any_improved |= iszero(old.consistent & CONSISTENT_IF_INACCESSIBLEMEMONLY)
        else
            return false
        end
    end
    if new.effect_free == ALWAYS_TRUE
        any_improved |= old.consistent != ALWAYS_TRUE
    elseif new.effect_free == EFFECT_FREE_IF_INACCESSIBLEMEMONLY
        old.effect_free == ALWAYS_TRUE && return false
        any_improved |= old.effect_free != EFFECT_FREE_IF_INACCESSIBLEMEMONLY
    elseif new.effect_free != old.effect_free
        return false
    end
    if new.nothrow
        any_improved |= !old.nothrow
    elseif new.nothrow != old.nothrow
        return false
    end
    if new.terminates
        any_improved |= !old.terminates
    elseif new.terminates != old.terminates
        return false
    end
    if new.notaskstate
        any_improved |= !old.notaskstate
    elseif new.notaskstate != old.notaskstate
        return false
    end
    if new.inaccessiblememonly == ALWAYS_TRUE
        any_improved |= old.inaccessiblememonly != ALWAYS_TRUE
    elseif new.inaccessiblememonly == INACCESSIBLEMEM_OR_ARGMEMONLY
        old.inaccessiblememonly == ALWAYS_TRUE && return false
        any_improved |= old.inaccessiblememonly != INACCESSIBLEMEM_OR_ARGMEMONLY
    elseif new.inaccessiblememonly != old.inaccessiblememonly
        return false
    end
    if new.noub == ALWAYS_TRUE
        any_improved |= old.noub != ALWAYS_TRUE
    elseif new.noub == NOUB_IF_NOINBOUNDS
        old.noub == ALWAYS_TRUE && return false
        any_improved |= old.noub != NOUB_IF_NOINBOUNDS
    elseif new.noub != old.noub
        return false
    end
    if new.nonoverlayed
        any_improved |= !old.nonoverlayed
    elseif new.nonoverlayed != old.nonoverlayed
        return false
    end
    return any_improved
end

function merge_effects(old::Effects, new::Effects)
    return Effects(
        merge_effectbits(old.consistent, new.consistent),
        merge_effectbits(old.effect_free, new.effect_free),
        merge_effectbits(old.nothrow, new.nothrow),
        merge_effectbits(old.terminates, new.terminates),
        merge_effectbits(old.notaskstate, new.notaskstate),
        merge_effectbits(old.inaccessiblememonly, new.inaccessiblememonly),
        merge_effectbits(old.noub, new.noub),
        merge_effectbits(old.nonoverlayed, new.nonoverlayed))
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
is_noub(effects::Effects)                = effects.noub === ALWAYS_TRUE
is_noub_if_noinbounds(effects::Effects)  = effects.noub === NOUB_IF_NOINBOUNDS
is_nonoverlayed(effects::Effects)        = effects.nonoverlayed

# implies `is_notaskstate` & `is_inaccessiblememonly`, but not explicitly checked here
is_foldable(effects::Effects) =
    is_consistent(effects) &&
    (is_noub(effects) || is_noub_if_noinbounds(effects)) &&
    is_effect_free(effects) &&
    is_terminates(effects)

is_foldable_nothrow(effects::Effects) =
    is_foldable(effects) &&
    is_nothrow(effects)

# TODO add `is_noub` here?
is_removable_if_unused(effects::Effects) =
    is_effect_free(effects) &&
    is_terminates(effects) &&
    is_nothrow(effects)

is_finalizer_inlineable(effects::Effects) =
    is_nothrow(effects) &&
    is_notaskstate(effects)

is_consistent_if_notreturned(effects::Effects)         = !iszero(effects.consistent & CONSISTENT_IF_NOTRETURNED)
is_consistent_if_inaccessiblememonly(effects::Effects) = !iszero(effects.consistent & CONSISTENT_IF_INACCESSIBLEMEMONLY)

is_effect_free_if_inaccessiblememonly(effects::Effects) = !iszero(effects.effect_free & EFFECT_FREE_IF_INACCESSIBLEMEMONLY)

is_inaccessiblemem_or_argmemonly(effects::Effects) = effects.inaccessiblememonly === INACCESSIBLEMEM_OR_ARGMEMONLY

function encode_effects(e::Effects)
    return ((e.consistent          % UInt32) << 0)  |
           ((e.effect_free         % UInt32) << 3)  |
           ((e.nothrow             % UInt32) << 5)  |
           ((e.terminates          % UInt32) << 6)  |
           ((e.notaskstate         % UInt32) << 7)  |
           ((e.inaccessiblememonly % UInt32) << 8)  |
           ((e.noub                % UInt32) << 10) |
           ((e.nonoverlayed        % UInt32) << 12)
end

function decode_effects(e::UInt32)
    return Effects(
        UInt8((e >> 0) & 0x07),
        UInt8((e >> 3) & 0x03),
        _Bool((e >> 5) & 0x01),
        _Bool((e >> 6) & 0x01),
        _Bool((e >> 7) & 0x01),
        UInt8((e >> 8) & 0x03),
        UInt8((e >> 10) & 0x03),
        _Bool((e >> 12) & 0x01))
end

function encode_effects_override(eo::EffectsOverride)
    e = 0x0000
    eo.consistent          && (e |= (0x0001 << 0))
    eo.effect_free         && (e |= (0x0001 << 1))
    eo.nothrow             && (e |= (0x0001 << 2))
    eo.terminates_globally && (e |= (0x0001 << 3))
    eo.terminates_locally  && (e |= (0x0001 << 4))
    eo.notaskstate         && (e |= (0x0001 << 5))
    eo.inaccessiblememonly && (e |= (0x0001 << 6))
    eo.noub                && (e |= (0x0001 << 7))
    eo.noub_if_noinbounds  && (e |= (0x0001 << 8))
    eo.nonoverlayed        && (e |= (0x0001 << 9))
    return e
end

function decode_effects_override(e::UInt16)
    return EffectsOverride(
        !iszero(e & (0x0001 << 0)),
        !iszero(e & (0x0001 << 1)),
        !iszero(e & (0x0001 << 2)),
        !iszero(e & (0x0001 << 3)),
        !iszero(e & (0x0001 << 4)),
        !iszero(e & (0x0001 << 5)),
        !iszero(e & (0x0001 << 6)),
        !iszero(e & (0x0001 << 7)),
        !iszero(e & (0x0001 << 8)),
        !iszero(e & (0x0001 << 9)))
end

decode_statement_effects_override(ssaflag::UInt32) =
    decode_effects_override(UInt16((ssaflag >> NUM_IR_FLAGS) & (1 << NUM_EFFECTS_OVERRIDES - 1)))

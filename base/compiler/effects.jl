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
  * `CONSISTENT_IF_INACCESSIBLE_MEM_ONLY`: the `:consistent`-cy of this method can later be
    refined to `ALWAYS_TRUE` in a case when `:inaccessible_mem_only` is proven.
- `effect_free::UInt8`:
  * `ALWAYS_TRUE`: this method is free from externally semantically visible side effects.
  * `ALWAYS_FALSE`: this method may not be free from externally semantically visible side effects, and there is
    no need for further analysis with respect to this effect property as this conclusion
    will not be refined anyway.
  * `EFFECT_FREE_IF_INACCESSIBLE_MEM_ONLY`: the `:effect-free`-ness of this method can later be
    refined to `ALWAYS_TRUE` in a case when `:inaccessible_mem_only` is proven.
- `no_throw::Bool`: this method is guaranteed to not throw an exception.
  If the execution of this method may raise `MethodError`s and similar exceptions, then
  the method is not considered as `:no_throw`.
  However, note that environment-dependent errors like `StackOverflowError` or `InterruptException`
  are not modeled by this effect and thus a method that may result in `StackOverflowError`
  does not necessarily need to taint `:no_throw` (although it should usually taint `:terminates` too).
- `terminates::Bool`: this method is guaranteed to terminate.
- `no_task_state::Bool`: this method does not access any state bound to the current
  task and may thus be moved to a different task without changing observable
  behavior. Note that this currently implies that `noyield` as well, since
  yielding modifies the state of the current task, though this may be split
  in the future.
- `inaccessible_mem_only::UInt8`:
  * `ALWAYS_TRUE`: this method does not access or modify externally accessible mutable memory.
    This state corresponds to LLVM's `inaccessible_mem_only` function attribute.
  * `ALWAYS_FALSE`: this method may access or modify externally accessible mutable memory.
  * `INACCESSIBLEMEM_OR_ARGMEMONLY`: this method does not access or modify externally accessible mutable memory,
    except that it may access or modify mutable memory pointed to by its call arguments.
    This may later be refined to `ALWAYS_TRUE` in a case when call arguments are known to be immutable.
    This state corresponds to LLVM's `inaccessiblemem_or_argmemonly` function attribute.
- `no_ub::UInt8`: indicates that the method will not execute any undefined behavior (for any input).
  Note that undefined behavior may technically cause the method to violate any other effect
  assertions (such as `:consistent` or `:effect_free`) as well, but we do not model this,
  and they assume the absence of undefined behavior.
  * `ALWAYS_TRUE`: this method is guaranteed to not execute any undefined behavior.
  * `ALWAYS_FALSE`: this method may execute undefined behavior.
  * `NOUB_IF_NOINBOUNDS`: this method is guaranteed to not execute any undefined behavior
    if the caller does not set nor propagate the `@inbounds` context.
- `non_overlayed::Bool`: indicates that any methods that may be called within this method
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
    - `?c` (yellow): `CONSISTENT_IF_NOTRETURNED` and/or `CONSISTENT_IF_INACCESSIBLE_MEM_ONLY`
2. `effect_free` (`e`):
    - `+e` (green): `ALWAYS_TRUE`
    - `-e` (red): `ALWAYS_FALSE`
    - `?e` (yellow): `EFFECT_FREE_IF_INACCESSIBLE_MEM_ONLY`
3. `no_throw` (`n`):
    - `+n` (green): `true`
    - `-n` (red): `false`
4. `terminates` (`t`):
    - `+t` (green): `true`
    - `-t` (red): `false`
5. `no_task_state` (`s`):
    - `+s` (green): `true`
    - `-s` (red): `false`
6. `inaccessible_mem_only` (`m`):
    - `+m` (green): `ALWAYS_TRUE`
    - `-m` (red): `ALWAYS_FALSE`
    - `?m` (yellow): `INACCESSIBLEMEM_OR_ARGMEMONLY`
7. `no_ub` (`u`):
    - `+u` (green): `true`
    - `-u` (red): `false`
    - `?u` (yellow): `NOUB_IF_NOINBOUNDS`

Additionally, if the `non_overlayed` property is false, a red prime symbol (′) is displayed after the tuple.
"""
struct Effects
    consistent::UInt8
    effect_free::UInt8
    no_throw::Bool
    terminates::Bool
    no_task_state::Bool
    inaccessible_mem_only::UInt8
    no_ub::UInt8
    non_overlayed::Bool
    function Effects(
        consistent::UInt8,
        effect_free::UInt8,
        no_throw::Bool,
        terminates::Bool,
        no_task_state::Bool,
        inaccessible_mem_only::UInt8,
        no_ub::UInt8,
        non_overlayed::Bool)
        return new(
            consistent,
            effect_free,
            no_throw,
            terminates,
            no_task_state,
            inaccessible_mem_only,
            no_ub,
            non_overlayed)
    end
end

const ALWAYS_TRUE  = 0x00
const ALWAYS_FALSE = 0x01

# :consistent-cy bits
const CONSISTENT_IF_NOTRETURNED         = 0x01 << 1
const CONSISTENT_IF_INACCESSIBLE_MEM_ONLY = 0x01 << 2

# :effect_free-ness bits
const EFFECT_FREE_IF_INACCESSIBLE_MEM_ONLY = 0x02

"""
`EFFECT_FREE_GLOBALLY` means that the statement is `:effect-free` and does not have a
caller-visible effect, but may not be removed from the function itself. This may e.g.
be used for effects that last only for the scope of the current function.
"""
const EFFECT_FREE_GLOBALLY = 0x03

# :inaccessible_mem_only bits
const INACCESSIBLEMEM_OR_ARGMEMONLY = 0x01 << 1

# :no_ub bits
const NOUB_IF_NOINBOUNDS = 0x01 << 1

const EFFECTS_TOTAL    = Effects(ALWAYS_TRUE,  ALWAYS_TRUE,  true,  true,  true,  ALWAYS_TRUE,  ALWAYS_TRUE,  true)
const EFFECTS_THROWS   = Effects(ALWAYS_TRUE,  ALWAYS_TRUE,  false, true,  true,  ALWAYS_TRUE,  ALWAYS_TRUE,  true)
const EFFECTS_UNKNOWN  = Effects(ALWAYS_FALSE, ALWAYS_FALSE, false, false, false, ALWAYS_FALSE, ALWAYS_FALSE, true) # unknown mostly, but it's not overlayed at least (e.g. it's not a call)
const _EFFECTS_UNKNOWN = Effects(ALWAYS_FALSE, ALWAYS_FALSE, false, false, false, ALWAYS_FALSE, ALWAYS_FALSE, false) # unknown really

function Effects(effects::Effects = _EFFECTS_UNKNOWN;
    consistent::UInt8 = effects.consistent,
    effect_free::UInt8 = effects.effect_free,
    no_throw::Bool = effects.no_throw,
    terminates::Bool = effects.terminates,
    no_task_state::Bool = effects.no_task_state,
    inaccessible_mem_only::UInt8 = effects.inaccessible_mem_only,
    no_ub::UInt8 = effects.no_ub,
    non_overlayed::Bool = effects.non_overlayed)
    return Effects(
        consistent,
        effect_free,
        no_throw,
        terminates,
        no_task_state,
        inaccessible_mem_only,
        no_ub,
        non_overlayed)
end

function is_better_effects(new::Effects, old::Effects)
    any_improved = false
    if new.consistent == ALWAYS_TRUE
        any_improved |= old.consistent != ALWAYS_TRUE
    else
        if !iszero(new.consistent & CONSISTENT_IF_NOTRETURNED)
            old.consistent == ALWAYS_TRUE && return false
            any_improved |= iszero(old.consistent & CONSISTENT_IF_NOTRETURNED)
        elseif !iszero(new.consistent & CONSISTENT_IF_INACCESSIBLE_MEM_ONLY)
            old.consistent == ALWAYS_TRUE && return false
            any_improved |= iszero(old.consistent & CONSISTENT_IF_INACCESSIBLE_MEM_ONLY)
        else
            return false
        end
    end
    if new.effect_free == ALWAYS_TRUE
        any_improved |= old.consistent != ALWAYS_TRUE
    elseif new.effect_free == EFFECT_FREE_IF_INACCESSIBLE_MEM_ONLY
        old.effect_free == ALWAYS_TRUE && return false
        any_improved |= old.effect_free != EFFECT_FREE_IF_INACCESSIBLE_MEM_ONLY
    elseif new.effect_free != old.effect_free
        return false
    end
    if new.no_throw
        any_improved |= !old.no_throw
    elseif new.no_throw != old.no_throw
        return false
    end
    if new.terminates
        any_improved |= !old.terminates
    elseif new.terminates != old.terminates
        return false
    end
    if new.no_task_state
        any_improved |= !old.no_task_state
    elseif new.no_task_state != old.no_task_state
        return false
    end
    if new.inaccessible_mem_only == ALWAYS_TRUE
        any_improved |= old.inaccessible_mem_only != ALWAYS_TRUE
    elseif new.inaccessible_mem_only == INACCESSIBLEMEM_OR_ARGMEMONLY
        old.inaccessible_mem_only == ALWAYS_TRUE && return false
        any_improved |= old.inaccessible_mem_only != INACCESSIBLEMEM_OR_ARGMEMONLY
    elseif new.inaccessible_mem_only != old.inaccessible_mem_only
        return false
    end
    if new.no_ub == ALWAYS_TRUE
        any_improved |= old.no_ub != ALWAYS_TRUE
    elseif new.no_ub == NOUB_IF_NOINBOUNDS
        old.no_ub == ALWAYS_TRUE && return false
        any_improved |= old.no_ub != NOUB_IF_NOINBOUNDS
    elseif new.no_ub != old.no_ub
        return false
    end
    if new.non_overlayed
        any_improved |= !old.non_overlayed
    elseif new.non_overlayed != old.non_overlayed
        return false
    end
    return any_improved
end

function merge_effects(old::Effects, new::Effects)
    return Effects(
        merge_effectbits(old.consistent, new.consistent),
        merge_effectbits(old.effect_free, new.effect_free),
        merge_effectbits(old.no_throw, new.no_throw),
        merge_effectbits(old.terminates, new.terminates),
        merge_effectbits(old.no_task_state, new.no_task_state),
        merge_effectbits(old.inaccessible_mem_only, new.inaccessible_mem_only),
        merge_effectbits(old.no_ub, new.no_ub),
        merge_effectbits(old.non_overlayed, new.non_overlayed))
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
is_no_throw(effects::Effects)             = effects.no_throw
is_terminates(effects::Effects)          = effects.terminates
is_no_task_state(effects::Effects)         = effects.no_task_state
is_inaccessible_mem_only(effects::Effects) = effects.inaccessible_mem_only === ALWAYS_TRUE
is_no_ub(effects::Effects)                = effects.no_ub === ALWAYS_TRUE
is_noub_if_noinbounds(effects::Effects)  = effects.no_ub === NOUB_IF_NOINBOUNDS
is_non_overlayed(effects::Effects)        = effects.non_overlayed

# implies `is_no_task_state` & `is_inaccessible_mem_only`, but not explicitly checked here
is_foldable(effects::Effects) =
    is_consistent(effects) &&
    (is_no_ub(effects) || is_noub_if_noinbounds(effects)) &&
    is_effect_free(effects) &&
    is_terminates(effects)

is_foldable_no_throw(effects::Effects) =
    is_foldable(effects) &&
    is_no_throw(effects)

# TODO add `is_no_ub` here?
is_removable_if_unused(effects::Effects) =
    is_effect_free(effects) &&
    is_terminates(effects) &&
    is_no_throw(effects)

is_finalizer_inlineable(effects::Effects) =
    is_no_throw(effects) &&
    is_no_task_state(effects)

is_consistent_if_notreturned(effects::Effects)         = !iszero(effects.consistent & CONSISTENT_IF_NOTRETURNED)
is_consistent_if_inaccessible_mem_only(effects::Effects) = !iszero(effects.consistent & CONSISTENT_IF_INACCESSIBLE_MEM_ONLY)

is_effect_free_if_inaccessible_mem_only(effects::Effects) = !iszero(effects.effect_free & EFFECT_FREE_IF_INACCESSIBLE_MEM_ONLY)

is_inaccessiblemem_or_argmemonly(effects::Effects) = effects.inaccessible_mem_only === INACCESSIBLEMEM_OR_ARGMEMONLY

function encode_effects(e::Effects)
    return ((e.consistent          % UInt32) << 0)  |
           ((e.effect_free         % UInt32) << 3)  |
           ((e.no_throw             % UInt32) << 5)  |
           ((e.terminates          % UInt32) << 6)  |
           ((e.no_task_state         % UInt32) << 7)  |
           ((e.inaccessible_mem_only % UInt32) << 8)  |
           ((e.no_ub                % UInt32) << 10) |
           ((e.non_overlayed        % UInt32) << 12)
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
    eo.no_throw             && (e |= (0x0001 << 2))
    eo.terminates_globally && (e |= (0x0001 << 3))
    eo.terminates_locally  && (e |= (0x0001 << 4))
    eo.no_task_state         && (e |= (0x0001 << 5))
    eo.inaccessible_mem_only && (e |= (0x0001 << 6))
    eo.no_ub                && (e |= (0x0001 << 7))
    eo.noub_if_noinbounds  && (e |= (0x0001 << 8))
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
        !iszero(e & (0x0001 << 8)))
end

decode_statement_effects_override(ssaflag::UInt32) =
    decode_effects_override(UInt16((ssaflag >> NUM_IR_FLAGS) & (1 << NUM_EFFECTS_OVERRIDES - 1)))

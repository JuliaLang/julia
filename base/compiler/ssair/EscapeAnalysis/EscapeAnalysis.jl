baremodule EscapeAnalysis

export
    analyze_escapes,
    cache_escapes!,
    getaliases,
    isaliased,
    has_no_escape,
    has_arg_escape,
    has_return_escape,
    has_thrown_escape,
    has_all_escape

const _TOP_MOD = ccall(:jl_base_relative_to, Any, (Any,), EscapeAnalysis)::Module

# imports
import ._TOP_MOD: ==, getindex, setindex!
# usings
import Core:
    MethodInstance, Const, Argument, SSAValue, PiNode, PhiNode, UpsilonNode, PhiCNode,
    ReturnNode, GotoNode, GotoIfNot, SimpleVector, sizeof, ifelse, arrayset, arrayref,
    arraysize
import ._TOP_MOD:     # Base definitions
    @__MODULE__, @eval, @assert, @specialize, @nospecialize, @inbounds, @inline, @noinline,
    @label, @goto, !, !==, !=, ≠, +, -, *, ≤, <, ≥, >, &, |, include, error, missing, copy,
    Vector, BitSet, IdDict, IdSet, UnitRange, Csize_t, ∪, ⊆, ∩, :, ∈, ∉, in, length, get,
    first, last, haskey, keys, get!, isempty, isassigned, pop!, push!, pushfirst!, empty!,
    delete!, max, min
import Core.Compiler: # Core.Compiler specific definitions
    isbitstype, isexpr, is_meta_expr_head, println,
    IRCode, IR_FLAG_EFFECT_FREE, widenconst, argextype, singleton_type, fieldcount_noerror,
    try_compute_field, try_compute_fieldidx, hasintersect, ⊑ as ⊑ₜ, intrinsic_nothrow,
    array_builtin_common_typecheck, arrayset_typecheck, setfield!_nothrow, alloc_array_ndims

if _TOP_MOD !== Core.Compiler
    include(@__MODULE__, "disjoint_set.jl")
else
    include(@__MODULE__, "compiler/ssair/EscapeAnalysis/disjoint_set.jl")
end

const AInfo = BitSet # XXX better to be IdSet{Int}?
struct IndexableFields
    infos::Vector{AInfo}
end
struct IndexableElements
    infos::IdDict{Int,AInfo}
end
struct Unindexable
    array::Bool
    info::AInfo
end

merge_to_unindexable(AliasInfo::IndexableFields) = Unindexable(false, merge_to_unindexable(AliasInfo.infos))
function merge_to_unindexable(AliasInfo::Unindexable, AliasInfos::IndexableFields)
    @assert !AliasInfo.array "invalid merge_to_unindexable"
    return Unindexable(false, merge_to_unindexable(AliasInfo.info, AliasInfos.infos))
end
merge_to_unindexable(infos::Vector{AInfo}) = merge_to_unindexable(AInfo(), infos)
function merge_to_unindexable(info::AInfo, infos::Vector{AInfo})
    for i = 1:length(infos)
        info = info ∪ infos[i]
    end
    return info
end
merge_to_unindexable(AliasInfo::IndexableElements) = Unindexable(true, merge_to_unindexable(AliasInfo.infos))
function merge_to_unindexable(AliasInfo::Unindexable, AliasInfos::IndexableElements)
    @assert AliasInfo.array "invalid merge_to_unindexable"
    return Unindexable(true, merge_to_unindexable(AliasInfo.info, AliasInfos.infos))
end
merge_to_unindexable(infos::IdDict{Int,AInfo}) = merge_to_unindexable(AInfo(), infos)
function merge_to_unindexable(info::AInfo, infos::IdDict{Int,AInfo})
    for idx in keys(infos)
        info = info ∪ infos[idx]
    end
    return info
end

const LivenessSet = BitSet

"""
    x::EscapeInfo

A lattice for escape information, which holds the following properties:
- `x.Analyzed::Bool`: not formally part of the lattice, only indicates `x` has not been analyzed or not
- `x.ReturnEscape::Bool`: indicates `x` can escape to the caller via return
- `x.ThrownEscape::BitSet`: records SSA statements numbers where `x` can be thrown as exception:
  this information will be used by `escape_exception!` to propagate potential escapes via exception
- `x.AliasInfo::Union{IndexableFields,Unindexable,Bool}`: maintains all possible values
  that can be aliased to fields or array elements of `x`:
  * `x.AliasInfo === false` indicates the fields/elements of `x` isn't analyzed yet
  * `x.AliasInfo === true` indicates the fields/elements of `x` can't be analyzed,
    e.g. the type of `x` is not known or is not concrete and thus its fields/elements
    can't be known precisely
  * `x.AliasInfo::IndexableFields` records all the possible values that can be aliased to fields of object `x` with precise index information
  * `x.AliasInfo::IndexableElements` records all the possible values that can be aliased to elements of array `x` with precise index information
  * `x.AliasInfo::Unindexable` records all the possible values that can be aliased to fields/elements of `x` without precise index information
- `x.Liveness::BitSet`: records SSA statement numbers where `x` should be live, e.g.
  to be used as a call argument, to be returned to a caller, or preserved for `:foreigncall`.
  `0 ∈ x.Liveness` has the special meaning that it's a call argument of the currently analyzed
  call frame (and thus it's visible from the caller immediately).
- `x.ArgEscape::Int` (not implemented yet): indicates it will escape to the caller through
  `setfield!` on argument(s)
  * `-1` : no escape
  * `0` : unknown or multiple
  * `n` : through argument N

There are utility constructors to create common `EscapeInfo`s, e.g.,
- `NoEscape()`: the bottom(-like) element of this lattice, meaning it won't escape to anywhere
- `AllEscape()`: the topmost element of this lattice, meaning it will escape to everywhere

`analyze_escapes` will transition these elements from the bottom to the top,
in the same direction as Julia's native type inference routine.
An abstract state will be initialized with the bottom(-like) elements:
- the call arguments are initialized as `ArgEscape()`, whose `Liveness` property includes `0`
  to indicate that it is passed as a call argument and visible from a caller immediately
- the other states are initialized as `NotAnalyzed()`, which is a special lattice element that
  is slightly lower than `NoEscape`, but at the same time doesn't represent any meaning
  other than it's not analyzed yet (thus it's not formally part of the lattice)
"""
struct EscapeInfo
    Analyzed::Bool
    ReturnEscape::Bool
    ThrownEscape::LivenessSet
    AliasInfo #::Union{IndexableFields,IndexableElements,Unindexable,Bool}
    Liveness::LivenessSet
    # TODO: ArgEscape::Int

    function EscapeInfo(
        Analyzed::Bool,
        ReturnEscape::Bool,
        ThrownEscape::LivenessSet,
        AliasInfo#=::Union{IndexableFields,IndexableElements,Unindexable,Bool}=#,
        Liveness::LivenessSet,
        )
        @nospecialize AliasInfo
        return new(
            Analyzed,
            ReturnEscape,
            ThrownEscape,
            AliasInfo,
            Liveness,
            )
    end
    function EscapeInfo(
        x::EscapeInfo,
        # non-concrete fields should be passed as default arguments
        # in order to avoid allocating non-concrete `NamedTuple`s
        AliasInfo#=::Union{IndexableFields,IndexableElements,Unindexable,Bool}=# = x.AliasInfo;
        Analyzed::Bool = x.Analyzed,
        ReturnEscape::Bool = x.ReturnEscape,
        ThrownEscape::LivenessSet = x.ThrownEscape,
        Liveness::LivenessSet = x.Liveness,
        )
        @nospecialize AliasInfo
        return new(
            Analyzed,
            ReturnEscape,
            ThrownEscape,
            AliasInfo,
            Liveness,
            )
    end
end

# precomputed default values in order to eliminate computations at each callsite
const BOT_THROWN_ESCAPE = LivenessSet()
const TOP_THROWN_ESCAPE = LivenessSet(1:100_000)

const BOT_ALIAS_INFO = false
const TOP_ALIAS_INFO = true

const BOT_LIVENESS = LivenessSet()
const TOP_LIVENESS = LivenessSet(0:100_000)
const ARG_LIVENESS = LivenessSet(0)

# the constructors
NotAnalyzed() = EscapeInfo(false, false, BOT_THROWN_ESCAPE, BOT_ALIAS_INFO, BOT_LIVENESS) # not formally part of the lattice
NoEscape() = EscapeInfo(true, false, BOT_THROWN_ESCAPE, BOT_ALIAS_INFO, BOT_LIVENESS)
ArgEscape() = EscapeInfo(true, false, BOT_THROWN_ESCAPE, TOP_ALIAS_INFO, ARG_LIVENESS) # TODO allow interprocedural alias analysis?
ReturnEscape(pc::Int) = EscapeInfo(true, true, BOT_THROWN_ESCAPE, BOT_ALIAS_INFO, LivenessSet(pc))
AllReturnEscape() = EscapeInfo(true, true, BOT_THROWN_ESCAPE, BOT_ALIAS_INFO, TOP_LIVENESS)
ThrownEscape(pc::Int) = EscapeInfo(true, false, LivenessSet(pc), BOT_ALIAS_INFO, BOT_LIVENESS)
AllEscape() = EscapeInfo(true, true, TOP_THROWN_ESCAPE, TOP_ALIAS_INFO, TOP_LIVENESS)

const ⊥, ⊤ = NotAnalyzed(), AllEscape()

# Convenience names for some ⊑ queries
has_no_escape(x::EscapeInfo) = !x.ReturnEscape && isempty(x.ThrownEscape) && 0 ∉ x.Liveness
has_arg_escape(x::EscapeInfo) = 0 in x.Liveness
has_return_escape(x::EscapeInfo) = x.ReturnEscape
has_return_escape(x::EscapeInfo, pc::Int) = x.ReturnEscape && pc in x.Liveness
has_thrown_escape(x::EscapeInfo) = !isempty(x.ThrownEscape)
has_thrown_escape(x::EscapeInfo, pc::Int) = pc in x.ThrownEscape
has_all_escape(x::EscapeInfo) = ⊤ ⊑ x

# utility lattice constructors
ignore_argescape(x::EscapeInfo) = EscapeInfo(x; Liveness=delete!(copy(x.Liveness), 0))
ignore_thrownescapes(x::EscapeInfo) = EscapeInfo(x; ThrownEscape=BOT_THROWN_ESCAPE)
ignore_aliasinfo(x::EscapeInfo) = EscapeInfo(x, BOT_ALIAS_INFO)
ignore_liveness(x::EscapeInfo) = EscapeInfo(x; Liveness=BOT_LIVENESS)

# we need to make sure this `==` operator corresponds to lattice equality rather than object equality,
# otherwise `propagate_changes` can't detect the convergence
x::EscapeInfo == y::EscapeInfo = begin
    # fast pass: better to avoid top comparison
    x === y && return true
    x.Analyzed === y.Analyzed || return false
    x.ReturnEscape === y.ReturnEscape || return false
    xt, yt = x.ThrownEscape, y.ThrownEscape
    if xt === TOP_THROWN_ESCAPE
        yt === TOP_THROWN_ESCAPE || return false
    elseif yt === TOP_THROWN_ESCAPE
        return false # x.ThrownEscape === TOP_THROWN_ESCAPE
    else
        xt == yt || return false
    end
    xa, ya = x.AliasInfo, y.AliasInfo
    if isa(xa, Bool)
        xa === ya || return false
    elseif isa(xa, IndexableFields)
        isa(ya, IndexableFields) || return false
        xa.infos == ya.infos || return false
    elseif isa(xa, IndexableElements)
        isa(ya, IndexableElements) || return false
        xa.infos == ya.infos || return false
    else
        xa = xa::Unindexable
        isa(ya, Unindexable) || return false
        xa.array === ya.array || return false
        xa.info == ya.info || return false
    end
    xl, yl = x.Liveness, y.Liveness
    if xl === TOP_LIVENESS
        yl === TOP_LIVENESS || return false
    elseif yl === TOP_LIVENESS
        return false # x.Liveness === TOP_LIVENESS
    else
        xl == yl || return false
    end
    return true
end

"""
    x::EscapeInfo ⊑ y::EscapeInfo -> Bool

The non-strict partial order over `EscapeInfo`.
"""
x::EscapeInfo ⊑ y::EscapeInfo = begin
    # fast pass: better to avoid top comparison
    if y === ⊤
        return true
    elseif x === ⊤
        return false # return y === ⊤
    elseif x === ⊥
        return true
    elseif y === ⊥
        return false # return x === ⊥
    end
    x.Analyzed ≤ y.Analyzed || return false
    x.ReturnEscape ≤ y.ReturnEscape || return false
    xt, yt = x.ThrownEscape, y.ThrownEscape
    if xt === TOP_THROWN_ESCAPE
        yt !== TOP_THROWN_ESCAPE && return false
    elseif yt !== TOP_THROWN_ESCAPE
        xt ⊆ yt || return false
    end
    xa, ya = x.AliasInfo, y.AliasInfo
    if isa(xa, Bool)
        xa && ya !== true && return false
    elseif isa(xa, IndexableFields)
        if isa(ya, IndexableFields)
            xinfos, yinfos = xa.infos, ya.infos
            xn, yn = length(xinfos), length(yinfos)
            xn > yn && return false
            for i in 1:xn
                xinfos[i] ⊆ yinfos[i] || return false
            end
        elseif isa(ya, IndexableElements)
            return false
        elseif isa(ya, Unindexable)
            ya.array && return false
            xinfos, yinfo = xa.infos, ya.info
            for i = length(xinfos)
                xinfos[i] ⊆ yinfo || return false
            end
        else
            ya === true || return false
        end
    elseif isa(xa, IndexableElements)
        if isa(ya, IndexableElements)
            xinfos, yinfos = xa.infos, ya.infos
            keys(xinfos) ⊆ keys(yinfos) || return false
            for idx in keys(xinfos)
                xinfos[idx] ⊆ yinfos[idx] || return false
            end
        elseif isa(ya, IndexableFields)
            return false
        elseif isa(ya, Unindexable)
            ya.array || return false
            xinfos, yinfo = xa.infos, ya.info
            for idx in keys(xinfos)
                xinfos[idx] ⊆ yinfo || return false
            end
        else
            ya === true || return false
        end
    else
        xa = xa::Unindexable
        if isa(ya, Unindexable)
            xa.array === ya.array || return false
            xinfo, yinfo = xa.info, ya.info
            xinfo ⊆ yinfo || return false
        else
            ya === true || return false
        end
    end
    xl, yl = x.Liveness, y.Liveness
    if xl === TOP_LIVENESS
        yl !== TOP_LIVENESS && return false
    elseif yl !== TOP_LIVENESS
        xl ⊆ yl || return false
    end
    return true
end

"""
    x::EscapeInfo ⊏ y::EscapeInfo -> Bool

The strict partial order over `EscapeInfo`.
This is defined as the irreflexive kernel of `⊏`.
"""
x::EscapeInfo ⊏ y::EscapeInfo = x ⊑ y && !(y ⊑ x)

"""
    x::EscapeInfo ⋤ y::EscapeInfo -> Bool

This order could be used as a slightly more efficient version of the strict order `⊏`,
where we can safely assume `x ⊑ y` holds.
"""
x::EscapeInfo ⋤ y::EscapeInfo = !(y ⊑ x)

"""
    x::EscapeInfo ⊔ y::EscapeInfo -> EscapeInfo

Computes the join of `x` and `y` in the partial order defined by `EscapeInfo`.
"""
x::EscapeInfo ⊔ y::EscapeInfo = begin
    # fast pass: better to avoid top join
    if x === ⊤ || y === ⊤
        return ⊤
    elseif x === ⊥
        return y
    elseif y === ⊥
        return x
    end
    xt, yt = x.ThrownEscape, y.ThrownEscape
    if xt === TOP_THROWN_ESCAPE || yt === TOP_THROWN_ESCAPE
        ThrownEscape = TOP_THROWN_ESCAPE
    elseif xt === BOT_THROWN_ESCAPE
        ThrownEscape = yt
    elseif yt === BOT_THROWN_ESCAPE
        ThrownEscape = xt
    else
        ThrownEscape = xt ∪ yt
    end
    xa, ya = x.AliasInfo, y.AliasInfo
    if xa === true || ya === true
        AliasInfo = true
    elseif xa === false
        AliasInfo = ya
    elseif ya === false
        AliasInfo = xa
    elseif isa(xa, IndexableFields)
        if isa(ya, IndexableFields)
            xinfos, yinfos = xa.infos, ya.infos
            xn, yn = length(xinfos), length(yinfos)
            nmax, nmin = max(xn, yn), min(xn, yn)
            infos = Vector{AInfo}(undef, nmax)
            for i in 1:nmax
                if i > nmin
                    infos[i] = (xn > yn ? xinfos : yinfos)[i]
                else
                    infos[i] = xinfos[i] ∪ yinfos[i]
                end
            end
            AliasInfo = IndexableFields(infos)
        elseif isa(ya, Unindexable) && !ya.array
            xinfos, yinfo = xa.infos, ya.info
            AliasInfo = merge_to_unindexable(ya, xa)
        else
            AliasInfo = true # handle conflicting case conservatively
        end
    elseif isa(xa, IndexableElements)
        if isa(ya, IndexableElements)
            xinfos, yinfos = xa.infos, ya.infos
            infos = IdDict{Int,AInfo}()
            for idx in keys(xinfos)
                if !haskey(yinfos, idx)
                    infos[idx] = xinfos[idx]
                else
                    infos[idx] = xinfos[idx] ∪ yinfos[idx]
                end
            end
            for idx in keys(yinfos)
                haskey(xinfos, idx) && continue # unioned already
                infos[idx] = yinfos[idx]
            end
            AliasInfo = IndexableElements(infos)
        elseif isa(ya, Unindexable) && ya.array
            AliasInfo = merge_to_unindexable(ya, xa)
        else
            AliasInfo = true # handle conflicting case conservatively
        end
    else
        xa = xa::Unindexable
        if isa(ya, IndexableFields) && !xa.array
            AliasInfo = merge_to_unindexable(xa, ya)
        elseif isa(ya, IndexableElements) && xa.array
            AliasInfo = merge_to_unindexable(xa, ya)
        elseif isa(ya, Unindexable) && xa.array === ya.array
            xinfo, yinfo = xa.info, ya.info
            info = xinfo ∪ yinfo
            AliasInfo = Unindexable(xa.array, info)
        else
            AliasInfo = true # handle conflicting case conservatively
        end
    end
    xl, yl = x.Liveness, y.Liveness
    if xl === TOP_LIVENESS || yl === TOP_LIVENESS
        Liveness = TOP_LIVENESS
    elseif xl === BOT_LIVENESS
        Liveness = yl
    elseif yl === BOT_LIVENESS
        Liveness = xl
    else
        Liveness = xl ∪ yl
    end
    return EscapeInfo(
        x.Analyzed | y.Analyzed,
        x.ReturnEscape | y.ReturnEscape,
        ThrownEscape,
        AliasInfo,
        Liveness,
        )
end

# TODO setup a more effient struct for cache
# which can discard escape information on SSS values and arguments that don't join dispatch signature

const AliasSet = IntDisjointSet{Int}

const ArrayInfo = IdDict{Int,Vector{Int}}

"""
    estate::EscapeState

Extended lattice that maps arguments and SSA values to escape information represented as [`EscapeInfo`](@ref).
Escape information imposed on SSA IR element `x` can be retrieved by `estate[x]`.
"""
struct EscapeState
    escapes::Vector{EscapeInfo}
    aliasset::AliasSet
    nargs::Int
    arrayinfo::Union{Nothing,ArrayInfo} # TODO make this aware of aliasing of nested arrays
end
function EscapeState(nargs::Int, nstmts::Int, arrayinfo::Union{Nothing,ArrayInfo})
    escapes = EscapeInfo[
        1 ≤ i ≤ nargs ? ArgEscape() : ⊥ for i in 1:(nargs+nstmts)]
    aliaset = AliasSet(nargs+nstmts)
    return EscapeState(escapes, aliaset, nargs, arrayinfo)
end
function getindex(estate::EscapeState, @nospecialize(x))
    xidx = iridx(x, estate)
    return xidx === nothing ? nothing : estate.escapes[xidx]
end
function setindex!(estate::EscapeState, v::EscapeInfo, @nospecialize(x))
    xidx = iridx(x, estate)
    if xidx !== nothing
        estate.escapes[xidx] = v
    end
    return estate
end

"""
    iridx(x, estate::EscapeState) -> xidx::Union{Int,Nothing}

Tries to convert analyzable IR element `x::Union{Argument,SSAValue}` to
its unique identifier number `xidx` that is valid in the analysis context of `estate`.
Returns `nothing` if `x` isn't maintained by `estate` and thus unanalyzable (e.g. `x::GlobalRef`).

`irval` is the inverse function of `iridx` (not formally), i.e.
`irval(iridx(x::Union{Argument,SSAValue}, state), state) === x`.
"""
function iridx(@nospecialize(x), estate::EscapeState)
    if isa(x, Argument)
        xidx = x.n
        @assert 1 ≤ xidx ≤ estate.nargs "invalid Argument"
    elseif isa(x, SSAValue)
        xidx = x.id + estate.nargs
    else
        return nothing
    end
    return xidx
end

"""
    irval(xidx::Int, estate::EscapeState) -> x::Union{Argument,SSAValue}

Converts its unique identifier number `xidx` to the original IR element `x::Union{Argument,SSAValue}`
that is analyzable in the context of `estate`.

`iridx` is the inverse function of `irval` (not formally), i.e.
`iridx(irval(xidx, state), state) === xidx`.
"""
function irval(xidx::Int, estate::EscapeState)
    x = xidx > estate.nargs ? SSAValue(xidx-estate.nargs) : Argument(xidx)
    return x
end

function getaliases(x::Union{Argument,SSAValue}, estate::EscapeState)
    xidx = iridx(x, estate)
    aliases = getaliases(xidx, estate)
    aliases === nothing && return nothing
    return Union{Argument,SSAValue}[irval(aidx, estate) for aidx in aliases]
end
function getaliases(xidx::Int, estate::EscapeState)
    aliasset = estate.aliasset
    root = find_root!(aliasset, xidx)
    if xidx ≠ root || aliasset.ranks[xidx] > 0
        # the size of this alias set containing `key` is larger than 1,
        # collect the entire alias set
        aliases = Int[]
        for aidx in 1:length(aliasset.parents)
            if aliasset.parents[aidx] == root
                push!(aliases, aidx)
            end
        end
        return aliases
    else
        return nothing
    end
end

isaliased(x::Union{Argument,SSAValue}, y::Union{Argument,SSAValue}, estate::EscapeState) =
    isaliased(iridx(x, estate), iridx(y, estate), estate)
isaliased(xidx::Int, yidx::Int, estate::EscapeState) =
    in_same_set(estate.aliasset, xidx, yidx)

"""
    ArgEscapeInfo(x::EscapeInfo) -> x′::ArgEscapeInfo

The data structure for caching `x::EscapeInfo` for interprocedural propagation,
which is slightly more efficient than the original `x::EscapeInfo` object.
"""
struct ArgEscapeInfo
    AllEscape::Bool
    ReturnEscape::Bool
    ThrownEscape::Bool
    function ArgEscapeInfo(x::EscapeInfo)
        x === ⊤ && return new(true, true, true)
        ThrownEscape = isempty(x.ThrownEscape) ? false : true
        return new(false, x.ReturnEscape, ThrownEscape)
    end
end

"""
    cache_escapes!(linfo::MethodInstance, estate::EscapeState, _::IRCode)

Transforms escape information of `estate` for interprocedural propagation,
and caches it in a global cache that can then be looked up later when
`linfo` callsite is seen again.
"""
function cache_escapes! end

# when working outside of Core.Compiler, cache as much as information for later inspection and debugging
if _TOP_MOD !== Core.Compiler
    struct EscapeCache
        cache::Vector{ArgEscapeInfo}
        state::EscapeState # preserved just for debugging purpose
        ir::IRCode         # preserved just for debugging purpose
    end
    const GLOBAL_ESCAPE_CACHE = IdDict{MethodInstance,EscapeCache}()
    function cache_escapes!(linfo::MethodInstance, estate::EscapeState, cacheir::IRCode)
        cache = EscapeCache(to_interprocedural(estate), estate, cacheir)
        GLOBAL_ESCAPE_CACHE[linfo] = cache
        return cache
    end
    argescapes_from_cache(cache::EscapeCache) = cache.cache
else
    const GLOBAL_ESCAPE_CACHE = IdDict{MethodInstance,Vector{ArgEscapeInfo}}()
    function cache_escapes!(linfo::MethodInstance, estate::EscapeState, _::IRCode)
        cache = to_interprocedural(estate)
        GLOBAL_ESCAPE_CACHE[linfo] = cache
        return cache
    end
    argescapes_from_cache(cache::Vector{ArgEscapeInfo}) = cache
end

function to_interprocedural(estate::EscapeState)
    cache = Vector{ArgEscapeInfo}(undef, estate.nargs)
    for i = 1:estate.nargs
        cache[i] = ArgEscapeInfo(estate.escapes[i])
    end
    return cache
end

__clear_escape_cache!() = empty!(GLOBAL_ESCAPE_CACHE)

abstract type Change end
struct EscapeChange <: Change
    xidx::Int
    xinfo::EscapeInfo
end
struct AliasChange <: Change
    xidx::Int
    yidx::Int
end
struct LivenessChange <: Change
    xidx::Int
    livepc::Int
end
const Changes = Vector{Change}

struct AnalysisState
    ir::IRCode
    estate::EscapeState
    changes::Changes
end

function getinst(ir::IRCode, idx::Int)
    nstmts = length(ir.stmts)
    if idx ≤ nstmts
        return ir.stmts[idx]
    else
        return ir.new_nodes.stmts[idx - nstmts]
    end
end

"""
    analyze_escapes(ir::IRCode, nargs::Int) -> estate::EscapeState

Analyzes escape information in `ir`.
`nargs` is the number of actual arguments of the analyzed call.
"""
function analyze_escapes(ir::IRCode, nargs::Int)
    stmts = ir.stmts
    nstmts = length(stmts) + length(ir.new_nodes.stmts)

    tryregions, arrayinfo = compute_frameinfo(ir)
    estate = EscapeState(nargs, nstmts, arrayinfo)
    changes = Changes() # keeps changes that happen at current statement
    astate = AnalysisState(ir, estate, changes)

    local debug_itr_counter = 0
    while true
        local anyupdate = false

        for pc in nstmts:-1:1
            stmt = getinst(ir, pc)[:inst]

            # collect escape information
            if isa(stmt, Expr)
                head = stmt.head
                if head === :call
                    escape_call!(astate, pc, stmt.args)
                elseif head === :invoke
                    escape_invoke!(astate, pc, stmt.args)
                elseif head === :new || head === :splatnew
                    escape_new!(astate, pc, stmt.args)
                elseif head === :(=)
                    lhs, rhs = stmt.args
                    if isa(lhs, GlobalRef) # global store
                        add_escape_change!(astate, rhs, ⊤)
                    else
                        unexpected_assignment!(ir, pc)
                    end
                elseif head === :foreigncall
                    escape_foreigncall!(astate, pc, stmt.args)
                elseif head === :throw_undef_if_not # XXX when is this expression inserted ?
                    add_escape_change!(astate, stmt.args[1], ThrownEscape(pc))
                elseif is_meta_expr_head(head)
                    # meta expressions doesn't account for any usages
                    continue
                elseif head === :enter || head === :leave || head === :the_exception || head === :pop_exception
                    # ignore these expressions since escapes via exceptions are handled by `escape_exception!`
                    # `escape_exception!` conservatively propagates `AllEscape` anyway,
                    # and so escape information imposed on `:the_exception` isn't computed
                    continue
                elseif head === :static_parameter ||  # this exists statically, not interested in its escape
                       head === :copyast ||           # XXX can this account for some escapes?
                       head === :undefcheck ||        # XXX can this account for some escapes?
                       head === :isdefined ||         # just returns `Bool`, nothing accounts for any escapes
                       head === :gc_preserve_begin || # `GC.@preserve` expressions themselves won't be used anywhere
                       head === :gc_preserve_end      # `GC.@preserve` expressions themselves won't be used anywhere
                    continue
                else
                    add_conservative_changes!(astate, pc, stmt.args)
                end
            elseif isa(stmt, ReturnNode)
                if isdefined(stmt, :val)
                    add_escape_change!(astate, stmt.val, ReturnEscape(pc))
                end
            elseif isa(stmt, PhiNode)
                escape_edges!(astate, pc, stmt.values)
            elseif isa(stmt, PiNode)
                escape_val_ifdefined!(astate, pc, stmt)
            elseif isa(stmt, PhiCNode)
                escape_edges!(astate, pc, stmt.values)
            elseif isa(stmt, UpsilonNode)
                escape_val_ifdefined!(astate, pc, stmt)
            elseif isa(stmt, GlobalRef) # global load
                add_escape_change!(astate, SSAValue(pc), ⊤)
            elseif isa(stmt, SSAValue)
                escape_val!(astate, pc, stmt)
            elseif isa(stmt, Argument)
                escape_val!(astate, pc, stmt)
            else # otherwise `stmt` can be GotoNode, GotoIfNot, and inlined values etc.
                continue
            end

            isempty(changes) && continue

            anyupdate |= propagate_changes!(estate, changes)

            empty!(changes)
        end

        tryregions !== nothing && escape_exception!(astate, tryregions)

        debug_itr_counter += 1

        anyupdate || break
    end

    # if debug_itr_counter > 2
    #     println("[EA] excessive iteration count found ", debug_itr_counter, " (", singleton_type(ir.argtypes[1]), ")")
    # end

    return estate
end

# propagate changes, and check convergence
function propagate_changes!(estate::EscapeState, changes::Changes)
    local anychanged = false
    for change in changes
        if isa(change, EscapeChange)
            anychanged |= propagate_escape_change!(estate, change)
        elseif isa(change, LivenessChange)
            anychanged |= propagate_liveness_change!(estate, change)
        else
            change = change::AliasChange
            anychanged |= propagate_alias_change!(estate, change)
        end
    end
    return anychanged
end

@inline propagate_escape_change!(estate::EscapeState, change::EscapeChange) =
    propagate_escape_change!(⊔, estate, change)

# allows this to work as lattice join as well as lattice meet
@inline function propagate_escape_change!(@specialize(op),
    estate::EscapeState, change::EscapeChange)
    (; xidx, xinfo) = change
    anychanged = _propagate_escape_change!(op, estate, xidx, xinfo)
    aliases = getaliases(xidx, estate)
    if aliases !== nothing
        for aidx in aliases
            anychanged |= _propagate_escape_change!(op, estate, aidx, xinfo)
        end
    end
    return anychanged
end

@inline function _propagate_escape_change!(@specialize(op),
    estate::EscapeState, xidx::Int, info::EscapeInfo)
    old = estate.escapes[xidx]
    new = op(old, info)
    if old ≠ new
        estate.escapes[xidx] = new
        return true
    end
    return false
end

# propagate Liveness changes separately in order to avoid constructing too many LivenessSet
@inline function propagate_liveness_change!(estate::EscapeState, change::LivenessChange)
    (; xidx, livepc) = change
    info = estate.escapes[xidx]
    Liveness = info.Liveness
    Liveness === TOP_LIVENESS && return false
    livepc in Liveness && return false
    if Liveness === BOT_LIVENESS || Liveness === ARG_LIVENESS
        # if this Liveness is a constant, we shouldn't modify it and propagate this change as a new EscapeInfo
        Liveness = copy(Liveness)
        push!(Liveness, livepc)
        estate.escapes[xidx] = EscapeInfo(info; Liveness)
        return true
    else
        # directly modify Liveness property in order to avoid excessive copies
        push!(Liveness, livepc)
        return true
    end
end

@inline function propagate_alias_change!(estate::EscapeState, change::AliasChange)
    (; xidx, yidx) = change
    xroot = find_root!(estate.aliasset, xidx)
    yroot = find_root!(estate.aliasset, yidx)
    if xroot ≠ yroot
        union!(estate.aliasset, xroot, yroot)
        xinfo = estate.escapes[xidx]
        yinfo = estate.escapes[yidx]
        xyinfo = xinfo ⊔ yinfo
        estate.escapes[xidx] = xyinfo
        estate.escapes[yidx] = xyinfo
        return true
    end
    return false
end

function add_escape_change!(astate::AnalysisState, @nospecialize(x), xinfo::EscapeInfo)
    xinfo === ⊥ && return nothing # performance optimization
    xidx = iridx(x, astate.estate)
    if xidx !== nothing
        if !isbitstype(widenconst(argextype(x, astate.ir)))
            push!(astate.changes, EscapeChange(xidx, xinfo))
        end
    end
    return nothing
end

function add_liveness_change!(astate::AnalysisState, @nospecialize(x), livepc::Int)
    xidx = iridx(x, astate.estate)
    if xidx !== nothing
        if !isbitstype(widenconst(argextype(x, astate.ir)))
            push!(astate.changes, LivenessChange(xidx, livepc))
        end
    end
    return nothing
end

function add_alias_change!(astate::AnalysisState, @nospecialize(x), @nospecialize(y))
    if isa(x, GlobalRef)
        return add_escape_change!(astate, y, ⊤)
    elseif isa(y, GlobalRef)
        return add_escape_change!(astate, x, ⊤)
    end
    estate = astate.estate
    xidx = iridx(x, estate)
    yidx = iridx(y, estate)
    if xidx !== nothing && yidx !== nothing && !isaliased(xidx, yidx, astate.estate)
        pushfirst!(astate.changes, AliasChange(xidx, yidx))
    end
    return nothing
end

function escape_edges!(astate::AnalysisState, pc::Int, edges::Vector{Any})
    ret = SSAValue(pc)
    for i in 1:length(edges)
        if isassigned(edges, i)
            v = edges[i]
            add_alias_change!(astate, ret, v)
        end
    end
end

function escape_val_ifdefined!(astate::AnalysisState, pc::Int, x)
    if isdefined(x, :val)
        escape_val!(astate, pc, x.val)
    end
end

function escape_val!(astate::AnalysisState, pc::Int, @nospecialize(val))
    ret = SSAValue(pc)
    add_alias_change!(astate, ret, val)
end

# NOTE if we don't maintain the alias set that is separated from the lattice state, we can do
# something like below: it essentially incorporates forward escape propagation in our default
# backward propagation, and leads to inefficient convergence that requires more iterations
# # lhs = rhs: propagate escape information of `rhs` to `lhs`
# function escape_alias!(astate::AnalysisState, @nospecialize(lhs), @nospecialize(rhs))
#     if isa(rhs, SSAValue) || isa(rhs, Argument)
#         vinfo = astate.estate[rhs]
#     else
#         return
#     end
#     add_escape_change!(astate, lhs, vinfo)
# end

# a preparatory linear scan to find:
# - regions in which potential throws will be caught
# - array allocations whose dimensions are known precisely (with some very simple alias analysis)
function compute_frameinfo(ir::IRCode)
    tryregions, arrayinfo = nothing, nothing
    nstmts, nnewnodes = length(ir.stmts), length(ir.new_nodes.stmts)
    for idx in 1:nstmts+nnewnodes
        stmt = getinst(ir, idx)[:inst]
        if isexpr(stmt, :enter)
            @assert idx ≤ nstmts "try/catch inside new_nodes unsupported"
            tryregions === nothing && (tryregions = UnitRange{Int}[])
            leave_block = stmt.args[1]::Int
            leave_pc = first(ir.cfg.blocks[leave_block].stmts)
            push!(tryregions, idx:leave_pc)
        elseif isexpr(stmt, :foreigncall)
            args = stmt.args
            name = args[1]
            nn = normalize(name)
            isa(nn, Symbol) || @goto next_stmt
            ndims = alloc_array_ndims(nn)
            ndims === nothing && @goto next_stmt
            if ndims ≠ 0
                length(args) ≥ ndims+6 || @goto next_stmt
                dims = Int[]
                for i in 1:ndims
                    dim = argextype(args[i+6], ir)
                    isa(dim, Const) || @goto next_stmt
                    dim = dim.val
                    isa(dim, Int) || @goto next_stmt
                    push!(dims, dim)
                end
            else
                length(args) ≥ 7 || @goto next_stmt
                dims = argextype(args[7], ir)
                if isa(dims, Const)
                    dims = dims.val
                    isa(dims, Tuple{Vararg{Int}}) || @goto next_stmt
                    dims = collect(Int, dims)
                else
                    dims === Tuple{} || @goto next_stmt
                    dims = Int[]
                end
            end
            if arrayinfo === nothing
                arrayinfo = ArrayInfo()
            end
            arrayinfo[idx] = dims
        elseif arrayinfo !== nothing
            if isa(stmt, PhiNode)
                values = stmt.values
                local dims = nothing
                for i = 1:length(values)
                    if isassigned(values, i)
                        val = values[i]
                        if isa(val, SSAValue) && haskey(arrayinfo, val.id)
                            if dims === nothing
                                dims = arrayinfo[val.id]
                            elseif dims ≠ arrayinfo[val.id]
                                dims = nothing
                                break
                            end
                        end
                    end
                end
                if dims !== nothing
                    arrayinfo[idx] = dims
                end
            elseif isa(stmt, PiNode)
                if isdefined(stmt, :val)
                    val = stmt.val
                    if isa(val, SSAValue) && haskey(arrayinfo, val.id)
                        arrayinfo[idx] = arrayinfo[val.id]
                    end
                end
            end
        end
        @label next_stmt
    end
    return tryregions, arrayinfo
end

"""
    escape_exception!(astate::AnalysisState, tryregions::Vector{UnitRange{Int}})

Propagates escapes via exceptions that can happen in `tryregions`.

Naively it seems enough to propagate escape information imposed on `:the_exception` object,
but actually there are several other ways to access to the exception object such as
`Base.current_exceptions` and manual catch of `rethrow`n object.
For example, escape analysis needs to account for potential escape of the allocated object
via `rethrow_escape!()` call in the example below:
```julia
const Gx = Ref{Any}()
@noinline function rethrow_escape!()
    try
        rethrow()
    catch err
        Gx[] = err
    end
end
unsafeget(x) = isassigned(x) ? x[] : throw(x)

code_escapes() do
    r = Ref{String}()
    try
        t = unsafeget(r)
    catch err
        t = typeof(err)  # `err` (which `r` may alias to) doesn't escape here
        rethrow_escape!() # `r` can escape here
    end
    return t
end
```

As indicated by the above example, it requires a global analysis in addition to a base escape
analysis to reason about all possible escapes via existing exception interfaces correctly.
For now we conservatively always propagate `AllEscape` to all potentially thrown objects,
since such an additional analysis might not be worthwhile to do given that exception handlings
and error paths usually don't need to be very performance sensitive, and optimizations of
error paths might be very ineffective anyway since they are sometimes "unoptimized"
intentionally for latency reasons.
"""
function escape_exception!(astate::AnalysisState, tryregions::Vector{UnitRange{Int}})
    estate = astate.estate
    # NOTE if `:the_exception` is the only way to access the exception, we can do:
    # exc = SSAValue(pc)
    # excinfo = estate[exc]
    excinfo = ⊤
    escapes = estate.escapes
    for i in 1:length(escapes)
        x = escapes[i]
        xt = x.ThrownEscape
        xt === TOP_THROWN_ESCAPE && @goto propagate_exception_escape # fast pass
        for pc in x.ThrownEscape
            for region in tryregions
                pc in region && @goto propagate_exception_escape # early break because of AllEscape
            end
        end
        continue
        @label propagate_exception_escape
        xval = irval(i, estate)
        add_escape_change!(astate, xval, excinfo)
    end
end

function escape_invoke!(astate::AnalysisState, pc::Int, args::Vector{Any})
    linfo = first(args)::MethodInstance
    cache = get(GLOBAL_ESCAPE_CACHE, linfo, nothing)
    if cache === nothing
        add_conservative_changes!(astate, pc, args, 2)
    else
        argescapes = argescapes_from_cache(cache)
        ret = SSAValue(pc)
        retinfo = astate.estate[ret] # escape information imposed on the call statement
        method = linfo.def::Method
        nargs = Int(method.nargs)
        for i in 2:length(args)
            arg = args[i]
            if i-1 ≤ nargs
                argi = i-1
            else # handle isva signature: COMBAK will this be invalid once we take alias information into account ?
                argi = nargs
            end
            arginfo = argescapes[argi]
            info = from_interprocedural(arginfo, retinfo, pc)
            if arginfo.ReturnEscape
                # if this argument can be "returned", in addition to propagating
                # the escape information imposed on this call argument within the callee,
                # we should also account for possible aliasing of this argument and the returned value
                add_escape_change!(astate, arg, info)
                add_alias_change!(astate, ret, arg)
            else
                # if this is simply passed as the call argument, we can just propagate
                # the escape information imposed on this call argument within the callee
                add_escape_change!(astate, arg, info)
            end
        end
    end
end

"""
    from_interprocedural(arginfo::ArgEscapeInfo, retinfo::EscapeInfo, pc::Int) -> x::EscapeInfo

Reinterprets the escape information imposed on the call argument which is cached as `arginfo`
in the context of the caller frame, where `retinfo` is the escape information imposed on
the return value and `pc` is the SSA statement number of the return value.
"""
function from_interprocedural(arginfo::ArgEscapeInfo, retinfo::EscapeInfo, pc::Int)
    arginfo.AllEscape && return ⊤

    ThrownEscape = arginfo.ThrownEscape ? LivenessSet(pc) : BOT_THROWN_ESCAPE

    return EscapeInfo(
        #=Analyzed=#true, #=ReturnEscape=#false, ThrownEscape,
        # FIXME implement interprocedural memory effect-analysis
        # currently, this essentially disables the entire field analysis
        # it might be okay from the SROA point of view, since we can't remove the allocation
        # as far as it's passed to a callee anyway, but still we may want some field analysis
        # for e.g. stack allocation or some other IPO optimizations
        #=AliasInfo=#TOP_ALIAS_INFO, #=Liveness=#LivenessSet(pc))
end

@noinline function unexpected_assignment!(ir::IRCode, pc::Int)
    @eval Main (ir = $ir; pc = $pc)
    error("unexpected assignment found: inspect `Main.pc` and `Main.pc`")
end

function escape_new!(astate::AnalysisState, pc::Int, args::Vector{Any})
    obj = SSAValue(pc)
    objinfo = astate.estate[obj]
    AliasInfo = objinfo.AliasInfo
    nargs = length(args)
    if isa(AliasInfo, Bool)
        @goto conservative_propagation
    elseif isa(AliasInfo, IndexableFields)
        # fields are known precisely: propagate escape information imposed on recorded possibilities to the exact field values
        infos = AliasInfo.infos
        nf = length(infos)
        objinfo = ignore_aliasinfo(objinfo)
        for i in 2:nargs
            i-1 > nf && break # may happen when e.g. ϕ-node merges values with different types
            arg = args[i]
            add_alias_escapes!(astate, arg, infos[i-1])
            push!(infos[i-1], -pc) # record def
            # propagate the escape information of this object ignoring field information
            add_escape_change!(astate, arg, objinfo)
            add_liveness_change!(astate, arg, pc)
        end
    elseif isa(AliasInfo, Unindexable) && !AliasInfo.array
        # fields are known partially: propagate escape information imposed on recorded possibilities to all fields values
        info = AliasInfo.info
        objinfo = ignore_aliasinfo(objinfo)
        for i in 2:nargs
            arg = args[i]
            add_alias_escapes!(astate, arg, info)
            push!(info, -pc) # record def
            # propagate the escape information of this object ignoring field information
            add_escape_change!(astate, arg, objinfo)
            add_liveness_change!(astate, arg, pc)
        end
    else
        # this object has been used as array, but it is allocated as struct here (i.e. should throw)
        # update obj's field information and just handle this case conservatively
        objinfo = escape_unanalyzable_obj!(astate, obj, objinfo)
        @label conservative_propagation
        # the fields couldn't be analyzed precisely: propagate the entire escape information
        # of this object to all its fields as the most conservative propagation
        for i in 2:nargs
            arg = args[i]
            add_escape_change!(astate, arg, objinfo)
            add_liveness_change!(astate, arg, pc)
        end
    end
    if !(getinst(astate.ir, pc)[:flag] & IR_FLAG_EFFECT_FREE ≠ 0)
        add_thrown_escapes!(astate, pc, args)
    end
end

function add_alias_escapes!(astate::AnalysisState, @nospecialize(v), ainfo::AInfo)
    estate = astate.estate
    for aidx in ainfo
        aidx < 0 && continue # ignore def
        x = SSAValue(aidx) # obviously this won't be true once we implement ArgEscape
        add_alias_change!(astate, v, x)
    end
end

function escape_unanalyzable_obj!(astate::AnalysisState, @nospecialize(obj), objinfo::EscapeInfo)
    objinfo = EscapeInfo(objinfo, TOP_ALIAS_INFO)
    add_escape_change!(astate, obj, objinfo)
    return objinfo
end

function add_thrown_escapes!(astate::AnalysisState, pc::Int, args::Vector{Any},
    first_idx::Int = 1, last_idx::Int = length(args))
    info = ThrownEscape(pc)
    for i in first_idx:last_idx
        add_escape_change!(astate, args[i], info)
    end
end

function add_liveness_changes!(astate::AnalysisState, pc::Int, args::Vector{Any},
    first_idx::Int = 1, last_idx::Int = length(args))
    for i in first_idx:last_idx
        arg = args[i]
        add_liveness_change!(astate, arg, pc)
    end
end

function add_fallback_changes!(astate::AnalysisState, pc::Int, args::Vector{Any},
    first_idx::Int = 1, last_idx::Int = length(args))
    info = ThrownEscape(pc)
    for i in first_idx:last_idx
        arg = args[i]
        add_escape_change!(astate, arg, info)
        add_liveness_change!(astate, arg, pc)
    end
end

function add_conservative_changes!(astate::AnalysisState, pc::Int, args::Vector{Any},
    first_idx::Int = 1, last_idx::Int = length(args))
    for i in first_idx:last_idx
        add_escape_change!(astate, args[i], ⊤)
    end
    add_escape_change!(astate, SSAValue(pc), ⊤) # it may return GlobalRef etc.
    return nothing
end

# escape every argument `(args[6:length(args[3])])` and the name `args[1]`
# TODO: we can apply a similar strategy like builtin calls to specialize some foreigncalls
function escape_foreigncall!(astate::AnalysisState, pc::Int, args::Vector{Any})
    nargs = length(args)
    if nargs < 6
        # invalid foreigncall, just escape everything
        add_conservative_changes!(astate, pc, args)
        return
    end
    argtypes = args[3]::SimpleVector
    nargs = length(argtypes)
    name = args[1]
    nn = normalize(name)
    if isa(nn, Symbol)
        boundserror_ninds = array_resize_info(nn)
        if boundserror_ninds !== nothing
            boundserror, ninds = boundserror_ninds
            escape_array_resize!(boundserror, ninds, astate, pc, args)
            return
        end
        if is_array_copy(nn)
            escape_array_copy!(astate, pc, args)
            return
        elseif is_array_isassigned(nn)
            escape_array_isassigned!(astate, pc, args)
            return
        end
        # if nn === :jl_gc_add_finalizer_th
        #     # TODO add `FinalizerEscape` ?
        # end
    end
    # NOTE array allocations might have been proven as nothrow (https://github.com/JuliaLang/julia/pull/43565)
    nothrow = astate.ir.stmts[pc][:flag] & IR_FLAG_EFFECT_FREE ≠ 0
    name_info = nothrow ? ⊥ : ThrownEscape(pc)
    add_escape_change!(astate, name, name_info)
    add_liveness_change!(astate, name, pc)
    for i = 1:nargs
        # we should escape this argument if it is directly called,
        # otherwise just impose ThrownEscape if not nothrow
        if argtypes[i] === Any
            arg_info = ⊤
        else
            arg_info = nothrow ? ⊥ : ThrownEscape(pc)
        end
        add_escape_change!(astate, args[5+i], arg_info)
        add_liveness_change!(astate, args[5+i], pc)
    end
    for i = (5+nargs):length(args)
        arg = args[i]
        add_escape_change!(astate, arg, ⊥)
        add_liveness_change!(astate, arg, pc)
    end
end

normalize(@nospecialize x) = isa(x, QuoteNode) ? x.value : x

function escape_call!(astate::AnalysisState, pc::Int, args::Vector{Any})
    ir = astate.ir
    ft = argextype(first(args), ir, ir.sptypes, ir.argtypes)
    f = singleton_type(ft)
    if isa(f, Core.IntrinsicFunction)
        # XXX somehow `:call` expression can creep in here, ideally we should be able to do:
        # argtypes = Any[argextype(args[i], astate.ir) for i = 2:length(args)]
        argtypes = Any[]
        for i = 2:length(args)
            arg = args[i]
            push!(argtypes, isexpr(arg, :call) ? Any : argextype(arg, ir))
        end
        if intrinsic_nothrow(f, argtypes)
            add_liveness_changes!(astate, pc, args, 2)
        else
            add_fallback_changes!(astate, pc, args, 2)
        end
        return # TODO accounts for pointer operations?
    end
    result = escape_builtin!(f, astate, pc, args)
    if result === missing
        # if this call hasn't been handled by any of pre-defined handlers,
        # we escape this call conservatively
        add_conservative_changes!(astate, pc, args)
        return
    elseif result === true
        add_liveness_changes!(astate, pc, args, 2)
        return # ThrownEscape is already checked
    else
        # we escape statements with the `ThrownEscape` property using the effect-freeness
        # computed by `stmt_effect_free` invoked within inlining
        # TODO throwness ≠ "effect-free-ness"
        if getinst(astate.ir, pc)[:flag] & IR_FLAG_EFFECT_FREE ≠ 0
            add_liveness_changes!(astate, pc, args, 2)
        else
            add_fallback_changes!(astate, pc, args, 2)
        end
        return
    end
end

escape_builtin!(@nospecialize(f), _...) = return missing

# safe builtins
escape_builtin!(::typeof(isa), _...) = return false
escape_builtin!(::typeof(typeof), _...) = return false
escape_builtin!(::typeof(sizeof), _...) = return false
escape_builtin!(::typeof(===), _...) = return false
# not really safe, but `ThrownEscape` will be imposed later
escape_builtin!(::typeof(isdefined), _...) = return false
escape_builtin!(::typeof(throw), _...) = return false

function escape_builtin!(::typeof(ifelse), astate::AnalysisState, pc::Int, args::Vector{Any})
    length(args) == 4 || return false
    f, cond, th, el = args
    ret = SSAValue(pc)
    condt = argextype(cond, astate.ir)
    if isa(condt, Const) && (cond = condt.val; isa(cond, Bool))
        if cond
            add_alias_change!(astate, th, ret)
        else
            add_alias_change!(astate, el, ret)
        end
    else
        add_alias_change!(astate, th, ret)
        add_alias_change!(astate, el, ret)
    end
    return false
end

function escape_builtin!(::typeof(typeassert), astate::AnalysisState, pc::Int, args::Vector{Any})
    length(args) == 3 || return false
    f, obj, typ = args
    ret = SSAValue(pc)
    add_alias_change!(astate, ret, obj)
    return false
end

function escape_builtin!(::typeof(tuple), astate::AnalysisState, pc::Int, args::Vector{Any})
    escape_new!(astate, pc, args)
    return false
end

function analyze_fields(ir::IRCode, @nospecialize(typ), @nospecialize(fld))
    nfields = fieldcount_noerror(typ)
    if nfields === nothing
        return Unindexable(false, AInfo()), 0
    end
    if isa(typ, DataType)
        fldval = try_compute_field(ir, fld)
        fidx = try_compute_fieldidx(typ, fldval)
    else
        fidx = nothing
    end
    if fidx === nothing
        return Unindexable(false, AInfo()), 0
    end
    return IndexableFields(AInfo[AInfo() for _ in 1:nfields]), fidx
end

function reanalyze_fields(ir::IRCode, AliasInfo::IndexableFields, @nospecialize(typ), @nospecialize(fld))
    nfields = fieldcount_noerror(typ)
    if nfields === nothing
        return merge_to_unindexable(AliasInfo), 0
    end
    if isa(typ, DataType)
        fldval = try_compute_field(ir, fld)
        fidx = try_compute_fieldidx(typ, fldval)
    else
        fidx = nothing
    end
    if fidx === nothing
        return merge_to_unindexable(AliasInfo), 0
    end
    infos = AliasInfo.infos
    ninfos = length(infos)
    if nfields > ninfos
        for _ in 1:(nfields-ninfos)
            push!(infos, AInfo())
        end
    end
    return AliasInfo, fidx
end

function escape_builtin!(::typeof(getfield), astate::AnalysisState, pc::Int, args::Vector{Any})
    length(args) ≥ 3 || return false
    ir, estate = astate.ir, astate.estate
    obj = args[2]
    typ = widenconst(argextype(obj, ir))
    if hasintersect(typ, Module) # global load
        add_escape_change!(astate, SSAValue(pc), ⊤)
    end
    if isa(obj, SSAValue) || isa(obj, Argument)
        objinfo = estate[obj]
    else
        return false
    end
    AliasInfo = objinfo.AliasInfo
    if isa(AliasInfo, Bool)
        AliasInfo && @goto conservative_propagation
        # the fields of this object haven't been analyzed yet: analyze them now
        AliasInfo, fidx = analyze_fields(ir, typ, args[3])
        if isa(AliasInfo, IndexableFields)
            @goto record_indexable_use
        else
            @goto record_unindexable_use
        end
    elseif isa(AliasInfo, IndexableFields)
        AliasInfo, fidx = reanalyze_fields(ir, AliasInfo, typ, args[3])
        isa(AliasInfo, Unindexable) && @goto record_unindexable_use
        @label record_indexable_use
        push!(AliasInfo.infos[fidx], pc) # record use
        objinfo = EscapeInfo(objinfo, AliasInfo)
        add_escape_change!(astate, obj, objinfo)
    elseif isa(AliasInfo, Unindexable) && !AliasInfo.array
        @label record_unindexable_use
        push!(AliasInfo.info, pc) # record use
        objinfo = EscapeInfo(objinfo, AliasInfo)
        add_escape_change!(astate, obj, objinfo)
    else
        # this object has been used as array, but it is used as struct here (i.e. should throw)
        # update obj's field information and just handle this case conservatively
        objinfo = escape_unanalyzable_obj!(astate, obj, objinfo)
        @label conservative_propagation
        # the field couldn't be analyzed precisely: propagate the escape information
        # imposed on the return value of this `getfield` call to the object itself
        # as the most conservative propagation
        ssainfo = estate[SSAValue(pc)]
        add_escape_change!(astate, obj, ssainfo)
    end
    return false
end

function escape_builtin!(::typeof(setfield!), astate::AnalysisState, pc::Int, args::Vector{Any})
    length(args) ≥ 4 || return false
    ir, estate = astate.ir, astate.estate
    obj = args[2]
    val = args[4]
    if isa(obj, SSAValue) || isa(obj, Argument)
        objinfo = estate[obj]
    else
        # unanalyzable object (e.g. obj::GlobalRef): escape field value conservatively
        add_escape_change!(astate, val, ⊤)
        @goto add_thrown_escapes
    end
    AliasInfo = objinfo.AliasInfo
    if isa(AliasInfo, Bool)
        AliasInfo && @goto conservative_propagation
        # the fields of this object haven't been analyzed yet: analyze them now
        typ = widenconst(argextype(obj, ir))
        AliasInfo, fidx = analyze_fields(ir, typ, args[3])
        if isa(AliasInfo, IndexableFields)
            @goto escape_indexable_def
        else
            @goto escape_unindexable_def
        end
    elseif isa(AliasInfo, IndexableFields)
        typ = widenconst(argextype(obj, ir))
        AliasInfo, fidx = reanalyze_fields(ir, AliasInfo, typ, args[3])
        isa(AliasInfo, Unindexable) && @goto escape_unindexable_def
        @label escape_indexable_def
        add_alias_escapes!(astate, val, AliasInfo.infos[fidx])
        push!(AliasInfo.infos[fidx], -pc) # record def
        objinfo = EscapeInfo(objinfo, AliasInfo)
        add_escape_change!(astate, obj, objinfo)
        # propagate the escape information of this object ignoring field information
        add_escape_change!(astate, val, ignore_aliasinfo(objinfo))
    elseif isa(AliasInfo, Unindexable) && !AliasInfo.array
        info = AliasInfo.info
        @label escape_unindexable_def
        add_alias_escapes!(astate, val, AliasInfo.info)
        push!(AliasInfo.info, -pc) # record def
        objinfo = EscapeInfo(objinfo, AliasInfo)
        add_escape_change!(astate, obj, objinfo)
        # propagate the escape information of this object ignoring field information
        add_escape_change!(astate, val, ignore_aliasinfo(objinfo))
    else
        # this object has been used as array, but it is used as struct here (i.e. should throw)
        # update obj's field information and just handle this case conservatively
        objinfo = escape_unanalyzable_obj!(astate, obj, objinfo)
        @label conservative_propagation
        # the field couldn't be analyzed: propagate the entire escape information
        # of this object to the value being assigned as the most conservative propagation
        add_escape_change!(astate, val, objinfo)
    end
    # also propagate escape information imposed on the return value of this `setfield!`
    ssainfo = estate[SSAValue(pc)]
    add_escape_change!(astate, val, ssainfo)
    # compute the throwness of this setfield! call here since builtin_nothrow doesn't account for that
    @label add_thrown_escapes
    argtypes = Any[]
    for i = 2:length(args)
        push!(argtypes, argextype(args[i], ir))
    end
    setfield!_nothrow(argtypes) || add_thrown_escapes!(astate, pc, args, 2)
    return true
end

function escape_builtin!(::typeof(arrayref), astate::AnalysisState, pc::Int, args::Vector{Any})
    length(args) ≥ 4 || return false
    # check potential thrown escapes from this arrayref call
    argtypes = Any[argextype(args[i], astate.ir) for i in 2:length(args)]
    boundcheckt = argtypes[1]
    aryt = argtypes[2]
    if !array_builtin_common_typecheck(boundcheckt, aryt, argtypes, 3)
        add_thrown_escapes!(astate, pc, args, 2)
    end
    ary = args[3]
    inbounds = isa(boundcheckt, Const) && !boundcheckt.val::Bool
    inbounds || add_escape_change!(astate, ary, ThrownEscape(pc))
    # we don't track precise index information about this array and thus don't know what values
    # can be referenced here: directly propagate the escape information imposed on the return
    # value of this `arrayref` call to the array itself as the most conservative propagation
    # but also with updated index information
    # TODO enable index analysis when constant values are available?
    estate = astate.estate
    if isa(ary, SSAValue) || isa(ary, Argument)
        aryinfo = estate[ary]
    else
        return true
    end
    AliasInfo = aryinfo.AliasInfo
    if isa(AliasInfo, Bool)
        AliasInfo && @goto conservative_propagation
        # the elements of this array haven't been analyzed yet: set AliasInfo now
        idx = array_nd_index(astate, ary, args[4:end])
        if isa(idx, Int)
            AliasInfo = IndexableElements(IdDict{Int,AInfo}())
            @goto record_indexable_use
        end
        AliasInfo = Unindexable(true, AInfo())
        @goto record_unindexable_use
    elseif isa(AliasInfo, IndexableElements)
        idx = array_nd_index(astate, ary, args[4:end])
        if !isa(idx, Int)
            AliasInfo = merge_to_unindexable(AliasInfo)
            @goto record_unindexable_use
        end
        @label record_indexable_use
        info = get!(()->AInfo(), AliasInfo.infos, idx)
        push!(info, pc) # record use
        add_escape_change!(astate, ary, EscapeInfo(aryinfo, AliasInfo)) # update with new AliasInfo
    elseif isa(AliasInfo, Unindexable) && AliasInfo.array
        @label record_unindexable_use
        push!(AliasInfo.info, pc) # record use
        add_escape_change!(astate, ary, EscapeInfo(aryinfo, AliasInfo)) # update with new AliasInfo
    else
        # this object has been used as struct, but it is used as array here (thus should throw)
        # update ary's element information and just handle this case conservatively
        aryinfo = escape_unanalyzable_obj!(astate, ary, aryinfo)
        @label conservative_propagation
        ssainfo = estate[SSAValue(pc)]
        add_escape_change!(astate, ary, ssainfo)
    end
    return true
end

function escape_builtin!(::typeof(arrayset), astate::AnalysisState, pc::Int, args::Vector{Any})
    length(args) ≥ 5 || return false
    # check potential escapes from this arrayset call
    # NOTE here we essentially only need to account for TypeError, assuming that
    # UndefRefError or BoundsError don't capture any of the arguments here
    argtypes = Any[argextype(args[i], astate.ir) for i in 2:length(args)]
    boundcheckt = argtypes[1]
    aryt = argtypes[2]
    valt = argtypes[3]
    if !(array_builtin_common_typecheck(boundcheckt, aryt, argtypes, 4) &&
         arrayset_typecheck(aryt, valt))
        add_thrown_escapes!(astate, pc, args, 2)
    end
    ary = args[3]
    val = args[4]
    inbounds = isa(boundcheckt, Const) && !boundcheckt.val::Bool
    inbounds || add_escape_change!(astate, ary, ThrownEscape(pc))
    # we don't track precise index information about this array and won't record what value
    # is being assigned here: directly propagate the escape information of this array to
    # the value being assigned as the most conservative propagation
    # TODO enable index analysis when constant values are available?
    estate = astate.estate
    if isa(ary, SSAValue) || isa(ary, Argument)
        aryinfo = estate[ary]
    else
        # unanalyzable object (e.g. obj::GlobalRef): escape field value conservatively
        add_escape_change!(astate, val, ⊤)
        return true
    end
    AliasInfo = aryinfo.AliasInfo
    if isa(AliasInfo, Bool)
        AliasInfo && @goto conservative_propagation
        # the elements of this array haven't been analyzed yet: set AliasInfo now
        idx = array_nd_index(astate, ary, args[5:end])
        if isa(idx, Int)
            AliasInfo = IndexableElements(IdDict{Int,AInfo}())
            @goto escape_indexable_def
        end
        AliasInfo = Unindexable(true, AInfo())
        @goto escape_unindexable_def
    elseif isa(AliasInfo, IndexableElements)
        idx = array_nd_index(astate, ary, args[5:end])
        if !isa(idx, Int)
            AliasInfo = merge_to_unindexable(AliasInfo)
            @goto escape_unindexable_def
        end
        @label escape_indexable_def
        info = get!(()->AInfo(), AliasInfo.infos, idx)
        add_alias_escapes!(astate, val, info)
        push!(info, -pc) # record def
        add_escape_change!(astate, ary, EscapeInfo(aryinfo, AliasInfo)) # update with new AliasInfo
        # propagate the escape information of this array ignoring elements information
        add_escape_change!(astate, val, ignore_aliasinfo(aryinfo))
    elseif isa(AliasInfo, Unindexable) && AliasInfo.array
        @label escape_unindexable_def
        add_alias_escapes!(astate, val, AliasInfo.info)
        push!(AliasInfo.info, -pc) # record def
        add_escape_change!(astate, ary, EscapeInfo(aryinfo, AliasInfo)) # update with new AliasInfo
        # propagate the escape information of this array ignoring elements information
        add_escape_change!(astate, val, ignore_aliasinfo(aryinfo))
    else
        # this object has been used as struct, but it is used as array here (thus should throw)
        # update ary's element information and just handle this case conservatively
        aryinfo = escape_unanalyzable_obj!(astate, ary, aryinfo)
        @label conservative_propagation
        add_escape_change!(astate, val, aryinfo)
    end
    # also propagate escape information imposed on the return value of this `arrayset`
    ssainfo = estate[SSAValue(pc)]
    add_escape_change!(astate, ary, ssainfo)
    return true
end

# NOTE this function models and thus should be synced with the implementation of:
# size_t array_nd_index(jl_array_t *a, jl_value_t **args, size_t nidxs, ...)
function array_nd_index(astate::AnalysisState, @nospecialize(ary), args::Vector{Any}, nidxs::Int = length(args))
    isa(ary, SSAValue) || return nothing
    aryid = ary.id
    arrayinfo = astate.estate.arrayinfo
    isa(arrayinfo, ArrayInfo) || return nothing
    haskey(arrayinfo, aryid) || return nothing
    dims = arrayinfo[aryid]
    local i = 0
    local k, stride = 0, 1
    local nd = length(dims)
    while k < nidxs
        arg = args[k+1]
        argval = argextype(arg, astate.ir)
        isa(argval, Const) || return nothing
        argval = argval.val
        isa(argval, Int) || return nothing
        ii = argval - 1
        i += ii * stride
        d = k ≥ nd ? 1 : dims[k+1]
        k < nidxs - 1 && ii ≥ d && return nothing # BoundsError
        stride *= d
        k += 1
    end
    while k < nd
        stride *= dims[k+1]
        k += 1
    end
    i ≥ stride && return nothing # BoundsError
    return i
end

function escape_builtin!(::typeof(arraysize), astate::AnalysisState, pc::Int, args::Vector{Any})
    length(args) == 3 || return false
    ary = args[2]
    dim = args[3]
    if !arraysize_typecheck(ary, dim, astate.ir)
        add_escape_change!(astate, ary, ThrownEscape(pc))
        add_escape_change!(astate, dim, ThrownEscape(pc))
    end
    # NOTE we may still see "arraysize: dimension out of range", but it doesn't capture anything
    return true
end

function arraysize_typecheck(@nospecialize(ary), @nospecialize(dim), ir::IRCode)
    aryt = argextype(ary, ir)
    aryt ⊑ₜ Array || return false
    dimt = argextype(dim, ir)
    dimt ⊑ₜ Int || return false
    return true
end

# returns nothing if this isn't array resizing operation,
# otherwise returns true if it can throw BoundsError and false if not
function array_resize_info(name::Symbol)
    if name === :jl_array_grow_beg || name === :jl_array_grow_end
        return false, 1
    elseif name === :jl_array_del_beg || name === :jl_array_del_end
        return true, 1
    elseif name === :jl_array_grow_at || name === :jl_array_del_at
        return true, 2
    else
        return nothing
    end
end

# NOTE may potentially throw "cannot resize array with shared data" error,
# but just ignore it since it doesn't capture anything
function escape_array_resize!(boundserror::Bool, ninds::Int,
    astate::AnalysisState, pc::Int, args::Vector{Any})
    length(args) ≥ 6+ninds || return add_fallback_changes!(astate, pc, args)
    ary = args[6]
    aryt = argextype(ary, astate.ir)
    aryt ⊑ₜ Array || return add_fallback_changes!(astate, pc, args)
    for i in 1:ninds
        ind = args[i+6]
        indt = argextype(ind, astate.ir)
        indt ⊑ₜ Integer || return add_fallback_changes!(astate, pc, args)
    end
    if boundserror
        # this array resizing can potentially throw `BoundsError`, impose it now
        add_escape_change!(astate, ary, ThrownEscape(pc))
    end
    # give up indexing analysis whenever we see array resizing
    # (since we track array dimensions only globally)
    mark_unindexable!(astate, ary)
    add_liveness_changes!(astate, pc, args, 6)
end

function mark_unindexable!(astate::AnalysisState, @nospecialize(ary))
    isa(ary, SSAValue) || return
    aryinfo = astate.estate[ary]
    AliasInfo = aryinfo.AliasInfo
    isa(AliasInfo, IndexableElements) || return
    AliasInfo = merge_to_unindexable(AliasInfo)
    add_escape_change!(astate, ary, EscapeInfo(aryinfo, AliasInfo))
end

is_array_copy(name::Symbol) = name === :jl_array_copy

# FIXME this implementation is very conservative, improve the accuracy and solve broken test cases
function escape_array_copy!(astate::AnalysisState, pc::Int, args::Vector{Any})
    length(args) ≥ 6 || return add_fallback_changes!(astate, pc, args)
    ary = args[6]
    aryt = argextype(ary, astate.ir)
    aryt ⊑ₜ Array || return add_fallback_changes!(astate, pc, args)
    if isa(ary, SSAValue) || isa(ary, Argument)
        newary = SSAValue(pc)
        aryinfo = astate.estate[ary]
        newaryinfo = astate.estate[newary]
        add_escape_change!(astate, newary, aryinfo)
        add_escape_change!(astate, ary, newaryinfo)
    end
    add_liveness_changes!(astate, pc, args, 6)
end

is_array_isassigned(name::Symbol) = name === :jl_array_isassigned

function escape_array_isassigned!(astate::AnalysisState, pc::Int, args::Vector{Any})
    if !array_isassigned_nothrow(args, astate.ir)
        add_thrown_escapes!(astate, pc, args)
    end
    add_liveness_changes!(astate, pc, args, 6)
end

function array_isassigned_nothrow(args::Vector{Any}, src::IRCode)
    # if !validate_foreigncall_args(args,
    #     :jl_array_isassigned, Cint, svec(Any,Csize_t), 0, :ccall)
    #     return false
    # end
    length(args) ≥ 7 || return false
    arytype = argextype(args[6], src)
    arytype ⊑ₜ Array || return false
    idxtype = argextype(args[7], src)
    idxtype ⊑ₜ Csize_t || return false
    return true
end

# # COMBAK do we want to enable this (and also backport this to Base for array allocations?)
# import Core.Compiler: Cint, svec
# function validate_foreigncall_args(args::Vector{Any},
#     name::Symbol, @nospecialize(rt), argtypes::SimpleVector, nreq::Int, convension::Symbol)
#     length(args) ≥ 5 || return false
#     normalize(args[1]) === name || return false
#     args[2] === rt || return false
#     args[3] === argtypes || return false
#     args[4] === vararg || return false
#     normalize(args[5]) === convension || return false
#     return true
# end

if isdefined(Core, :ImmutableArray)

import Core: ImmutableArray, arrayfreeze, mutating_arrayfreeze, arraythaw

escape_builtin!(::typeof(arrayfreeze), astate::AnalysisState, pc::Int, args::Vector{Any}) =
    is_safe_immutable_array_op(Array, astate, args)
escape_builtin!(::typeof(mutating_arrayfreeze), astate::AnalysisState, pc::Int, args::Vector{Any}) =
    is_safe_immutable_array_op(Array, astate, args)
escape_builtin!(::typeof(arraythaw), astate::AnalysisState, pc::Int, args::Vector{Any}) =
    is_safe_immutable_array_op(ImmutableArray, astate, args)
function is_safe_immutable_array_op(@nospecialize(arytype), astate::AnalysisState, args::Vector{Any})
    length(args) == 2 || return false
    argextype(args[2], astate.ir) ⊑ₜ arytype || return false
    return true
end

end # if isdefined(Core, :ImmutableArray)

# NOTE define fancy package utilities when developing EA as an external package
if _TOP_MOD !== Core.Compiler
    include(@__MODULE__, "EAUtils.jl")
    using .EAUtils: code_escapes, @code_escapes
    export code_escapes, @code_escapes
end

end # baremodule EscapeAnalysis

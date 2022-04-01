baremodule EscapeAnalysis

export
    analyze_escapes,
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
    ReturnNode, GotoNode, GotoIfNot, SimpleVector, MethodMatch, CodeInstance,
    sizeof, ifelse, arrayset, arrayref, arraysize
import ._TOP_MOD:     # Base definitions
    @__MODULE__, @eval, @assert, @specialize, @nospecialize, @inbounds, @inline, @noinline,
    @label, @goto, !, !==, !=, ≠, +, -, *, ≤, <, ≥, >, &, |, <<, error, missing, copy,
    Vector, BitSet, IdDict, IdSet, UnitRange, Csize_t, Callable, ∪, ⊆, ∩, :, ∈, ∉, =>,
    in, length, get, first, last, haskey, keys, get!, isempty, isassigned,
    pop!, push!, pushfirst!, empty!, delete!, max, min, enumerate, unwrap_unionall,
    ismutabletype
import Core.Compiler: # Core.Compiler specific definitions
    Bottom, InferenceResult, IRCode, IR_FLAG_EFFECT_FREE,
    isbitstype, isexpr, is_meta_expr_head, println, widenconst, argextype, singleton_type,
    fieldcount_noerror, try_compute_field, try_compute_fieldidx, hasintersect, ⊑,
    intrinsic_nothrow, array_builtin_common_typecheck, arrayset_typecheck,
    setfield!_nothrow, alloc_array_ndims, stmt_effect_free, check_effect_free!

include(x) = _TOP_MOD.include(@__MODULE__, x)
if _TOP_MOD === Core.Compiler
    include("compiler/ssair/EscapeAnalysis/disjoint_set.jl")
else
    include("disjoint_set.jl")
end

const AInfo = IdSet{Any}
const LivenessSet = BitSet

"""
    x::EscapeInfo

A lattice for escape information, which holds the following properties:
- `x.Analyzed::Bool`: not formally part of the lattice, only indicates `x` has not been analyzed or not
- `x.ReturnEscape::Bool`: indicates `x` can escape to the caller via return
- `x.ThrownEscape::BitSet`: records SSA statement numbers where `x` can be thrown as exception:
  * `isempty(x.ThrownEscape)`: `x` will never be thrown in this call frame (the bottom)
  * `pc ∈ x.ThrownEscape`: `x` may be thrown at the SSA statement at `pc`
  * `-1 ∈ x.ThrownEscape`: `x` may be thrown at arbitrary points of this call frame (the top)
  This information will be used by `escape_exception!` to propagate potential escapes via exception.
- `x.AliasInfo::Union{Bool,IndexableFields,IndexableElements,Unindexable}`: maintains all possible values
  that can be aliased to fields or array elements of `x`:
  * `x.AliasInfo === false` indicates the fields/elements of `x` aren't analyzed yet
  * `x.AliasInfo === true` indicates the fields/elements of `x` can't be analyzed,
    e.g. the type of `x` is not known or is not concrete and thus its fields/elements
    can't be known precisely
  * `x.AliasInfo::IndexableFields` records all the possible values that can be aliased to fields of object `x` with precise index information
  * `x.AliasInfo::IndexableElements` records all the possible values that can be aliased to elements of array `x` with precise index information
  * `x.AliasInfo::Unindexable` records all the possible values that can be aliased to fields/elements of `x` without precise index information
- `x.Liveness::BitSet`: records SSA statement numbers where `x` should be live, e.g.
  to be used as a call argument, to be returned to a caller, or preserved for `:foreigncall`:
  * `isempty(x.Liveness)`: `x` is never be used in this call frame (the bottom)
  * `0 ∈ x.Liveness` also has the special meaning that it's a call argument of the currently
    analyzed call frame (and thus it's visible from the caller immediately).
  * `pc ∈ x.Liveness`: `x` may be used at the SSA statement at `pc`
  * `-1 ∈ x.Liveness`: `x` may be used at arbitrary points of this call frame (the top)

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
# NOTE the lattice operations should try to avoid actual set computations on this top value,
# and e.g. LivenessSet(0:1000000) should also work without incurring excessive computations
const TOP_THROWN_ESCAPE = LivenessSet(-1)

const BOT_LIVENESS = LivenessSet()
# NOTE the lattice operations should try to avoid actual set computations on this top value,
# and e.g. LivenessSet(0:1000000) should also work without incurring excessive computations
const TOP_LIVENESS = LivenessSet(-1:0)
const ARG_LIVENESS = LivenessSet(0)

# the constructors
NotAnalyzed() = EscapeInfo(false, false, BOT_THROWN_ESCAPE, false, BOT_LIVENESS) # not formally part of the lattice
NoEscape() = EscapeInfo(true, false, BOT_THROWN_ESCAPE, false, BOT_LIVENESS)
ArgEscape() = EscapeInfo(true, false, BOT_THROWN_ESCAPE, true, ARG_LIVENESS)
ReturnEscape(pc::Int) = EscapeInfo(true, true, BOT_THROWN_ESCAPE, false, LivenessSet(pc))
AllReturnEscape() = EscapeInfo(true, true, BOT_THROWN_ESCAPE, false, TOP_LIVENESS)
ThrownEscape(pc::Int) = EscapeInfo(true, false, LivenessSet(pc), false, BOT_LIVENESS)
AllEscape() = EscapeInfo(true, true, TOP_THROWN_ESCAPE, true, TOP_LIVENESS)

const ⊥, ⊤ = NotAnalyzed(), AllEscape()

# Convenience names for some ⊑ₑ queries
has_no_escape(x::EscapeInfo) = !x.ReturnEscape && isempty(x.ThrownEscape) && 0 ∉ x.Liveness
has_arg_escape(x::EscapeInfo) = 0 in x.Liveness
has_return_escape(x::EscapeInfo) = x.ReturnEscape
has_return_escape(x::EscapeInfo, pc::Int) = x.ReturnEscape && (-1 ∈ x.Liveness || pc in x.Liveness)
has_thrown_escape(x::EscapeInfo) = !isempty(x.ThrownEscape)
has_thrown_escape(x::EscapeInfo, pc::Int) = -1 ∈ x.ThrownEscape  || pc in x.ThrownEscape
has_all_escape(x::EscapeInfo) = ⊤ ⊑ₑ x

# utility lattice constructors
ignore_argescape(x::EscapeInfo) = EscapeInfo(x; Liveness=delete!(copy(x.Liveness), 0))
ignore_thrownescapes(x::EscapeInfo) = EscapeInfo(x; ThrownEscape=BOT_THROWN_ESCAPE)
ignore_aliasinfo(x::EscapeInfo) = EscapeInfo(x, false)
ignore_liveness(x::EscapeInfo) = EscapeInfo(x; Liveness=BOT_LIVENESS)

# AliasInfo
struct IndexableFields
    infos::Vector{AInfo}
end
struct IndexableElements
    infos::IdDict{Int,AInfo}
end
struct Unindexable
    info::AInfo
end
IndexableFields(nflds::Int) = IndexableFields(AInfo[AInfo() for _ in 1:nflds])
Unindexable() = Unindexable(AInfo())

merge_to_unindexable(AliasInfo::IndexableFields) = Unindexable(merge_to_unindexable(AliasInfo.infos))
merge_to_unindexable(AliasInfo::Unindexable, AliasInfos::IndexableFields) = Unindexable(merge_to_unindexable(AliasInfo.info, AliasInfos.infos))
merge_to_unindexable(infos::Vector{AInfo}) = merge_to_unindexable(AInfo(), infos)
function merge_to_unindexable(info::AInfo, infos::Vector{AInfo})
    for i = 1:length(infos)
        info = info ∪ infos[i]
    end
    return info
end
merge_to_unindexable(AliasInfo::IndexableElements) = Unindexable(merge_to_unindexable(AliasInfo.infos))
merge_to_unindexable(AliasInfo::Unindexable, AliasInfos::IndexableElements) = Unindexable(merge_to_unindexable(AliasInfo.info, AliasInfos.infos))
merge_to_unindexable(infos::IdDict{Int,AInfo}) = merge_to_unindexable(AInfo(), infos)
function merge_to_unindexable(info::AInfo, infos::IdDict{Int,AInfo})
    for idx in keys(infos)
        info = info ∪ infos[idx]
    end
    return info
end

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
    x::EscapeInfo ⊑ₑ y::EscapeInfo -> Bool

The non-strict partial order over [`EscapeInfo`](@ref).
"""
x::EscapeInfo ⊑ₑ y::EscapeInfo = begin
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
    x::EscapeInfo ⊏ₑ y::EscapeInfo -> Bool

The strict partial order over [`EscapeInfo`](@ref).
This is defined as the irreflexive kernel of `⊏ₑ`.
"""
x::EscapeInfo ⊏ₑ y::EscapeInfo = x ⊑ₑ y && !(y ⊑ₑ x)

"""
    x::EscapeInfo ⋤ₑ y::EscapeInfo -> Bool

This order could be used as a slightly more efficient version of the strict order `⊏ₑ`,
where we can safely assume `x ⊑ₑ y` holds.
"""
x::EscapeInfo ⋤ₑ y::EscapeInfo = !(y ⊑ₑ x)

"""
    x::EscapeInfo ⊔ₑ y::EscapeInfo -> EscapeInfo

Computes the join of `x` and `y` in the partial order defined by [`EscapeInfo`](@ref).
"""
x::EscapeInfo ⊔ₑ y::EscapeInfo = begin
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
    AliasInfo = merge_alias_info(x.AliasInfo, y.AliasInfo)
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

function merge_alias_info(@nospecialize(xa), @nospecialize(ya))
    if xa === true || ya === true
        return true
    elseif xa === false
        return ya
    elseif ya === false
        return xa
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
            return IndexableFields(infos)
        elseif isa(ya, Unindexable)
            xinfos, yinfo = xa.infos, ya.info
            return merge_to_unindexable(ya, xa)
        else
            return true # handle conflicting case conservatively
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
            return IndexableElements(infos)
        elseif isa(ya, Unindexable)
            return merge_to_unindexable(ya, xa)
        else
            return true # handle conflicting case conservatively
        end
    else
        xa = xa::Unindexable
        if isa(ya, IndexableFields)
            return merge_to_unindexable(xa, ya)
        elseif isa(ya, IndexableElements)
            return merge_to_unindexable(xa, ya)
        else
            ya = ya::Unindexable
            xinfo, yinfo = xa.info, ya.info
            info = xinfo ∪ yinfo
            return Unindexable(info)
        end
    end
end

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
    arrayinfo::Union{Nothing,ArrayInfo}
end
function EscapeState(nargs::Int, nstmts::Int, arrayinfo::Union{Nothing,ArrayInfo})
    escapes = EscapeInfo[
        1 ≤ i ≤ nargs ? ArgEscape() : ⊥ for i in 1:(nargs+nstmts)]
    aliasset = AliasSet(nargs+nstmts)
    return EscapeState(escapes, aliasset, nargs, arrayinfo)
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

struct ArgEscapeInfo
    EscapeBits::UInt8
end
function ArgEscapeInfo(x::EscapeInfo)
    x === ⊤ && return ArgEscapeInfo(ARG_ALL_ESCAPE)
    EscapeBits = 0x00
    has_return_escape(x) && (EscapeBits |= ARG_RETURN_ESCAPE)
    has_thrown_escape(x) && (EscapeBits |= ARG_THROWN_ESCAPE)
    return ArgEscapeInfo(EscapeBits)
end

const ARG_ALL_ESCAPE    = 0x01 << 0
const ARG_RETURN_ESCAPE = 0x01 << 1
const ARG_THROWN_ESCAPE = 0x01 << 2

has_no_escape(x::ArgEscapeInfo)     = !has_all_escape(x) && !has_return_escape(x) && !has_thrown_escape(x)
has_all_escape(x::ArgEscapeInfo)    = x.EscapeBits & ARG_ALL_ESCAPE    ≠ 0
has_return_escape(x::ArgEscapeInfo) = x.EscapeBits & ARG_RETURN_ESCAPE ≠ 0
has_thrown_escape(x::ArgEscapeInfo) = x.EscapeBits & ARG_THROWN_ESCAPE ≠ 0

struct ArgAliasing
    aidx::Int
    bidx::Int
end

struct ArgEscapeCache
    argescapes::Vector{ArgEscapeInfo}
    argaliases::Vector{ArgAliasing}
end

function ArgEscapeCache(estate::EscapeState)
    nargs = estate.nargs
    argescapes = Vector{ArgEscapeInfo}(undef, nargs)
    argaliases = ArgAliasing[]
    for i = 1:nargs
        info = estate.escapes[i]
        @assert info.AliasInfo === true
        argescapes[i] = ArgEscapeInfo(info)
        for j = (i+1):nargs
            if isaliased(i, j, estate)
                push!(argaliases, ArgAliasing(i, j))
            end
        end
    end
    return ArgEscapeCache(argescapes, argaliases)
end

"""
    is_ipo_profitable(ir::IRCode, nargs::Int) -> Bool

Heuristically checks if there is any profitability to run the escape analysis on `ir`
and generate IPO escape information cache. Specifically, this function examines
if any call argument is "interesting" in terms of their escapability.
"""
function is_ipo_profitable(ir::IRCode, nargs::Int)
    for i = 1:nargs
        t = unwrap_unionall(widenconst(ir.argtypes[i]))
        t <: IO && return false # bail out IO-related functions
        is_ipo_profitable_type(t) && return true
    end
    return false
end
function is_ipo_profitable_type(@nospecialize t)
    if isa(t, Union)
        return is_ipo_profitable_type(t.a) && is_ipo_profitable_type(t.b)
    end
    (t === String || t === Symbol || t === Module || t === SimpleVector) && return false
    return ismutabletype(t)
end

abstract type Change end
struct EscapeChange <: Change
    xidx::Int
    xinfo::EscapeInfo
end
struct AliasChange <: Change
    xidx::Int
    yidx::Int
end
struct ArgAliasChange <: Change
    xidx::Int
    yidx::Int
end
struct LivenessChange <: Change
    xidx::Int
    livepc::Int
end
const Changes = Vector{Change}

struct AnalysisState{T<:Callable}
    ir::IRCode
    estate::EscapeState
    changes::Changes
    get_escape_cache::T
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
    analyze_escapes(ir::IRCode, nargs::Int, call_resolved::Bool, get_escape_cache::Callable)
        -> estate::EscapeState

Analyzes escape information in `ir`:
- `nargs`: the number of actual arguments of the analyzed call
- `call_resolved`: if interprocedural calls are already resolved by `ssa_inlining_pass!`
- `get_escape_cache(::Union{InferenceResult,MethodInstance}) -> Union{Nothing,ArgEscapeCache}`:
  retrieves cached argument escape information
"""
function analyze_escapes(ir::IRCode, nargs::Int, call_resolved::Bool, get_escape_cache::T) where T<:Callable
    stmts = ir.stmts
    nstmts = length(stmts) + length(ir.new_nodes.stmts)

    tryregions, arrayinfo, callinfo = compute_frameinfo(ir, call_resolved)
    estate = EscapeState(nargs, nstmts, arrayinfo)
    changes = Changes() # keeps changes that happen at current statement
    astate = AnalysisState(ir, estate, changes, get_escape_cache)

    local debug_itr_counter = 0
    while true
        local anyupdate = false

        for pc in nstmts:-1:1
            stmt = getinst(ir, pc)[:inst]

            # collect escape information
            if isa(stmt, Expr)
                head = stmt.head
                if head === :call
                    if callinfo !== nothing
                        escape_call!(astate, pc, stmt.args, callinfo)
                    else
                        escape_call!(astate, pc, stmt.args)
                    end
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

"""
    compute_frameinfo(ir::IRCode, call_resolved::Bool) -> (tryregions, arrayinfo, callinfo)

A preparatory linear scan before the escape analysis on `ir` to find:
- `tryregions::Union{Nothing,Vector{UnitRange{Int}}}`: regions in which potential `throw`s can be caught (used by `escape_exception!`)
- `arrayinfo::Union{Nothing,IdDict{Int,Vector{Int}}}`: array allocations whose dimensions are known precisely (with some very simple local analysis)
- `callinfo::`: when `!call_resolved`, `compute_frameinfo` additionally returns `callinfo::Vector{Union{MethodInstance,InferenceResult}}`,
  which contains information about statically resolved callsites.
  The inliner will use essentially equivalent interprocedural information to inline callees as well as resolve static callsites,
  this additional information won't be required when analyzing post-inlining IR.

!!! note
    This array dimension analysis to compute `arrayinfo` is very local and doesn't account
    for flow-sensitivity nor complex aliasing.
    Ideally this dimension analysis should be done as a part of type inference that
    propagates array dimenstions in a flow sensitive way.
"""
function compute_frameinfo(ir::IRCode, call_resolved::Bool)
    nstmts, nnewnodes = length(ir.stmts), length(ir.new_nodes.stmts)
    tryregions, arrayinfo = nothing, nothing
    if !call_resolved
        callinfo = Vector{Any}(undef, nstmts+nnewnodes)
    else
        callinfo = nothing
    end
    for idx in 1:nstmts+nnewnodes
        inst = getinst(ir, idx)
        stmt = inst[:inst]
        if !call_resolved
            # TODO don't call `check_effect_free!` in the inlinear
            check_effect_free!(ir, idx, stmt, inst[:type])
        end
        if callinfo !== nothing && isexpr(stmt, :call)
            callinfo[idx] = resolve_call(ir, stmt, inst[:info])
        elseif isexpr(stmt, :enter)
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
            # TODO this super limited alias analysis is able to handle only very simple cases
            # this should be replaced with a proper forward dimension analysis
            if isa(stmt, PhiNode)
                values = stmt.values
                local dims = nothing
                for i = 1:length(values)
                    if isassigned(values, i)
                        val = values[i]
                        if isa(val, SSAValue) && haskey(arrayinfo, val.id)
                            if dims === nothing
                                dims = arrayinfo[val.id]
                                continue
                            elseif dims == arrayinfo[val.id]
                                continue
                            end
                        end
                    end
                    @goto next_stmt
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
    return tryregions, arrayinfo, callinfo
end

# define resolve_call
if _TOP_MOD === Core.Compiler
    include("compiler/ssair/EscapeAnalysis/interprocedural.jl")
else
    include("interprocedural.jl")
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
    propagate_escape_change!(⊔ₑ, estate, change)

# allows this to work as lattice join as well as lattice meet
@inline function propagate_escape_change!(@specialize(op),
    estate::EscapeState, change::EscapeChange)
    (; xidx, xinfo) = change
    anychanged = _propagate_escape_change!(op, estate, xidx, xinfo)
    # COMBAK is there a more efficient method of escape information equalization on aliasset?
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
    anychange = false
    (; xidx, yidx) = change
    aliasset = estate.aliasset
    xroot = find_root!(aliasset, xidx)
    yroot = find_root!(aliasset, yidx)
    if xroot ≠ yroot
        union!(aliasset, xroot, yroot)
        return true
    end
    return false
end

function add_escape_change!(astate::AnalysisState, @nospecialize(x), xinfo::EscapeInfo,
    force::Bool = false)
    xinfo === ⊥ && return nothing # performance optimization
    xidx = iridx(x, astate.estate)
    if xidx !== nothing
        if force || !isbitstype(widenconst(argextype(x, astate.ir)))
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
    if xidx !== nothing && yidx !== nothing
        if !isaliased(xidx, yidx, astate.estate)
            pushfirst!(astate.changes, AliasChange(xidx, yidx))
        end
        # add new escape change here so that it's shared among the expanded `aliasset` in `propagate_escape_change!`
        xinfo = estate.escapes[xidx]
        yinfo = estate.escapes[yidx]
        add_escape_change!(astate, x, xinfo ⊔ₑ yinfo, #=force=#true)
    end
    return nothing
end

struct LocalDef
    idx::Int
end
struct LocalUse
    idx::Int
end

function add_alias_escapes!(astate::AnalysisState, @nospecialize(v), ainfo::AInfo)
    estate = astate.estate
    for x in ainfo
        isa(x, LocalUse) || continue # ignore def
        x = SSAValue(x.idx) # obviously this won't be true once we implement interprocedural AliasInfo
        add_alias_change!(astate, v, x)
    end
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

function escape_unanalyzable_obj!(astate::AnalysisState, @nospecialize(obj), objinfo::EscapeInfo)
    objinfo = EscapeInfo(objinfo, true)
    add_escape_change!(astate, obj, objinfo)
    return objinfo
end

@noinline function unexpected_assignment!(ir::IRCode, pc::Int)
    @eval Main (ir = $ir; pc = $pc)
    error("unexpected assignment found: inspect `Main.pc` and `Main.pc`")
end

is_effect_free(ir::IRCode, pc::Int) = getinst(ir, pc)[:flag] & IR_FLAG_EFFECT_FREE ≠ 0

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
        for pc in xt
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

# escape statically-resolved call, i.e. `Expr(:invoke, ::MethodInstance, ...)`
escape_invoke!(astate::AnalysisState, pc::Int, args::Vector{Any}) =
    escape_invoke!(astate, pc, args, first(args)::MethodInstance, 2)

function escape_invoke!(astate::AnalysisState, pc::Int, args::Vector{Any},
    linfo::Linfo, first_idx::Int, last_idx::Int = length(args))
    if isa(linfo, InferenceResult)
        cache = astate.get_escape_cache(linfo)
        linfo = linfo.linfo
    else
        cache = astate.get_escape_cache(linfo)
    end
    if cache === nothing
        return add_conservative_changes!(astate, pc, args, 2)
    else
        cache = cache::ArgEscapeCache
    end
    ret = SSAValue(pc)
    retinfo = astate.estate[ret] # escape information imposed on the call statement
    method = linfo.def::Method
    nargs = Int(method.nargs)
    for (i, argidx) in enumerate(first_idx:last_idx)
        arg = args[argidx]
        if i > nargs
            # handle isva signature
            # COMBAK will this be invalid once we take alias information into account?
            i = nargs
        end
        arginfo = cache.argescapes[i]
        info = from_interprocedural(arginfo, pc)
        if has_return_escape(arginfo)
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
    for (; aidx, bidx) in cache.argaliases
        add_alias_change!(astate, args[aidx-(first_idx-1)], args[bidx-(first_idx-1)])
    end
    # we should disable the alias analysis on this newly introduced object
    add_escape_change!(astate, ret, EscapeInfo(retinfo, true))
end

"""
    from_interprocedural(arginfo::ArgEscapeInfo, pc::Int) -> x::EscapeInfo

Reinterprets the escape information imposed on the call argument which is cached as `arginfo`
in the context of the caller frame, where `pc` is the SSA statement number of the return value.
"""
function from_interprocedural(arginfo::ArgEscapeInfo, pc::Int)
    has_all_escape(arginfo) && return ⊤

    ThrownEscape = has_thrown_escape(arginfo) ? LivenessSet(pc) : BOT_THROWN_ESCAPE

    return EscapeInfo(
        #=Analyzed=#true, #=ReturnEscape=#false, ThrownEscape,
        # FIXME implement interprocedural memory effect-analysis
        # currently, this essentially disables the entire field analysis
        # it might be okay from the SROA point of view, since we can't remove the allocation
        # as far as it's passed to a callee anyway, but still we may want some field analysis
        # for e.g. stack allocation or some other IPO optimizations
        #=AliasInfo=#true, #=Liveness=#LivenessSet(pc))
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
    nothrow = is_effect_free(astate.ir, pc)
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

function escape_call!(astate::AnalysisState, pc::Int, args::Vector{Any}, callinfo::Vector{Any})
    info = callinfo[pc]
    if isa(info, Bool)
        info && return # known to be no escape
        # now cascade to the builtin handling
        escape_call!(astate, pc, args)
        return
    elseif isa(info, CallInfo)
        for linfo in info.linfos
            escape_invoke!(astate, pc, args, linfo, 1)
        end
        # accounts for a potential escape via MethodError
        info.nothrow || add_thrown_escapes!(astate, pc, args)
        return
    else
        @assert info === missing
        # if this call couldn't be analyzed, escape it conservatively
        add_conservative_changes!(astate, pc, args)
    end
end

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
        # if this call hasn't been handled by any of pre-defined handlers, escape it conservatively
        add_conservative_changes!(astate, pc, args)
        return
    elseif result === true
        add_liveness_changes!(astate, pc, args, 2)
        return # ThrownEscape is already checked
    else
        # we escape statements with the `ThrownEscape` property using the effect-freeness
        # computed by `stmt_effect_free` invoked within inlining
        # TODO throwness ≠ "effect-free-ness"
        if is_effect_free(astate.ir, pc)
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

function escape_new!(astate::AnalysisState, pc::Int, args::Vector{Any})
    obj = SSAValue(pc)
    objinfo = astate.estate[obj]
    AliasInfo = objinfo.AliasInfo
    nargs = length(args)
    if isa(AliasInfo, Bool)
        AliasInfo && @goto conservative_propagation
        # AliasInfo of this object hasn't been analyzed yet: set AliasInfo now
        typ = widenconst(argextype(obj, astate.ir))
        nflds = fieldcount_noerror(typ)
        if nflds === nothing
            AliasInfo = Unindexable()
            @goto escape_unindexable_def
        else
            AliasInfo = IndexableFields(nflds)
            @goto escape_indexable_def
        end
    elseif isa(AliasInfo, IndexableFields)
        @label escape_indexable_def
        # fields are known precisely: propagate escape information imposed on recorded possibilities to the exact field values
        infos = AliasInfo.infos
        nf = length(infos)
        objinfo′ = ignore_aliasinfo(objinfo)
        for i in 2:nargs
            i-1 > nf && break # may happen when e.g. ϕ-node merges values with different types
            arg = args[i]
            add_alias_escapes!(astate, arg, infos[i-1])
            push!(infos[i-1], LocalDef(pc))
            # propagate the escape information of this object ignoring field information
            add_escape_change!(astate, arg, objinfo′)
            add_liveness_change!(astate, arg, pc)
        end
        add_escape_change!(astate, obj, EscapeInfo(objinfo, AliasInfo)) # update with new AliasInfo
    elseif isa(AliasInfo, Unindexable)
        @label escape_unindexable_def
        # fields are known partially: propagate escape information imposed on recorded possibilities to all fields values
        info = AliasInfo.info
        objinfo′ = ignore_aliasinfo(objinfo)
        for i in 2:nargs
            arg = args[i]
            add_alias_escapes!(astate, arg, info)
            push!(info, LocalDef(pc))
            # propagate the escape information of this object ignoring field information
            add_escape_change!(astate, arg, objinfo′)
            add_liveness_change!(astate, arg, pc)
        end
        add_escape_change!(astate, obj, EscapeInfo(objinfo, AliasInfo)) # update with new AliasInfo
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
    if !is_effect_free(astate.ir, pc)
        add_thrown_escapes!(astate, pc, args)
    end
end

function escape_builtin!(::typeof(tuple), astate::AnalysisState, pc::Int, args::Vector{Any})
    escape_new!(astate, pc, args)
    return false
end

function analyze_fields(ir::IRCode, @nospecialize(typ), @nospecialize(fld))
    nflds = fieldcount_noerror(typ)
    if nflds === nothing
        return Unindexable(), 0
    end
    if isa(typ, DataType)
        fldval = try_compute_field(ir, fld)
        fidx = try_compute_fieldidx(typ, fldval)
    else
        fidx = nothing
    end
    if fidx === nothing
        return Unindexable(), 0
    end
    return IndexableFields(nflds), fidx
end

function reanalyze_fields(ir::IRCode, AliasInfo::IndexableFields, @nospecialize(typ), @nospecialize(fld))
    nflds = fieldcount_noerror(typ)
    if nflds === nothing
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
    if nflds > ninfos
        for _ in 1:(nflds-ninfos)
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
        # AliasInfo of this object hasn't been analyzed yet: set AliasInfo now
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
        push!(AliasInfo.infos[fidx], LocalUse(pc))
        add_escape_change!(astate, obj, EscapeInfo(objinfo, AliasInfo)) # update with new AliasInfo
    elseif isa(AliasInfo, Unindexable)
        @label record_unindexable_use
        push!(AliasInfo.info, LocalUse(pc))
        add_escape_change!(astate, obj, EscapeInfo(objinfo, AliasInfo)) # update with new AliasInfo
    else
        # this object has been used as array, but it is used as struct here (i.e. should throw)
        # update obj's field information and just handle this case conservatively
        objinfo = escape_unanalyzable_obj!(astate, obj, objinfo)
        @label conservative_propagation
        # at the extreme case, a field of `obj` may point to `obj` itself
        # so add the alias change here as the most conservative propagation
        add_alias_change!(astate, obj, SSAValue(pc))
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
        # AliasInfo of this object hasn't been analyzed yet: set AliasInfo now
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
        push!(AliasInfo.infos[fidx], LocalDef(pc))
        objinfo = EscapeInfo(objinfo, AliasInfo)
        add_escape_change!(astate, obj, objinfo) # update with new AliasInfo
        # propagate the escape information of this object ignoring field information
        add_escape_change!(astate, val, ignore_aliasinfo(objinfo))
    elseif isa(AliasInfo, Unindexable)
        info = AliasInfo.info
        @label escape_unindexable_def
        add_alias_escapes!(astate, val, AliasInfo.info)
        push!(AliasInfo.info, LocalDef(pc))
        objinfo = EscapeInfo(objinfo, AliasInfo)
        add_escape_change!(astate, obj, objinfo) # update with new AliasInfo
        # propagate the escape information of this object ignoring field information
        add_escape_change!(astate, val, ignore_aliasinfo(objinfo))
    else
        # this object has been used as array, but it is used as struct here (i.e. should throw)
        # update obj's field information and just handle this case conservatively
        objinfo = escape_unanalyzable_obj!(astate, obj, objinfo)
        @label conservative_propagation
        # the field couldn't be analyzed: alias this object to the value being assigned
        # as the most conservative propagation (as required for ArgAliasing)
        add_alias_change!(astate, val, obj)
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
    estate = astate.estate
    if isa(ary, SSAValue) || isa(ary, Argument)
        aryinfo = estate[ary]
    else
        return true
    end
    AliasInfo = aryinfo.AliasInfo
    if isa(AliasInfo, Bool)
        AliasInfo && @goto conservative_propagation
        # AliasInfo of this array hasn't been analyzed yet: set AliasInfo now
        idx = array_nd_index(astate, ary, args[4:end])
        if isa(idx, Int)
            AliasInfo = IndexableElements(IdDict{Int,AInfo}())
            @goto record_indexable_use
        end
        AliasInfo = Unindexable()
        @goto record_unindexable_use
    elseif isa(AliasInfo, IndexableElements)
        idx = array_nd_index(astate, ary, args[4:end])
        if !isa(idx, Int)
            AliasInfo = merge_to_unindexable(AliasInfo)
            @goto record_unindexable_use
        end
        @label record_indexable_use
        info = get!(()->AInfo(), AliasInfo.infos, idx)
        push!(info, LocalUse(pc))
        add_escape_change!(astate, ary, EscapeInfo(aryinfo, AliasInfo)) # update with new AliasInfo
    elseif isa(AliasInfo, Unindexable)
        @label record_unindexable_use
        push!(AliasInfo.info, LocalUse(pc))
        add_escape_change!(astate, ary, EscapeInfo(aryinfo, AliasInfo)) # update with new AliasInfo
    else
        # this object has been used as struct, but it is used as array here (thus should throw)
        # update ary's element information and just handle this case conservatively
        aryinfo = escape_unanalyzable_obj!(astate, ary, aryinfo)
        @label conservative_propagation
        # at the extreme case, an element of `ary` may point to `ary` itself
        # so add the alias change here as the most conservative propagation
        add_alias_change!(astate, ary, SSAValue(pc))
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
        # AliasInfo of this array hasn't been analyzed yet: set AliasInfo now
        idx = array_nd_index(astate, ary, args[5:end])
        if isa(idx, Int)
            AliasInfo = IndexableElements(IdDict{Int,AInfo}())
            @goto escape_indexable_def
        end
        AliasInfo = Unindexable()
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
        push!(info, LocalDef(pc))
        add_escape_change!(astate, ary, EscapeInfo(aryinfo, AliasInfo)) # update with new AliasInfo
        # propagate the escape information of this array ignoring elements information
        add_escape_change!(astate, val, ignore_aliasinfo(aryinfo))
    elseif isa(AliasInfo, Unindexable)
        @label escape_unindexable_def
        add_alias_escapes!(astate, val, AliasInfo.info)
        push!(AliasInfo.info, LocalDef(pc))
        add_escape_change!(astate, ary, EscapeInfo(aryinfo, AliasInfo)) # update with new AliasInfo
        # propagate the escape information of this array ignoring elements information
        add_escape_change!(astate, val, ignore_aliasinfo(aryinfo))
    else
        # this object has been used as struct, but it is used as array here (thus should throw)
        # update ary's element information and just handle this case conservatively
        aryinfo = escape_unanalyzable_obj!(astate, ary, aryinfo)
        @label conservative_propagation
        add_alias_change!(astate, val, ary)
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
    aryt ⊑ Array || return false
    dimt = argextype(dim, ir)
    dimt ⊑ Int || return false
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
    aryt ⊑ Array || return add_fallback_changes!(astate, pc, args)
    for i in 1:ninds
        ind = args[i+6]
        indt = argextype(ind, astate.ir)
        indt ⊑ Integer || return add_fallback_changes!(astate, pc, args)
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
    aryt ⊑ Array || return add_fallback_changes!(astate, pc, args)
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
    arytype ⊑ Array || return false
    idxtype = argextype(args[7], src)
    idxtype ⊑ Csize_t || return false
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
    argextype(args[2], astate.ir) ⊑ arytype || return false
    return true
end

end # if isdefined(Core, :ImmutableArray)

if _TOP_MOD !== Core.Compiler
    # NOTE define fancy package utilities when developing EA as an external package
    include("EAUtils.jl")
    using .EAUtils
    export code_escapes, @code_escapes, __clear_cache!
end

end # baremodule EscapeAnalysis

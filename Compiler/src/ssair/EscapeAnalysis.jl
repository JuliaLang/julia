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

using Base: Base

# imports
import Base: ==, copy, getindex, setindex!
# usings
using Core
using Core: Builtin, IntrinsicFunction, SimpleVector, ifelse, sizeof
using Core.IR
using Base:       # Base definitions
    @__MODULE__, @assert, @eval, @goto, @inbounds, @inline, @label, @noinline,
    @nospecialize, @specialize, BitSet, IdDict, IdSet, UnitRange, Vector,
    delete!, empty!, enumerate, first, get, get!, hasintersect, haskey, isassigned,
    isempty, length, max, min, missing, println, push!, pushfirst!,
    !, !==, &, *, +, -, :, <, <<, >, |, ∈, ∉, ∩, ∪, ≠, ≤, ≥, ⊆
using ..Compiler: # Compiler specific definitions
    AbstractLattice, Compiler, IRCode, IR_FLAG_NOTHROW,
    argextype, fieldcount_noerror, has_flag, intrinsic_nothrow, is_meta_expr_head,
    is_identity_free_argtype, isexpr, setfield!_nothrow, singleton_type, try_compute_field,
    try_compute_fieldidx, widenconst

function include(x::String)
    if !isdefined(Base, :end_base_include)
        # During bootstrap, all includes are relative to `base/`
        x = ccall(:jl_prepend_string, Ref{String}, (Any, Any), "ssair/", x)
    end
    Compiler.include(@__MODULE__, x)
end

include("disjoint_set.jl")

const AInfo = IdSet{Any}

"""
    x::EscapeInfo

A lattice for escape information, which holds the following properties:
- `x.Analyzed::Bool`: not formally part of the lattice, only indicates whether `x` has been analyzed
- `x.ReturnEscape::Bool`: indicates `x` can escape to the caller via return
- `x.ThrownEscape::BitSet`: records SSA statement numbers where `x` can be thrown as exception:
  * `isempty(x.ThrownEscape)`: `x` will never be thrown in this call frame (the bottom)
  * `pc ∈ x.ThrownEscape`: `x` may be thrown at the SSA statement at `pc`
  * `-1 ∈ x.ThrownEscape`: `x` may be thrown at arbitrary points of this call frame (the top)
  This information will be used by `escape_exception!` to propagate potential escapes via exception.
- `x.AliasInfo::Union{Bool,IndexableFields,Unindexable}`: maintains all possible values
  that can be aliased to fields or array elements of `x`:
  * `x.AliasInfo === false` indicates the fields/elements of `x` aren't analyzed yet
  * `x.AliasInfo === true` indicates the fields/elements of `x` can't be analyzed,
    e.g. the type of `x` is not known or is not concrete and thus its fields/elements
    can't be known precisely
  * `x.AliasInfo::IndexableFields` records all the possible values that can be aliased to fields of object `x` with precise index information
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
    ThrownEscape::BitSet
    AliasInfo #::Union{IndexableFields,Unindexable,Bool}
    Liveness::BitSet

    function EscapeInfo(
        Analyzed::Bool,
        ReturnEscape::Bool,
        ThrownEscape::BitSet,
        AliasInfo#=::Union{IndexableFields,Unindexable,Bool}=#,
        Liveness::BitSet)
        @nospecialize AliasInfo
        return new(
            Analyzed,
            ReturnEscape,
            ThrownEscape,
            AliasInfo,
            Liveness)
    end
    function EscapeInfo(
        x::EscapeInfo,
        # non-concrete fields should be passed as default arguments
        # in order to avoid allocating non-concrete `NamedTuple`s
        AliasInfo#=::Union{IndexableFields,Unindexable,Bool}=# = x.AliasInfo;
        Analyzed::Bool = x.Analyzed,
        ReturnEscape::Bool = x.ReturnEscape,
        ThrownEscape::BitSet = x.ThrownEscape,
        Liveness::BitSet = x.Liveness)
        @nospecialize AliasInfo
        return new(
            Analyzed,
            ReturnEscape,
            ThrownEscape,
            AliasInfo,
            Liveness)
    end
end

# precomputed default values in order to eliminate computations at each callsite

const BOT_THROWN_ESCAPE = BitSet()
# NOTE the lattice operations should try to avoid actual set computations on this top value,
# and e.g. BitSet(0:1000000) should also work without incurring excessive computations
const TOP_THROWN_ESCAPE = BitSet(-1)

const BOT_LIVENESS = BitSet()
# NOTE the lattice operations should try to avoid actual set computations on this top value,
# and e.g. BitSet(0:1000000) should also work without incurring excessive computations
const TOP_LIVENESS = BitSet(-1:0)
const ARG_LIVENESS = BitSet(0)

# the constructors
NotAnalyzed() = EscapeInfo(false, false, BOT_THROWN_ESCAPE, false, BOT_LIVENESS) # not formally part of the lattice
NoEscape() = EscapeInfo(true, false, BOT_THROWN_ESCAPE, false, BOT_LIVENESS)
ArgEscape() = EscapeInfo(true, false, BOT_THROWN_ESCAPE, true, ARG_LIVENESS)
ReturnEscape(pc::Int) = EscapeInfo(true, true, BOT_THROWN_ESCAPE, false, BitSet(pc))
AllReturnEscape() = EscapeInfo(true, true, BOT_THROWN_ESCAPE, false, TOP_LIVENESS)
ThrownEscape(pc::Int) = EscapeInfo(true, false, BitSet(pc), false, BOT_LIVENESS)
AllEscape() = EscapeInfo(true, true, TOP_THROWN_ESCAPE, true, TOP_LIVENESS)

const ⊥, ⊤ = NotAnalyzed(), AllEscape()

# Convenience names for some ⊑ₑ queries
has_no_escape(x::EscapeInfo) = !x.ReturnEscape && isempty(x.ThrownEscape) && 0 ∉ x.Liveness
has_arg_escape(x::EscapeInfo) = 0 ∈ x.Liveness
has_return_escape(x::EscapeInfo) = x.ReturnEscape
has_return_escape(x::EscapeInfo, pc::Int) = x.ReturnEscape && (-1 ∈ x.Liveness || pc ∈ x.Liveness)
has_thrown_escape(x::EscapeInfo) = !isempty(x.ThrownEscape)
has_thrown_escape(x::EscapeInfo, pc::Int) = -1 ∈ x.ThrownEscape || pc ∈ x.ThrownEscape
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
struct Unindexable
    info::AInfo
end
IndexableFields(nflds::Int) = IndexableFields(AInfo[AInfo() for _ in 1:nflds])
Unindexable() = Unindexable(AInfo())
copy(AliasInfo::IndexableFields) = IndexableFields(AInfo[copy(info) for info in AliasInfo.infos])
copy(AliasInfo::Unindexable) = Unindexable(copy(AliasInfo.info))

merge_to_unindexable(AliasInfo::IndexableFields) = Unindexable(merge_to_unindexable(AliasInfo.infos))
merge_to_unindexable(AliasInfo::Unindexable, AliasInfos::IndexableFields) = Unindexable(merge_to_unindexable(AliasInfo.info, AliasInfos.infos))
merge_to_unindexable(infos::Vector{AInfo}) = merge_to_unindexable(AInfo(), infos)
function merge_to_unindexable(info::AInfo, infos::Vector{AInfo})
    for i = 1:length(infos)
        info = info ∪ infos[i]
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
        elseif isa(ya, Unindexable)
            xinfos, yinfo = xa.infos, ya.info
            for i = length(xinfos)
                xinfos[i] ⊆ yinfo || return false
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
    else
        xa = xa::Unindexable
        if isa(ya, IndexableFields)
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

"""
    estate::EscapeState

Extended lattice that maps arguments and SSA values to escape information represented as [`EscapeInfo`](@ref).
Escape information imposed on SSA IR element `x` can be retrieved by `estate[x]`.
"""
struct EscapeState
    escapes::Vector{EscapeInfo}
    aliasset::AliasSet
    nargs::Int
end
function EscapeState(nargs::Int, nstmts::Int)
    escapes = EscapeInfo[
        1 ≤ i ≤ nargs ? ArgEscape() : ⊥ for i in 1:(nargs+nstmts)]
    aliasset = AliasSet(nargs+nstmts)
    return EscapeState(escapes, aliasset, nargs)
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
    return xidx > estate.nargs ? SSAValue(xidx-estate.nargs) : Argument(xidx)
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
    escape_bits::UInt8
end
function ArgEscapeInfo(x::EscapeInfo)
    x === ⊤ && return ArgEscapeInfo(ARG_ALL_ESCAPE)
    escape_bits = 0x00
    has_return_escape(x) && (escape_bits |= ARG_RETURN_ESCAPE)
    has_thrown_escape(x) && (escape_bits |= ARG_THROWN_ESCAPE)
    return ArgEscapeInfo(escape_bits)
end

const ARG_ALL_ESCAPE    = 0x01 << 0
const ARG_RETURN_ESCAPE = 0x01 << 1
const ARG_THROWN_ESCAPE = 0x01 << 2

has_no_escape(x::ArgEscapeInfo)     = !has_all_escape(x) && !has_return_escape(x) && !has_thrown_escape(x)
has_all_escape(x::ArgEscapeInfo)    = x.escape_bits & ARG_ALL_ESCAPE    ≠ 0
has_return_escape(x::ArgEscapeInfo) = x.escape_bits & ARG_RETURN_ESCAPE ≠ 0
has_thrown_escape(x::ArgEscapeInfo) = x.escape_bits & ARG_THROWN_ESCAPE ≠ 0

struct ArgAliasing
    aidx::Int
    bidx::Int
end

struct ArgEscapeCache
    argescapes::Vector{ArgEscapeInfo}
    argaliases::Vector{ArgAliasing}
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
        return new(argescapes, argaliases)
    end
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

struct AnalysisState{GetEscapeCache, Lattice<:AbstractLattice}
    ir::IRCode
    estate::EscapeState
    changes::Changes
    𝕃ₒ::Lattice
    get_escape_cache::GetEscapeCache
end

"""
    analyze_escapes(ir::IRCode, nargs::Int, get_escape_cache) -> estate::EscapeState

Analyzes escape information in `ir`:
- `nargs`: the number of actual arguments of the analyzed call
- `get_escape_cache(::MethodInstance) -> Union{Bool,ArgEscapeCache}`:
  retrieves cached argument escape information
"""
function analyze_escapes(ir::IRCode, nargs::Int, 𝕃ₒ::AbstractLattice, get_escape_cache)
    stmts = ir.stmts
    nstmts = length(stmts) + length(ir.new_nodes.stmts)

    tryregions = compute_frameinfo(ir)
    estate = EscapeState(nargs, nstmts)
    changes = Changes() # keeps changes that happen at current statement
    astate = AnalysisState(ir, estate, changes, 𝕃ₒ, get_escape_cache)

    local debug_itr_counter = 0
    while true
        local anyupdate = false

        for pc in nstmts:-1:1
            stmt = ir[SSAValue(pc)][:stmt]

            # collect escape information
            if isa(stmt, Expr)
                head = stmt.head
                if head === :call
                    escape_call!(astate, pc, stmt.args)
                elseif head === :invoke
                    escape_invoke!(astate, pc, stmt.args)
                elseif head === :new || head === :splatnew
                    escape_new!(astate, pc, stmt.args)
                elseif head === :foreigncall
                    escape_foreigncall!(astate, pc, stmt.args)
                elseif head === :throw_undef_if_not # XXX when is this expression inserted ?
                    add_escape_change!(astate, stmt.args[1], ThrownEscape(pc))
                elseif is_meta_expr_head(head)
                    # meta expressions doesn't account for any usages
                    continue
                elseif head === :leave || head === :the_exception || head === :pop_exception
                    # ignore these expressions since escapes via exceptions are handled by `escape_exception!`
                    # `escape_exception!` conservatively propagates `AllEscape` anyway,
                    # and so escape information imposed on `:the_exception` isn't computed
                    continue
                elseif head === :gc_preserve_begin
                    # GC preserve is handled by `escape_gc_preserve!`
                elseif head === :gc_preserve_end
                    escape_gc_preserve!(astate, pc, stmt.args)
                elseif head === :static_parameter ||  # this exists statically, not interested in its escape
                       head === :copyast ||           # XXX escape something?
                       head === :isdefined            # just returns `Bool`, nothing accounts for any escapes
                    continue
                else
                    add_conservative_changes!(astate, pc, stmt.args)
                end
            elseif isa(stmt, EnterNode)
                # Handled via escape_exception!
                continue
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
    compute_frameinfo(ir::IRCode) -> tryregions

A preparatory linear scan before the escape analysis on `ir` to find
`tryregions::Union{Nothing,Vector{UnitRange{Int}}}`, that represent regions in which
potential `throw`s can be caught (used by `escape_exception!`)
"""
function compute_frameinfo(ir::IRCode)
    nstmts, nnewnodes = length(ir.stmts), length(ir.new_nodes.stmts)
    tryregions = nothing
    for idx in 1:nstmts+nnewnodes
        inst = ir[SSAValue(idx)]
        stmt = inst[:stmt]
        if isa(stmt, EnterNode)
            leave_block = stmt.catch_dest
            if leave_block ≠ 0
                @assert idx ≤ nstmts "try/catch inside new_nodes unsupported"
                tryregions === nothing && (tryregions = UnitRange{Int}[])
                leave_pc = first(ir.cfg.blocks[leave_block].stmts)
                push!(tryregions, idx:leave_pc)
            end
        end
    end
    return tryregions
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

# propagate Liveness changes separately in order to avoid constructing too many BitSet
@inline function propagate_liveness_change!(estate::EscapeState, change::LivenessChange)
    (; xidx, livepc) = change
    info = estate.escapes[xidx]
    Liveness = info.Liveness
    Liveness === TOP_LIVENESS && return false
    livepc ∈ Liveness && return false
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
        if force || !is_identity_free_argtype(argextype(x, astate.ir))
            push!(astate.changes, EscapeChange(xidx, xinfo))
        end
    end
    return nothing
end

function add_liveness_change!(astate::AnalysisState, @nospecialize(x), livepc::Int)
    xidx = iridx(x, astate.estate)
    if xidx !== nothing
        if !is_identity_free_argtype(argextype(x, astate.ir))
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

is_nothrow(ir::IRCode, pc::Int) = has_flag(ir[SSAValue(pc)], IR_FLAG_NOTHROW)

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
    # TODO? set up a special effect bit that checks the existence of `rethrow` and `current_exceptions` and use it here
    excinfo = ⊤
    escapes = estate.escapes
    for i in 1:length(escapes)
        x = escapes[i]
        xt = x.ThrownEscape
        xt === TOP_THROWN_ESCAPE && @goto propagate_exception_escape # fast pass
        for pc in xt
            for region in tryregions
                pc ∈ region && @goto propagate_exception_escape # early break because of AllEscape
            end
        end
        continue
        @label propagate_exception_escape
        xval = irval(i, estate)
        add_escape_change!(astate, xval, excinfo)
    end
end

# escape statically-resolved call, i.e. `Expr(:invoke, ::MethodInstance, ...)`
function escape_invoke!(astate::AnalysisState, pc::Int, args::Vector{Any})
    codeinst = first(args)
    if codeinst isa MethodInstance
        mi = codeinst
    else
        mi = (codeinst::CodeInstance).def
    end
    first_idx, last_idx = 2, length(args)
    add_liveness_changes!(astate, pc, args, first_idx, last_idx)
    # TODO inspect `astate.ir.stmts[pc][:info]` and use const-prop'ed `InferenceResult` if available
    cache = astate.get_escape_cache(codeinst)
    ret = SSAValue(pc)
    if cache isa Bool
        if cache
            # This method call is very simple and has good effects, so there's no need to
            # escape its arguments. However, since the arguments might be returned, we need
            # to consider the possibility of aliasing between them and the return value.
            for argidx = first_idx:last_idx
                arg = args[argidx]
                if arg isa GlobalRef
                    continue # :effect_free guarantees that nothings escapes to the global scope
                end
                if !is_identity_free_argtype(argextype(arg, astate.ir))
                    add_alias_change!(astate, ret, arg)
                end
            end
            return nothing
        else
            return add_conservative_changes!(astate, pc, args, 2)
        end
    end
    cache = cache::ArgEscapeCache
    retinfo = astate.estate[ret] # escape information imposed on the call statement
    method = mi.def::Method
    nargs = Int(method.nargs)
    for (i, argidx) in enumerate(first_idx:last_idx)
        arg = args[argidx]
        if i > nargs
            # handle isva signature
            # COMBAK will this be invalid once we take alias information into account?
            i = nargs
        end
        argescape = cache.argescapes[i]
        info = from_interprocedural(argescape, pc)
        # propagate the escape information imposed on this call argument by the callee
        add_escape_change!(astate, arg, info)
        if has_return_escape(argescape)
            # if this argument can be "returned", we should also account for possible
            # aliasing between this argument and the returned value
            add_alias_change!(astate, ret, arg)
        end
    end
    for (; aidx, bidx) in cache.argaliases
        add_alias_change!(astate, args[aidx+(first_idx-1)], args[bidx+(first_idx-1)])
    end
    # we should disable the alias analysis on this newly introduced object
    add_escape_change!(astate, ret, EscapeInfo(retinfo, true))
end

"""
    from_interprocedural(argescape::ArgEscapeInfo, pc::Int) -> x::EscapeInfo

Reinterprets the escape information imposed on the call argument which is cached as `argescape`
in the context of the caller frame, where `pc` is the SSA statement number of the return value.
"""
function from_interprocedural(argescape::ArgEscapeInfo, pc::Int)
    has_all_escape(argescape) && return ⊤
    ThrownEscape = has_thrown_escape(argescape) ? BitSet(pc) : BOT_THROWN_ESCAPE
    # TODO implement interprocedural memory effect-analysis:
    # currently, this essentially disables the entire field analysis–it might be okay from
    # the SROA point of view, since we can't remove the allocation as far as it's passed to
    # a callee anyway, but still we may want some field analysis for e.g. stack allocation
    # or some other IPO optimizations
    AliasInfo = true
    Liveness = BitSet(pc)
    return EscapeInfo(#=Analyzed=#true, #=ReturnEscape=#false, ThrownEscape, AliasInfo, Liveness)
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
    # NOTE array allocations might have been proven as nothrow (https://github.com/JuliaLang/julia/pull/43565)
    nothrow = is_nothrow(astate.ir, pc)
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

function escape_gc_preserve!(astate::AnalysisState, pc::Int, args::Vector{Any})
    @assert length(args) == 1 "invalid :gc_preserve_end"
    val = args[1]
    @assert val isa SSAValue "invalid :gc_preserve_end"
    beginstmt = astate.ir[val][:stmt]
    @assert isexpr(beginstmt, :gc_preserve_begin) "invalid :gc_preserve_end"
    beginargs = beginstmt.args
    # COMBAK we might need to add liveness for all statements from `:gc_preserve_begin` to `:gc_preserve_end`
    add_liveness_changes!(astate, pc, beginargs)
end

function escape_call!(astate::AnalysisState, pc::Int, args::Vector{Any})
    ft = argextype(first(args), astate.ir)
    f = singleton_type(ft)
    if f isa IntrinsicFunction
        if is_nothrow(astate.ir, pc)
            add_liveness_changes!(astate, pc, args, 2)
        else
            add_fallback_changes!(astate, pc, args, 2)
        end
        # TODO needs to account for pointer operations?
    elseif f isa Builtin
        result = escape_builtin!(f, astate, pc, args)
        if result === missing
            # if this call hasn't been handled by any of pre-defined handlers, escape it conservatively
            add_conservative_changes!(astate, pc, args)
        elseif result === true
            add_liveness_changes!(astate, pc, args, 2)
        elseif is_nothrow(astate.ir, pc)
            add_liveness_changes!(astate, pc, args, 2)
        else
            add_fallback_changes!(astate, pc, args, 2)
        end
    else
        # escape this generic function or unknown function call conservatively
        add_conservative_changes!(astate, pc, args)
    end
end

escape_builtin!(@nospecialize(f), _...) = missing

# safe builtins
escape_builtin!(::typeof(isa), _...) = false
escape_builtin!(::typeof(typeof), _...) = false
escape_builtin!(::typeof(sizeof), _...) = false
escape_builtin!(::typeof(===), _...) = false
escape_builtin!(::typeof(Core.donotdelete), _...) = false
# not really safe, but `ThrownEscape` will be imposed later
escape_builtin!(::typeof(isdefined), _...) = false
escape_builtin!(::typeof(throw), _...) = false
escape_builtin!(::typeof(Core.throw_methoderror), _...) = false

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
        AliasInfo = copy(AliasInfo)
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
        AliasInfo = copy(AliasInfo)
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
    if !is_nothrow(astate.ir, pc)
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

function reanalyze_fields(AliasInfo::IndexableFields, ir::IRCode, @nospecialize(typ), @nospecialize(fld))
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
    AliasInfo = copy(AliasInfo)
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
        # unanalyzable object, so the return value is also unanalyzable
        add_escape_change!(astate, SSAValue(pc), ⊤)
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
        AliasInfo, fidx = reanalyze_fields(AliasInfo, ir, typ, args[3])
        isa(AliasInfo, Unindexable) && @goto record_unindexable_use
        @label record_indexable_use
        push!(AliasInfo.infos[fidx], LocalUse(pc))
        add_escape_change!(astate, obj, EscapeInfo(objinfo, AliasInfo)) # update with new AliasInfo
    elseif isa(AliasInfo, Unindexable)
        AliasInfo = copy(AliasInfo)
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
        AliasInfo, fidx = reanalyze_fields(AliasInfo, ir, typ, args[3])
        isa(AliasInfo, Unindexable) && @goto escape_unindexable_def
        @label escape_indexable_def
        add_alias_escapes!(astate, val, AliasInfo.infos[fidx])
        push!(AliasInfo.infos[fidx], LocalDef(pc))
        objinfo = EscapeInfo(objinfo, AliasInfo)
        add_escape_change!(astate, obj, objinfo) # update with new AliasInfo
        # propagate the escape information of this object ignoring field information
        add_escape_change!(astate, val, ignore_aliasinfo(objinfo))
    elseif isa(AliasInfo, Unindexable)
        AliasInfo = copy(AliasInfo)
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
    if length(args) == 4 && setfield!_nothrow(astate.𝕃ₒ,
        argextype(args[2], ir), argextype(args[3], ir), argextype(args[4], ir))
        return true
    elseif length(args) == 3 && setfield!_nothrow(astate.𝕃ₒ,
        argextype(args[2], ir), argextype(args[3], ir))
        return true
    else
        add_thrown_escapes!(astate, pc, args, 2)
        return true
    end
end

function escape_builtin!(::typeof(Core.finalizer), astate::AnalysisState, pc::Int, args::Vector{Any})
    if length(args) ≥ 3
        obj = args[3]
        add_liveness_change!(astate, obj, pc) # TODO setup a proper FinalizerEscape?
    end
    return false
end

end # baremodule EscapeAnalysis

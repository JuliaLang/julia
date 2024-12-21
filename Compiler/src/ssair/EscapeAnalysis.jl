baremodule EscapeAnalysis

export
    analyze_escapes,
    getaliases,
    is_load_forwardable,
    is_not_analyzed,
    isaliased,
    has_no_escape,
    has_arg_escape,
    has_return_escape,
    has_thrown_escape,
    has_all_escape

using Base: Base

# imports
import Base: ==, ∈, copy, delete!, getindex, isempty, setindex!
# usings
using Core: Builtin, IntrinsicFunction, SimpleVector, ifelse, sizeof
using Core.IR
using Base:       # Base definitions
    @__MODULE__, @assert, @eval, @goto, @inbounds, @inline, @isdefined, @label, @noinline,
    @nospecialize, @specialize, BitSet, IdDict, IdSet, Pair, RefValue, Vector,
    _bits_findnext, copy!, empty!, enumerate, error, fill!, first, get, hasintersect,
    haskey, isassigned, isexpr, keys, last, length, max, min, missing, only, println, push!,
    pushfirst!, resize!, :, !, !==, <, <<, >, =>, ≠, ≤, ≥, ∉, ∌, ⊆, ⊊, ⊇, ⊋, &, *, +, -, |
using ..Compiler: # Compiler specific definitions
    @show, Compiler, HandlerInfo, IRCode, IR_FLAG_NOTHROW, NewNodeInfo, SimpleHandler,
    argextype, argument_datatype, compute_trycatch, datatype_min_ninitialized,
    fieldcount_noerror, gethandler, has_flag, is_identity_free_argtype,
    is_inaccessiblememonly, is_inaccessiblemem_or_argmemonly, is_meta_expr_head,
    is_nothrow, isterminator, singleton_type, try_compute_field, try_compute_fieldidx,
    widenconst

function include(x::String)
    if !isdefined(Base, :end_base_include)
        # During bootstrap, all includes are relative to `base/`
        x = ccall(:jl_prepend_string, Ref{String}, (Any, Any), "ssair/", x)
    end
    Compiler.include(@__MODULE__, x)
end

include("disjoint_set.jl")

abstract type MemoryKind end
struct UninitializedMemory <: MemoryKind end
struct CallerMemory <: MemoryKind
    id::Int
    arg::Argument # TODO arg::Union{Argument,CallerMemory}
    fidx::Int
end
# struct SSAValueMemory <: MemoryKind end
# struct GlobalRefMemory <: MemoryKind end
# struct LiteralMemory <: MemoryKind end

# TODO [^CFG-aware `MemoryInfo`]:
# By incorporating some form of CFG information into `MemoryInfo`, it becomes possible
# to enable load-forwarding even in cases where conflicts occur by inserting φ-nodes.

abstract type MemoryInfo end
struct MustAliasMemoryInfo <: MemoryInfo
    alias::Any # anything that is valid as IR elements (e.g. `SSAValue`, `Argument`, `GlobalRef`, literals, and `MemoryKind`)
    MustAliasMemoryInfo(@nospecialize alias) = new(alias)
end
struct MayAliasMemoryInfo <: MemoryInfo
    aliases::IdSet{Any}
    function MayAliasMemoryInfo(aliases::IdSet{Any})
        @assert length(aliases) ≥ 2
        return new(aliases)
    end
end
struct UnknownMemoryInfo <: MemoryInfo end # not part of the `⊑ₘ` lattice, just a marker for `ssamemoryinfo`
const ⊤ₘ = UnknownMemoryInfo()

x::MemoryInfo == y::MemoryInfo = begin
    @nospecialize
    if x isa MayAliasMemoryInfo
        return y isa MayAliasMemoryInfo && x.aliases == y.aliases
    else
        x = x::MustAliasMemoryInfo
        y isa MustAliasMemoryInfo || return false
        return x === y
    end
end
function copy(x::MemoryInfo)
    @nospecialize x
    if x isa MayAliasMemoryInfo
        return MayAliasMemoryInfo(copy(x.aliases))
    end
    return x
end

abstract type ObjectInfo end
struct HasUnanalyzedMemory <: ObjectInfo end
const FieldInfos = Vector{MemoryInfo}
struct HasIndexableFields <: ObjectInfo
    fields::FieldInfos
end
const CallerMemoryList = Vector{Union{CallerMemory,IdSet{CallerMemory}}}
struct HasIndexableCallerFields <: ObjectInfo
    fields::FieldInfos
    caller_memory_list::CallerMemoryList
    function HasIndexableCallerFields(
        fields::FieldInfos,
        caller_memory_list::CallerMemoryList)
        @assert length(fields) == length(caller_memory_list)
        return new(fields, caller_memory_list)
    end
end
struct HasUnknownMemory <: ObjectInfo end
const ⊥ₒ, ⊤ₒ = HasUnanalyzedMemory(), HasUnknownMemory()
x::ObjectInfo == y::ObjectInfo = begin
    @nospecialize
    x === y && return true
    if x === ⊥ₒ
        return y === ⊥ₒ
    elseif x === ⊤ₒ
        return y === ⊤ₒ
    elseif x isa HasIndexableFields
        y isa HasIndexableFields || return false
        return x.fields == y.fields
    else
        x = x::HasIndexableCallerFields
        y isa HasIndexableCallerFields || return false
        return x.fields == y.fields && x.caller_memory_list == y.caller_memory_list
    end
end
function copy(x::ObjectInfo)
    @nospecialize x
    if x isa HasIndexableFields
        return HasIndexableFields(
            MemoryInfo[copy(xfinfo) for xfinfo in x.fields])
    elseif x isa HasIndexableCallerFields
        return HasIndexableCallerFields(
            MemoryInfo[copy(xfinfo) for xfinfo in x.fields],
            Union{CallerMemory,IdSet{CallerMemory}}[
                caller_memory isa IdSet{CallerMemory} ? copy(caller_memory) : caller_memory
                for caller_memory in x.caller_memory_list])
    end
    return x
end

abstract type Liveness end
struct BotLiveness <: Liveness end
struct PCLiveness <: Liveness
    pcs::BitSet
end
PCLiveness(pc::Int) = PCLiveness(BitSet(pc))
struct TopLiveness <: Liveness end
const ⊥ₗ, ⊤ₗ = BotLiveness(), TopLiveness()
x::Liveness == y::Liveness = begin
    @nospecialize
    if x === ⊥ₗ
        return y === ⊥ₗ
    elseif x === ⊤ₗ
        return y === ⊤ₗ
    else
        x = x::PCLiveness
        y isa PCLiveness || return false
        return x.pcs == y.pcs
    end
end
pc::Int ∈ x::Liveness = begin
    @nospecialize x
    if x === ⊤ₗ
        return true
    elseif x === ⊥ₗ
        return false
    else
        x = x::PCLiveness
        return pc ∈ x.pcs
    end
end
function isempty(x::Liveness)
    @nospecialize x
    if x === ⊥ₗ
        return true
    elseif x === ⊤ₗ
        return false
    else
        return isempty(x.pcs)
    end
end
function copy(x::Liveness)
    @nospecialize x
    if x isa PCLiveness
        return PCLiveness(copy(x.pcs))
    end
    return x
end
function delete!(x::Liveness, pc::Int)
    @nospecialize x
    if x isa PCLiveness
        delete!(x.pcs, pc)
    end
    return x
end

"""
    x::EscapeInfo

A lattice for escape information, which holds the following properties:
- `x.ReturnEscape::Bool`: indicates `x` can escape to the caller via return
- `x.ThrownEscape::BitSet`: records SSA statement numbers where `x` can be thrown as exception:
  * `isempty(x.ThrownEscape)`: `x` will never be thrown in this call frame (the bottom)
  * `pc ∈ x.ThrownEscape`: `x` may be thrown at the SSA statement at `pc`
  * `-1 ∈ x.ThrownEscape`: `x` may be thrown at arbitrary points of this call frame (the top)
  This information will be used by `escape_exception!` to propagate potential escapes via exception.
- `x.ObjectInfo::ObjectInfo`: maintains all possible values
  that can be aliased to fields or array elements of `x`:
  * `x.ObjectInfo::HasUnanalyzedMemory` indicates the fields/elements of `x` aren't analyzed yet
  * `x.ObjectInfo::HasUnknownMemory` indicates the fields/elements of `x` can't be analyzed,
    e.g. the type of `x` is not known or is not concrete and thus its fields/elements
    can't be known precisely
  * `x.ObjectInfo::HasIndexableFields` records all the possible values that can be aliased to fields of object `x` with precise index information
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
- the other states are initialized as `NoEscape()`
"""
struct EscapeInfo
    ReturnEscape::Bool
    ThrownEscape::Bool
    ObjectInfo::ObjectInfo
    Liveness::Liveness

    function EscapeInfo(
        ReturnEscape::Bool,
        ThrownEscape::Bool,
        ObjectInfo::ObjectInfo,
        Liveness::Liveness)
        return new(
            ReturnEscape,
            ThrownEscape,
            ObjectInfo,
            Liveness)
    end
    function EscapeInfo(
        x::EscapeInfo;
        ReturnEscape::Bool = x.ReturnEscape,
        ThrownEscape::Bool = x.ThrownEscape,
        ObjectInfo::ObjectInfo = x.ObjectInfo,
        Liveness::Liveness = x.Liveness)
        return new(
            ReturnEscape,
            ThrownEscape,
            ObjectInfo,
            Liveness)
    end
end

function copy(x::EscapeInfo)
    return EscapeInfo(
        x.ReturnEscape,
        x.ThrownEscape,
        copy(x.ObjectInfo),
        copy(x.Liveness))
end

ArgLiveness() = PCLiveness(BitSet(0))

NoEscape() = EscapeInfo(false, false, ⊥ₒ, ⊥ₗ)
ArgEscape(x::ObjectInfo=⊤ₒ) = EscapeInfo(false, false, x, ArgLiveness())
CallerEscape() = EscapeInfo(false, false, ⊤ₒ, ArgLiveness()) # COMBAK allow only one-level inter-procedural alias analysis (for now)
AllEscape() = EscapeInfo(true, true, ⊤ₒ, ⊤ₗ)

const ⊥, ⊤ = NoEscape(), AllEscape()

# Convenience names for some ⊑ₑ queries
function is_not_analyzed(x::EscapeInfo)
    if x.Liveness === BotLiveness()
        @assert !x.ThrownEscape && !x.ReturnEscape && x.ObjectInfo === HasUnanalyzedMemory()
        return true
    else
        return false
    end
end
has_no_escape(x::EscapeInfo) = !x.ReturnEscape && !x.ThrownEscape && 0 ∉ x.Liveness
has_arg_escape(x::EscapeInfo) = 0 ∈ x.Liveness
has_return_escape(x::EscapeInfo) = x.ReturnEscape
has_return_escape(x::EscapeInfo, pc::Int) = x.ReturnEscape && pc ∈ x.Liveness
has_thrown_escape(x::EscapeInfo) = x.ThrownEscape
function has_all_escape(x::EscapeInfo)
    if x.Liveness === TopLiveness() # top-liveness == this object may exist anywhere
        @assert x.ThrownEscape && x.ReturnEscape && x.ObjectInfo === HasUnknownMemory()
        return true
    else
        return false
    end
end

# utility lattice constructors
ignore_argescape(x::EscapeInfo) = EscapeInfo(x; Liveness=delete!(copy(x.Liveness), 0))
ignore_thrownescapes(x::EscapeInfo) = EscapeInfo(x; ThrownEscape=false)
ignore_objectinfo(x::EscapeInfo) = EscapeInfo(x; ObjectInfo=⊥ₒ)
ignore_liveness(x::EscapeInfo) = EscapeInfo(x; Liveness=⊥ₗ)

# we need to make sure this `==` operator corresponds to lattice equality rather than object equality,
# otherwise `propagate_changes` can't detect the convergence
x::EscapeInfo == y::EscapeInfo = begin
    # fast pass: better to avoid top comparison
    x === y && return true
    x.ReturnEscape === y.ReturnEscape || return false
    x.ThrownEscape === y.ThrownEscape || return false
    x.ObjectInfo == y.ObjectInfo || return false
    x.Liveness == y.Liveness || return false
    return true
end

"""
    x::EscapeInfo ⊑ₑ y::EscapeInfo -> Bool

The non-strict partial order over [`EscapeInfo`](@ref).
"""
x::EscapeInfo ⊑ₑ y::EscapeInfo = begin
    @nospecialize
    # fast pass: better to avoid top comparison
    if x === ⊥
        return true
    elseif x === ⊤
        return y === ⊤
    elseif y === ⊥
        return false
    elseif y === ⊤
        return true
    end
    x.ReturnEscape ≤ y.ReturnEscape || return false
    x.ThrownEscape ≤ y.ThrownEscape || return false
    x.ObjectInfo ⊑ₒ y.ObjectInfo || return false
    x.Liveness ⊑ₗ y.Liveness || return false
    return true
end

x::Liveness ⊑ₗ y::Liveness = begin
    @nospecialize
    if x === ⊥ₗ
        return true
    elseif x === ⊤ₗ
        return y === ⊤ₗ
    elseif y === ⊥ₗ
        return false
    elseif y === ⊤ₗ
        return true
    else
        x, y = x::PCLiveness, y::PCLiveness
        return x.pcs ⊆ y.pcs
    end
end

x::ObjectInfo ⊑ₒ y::ObjectInfo = begin
    @nospecialize
    if x === ⊥ₒ
        return true
    elseif x === ⊤ₒ
        return y === ⊤ₒ
    elseif y === ⊥ₒ
        return false
    elseif y === ⊤ₒ
        return true
    elseif x isa HasIndexableCallerFields
        if y isa HasIndexableCallerFields
            xcallers, ycallers = x.caller_memory_list, y.caller_memory_list
            xn, yn = length(xcallers), length(ycallers)
            xn ≤ yn || return false
            for i in 1:xn
                xcallerᵢ = xcallers[i]
                ycallerᵢ = ycallers[i]
                if !(xcallerᵢ isa IdSet{CallerMemory})
                    if !(ycallerᵢ isa IdSet{CallerMemory})
                        xcallerᵢ == ycallerᵢ || return false
                    else
                        xcallerᵢ ∈ ycallerᵢ || return false
                    end
                else
                    if !(ycallerᵢ isa IdSet{CallerMemory})
                        return false
                    else
                        xcallerᵢ ⊆ ycallerᵢ || return false
                    end
                end
            end
            xfields, yfields = x.fields, y.fields
            @goto compare_xfields_yfields
        else
            return false
        end
    else
        x = x::HasIndexableFields
        if y isa HasIndexableCallerFields
            xfields, yfields = x.fields, y.fields
            @goto compare_xfields_yfields
        else
            y = y::HasIndexableFields
            xfields, yfields = x.fields, y.fields
            @label compare_xfields_yfields
            xn, yn = length(xfields), length(yfields)
            xn ≤ yn || return false
            for i in 1:xn
                xfields[i] ⊑ₘ yfields[i] || return false
            end
            return true
        end
    end
end

x::MemoryInfo ⊑ₘ y::MemoryInfo = begin
    @nospecialize
    if x isa MustAliasMemoryInfo
        if y isa MustAliasMemoryInfo
            return x.alias === y.alias
        else
            y = y::MayAliasMemoryInfo
            return x.alias ∈ y.aliases
        end
    else
        x = x::MayAliasMemoryInfo
        if y isa MustAliasMemoryInfo
            return false
        else
            y = y::MayAliasMemoryInfo
            return x.aliases ⊆ y.aliases
        end
    end
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
    x::EscapeInfo ⊔ₑꜝ y::EscapeInfo -> (xy::EscapeInfo, changed::Bool)

Computes the join of `x` and `y` in the partial order defined by [`EscapeInfo`](@ref).
This operation is destructive and modifies `x` in place.
`changed` indicates whether the join result is different from `x`.
"""
x::EscapeInfo ⊔ₑꜝ y::EscapeInfo = begin
    # fast pass: better to avoid top join
    if x === ⊥
        if y === ⊥
            return ⊥, false
        else
            return copy(y), true
        end
    elseif x === ⊤
        return ⊤, false
    elseif y === ⊥
        return x, false
    elseif y === ⊤
        return ⊤, true # x !== ⊤
    end
    changed = false
    ReturnEscape = x.ReturnEscape | y.ReturnEscape
    changed |= x.ReturnEscape < ReturnEscape
    ThrownEscape = x.ThrownEscape | y.ThrownEscape
    changed |= x.ThrownEscape < y.ThrownEscape
    ObjectInfo, changed′ = x.ObjectInfo ⊔ₒꜝ y.ObjectInfo
    changed |= changed′
    Liveness, changed′ = x.Liveness ⊔ₗꜝ y.Liveness
    changed |= changed′
    xy = EscapeInfo(
        ReturnEscape,
        ThrownEscape,
        ObjectInfo,
        Liveness)
    return xy, changed
end
x::EscapeInfo ⊔ₑ y::EscapeInfo = first(copy(x) ⊔ₑꜝ y)

x::Liveness ⊔ₗꜝ y::Liveness = begin
    @nospecialize
    if x === ⊥ₗ
        if y === ⊥ₗ
            return ⊥ₗ, false
        else
            return copy(y), true
        end
    elseif x === ⊤ₗ
        return ⊤ₗ, false
    elseif y === ⊥ₗ
        return x, false
    elseif y === ⊤ₗ
        return ⊤ₗ, true # x !== ⊤ₗ
    end
    x, y = x::PCLiveness, y::PCLiveness
    if x.pcs ⊇ y.pcs
        return x, false
    end
    union!(x.pcs, y.pcs)
    return x, true
end
x::Liveness ⊔ₗ y::Liveness = (@nospecialize; first(copy(x) ⊔ₗꜝ y))

x::ObjectInfo ⊔ₒꜝ y::ObjectInfo = begin
    @nospecialize
    if x === ⊥ₒ
        if y === ⊥ₒ
            return ⊥ₒ, false
        else
            return copy(y), true
        end
    elseif x === ⊤ₒ
        return ⊤ₒ, false
    elseif y === ⊥ₒ
        return x, false
    elseif y === ⊤ₒ
        return ⊤ₒ, true # x !== ⊤ₒ
    elseif x isa HasIndexableCallerFields
        if y isa HasIndexableCallerFields
            xfields, yfields = x.fields, y.fields
            xcallers, ycallers = x.caller_memory_list, y.caller_memory_list
            xn, yn = length(xfields), length(yfields)
            nmax, nmin = max(xn, yn), min(xn, yn)
            changed = false
            if xn < nmax
                resize!(xfields, nmax)
                resize!(xcallers, nmax)
                changed |= true
            end
            for i in 1:nmax
                if nmin < i
                    if xn < nmax
                        xcallers[i] = copy(ycallers[i])
                    end
                else
                    xcallerᵢ, ycallerᵢ = xcallers[i], ycallers[i]
                    if !(xcallerᵢ isa IdSet{CallerMemory})
                        if !(ycallerᵢ isa IdSet{CallerMemory})
                            if xcallerᵢ ≠ ycallerᵢ
                                xcallers[i] = IdSet{CallerMemory}()
                                push!(xcallers[i], xcallerᵢ)
                                push!(xcallers[i], ycallerᵢ)
                                changed |= true
                            end
                        else
                            if xcallerᵢ ∉ ycallerᵢ
                                xcallers[i] = push!(copy(ycallerᵢ), xcallerᵢ)
                                changed |= true
                            end
                        end
                    else
                        if !(ycallerᵢ isa IdSet{CallerMemory})
                            if xcallerᵢ ∌ ycallerᵢ
                                push!(xcallerᵢ, ycallerᵢ)
                                changed |= true
                            end
                        else
                            if xcallerᵢ ⊋ ycallerᵢ
                                union!(xcallerᵢ, ycallerᵢ)
                                changed |= true
                            end
                        end
                    end
                end
            end
            @goto merge_xfields_yfields
        else
            y = y::HasIndexableFields
            xfields, yfields = x.fields, y.fields
            xcallers = x.caller_memory_list
            xn, yn = length(xfields), length(yfields)
            nmax, nmin = max(xn, yn), min(xn, yn)
            if xn < nmax
                resize!(xfields, nmax)
                resize!(xcallers, nmax)
                for i = xn+1:nmax
                    xcallers[i] = IdSet{CallerMemory}()
                end
            end
            changed = true
            @goto merge_xfields_yfields
        end
    else
        x = x::HasIndexableFields
        if y isa HasIndexableCallerFields
            xfields, yfields = x.fields, y.fields
            ycallers = copy(y.caller_memory_list)
            xn, yn = length(xfields), length(yfields)
            nmax, nmin = max(xn, yn), min(xn, yn)
            if xn < nmax
                resize!(xfields, nmax)
            elseif yn < nmax
                resize!(ycallers, nmax)
                for i = yn+1:nmax
                    ycallers[i] = IdSet{CallerMemory}()
                end
            end
            x = HasIndexableCallerFields(xfields, ycallers)
            changed = true
            @goto merge_xfields_yfields
        else
            y = y::HasIndexableFields
            xfields, yfields = x.fields, y.fields
            xn, yn = length(xfields), length(yfields)
            nmax, nmin = max(xn, yn), min(xn, yn)
            changed = false
            if xn < nmax
                resize!(xfields, nmax)
                changed |= true
            end
            @label merge_xfields_yfields
            for i in 1:nmax
                if nmin < i
                    if xn < nmax
                        xfields[i] = copy(yfields[i])
                    end
                else
                    xfields[i], changed′ = xfields[i] ⊔ₘꜝ yfields[i]
                    changed |= changed′
                end
            end
            return x, changed
        end
    end
end
x::ObjectInfo ⊔ₒ y::ObjectInfo = (@nospecialize; first(copy(x) ⊔ₒꜝ y))

x::MemoryInfo ⊔ₘꜝ y::MemoryInfo = begin
    @nospecialize
    if x isa MustAliasMemoryInfo
        if y isa MustAliasMemoryInfo
            if x.alias !== y.alias
                # TODO [^CFG-aware `MemoryInfo`]
                aliases = IdSet{Any}()
                push!(aliases, x.alias)
                push!(aliases, y.alias)
                return MayAliasMemoryInfo(aliases), true
            else
                return x, false
            end
        else
            y = y::MayAliasMemoryInfo
            if x.alias ∈ y.aliases
                return copy(y), true
            else
                x′ = copy(y)
                push!(x′.aliases, x.alias)
                return x′, true
            end
        end
    else
        x = x::MayAliasMemoryInfo
        if y isa MustAliasMemoryInfo
            if x.aliases ∋ y.alias
                return x, false
            else
                push!(x.aliases, y.alias)
                return x, true
            end
        else
            y = y::MayAliasMemoryInfo
            if x.aliases ⊇ y.aliases
                return x, false
            else
                union!(x.aliases, y.aliases)
                return x, true
            end
        end
    end
end
x::MemoryInfo ⊔ₘ y::MemoryInfo = (@nospecialize; first(copy(x) ⊔ₘꜝ y))

const EscapeTable = IdDict{Int,EscapeInfo} # TODO `Dict` would be more efficient?

struct AnalysisFrameInfo
    nargs::Int
    nstmts::Int
    caller_memory_map::Vector{CallerMemory}
end

struct BlockEscapeState
    escapes::EscapeTable
    afinfo::AnalysisFrameInfo
end

function getindex(bbstate::BlockEscapeState, @nospecialize(x))
    xidx = iridx(x, bbstate)
    return xidx === nothing ? nothing : bbstate[xidx]
end
function getindex(bbstate::BlockEscapeState, xidx::Int)
    return get(bbstate.escapes, xidx) do
        (; nargs, nstmts) = bbstate.afinfo
        if xidx ≤ nargs
            return ArgEscape()
        elseif nargs < xidx ≤ nargs + nstmts
            return ⊥
        else
            error("The escape information for `CallerMemory` should be initialized in the `AnalysisState` constructor")
        end
    end
end
function setindex!(bbstate::BlockEscapeState, xinfo::EscapeInfo, @nospecialize(x))
    xidx = iridx(x, bbstate)
    if xidx !== nothing
        bbstate[xidx] = xinfo
    end
    return bbstate
end
function setindex!(bbstate::BlockEscapeState, xinfo::EscapeInfo, xidx::Int)
    return bbstate.escapes[xidx] = xinfo
end
function copy(bbstate::BlockEscapeState)
    escapes = EscapeTable(i => copy(x) for (i, x) in bbstate.escapes)
    return BlockEscapeState(escapes, bbstate.afinfo)
end
bbstate1::BlockEscapeState == bbstate2::BlockEscapeState =
    bbstate1.escapes == bbstate2.escapes && bbstate1.afinfo === bbstate2.afinfo

const AnalyzableIRElement = Union{Argument,SSAValue,CallerMemory}

"""
    iridx(x, bbstate::BlockEscapeState) -> xidx::Union{Int,Nothing}

Tries to convert analyzable IR element `x::AnalyzableIRElement` to
its unique identifier number `xidx` that is valid in the analysis context of `bbstate`.
Returns `nothing` if `x` isn't maintained by `bbstate` and thus unanalyzable (e.g. `x::GlobalRef`).

`irval` is the inverse function of `iridx` (not formally), i.e.
`irval(iridx(x::AnalyzableIRElement, state), state) === x`.
"""
iridx(@nospecialize(x), bbstate::BlockEscapeState) = iridx(x, bbstate.afinfo)
function iridx(@nospecialize(x), afinfo::AnalysisFrameInfo)
    (; nargs, nstmts) = afinfo
    if isa(x, Argument)
        xidx = x.n
        @assert 1 ≤ xidx ≤ nargs "invalid Argument"
    elseif isa(x, SSAValue)
        xidx = x.id + nargs
        @assert nargs < xidx ≤ nargs + nstmts "invalid SSAValue"
    elseif isa(x, CallerMemory)
        xidx = nargs + nstmts + x.id
        @assert nargs + nstmts < xidx ≤ nargs + nstmts + length(afinfo.caller_memory_map) "invalid CallerMemory"
    else
        return nothing
    end
    return xidx
end

"""
    irval(xidx::Int, bbstate::BlockEscapeState) -> x::AnalyzableIRElement

Converts its unique identifier number `xidx` to the original IR element `x::AnalyzableIRElement`
that is analyzable in the context of `bbstate`.

`iridx` is the inverse function of `irval` (not formally), i.e.
`iridx(irval(xidx, state), state) === xidx`.
"""
irval(xidx::Int, bbstate::BlockEscapeState) = irval(xidx, bbstate.afinfo)
function irval(xidx::Int, afinfo::AnalysisFrameInfo)
    (; nargs, nstmts) = afinfo
    if xidx ≤ nargs
        return Argument(xidx)
    elseif xidx ≤ nargs + nstmts
        return SSAValue(xidx - nargs)
    elseif nargs + nstmts < xidx ≤ nargs + nstmts + length(afinfo.caller_memory_map)
        return afinfo.caller_memory_map[xidx - nargs - nstmts]
    else
        error("invalid xidx::Int")
    end
end

abstract type Change end
const Changes = Vector{Change}

const SSAMemoryInfo = IdDict{Int,MemoryInfo}

const LINEAR_BBESCAPES = Union{Bool,BlockEscapeState}[false]

const AliasSet = IntDisjointSet{Int}

struct AnalysisState{GetEscapeCache}
    ir::IRCode
    afinfo::AnalysisFrameInfo
    new_nodes_map::Union{Nothing,IdDict{Int,Vector{Pair{Int,NewNodeInfo}}}}
    # escape states for each basic block:
    # - `bbescape === false` indicates the state for the block has not been initialized
    # - `bbescape === true` indicates the state for the block is known to be identical to
    #   the state of its single predecessor
    bbescapes::Vector{Union{Bool,BlockEscapeState}}
    aliasset::AliasSet
    get_escape_cache::GetEscapeCache
    #= results =#
    retescape::BlockEscapeState
    ssamemoryinfo::SSAMemoryInfo
    caller_memory_map::Vector{CallerMemory}
    #= temporary states =#
    currstate::BlockEscapeState                 # the state currently being analyzed
    changes::Changes                            # changes made at the current statement
    visited::BitSet
    equalized_roots::BitSet
    handler_info::Union{Nothing,HandlerInfo{SimpleHandler}}
end
function AnalysisState(ir::IRCode, nargs::Int, get_escape_cache)
    if isempty(ir.new_nodes)
        new_nodes_map = nothing
    else
        new_nodes_map = IdDict{Int,Vector{Pair{Int,NewNodeInfo}}}()
        for (i, nni) in enumerate(ir.new_nodes.info)
            if haskey(new_nodes_map, nni.pos)
                push!(new_nodes_map[nni.pos], i => nni)
            else
                new_nodes_map[nni.pos] = Pair{Int,NewNodeInfo}[i => nni]
            end
        end
    end
    nbbs = length(ir.cfg.blocks)
    nstmts = length(ir.stmts) + length(ir.new_nodes)
    caller_memory_map = CallerMemory[]
    currtable = EscapeTable()
    aliasset = AliasSet(nargs + nstmts)
    for argn = 1:nargs
        object_info = initialize_argument_object_info!(
            caller_memory_map, currtable, aliasset, argn, ir, nargs, nstmts)
        currtable[argn] = ArgEscape(object_info)
    end
    afinfo = AnalysisFrameInfo(nargs, nstmts, caller_memory_map)
    if nbbs == 1 # optimization for linear control flow
        bbescapes = LINEAR_BBESCAPES # avoid unnecessary allocation
        retescape = currstate = BlockEscapeState(currtable, afinfo) # no need to maintain a separate state
    else
        bbescapes = fill!(Vector{Union{Bool,BlockEscapeState}}(undef, nbbs), false)
        retescape = BlockEscapeState(EscapeTable(), afinfo)
        currstate = BlockEscapeState(currtable, afinfo)
    end
    retescape[0] = ⊥
    ssamemoryinfo = SSAMemoryInfo()
    changes = Changes()
    visited = BitSet()
    equalized_roots = BitSet()
    handler_info = compute_trycatch(ir)
    return AnalysisState(
        ir, afinfo, new_nodes_map,
        bbescapes, aliasset, get_escape_cache,
        retescape, ssamemoryinfo, caller_memory_map,
        currstate, changes, visited, equalized_roots, handler_info)
end

# COMBAK allow only one-level inter-procedural alias analysis (for now)

function initialize_argument_object_info!(
    caller_memory_map::Vector{CallerMemory}, currtable::EscapeTable, aliasset::AliasSet,
    argn::Int, ir::IRCode, nargs::Int, nstmts::Int)
    arg = Argument(argn)
    argtyp = argextype(arg, ir)
    is_identity_free_argtype(argtyp) && return ⊥ₒ
    typ = argument_datatype(argtyp)
    typ isa DataType || return ⊤ₒ
    nflds = fieldcount_noerror(typ)
    if nflds === nothing
        return ⊤ₒ
    end
    fields = FieldInfos(undef, nflds)
    caller_memory_list = CallerMemoryList(undef, nflds)
    nmin = datatype_min_ninitialized(typ)
    for fidx = 1:nflds
        caller_memory = add_caller_memory!(
            caller_memory_map, currtable, aliasset,
            arg, fidx, nargs, nstmts)
        if fidx > nmin # maybe undef
            aliases = IdSet{Any}()
            push!(aliases, caller_memory)
            push!(aliases, UninitializedMemory())
            memory_info = MayAliasMemoryInfo(aliases)
        else
            memory_info = MustAliasMemoryInfo(caller_memory)
        end
        fields[fidx] = memory_info
        caller_memory_list[fidx] = caller_memory
    end
    # return HasIndexableCallerFields(fields, caller_memory_list)
    return HasIndexableFields(fields)
end

function add_caller_memory!(
    caller_memory_map::Vector{CallerMemory}, currtable::EscapeTable, aliasset::AliasSet,
    arg::Argument, fidx::Int, nargs::Int, nstmts::Int)
    id = length(caller_memory_map) + 1
    cidx = nargs + nstmts + id
    currtable[cidx] = CallerEscape()
    aidx = push!(aliasset)
    @assert cidx == aidx
    caller_memory = CallerMemory(id, arg, fidx)
    push!(caller_memory_map, caller_memory)
    return caller_memory
end

function getaliases(aliasset::AliasSet, afinfo::AnalysisFrameInfo, x::AnalyzableIRElement)
    aliases = getaliases(aliasset, iridx(x, afinfo))
    aliases === nothing && return nothing
    return (irval(aidx, afinfo) for aidx in aliases)
end
function getaliases(aliasset::AliasSet, xidx::Int)
    xroot, hasalias = getaliasroot!(aliasset, xidx)
    if hasalias
        # the size of this alias set containing `key` is larger than 1,
        # collect the entire alias set
        return (aidx for aidx in 1:length(aliasset.parents)
            if _find_root_impl!(aliasset.parents, aidx) == xroot)
    else
        return nothing
    end
end
@inline function getaliasroot!(aliasset::AliasSet, xidx::Int)
    root = find_root!(aliasset, xidx)
    if xidx ≠ root || aliasset.ranks[xidx] > 0
        return root, true
    else
        return root, false
    end
end
getaliases(astate::AnalysisState, x::AnalyzableIRElement) = getaliases(astate.aliasset, astate.afinfo, x)
getaliases(astate::AnalysisState, xidx::Int) = getaliases(astate.aliasset, xidx)

isaliased(aliasset::AliasSet, afinfo::AnalysisFrameInfo, x::AnalyzableIRElement, y::AnalyzableIRElement) = isaliased(aliasset, iridx(x, afinfo), iridx(y, afinfo))
isaliased(aliasset::AliasSet, xidx::Int, yidx::Int) = in_same_set(aliasset, xidx, yidx)
isaliased(astate::AnalysisState, x::AnalyzableIRElement, y::AnalyzableIRElement) = isaliased(astate.aliasset, astate.afinfo, x, y)
isaliased(astate::AnalysisState, xidx::Int, yidx::Int) = isaliased(astate.aliasset, xidx, yidx)

"""
    eresult::EscapeResult

Extended lattice that maps arguments and SSA values to escape information represented as [`EscapeInfo`](@ref).
Escape information imposed on SSA IR element `x` can be retrieved by `eresult[x]`.
"""
struct EscapeResult
    afinfo::AnalysisFrameInfo
    bbescapes::Vector{Union{Bool,BlockEscapeState}}
    aliasset::AliasSet
    retescape::BlockEscapeState
    ssamemoryinfo::SSAMemoryInfo
    caller_memory_map::Vector{CallerMemory}
    function EscapeResult(astate::AnalysisState)
        return new(astate.afinfo, astate.bbescapes, astate.aliasset, astate.retescape,
            astate.ssamemoryinfo, astate.caller_memory_map)
    end
end
iridx(x::AnalyzableIRElement, eresult::EscapeResult) = iridx(x, eresult.afinfo)
irval(xidx::Int, eresult::EscapeResult) = irval(xidx, eresult.afinfo)
getindex(eresult::EscapeResult, @nospecialize(x)) = getindex(eresult.retescape, x)
getaliases(eresult::EscapeResult, x::AnalyzableIRElement) = getaliases(eresult.aliasset, eresult.afinfo, x)
getaliases(eresult::EscapeResult, xidx::Int) = getaliases(eresult.aliasset, xidx)
isaliased(eresult::EscapeResult, x::AnalyzableIRElement, y::AnalyzableIRElement) = isaliased(eresult.aliasset, eresult.afinfo, x, y)
isaliased(eresult::EscapeResult, xidx::Int, yidx::Int) = isaliased(eresult.aliasset, xidx, yidx)

function is_load_forwardable((; ssamemoryinfo)::EscapeResult, pc::Int)
    haskey(ssamemoryinfo, pc) || return false
    return ssamemoryinfo[pc] isa MustAliasMemoryInfo
end

"""
    analyze_escapes(ir::IRCode, nargs::Int, get_escape_cache) -> eresult::EscapeResult

Analyzes escape information in `ir`:
- `nargs`: the number of actual arguments of the analyzed call
- `get_escape_cache(::MethodInstance) -> Union{Bool,EscapeCache}`:
  retrieves cached argument escape information
"""
function analyze_escapes(ir::IRCode, nargs::Int, get_escape_cache)
    currbb = 1
    bbs = ir.cfg.blocks
    W = BitSet()
    W.offset = 0 # for _bits_findnext
    astate = AnalysisState(ir, nargs, get_escape_cache)
    (; new_nodes_map, currstate) = astate

    while true
        local nextbb::Int, nextcurrbb::Int
        bbstart, bbend = first(bbs[currbb].stmts), last(bbs[currbb].stmts)
        for pc = bbstart:bbend
            local new_nodes_counter::Int = 0
            if new_nodes_map === nothing || pc ∉ keys(new_nodes_map)
                stmt = ir[SSAValue(pc)][:stmt]
                isterminator = pc == bbend
            else
                new_nodes = new_nodes_map[pc]
                attach_before_idxs = Int[i for (i, nni) in new_nodes if !nni.attach_after]
                attach_after_idxs  = Int[i for (i, nni) in new_nodes if nni.attach_after]
                na, nb = length(attach_after_idxs), length(attach_before_idxs)
                n_nodes = new_nodes_counter = na + nb + 1 # +1 for this statement
                @label analyze_new_node
                curridx = n_nodes - new_nodes_counter + 1
                if curridx ≤ nb
                    stmt = ir.new_nodes.stmts[attach_before_idxs[curridx]][:stmt]
                elseif curridx == nb + 1
                    stmt = ir[SSAValue(pc)][:stmt]
                else
                    @assert curridx ≤ n_nodes
                    stmt = ir.new_nodes.stmts[attach_after_idxs[curridx - nb - 1]][:stmt]
                end
                isterminator = curridx == n_nodes
                new_nodes_counter -= 1
            end

            if isterminator
                # if this is the last statement of the current block, handle the control-flow
                if stmt isa GotoNode
                    succs = bbs[currbb].succs
                    @assert length(succs) == 1 "GotoNode with multiple successors"
                    nextbb = only(succs)
                    @goto propagate_state
                elseif stmt isa GotoIfNot
                    condval = stmt.cond
                    if condval === true
                        @goto fall_through
                    elseif condval === false
                        nextbb = falsebb
                        @goto propagate_state
                    else
                        succs = bbs[currbb].succs
                        if length(succs) == 1
                            nextbb = only(succs)
                            @assert stmt.dest == nextbb + 1 "Invalid GotoIfNot"
                            @goto propagate_state
                        end
                        @assert length(succs) == 2 "GotoIfNot with ≥2 successors"
                        truebb = currbb + 1
                        falsebb = succs[1] == truebb ? succs[2] : succs[1]
                        falseret = propagate_bbstate!(astate, currstate, falsebb, #=allow_direct_propagation=#!(@isdefined nextcurrbb))
                        if falseret === nothing
                            @assert currbb == only(bbs[falsebb].preds)
                            nextcurrbb = falsebb
                        elseif falseret
                            push!(W, falsebb)
                        end
                        @goto fall_through
                    end
                elseif stmt isa ReturnNode
                    if isdefined(stmt, :val)
                        add_return_escape_change!(astate, stmt.val)
                        if !isempty(astate.changes)
                            apply_changes!(astate, pc)
                            empty!(astate.changes)
                        end
                    end
                    if length(bbs) == 1 # see the constructor of `AnalysisState`
                        @assert astate.retescape === currstate # `astate.retescape` has been updated in-place
                    else
                        propagate_ret_state!(astate, currstate)
                    end
                    if isdefined(stmt, :val)
                        # Accumulate the escape information of the return value
                        # so that it can be expanded in the caller context.
                        retval = stmt.val
                        if retval isa GlobalRef
                            with_profitable_irval(astate, retval) do _::Int
                                astate.retescape[0] = ⊤
                            end
                        elseif retval isa SSAValue || retval isa Argument
                            retinfo = currstate[retval]
                            newrinfo, changed = astate.retescape[0] ⊔ₑꜝ retinfo
                            if changed
                                astate.retescape[0] = newrinfo
                            end
                        end
                    end
                    @goto next_bb
                elseif stmt isa EnterNode
                    @goto fall_through
                elseif isexpr(stmt, :leave)
                    @goto fall_through
                end
                # fall through terminator – treat as a regular statement
            end

            # process non control-flow statements
            analyze_stmt!(astate, pc, stmt)
            if !isempty(astate.changes)
                excstate_excbb = apply_changes!(astate, pc)
                empty!(astate.changes)
                if excstate_excbb !== nothing
                    # propagate the escape information of this block to the exception handler block
                    excstate, excbb = excstate_excbb
                    if propagate_bbstate!(astate, excstate, excbb)::Bool
                        push!(W, excbb)
                    end
                end
            else
                # propagate the escape information of this block to the exception handler block
                # even if there are no changes made on this statement
                if !is_nothrow(ir, pc)
                    curr_hand = gethandler(astate.handler_info, pc)
                    if curr_hand !== nothing
                        enter_node = ir[SSAValue(curr_hand.enter_idx)][:stmt]::EnterNode
                        if propagate_bbstate!(astate, currstate, enter_node.catch_dest)::Bool
                            push!(W, enter_node.catch_dest)
                        end
                    end
                end
            end

            if new_nodes_counter > 0
                @goto analyze_new_node
            end
        end

        begin @label fall_through
            nextbb = currbb + 1
        end

        begin @label propagate_state
            ret = propagate_bbstate!(astate, currstate, nextbb, #=allow_direct_propagation=#!(@isdefined nextcurrbb))
            if ret === nothing
                @assert currbb == only(bbs[nextbb].preds)
                nextcurrbb = nextbb
            elseif ret
                push!(W, nextbb)
            end
        end

        begin @label next_bb
            if !(@isdefined nextcurrbb)
                nextcurrbb = _bits_findnext(W.bits, 1)
                nextcurrbb == -1 && break # the working set is empty
                delete!(W, nextcurrbb)
                nextcurrstate = astate.bbescapes[nextcurrbb]
                if nextcurrstate isa Bool
                    @assert nextcurrstate === false
                    empty!(currstate.escapes) # initialize the state
                else
                    copy!(currstate.escapes, nextcurrstate.escapes) # overwrite the state
                end
            else
                # propagate the current state to the next block directly
            end
            currbb = nextcurrbb
        end
    end

    return EscapeResult(astate)
end

function propagate_bbstate!(astate::AnalysisState, currstate::BlockEscapeState, nextbbidx::Int,
                            allow_direct_propagation::Bool=false)
    bbescapes = astate.bbescapes
    nextstate = bbescapes[nextbbidx]
    if nextstate isa Bool
        nextbb = astate.ir.cfg.blocks[nextbbidx]
        if allow_direct_propagation && length(nextbb.stmts) == 1 && length(nextbb.preds) == 1
            # Performance optimization:
            # If the next block has a single predecessor (the current block) and it has a
            # single statament which is a non-returning terminator, then it can simply
            # propagate the current state to the subsequent block(s). This is valid because
            # it does not update the escape information at all, and even if the state is
            # updated during the further iterations of abstract interpretation, the updated
            # state is always propagated from the predecessor.
            # In such cases, set `bbescapes[nextbbidx] = true` to explicitly indicate that
            # the state for this next block is identical to the state of the predecessor,
            # avoiding unnecessary copying of the state.
            nextpc = only(nextbb.stmts)
            new_nodes_map = astate.new_nodes_map
            if new_nodes_map === nothing || nextpc ∉ keys(new_nodes_map)
                nextstmt = astate.ir[SSAValue(nextpc)][:stmt]
                if is_stmt_escape_free(nextstmt)
                    bbescapes[nextbbidx] = true
                    return nothing
                end
            end
        end
        bbescapes[nextbbidx] = copy(currstate)
        return true
    else
        return propagate_bbstate!(nextstate, currstate)
    end
end

# NOTE we can't include `ReturnNode` here since it may add `ReturnEscape` change
is_stmt_escape_free(@nospecialize(stmt)) =
    stmt === nothing || stmt isa GotoNode || stmt isa Core.GotoIfNot ||
    stmt isa EnterNode || isexpr(stmt, :leave)

function propagate_bbstate!(nextstate::BlockEscapeState, currstate::BlockEscapeState)
    anychanged = false
    for (idx, newinfo) in currstate.escapes
        if haskey(nextstate.escapes, idx)
            oldinfo = nextstate.escapes[idx]
            newnewinfo, changed = oldinfo ⊔ₑꜝ newinfo
            if changed
                nextstate.escapes[idx] = newnewinfo
                anychanged |= true
            end
        else
            nextstate.escapes[idx] = copy(newinfo)
            anychanged |= true
        end
    end
    return anychanged
end

propagate_ret_state!(astate::AnalysisState, currstate::BlockEscapeState) =
    propagate_bbstate!(astate.retescape, currstate)

# Apply
# =====
# Apply `Change`s and update the current escape information `currstate`

struct AllEscapeChange <: Change
    xidx::Int
end
struct ReturnEscapeChange <: Change
    xidx::Int
end
struct ThrownEscapeChange <: Change
    xidx::Int
end
struct ObjectInfoChange <: Change
    xidx::Int
    ObjectInfo::ObjectInfo
end
struct LivenessChange <: Change
    xidx::Int
end
struct AliasChange <: Change
    xidx::Int
    yidx::Int
end

# apply changes, equalize updated escape information across the aliases sets,
# and also return a new escape state for the exception handler block
# if the current statement is inside a `try` block
function apply_changes!(astate::AnalysisState, pc::Int)
    @assert isempty(astate.equalized_roots) "`astate.equalized_roots` should be empty"
    nothrow = is_nothrow(astate.ir, pc)
    curr_hand = gethandler(astate.handler_info, pc)
    for change in astate.changes
        if change isa AllEscapeChange
            apply_all_escape_change!(astate, change)
        elseif change isa ReturnEscapeChange
            apply_return_escape_change!(astate, change)
        elseif change isa ThrownEscapeChange
            @assert !nothrow "ThrownEscapeChange in a nothrow statement"
            if curr_hand !== nothing
                # If there is a handler for this statement, escape information needs to be
                # propagated to the exception handler block via `propagate_exct_state!`
                # after all alias information has been updated.
                # Note that it is not necessary to update the `ThrownEscape` information for
                # this current block for this case. This is because if an object is raised,
                # the control flow will transition to the exception handler block. Otherwise,
                # no exception is raised, thus the `ThrownEscape` information for this
                # statement does not need to be propagated to subsequent statements.
                continue
            else
                apply_thrown_escape_change!(astate, change)
            end
        elseif change isa ObjectInfoChange
            apply_object_info_change!(astate, change)
        elseif change isa LivenessChange
            apply_liveness_change!(astate, change, pc)
        else
            apply_alias_change!(astate, change::AliasChange)
        end
    end
    equalize_aliased_escapes!(astate)
    empty!(astate.equalized_roots)
    if !nothrow && curr_hand !== nothing
        return make_exc_state(astate, curr_hand.enter_idx)
    else
        return nothing
    end
end

# When this statement is inside a `try` block with an associated exception handler,
# escape information needs to be propagated to the `catch` block of the exception handler.
# Additionally, the escape information needs to be updated to reflect the possibility that
# objects raised as the exception might be `catch`ed.
# In the current Julia implementation, mechanisms like `rethrow()` or `Expr(:the_exception)`
# allow access to the exception object from anywhere. Consequently, any objects that might
# be raised as exceptions must be assigned the most conservative escape information.
# As a potential future improvement, if effect analysis or similar techniques can guarantee
# that no function calls capable of returning exception objects
# (e.g., `Base.current_exceptions()`) exist within the `catch` block, we could avoid
# propagating the most conservative escape information. Instead, alias information could be
# added between all objects that might be raised as exceptions and the `:the_exception` of
# the `catch` block.
function make_exc_state(astate::AnalysisState, enter_idx::Int)
    enter_node = astate.ir[SSAValue(enter_idx)][:stmt]::EnterNode
    excstate = copy(astate.currstate)
    # update escape information of any objects that might be raised as exception:
    # currently their escape information is simply widened to `⊤` (the most conservative information)
    for change in astate.changes
        local xidx::Int
        if change isa ThrownEscapeChange
            xidx = change.xidx
        elseif change isa AllEscapeChange
            xidx = change.xidx
        else
            continue
        end
        aliases = getaliases(astate, xidx)
        if aliases !== nothing
            for aidx in aliases
                excstate[aidx] = ⊤
            end
        else
            excstate[xidx] = ⊤
        end
    end
    return excstate, enter_node.catch_dest
end

function apply_all_escape_change!(astate::AnalysisState, change::AllEscapeChange)
    (; xidx) = change
    oldinfo = astate.currstate[xidx]
    newnewinfo, changed = oldinfo ⊔ₑꜝ ⊤
    if changed
        astate.currstate[xidx] = newnewinfo
        record_equalized_root!(astate, xidx)
    end
    nothing
end

function apply_return_escape_change!(astate::AnalysisState, change::ReturnEscapeChange)
    (; xidx) = change
    xinfo = astate.currstate[xidx]
    (; ReturnEscape) = xinfo
    if !ReturnEscape
        astate.currstate[xidx] = EscapeInfo(xinfo; ReturnEscape=true)
        record_equalized_root!(astate, xidx)
    end
end

function apply_thrown_escape_change!(astate::AnalysisState, change::ThrownEscapeChange)
    (; xidx) = change
    xinfo = astate.currstate[xidx]
    (; ThrownEscape) = xinfo
    if !ThrownEscape
        astate.currstate[xidx] = EscapeInfo(xinfo; ThrownEscape=true)
        record_equalized_root!(astate, xidx)
    end
end

function apply_object_info_change!(astate::AnalysisState, change::ObjectInfoChange)
    (; xidx, ObjectInfo) = change
    astate.currstate[xidx] = EscapeInfo(astate.currstate[xidx]; ObjectInfo)
    record_equalized_root!(astate, xidx)
end

function apply_liveness_change!(astate::AnalysisState, change::LivenessChange, pc::Int)
    (; xidx) = change
    xinfo = astate.currstate[xidx]
    (; Liveness) = xinfo
    if Liveness === ⊤ₗ
        return nothing
    elseif Liveness === ⊥ₗ
        astate.currstate[xidx] = EscapeInfo(xinfo; Liveness=PCLiveness(pc))
    else
        Liveness = Liveness::PCLiveness
        if pc ∈ Liveness.pcs
            return nothing
        end
        push!(Liveness.pcs, pc)
    end
    record_equalized_root!(astate, xidx)
end

function apply_alias_change!(astate::AnalysisState, change::AliasChange)
    anychange = false
    (; xidx, yidx) = change
    aliasset = astate.aliasset
    xroot = find_root!(aliasset, xidx)
    yroot = find_root!(aliasset, yidx)
    if xroot ≠ yroot
        xroot = union!(aliasset, xroot, yroot)
        record_equalized_root!(astate, xroot)
        xroot ≠ yroot && record_equalized_root!(astate, yroot)
    end
    nothing
end

function record_equalized_root!(astate::AnalysisState, xidx::Int)
    xroot, hasalias = getaliasroot!(astate.aliasset, xidx)
    if hasalias
        push!(astate.equalized_roots, xroot)
    end
end

function equalize_aliased_escapes!(astate::AnalysisState)
    for xroot in astate.equalized_roots
        equalize_aliased_escapes!(astate, xroot)
    end
end
function equalize_aliased_escapes!(astate::AnalysisState, xroot::Int)
    aliases = getaliases(astate, xroot)
    @assert aliases !== nothing "no aliases found"
    ainfo = ⊥
    for aidx in aliases
        ainfo, _ = ainfo ⊔ₑꜝ astate.currstate[aidx]
    end
    for aidx in aliases
        astate.currstate[aidx] = ainfo
    end
end

# Add
# ===
# Store `Change`s for the current escape state that will be applied later

function add_all_escape_change!(astate::AnalysisState, @nospecialize(x))
    @assert isempty(astate.visited)
    _add_all_escape_change!(astate, x)
    empty!(astate.visited)
end
function _add_all_escape_change!(astate::AnalysisState, @nospecialize(x))
    with_profitable_irval(astate, x) do xidx::Int
        __add_all_escape_change!(astate, xidx)
    end
end
function __add_all_escape_change!(astate::AnalysisState, xidx::Int)
    push!(astate.changes, AllEscapeChange(xidx))
    # Propagate the updated information to the field values of `x`
    traverse_object_memory(astate, xidx) do @nospecialize aval
        _add_all_escape_change!(astate, aval)
    end
end

function add_return_escape_change!(astate::AnalysisState, @nospecialize(x))
    @assert isempty(astate.visited)
    _add_return_escape_change!(astate, x)
    empty!(astate.visited)
end
function _add_return_escape_change!(astate::AnalysisState, @nospecialize(x))
    with_profitable_irval(astate, x) do xidx::Int
        push!(astate.changes, ReturnEscapeChange(xidx))
        push!(astate.changes, LivenessChange(xidx))
        # Propagate the updated information to the field values of `x`
        traverse_object_memory(astate, xidx) do @nospecialize aval
            _add_return_escape_change!(astate, aval)
        end
    end
end

function add_thrown_escape_change!(astate::AnalysisState, @nospecialize(x))
    @assert isempty(astate.visited)
    _add_thrown_escape_change!(astate, x)
    empty!(astate.visited)
end
function _add_thrown_escape_change!(astate::AnalysisState, @nospecialize(x))
    with_profitable_irval(astate, x) do xidx::Int
        push!(astate.changes, ThrownEscapeChange(xidx))
        # Propagate the updated information to the field values of `x`
        traverse_object_memory(astate, xidx) do @nospecialize aval
            _add_thrown_escape_change!(astate, aval)
        end
    end
end

function add_object_info_change!(astate::AnalysisState, @nospecialize(x), ObjectInfo::ObjectInfo)
    with_profitable_irval(astate, x) do xidx::Int
        push!(astate.changes, ObjectInfoChange(xidx, ObjectInfo))
        if ObjectInfo === ⊤ₒ
            # The field values might have been analyzed precisely, but now that is no longer possible:
            # Alias `obj` with its field values so that escape information for `obj` is directly
            # propagated to the field values.
            traverse_object_memory(astate, xidx, #=track_visited=#false) do @nospecialize aval
                _add_alias_change!(astate, xidx, aval)
            end
        end
    end
end

function add_liveness_change!(astate::AnalysisState, @nospecialize(x))
    @assert isempty(astate.visited)
    _add_liveness_change!(astate, x)
    empty!(astate.visited)
end
function _add_liveness_change!(astate::AnalysisState, @nospecialize(x))
    with_profitable_irval(astate, x) do xidx::Int
        push!(astate.changes, LivenessChange(xidx))
        # Propagate the updated information to the field values of `x`
        traverse_object_memory(astate, xidx) do @nospecialize aval
            _add_liveness_change!(astate, aval)
        end
    end
end

function with_profitable_irval(callback, astate::AnalysisState, @nospecialize(x))
    xidx = iridx(x, astate.currstate)
    if xidx !== nothing
        if !is_identity_free_argtype(argextype(x, astate.ir))
            callback(xidx)
        end
    end
    nothing
end

function with_profitable_irvals(callback, astate::AnalysisState, @nospecialize(x), @nospecialize(y))
    xidx = iridx(x, astate.currstate)
    yidx = iridx(y, astate.currstate)
    if xidx !== nothing && yidx !== nothing
        if (!is_identity_free_argtype(argextype(x, astate.ir)) &&
            !is_identity_free_argtype(argextype(y, astate.ir)))
            callback(xidx, yidx)
        end
    end
    nothing
end

function traverse_object_memory(callback, astate::AnalysisState, xidx::Int,
                                track_visited::Bool=true)
    (; ObjectInfo) = astate.currstate[xidx]
    if ObjectInfo isa HasIndexableFields && (!track_visited || xidx ∉ astate.visited)
        track_visited && push!(astate.visited, xidx) # avoid infinite traversal for cyclic references
        for xfinfo in ObjectInfo.fields
            if xfinfo isa MustAliasMemoryInfo
                callback(xfinfo.alias)
            else
                xfinfo = xfinfo::MayAliasMemoryInfo
                for aval in xfinfo.aliases
                    callback(aval)
                end
            end
        end
    end
end

function add_alias_change!(astate::AnalysisState, @nospecialize(x), @nospecialize(y))
    if isa(x, GlobalRef)
        return add_all_escape_change!(astate, y)
    elseif isa(y, GlobalRef)
        return add_all_escape_change!(astate, x)
    end
    with_profitable_irvals(astate, x, y) do xidx::Int, yidx::Int
        if !isaliased(astate, xidx, yidx)
            push!(astate.changes, AliasChange(xidx, yidx))
        end
    end
    return nothing
end

function _add_alias_change!(astate::AnalysisState, xidx::Int, @nospecialize(y))
    if isa(y, GlobalRef)
        return __add_all_escape_change!(astate, xidx)
    end
    with_profitable_irval(astate, y) do yidx::Int
        if !isaliased(astate, xidx, yidx)
            push!(astate.changes, AliasChange(xidx, yidx))
        end
    end
    return nothing
end

function add_liveness_changes!(astate::AnalysisState, args::Vector{Any},
                               first_idx::Int = 1, last_idx::Int = length(args))
    for i in first_idx:last_idx
        arg = args[i]
        add_liveness_change!(astate, arg)
    end
end

function add_fallback_changes!(astate::AnalysisState, args::Vector{Any},
                               first_idx::Int = 1, last_idx::Int = length(args))
    for i in first_idx:last_idx
        arg = args[i]
        add_thrown_escape_change!(astate, arg)
        add_liveness_change!(astate, arg)
    end
end

function add_conservative_changes!(astate::AnalysisState, pc::Int, args::Vector{Any},
                                   first_idx::Int = 1, last_idx::Int = length(args))
    for i in first_idx:last_idx
        add_all_escape_change!(astate, args[i])
    end
    add_all_escape_change!(astate, SSAValue(pc)) # it may return GlobalRef etc.
    return nothing
end

# Analyze
# =======
# Subroutines to analyze the current statement and add `Change`s from it

function analyze_stmt!(astate::AnalysisState, pc::Int, @nospecialize(stmt))
    @assert isempty(astate.changes) "`astate.changes` should have been applied"
    if isa(stmt, Expr)
        head = stmt.head
        if head === :call
            analyze_call!(astate, pc, stmt.args)
        elseif head === :invoke
            analyze_invoke!(astate, pc, stmt.args)
        elseif head === :new || head === :splatnew
            analyze_new!(astate, pc, stmt.args)
        elseif head === :foreigncall
            analyze_foreigncall!(astate, pc, stmt.args)
        elseif is_meta_expr_head(head)
            # meta expressions doesn't account for any usages
        elseif head === :the_exception || head === :pop_exception
            # ignore these expressions since escapes via exceptions are handled by `escape_exception!`:
            # `escape_exception!` conservatively propagates `AllEscape` anyway,
            # and so escape information imposed on `:the_exception` isn't computed
        elseif head === :gc_preserve_begin
            # GC preserve is handled by `escape_gc_preserve!`
        elseif head === :gc_preserve_end
            analyze_gc_preserve!(astate, pc, stmt.args)
        elseif head === :static_parameter ||  # this exists statically, not interested in its escape
               head === :copyast ||           # XXX escape something?
               head === :isdefined ||         # returns `Bool`, nothing accounts for any escapes
               head === :throw_undef_if_not   # may throwx `UndefVarError`, nothing accounts for any escapes
        else
            @assert head !== :leave "Found unexpected IR element"
            add_conservative_changes!(astate, pc, stmt.args)
        end
    elseif isa(stmt, PhiNode)
        analyze_edges!(astate, pc, stmt.values)
    elseif isa(stmt, PiNode)
        if isdefined(stmt, :val)
            add_alias_change!(astate, SSAValue(pc), stmt.val)
        end
    elseif isa(stmt, PhiCNode)
        analyze_edges!(astate, pc, stmt.values)
    elseif isa(stmt, UpsilonNode)
        if isdefined(stmt, :val)
            add_alias_change!(astate, SSAValue(pc), stmt.val)
        end
    elseif isa(stmt, GlobalRef) # global load
        add_all_escape_change!(astate, SSAValue(pc))
    elseif isa(stmt, SSAValue)
        add_alias_change!(astate, SSAValue(pc), stmt)
    elseif isa(stmt, Argument)
        add_alias_change!(astate, SSAValue(pc), stmt)
    else
        # otherwise `stmt` can be inlined literal values etc.
        @assert !isterminator(stmt) "Found unexpected IR element"
    end
end

function analyze_edges!(astate::AnalysisState, pc::Int, edges::Vector{Any})
    ret = SSAValue(pc)
    for i in 1:length(edges)
        if isassigned(edges, i)
            add_alias_change!(astate, ret, edges[i])
        end
    end
end

struct Parameter <: MemoryKind
    n::Int
end
Parameter(arg::Argument) = Parameter(arg.n)
struct ParameterMemory <: MemoryKind
    id::Int
    param::Parameter
    fidx::Int
end
ParameterMemory((; id, arg, fidx)::CallerMemory) = ParameterMemory(id, Parameter(arg), fidx)

abstract type InterMemoryInfo end
struct InterMustAliasMemoryInfo <: InterMemoryInfo
    alias::Any
end
struct InterMayAliasMemoryInfo <: InterMemoryInfo
    aliases::Vector{Any}
end
abstract type InterObjectInfo end
struct HasInterUnanalyzedMemory <: InterObjectInfo end
struct HasInterIndexableFields <: InterObjectInfo
    fields::Vector{InterMemoryInfo}
end
struct HasInterUnknownMemory <: InterObjectInfo end
const ⊥ₒ̅, ⊤ₒ̅ = HasInterUnanalyzedMemory(), HasInterUnknownMemory()

struct InterEscapeInfo
    escape_bits::UInt8
    InterObjectInfo::InterObjectInfo
end
function InterEscapeInfo(x::EscapeInfo)
    has_all_escape(x) && return InterEscapeInfo(ARG_ALL_ESCAPE, ⊤ₒ̅)
    escape_bits = 0x00
    has_return_escape(x) && (escape_bits |= ARG_RETURN_ESCAPE)
    has_thrown_escape(x) && (escape_bits |= ARG_THROWN_ESCAPE)
    InterObjectInfo = convert_to_inter_object_info(x.ObjectInfo)
    return InterEscapeInfo(escape_bits, InterObjectInfo)
end

function convert_to_inter_object_info(@nospecialize(x::ObjectInfo))
    if x === ⊥ₒ
        return ⊥ₒ̅
    elseif x === ⊤ₒ
        return ⊤ₒ̅
    else
        x = x::Union{HasIndexableFields,HasIndexableCallerFields}
        nf = length(x.fields)
        inter_fields = Vector{InterMemoryInfo}(undef, nf)
        for i = 1:nf
            xfinfo = x.fields[i]
            if xfinfo isa MayAliasMemoryInfo
                inter_aliases = Any[]
                for aval in xfinfo.aliases
                    push!(inter_aliases, convert_to_inter_irval(aval))
                end
                inter_xfinfo = InterMayAliasMemoryInfo(inter_aliases)
            else
                xfinfo = xfinfo::MustAliasMemoryInfo
                inter_xfinfo = InterMustAliasMemoryInfo(convert_to_inter_irval(xfinfo.alias))
            end
            inter_fields[i] = inter_xfinfo
        end
        return HasInterIndexableFields(inter_fields)
    end
end

function convert_to_inter_irval(@nospecialize(val))
    if val isa Argument
        return Parameter(val)
    elseif val isa SSAValue
        println("TODO")
    elseif val isa CallerMemory
        return ParameterMemory(val)
    else
        return val
    end
end

const ARG_ALL_ESCAPE    = 0x01 << 0
const ARG_RETURN_ESCAPE = 0x01 << 1
const ARG_THROWN_ESCAPE = 0x01 << 2

has_no_escape(x::InterEscapeInfo)     = !has_all_escape(x) && !has_return_escape(x) && !has_thrown_escape(x)
has_all_escape(x::InterEscapeInfo)    = x.escape_bits & ARG_ALL_ESCAPE    ≠ 0
has_return_escape(x::InterEscapeInfo) = x.escape_bits & ARG_RETURN_ESCAPE ≠ 0
has_thrown_escape(x::InterEscapeInfo) = x.escape_bits & ARG_THROWN_ESCAPE ≠ 0

struct EscapeCache
    nparams::Int
    param_memory_map::Vector{ParameterMemory}
    escapes::Vector{InterEscapeInfo}
    aliasset::AliasSet
    retescape::InterEscapeInfo # TODO maintain distingushied memory for return values?
    function EscapeCache(eresult::EscapeResult)
        (; nargs, nstmts, caller_memory_map) = eresult.afinfo
        param_memory_map = ParameterMemory[
            ParameterMemory(caller_memory) for caller_memory in caller_memory_map]
        n_caller_memory = length(caller_memory_map)
        nelms = nargs + n_caller_memory
        cached_escapes = Vector{InterEscapeInfo}(undef, nelms)
        cached_aliasset = AliasSet(nelms)
        for i = 1:nargs
            xidx = i
            cached_xidx = xidx
            cached_escapes[cached_xidx] = InterEscapeInfo(eresult[xidx])
            add_alias_for_cache!(cached_aliasset, eresult, xidx, cached_xidx)
        end
        for i = 1:n_caller_memory
            xidx = nargs + nstmts + i
            cached_xidx = xidx - nstmts
            cached_escapes[cached_xidx] = InterEscapeInfo(eresult[xidx])
            add_alias_for_cache!(cached_aliasset, eresult, xidx, cached_xidx)
        end
        retescape = InterEscapeInfo(eresult[0])
        return new(nargs, param_memory_map, cached_escapes, cached_aliasset, retescape)
    end
end

function add_alias_for_cache!(cached_aliasset::AliasSet, eresult::EscapeResult, xidx::Int, cached_xidx::Int)
    aliases = getaliases(eresult, xidx)
    if aliases !== nothing
        for aidx in aliases
            aval = irval(aidx, eresult)
            if aval isa Argument
                paramidx = aval.n
                union!(cached_aliasset, cached_xidx, paramidx)
            elseif aval isa SSAValue
                println("TODO")
            elseif aval isa CallerMemory
                param_memory_id = aval.id + eresult.afinfo.nargs
                union!(cached_aliasset, cached_xidx, param_memory_id)
            end
        end
    end
end

# analyze statically-resolved call, i.e. `Expr(:invoke, ::MethodInstance, ...)`
function analyze_invoke!(astate::AnalysisState, pc::Int, args::Vector{Any})
    codeinst = first(args)
    first_idx, last_idx = 2, length(args)
    add_liveness_changes!(astate, args, first_idx, last_idx)
    escape_cache = astate.get_escape_cache(codeinst)
    retval = SSAValue(pc)
    if escape_cache isa Bool
        if escape_cache
            # This method call is very simple and has good effects, so there's no need to
            # escape its arguments. However, since the arguments might be returned, we need
            # to consider the possibility of aliasing between them and the return value.
            for argidx = first_idx:last_idx
                arg = args[argidx]
                if arg isa GlobalRef
                    continue # :effect_free guarantees that nothings escapes to the global scope
                end
                if !is_identity_free_argtype(argextype(arg, astate.ir))
                    add_alias_change!(astate, retval, arg)
                end
            end
            return nothing
        else
            return add_conservative_changes!(astate, pc, args, #=first_idx=#2)
        end
    end
    escape_cache = escape_cache::EscapeCache
    for (paramidx, argidx) in enumerate(first_idx:last_idx)
        if paramidx > escape_cache.nparams
            # handle isva signature
            # COMBAK will this be invalid once we take alias information into account?
            paramidx = escape_cache.nparams
        end
        add_inter_procedural_change!(astate, retval, args, paramidx, argidx, escape_cache)
    end
    # # propagate the escape information of the return value to the `retval::SSAValue`
    # add_inter_procedural_change(astate, retval, retval, retescape)
end

# TODO account for aliasing between parameter memory

function add_inter_procedural_change!(astate::AnalysisState,
    retval::SSAValue, args::Vector{Any}, paramidx::Int, argidx::Int, escape_cache::EscapeCache)
    argval = args[argidx]
    argescape = escape_cache.escapes[paramidx]
    if has_all_escape(argescape)
        add_all_escape_change!(astate, argval)
    else
        if !is_nothrow(astate.ir, retval) && has_thrown_escape(argescape)
            add_thrown_escape_change!(astate, argval)
        end
        if has_return_escape(argescape)
            add_alias_change!(astate, argval, retval)
        end
        object_info = convert_to_object_info(argescape.InterObjectInfo, astate, args)
        add_object_info_change!(astate, argval, object_info)
        add_liveness_change!(astate, argval)
    end
end

function convert_to_object_info(@nospecialize(x::InterObjectInfo), astate::AnalysisState, args::Vector{Any})
    if x === ⊥ₒ̅
        return ⊥ₒ
    elseif x === ⊤ₒ̅
        return ⊤ₒ
    else
        x = x::HasInterIndexableFields
        nf = length(x.fields)
        fields = FieldInfos(undef, nf)
        for i = 1:nf
            inter_xfinfo = x.fields[i]
            if inter_xfinfo isa InterMayAliasMemoryInfo
                aliases = IdSet{Any}()
                for j = 1:length(inter_xfinfo.aliases)
                    irvalsingle = convert_to_irval(inter_xfinfo.aliases[j], astate, args)
                    if irvalsingle === nothing
                        return ⊤ₒ
                    end
                    irval, single = irvalsingle
                    if single
                        push!(aliases, irval)
                    else
                        for ival in irval
                            push!(aliases, aval)
                        end
                    end
                end
                xfinfo = MayAliasMemoryInfo(aliases)
            else
                inter_xfinfo = inter_xfinfo::InterMustAliasMemoryInfo
                irvalsingle = convert_to_irval(inter_xfinfo.alias, astate, args)
                if irvalsingle === nothing
                    return ⊤ₒ
                end
                irval, single = irvalsingle
                if single
                    xfinfo = MustAliasMemoryInfo(irval)
                else
                    aliases = IdSet{Any}()
                    for ival in irval
                        push!(aliases, ival)
                    end
                    xfinfo = MayAliasMemoryInfo(aliases)
                end
            end
            fields[i] = xfinfo
        end
        return HasIndexableFields(fields)
    end
end

function convert_to_irval(@nospecialize(interval), astate::AnalysisState, args::Vector{Any})
    if interval isa Parameter
        return args[interval.n+1], true
    elseif interval isa ParameterMemory
        arg = args[interval.param.n+1]
        if arg isa SSAValue || arg isa Argument
            ainfo = astate.currstate[arg]
            object_info = ainfo.ObjectInfo
            if object_info isa HasIndexableFields
                @assert 1 ≤ interval.fidx ≤ length(object_info.fields)
                afinfo = object_info.fields[interval.fidx]
                if afinfo isa MayAliasMemoryInfo
                    return afinfo.aliases, false
                else
                    afinfo = afinfo::MustAliasMemoryInfo
                    return afinfo.alias, true
                end
            else
                return nothing
            end
        else
            return nothing
        end
    else
        return interval, true
    end
end

# analyze every argument `(args[6:length(args[3])])` and the name `args[1]`
# TODO: we can apply a similar strategy like builtin calls to specialize some foreigncalls
function analyze_foreigncall!(astate::AnalysisState, pc::Int, args::Vector{Any})
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
    nothrow || add_thrown_escape_change!(astate, name)
    add_liveness_change!(astate, name)
    for i = 1:nargs
        # we should escape this argument if it is directly called,
        # otherwise just impose ThrownEscape if not nothrow
        arg = args[5+i]
        if argtypes[i] === Any
            add_all_escape_change!(astate, arg)
        elseif nothrow
            add_liveness_change!(astate, arg)
        else
            add_thrown_escape_change!(astate, arg)
            add_liveness_change!(astate, arg)
        end
    end
    for i = (5+nargs):length(args)
        arg = args[i]
        add_liveness_change!(astate, arg)
    end
end

function analyze_gc_preserve!(astate::AnalysisState, pc::Int, args::Vector{Any})
    @assert length(args) == 1 "invalid :gc_preserve_end"
    val = args[1]
    @assert val isa SSAValue "invalid :gc_preserve_end"
    beginstmt = astate.ir[val][:stmt]
    @assert isexpr(beginstmt, :gc_preserve_begin) "invalid :gc_preserve_end"
    beginargs = beginstmt.args
    # COMBAK we might need to add liveness for all statements from `:gc_preserve_begin` to `:gc_preserve_end`
    add_liveness_changes!(astate, beginargs)
end

function analyze_call!(astate::AnalysisState, pc::Int, args::Vector{Any})
    ft = argextype(first(args), astate.ir)
    f = singleton_type(ft)
    if f isa IntrinsicFunction
        if is_nothrow(astate.ir, pc)
            add_liveness_changes!(astate, args, 2)
        else
            add_fallback_changes!(astate, args, 2)
        end
        # TODO needs to account for pointer operations?
    elseif f isa Builtin
        result = analyze_builtin!(f, astate, pc, args)
        if result === missing
            # if this call hasn't been handled by any of pre-defined handlers, escape it conservatively
            add_conservative_changes!(astate, pc, args)
        elseif result === true || is_nothrow(astate.ir, pc)
            add_liveness_changes!(astate, args, 2)
        else
            add_fallback_changes!(astate, args, 2)
        end
    else
        # escape this generic function or unknown function call conservatively
        add_conservative_changes!(astate, pc, args)
    end
end

analyze_builtin!(@nospecialize(f), _...) = missing

# safe builtins
analyze_builtin!(::typeof(isa), _...) = false
analyze_builtin!(::typeof(typeof), _...) = false
analyze_builtin!(::typeof(sizeof), _...) = false
analyze_builtin!(::typeof(===), _...) = false
analyze_builtin!(::typeof(Core.donotdelete), _...) = false
# not really safe, but `ThrownEscape` will be imposed later
analyze_builtin!(::typeof(throw), _...) = false
analyze_builtin!(::typeof(Core.throw_methoderror), _...) = false

function analyze_builtin!(::typeof(ifelse), astate::AnalysisState, pc::Int, args::Vector{Any})
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

function analyze_builtin!(::typeof(typeassert), astate::AnalysisState, pc::Int, args::Vector{Any})
    length(args) == 3 || return false
    f, obj, typ = args
    add_alias_change!(astate, SSAValue(pc), obj)
    return false
end

function analyze_new!(astate::AnalysisState, pc::Int, args::Vector{Any}, add_liveness::Bool=true)
    obj = SSAValue(pc)
    nargs = length(args)
    typ = widenconst(argextype(obj, astate.ir))
    nflds = fieldcount_noerror(typ)
    if nflds === nothing
        # The values stored into the fields can't be analyzed precisely:
        # Alias `obj` with its field values so that escape information for `obj` is directly
        # propagated to the field values.
        for i in 2:nargs
            arg = args[i]
            add_alias_change!(astate, obj, arg)
            add_liveness && add_liveness_change!(astate, arg)
        end
        add_object_info_change!(astate, obj, ⊤ₒ)
    else
        fields = FieldInfos(undef, nflds)
        for i = 1:nflds
            if i+1 > nargs
                xfinfo = MustAliasMemoryInfo(UninitializedMemory())
            else
                arg = args[i+1]
                xfinfo = MustAliasMemoryInfo(arg)
                add_liveness && add_liveness_change!(astate, arg)
            end
            fields[i] = xfinfo
        end
        add_object_info_change!(astate, obj, HasIndexableFields(fields))
    end
    if !is_nothrow(astate.ir, pc)
        add_thrown_escape_change!(astate, obj)
    end
end

function analyze_builtin!(::typeof(tuple), astate::AnalysisState, pc::Int, args::Vector{Any})
    # `add_liveness = false` since it will be added in `escape_call!` instead
    analyze_new!(astate, pc, args, #=add_liveness=#false)
    return true # `tuple` call is always no throw
end

function analyze_builtin!(::typeof(isdefined), astate::AnalysisState, pc::Int, args::Vector{Any})
    length(args) ≥ 3 || return false
    ir = astate.ir
    obj = args[2]
    if isa(obj, SSAValue) || isa(obj, Argument)
        objinfo = astate.currstate[obj]
    else
        return false
    end
    xoinfo = objinfo.ObjectInfo
    if xoinfo isa HasIndexableFields || xoinfo isa HasIndexableCallerFields
        fval = try_compute_field(ir, args[3])
        fval === nothing && return false
        objtyp = widenconst(argextype(obj, ir))
        fidx = try_compute_fieldidx(objtyp, fval)
        fidx === nothing && return false
        @assert length(xoinfo.fields) ≥ fidx "invalid field index"
        xfinfo = xoinfo.fields[fidx]
        local initialized::Union{Nothing,Bool} = nothing
        if xfinfo isa MustAliasMemoryInfo
            aval = xfinfo.alias
            initialized = aval !== UninitializedMemory()
        else
            xfinfo = xfinfo::MayAliasMemoryInfo
            for aval in xfinfo.aliases
                if initialized === nothing
                    initialized = aval !== UninitializedMemory()
                elseif initialized
                    if aval === UninitializedMemory()
                        return false
                    end
                else
                    if aval !== UninitializedMemory()
                        return false
                    end
                end
            end
        end
        if initialized !== nothing
            astate.ssamemoryinfo[pc] = MustAliasMemoryInfo(initialized)
        end
    end
    return false
end

function analyze_builtin!(::typeof(getfield), astate::AnalysisState, pc::Int, args::Vector{Any})
    length(args) ≥ 3 || return false
    ir = astate.ir
    obj = args[2]
    objtyp = widenconst(argextype(obj, ir))
    retval = SSAValue(pc)
    if hasintersect(objtyp, Module) # global load
        @goto unanalyzable_object
    elseif isa(obj, SSAValue) || isa(obj, Argument)
        objinfo = astate.currstate[obj]
    else
        @label unanalyzable_object
        add_all_escape_change!(astate, obj)
        add_all_escape_change!(astate, retval)
        return false
    end
    nothrow = is_nothrow(astate.ir, pc)
    xoinfo = objinfo.ObjectInfo
    if xoinfo isa HasIndexableFields || xoinfo isa HasIndexableCallerFields
        fval = try_compute_field(ir, args[3])
        fval === nothing && @goto conservative_propagation
        fidx = try_compute_fieldidx(objtyp, fval)
        fidx === nothing && @goto conservative_propagation
        @assert length(xoinfo.fields) ≥ fidx "invalid field index"
        xfinfo = xoinfo.fields[fidx]
        @label precise_propagation
        local all_initialized::Bool = true
        if xfinfo isa MustAliasMemoryInfo
            aval = xfinfo.alias
            add_alias_change!(astate, retval, aval)
            all_initialized &= !(aval === UninitializedMemory())
        else
            xfinfo = xfinfo::MayAliasMemoryInfo
            for aval in xfinfo.aliases
                add_alias_change!(astate, retval, aval)
                all_initialized &= !(aval === UninitializedMemory())
            end
        end
        nothrow = all_initialized # refine `nothrow` information if possible
        if xoinfo isa HasIndexableCallerFields
            @assert length(xoinfo.caller_memory_list) ≥ fidx "invalid field index"
            caller_memory = xoinfo.caller_memory_list[fidx]
            if caller_memory isa IdSet{CallerMemory}
                for argmem in caller_memory
                    add_alias_change!(astate, retval, argmem)
                end
            else
                add_alias_change!(astate, retval, caller_memory)
            end
            astate.ssamemoryinfo[pc] = ⊤ₘ # load forwarding is impossible for fields with caller memories
        else
            astate.ssamemoryinfo[pc] = xfinfo
        end
    else
        @label conservative_propagation
        # the field being read couldn't be analyzed precisely, now we need to:
        # 1. mark its `ObjectInfo` as `HasUnknownMemory`, and also
        # 2. alias the object to the returned value (since the field may opoint to `obj` itself)
        add_object_info_change!(astate, obj, ⊤ₒ)     # 1
        add_alias_change!(astate, obj, retval)       # 2
        add_all_escape_change!(astate, retval)
        astate.ssamemoryinfo[pc] = ⊤ₘ
    end
    return nothrow
end

function analyze_builtin!(::typeof(setfield!), astate::AnalysisState, pc::Int, args::Vector{Any})
    length(args) ≥ 4 || return false
    ir = astate.ir
    obj, val = args[2], args[4]
    objtyp = widenconst(argextype(obj, ir))
    if hasintersect(objtyp, Module) # global store
        add_all_escape_change!(astate, val)
        return false
    elseif isa(obj, SSAValue) || isa(obj, Argument)
        objinfo = astate.currstate[obj]
    else
        # unanalyzable object (e.g. obj::GlobalRef): escape field value conservatively
        add_all_escape_change!(astate, val)
        return false
    end
    nothrow = is_nothrow(astate.ir, pc)
    xoinfo = objinfo.ObjectInfo
    if xoinfo isa HasIndexableFields || xoinfo isa HasIndexableCallerFields
        fval = try_compute_field(ir, args[3])
        fval === nothing && @goto conservative_propagation
        fidx = try_compute_fieldidx(objtyp, fval)
        fidx === nothing && @goto conservative_propagation
        @assert length(xoinfo.fields) ≥ fidx "invalid field index"
        xoinfo.fields[fidx] = MustAliasMemoryInfo(val)
        add_object_info_change!(astate, obj, xoinfo)
    else
        @label conservative_propagation
        # the field being stored couldn't be analyzed precisely, now we need to:
        # 1. mark its `ObjectInfo` as `HasUnknownMemory`, and also
        # 2. alias the object to the stored value (since the field may opoint to `obj` itself)
        add_object_info_change!(astate, obj, ⊤ₒ) # 1
        add_alias_change!(astate, obj, val)      # 2
    end
    # also propagate escape information imposed on the return value of this `setfield!`
    add_alias_change!(astate, val, SSAValue(pc))
    return false
end

function analyze_builtin!(::typeof(Core.finalizer), astate::AnalysisState, pc::Int, args::Vector{Any})
    if length(args) ≥ 3
        obj = args[3]
        add_liveness_change!(astate, obj) # TODO setup a proper FinalizerEscape?
    end
    return false
end

end # baremodule EscapeAnalysis

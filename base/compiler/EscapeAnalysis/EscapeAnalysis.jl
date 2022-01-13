baremodule EscapeAnalysis

export
    analyze_escapes,
    cache_escapes!,
    has_no_escape,
    has_return_escape,
    has_thrown_escape,
    has_all_escape,
    is_load_forwardable,
    is_sroa_eligible,
    can_elide_finalizer

# analysis
# ========

const _TOP_MOD = ccall(:jl_base_relative_to, Any, (Any,), EscapeAnalysis)::Module

# imports
import ._TOP_MOD: ==, getindex, setindex!
# usings
import Core:
    MethodInstance, Const, Argument, SSAValue, PiNode, PhiNode, UpsilonNode, PhiCNode,
    ReturnNode, GotoNode, GotoIfNot, SimpleVector, sizeof, ifelse, arrayset, arrayref,
    arraysize, ImmutableArray, arrayfreeze, mutating_arrayfreeze, arraythaw
import ._TOP_MOD:     # Base definitions
    @__MODULE__, @eval, @assert, @nospecialize, @inbounds, @inline, @noinline, @label, @goto,
    !, !==, !=, ≠, +, -, ≤, <, ≥, >, &, |, include, error, missing, copy,
    Vector, BitSet, IdDict, IdSet, UnitRange, ∪, ⊆, ∩, :, ∈, ∉, in, length, get, first, last,
    isempty, isassigned, pop!, push!, empty!, max, min, Csize_t
import Core.Compiler: # Core.Compiler specific definitions
    isbitstype, isexpr, is_meta_expr_head, println,
    IRCode, IR_FLAG_EFFECT_FREE, widenconst, argextype, singleton_type, fieldcount_noerror,
    try_compute_field, try_compute_fieldidx, hasintersect, ⊑ as ⊑ₜ, intrinsic_nothrow,
    array_builtin_common_typecheck, arrayset_typecheck, setfield!_nothrow, compute_trycatch

if _TOP_MOD !== Core.Compiler
    include(@__MODULE__, "disjoint_set.jl")
else
    include(@__MODULE__, "compiler/EscapeAnalysis/disjoint_set.jl")
end

# XXX better to be IdSet{Int}?
const FieldEscape = BitSet
const FieldEscapes = Vector{BitSet}
# for x in ArrayEscapes:
# - x::Int: `irval(x, estate)` imposes escapes on the array elements
# - x::SSAValue: array elements can potentially escape via BoundsError at this SSA statement
const ArrayEscapes = IdSet{Union{Int,SSAValue}}

"""
    x::EscapeLattice

A lattice for escape information, which holds the following properties:
- `x.Analyzed::Bool`: not formally part of the lattice, only indicates `x` has not been analyzed or not
- `x.ReturnEscape::BitSet`: records SSA statements where `x` can escape to the caller via return
  where `0 ∈ x.ReturnEscape` has the special meaning that it's visible to the caller
  simply because it's passed as call argument
- `x.ThrownEscape::BitSet`: records SSA statements where `x` can be thrown as exception:
  this information will be used by `escape_exception!` to propagate potential escapes via exception
- `x.AliasEscapes::Union{FieldEscapes,ArrayEscapes,Bool}`: maintains all possible values
  that may escape objects that can be referenced from `x`:
  * `x.AliasEscapes === false` indicates the fields/elements of `x` isn't analyzed yet
  * `x.AliasEscapes === true` indicates the fields/elements of `x` can't be analyzed,
    e.g. the type of `x` is not known or is not concrete and thus its fields/elements
    can't be known precisely
  * `x.AliasEscapes::FieldEscapes` records all the possible values that can escape fields of `x`,
    which allows EA to propagate propagate escape information imposed on a field
    of `x` to its values (by analyzing `Expr(:new, ...)` and `setfield!(x, ...)`).
  * `x.AliasEscapes::ArrayEscapes` records all the possible values that can escape elements of `x`,
    or all SSA staements that can potentially escape elements of `x` via `BoundsError`.
- `x.ArgEscape::Int` (not implemented yet): indicates it will escape to the caller through
  `setfield!` on argument(s)
  * `-1` : no escape
  * `0` : unknown or multiple
  * `n` : through argument N

There are utility constructors to create common `EscapeLattice`s, e.g.,
- `NoEscape()`: the bottom(-like) element of this lattice, meaning it won't escape to anywhere
- `AllEscape()`: the topmost element of this lattice, meaning it will escape to everywhere

`analyze_escapes` will transition these elements from the bottom to the top,
in the same direction as Julia's native type inference routine.
An abstract state will be initialized with the bottom(-like) elements:
- the call arguments are initialized as `ArgumentReturnEscape()`, because they're visible from a caller immediately
- the other states are initialized as `NotAnalyzed()`, which is a special lattice element that
  is slightly lower than `NoEscape`, but at the same time doesn't represent any meaning
  other than it's not analyzed yet (thus it's not formally part of the lattice).
"""
struct EscapeLattice
    Analyzed::Bool
    ReturnEscape::BitSet
    ThrownEscape::BitSet
    AliasEscapes #::Union{FieldEscapes,ArrayEscapes,Bool}
    # TODO: ArgEscape::Int

    function EscapeLattice(
        Analyzed::Bool,
        ReturnEscape::BitSet,
        ThrownEscape::BitSet,
        AliasEscapes#=::Union{FieldEscapes,ArrayEscapes,Bool}=#,
        )
        @nospecialize AliasEscapes
        return new(
            Analyzed,
            ReturnEscape,
            ThrownEscape,
            AliasEscapes,
            )
    end
    function EscapeLattice(
        x::EscapeLattice,
        # non-concrete fields should be passed as default arguments
        # in order to avoid allocating non-concrete `NamedTuple`s
        AliasEscapes#=::Union{FieldEscapes,ArrayEscapes,Bool}=# = x.AliasEscapes;
        Analyzed::Bool = x.Analyzed,
        ReturnEscape::BitSet = x.ReturnEscape,
        ThrownEscape::BitSet = x.ThrownEscape,
        )
        @nospecialize AliasEscapes
        return new(
            Analyzed,
            ReturnEscape,
            ThrownEscape,
            AliasEscapes,
            )
    end
end

# precomputed default values in order to eliminate computations at each callsite
const BOT_RETURN_ESCAPE = BitSet()
const ARG_RETURN_ESCAPE = BitSet(0)
const TOP_RETURN_ESCAPE = BitSet(0:100_000)

const BOT_THROWN_ESCAPE = BitSet()
const TOP_THROWN_ESCAPE = BitSet(0:100_000)

const BOT_ALIAS_ESCAPES = false
const TOP_ALIAS_ESCAPES = true

# the constructors
NotAnalyzed() = EscapeLattice(false, BOT_RETURN_ESCAPE, BOT_THROWN_ESCAPE, BOT_ALIAS_ESCAPES) # not formally part of the lattice
NoEscape() = EscapeLattice(true, BOT_RETURN_ESCAPE, BOT_THROWN_ESCAPE, BOT_ALIAS_ESCAPES)
ReturnEscape(pc::Int) = EscapeLattice(true, BitSet(pc), BOT_THROWN_ESCAPE, BOT_ALIAS_ESCAPES)
ArgumentReturnEscape() = EscapeLattice(true, ARG_RETURN_ESCAPE, BOT_THROWN_ESCAPE, TOP_ALIAS_ESCAPES) # TODO allow interprocedural field analysis?
AllReturnEscape() = EscapeLattice(true, TOP_RETURN_ESCAPE, BOT_THROWN_ESCAPE, BOT_ALIAS_ESCAPES)
ThrownEscape(pc::Int) = EscapeLattice(true, BOT_RETURN_ESCAPE, BitSet(pc), BOT_ALIAS_ESCAPES)
ThrownEscape(pcs::BitSet) = EscapeLattice(true, BOT_RETURN_ESCAPE, pcs, BOT_ALIAS_ESCAPES)
AllEscape() = EscapeLattice(true, TOP_RETURN_ESCAPE, TOP_THROWN_ESCAPE, TOP_ALIAS_ESCAPES)

const ⊥, ⊤ = NotAnalyzed(), AllEscape()

# Convenience names for some ⊑ queries
has_no_escape(x::EscapeLattice) = ignore_aliasescapes(x) ⊑ NoEscape()
has_return_escape(x::EscapeLattice) = !isempty(x.ReturnEscape)
has_return_escape(x::EscapeLattice, pc::Int) = pc in x.ReturnEscape
has_thrown_escape(x::EscapeLattice) = !isempty(x.ThrownEscape)
has_thrown_escape(x::EscapeLattice, pc::Int) = pc in x.ThrownEscape
has_all_escape(x::EscapeLattice) = ⊤ ⊑ x

# utility lattice constructors
ignore_thrownescapes(x::EscapeLattice) = EscapeLattice(x; ThrownEscape=BOT_THROWN_ESCAPE)
ignore_aliasescapes(x::EscapeLattice) = EscapeLattice(x, BOT_ALIAS_ESCAPES)

"""
    is_load_forwardable(x::EscapeLattice) -> Bool

Queries if `x` is elibigle for store-to-load forwarding optimization.
"""
function is_load_forwardable(x::EscapeLattice)
    if x.AliasEscapes === false || # allows this query to work for immutables since we don't impose escape on them
       isa(x.AliasEscapes, FieldEscapes)
        # NOTE technically we also need to check `!has_thrown_escape(x)` here as well,
        # but we can also do equivalent check during forwarding
        return true
    end
    return false
end

"""
    is_sroa_eligible(x::EscapeLattice) -> Bool

Queries allocation eliminability by SROA.
"""
is_sroa_eligible(x::EscapeLattice) = is_load_forwardable(x) && !has_return_escape(x)

"""
    can_elide_finalizer(x::EscapeLattice, pc::Int) -> Bool

Queries the validity of the finalizer elision optimization at the return site of SSA statement `pc`,
which inserts `finalize` call when the lifetime of interested object ends.
Note that we don't need to take `x.ThrownEscape` into account because it would have never
been thrown when the program execution reaches the `return` site.
"""
can_elide_finalizer(x::EscapeLattice, pc::Int) =
    !(has_return_escape(x, 0) || has_return_escape(x, pc))

# we need to make sure this `==` operator corresponds to lattice equality rather than object equality,
# otherwise `propagate_changes` can't detect the convergence
x::EscapeLattice == y::EscapeLattice = begin
    # fast pass: better to avoid top comparison
    x === y && return true
    xr, yr = x.ReturnEscape, y.ReturnEscape
    if xr === TOP_RETURN_ESCAPE
        yr === TOP_RETURN_ESCAPE || return false
    elseif yr === TOP_RETURN_ESCAPE
        return false # x.ReturnEscape === TOP_RETURN_ESCAPE
    else
        xr == yr || return false
    end
    xt, yt = x.ThrownEscape, y.ThrownEscape
    if xt === TOP_THROWN_ESCAPE
        yt === TOP_THROWN_ESCAPE || return false
    elseif yt === TOP_THROWN_ESCAPE
        return false # x.ThrownEscape === TOP_THROWN_ESCAPE
    else
        xt == yt || return false
    end
    xf, yf = x.AliasEscapes, y.AliasEscapes
    if isa(xf, Bool)
        xf === yf || return false
    elseif isa(xf, FieldEscapes)
        isa(yf, FieldEscapes) || return false
        xf == yf || return false
    else
        xf = xf::ArrayEscapes
        isa(yf, ArrayEscapes) || return false
        xf == yf || return false
    end
    return x.Analyzed === y.Analyzed
end

"""
    x::EscapeLattice ⊑ y::EscapeLattice -> Bool

The non-strict partial order over `EscapeLattice`.
"""
x::EscapeLattice ⊑ y::EscapeLattice = begin
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
    xr, yr = x.ReturnEscape, y.ReturnEscape
    if xr === TOP_RETURN_ESCAPE
        yr !== TOP_RETURN_ESCAPE && return false
    elseif yr !== TOP_RETURN_ESCAPE
        xr ⊆ yr || return false
    end
    xt, yt = x.ThrownEscape, y.ThrownEscape
    if xt === TOP_THROWN_ESCAPE
        yt !== TOP_THROWN_ESCAPE && return false
    elseif yt !== TOP_THROWN_ESCAPE
        xt ⊆ yt || return false
    end
    xf, yf = x.AliasEscapes, y.AliasEscapes
    if isa(xf, Bool)
        xf && yf !== true && return false
    elseif isa(xf, FieldEscapes)
        if isa(yf, FieldEscapes)
            xn, yn = length(xf), length(yf)
            xn > yn && return false
            for i in 1:xn
                xf[i] ⊆ yf[i] || return false
            end
        else
            yf === true || return false
        end
    else
        xf = xf::ArrayEscapes
        if isa(yf, ArrayEscapes)
            xf ⊆ yf || return false
        else
            yf === true || return false
        end
    end
    return x.Analyzed ≤ y.Analyzed
end

"""
    x::EscapeLattice ⊏ y::EscapeLattice -> Bool

The strict partial order over `EscapeLattice`.
This is defined as the irreflexive kernel of `⊏`.
"""
x::EscapeLattice ⊏ y::EscapeLattice = x ⊑ y && !(y ⊑ x)

"""
    x::EscapeLattice ⋤ y::EscapeLattice -> Bool

This order could be used as a slightly more efficient version of the strict order `⊏`,
where we can safely assume `x ⊑ y` holds.
"""
x::EscapeLattice ⋤ y::EscapeLattice = !(y ⊑ x)

"""
    x::EscapeLattice ⊔ y::EscapeLattice -> EscapeLattice

Computes the join of `x` and `y` in the partial order defined by `EscapeLattice`.
"""
x::EscapeLattice ⊔ y::EscapeLattice = begin
    # fast pass: better to avoid top join
    if x === ⊤ || y === ⊤
        return ⊤
    elseif x === ⊥
        return y
    elseif y === ⊥
        return x
    end
    xr, yr = x.ReturnEscape, y.ReturnEscape
    if xr === TOP_RETURN_ESCAPE || yr === TOP_RETURN_ESCAPE
        ReturnEscape = TOP_RETURN_ESCAPE
    elseif xr === BOT_RETURN_ESCAPE
        ReturnEscape = yr
    elseif yr === BOT_RETURN_ESCAPE
        ReturnEscape = xr
    else
        ReturnEscape = xr ∪ yr
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
    xf, yf = x.AliasEscapes, y.AliasEscapes
    if xf === true || yf === true
        AliasEscapes = true
    elseif xf === false
        AliasEscapes = yf
    elseif yf === false
        AliasEscapes = xf
    elseif isa(xf, FieldEscapes)
        if isa(yf, FieldEscapes)
            xn, yn = length(xf), length(yf)
            nmax, nmin = max(xn, yn), min(xn, yn)
            AliasEscapes = Vector{FieldEscape}(undef, nmax)
            for i in 1:nmax
                if i > nmin
                    AliasEscapes[i] = (xn > yn ? xf : yf)[i]
                else
                    AliasEscapes[i] = xf[i] ∪ yf[i]
                end
            end
        else
            AliasEscapes = true # handle conflicting case conservatively
        end
    else
        xf = xf::ArrayEscapes
        if isa(yf, ArrayEscapes)
            AliasEscapes = xf ∪ yf
        else
            AliasEscapes = true # handle conflicting case conservatively
        end
    end
    return EscapeLattice(
        x.Analyzed | y.Analyzed,
        ReturnEscape,
        ThrownEscape,
        AliasEscapes,
        )
end

# TODO setup a more effient struct for cache
# which can discard escape information on SSS values and arguments that don't join dispatch signature

const AliasSet = IntDisjointSet{Int}

"""
    estate::EscapeState

Extended lattice that maps arguments and SSA values to escape information represented as `EscapeLattice`.
Escape information imposed on SSA IR element `x` can be retrieved by `estate[x]`.
"""
struct EscapeState
    escapes::Vector{EscapeLattice}
    aliasset::AliasSet
    nargs::Int
end
function EscapeState(nargs::Int, nstmts::Int)
    escapes = EscapeLattice[
        1 ≤ i ≤ nargs ? ArgumentReturnEscape() : ⊥ for i in 1:(nargs+nstmts)]
    aliaset = AliasSet(nargs+nstmts)
    return EscapeState(escapes, aliaset, nargs)
end
function getindex(estate::EscapeState, @nospecialize(x))
    if isa(x, Argument) || isa(x, SSAValue)
        return estate.escapes[iridx(x, estate)]
    else
        return nothing
    end
end
function setindex!(estate::EscapeState, v::EscapeLattice, @nospecialize(x))
    if isa(x, Argument) || isa(x, SSAValue)
        estate.escapes[iridx(x, estate)] = v
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

"""
    EscapeLatticeCache(x::EscapeLattice) -> x′::EscapeLatticeCache

The data structure for caching `x::EscapeLattice` for interprocedural propagation,
which is slightly more efficient than the original `x` object.
"""
struct EscapeLatticeCache
    AllEscape::Bool
    ReturnEscape::Bool
    ThrownEscape::Bool
    function EscapeLatticeCache(x::EscapeLattice)
        x === ⊤ && return new(true, true, true)
        ReturnEscape = x.ReturnEscape === ARG_RETURN_ESCAPE ? false : true
        ThrownEscape = isempty(x.ThrownEscape)              ? false : true
        return new(false, ReturnEscape, ThrownEscape)
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
        cache::Vector{EscapeLatticeCache}
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
    const GLOBAL_ESCAPE_CACHE = IdDict{MethodInstance,Vector{EscapeLatticeCache}}()
    function cache_escapes!(linfo::MethodInstance, estate::EscapeState, _::IRCode)
        cache = to_interprocedural(estate)
        GLOBAL_ESCAPE_CACHE[linfo] = cache
        return cache
    end
    argescapes_from_cache(cache::Vector{EscapeLatticeCache}) = cache
end

function to_interprocedural(estate::EscapeState)
    cache = Vector{EscapeLatticeCache}(undef, estate.nargs)
    for i = 1:estate.nargs
        cache[i] = EscapeLatticeCache(estate.escapes[i])
    end
    return cache
end

__clear_escape_cache!() = empty!(GLOBAL_ESCAPE_CACHE)

const EscapeChange = Pair{Int,EscapeLattice}
const AliasChange  = Pair{Int,Int}
const Changes      = Vector{Union{EscapeChange,AliasChange}}

struct AnalysisState
    ir::IRCode
    estate::EscapeState
    changes::Changes
end

"""
    analyze_escapes(ir::IRCode, nargs::Int) -> estate::EscapeState

Analyzes escape information in `ir`.
`nargs` is the number of actual arguments of the analyzed call.
"""
function analyze_escapes(ir::IRCode, nargs::Int)
    stmts = ir.stmts
    nstmts = length(stmts)

    # only manage a single state, some flow-sensitivity is encoded as `EscapeLattice` properties
    estate = EscapeState(nargs, nstmts)
    changes = Changes() # stashes changes that happen at current statement
    tryregions = compute_tryregions(ir)
    astate = AnalysisState(ir, estate, changes)

    local debug_itr_counter = 0
    while true
        local anyupdate = false

        for pc in nstmts:-1:1
            stmt = stmts[pc][:inst]

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
                        invalid_escape_assignment!(ir, pc)
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
                    for x in stmt.args
                        add_escape_change!(astate, x, ⊤)
                    end
                end
            elseif isa(stmt, ReturnNode)
                if isdefined(stmt, :val)
                    add_escape_change!(astate, stmt.val, ReturnEscape(pc))
                end
            elseif isa(stmt, PhiNode)
                escape_edges!(astate, pc, stmt.values)
            elseif isa(stmt, PiNode)
                escape_val!(astate, pc, stmt)
            elseif isa(stmt, PhiCNode)
                escape_edges!(astate, pc, stmt.values)
            elseif isa(stmt, UpsilonNode)
                escape_val!(astate, pc, stmt)
            elseif isa(stmt, GlobalRef) # global load
                add_escape_change!(astate, SSAValue(pc), ⊤)
            elseif isa(stmt, SSAValue) # after SROA, we may see SSA value as statement
                escape_ssa!(astate, pc, stmt)
            else
                @assert stmt isa GotoNode || stmt isa GotoIfNot || stmt === nothing # TODO remove me
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
            xidx, info = change
            aliases = getaliases(xidx, estate)
            if aliases !== nothing
                for aidx in aliases
                    morechange = EscapeChange(aidx, info)
                    anychanged |= propagate_escape_change!(estate, morechange)
                end
            end
        else
            anychanged |= propagate_alias_change!(estate, change)
        end
    end
    return anychanged
end

function propagate_escape_change!(estate::EscapeState, change::EscapeChange)
    xidx, info = change
    old = estate.escapes[xidx]
    new = old ⊔ info
    if old ≠ new
        estate.escapes[xidx] = new
        return true
    end
    return false
end

function propagate_alias_change!(estate::EscapeState, change::AliasChange)
    xidx, yidx = change
    xroot = find_root!(estate.aliasset, xidx)
    yroot = find_root!(estate.aliasset, yidx)
    if xroot ≠ yroot
        union!(estate.aliasset, xroot, yroot)
        return true
    end
    return false
end

function add_escape_change!(astate::AnalysisState, @nospecialize(x), info::EscapeLattice)
    info === ⊥ && return # performance optimization
    xidx = iridx(x, astate.estate)
    if xidx !== nothing
        if !isbitstype(widenconst(argextype(x, astate.ir)))
            push!(astate.changes, EscapeChange(xidx, info))
        end
    end
end

function add_alias_change!(astate::AnalysisState, @nospecialize(x), @nospecialize(y))
    xidx = iridx(x, astate.estate)
    yidx = iridx(y, astate.estate)
    if xidx !== nothing && yidx !== nothing
        push!(astate.changes, AliasChange(xidx, yidx))
    end
end

function escape_edges!(astate::AnalysisState, pc::Int, edges::Vector{Any})
    info = astate.estate[SSAValue(pc)]
    for i in 1:length(edges)
        if isassigned(edges, i)
            v = edges[i]
            add_escape_change!(astate, v, info)
            add_alias_change!(astate, SSAValue(pc), v)
        end
    end
end

escape_val!(astate::AnalysisState, pc::Int, x) =
    isdefined(x, :val) && _escape_val!(astate, pc, x.val)

function _escape_val!(astate::AnalysisState, pc::Int, @nospecialize(val))
    ret = SSAValue(pc)
    info = astate.estate[ret]
    add_escape_change!(astate, val, info)
    add_alias_change!(astate, ret, val)
end

escape_ssa!(astate::AnalysisState, pc::Int, ssa::SSAValue) =
    _escape_val!(astate, pc, ssa)

# NOTE if we don't maintain the alias set that is separated from the lattice state, we can do
# soemthing like below: it essentially incorporates forward escape propagation in our default
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

# linear scan to find regions in which potential throws will be caught
function compute_tryregions(ir::IRCode)
    tryregions = nothing
    for idx in 1:length(ir.stmts)
        stmt = ir.stmts[idx][:inst]
        if isexpr(stmt, :enter)
            tryregions === nothing && (tryregions = UnitRange{Int}[])
            leave_block = stmt.args[1]::Int
            leave_pc = first(ir.cfg.blocks[leave_block].stmts)
            push!(tryregions, idx:leave_pc)
        end
    end
    return tryregions
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
        for i in 2:length(args)
            x = args[i]
            add_escape_change!(astate, x, ⊤)
        end
    else
        argescapes = argescapes_from_cache(cache)
        retinfo = astate.estate[SSAValue(pc)] # escape information imposed on the call statement
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
            add_escape_change!(astate, arg, info)
        end
    end
end

"""
    from_interprocedural(arginfo::EscapeLatticeCache, retinfo::EscapeLattice, pc::Int) -> x::EscapeLattice

Reinterprets the escape information imposed on the call argument which is cached as `arginfo`
in the context of the caller frame, where `retinfo` is the escape information imposed on
the return value and `pc` is the SSA statement number of the return value.
"""
function from_interprocedural(arginfo::EscapeLatticeCache, retinfo::EscapeLattice, pc::Int)
    arginfo.AllEscape && return ⊤

    ThrownEscape = arginfo.ThrownEscape ? BitSet(pc) : BOT_THROWN_ESCAPE

    newarginfo = EscapeLattice(
        #=Analyzed=#true, #=ReturnEscape=#BOT_RETURN_ESCAPE, ThrownEscape,
        # FIXME implement interprocedural memory effect-analysis
        # currently, this essentially disables the entire field analysis
        # it might be okay from the SROA point of view, since we can't remove the allocation
        # as far as it's passed to a callee anyway, but still we may want some field analysis
        # for e.g. stack allocation or some other IPO optimizations
        TOP_ALIAS_ESCAPES)

    if !arginfo.ReturnEscape
        # if this is simply passed as the call argument, we can discard the `ReturnEscape`
        # information and just propagate the other escape information
        return newarginfo
    end

    # if this argument can be "returned", we have to merge its escape information with that imposed on the return value
    return newarginfo ⊔ retinfo
end

@noinline function invalid_escape_assignment!(ir::IRCode, pc::Int)
    @eval Main (ir = $ir; pc = $pc)
    error("unexpected assignment found: inspect `Main.pc` and `Main.pc`")
end

function escape_new!(astate::AnalysisState, pc::Int, args::Vector{Any})
    obj = SSAValue(pc)
    objinfo = astate.estate[obj]
    AliasEscapes = objinfo.AliasEscapes
    nargs = length(args)
    if isa(AliasEscapes, Bool)
        @label conservative_propagation
        # the fields couldn't be analyzed precisely: propagate the entire escape information
        # of this object to all its fields as the most conservative propagation
        for i in 2:nargs
            add_escape_change!(astate, args[i], objinfo)
        end
    elseif isa(AliasEscapes, FieldEscapes)
        # fields are known: propagate escape information imposed on recorded possibilities
        nf = length(AliasEscapes)
        for i in 2:nargs
            # fields are known: propagate the escape information of this object ignoring field information
            add_escape_change!(astate, args[i], ignore_aliasescapes(objinfo))
            # fields are known: propagate escape information imposed on recorded possibilities
            i-1 > nf && break # may happen when e.g. ϕ-node merges values with different types
            escape_field!(astate, args[i], AliasEscapes[i-1])
        end
    else
        # this object has been used as array, but it is allocated as struct here (i.e. should throw)
        # update obj's field information and just handle this case conservatively
        @assert isa(AliasEscapes, ArrayEscapes)
        objinfo = escape_unanalyzable_obj!(astate, obj, objinfo)
        @goto conservative_propagation
    end
    if !(astate.ir.stmts.flag[pc] & IR_FLAG_EFFECT_FREE ≠ 0)
        add_thrown_escapes!(astate, pc, args)
    end
end

function escape_field!(astate::AnalysisState, @nospecialize(v), xf::FieldEscape)
    estate = astate.estate
    for xidx in xf
        x = irval(xidx, estate)::SSAValue # TODO remove me once we implement ArgEscape
        add_escape_change!(astate, v, estate[x])
        add_alias_change!(astate, v, x)
    end
end

function escape_unanalyzable_obj!(astate::AnalysisState, @nospecialize(obj), objinfo::EscapeLattice)
    objinfo = EscapeLattice(objinfo, TOP_ALIAS_ESCAPES)
    add_escape_change!(astate, obj, objinfo)
    return objinfo
end

function add_thrown_escapes!(astate::AnalysisState, pc::Int, args::Vector{Any},
    first_idx::Int = 1, last_idx::Int = length(args))
    for i in first_idx:last_idx
        add_escape_change!(astate, args[i], ThrownEscape(pc))
    end
end

# escape every argument `(args[6:length(args[3])])` and the name `args[1]`
# TODO: we can apply a similar strategy like builtin calls to specialize some foreigncalls
function escape_foreigncall!(astate::AnalysisState, pc::Int, args::Vector{Any})
    nargs = length(args)
    if nargs < 6
        # invalid foreigncall, just escape everything
        return add_thrown_escapes!(astate, pc, args)
    end
    foreigncall_nargs = length((args[3])::SimpleVector)
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
    if !(astate.ir.stmts[pc][:flag] & IR_FLAG_EFFECT_FREE ≠ 0)
        add_escape_change!(astate, name, ThrownEscape(pc))
        for i in 6:5+foreigncall_nargs
            add_escape_change!(astate, args[i], ThrownEscape(pc))
        end
    end
end

normalize(@nospecialize x) = isa(x, QuoteNode) ? x.value : x

# NOTE error cases will be handled in `analyze_escapes` anyway, so we don't need to take care of them below
# TODO implement more builtins, make them more accurate
# TODO use `T_IFUNC`-like logic and don't not abuse dispatch ?

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
        intrinsic_nothrow(f, argtypes) || add_thrown_escapes!(astate, pc, args, 2)
        return # TODO accounts for pointer operations
    end
    result = escape_builtin!(f, astate, pc, args)
    if result === missing
        # if this call hasn't been handled by any of pre-defined handlers,
        # we escape this call conservatively
        for i in 2:length(args)
            add_escape_change!(astate, args[i], ⊤)
        end
        return
    elseif result === true
        return # ThrownEscape is already checked
    end
    # we escape statements with the `ThrownEscape` property using the effect-freeness
    # computed by `stmt_effect_free` invoked within inlining
    # TODO throwness ≠ "effect-free-ness"
    if !(astate.ir.stmts.flag[pc] & IR_FLAG_EFFECT_FREE ≠ 0)
        add_thrown_escapes!(astate, pc, args, 2)
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
    info = astate.estate[ret]
    condt = argextype(cond, astate.ir)
    if isa(condt, Const) && (cond = condt.val; isa(cond, Bool))
        if cond
            add_escape_change!(astate, th, info)
            add_alias_change!(astate, th, ret)
        else
            add_escape_change!(astate, el, info)
            add_alias_change!(astate, el, ret)
        end
    else
        add_escape_change!(astate, th, info)
        add_alias_change!(astate, th, ret)
        add_escape_change!(astate, el, info)
        add_alias_change!(astate, el, ret)
    end
    return false
end

function escape_builtin!(::typeof(typeassert), astate::AnalysisState, pc::Int, args::Vector{Any})
    length(args) == 3 || return false
    f, obj, typ = args
    ret = SSAValue(pc)
    info = astate.estate[ret]
    add_escape_change!(astate, obj, info)
    add_alias_change!(astate, ret, obj)
    return false
end

function escape_builtin!(::typeof(tuple), astate::AnalysisState, pc::Int, args::Vector{Any})
    escape_new!(astate, pc, args)
    return false
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
    AliasEscapes = objinfo.AliasEscapes
    if isa(AliasEscapes, Bool)
        if !AliasEscapes
            # the fields of this object haven't been analyzed yet: analyze them now
            nfields = fieldcount_noerror(typ)
            if nfields !== nothing
                AliasEscapes = FieldEscape[FieldEscape() for _ in 1:nfields]
                @goto record_field_escape
            end
            # unsuccessful field analysis: update obj's field information
            objinfo = escape_unanalyzable_obj!(astate, obj, objinfo)
        end
        @label conservative_propagation
        # the field couldn't be analyzed precisely: propagate the escape information
        # imposed on the return value of this `getfield` call to the object itself
        # as the most conservative propagation
        ssainfo = estate[SSAValue(pc)]
        add_escape_change!(astate, obj, ssainfo)
    elseif isa(AliasEscapes, FieldEscapes)
        nfields = fieldcount_noerror(typ)
        if nfields === nothing
            # unsuccessful field analysis: update obj's field information
            objinfo = escape_unanalyzable_obj!(astate, obj, objinfo)
            @goto conservative_propagation
        else
            AliasEscapes = copy(AliasEscapes)
            if nfields > length(AliasEscapes)
                for _ in 1:(nfields-length(AliasEscapes))
                    push!(AliasEscapes, FieldEscape())
                end
            end
        end
        # fields are known: record the return value of this `getfield` call as a possibility
        # that imposes escape on field(s) being referenced
        @label record_field_escape
        if isa(typ, DataType)
            fld = args[3]
            fldval = try_compute_field(ir, fld)
            fidx = try_compute_fieldidx(typ, fldval)
        else
            fidx = nothing
        end
        if fidx !== nothing
            # the field is known precisely: propagate this escape information to the field
            push!(AliasEscapes[fidx], iridx(SSAValue(pc), estate))
        else
            # the field isn't known precisely: propagate this escape information to all the fields
            for FieldEscape in AliasEscapes
                push!(FieldEscape, iridx(SSAValue(pc), estate))
            end
        end
        add_escape_change!(astate, obj, EscapeLattice(objinfo, AliasEscapes))
    else
        # this object has been used as array, but it is used as struct here (i.e. should throw)
        # update obj's field information and just handle this case conservatively
        @assert isa(AliasEscapes, ArrayEscapes)
        objinfo = escape_unanalyzable_obj!(astate, obj, objinfo)
        @goto conservative_propagation
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
    AliasEscapes = objinfo.AliasEscapes
    if isa(AliasEscapes, Bool)
        if !AliasEscapes
            # the fields of this object haven't been analyzed yet: analyze them now
            typ = widenconst(argextype(obj, ir))
            nfields = fieldcount_noerror(typ)
            if nfields !== nothing
                # successful field analysis: update obj's field information
                AliasEscapes = FieldEscape[FieldEscape() for _ in 1:nfields]
                objinfo = EscapeLattice(objinfo, AliasEscapes)
                add_escape_change!(astate, obj, objinfo)
                @goto add_field_escape
            end
            # unsuccessful field analysis: update obj's field information
            objinfo = escape_unanalyzable_obj!(astate, obj, objinfo)
        end
        @label conservative_propagation
        # the field couldn't be analyzed precisely: propagate the entire escape information
        # of this object to the value being assigned as the most conservative propagation
        add_escape_change!(astate, val, objinfo)
    elseif isa(AliasEscapes, FieldEscapes)
        typ = widenconst(argextype(obj, ir))
        nfields = fieldcount_noerror(typ)
        if nfields === nothing
            # unsuccessful field analysis: update obj's field information
            objinfo = escape_unanalyzable_obj!(astate, obj, objinfo)
            @goto conservative_propagation
        elseif nfields > length(AliasEscapes)
            AliasEscapes = copy(AliasEscapes)
            for _ in 1:(nfields-length(AliasEscapes))
                push!(AliasEscapes, FieldEscape())
            end
        end
        # fields are known: propagate escape information imposed on recorded possibilities
        @label add_field_escape
        if isa(typ, DataType)
            fld = args[3]
            fldval = try_compute_field(ir, fld)
            fidx = try_compute_fieldidx(typ, fldval)
        else
            fidx = nothing
        end
        if fidx !== nothing
            # the field is known precisely: propagate this escape information to the field
            escape_field!(astate, val, AliasEscapes[fidx])
        else
            # the field isn't known precisely: propagate this escape information to all the fields
            for FieldEscape in AliasEscapes
                escape_field!(astate, val, FieldEscape)
            end
        end
        # fields are known: propagate the escape information of this object ignoring field information
        add_escape_change!(astate, val, ignore_aliasescapes(objinfo))
    else
        # this object has been used as array, but it is "used" as struct here (i.e. should throw)
        # update obj's field information and just handle this case conservatively
        @assert isa(AliasEscapes, ArrayEscapes)
        objinfo = escape_unanalyzable_obj!(astate, obj, objinfo)
        @goto conservative_propagation
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

const Arrayish = Union{Array,Core.ImmutableArray}

function escape_builtin!(::typeof(arrayref), astate::AnalysisState, pc::Int, args::Vector{Any})
    length(args) ≥ 4 || return false
    # check potential thrown escapes from this arrayref call
    argtypes = Any[argextype(args[i], astate.ir) for i in 2:length(args)]
    boundcheckt = argtypes[1]
    aryt = argtypes[2]
    if !array_builtin_common_typecheck(Arrayish, boundcheckt, aryt, argtypes, 3)
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
    AliasEscapes = aryinfo.AliasEscapes
    ret = SSAValue(pc)
    if isa(AliasEscapes, Bool)
        if !AliasEscapes
            # the elements of this array haven't been analyzed yet: set ArrayEscapes now
            AliasEscapes = ArrayEscapes()
            @goto record_element_escape
        end
        @label conservative_propagation
        ssainfo = estate[ret]
        add_escape_change!(astate, ary, ssainfo)
    elseif isa(AliasEscapes, ArrayEscapes)
        # record the return value of this `arrayref` call as a possibility that imposes escape
        AliasEscapes = copy(AliasEscapes)
        @label record_element_escape
        push!(AliasEscapes, iridx(ret, estate))
        inbounds || push!(AliasEscapes, SSAValue(pc)) # record possible BoundsError at this arrayref
        add_escape_change!(astate, ary, EscapeLattice(aryinfo, AliasEscapes))
    else
        # this object has been used as struct, but it is used as array here (thus should throw)
        # update ary's element information and just handle this case conservatively
        @assert isa(AliasEscapes, FieldEscapes)
        aryinfo = escape_unanalyzable_obj!(astate, ary, aryinfo)
        @goto conservative_propagation
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
    if !(array_builtin_common_typecheck(Array, boundcheckt, aryt, argtypes, 4) &&
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
    AliasEscapes = aryinfo.AliasEscapes
    if isa(AliasEscapes, Bool)
        if !AliasEscapes
            # the elements of this array haven't been analyzed yet: don't need to consider ArrayEscapes for now
            @goto add_ary_escape
        end
        @label conservative_propagation
        add_escape_change!(astate, val, aryinfo)
    elseif isa(AliasEscapes, ArrayEscapes)
        for xidx in AliasEscapes
            if isa(xidx, Int)
                x = irval(xidx, estate)::SSAValue # TODO remove me once we implement ArgEscape
                add_escape_change!(astate, val, estate[x])
                add_alias_change!(astate, val, x)
            else
                add_escape_change!(astate, val, ThrownEscape((xidx::SSAValue).id))
            end
        end
        @label add_ary_escape
        add_escape_change!(astate, val, ignore_aliasescapes(aryinfo))
    else
        # this object has been used as struct, but it is "used" as array here (thus should throw)
        # update ary's element information and just handle this case conservatively
        @assert isa(AliasEscapes, FieldEscapes)
        aryinfo = escape_unanalyzable_obj!(astate, ary, aryinfo)
        @goto conservative_propagation
    end
    # also propagate escape information imposed on the return value of this `arrayset`
    ssainfo = estate[SSAValue(pc)]
    add_escape_change!(astate, ary, ssainfo)
    return true
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
    length(args) ≥ 6+ninds || return add_thrown_escapes!(astate, pc, args)
    ary = args[6]
    aryt = argextype(ary, astate.ir)
    aryt ⊑ₜ Array || return add_thrown_escapes!(astate, pc, args)
    for i in 1:ninds
        ind = args[i+6]
        indt = argextype(ind, astate.ir)
        indt ⊑ₜ Integer || return add_thrown_escapes!(astate, pc, args)
    end
    if boundserror
        # this array resizing can potentially throw `BoundsError`, impose it now
        add_escape_change!(astate, ary, ThrownEscape(pc))
    end
end

is_array_copy(name::Symbol) = name === :jl_array_copy

# FIXME this implementation is very conservative, improve the accuracy and solve broken test cases
function escape_array_copy!(astate::AnalysisState, pc::Int, args::Vector{Any})
    length(args) ≥ 6 || return add_thrown_escapes!(astate, pc, args)
    ary = args[6]
    aryt = argextype(ary, astate.ir)
    aryt ⊑ₜ Array || return add_thrown_escapes!(astate, pc, args)
    if isa(ary, SSAValue) || isa(ary, Argument)
        newary = SSAValue(pc)
        aryinfo = astate.estate[ary]
        newaryinfo = astate.estate[newary]
        add_escape_change!(astate, newary, aryinfo)
        add_escape_change!(astate, ary, newaryinfo)
    end
end

is_array_isassigned(name::Symbol) = name === :jl_array_isassigned

function escape_array_isassigned!(astate::AnalysisState, pc::Int, args::Vector{Any})
    if !array_isassigned_nothrow(args, astate.ir)
        add_thrown_escapes!(astate, pc, args)
    end
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

escape_builtin!(::typeof(arrayfreeze), astate::AnalysisState, pc::Int, args::Vector{Any}) =
    is_safe_immutable_array_op(Array, astate, pc, args)
escape_builtin!(::typeof(mutating_arrayfreeze), astate::AnalysisState, pc::Int, args::Vector{Any}) =
    is_safe_immutable_array_op(Array, astate, pc, args)
escape_builtin!(::typeof(arraythaw), astate::AnalysisState, pc::Int, args::Vector{Any}) =
    is_safe_immutable_array_op(ImmutableArray, astate, pc, args)
function is_safe_immutable_array_op(@nospecialize(arytype), astate::AnalysisState, pc::Int, args::Vector{Any})
    length(args) == 2 || return false
    argextype(args[2], astate.ir) ⊑ₜ arytype || return false
    return true
end

# NOTE define fancy package utilities when developing EA as an external package
if _TOP_MOD !== Core.Compiler
    include(@__MODULE__, "EAUtils.jl")
end

end # baremodule EscapeAnalysis

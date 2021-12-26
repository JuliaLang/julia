baremodule EscapeAnalysis

export
    find_escapes,
    GLOBAL_ESCAPE_CACHE,
    has_not_analyzed,
    has_no_escape,
    has_return_escape,
    has_thrown_escape,
    has_all_escape,
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
    ReturnNode, GotoNode, GotoIfNot, SimpleVector
import ._TOP_MOD:     # Base definitions
    @__MODULE__, @eval, @assert, @nospecialize, @inbounds, @inline, @noinline, @label, @goto,
    !, !==, !=, ≠, +, -, ≤, <, ≥, >, &, |, include, error, missing,
    Vector, BitSet, IdDict, IdSet, ∪, ⊆, ∩, :, length, get, first, last, in, isempty,
    isassigned, push!, empty!, max, min
import Core.Compiler: # Core.Compiler specific definitions
    isbitstype, isexpr, is_meta_expr_head, copy, println,
    IRCode, IR_FLAG_EFFECT_FREE, widenconst, argextype, singleton_type, fieldcount_noerror,
    try_compute_fieldidx, hasintersect

if isdefined(Core.Compiler, :try_compute_field)
    import Core.Compiler: try_compute_field
else
    function try_compute_field(ir::IRCode, @nospecialize(field))
        # fields are usually literals, handle them manually
        if isa(field, QuoteNode)
            field = field.value
        elseif isa(field, Int) || isa(field, Symbol)
        # try to resolve other constants, e.g. global reference
        else
            field = argextype(field, ir)
            if isa(field, Const)
                field = field.val
            else
                return nothing
            end
        end
        return isa(field, Union{Int, Symbol}) ? field : nothing
    end
end

if _TOP_MOD !== Core.Compiler
    include(@__MODULE__, "disjoint_set.jl")
else
    include(@__MODULE__, "compiler/EscapeAnalysis/disjoint_set.jl")
end

const EscapeSet  = BitSet # XXX better to be IdSet{Int}?
const EscapeSets = Vector{EscapeSet}

"""
    x::EscapeLattice

A lattice for escape information, which holds the following properties:
- `x.Analyzed::Bool`: not formally part of the lattice, indicates `x` has not been analyzed at all
- `x.ReturnEscape::Bool`: indicates `x` may escape to the caller via return,
  where `x.ReturnEscape && 0 ∈ x.EscapeSites` has the special meaning that it's visible to
  the caller simply because it's passed as call argument
- `x.ThrownEscape::Bool`: indicates `x` may escape to somewhere through an exception
- `x.EscapeSites::BitSet`: records SSA statements where `x` can escape via any of
  `ReturnEscape` or `ThrownEscape`
- `x.FieldEscapes::Union{Vector{IdSet{Any}},Bool}`: maintains all possible values that impose
  escape information on fields of `x`:
  * `x.FieldEscapes === false` indicates the fields of `x` isn't analyzed yet
  * `x.FieldEscapes === true` indicates the fields of `x` can't be analyzed, e.g. the type of `x`
    is not known or is not concrete and thus its fields can't be known precisely
  * otherwise `x.FieldEscapes::Vector{IdSet{Any}}` holds all the possible values that can escape
    fields of `x`, which allows EA to propagate propagate escape information imposed on a field
    of `x` to its values (by analyzing `Expr(:new, ...)` and `setfield!(x, ...)`).
- `x.ArgEscape::Int` (not implemented yet): indicates it will escape to the caller through
  `setfield!` on argument(s)
  * `-1` : no escape
  * `0` : unknown or multiple
  * `n` : through argument N

There are utility constructors to create common `EscapeLattice`s, e.g.,
- `NoEscape()`: the bottom element of this lattice, meaning it won't escape to anywhere
- `AllEscape()`: the topmost element of this lattice, meaning it will escape to everywhere

`find_escapes` will transition these elements from the bottom to the top,
in the same direction as Julia's native type inference routine.
An abstract state will be initialized with the bottom(-like) elements:
- the call arguments are initialized as `ArgumentReturnEscape()`, because they're visible from a caller immediately
- the other states are initialized as `NotAnalyzed()`, which is a special lattice element that
  is slightly lower than `NoEscape`, but at the same time doesn't represent any meaning
  other than it's not analyzed yet (thus it's not formally part of the lattice).
"""
struct EscapeLattice
    Analyzed::Bool
    ReturnEscape::Bool
    ThrownEscape::Bool
    EscapeSites::BitSet
    FieldEscapes::Union{EscapeSets,Bool}
    # TODO: ArgEscape::Int

    function EscapeLattice(Analyzed::Bool,
                           ReturnEscape::Bool,
                           ThrownEscape::Bool,
                           EscapeSites::BitSet,
                           FieldEscapes::Union{EscapeSets,Bool},
                           )
        @nospecialize FieldEscapes
        return new(
            Analyzed,
            ReturnEscape,
            ThrownEscape,
            EscapeSites,
            FieldEscapes,
            )
    end
    function EscapeLattice(x::EscapeLattice,
                           # non-concrete fields should be passed as default arguments
                           # in order to avoid allocating non-concrete `NamedTuple`s
                           FieldEscapes::Union{EscapeSets,Bool} = x.FieldEscapes;
                           Analyzed::Bool = x.Analyzed,
                           ReturnEscape::Bool = x.ReturnEscape,
                           ThrownEscape::Bool = x.ThrownEscape,
                           EscapeSites::BitSet = x.EscapeSites,
                           )
        @nospecialize FieldEscapes
        return new(
            Analyzed,
            ReturnEscape,
            ThrownEscape,
            EscapeSites,
            FieldEscapes,
            )
    end
end

# precomputed default values in order to eliminate computations at each callsite
const BOT_ESCAPE_SITES = BitSet()
const ARGUMENT_ESCAPE_SITES = BitSet(0)
const TOP_ESCAPE_SITES = BitSet(0:100_000)

const BOT_FIELD_SETS = false
const TOP_FIELD_SETS = true

# the constructors
NotAnalyzed() = EscapeLattice(false, false, false, BOT_ESCAPE_SITES, BOT_FIELD_SETS) # not formally part of the lattice
NoEscape() = EscapeLattice(true, false, false, BOT_ESCAPE_SITES, BOT_FIELD_SETS)
ReturnEscape(pc::Int) = EscapeLattice(true, true, false, BitSet(pc), BOT_FIELD_SETS)
ThrownEscape(pc::Int) = EscapeLattice(true, false, true, BitSet(pc), BOT_FIELD_SETS)
ArgumentReturnEscape() = EscapeLattice(true, true, false, ARGUMENT_ESCAPE_SITES, TOP_FIELD_SETS) # TODO allow interprocedural field analysis?
AllEscape() = EscapeLattice(true, true, true, TOP_ESCAPE_SITES, TOP_FIELD_SETS)

# Convenience names for some ⊑ queries
has_not_analyzed(x::EscapeLattice) = x == NotAnalyzed()
has_no_escape(x::EscapeLattice) = ignore_fieldsets(x) ⊑ NoEscape()
has_return_escape(x::EscapeLattice) = x.ReturnEscape
has_return_escape(x::EscapeLattice, pc::Int) = has_return_escape(x) && pc in x.EscapeSites
has_thrown_escape(x::EscapeLattice) = x.ThrownEscape
has_thrown_escape(x::EscapeLattice, pc::Int) = has_thrown_escape(x) && pc in x.EscapeSites
has_all_escape(x::EscapeLattice) = AllEscape() ⊑ x

ignore_fieldsets(x::EscapeLattice) = EscapeLattice(x, BOT_FIELD_SETS)
has_fieldsets(x::EscapeLattice) = !isa(x.FieldEscapes, Bool)

# TODO is_sroa_eligible: consider throwness?

"""
    is_sroa_eligible(x::EscapeLattice) -> Bool

Queries allocation eliminability by SROA.
"""
is_sroa_eligible(x::EscapeLattice) = x.FieldEscapes !== TOP_FIELD_SETS && !has_return_escape(x)

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
    xf, yf = x.FieldEscapes, y.FieldEscapes
    if isa(xf, Bool)
        isa(yf, Bool) || return false
        xf === yf || return false
    else
        isa(yf, Bool) && return false
        xf == yf || return false
    end
    return x.Analyzed === y.Analyzed &&
           x.ReturnEscape === y.ReturnEscape &&
           x.ThrownEscape === y.ThrownEscape &&
           x.EscapeSites == y.EscapeSites &&
           true
end

"""
    x::EscapeLattice ⊑ y::EscapeLattice -> Bool

The non-strict partial order over `EscapeLattice`.
"""
x::EscapeLattice ⊑ y::EscapeLattice = begin
    xf, yf = x.FieldEscapes, y.FieldEscapes
    if isa(xf, Bool)
        xf && yf !== true && return false
    else
        if isa(yf, Bool)
            yf === false && return false
        else
            xf, yf = xf::EscapeSets, yf::EscapeSets
            xn, yn = length(xf), length(yf)
            xn > yn && return false
            for i in 1:xn
                xf[i] ⊆ yf[i] || return false
            end
        end
    end
    if x.Analyzed ≤ y.Analyzed &&
       x.ReturnEscape ≤ y.ReturnEscape &&
       x.ThrownEscape ≤ y.ThrownEscape &&
       x.EscapeSites ⊆ y.EscapeSites &&
       true
        return true
    end
    return false
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
    xf, yf = x.FieldEscapes, y.FieldEscapes
    if xf === true || yf === true
        FieldEscapes = true
    elseif xf === false
        FieldEscapes = yf
    elseif yf === false
        FieldEscapes = xf
    else
        xf, yf = xf::EscapeSets, yf::EscapeSets
        xn, yn = length(xf), length(yf)
        nmax, nmin = max(xn, yn), min(xn, yn)
        FieldEscapes = EscapeSets(undef, nmax)
        for i in 1:nmax
            if i > nmin
                FieldEscapes[i] = (xn > yn ? xf : yf)[i]
            else
                FieldEscapes[i] = xf[i] ∪ yf[i]
            end
        end
    end
    # try to avoid new allocations as minor optimizations
    xe, ye = x.EscapeSites, y.EscapeSites
    if xe === TOP_ESCAPE_SITES || ye === TOP_ESCAPE_SITES
        EscapeSites = TOP_ESCAPE_SITES
    elseif xe === BOT_ESCAPE_SITES
        EscapeSites = ye
    elseif ye === BOT_ESCAPE_SITES
        EscapeSites = xe
    else
        EscapeSites = xe ∪ ye
    end
    return EscapeLattice(
        x.Analyzed | y.Analyzed,
        x.ReturnEscape | y.ReturnEscape,
        x.ThrownEscape | y.ThrownEscape,
        EscapeSites,
        FieldEscapes,
        )
end

"""
    x::EscapeLattice ⊓ y::EscapeLattice -> EscapeLattice

Computes the meet of `x` and `y` in the partial order defined by `EscapeLattice`.
"""
x::EscapeLattice ⊓ y::EscapeLattice = begin
    return EscapeLattice(
        x.Analyzed & y.Analyzed,
        x.ReturnEscape & y.ReturnEscape,
        x.ThrownEscape & y.ThrownEscape,
        x.EscapeSites ∩ y.EscapeSites,
        x.FieldEscapes, # FIXME
        )
end

# TODO setup a more effient struct for cache
# which can discard escape information on SSS values and arguments that don't join dispatch signature

const AliasSet = IntDisjointSet{Int}

"""
    estate::EscapeState

Extended lattice that maps arguments and SSA values to escape information represented as `EscapeLattice`:
- `estate.arguments::Vector{EscapeLattice}`: escape information about "arguments";
  note that "argument" can include both call arguments and slots appearing in analysis frame
- `ssavalues::Vector{EscapeLattice}`: escape information about each SSA value
- `aliaset::IntDisjointSet{Int}`: a disjoint set that maintains aliased arguments and SSA values
"""
struct EscapeState
    escapes::Vector{EscapeLattice}
    aliasset::AliasSet
    nargs::Int
end
function EscapeState(nargs::Int, nstmts::Int)
    escapes = EscapeLattice[
        1 ≤ i ≤ nargs ? ArgumentReturnEscape() : NotAnalyzed() for i in 1:(nargs+nstmts)]
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

`irval` can be used as an inverse function of `iridx`, i.e.
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

`iridx` can be used as an inverse function of `irval`, i.e.
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

if _TOP_MOD !== Core.Compiler
    struct EscapeCache
        state::EscapeState
        ir::IRCode # we preserve `IRCode` as well just for debugging purpose
    end
    const GLOBAL_ESCAPE_CACHE = IdDict{MethodInstance,EscapeCache}()
    argescapes_from_cache(cache::EscapeCache) =
        cache.state.escapes[1:cache.state.nargs]
else
    const GLOBAL_ESCAPE_CACHE = IdDict{MethodInstance,Vector{EscapeLattice}}()
    argescapes_from_cache(cache::Vector{EscapeLattice}) = cache
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
    find_escapes(ir::IRCode, nargs::Int) -> EscapeState

Analyzes escape information in `ir`.
`nargs` is the number of actual arguments of the analyzed call.
"""
function find_escapes(ir::IRCode, nargs::Int)
    stmts = ir.stmts
    nstmts = length(stmts)

    # only manage a single state, some flow-sensitivity is encoded as `EscapeLattice` properties
    estate = EscapeState(nargs, nstmts)
    changes = Changes() # stashes changes that happen at current statement
    astate = AnalysisState(ir, estate, changes)

    local debug_itr_counter = 0
    while true
        local anyupdate = false

        for pc in nstmts:-1:1
            stmt = stmts.inst[pc]

            # we escape statements with the `ThrownEscape` property using the effect-freeness
            # information computed by the inliner
            is_effect_free = stmts.flag[pc] & IR_FLAG_EFFECT_FREE ≠ 0

            # collect escape information
            if isa(stmt, Expr)
                head = stmt.head
                if head === :call
                    has_changes = escape_call!(astate, pc, stmt.args)
                    # TODO throwness ≠ "effect-free-ness"
                    if !is_effect_free
                        for x in stmt.args
                            add_escape_change!(astate, x, ThrownEscape(pc))
                        end
                    else
                        has_changes || continue
                    end
                elseif head === :invoke
                    escape_invoke!(astate, pc, stmt.args)
                elseif head === :new || head === :splatnew
                    escape_new!(astate, pc, stmt.args)
                elseif head === :(=)
                    lhs, rhs = stmt.args
                    if isa(lhs, GlobalRef) # global store
                        add_escape_change!(astate, rhs, AllEscape())
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
                elseif head === :static_parameter
                    # :static_parameter refers any of static parameters, but since they exist
                    # statically, we're really not interested in their escapes
                    continue
                elseif head === :copyast
                    # copyast simply copies a surface syntax AST, and should never use any of arguments or SSA values
                    continue
                elseif head === :undefcheck
                    # undefcheck is temporarily inserted by compiler
                    # it will be processd be later pass so it won't change any of escape states
                    continue
                elseif head === :the_exception
                    # we don't propagate escape information on exceptions via this expression, but rather
                    # use a dedicated lattice property `ThrownEscape`
                    continue
                elseif head === :isdefined
                    # just returns `Bool`, nothing accounts for any usages
                    continue
                elseif head === :enter || head === :leave || head === :pop_exception
                    # these exception frame managements doesn't account for any usages
                    # we can just ignore escape information from
                    continue
                elseif head === :gc_preserve_begin || head === :gc_preserve_end
                    # `GC.@preserve` may "use" arbitrary values, but we can just ignore the escape information
                    # imposed on `GC.@preserve` expressions since they're supposed to never be used elsewhere
                    continue
                else
                    for x in stmt.args
                        add_escape_change!(astate, x, AllEscape())
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
                add_escape_change!(astate, SSAValue(pc), AllEscape())
            elseif isa(stmt, SSAValue)
                # NOTE after SROA, we may see SSA value as statement
                info = estate[SSAValue(pc)]
                add_escape_change!(astate, stmt, info)
                add_alias_change!(astate, stmt, SSAValue(pc))
            else
                @assert stmt isa GotoNode || stmt isa GotoIfNot || stmt === nothing # TODO remove me
                continue
            end

            isempty(changes) && continue

            anyupdate |= propagate_changes!(estate, changes)

            empty!(changes)
        end

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

function escape_val!(astate::AnalysisState, pc::Int, x)
    if isdefined(x, :val)
        info = astate.estate[SSAValue(pc)]
        add_escape_change!(astate, x.val, info)
        add_alias_change!(astate, SSAValue(pc), x.val)
    end
end

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

function escape_invoke!(astate::AnalysisState, pc::Int, args::Vector{Any})
    linfo = first(args)::MethodInstance
    cache = get(GLOBAL_ESCAPE_CACHE, linfo, nothing)
    args = args[2:end]
    if cache === nothing
        for x in args
            add_escape_change!(astate, x, AllEscape())
        end
    else
        argescapes = argescapes_from_cache(cache)
        retinfo = astate.estate[SSAValue(pc)] # escape information imposed on the call statement
        method = linfo.def::Method
        nargs = Int(method.nargs)
        for i in 1:length(args)
            arg = args[i]
            if i ≤ nargs
                argi = i
            else # handle isva signature: COMBAK will this be invalid once we take alias information into account ?
                argi = nargs
            end
            arginfo = argescapes[argi]
            isempty(arginfo.ReturnEscape) && invalid_escape_invoke!(astate, linfo, linfo_estate)
            info = from_interprocedural(arginfo, retinfo, pc)
            add_escape_change!(astate, arg, info)
        end
    end
end

# reinterpret the escape information imposed on the callee argument (`arginfo`) in the
# context of the caller frame using the escape information imposed on the return value (`retinfo`)
function from_interprocedural(arginfo::EscapeLattice, retinfo::EscapeLattice, pc::Int)
    @assert arginfo.ReturnEscape
    if arginfo.ThrownEscape
        EscapeSites = BitSet(pc)
    else
        EscapeSites = BOT_ESCAPE_SITES
    end
    newarginfo = EscapeLattice(
        #=Analyzed=#true, #=ReturnEscape=#false, arginfo.ThrownEscape, EscapeSites,
        # FIXME implement interprocedural effect-analysis
        # currently, this essentially disables the entire field analysis
        # it might be okay from the SROA point of view, since we can't remove the allocation
        # as far as it's passed to a callee anyway, but still we may want some field analysis
        # in order to stack allocate it
        TOP_FIELD_SETS)
    if arginfo.EscapeSites === ARGUMENT_ESCAPE_SITES
        # if this is simply passed as the call argument, we can discard the `ReturnEscape`
        # information and just propagate the other escape information
        return newarginfo
    else
        # if this can be returned, we have to merge its escape information with
        # that of the current statement
        return newarginfo ⊔ retinfo
    end
end

@noinline function invalid_escape_invoke!(astate::AnalysisState, linfo::MethodInstance, linfo_estate::EscapeState)
    @eval Main (astate = $astate; linfo = $linfo; linfo_estate = $linfo_estate)
    error("invalid escape lattice element returned from inter-procedural context: inspect `Main.astate`, `Main.linfo` and `Main.linfo_estate`")
end

@noinline function invalid_escape_assignment!(ir::IRCode, pc::Int)
    @eval Main (ir = $ir; pc = $pc)
    error("unexpected assignment found: inspect `Main.pc` and `Main.pc`")
end

function escape_new!(astate::AnalysisState, pc::Int, args::Vector{Any})
    objinfo = astate.estate[SSAValue(pc)]
    if objinfo == NotAnalyzed()
        objinfo = NoEscape()
    end
    FieldEscapes = objinfo.FieldEscapes
    nargs = length(args)
    if isa(FieldEscapes, Bool)
        # the fields couldn't be analyzed precisely: directly propagate the escape information
        # of this object to all its fields (which is the most conservative option)
        for i in 2:nargs
            add_escape_change!(astate, args[i], objinfo)
        end
    else
        # fields are known: propagate escape information imposed on recorded possibilities
        nf = length(FieldEscapes)
        for i in 2:nargs
            i-1 > nf && break # may happen when e.g. ϕ-node merges values with different types
            escape_field!(astate, args[i], FieldEscapes[i-1])
        end
    end
end

function escape_field!(astate::AnalysisState, @nospecialize(v), FieldEscape::EscapeSet)
    estate = astate.estate
    for xidx in FieldEscape
        x = irval(xidx, estate)::SSAValue # TODO remove me once we implement ArgEscape
        add_escape_change!(astate, v, estate[x])
        add_alias_change!(astate, v, x)
    end
end

# escape every argument `(args[6:length(args[3])])` and the name `args[1]`
# TODO: we can apply a similar strategy like builtin calls to specialize some foreigncalls
function escape_foreigncall!(astate::AnalysisState, pc::Int, args::Vector{Any})
    foreigncall_nargs = length((args[3])::SimpleVector)
    name = args[1]
    # if normalize(name) === :jl_gc_add_finalizer_th
    #     # add `FinalizerEscape` ?
    # end
    add_escape_change!(astate, name, ThrownEscape(pc))
    for i in 6:5+foreigncall_nargs
        add_escape_change!(astate, args[i], ThrownEscape(pc))
    end
end

# NOTE error cases will be handled in `find_escapes` anyway, so we don't need to take care of them below
# TODO implement more builtins, make them more accurate
# TODO use `T_IFUNC`-like logic and don't not abuse dispatch ?

function escape_call!(astate::AnalysisState, pc::Int, args::Vector{Any})
    ir = astate.ir
    ft = argextype(first(args), ir, ir.sptypes, ir.argtypes)
    f = singleton_type(ft)
    if isa(f, Core.IntrinsicFunction)
        return false # COMBAK we may break soundness here, e.g. `pointerref`
    end
    result = escape_builtin!(f, astate, pc, args)
    if result === false
        return false # nothing to propagate
    elseif result === missing
        # if this call hasn't been handled by any of pre-defined handlers,
        # we escape this call conservatively
        for i in 2:length(args)
            add_escape_change!(astate, args[i], AllEscape())
        end
        return true
    else
        return true
    end
end

escape_builtin!(@nospecialize(f), _...) = return missing

# safe builtins
escape_builtin!(::typeof(isa), _...) = return false
escape_builtin!(::typeof(typeof), _...) = return false
escape_builtin!(::typeof(Core.sizeof), _...) = return false
escape_builtin!(::typeof(===), _...) = return false
# not really safe, but `ThrownEscape` will be imposed later
escape_builtin!(::typeof(isdefined), _...) = return false
escape_builtin!(::typeof(throw), _...) = return false

function escape_builtin!(::typeof(Core.ifelse), astate::AnalysisState, pc::Int, args::Vector{Any})
    length(args) == 4 || return nothing
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
    return nothing
end

function escape_builtin!(::typeof(typeassert), astate::AnalysisState, pc::Int, args::Vector{Any})
    length(args) == 3 || return nothing
    f, obj, typ = args
    ret = SSAValue(pc)
    info = astate.estate[ret]
    add_escape_change!(astate, obj, info)
    add_alias_change!(astate, ret, obj)
    return nothing
end

function escape_builtin!(::typeof(tuple), astate::AnalysisState, pc::Int, args::Vector{Any})
    escape_new!(astate, pc, args)
    return nothing
end

function escape_builtin!(::typeof(getfield), astate::AnalysisState, pc::Int, args::Vector{Any})
    length(args) ≥ 3 || return nothing
    ir, estate = astate.ir, astate.estate
    obj = args[2]
    typ = widenconst(argextype(obj, ir))
    if hasintersect(typ, Module) # global load
        add_escape_change!(astate, SSAValue(pc), AllEscape())
    end
    if isa(obj, SSAValue) || isa(obj, Argument)
        objinfo = estate[obj]
    else
        return
    end
    FieldEscapes = objinfo.FieldEscapes
    if isa(FieldEscapes, Bool)
        if !FieldEscapes
            # the fields of this object aren't analyzed yet: analyze them now
            nfields = fieldcount_noerror(typ)
            if nfields !== nothing
                FieldEscapes = EscapeSet[EscapeSet() for _ in 1:nfields]
                @goto add_field_escape
            end
        end
        # the field couldn't be analyzed precisely: directly propagate the escape information
        # imposed on the return value of this `getfield` call to the object (which is the most conservative option)
        # but also with updated field information
        ssainfo = estate[SSAValue(pc)]
        if ssainfo == NotAnalyzed()
            ssainfo = NoEscape()
        end
        add_escape_change!(astate, obj, EscapeLattice(ssainfo, TOP_FIELD_SETS))
    else
        # fields are known: record the return value of this `getfield` call as a possibility that imposes escape
        FieldEscapes = copy(FieldEscapes)
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
            push!(FieldEscapes[fidx], iridx(SSAValue(pc), estate))
        else
            # the field isn't known precisely: propagate this escape information to all the fields
            for FieldEscape in FieldEscapes
                push!(FieldEscape, iridx(SSAValue(pc), estate))
            end
        end
        add_escape_change!(astate, obj, EscapeLattice(objinfo, FieldEscapes))
    end
    return nothing
end

function escape_builtin!(::typeof(setfield!), astate::AnalysisState, pc::Int, args::Vector{Any})
    length(args) ≥ 4 || return nothing
    ir, estate = astate.ir, astate.estate
    obj, fld, val = args[2:4]
    if isa(obj, SSAValue) || isa(obj, Argument)
        objinfo = estate[obj]
    else
        # unanalyzable object (e.g. obj::GlobalRef): escape field value conservatively
        add_escape_change!(astate, val, AllEscape())
        return
    end
    FieldEscapes = objinfo.FieldEscapes
    if isa(FieldEscapes, Bool)
        if !FieldEscapes
            # the fields of this object aren't analyzed yet: analyze them now
            typ = widenconst(argextype(obj, ir))
            nfields = fieldcount_noerror(typ)
            if nfields !== nothing
                # unsuccessful field analysis: update obj's escape information with new field information
                FieldEscapes = EscapeSet[EscapeSet() for _ in 1:nfields]
                objinfo = EscapeLattice(objinfo, FieldEscapes)
                add_escape_change!(astate, obj, objinfo)
                @goto add_field_escape
            end
            # unsuccessful field analysis: update obj's escape information with new field information
            objinfo = EscapeLattice(objinfo, TOP_FIELD_SETS)
            add_escape_change!(astate, obj, objinfo)
        end
        # the field couldn't be analyzed precisely: directly propagate the escape information
        # of this object to the field (which is the most conservative option)
        add_escape_change!(astate, val, objinfo)
    else
        # fields are known: propagate escape information imposed on recorded possibilities
        typ = widenconst(argextype(obj, ir))
        @label add_field_escape
        if isa(typ, DataType)
            fldval = try_compute_field(ir, fld)
            fidx = try_compute_fieldidx(typ, fldval)
        else
            fidx = nothing
        end
        if fidx !== nothing
            # the field is known precisely: propagate this escape information to the field
            escape_field!(astate, val, FieldEscapes[fidx])
        else
            # the field isn't known precisely: propagate this escape information to all the fields
            for FieldEscape in FieldEscapes
                escape_field!(astate, val, FieldEscape)
            end
        end
    end
    # also propagate escape information imposed on the return value of this `setfield!`
    ssainfo = estate[SSAValue(pc)]
    if ssainfo == NotAnalyzed()
        ssainfo = NoEscape()
    end
    add_escape_change!(astate, val, ssainfo)
    return nothing
end

# NOTE define fancy package utilities when developing EA as an external package
if _TOP_MOD !== Core.Compiler
    include(@__MODULE__, "utils.jl")
end

end # baremodule EscapeAnalysis

baremodule EscapeAnalysis

export
    find_escapes,
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
import ._TOP_MOD: ==
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

const EscapeSet  = IdSet{Any}
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

"""
    state::EscapeState

Extended lattice that maps arguments and SSA values to escape information represented as `EscapeLattice`:
- `state.arguments::Vector{EscapeLattice}`: escape information about "arguments";
  note that "argument" can include both call arguments and slots appearing in analysis frame
- `ssavalues::Vector{EscapeLattice}`: escape information about each SSA value
- `aliaset::IntDisjointSet{Int}`: a disjoint set that maintains aliased arguments and SSA values
"""
struct EscapeState
    arguments::Vector{EscapeLattice}
    ssavalues::Vector{EscapeLattice}
    aliasset::IntDisjointSet{Int}
end
function EscapeState(nslots::Int, nargs::Int, nstmts::Int)
    arguments = EscapeLattice[
        1 ≤ i ≤ nargs ? ArgumentReturnEscape() : NotAnalyzed() for i in 1:nslots]
    ssavalues = EscapeLattice[NotAnalyzed() for _ in 1:nstmts]
    aliaset = AliasSet(nslots+nstmts)
    return EscapeState(arguments, ssavalues, aliaset)
end

const AliasSet = IntDisjointSet{Int}
function alias_idx(@nospecialize(x), ir::IRCode)
    if isa(x, Argument)
        return x.n
    elseif isa(x, SSAValue)
        return x.id + length(ir.argtypes)
    else
        return nothing
    end
end
function alias_val(idx::Int, ir::IRCode)
    n = length(ir.argtypes)
    return idx > n ? SSAValue(idx-n) : Argument(idx)
end
function get_aliases(aliasset::AliasSet, @nospecialize(key), ir::IRCode)
    idx = alias_idx(key, ir)
    idx === nothing && return nothing
    root = find_root!(aliasset, idx)
    if idx ≠ root || aliasset.ranks[idx] > 0
        # the size of this alias set containing `key` is larger than 1,
        # collect the entire alias set
        aliases = Union{Argument,SSAValue}[]
        for i in 1:length(aliasset.parents)
            if aliasset.parents[i] == root
                push!(aliases, alias_val(i, ir))
            end
        end
        return aliases
    else
        return nothing
    end
end

# we preserve `IRCode` as well just for debugging purpose
const GLOBAL_ESCAPE_CACHE = IdDict{MethodInstance,Tuple{EscapeState,IRCode}}()
__clear_escape_cache!() = empty!(GLOBAL_ESCAPE_CACHE)

const EscapeChange = Pair{Union{Argument,SSAValue},EscapeLattice}
const AliasChange  = Pair{Int,Int}
const Changes      = Vector{Union{EscapeChange,AliasChange}}

"""
    find_escapes(ir::IRCode, nargs::Int) -> EscapeState

Analyzes escape information in `ir`.
`nargs` is the number of actual arguments of the analyzed call.
"""
function find_escapes(ir::IRCode, nargs::Int)
    (; stmts, sptypes, argtypes) = ir
    nstmts = length(stmts)

    # only manage a single state, some flow-sensitivity is encoded as `EscapeLattice` properties
    state = EscapeState(length(argtypes), nargs, nstmts)
    changes = Changes() # stashes changes that happen at current statement

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
                    has_changes = escape_call!(ir, pc, stmt.args, state, changes)
                    # TODO throwness ≠ "effect-free-ness"
                    if !is_effect_free
                        for x in stmt.args
                            add_escape_change!(x, ir, ThrownEscape(pc), changes)
                        end
                    else
                        has_changes || continue
                    end
                elseif head === :invoke
                    escape_invoke!(ir, pc, stmt.args, state, changes)
                elseif head === :new || head === :splatnew
                    escape_new!(ir, pc, stmt.args, state, changes)
                elseif head === :(=)
                    lhs, rhs = stmt.args
                    if isa(lhs, GlobalRef) # global store
                        add_escape_change!(rhs, ir, AllEscape(), changes)
                    else
                        invalid_escape_assignment!(ir, pc)
                    end
                elseif head === :foreigncall
                    escape_foreigncall!(ir, pc, stmt.args, state, changes)
                elseif head === :throw_undef_if_not # XXX when is this expression inserted ?
                    add_escape_change!(stmt.args[1], ir, ThrownEscape(pc), changes)
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
                        add_escape_change!(x, ir, AllEscape(), changes)
                    end
                end
            elseif isa(stmt, ReturnNode)
                if isdefined(stmt, :val)
                    add_escape_change!(stmt.val, ir, ReturnEscape(pc), changes)
                end
            elseif isa(stmt, PhiNode)
                escape_edges!(ir, pc, stmt.values, state, changes)
            elseif isa(stmt, PiNode)
                escape_val!(ir, pc, stmt, state, changes)
            elseif isa(stmt, PhiCNode)
                escape_edges!(ir, pc, stmt.values, state, changes)
            elseif isa(stmt, UpsilonNode)
                escape_val!(ir, pc, stmt, state, changes)
            elseif isa(stmt, GlobalRef) # global load
                add_escape_change!(SSAValue(pc), ir, AllEscape(), changes)
            elseif isa(stmt, SSAValue)
                # NOTE after SROA, we may see SSA value as statement
                info = state.ssavalues[pc]
                add_escape_change!(stmt, ir, info, changes)
                add_alias_change!(stmt, SSAValue(pc), ir, changes)
            else
                @assert stmt isa GotoNode || stmt isa GotoIfNot || stmt === nothing # TODO remove me
                continue
            end

            isempty(changes) && continue

            anyupdate |= propagate_changes!(state, changes, ir)

            empty!(changes)
        end

        debug_itr_counter += 1

        anyupdate || break
    end

    # if debug_itr_counter > 2
    #     println("[EA] excessive iteration count found ", debug_itr_counter, " (", singleton_type(ir.argtypes[1]), ")")
    # end

    return state
end

# propagate changes, and check convergence
function propagate_changes!(state::EscapeState, changes::Changes, ir::IRCode)
    local anychanged = false
    for change in changes
        if isa(change, EscapeChange)
            anychanged |= propagate_escape_change!(state, change)
            x, info = change
            aliases = get_aliases(state.aliasset, x, ir)
            if aliases !== nothing
                for alias in aliases
                    morechange = EscapeChange(alias, info)
                    anychanged |= propagate_escape_change!(state, morechange)
                end
            end
        else
            anychanged |= propagate_alias_change!(state, change)
        end
    end
    return anychanged
end

function propagate_escape_change!(state::EscapeState, change::EscapeChange)
    x, info = change
    if isa(x, Argument)
        old = state.arguments[x.n]
        new = old ⊔ info
        if old ≠ new
            state.arguments[x.n] = new
            return true
        end
    else
        x = x::SSAValue
        old = state.ssavalues[x.id]
        new = old ⊔ info
        if old ≠ new
            state.ssavalues[x.id] = new
            return true
        end
    end
    return false
end

function propagate_alias_change!(state::EscapeState, change::AliasChange)
    x, y = change
    xroot = find_root!(state.aliasset, x)
    yroot = find_root!(state.aliasset, y)
    if xroot ≠ yroot
        union!(state.aliasset, xroot, yroot)
        return true
    end
    return false
end

function add_escape_change!(@nospecialize(x), ir::IRCode, info::EscapeLattice, changes::Changes)
    if isa(x, Argument) || isa(x, SSAValue)
        if !isbitstype(widenconst(argextype(x, ir)))
            push!(changes, EscapeChange(x, info))
        end
    end
end

function add_alias_change!(@nospecialize(x), @nospecialize(y), ir::IRCode, changes::Changes)
    xidx = alias_idx(x, ir)
    yidx = alias_idx(y, ir)
    if xidx !== nothing && yidx !== nothing
        push!(changes, AliasChange(xidx, yidx))
    end
end

function escape_edges!(ir::IRCode, pc::Int, edges::Vector{Any},
                       state::EscapeState, changes::Changes)
    info = state.ssavalues[pc]
    for i in 1:length(edges)
        if isassigned(edges, i)
            v = edges[i]
            add_escape_change!(v, ir, info, changes)
            add_alias_change!(SSAValue(pc), v, ir, changes)
        end
    end
end

function escape_val!(ir::IRCode, pc::Int, x, state::EscapeState, changes::Changes)
    if isdefined(x, :val)
        info = state.ssavalues[pc]
        add_escape_change!(x.val, ir, info, changes)
        add_alias_change!(SSAValue(pc), x.val, ir, changes)
    end
end

# NOTE if we don't maintain the alias set that is separated from the lattice state, we can do
# soemthing like below: it essentially incorporates forward escape propagation in our default
# backward propagation, and leads to inefficient convergence that requires more iterations
# # lhs = rhs: propagate escape information of `rhs` to `lhs`
# function escape_alias!(@nospecialize(lhs), @nospecialize(rhs),
#     ir::IRCode, state::EscapeState, changes::Changes)
#     if isa(rhs, SSAValue)
#         vinfo = state.ssavalues[rhs.id]
#     elseif isa(rhs, Argument)
#         vinfo = state.arguments[rhs.n]
#     else
#         return
#     end
#     add_escape_change!(lhs, ir, vinfo, changes)
# end

function escape_invoke!(ir::IRCode, pc::Int, args::Vector{Any},
                        state::EscapeState, changes::Changes)
    linfo = first(args)::MethodInstance
    cache = get(GLOBAL_ESCAPE_CACHE, linfo, nothing)
    args = args[2:end]
    if cache === nothing
        for x in args
            add_escape_change!(x, ir, AllEscape(), changes)
        end
    else
        (linfostate, #=, ir::IRCode=#) = cache
        retinfo = state.ssavalues[pc] # escape information imposed on the call statement
        method = linfo.def::Method
        nargs = Int(method.nargs)
        for i in 1:length(args)
            arg = args[i]
            if i ≤ nargs
                arginfo = linfostate.arguments[i]
            else # handle isva signature: COMBAK will this be invalid once we take alias information into account ?
                arginfo = linfostate.arguments[nargs]
            end
            isempty(arginfo.ReturnEscape) && invalid_escape_invoke!(ir, linfo)
            info = from_interprocedural(arginfo, retinfo, pc)
            add_escape_change!(arg, ir, info, changes)
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

@noinline function invalid_escape_invoke!(ir::IRCode, linfo::MethodInstance)
    @eval Main (ir = $ir; linfo = $linfo)
    error("invalid escape lattice element returned from inter-procedural context: inspect `Main.ir` and `Main.linfo`")
end

@noinline function invalid_escape_assignment!(ir::IRCode, pc::Int)
    @eval Main (ir = $ir; pc = $pc)
    error("unexpected assignment found: inspect `Main.pc` and `Main.pc`")
end

function escape_new!(ir::IRCode, pc::Int, args::Vector{Any},
                     state::EscapeState, changes::Changes)
    objinfo = state.ssavalues[pc]
    if objinfo == NotAnalyzed()
        objinfo = NoEscape()
    end
    FieldEscapes = objinfo.FieldEscapes
    nargs = length(args)
    if isa(FieldEscapes, Bool)
        # the fields couldn't be analyzed precisely: directly propagate the escape information
        # of this object to all its fields (which is the most conservative option)
        for i in 2:nargs
            add_escape_change!(args[i], ir, objinfo, changes)
        end
    else
        # fields are known: propagate escape information imposed on recorded possibilities
        nf = length(FieldEscapes)
        for i in 2:nargs
            i-1 > nf && break # may happen when e.g. ϕ-node merges values with different types
            escape_field!(args[i], FieldEscapes[i-1], ir, state, changes)
        end
    end
end

function escape_field!(@nospecialize(v), FieldEscape::EscapeSet, ir::IRCode, state::EscapeState, changes::Changes)
    for x in FieldEscape
        if isa(x, SSAValue)
            add_escape_change!(v, ir, state.ssavalues[x.id], changes)
        elseif isa(x, Argument)
            add_escape_change!(v, ir, state.arguments[x.n], changes)
        else
            continue
        end
        add_alias_change!(v, x, ir, changes)
    end
end

# escape every argument `(args[6:length(args[3])])` and the name `args[1]`
# TODO: we can apply a similar strategy like builtin calls to specialize some foreigncalls
function escape_foreigncall!(ir::IRCode, pc::Int, args::Vector{Any},
                             state::EscapeState, changes::Changes)
    foreigncall_nargs = length((args[3])::SimpleVector)
    name = args[1]
    # if normalize(name) === :jl_gc_add_finalizer_th
    #     # add `FinalizerEscape` ?
    # end
    add_escape_change!(name, ir, ThrownEscape(pc), changes)
    for i in 6:5+foreigncall_nargs
        add_escape_change!(args[i], ir, ThrownEscape(pc), changes)
    end
end

# NOTE error cases will be handled in `find_escapes` anyway, so we don't need to take care of them below
# TODO implement more builtins, make them more accurate
# TODO use `T_IFUNC`-like logic and don't not abuse dispatch ?

function escape_call!(ir::IRCode, pc::Int, args::Vector{Any},
                      state::EscapeState, changes::Changes)
    ft = argextype(first(args), ir, ir.sptypes, ir.argtypes)
    f = singleton_type(ft)
    if isa(f, Core.IntrinsicFunction)
        return false # COMBAK we may break soundness here, e.g. `pointerref`
    end
    result = escape_builtin!(f, ir, pc, args, state, changes)
    if result === false
        return false # nothing to propagate
    elseif result === missing
        # if this call hasn't been handled by any of pre-defined handlers,
        # we escape this call conservatively
        for i in 2:length(args)
            add_escape_change!(args[i], ir, AllEscape(), changes)
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

function escape_builtin!(::typeof(Core.ifelse), ir::IRCode, pc::Int, args::Vector{Any}, state::EscapeState, changes::Changes)
    length(args) == 4 || return nothing
    f, cond, th, el = args
    info = state.ssavalues[pc]
    condt = argextype(cond, ir)
    ret = SSAValue(pc)
    if isa(condt, Const) && (cond = condt.val; isa(cond, Bool))
        if cond
            add_escape_change!(th, ir, info, changes)
            add_alias_change!(th, ret, ir, changes)
        else
            add_escape_change!(el, ir, info, changes)
            add_alias_change!(el, ret, ir, changes)
        end
    else
        add_escape_change!(th, ir, info, changes)
        add_escape_change!(el, ir, info, changes)
        add_alias_change!(th, ret, ir, changes)
        add_alias_change!(el, ret, ir, changes)
    end
    return nothing
end

function escape_builtin!(::typeof(typeassert), ir::IRCode, pc::Int, args::Vector{Any}, state::EscapeState, changes::Changes)
    length(args) == 3 || return nothing
    f, obj, typ = args
    info = state.ssavalues[pc]
    add_escape_change!(obj, ir, info, changes)
    add_alias_change!(SSAValue(pc), obj, ir, changes)
    return nothing
end

function escape_builtin!(::typeof(tuple), ir::IRCode, pc::Int, args::Vector{Any}, state::EscapeState, changes::Changes)
    escape_new!(ir, pc, args, state, changes)
    return nothing
end

function escape_builtin!(::typeof(getfield), ir::IRCode, pc::Int, args::Vector{Any}, state::EscapeState, changes::Changes)
    length(args) ≥ 3 || return nothing
    obj = args[2]
    typ = widenconst(argextype(obj, ir))
    if hasintersect(typ, Module) # global load
        add_escape_change!(SSAValue(pc), ir, AllEscape(), changes)
    end
    if isa(obj, SSAValue)
        objinfo = state.ssavalues[obj.id]
    elseif isa(obj, Argument)
        objinfo = state.arguments[obj.n]
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
        ssainfo = state.ssavalues[pc]
        if ssainfo == NotAnalyzed()
            ssainfo = NoEscape()
        end
        add_escape_change!(obj, ir, EscapeLattice(ssainfo, TOP_FIELD_SETS), changes)
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
            push!(FieldEscapes[fidx], SSAValue(pc))
        else
            # the field isn't known precisely: propagate this escape information to all the fields
            for FieldEscape in FieldEscapes
                push!(FieldEscape, SSAValue(pc))
            end
        end
        add_escape_change!(obj, ir, EscapeLattice(objinfo, FieldEscapes), changes)
    end
    return nothing
end

function escape_builtin!(::typeof(setfield!), ir::IRCode, pc::Int, args::Vector{Any}, state::EscapeState, changes::Changes)
    length(args) ≥ 4 || return nothing
    obj, fld, val = args[2:4]
    if isa(obj, SSAValue)
        objinfo = state.ssavalues[obj.id]
    elseif isa(obj, Argument)
        objinfo = state.arguments[obj.n]
    else
        # unanalyzable object (e.g. obj::GlobalRef): escape field value conservatively
        add_escape_change!(val, ir, AllEscape(), changes)
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
                add_escape_change!(obj, ir, objinfo, changes)
                @goto add_field_escape
            end
            # unsuccessful field analysis: update obj's escape information with new field information
            objinfo = EscapeLattice(objinfo, TOP_FIELD_SETS)
            add_escape_change!(obj, ir, objinfo, changes)
        end
        # the field couldn't be analyzed precisely: directly propagate the escape information
        # of this object to the field (which is the most conservative option)
        add_escape_change!(val, ir, objinfo, changes)
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
            escape_field!(val, FieldEscapes[fidx], ir, state, changes)
        else
            # the field isn't known precisely: propagate this escape information to all the fields
            for FieldEscape in FieldEscapes
                escape_field!(val, FieldEscape, ir, state, changes)
            end
        end
    end
    # also propagate escape information imposed on the return value of this `setfield!`
    ssainfo = state.ssavalues[pc]
    if ssainfo == NotAnalyzed()
        ssainfo = NoEscape()
    end
    add_escape_change!(val, ir, ssainfo, changes)
    return nothing
end

# NOTE define fancy package utilities when developing EA as an external package
if _TOP_MOD !== Core.Compiler
    include(@__MODULE__, "utils.jl")
end

end # baremodule EscapeAnalysis

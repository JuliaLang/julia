baremodule EscapeAnalysis

export
    find_escapes,
    has_not_analyzed,
    has_no_escape,
    has_return_escape,
    has_thrown_escape,
    has_all_escape,
    can_elide_finalizer

# analysis
# ========

const _TOP_MOD = ccall(:jl_base_relative_to, Any, (Any,), EscapeAnalysis)::Module

# imports
import ._TOP_MOD: ==
# usings
import Core:
    MethodInstance,
    Const,
    Argument,
    SSAValue,
    PiNode,
    PhiNode,
    UpsilonNode,
    PhiCNode,
    ReturnNode,
    GotoNode,
    GotoIfNot,
    SimpleVector
import ._TOP_MOD:     # Base definitions
    @eval, @assert, @nospecialize, @__MODULE__, Vector, BitSet, IdDict,
    !, !==, ≠, +, -, ≤, &, |, include, error, missing, println,
    ∪, ⊆, ∩, :, length, get, first, last, in, isempty, isassigned, push!, empty!
import Core.Compiler: # Core.Compiler specific definitions
    IRCode, IR_FLAG_EFFECT_FREE,
    isbitstype, isexpr, is_meta_expr_head, widenconst, argextype, singleton_type

"""
    x::EscapeLattice

A lattice for escape information, which holds the following properties:
- `x.Analyzed::Bool`: not formally part of the lattice, indicates `x` has not been analyzed at all
- `x.ReturnEscape::Bool`: indicates `x` may escape to the caller via return (possibly as a field),
    where `x.ReturnEscape && 0 ∈ x.EscapeSites` has the special meaning that it's visible to
    the caller simply because it's passed as call argument
- `x.ThrownEscape::Bool`: indicates `x` may escape to somewhere through an exception (possibly as a field)
- `x.EscapeSites::BitSet`: records program counters (SSA numbers) where `x` can escape
- `x.ArgEscape::Int` (not implemented yet): indicates it will escape to the caller through `setfield!` on argument(s)
  * `-1` : no escape
  * `0` : unknown or multiple
  * `n` : through argument N

These attributes can be combined to create a partial lattice that has a finite height, given
that input program has a finite number of statements, which is assured by Julia's semantics.

There are utility constructors to create common `EscapeLattice`s, e.g.,
- `NoEscape()`: the bottom element of this lattice, meaning it won't escape to anywhere
- `AllEscape()`: the topmost element of this lattice, meaning it will escape to everywhere

The escape analysis will transition these elements from the bottom to the top,
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
    # TODO: ArgEscape::Int
end

# precomputed default values in order to eliminate computations at each callsite
const EMPTY_ESCAPE_SITES = BitSet()
const ARGUMENT_ESCAPE_SITES = BitSet(0)

# the constructors
NotAnalyzed() = EscapeLattice(false, false, false, EMPTY_ESCAPE_SITES) # not formally part of the lattice
NoEscape() = EscapeLattice(true, false, false, EMPTY_ESCAPE_SITES)
ReturnEscape(pc::Int) = EscapeLattice(true, true, false, BitSet(pc))
ThrownEscape(pc::Int) = EscapeLattice(true, false, true, BitSet(pc))
ArgumentReturnEscape() = EscapeLattice(true, true, false, ARGUMENT_ESCAPE_SITES)
let
    all_escape_sites = BitSet(0:100_000)
    global AllEscape() = EscapeLattice(true, true, true, all_escape_sites)
    # used for `show`
    global AllReturnEscape() = EscapeLattice(true, true, false, all_escape_sites)
    global AllThrownEscape() = EscapeLattice(true, false, true, all_escape_sites)
end

# Convenience names for some ⊑ queries
export
    has_not_analyzed,
    has_no_escape,
    has_return_escape,
    has_thrown_escape,
    has_all_escape,
    can_elide_finalizer
has_not_analyzed(x::EscapeLattice) = x == NotAnalyzed()
has_no_escape(x::EscapeLattice) = x ⊑ NoEscape()
has_return_escape(x::EscapeLattice) = x.ReturnEscape
has_return_escape(x::EscapeLattice, pc::Int) = has_return_escape(x) && pc in x.EscapeSites
has_thrown_escape(x::EscapeLattice) = x.ThrownEscape
has_thrown_escape(x::EscapeLattice, pc::Int) = has_thrown_escape(x) && pc in x.EscapeSites
has_all_escape(x::EscapeLattice) = AllEscape() ⊑ x

"""
    can_elide_finalizer(x::EscapeLattice, pc::Int) -> Bool

Queries the validity of the finalizer elision optimization at the `return` site of statement `pc`,
which inserts `finalize` call when the lifetime of interested object ends.
Note that we don't need to take `x.ThrownEscape` into account because it would have never
been thrown when the program execution reaches the `return` site.
"""
can_elide_finalizer(x::EscapeLattice, pc::Int) =
    !(has_return_escape(x, 0) || has_return_escape(x, pc))

# we need to make sure this `==` operator corresponds to lattice equality rather than object equality,
# otherwise `propagate_changes` can't detect the convergence
x::EscapeLattice == y::EscapeLattice = begin
    return x.Analyzed === y.Analyzed &&
           x.ReturnEscape === y.ReturnEscape &&
           x.ThrownEscape === y.ThrownEscape &&
           x.EscapeSites == y.EscapeSites &&
           true
end

x::EscapeLattice ⊑ y::EscapeLattice = begin
    if x.Analyzed ≤ y.Analyzed &&
       x.ReturnEscape ≤ y.ReturnEscape &&
       x.ThrownEscape ≤ y.ThrownEscape &&
       x.EscapeSites ⊆ y.EscapeSites &&
       true
        return true
    end
    return false
end
x::EscapeLattice ⊏ y::EscapeLattice = x ⊑ y && !(y ⊑ x)
x::EscapeLattice ⋤ y::EscapeLattice = !(y ⊑ x)

x::EscapeLattice ⊔ y::EscapeLattice = begin
    return EscapeLattice(
        x.Analyzed | y.Analyzed,
        x.ReturnEscape | y.ReturnEscape,
        x.ThrownEscape | y.ThrownEscape,
        x.EscapeSites ∪ y.EscapeSites,
        )
end

x::EscapeLattice ⊓ y::EscapeLattice = begin
    return EscapeLattice(
        x.Analyzed & y.Analyzed,
        x.ReturnEscape & y.ReturnEscape,
        x.ThrownEscape & y.ThrownEscape,
        x.EscapeSites ∩ y.EscapeSites,
        )
end

# TODO setup a more effient struct for cache
# which can discard escape information on SSS values and arguments that don't join dispatch signature

"""
    state::EscapeState

Extended lattice that maps arguments and SSA values to escape information represented as `EscapeLattice`:
- `state.arguments::Vector{EscapeLattice}`: escape information about "arguments" – note that
  "argument" can include both call arguments and slots appearing in analysis frame
- `ssavalues::Vector{EscapeLattice}`: escape information about each SSA value
"""
struct EscapeState
    arguments::Vector{EscapeLattice}
    ssavalues::Vector{EscapeLattice}
end
function EscapeState(nslots::Int, nargs::Int, nstmts::Int)
    arguments = EscapeLattice[
        1 ≤ i ≤ nargs ? ArgumentReturnEscape() : NotAnalyzed() for i in 1:nslots]
    ssavalues = EscapeLattice[NotAnalyzed() for _ in 1:nstmts]
    return EscapeState(arguments, ssavalues)
end

# we preserve `IRCode` as well just for debugging purpose
const GLOBAL_ESCAPE_CACHE = IdDict{MethodInstance,Tuple{EscapeState,IRCode}}()
__clear_escape_cache!() = empty!(GLOBAL_ESCAPE_CACHE)

const Change  = Pair{Union{Argument,SSAValue},EscapeLattice}
const Changes = Vector{Change}

"""
    find_escapes(ir::IRCode, nargs::Int) -> EscapeState

Escape analysis implementation is based on the data-flow algorithm described in the paper [^MM02].
The analysis works on the lattice of [`EscapeLattice`](@ref) and transitions lattice elements
from the bottom to the top in a _backward_ way, i.e. data flows from usage cites to definitions,
until every lattice gets converged to a fixed point by maintaining a (conceptual) working set
that contains program counters corresponding to remaining SSA statements to be analyzed.
The analysis only manages a single global state that tracks `EscapeLattice` of each argument
and SSA statement, but also note that some flow-sensitivity is encoded as program counters
recorded in the `EscapeSites` property of each each lattice element.

[^MM02]: _A Graph-Free approach to Data-Flow Analysis_.
         Markas Mohnen, 2002, April.
         <https://api.semanticscholar.org/CorpusID:28519618>.
"""
function find_escapes(ir::IRCode, nargs::Int)
    (; stmts, sptypes, argtypes) = ir
    nstmts = length(stmts)

    # only manage a single state, some flow-sensitivity is encoded as `EscapeLattice` properties
    state = EscapeState(length(argtypes), nargs, nstmts)
    changes = Changes() # stashes changes that happen at current statement

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
                    if !is_effect_free
                        for x in stmt.args
                            add_change!(x, ir, ThrownEscape(pc), changes)
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
                        add_change!(rhs, ir, AllEscape(), changes)
                    end
                elseif head === :foreigncall
                    escape_foreigncall!(ir, pc, stmt.args, state, changes)
                elseif head === :throw_undef_if_not # XXX when is this expression inserted ?
                    add_change!(stmt.args[1], ir, ThrownEscape(pc), changes)
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
                        add_change!(x, ir, AllEscape(), changes)
                    end
                end
            elseif isa(stmt, GlobalRef) # global load
                add_change!(SSAValue(pc), ir, AllEscape(), changes)
            elseif isa(stmt, PiNode)
                if isdefined(stmt, :val)
                    info = state.ssavalues[pc]
                    add_change!(stmt.val, ir, info, changes)
                end
            elseif isa(stmt, PhiNode)
                escape_backedges!(ir, pc, stmt.values, state, changes)
            elseif isa(stmt, PhiCNode)
                escape_backedges!(ir, pc, stmt.values, state, changes)
            elseif isa(stmt, UpsilonNode)
                if isdefined(stmt, :val)
                    info = state.ssavalues[pc]
                    add_change!(stmt.val, ir, info, changes)
                end
            elseif isa(stmt, ReturnNode)
                if isdefined(stmt, :val)
                    add_change!(stmt.val, ir, ReturnEscape(pc), changes)
                end
            elseif isa(stmt, SSAValue)
                # NOTE after SROA, we may see SSA value as statement
                info = state.ssavalues[pc]
                add_change!(stmt, ir, info, changes)
            else
                @assert stmt isa GotoNode || stmt isa GotoIfNot || stmt === nothing # TODO remove me
                continue
            end

            isempty(changes) && continue

            anyupdate |= propagate_changes!(state, changes)

            empty!(changes)
        end

        anyupdate || break
    end

    return state
end

# propagate changes, and check convergence
function propagate_changes!(state::EscapeState, changes::Changes)
    local anychanged = false

    for (x, info) in changes
        if isa(x, Argument)
            old = state.arguments[x.n]
            new = old ⊔ info
            if old ≠ new
                state.arguments[x.n] = new
                anychanged |= true
            end
        else
            x = x::SSAValue
            old = state.ssavalues[x.id]
            new = old ⊔ info
            if old ≠ new
                state.ssavalues[x.id] = new
                anychanged |= true
            end
        end
    end

    return anychanged
end

function add_change!(@nospecialize(x), ir::IRCode, info::EscapeLattice, changes::Changes)
    if isa(x, Argument) || isa(x, SSAValue)
        if !isbitstype(widenconst(argextype(x, ir, ir.sptypes, ir.argtypes)))
            push!(changes, Change(x, info))
        end
    end
end

function escape_backedges!(ir::IRCode, pc::Int, backedges::Vector{Any},
                           state::EscapeState, changes::Changes)
    info = state.ssavalues[pc]
    for i in 1:length(backedges)
        if isassigned(backedges, i)
            add_change!(backedges[i], ir, info, changes)
        end
    end
end

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
            add_change!(args[i], ir, AllEscape(), changes)
        end
        return true
    else
        return true
    end
end

function escape_invoke!(ir::IRCode, pc::Int, args::Vector{Any},
                        state::EscapeState, changes::Changes)
    linfo = first(args)::MethodInstance
    cache = get(GLOBAL_ESCAPE_CACHE, linfo, nothing)
    args = args[2:end]
    if cache === nothing
        for x in args
            add_change!(x, ir, AllEscape(), changes)
        end
    else
        (linfostate, _ #=ir::IRCode=#) = cache
        retinfo = state.ssavalues[pc] # escape information imposed on the call statement
        method = linfo.def::Method
        nargs = Int(method.nargs)
        for i in 1:length(args)
            arg = args[i]
            if i ≤ nargs
                arginfo = linfostate.arguments[i]
            else # handle isva signature: COMBAK will this invalid once we encode alias information ?
                arginfo = linfostate.arguments[nargs]
            end
            if isempty(arginfo.ReturnEscape)
                @eval Main (ir = $ir; linfo = $linfo)
                error("invalid escape lattice element returned from inter-procedural context: inspect `Main.ir` and `Main.linfo`")
            end
            info = from_interprocedural(arginfo, retinfo, pc)
            add_change!(arg, ir, info, changes)
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
        EscapeSites = EMPTY_ESCAPE_SITES
    end
    newarginfo = EscapeLattice(true, false, arginfo.ThrownEscape, EscapeSites)
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

function escape_new!(ir::IRCode, pc::Int, args::Vector{Any},
                     state::EscapeState, changes::Changes)
    info = state.ssavalues[pc]
    if info == NotAnalyzed()
        info = NoEscape()
        add_change!(SSAValue(pc), ir, info, changes) # we will be interested in if this allocation escapes or not
    end

    # propagate the escape information of this object to all its fields as well
    # since they can be accessed through the object
    for i in 2:length(args)
        add_change!(args[i], ir, info, changes)
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
    add_change!(name, ir, ThrownEscape(pc), changes)
    for i in 6:5+foreigncall_nargs
        add_change!(args[i], ir, ThrownEscape(pc), changes)
    end
end

# NOTE error cases will be handled in `find_escapes` anyway, so we don't need to take care of them below
# TODO implement more builtins, make them more accurate
# TODO use `T_IFUNC`-like logic and don't not abuse dispatch ?

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
    length(args) == 4 || return
    f, cond, th, el = args
    info = state.ssavalues[pc]
    condt = argextype(cond, ir, ir.sptypes, ir.argtypes)
    if isa(condt, Const) && (cond = condt.val; isa(cond, Bool))
        if cond
            add_change!(th, ir, info, changes)
        else
            add_change!(el, ir, info, changes)
        end
    else
        add_change!(th, ir, info, changes)
        add_change!(el, ir, info, changes)
    end
end

function escape_builtin!(::typeof(typeassert), ir::IRCode, pc::Int, args::Vector{Any}, state::EscapeState, changes::Changes)
    length(args) == 3 || return
    f, obj, typ = args
    info = state.ssavalues[pc]
    add_change!(obj, ir, info, changes)
end

function escape_builtin!(::typeof(tuple), ir::IRCode, pc::Int, args::Vector{Any}, state::EscapeState, changes::Changes)
    info = state.ssavalues[pc]
    if info == NotAnalyzed()
        info = NoEscape()
    end
    for i in 2:length(args)
        add_change!(args[i], ir, info, changes)
    end
end

# TODO don't propagate escape information to the 1st argument, but propagate information to aliased field
function escape_builtin!(::typeof(getfield), ir::IRCode, pc::Int, args::Vector{Any}, state::EscapeState, changes::Changes)
    # only propagate info when the field itself is non-bitstype
    isbitstype(widenconst(ir.stmts.type[pc])) && return true
    info = state.ssavalues[pc]
    if info == NotAnalyzed()
        info = NoEscape()
    end
    for i in 2:length(args)
        add_change!(args[i], ir, info, changes)
    end
end

# NOTE define fancy package utilities when developing EA as an external package
if !(_TOP_MOD === Core.Compiler)
    include(@__MODULE__, "utils.jl")
end

end # baremodule EscapeAnalysis

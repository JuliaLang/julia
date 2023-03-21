# This file is a part of Julia. License is MIT: https://julialang.org/license

###########
# generic #
###########

if !@isdefined(var"@timeit")
    # This is designed to allow inserting timers when loading a second copy
    # of inference for performing performance experiments.
    macro timeit(args...)
        esc(args[end])
    end
end

# avoid cycle due to over-specializing `any` when used by inference
function _any(@nospecialize(f), a)
    for x in a
        f(x) && return true
    end
    return false
end
any(@nospecialize(f), itr) = _any(f, itr)
any(itr) = _any(identity, itr)

function _all(@nospecialize(f), a)
    for x in a
        f(x) || return false
    end
    return true
end
all(@nospecialize(f), itr) = _all(f, itr)
all(itr) = _all(identity, itr)

function contains_is(itr, @nospecialize(x))
    for y in itr
        if y === x
            return true
        end
    end
    return false
end

anymap(f::Function, a::Array{Any,1}) = Any[ f(a[i]) for i in 1:length(a) ]

###########
# scoping #
###########

_topmod(m::Module) = ccall(:jl_base_relative_to, Any, (Any,), m)::Module

function istopfunction(@nospecialize(f), name::Symbol)
    tn = typeof(f).name
    if tn.mt.name === name
        top = _topmod(tn.module)
        return isdefined(top, name) && isconst(top, name) && f === getglobal(top, name)
    end
    return false
end

#######
# AST #
#######

# Meta expression head, these generally can't be deleted even when they are
# in a dead branch but can be ignored when analyzing uses/liveness.
is_meta_expr_head(head::Symbol) = head === :boundscheck || head === :meta || head === :loopinfo
is_meta_expr(@nospecialize x) = isa(x, Expr) && is_meta_expr_head(x.head)

sym_isless(a::Symbol, b::Symbol) = ccall(:strcmp, Int32, (Ptr{UInt8}, Ptr{UInt8}), a, b) < 0

function is_self_quoting(@nospecialize(x))
    return isa(x,Number) || isa(x,AbstractString) || isa(x,Tuple) || isa(x,Type) ||
        isa(x,Char) || x === nothing || isa(x,Function)
end

function quoted(@nospecialize(x))
    return is_self_quoting(x) ? x : QuoteNode(x)
end

############
# inlining #
############

const MAX_INLINE_CONST_SIZE = 256

function count_const_size(@nospecialize(x), count_self::Bool = true)
    (x isa Type || x isa Symbol) && return 0
    ismutable(x) && return MAX_INLINE_CONST_SIZE + 1
    isbits(x) && return Core.sizeof(x)
    dt = typeof(x)
    sz = count_self ? sizeof(dt) : 0
    sz > MAX_INLINE_CONST_SIZE && return MAX_INLINE_CONST_SIZE + 1
    dtfd = DataTypeFieldDesc(dt)
    for i = 1:nfields(x)
        isdefined(x, i) || continue
        f = getfield(x, i)
        if !dtfd[i].isptr && datatype_pointerfree(typeof(f))
            continue
        end
        sz += count_const_size(f, dtfd[i].isptr)
        sz > MAX_INLINE_CONST_SIZE && return MAX_INLINE_CONST_SIZE + 1
    end
    return sz
end

function is_inlineable_constant(@nospecialize(x))
    return count_const_size(x) <= MAX_INLINE_CONST_SIZE
end

###########################
# MethodInstance/CodeInfo #
###########################

invoke_api(li::CodeInstance) = ccall(:jl_invoke_api, Cint, (Any,), li)
use_const_api(li::CodeInstance) = invoke_api(li) == 2

function get_staged(mi::MethodInstance)
    may_invoke_generator(mi) || return nothing
    try
        # user code might throw errors – ignore them
        ci = ccall(:jl_code_for_staged, Any, (Any,), mi)::CodeInfo
        return ci
    catch
        return nothing
    end
end

function retrieve_code_info(linfo::MethodInstance)
    m = linfo.def::Method
    c = nothing
    if isdefined(m, :generator)
        # user code might throw errors – ignore them
        c = get_staged(linfo)
    end
    if c === nothing && isdefined(m, :source)
        src = m.source
        if src === nothing
            # can happen in images built with --strip-ir
            return nothing
        elseif isa(src, Array{UInt8,1})
            c = ccall(:jl_uncompress_ir, Any, (Any, Ptr{Cvoid}, Any), m, C_NULL, src)
        else
            c = copy(src::CodeInfo)
        end
    end
    if c isa CodeInfo
        c.parent = linfo
        return c
    end
    return nothing
end

function get_compileable_sig(method::Method, @nospecialize(atype), sparams::SimpleVector)
    isa(atype, DataType) || return nothing
    mt = ccall(:jl_method_get_table, Any, (Any,), method)
    mt === nothing && return nothing
    return ccall(:jl_normalize_to_compilable_sig, Any, (Any, Any, Any, Any),
        mt, atype, sparams, method)
end

isa_compileable_sig(@nospecialize(atype), sparams::SimpleVector, method::Method) =
    !iszero(ccall(:jl_isa_compileable_sig, Int32, (Any, Any, Any), atype, sparams, method))

# eliminate UnionAll vars that might be degenerate due to having identical bounds,
# or a concrete upper bound and appearing covariantly.
function subst_trivial_bounds(@nospecialize(atype))
    if !isa(atype, UnionAll)
        return atype
    end
    v = atype.var
    if isconcretetype(v.ub) || v.lb === v.ub
        subst = try
            atype{v.ub}
        catch
            # Note in rare cases a var bound might not be valid to substitute.
            nothing
        end
        if subst !== nothing
            return subst_trivial_bounds(subst)
        end
    end
    return UnionAll(v, subst_trivial_bounds(atype.body))
end

has_typevar(@nospecialize(t), v::TypeVar) = ccall(:jl_has_typevar, Cint, (Any, Any), t, v) != 0

# If removing trivial vars from atype results in an equivalent type, use that
# instead. Otherwise we can get a case like issue #38888, where a signature like
#   f(x::S) where S<:Int
# gets cached and matches a concrete dispatch case.
function normalize_typevars(method::Method, @nospecialize(atype), sparams::SimpleVector)
    at2 = subst_trivial_bounds(atype)
    if at2 !== atype && at2 == atype
        atype = at2
        sp_ = ccall(:jl_type_intersection_with_env, Any, (Any, Any), at2, method.sig)::SimpleVector
        sparams = sp_[2]::SimpleVector
    end
    return Pair{Any,SimpleVector}(atype, sparams)
end

# get a handle to the unique specialization object representing a particular instantiation of a call
function specialize_method(method::Method, @nospecialize(atype), sparams::SimpleVector; preexisting::Bool=false, compilesig::Bool=false)
    if isa(atype, UnionAll)
        atype, sparams = normalize_typevars(method, atype, sparams)
    end
    if compilesig
        new_atype = get_compileable_sig(method, atype, sparams)
        new_atype === nothing && return nothing
        if atype !== new_atype
            sp_ = ccall(:jl_type_intersection_with_env, Any, (Any, Any), new_atype, method.sig)::SimpleVector
            if sparams === sp_[2]::SimpleVector
                atype = new_atype
            end
        end
    end
    if preexisting
        # check cached specializations
        # for an existing result stored there
        return ccall(:jl_specializations_lookup, Any, (Any, Any), method, atype)::Union{Nothing,MethodInstance}
    end
    return ccall(:jl_specializations_get_linfo, Ref{MethodInstance}, (Any, Any, Any), method, atype, sparams)
end

function specialize_method(match::MethodMatch; kwargs...)
    return specialize_method(match.method, match.spec_types, match.sparams; kwargs...)
end

"""
    is_declared_inline(method::Method) -> Bool

Check if `method` is declared as `@inline`.
"""
is_declared_inline(method::Method) = _is_declared_inline(method, true)

"""
    is_declared_noinline(method::Method) -> Bool

Check if `method` is declared as `@noinline`.
"""
is_declared_noinline(method::Method) = _is_declared_inline(method, false)

function _is_declared_inline(method::Method, inline::Bool)
    isdefined(method, :source) || return false
    src = method.source
    isa(src, MaybeCompressed) || return false
    return (inline ? is_declared_inline : is_declared_noinline)(src)
end

"""
    is_aggressive_constprop(method::Union{Method,CodeInfo}) -> Bool

Check if `method` is declared as `Base.@constprop :aggressive`.
"""
is_aggressive_constprop(method::Union{Method,CodeInfo}) = method.constprop == 0x01

"""
    is_no_constprop(method::Union{Method,CodeInfo}) -> Bool

Check if `method` is declared as `Base.@constprop :none`.
"""
is_no_constprop(method::Union{Method,CodeInfo}) = method.constprop == 0x02

#############
# backedges #
#############

"""
    BackedgeIterator(backedges::Vector{Any})

Return an iterator over a list of backedges. Iteration returns `(sig, caller)` elements,
which will be one of the following:

- `BackedgePair(nothing, caller::MethodInstance)`: a call made by ordinary inferable dispatch
- `BackedgePair(invokesig::Type, caller::MethodInstance)`: a call made by `invoke(f, invokesig, args...)`
- `BackedgePair(specsig::Type, mt::MethodTable)`: an abstract call

# Examples

```julia
julia> callme(x) = x+1
callme (generic function with 1 method)

julia> callyou(x) = callme(x)
callyou (generic function with 1 method)

julia> callyou(2.0)
3.0

julia> mi = which(callme, (Any,)).specializations
MethodInstance for callme(::Float64)

julia> @eval Core.Compiler for (; sig, caller) in BackedgeIterator(Main.mi.backedges)
           println(sig)
           println(caller)
       end
nothing
callyou(Float64) from callyou(Any)
```
"""
struct BackedgeIterator
    backedges::Vector{Any}
end

const empty_backedge_iter = BackedgeIterator(Any[])

struct BackedgePair
    sig # ::Union{Nothing,Type}
    caller::Union{MethodInstance,MethodTable}
    BackedgePair(@nospecialize(sig), caller::Union{MethodInstance,MethodTable}) = new(sig, caller)
end

function iterate(iter::BackedgeIterator, i::Int=1)
    backedges = iter.backedges
    i > length(backedges) && return nothing
    item = backedges[i]
    isa(item, MethodInstance) && return BackedgePair(nothing, item), i+1      # regular dispatch
    isa(item, MethodTable) && return BackedgePair(backedges[i+1], item), i+2  # abstract dispatch
    return BackedgePair(item, backedges[i+1]::MethodInstance), i+2            # `invoke` calls
end

#########
# types #
#########

function singleton_type(@nospecialize(ft))
    ft = widenslotwrapper(ft)
    if isa(ft, Const)
        return ft.val
    elseif isconstType(ft)
        return ft.parameters[1]
    elseif issingletontype(ft)
        return ft.instance
    end
    return nothing
end

function maybe_singleton_const(@nospecialize(t))
    if isa(t, DataType)
        if issingletontype(t)
            return Const(t.instance)
        elseif isconstType(t)
            return Const(t.parameters[1])
        end
    end
    return t
end

###################
# SSAValues/Slots #
###################

function ssamap(f, @nospecialize(stmt))
    urs = userefs(stmt)
    for op in urs
        val = op[]
        if isa(val, SSAValue)
            op[] = f(val)
        end
    end
    return urs[]
end

function foreachssa(@specialize(f), @nospecialize(stmt))
    urs = userefs(stmt)
    for op in urs
        val = op[]
        if isa(val, SSAValue)
            f(val)
        end
    end
end

function foreach_anyssa(@specialize(f), @nospecialize(stmt))
    urs = userefs(stmt)
    for op in urs
        val = op[]
        if isa(val, AnySSAValue)
            f(val)
        end
    end
end

function find_ssavalue_uses(body::Vector{Any}, nvals::Int)
    uses = BitSet[ BitSet() for i = 1:nvals ]
    for line in 1:length(body)
        e = body[line]
        if isa(e, ReturnNode)
            e = e.val
        elseif isa(e, GotoIfNot)
            e = e.cond
        end
        if isa(e, SSAValue)
            push!(uses[e.id], line)
        elseif isa(e, Expr)
            find_ssavalue_uses(e, uses, line)
        elseif isa(e, PhiNode)
            find_ssavalue_uses(e, uses, line)
        end
    end
    return uses
end

function find_ssavalue_uses(e::Expr, uses::Vector{BitSet}, line::Int)
    head = e.head
    is_meta_expr_head(head) && return
    skiparg = (head === :(=))
    for a in e.args
        if skiparg
            skiparg = false
        elseif isa(a, SSAValue)
            push!(uses[a.id], line)
        elseif isa(a, Expr)
            find_ssavalue_uses(a, uses, line)
        end
    end
end

function find_ssavalue_uses(e::PhiNode, uses::Vector{BitSet}, line::Int)
    for val in e.values
        if isa(val, SSAValue)
            push!(uses[val.id], line)
        end
    end
end

function is_throw_call(e::Expr)
    if e.head === :call
        f = e.args[1]
        if isa(f, GlobalRef)
            ff = abstract_eval_globalref(f)
            if isa(ff, Const) && ff.val === Core.throw
                return true
            end
        end
    end
    return false
end

function mark_throw_blocks!(src::CodeInfo, handler_at::Vector{Int})
    for stmt in find_throw_blocks(src.code, handler_at)
        src.ssaflags[stmt] |= IR_FLAG_THROW_BLOCK
    end
    return nothing
end

function find_throw_blocks(code::Vector{Any}, handler_at::Vector{Int})
    stmts = BitSet()
    n = length(code)
    for i in n:-1:1
        s = code[i]
        if isa(s, Expr)
            if s.head === :gotoifnot
                if i+1 in stmts && s.args[2]::Int in stmts
                    push!(stmts, i)
                end
            elseif s.head === :return
                # see `ReturnNode` handling
            elseif is_throw_call(s)
                if handler_at[i] == 0
                    push!(stmts, i)
                end
            elseif i+1 in stmts
                push!(stmts, i)
            end
        elseif isa(s, ReturnNode)
            # NOTE: it potentially makes sense to treat unreachable nodes
            # (where !isdefined(s, :val)) as `throw` points, but that can cause
            # worse codegen around the call site (issue #37558)
        elseif isa(s, GotoNode)
            if s.label in stmts
                push!(stmts, i)
            end
        elseif isa(s, GotoIfNot)
            if i+1 in stmts && s.dest in stmts
                push!(stmts, i)
            end
        elseif i+1 in stmts
            push!(stmts, i)
        end
    end
    return stmts
end

# using a function to ensure we can infer this
@inline function slot_id(s)
    isa(s, SlotNumber) && return s.id
    isa(s, Argument) && return s.n
    return (s::TypedSlot).id
end

###########
# options #
###########

is_root_module(m::Module) = false

inlining_enabled() = (JLOptions().can_inline == 1)
function coverage_enabled(m::Module)
    ccall(:jl_generating_output, Cint, ()) == 0 || return false # don't alter caches
    cov = JLOptions().code_coverage
    if cov == 1 # user
        m = moduleroot(m)
        m === Core && return false
        isdefined(Main, :Base) && m === Main.Base && return false
        return true
    elseif cov == 2 # all
        return true
    end
    return false
end
function inbounds_option()
    opt_check_bounds = JLOptions().check_bounds
    opt_check_bounds == 0 && return :default
    opt_check_bounds == 1 && return :on
    return :off
end

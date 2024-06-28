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
    (x isa Type || x isa Core.TypeName || x isa Symbol) && return 0
    if ismutable(x)
        # No definite size
        (isa(x, GenericMemory) || isa(x, String) || isa(x, SimpleVector)) &&
            return MAX_INLINE_CONST_SIZE + 1
        if isa(x, Module)
            # We allow modules, because we already assume they are externally
            # rooted, so we count their contents as 0 size.
            return sizeof(Ptr{Cvoid})
        end
        # We allow mutable types with no mutable fields (i.e. those mutable
        # types used for identity only). The intent of this function is to
        # prevent the rooting of large amounts of data that may have been
        # speculatively computed. If the struct can get mutated later, we
        # cannot assess how much data we might end up rooting. However, if
        # the struct is mutable only for identity, the query still works.
        for i = 1:nfields(x)
            if !isconst(typeof(x), i)
                return MAX_INLINE_CONST_SIZE + 1
            end
        end
    end
    isbits(x) && return Core.sizeof(x)
    dt = typeof(x)
    sz = count_self ? sizeof(dt) : 0
    sz > MAX_INLINE_CONST_SIZE && return MAX_INLINE_CONST_SIZE + 1
    dtfd = DataTypeFieldDesc(dt)
    for i = 1:Int(datatype_nfields(dt))
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

is_nospecialized(method::Method) = method.nospecialize ≠ 0

is_nospecializeinfer(method::Method) = method.nospecializeinfer && is_nospecialized(method)

###########################
# MethodInstance/CodeInfo #
###########################

invoke_api(li::CodeInstance) = ccall(:jl_invoke_api, Cint, (Any,), li)
use_const_api(li::CodeInstance) = invoke_api(li) == 2

function get_staged(mi::MethodInstance, world::UInt)
    may_invoke_generator(mi) || return nothing
    cache_ci = (mi.def::Method).generator isa Core.CachedGenerator ?
        RefValue{CodeInstance}() : nothing
    try
        return call_get_staged(mi, world, cache_ci)
    catch # user code might throw errors – ignore them
        return nothing
    end
end

# enable caching of unoptimized generated code if the generator is `CachedGenerator`
function call_get_staged(mi::MethodInstance, world::UInt, cache_ci::RefValue{CodeInstance})
    token = @_gc_preserve_begin cache_ci
    cache_ci_ptr = pointer_from_objref(cache_ci)
    src = ccall(:jl_code_for_staged, Ref{CodeInfo}, (Any, UInt, Ptr{CodeInstance}), mi, world, cache_ci_ptr)
    @_gc_preserve_end token
    return src
end
function call_get_staged(mi::MethodInstance, world::UInt, ::Nothing)
    return ccall(:jl_code_for_staged, Ref{CodeInfo}, (Any, UInt, Ptr{Cvoid}), mi, world, C_NULL)
end

function get_cached_uninferred(mi::MethodInstance, world::UInt)
    ccall(:jl_cached_uninferred, Any, (Any, UInt), mi.cache, world)::CodeInstance
end

function retrieve_code_info(mi::MethodInstance, world::UInt)
    def = mi.def
    if !isa(def, Method)
        ci = get_cached_uninferred(mi, world)
        src = ci.inferred
        # Inference may corrupt the src, which is fine, because this is a
        # (short-lived) top-level thunk, but set it to NULL anyway, so we
        # can catch it if somebody tries to read it again by accident.
        # @atomic ci.inferred = C_NULL
        return src
    end
    c = hasgenerator(def) ? get_staged(mi, world) : nothing
    if c === nothing && isdefined(def, :source)
        src = def.source
        if src === nothing
            # can happen in images built with --strip-ir
            return nothing
        elseif isa(src, String)
            c = ccall(:jl_uncompress_ir, Ref{CodeInfo}, (Any, Ptr{Cvoid}, Any), def, C_NULL, src)
        else
            c = copy(src::CodeInfo)
        end
    end
    if c isa CodeInfo
        c.parent = mi
        return c
    end
    return nothing
end

function get_compileable_sig(method::Method, @nospecialize(atype), sparams::SimpleVector)
    isa(atype, DataType) || return nothing
    mt = ccall(:jl_method_get_table, Any, (Any,), method)
    mt === nothing && return nothing
    return ccall(:jl_normalize_to_compilable_sig, Any, (Any, Any, Any, Any, Cint),
        mt, atype, sparams, method, #=int return_if_compileable=#1)
end

function get_nospecializeinfer_sig(method::Method, @nospecialize(atype), sparams::SimpleVector)
    isa(atype, DataType) || return method.sig
    mt = ccall(:jl_method_get_table, Any, (Any,), method)
    mt === nothing && return method.sig
    return ccall(:jl_normalize_to_compilable_sig, Any, (Any, Any, Any, Any, Cint),
        mt, atype, sparams, method, #=int return_if_compileable=#0)
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
@inline function specialize_method(method::Method, @nospecialize(atype), sparams::SimpleVector; preexisting::Bool=false)
    if isa(atype, UnionAll)
        atype, sparams = normalize_typevars(method, atype, sparams)
    end
    if is_nospecializeinfer(method)
        atype = get_nospecializeinfer_sig(method, atype, sparams)
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

@nospecializeinfer function singleton_type(@nospecialize(ft))
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

@nospecializeinfer function maybe_singleton_const(@nospecialize(t))
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
            isdefined(e, :val) || continue
            e = e.val
        elseif isa(e, GotoIfNot)
            e = e.cond
        end
        if isa(e, SSAValue)
            push!(uses[e.id], line)
        elseif isa(e, Expr)
            find_ssavalue_uses!(uses, e, line)
        elseif isa(e, PhiNode)
            find_ssavalue_uses!(uses, e, line)
        end
    end
    return uses
end

function find_ssavalue_uses!(uses::Vector{BitSet}, e::Expr, line::Int)
    head = e.head
    is_meta_expr_head(head) && return
    skiparg = (head === :(=))
    for a in e.args
        if skiparg
            skiparg = false
        elseif isa(a, SSAValue)
            push!(uses[a.id], line)
        elseif isa(a, Expr)
            find_ssavalue_uses!(uses, a, line)
        end
    end
end

function find_ssavalue_uses!(uses::Vector{BitSet}, e::PhiNode, line::Int)
    values = e.values
    for i = 1:length(values)
        isassigned(values, i) || continue
        val = values[i]
        if isa(val, SSAValue)
            push!(uses[val.id], line)
        end
    end
end

# using a function to ensure we can infer this
@inline function slot_id(s)
    isa(s, SlotNumber) && return s.id
    return (s::Argument).n
end

###########
# options #
###########

inlining_enabled() = (JLOptions().can_inline == 1)

function coverage_enabled(m::Module)
    generating_output() && return false # don't alter caches
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

is_asserts() = ccall(:jl_is_assertsbuild, Cint, ()) == 1

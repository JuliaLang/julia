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

function contains_is(itr, @nospecialize(x))
    for y in itr
        if y === x
            return true
        end
    end
    return false
end

anymap(f::Function, a::Array{Any,1}) = Any[ f(a[i]) for i in 1:length(a) ]

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
        if isa(x, Module) || isa(x, Method) || isa(x, CodeInstance)
            # We allow modules, methods and CodeInstance, because we already assume they are externally
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
        if (def.did_scan_source & 0x1) == 0x0
            # This scan must happen:
            #   1. After method definition
            #   2. Before any code instances that may have relied on information
            #      from implicit GlobalRefs for this method are added to the cache
            #   3. Preferably while the IR is already uncompressed
            #   4. As late as possible, as early adding of the backedges may cause
            #      spurious invalidations.
            #
            # At the moment we do so here, because
            #  1. It's reasonably late
            #  2. It has easy access to the uncompressed IR
            #  3. We necessarily pass through here before relying on any
            #     information obtained from implicit GlobalRefs.
            #
            # However, the exact placement of this scan is not as important as
            # long as the above conditions are met.
            ccall(:jl_scan_method_source_now, Cvoid, (Any, Any), def, c)
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


isa_compileable_sig(@nospecialize(atype), sparams::SimpleVector, method::Method) =
    !iszero(ccall(:jl_isa_compileable_sig, Int32, (Any, Any, Any), atype, sparams, method))

isa_compileable_sig(m::MethodInstance) = (def = m.def; !isa(def, Method) || isa_compileable_sig(m.specTypes, m.sparam_vals, def))
isa_compileable_sig(m::ABIOverride) = false

has_typevar(@nospecialize(t), v::TypeVar) = ccall(:jl_has_typevar, Cint, (Any, Any), t, v) != 0

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

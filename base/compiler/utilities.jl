# This file is a part of Julia. License is MIT: https://julialang.org/license

###########
# generic #
###########

if !isdefined(@__MODULE__, Symbol("@timeit"))
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

function _all(@nospecialize(f), a)
    for x in a
        f(x) || return false
    end
    return true
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

###########
# scoping #
###########

_topmod(m::Module) = ccall(:jl_base_relative_to, Any, (Any,), m)::Module

function istopfunction(@nospecialize(f), name::Symbol)
    tn = typeof(f).name
    if tn.mt.name === name
        top = _topmod(tn.module)
        return isdefined(top, name) && isconst(top, name) && f === getfield(top, name)
    end
    return false
end

#######
# AST #
#######

# Meta expression head, these generally can't be deleted even when they are
# in a dead branch but can be ignored when analyzing uses/liveness.
is_meta_expr_head(head::Symbol) = (head === :inbounds || head === :boundscheck || head === :meta || head === :loopinfo)

sym_isless(a::Symbol, b::Symbol) = ccall(:strcmp, Int32, (Ptr{UInt8}, Ptr{UInt8}), a, b) < 0

function is_self_quoting(@nospecialize(x))
    return isa(x,Number) || isa(x,AbstractString) || isa(x,Tuple) || isa(x,Type) ||
        isa(x,Char) || x === nothing || isa(x,Function)
end

function quoted(@nospecialize(x))
    return is_self_quoting(x) ? x : QuoteNode(x)
end

function is_inlineable_constant(@nospecialize(x))
    if x isa Type || x isa Symbol
        return true
    end
    return isbits(x) && Core.sizeof(x) <= MAX_INLINE_CONST_SIZE
end

###########################
# MethodInstance/CodeInfo #
###########################

function invoke_api(li::CodeInstance)
    return ccall(:jl_invoke_api, Cint, (Any,), li)
end

function get_staged(li::MethodInstance)
    may_invoke_generator(li) || return nothing
    try
        # user code might throw errors – ignore them
        return ccall(:jl_code_for_staged, Any, (Any,), li)::CodeInfo
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
        if isa(src, Array{UInt8,1})
            c = ccall(:jl_uncompress_ir, Any, (Any, Ptr{Cvoid}, Any), m, C_NULL, src)
        else
            c = copy(src::CodeInfo)
        end
    end
    if c isa CodeInfo
        c.parent = linfo
        return c
    end
end

function inf_for_methodinstance(mi::MethodInstance, min_world::UInt, max_world::UInt=min_world)
    return ccall(:jl_rettype_inferred, Any, (Any, UInt, UInt), mi, min_world, max_world)::Union{Nothing, CodeInstance}
end


# get a handle to the unique specialization object representing a particular instantiation of a call
function specialize_method(method::Method, @nospecialize(atypes), sparams::SimpleVector, preexisting::Bool=false)
    if preexisting
        if method.specializations !== nothing
            # check cached specializations
            # for an existing result stored there
            return ccall(:jl_specializations_lookup, Any, (Any, Any), method, atypes)
        end
        return nothing
    end
    return ccall(:jl_specializations_get_linfo, Ref{MethodInstance}, (Any, Any, Any), method, atypes, sparams)
end

# This function is used for computing alternate limit heuristics
function method_for_inference_heuristics(method::Method, @nospecialize(sig), sparams::SimpleVector)
    if isdefined(method, :generator) && method.generator.expand_early && may_invoke_generator(method, sig, sparams)
        method_instance = specialize_method(method, sig, sparams, false)
        if isa(method_instance, MethodInstance)
            cinfo = get_staged(method_instance)
            if isa(cinfo, CodeInfo)
                method2 = cinfo.method_for_inference_limit_heuristics
                if method2 isa Method
                    return method2
                end
            end
        end
    end
    return nothing
end

argextype(@nospecialize(x), state) = argextype(x, state.src, state.sptypes, state.slottypes)

const empty_slottypes = Any[]

function argextype(@nospecialize(x), src, sptypes::Vector{Any}, slottypes::Vector{Any} = empty_slottypes)
    if isa(x, Expr)
        if x.head === :static_parameter
            return sptypes[x.args[1]]
        elseif x.head === :boundscheck
            return Bool
        elseif x.head === :copyast
            return argextype(x.args[1], src, sptypes, slottypes)
        end
        @assert false "argextype only works on argument-position values"
    elseif isa(x, SlotNumber)
        return slottypes[(x::SlotNumber).id]
    elseif isa(x, TypedSlot)
        return (x::TypedSlot).typ
    elseif isa(x, SSAValue)
        return abstract_eval_ssavalue(x::SSAValue, src)
    elseif isa(x, Argument)
        return isa(src, IncrementalCompact) ? src.ir.argtypes[x.n] : src.argtypes[x.n]
    elseif isa(x, QuoteNode)
        return AbstractEvalConstant((x::QuoteNode).value)
    elseif isa(x, GlobalRef)
        return abstract_eval_global(x.mod, (x::GlobalRef).name)
    elseif isa(x, PhiNode)
        return Any
    elseif isa(x, PiNode)
        return x.typ
    else
        return AbstractEvalConstant(x)
    end
end

###################
# SSAValues/Slots #
###################

function find_ssavalue_uses(body::Vector{Any}, nvals::Int)
    uses = BitSet[ BitSet() for i = 1:nvals ]
    for line in 1:length(body)
        e = body[line]
        if isa(e, SSAValue)
            push!(uses[e.id], line)
        elseif isa(e, Expr)
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

# using a function to ensure we can infer this
@inline slot_id(s) = isa(s, SlotNumber) ? (s::SlotNumber).id : (s::TypedSlot).id

###########
# options #
###########

is_root_module(m::Module) = false

inlining_enabled() = (JLOptions().can_inline == 1)
function coverage_enabled(m::Module)
    ccall(:jl_generating_output, Cint, ()) == 0 || return false # don't alter caches
    cov = JLOptions().code_coverage
    if cov == 1
        m = moduleroot(m)
        m === Core && return false
        isdefined(Main, :Base) && m === Main.Base && return false
        return true
    elseif cov == 2
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

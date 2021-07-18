# This file is a part of Julia. License is MIT: https://julialang.org/license

# # those primitive defined in types.jl
#
# struct _PluginContext{plugins} end
#
# const PluginContext = Type{<:_PluginContext}
#
# abstract type AbstractCompilerPlugin end

# plugin management
# =================

PluginContext(plugins::Vector{Any}) = _PluginContext{to_tuple_type(plugins)}

get_plugins(@nospecialize(ctx::PluginContext)) = first(ctx.parameters).parameters::SimpleVector

"""
    (::Type{<:AbstractCompilerPlugin})(f) = f()

The entry into a plugin context
Enter a plugin context from the entry call `f()`.

!!! warning
    This functor shouldn't be overloaded, the whole system will be broken otherwise.
"""
(::Type{<:AbstractCompilerPlugin})(f) = f()
# (::Type{<:AbstractCompilerPlugin})(f, args...; kwargs...) = f(args...; kwargs...)

const PLUGIN_ENTRY_METHOD = first(methods(AbstractCompilerPlugin))

# overdubbing
# ===========

(::PluginContext)(args...) = return # a dummy method body

const NULL_CONTEXT  = PluginContext(Any[])
const SHADOW_METHOD = let
    ms = methods(NULL_CONTEXT, (Tuple{Any,Vararg{Any}}))
    @assert length(ms) == 1
    first(ms)
end

is_shadow((; linfo)::Union{InferenceState,OptimizationState}) = is_shadow(linfo)
is_shadow((; def, specTypes)::MethodInstance) = isa(def, Method) && _is_shadow(def, specTypes)
is_shadow((; method, spec_types)::MethodMatch) = _is_shadow(method, spec_types)
function _is_shadow(def::Method, @nospecialize(spec_types))
    def === SHADOW_METHOD || return false
    ft = unwrap_unionall(spec_types).parameters[1]
    return is_plugin_ft(ft)
end

function is_overdubbing_call(argtypes::Vector{Any})
    length(argtypes) < 1 && return false
    ft = widenconst(argtypes[1])
    return is_plugin_ft(ft)
end

function is_plugin_ft(@nospecialize(ft))
    isType(ft) || return false
    t = ft.parameters[1]
    (t !== Bottom && t <: _PluginContext) || return false
    @assert t !== NULL_CONTEXT "invalid plugin context introduced"
    return true
end

# a runtime mapping of shadow method instance to overdubbed method instance
const OVERDUB_CACHE_TABLE = IdDict{MethodInstance,MethodInstance}()

function maybe_get_overdubbed(sv::Union{InferenceState,OptimizationState})
    is_shadow(sv) || return sv.linfo # fast pass
    return maybe_get_overdubbed(sv.linfo)
end
maybe_get_overdubbed(mi::MethodInstance) = get(OVERDUB_CACHE_TABLE, mi, mi)

struct CallSig
    argtypes::Vector{Any}
    atype
    CallSig(argtypes::Vector{Any}, @nospecialize(atype)) = new(argtypes, atype)
end

struct OverdubCallSig
    shadow::CallSig
    overdub::CallSig
end

const AnyCallSig = Union{CallSig,OverdubCallSig}

struct OverdubMethodMatch
    shadow::MethodMatch
    overdub::MethodMatch
    function OverdubMethodMatch(shadow::MethodMatch, overdub::MethodMatch)
        @assert shadow.sparams === svec()
        @assert is_shadow(shadow)
        return new(shadow, overdub)
    end
end

const AnyMethodMatch = Union{MethodMatch,OverdubMethodMatch}

unwrap_shadow(callsig::AnyCallSig) = isa(callsig, CallSig) ? callsig : callsig.shadow
unwrap_overdub(callsig::AnyCallSig) = isa(callsig, CallSig) ? callsig : callsig.overdub
unwrap_shadow(match::AnyMethodMatch) = isa(match, MethodMatch) ? match : match.shadow
unwrap_overdub(match::AnyMethodMatch) = isa(match, MethodMatch) ? match : match.overdub

function specialize_method(match::OverdubMethodMatch; kwargs...)
    shadow = specialize_method(unwrap_shadow(match); kwargs...)::MethodInstance
    if !haskey(OVERDUB_CACHE_TABLE, shadow)
        overdub = specialize_method(unwrap_overdub(match); kwargs...)::MethodInstance
        OVERDUB_CACHE_TABLE[shadow] = overdub
    end
    return shadow
end

function retrieve_code_info_overdubbed(shadow::MethodInstance)
    overdub = OVERDUB_CACHE_TABLE[shadow]
    overdub.def.isva && throw("TODO: support vararg splatting")
    return _retrieve_code_info_overdubbed(overdub)
end

"""
    _retrieve_code_info_overdubbed(overdub::MethodInstance)

Retrieves the source of `overdub`bed method, and transforms it so that it can fit into the
execution with `SHADOW_METHOD`'s call signature. The transformation take the following procedures:
1. prepend SSA statements so that they can be referred in place of the overdubbed call arguments
2. fix up all the references to call argument or SSA values according to the step 1.
3. replace the references to static parameters with quoted type
4. fix up some other meta information accordingly
5. modify all the `:call` expressions so that it will be overdubbed again by context,
   which is propagated as the first argument of `SHADOW_METHOD`

Note that the step 4. modifies all the `:call`s whatever it can be overdubbed or not.
`fixup_no_overdubs!` will re-transform those calls if inference reveals out they can't be overdubbed.
"""
function _retrieve_code_info_overdubbed(overdub::MethodInstance)
    # retrieve the original source
    src = retrieve_code_info(overdub)
    src === nothing && return nothing
    src = copy(src) # make sure not to modify the original

    # fixup slot information
    src.slotnames = Symbol[Symbol("#self#"), :args, Symbol("#overdub#"), src.slotnames[2:end]...]
    src.slotflags = UInt8[0x00, 0x00, src.slotflags...]

    # transform statements
    code = src.code
    ssavaluetypes0 = length(code)
    ssaoffset = nargs = overdub.def.nargs
    function transform_overdub_value(@nospecialize(x))
        if isa(x, SlotNumber)
            slot = slot_id(x)
            if 1 ≤ slot ≤ nargs
                return SSAValue(slot) # refer to the overdubbed call arguments as a SSA value
            else
                return SlotNumber(slot+2) # non-call arguments are now offset by 2
            end
        elseif isa(x, SSAValue)
            return SSAValue(x.id + ssaoffset)
        end
        return x
    end
    function transform_overdub_code(@nospecialize(stmt))
        if isa(stmt, NewvarNode)
            return NewvarNode(transform_overdub_value(stmt.slot))
        elseif isa(stmt, Expr)
            head = stmt.head
            if head === :call
                return Expr(:call, SlotNumber(1), anymap(transform_overdub_code, stmt.args)...)
            elseif head === :static_parameter
                return quoted(overdub.sparam_vals[stmt.args[1]::Int])
            end
            return Expr(head, anymap(transform_overdub_code, stmt.args)...)
        elseif isa(stmt, GotoNode)
            return GotoNode(stmt.label+ssaoffset)
        elseif isa(stmt, GotoIfNot)
            return GotoIfNot(transform_overdub_value(stmt.cond), stmt.dest+ssaoffset)
        elseif isa(stmt, ReturnNode)
            return ReturnNode(transform_overdub_code(stmt.val))
        end
        return transform_overdub_value(stmt)
    end
    code = anymap(transform_overdub_code, code)
    # splat the overdubbed arguments so that they can be referred as SSA values
    prepend!(code, Any[Expr(:call, GlobalRef(Core, :getfield), SlotNumber(2), i) for i in 1:nargs])
    src.code = code

    # fix up SSA information
    ssavaluetypes = src.ssavaluetypes = length(code)
    push!(src.linetable, LineInfoNode(@__MODULE__, :transform_overdub_src!, Symbol("compiler/plugin.jl"), @__LINE__, 0))
    codeloc = length(src.linetable)
    prepend!(src.codelocs, [codeloc for _ in 1:(ssavaluetypes-ssavaluetypes0)])

    return src
end

# TODO allow general function call, not limited to a call to nullary lambda
# for i in 1:length(ir.stmts)
#     stmt = ir.stmts.inst[i]
#     if isexpr(stmt, :call) && length(stmt.args) > 2
#         a1, a2 = stmt.args[1], stmt.args[2]
#         if widenconst(argextype(a1, ir, ir.sptypes, ir.argtypes)) === typeof(Core._apply_iterate) &&
#            widenconst(argextype(a2, ir, ir.sptypes, ir.argtypes)) === typeof(Core.Compiler.iterate)
#             args = Expr(:call, GlobalRef(Core, :tuple))
#             argtypes = Any[]
#             for arg in stmt.args[3:end]
#                 push!(args.args, arg)
#                 push!(argtypes, widenconst(argextype(arg, ir, ir.sptypes, ir.argtypes)))
#             end
#             argtypes = Tuple{argtypes...}
#             argsssa = insert_node!(ir, i, NewInstruction(args, argtypes))
#             ir[SSAValue(i)] = Expr(:call, a1, a2, opt.context, argsssa)
#        end
#     end
# end
# TODO allow non sigleton plugin context
function transform_plugin_entry!(src::CodeInfo, @nospecialize(ctx))
    @assert length(src.code) == 2
    entry, ret = src.code
    @assert isexpr(entry, :call, 1) && isa(ret, ReturnNode)
    src.code[1] = Expr(:call, QuoteNode(ctx), entry.args[1])
end

# record the call at the current program counter isn't eligible to be overloaded
function no_overdub!((; no_overdubbing_calls, currpc)::InferenceState)
    no_overdubbing_calls === nothing && return
    push!(no_overdubbing_calls, currpc)
end

# we overdub whatever call within `transform_overdub_src!`, but those handled within
# `abstract_call_known` aren't eligible to be overdubbed so fix those calls here
fixup_no_overdubs!(::Nothing, ::CodeInfo) = return
function fixup_no_overdubs!(no_overdub_calls::BitSet, src::CodeInfo)
    code = src.code
    for i in 1:length(code)
        if i in no_overdub_calls
            stmt = code[i]
            newstmt = nothing
            if isexpr(stmt, :call)
                f = first(stmt.args)
                if f === SlotNumber(1)
                    newstmt = Expr(:call, stmt.args[2:end]...)
                else
                    @assert f === GlobalRef(Core, :getfield)
                end
            elseif isexpr(stmt, :(=))
                lhs, rhs = stmt.args
                @assert isexpr(rhs, :call)
                f = first(rhs.args)
                if f === SlotNumber(1)
                    newstmt = Expr(:(=), lhs, Expr(:call, rhs.args[2:end]...))
                else
                    @assert f === GlobalRef(Core, :getfield)
                end
            else
                Core.eval(Main, :(no_overdub_calls = $no_overdub_calls; src = $src))
                @assert false "invalid source"
            end
            if newstmt !== nothing
                code[i] = newstmt
            end
        end
    end
    return src.code = code
end

# hooks
# =====

function preinf_hook!(frame::InferenceState)
    ctx = frame.context
    ctx === NULL_CONTEXT && return # fast pass
    for plugin in get_plugins(ctx)
        @invokelatest preinf_hook!(plugin, frame)
    end
end
preinf_hook!(::Type{<:AbstractCompilerPlugin}, frame::InferenceState) = return

function postinf_hook!(frame::InferenceState)
    ctx = frame.context
    ctx === NULL_CONTEXT && return # fast pass
    for plugin in get_plugins(ctx)
        @invokelatest postinf_hook!(plugin, frame)
    end
end
postinf_hook!(::Type{<:AbstractCompilerPlugin}, frame::InferenceState) = return

function preopt_hook!(opt::OptimizationState)
    ctx = opt.context
    ctx === NULL_CONTEXT && return # fast pass
    for plugin in get_plugins(ctx)
        @invokelatest preopt_hook!(plugin, opt)
    end
end
preopt_hook!(::Type{<:AbstractCompilerPlugin}, opt::OptimizationState) = return

function postopt_hook!(opt::OptimizationState)
    ctx = opt.context
    ctx === NULL_CONTEXT && return # fast pass
    for plugin in get_plugins(ctx)
        @invokelatest postopt_hook!(plugin, opt)
    end
end
postopt_hook!(::Type{<:AbstractCompilerPlugin}, opt::OptimizationState) = return

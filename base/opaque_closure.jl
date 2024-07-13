# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    @opaque ([type, ]args...) -> body

Marks a given closure as "opaque". Opaque closures capture the
world age of their creation (as opposed to their invocation).
This allows for more aggressive optimization of the capture
list, but trades off against the ability to inline opaque
closures at the call site, if their creation is not statically
visible.

An argument tuple type (`type`) may optionally be specified, to
specify allowed argument types in a more flexible way. In particular,
the argument type may be fixed length even if the function is variadic.

!!! warning
    This interface is experimental and subject to change or removal without notice.
"""
macro opaque(ex)
    esc(Expr(:opaque_closure, nothing, nothing, nothing, #= allow_partial =# true, ex))
end

macro opaque(ty, ex)
    if Base.isexpr(ty, :->)
        (AT, body) = ty.args
        filter!((n)->!isa(n, Core.LineNumberNode), body.args)
        if !Base.isexpr(body, :block) || length(body.args) != 1
            error("Opaque closure type must be specified in the form Tuple{T,U...}->RT")
        end
        RT = only(body.args)
    else
        error("Opaque closure type must be specified in the form Tuple{T,U...}->RT")
    end
    AT = (AT !== :_) ? AT : nothing
    RT = (RT !== :_) ? RT : nothing
    return esc(Expr(:opaque_closure, AT, RT, RT, #= allow_partial =# true, ex))
end

# OpaqueClosure construction from pre-inferred CodeInfo/IRCode
using Core.Compiler: IRCode, SSAValue
using Core: CodeInfo

function compute_ir_rettype(ir::IRCode)
    rt = Union{}
    for i = 1:length(ir.stmts)
        stmt = ir[SSAValue(i)][:stmt]
        if isa(stmt, Core.Compiler.ReturnNode) && isdefined(stmt, :val)
            rt = Core.Compiler.tmerge(Core.Compiler.argextype(stmt.val, ir), rt)
        end
    end
    return Core.Compiler.widenconst(rt)
end

function compute_oc_signature(ir::IRCode, nargs::Int, isva::Bool)
    argtypes = Vector{Any}(undef, nargs)
    for i = 1:nargs
        argtypes[i] = Core.Compiler.widenconst(ir.argtypes[i+1])
    end
    if isva
        lastarg = pop!(argtypes)
        if lastarg <: Tuple
            append!(argtypes, lastarg.parameters)
        else
            push!(argtypes, Vararg{Any})
        end
    end
    return Tuple{argtypes...}
end

function Core.OpaqueClosure(ir::IRCode, @nospecialize env...;
                            isva::Bool = false,
                            slotnames::Union{Nothing,Vector{Symbol}}=nothing,
                            kwargs...)
    # NOTE: we need ir.argtypes[1] == typeof(env)
    ir = Core.Compiler.copy(ir)
    # if the user didn't specify a definition MethodInstance or filename Symbol to use for the debuginfo, set a filename now
    ir.debuginfo.def === nothing && (ir.debuginfo.def = :var"generated IR for OpaqueClosure")
    nargtypes = length(ir.argtypes)
    nargs = nargtypes-1
    sig = compute_oc_signature(ir, nargs, isva)
    rt = compute_ir_rettype(ir)
    src = ccall(:jl_new_code_info_uninit, Ref{CodeInfo}, ())
    if slotnames === nothing
        src.slotnames = fill(:none, nargtypes)
    else
        length(slotnames) == nargtypes || error("mismatched `argtypes` and `slotnames`")
        src.slotnames = slotnames
    end
    src.slotflags = fill(zero(UInt8), nargtypes)
    src.slottypes = copy(ir.argtypes)
    src.isva = isva
    src.nargs = nargtypes
    src = Core.Compiler.ir_to_codeinf!(src, ir)
    src.rettype = rt
    return generate_opaque_closure(sig, Union{}, rt, src, nargs, isva, env...; kwargs...)
end

function Core.OpaqueClosure(src::CodeInfo, @nospecialize env...; rettype, sig, nargs, isva=false, kwargs...)
    return generate_opaque_closure(sig, Union{}, rettype, src, nargs, isva, env...; kwargs...)
end

function generate_opaque_closure(@nospecialize(sig), @nospecialize(rt_lb), @nospecialize(rt_ub),
                                 src::CodeInfo, nargs::Int, isva::Bool, @nospecialize env...;
                                 mod::Module=@__MODULE__,
                                 lineno::Int=0,
                                 file::Union{Nothing,Symbol}=nothing,
                                 do_compile::Bool=true,
                                 isinferred::Bool=true)
    return ccall(:jl_new_opaque_closure_from_code_info, Any, (Any, Any, Any, Any, Any, Cint, Any, Cint, Cint, Any, Cint, Cint),
        sig, rt_lb, rt_ub, mod, src, lineno, file, nargs, isva, env, do_compile, isinferred)
end

struct Slot{T} end
struct Splat{T}
    value::T
end

# Args is a Tuple{Vararg{Union{Slot{T},Some{T}}}} where Slot{T} represents
# an uncurried argument slot, and Some{T} represents an argument to curry.
@noinline @generated function Core.OpaqueClosure(Args::Tuple, ::Slot{RT}) where RT
    AT = Any[]
    call = Expr(:call)
    extracted = Expr[]
    closure_args = Expr(:tuple)
    for (i, T) in enumerate(Args.parameters)
        v = Symbol("arg", i)
        is_splat = T <: Splat
        if is_splat # TODO: check position
            push!(call.args, :($v...))
            T = T.parameters[1]
        else
            push!(call.args, v)
        end
        if T <: Some
            push!(extracted, :($v = something(Args[$i])))
        elseif T <: Slot
            SlotT = T.parameters[1]
            push!(AT, is_splat ? Vararg{SlotT} : SlotT)
            push!(closure_args.args, call.args[end])
        else @assert false end
    end
    AT = Tuple{AT...}
    return Base.remove_linenums!(quote
        $(extracted...)
        $(Expr(:opaque_closure, AT, RT, RT, #= allow_partial =# false, :(($(closure_args))->@inline $(call))))
    end)
end

"""
    TypedCallable{AT,RT}

TypedCallable provides a wrapper for callable objects, with the following benefits:
    1. Enforced type-stability (for concrete AT/RT types)
    2. Fast calling convention (frequently < 10 ns / call)
    3. Normal Julia dispatch semantics (sees new Methods, etc.) + invoke_latest
    4. Full pre-compilation support (including `--trim` compatibility)

## Examples

```julia
const callbacks = @TypedCallable{(::Int,::Int)->Bool}[]

register_callback!(callbacks, f::F) where {F<:Function} =
    push!(callbacks, @TypedCallable f(::Int,::Int)::Bool)

register_callback!(callbacks, (x,y)->(x == y))
register_callback!(callbacks, (x,y)->(x != y))

# Calling a random (or runtime-known) callback is fast!
@btime callbacks[rand(1:2)](1,1)
```

# Extended help

### As an invalidation barrier

TypedCallable can also be used as an "invalidation barrier", since the caller of a
TypedCallable is not affected by any invalidations of its callee(s). This doesn't
completely cure the original invalidation, but it stops it from propagating all the
way through your code.

This can be especially helpful, e.g., when calling back to user-provided functions
whose invalidations you may have no control over.
"""
mutable struct TypedCallable{AT,RT}
    @atomic oc::Base.RefValue{Core.OpaqueClosure{AT,RT}}
    const task::Union{Task,Nothing}
    const build_oc::Function
end

function Base.show(io::IO, tc::Base.Experimental.TypedCallable)
    A, R = typeof(tc).parameters
    Base.print(io, "@TypedCallable{")
    Base.show_tuple_as_call(io, Symbol(""), A; hasfirst=false)
    Base.print(io, "->◌::", R, "}()")
end

function rebuild_in_world!(@nospecialize(self::TypedCallable), world::UInt)
    oc = Base.invoke_in_world(world, self.build_oc)
    @atomic :release self.oc = Base.Ref(oc)
    return oc
end

@inline function (self::TypedCallable{AT,RT})(args...) where {AT,RT}
    invoke_world = if self.task === nothing
        Base.get_world_counter() # Base.unsafe_load(cglobal(:jl_world_counter, UInt), :acquire) ?
    elseif self.task === Base.current_task()
        Base.tls_world_age()
    else
        error("TypedCallable{...} was called from a different task than it was created in.")
    end
    oc = (@atomic :acquire self.oc)[]
    if oc.world != invoke_world
        oc = @noinline rebuild_in_world!(self, invoke_world)::Core.OpaqueClosure{AT,RT}
    end
    return oc(args...)
end

function _TypedCallable_type(ex)
    type_err = "Invalid @TypedCallable expression: $(ex)\nExpected \"@TypedCallable{(::T,::U,...)->RT}\""

    # Unwrap {...}
    (length(ex.args) != 1) && error(type_err)
    ex = ex.args[1]

    # Unwrap (...)->RT
    !(Base.isexpr(ex, :->) && length(ex.args) == 2) && error(type_err)
    tuple_, rt = ex.args
    if !(Base.isexpr(tuple_, :tuple) && all((x)->Base.isexpr(x, :(::)), tuple_.args))
        # note: (arg::T, ...) is specifically allowed (the "arg" part is unused)
        error(type_err)
    end
    !Base.isexpr(rt, :block) && error(type_err)

    # Remove any LineNumberNodes inserted by lowering
    filter!((x)->!isa(x,Core.LineNumberNode), rt.args)
    (length(rt.args) != 1) && error(type_err)

    # Build args
    AT = Expr[esc(last(x.args)) for x in tuple_.args]
    RT = rt.args[1]

    # Unwrap ◌::T to T
    if Base.isexpr(RT, :(::)) && length(RT.args) == 2 && RT.args[1] == :◌
        RT = RT.args[2]
    end

    return :($TypedCallable{Tuple{$(AT...)}, $(esc(RT))})
end

function _TypedCallable_closure(ex)
    if Base.isexpr(ex, :call)
        error("""
              Invalid @TypedCallable expression: $(ex)
              An explicit return type assert is required (e.g. "@TypedCallable f(...)::RT")
              """)
    end

    call_, RT = ex.args
    if !Base.isexpr(call_, :call)
        error("""Invalid @TypedCallable expression: $(ex)
                 The supported syntax is:
                     @TypedCallable{(::T,::U,...)->RT} (to construct the type)
                     @TypedCallable f(x,::T,...)::RT (to construct the TypedCallable)
                 """)
    end
    oc_args = map(call_.args) do arg
        is_splat = Base.isexpr(arg, :(...))
        arg = is_splat ? arg.args[1] : arg
        transformed = if Base.isexpr(arg, :(::))
            if length(arg.args) == 1 # it's a "slot"
                slot_ty = esc(only(arg.args))
                :(Slot{$slot_ty}())
            elseif length(arg.args) == 2
                (arg, ty) = arg.args
                :(Some{$(esc(ty))}($(esc(arg))))
            else @assert false end
        else
            :(Some($(esc(arg))))
        end
        return is_splat ? Expr(:call, Splat, transformed) : transformed
    end
    # TODO: kwargs support
    RT = :(Slot{$(esc(RT))}())
    invoke_latest = true # expose as flag?
    task = invoke_latest ? nothing : :(Base.current_task())
    return quote
        build_oc = ()->Core.OpaqueClosure(($(oc_args...),), $(RT))
        $(TypedCallable)(Ref(build_oc()), $task, build_oc)
    end
end

macro TypedCallable(ex)
    if Base.isexpr(ex, :braces)
        return _TypedCallable_type(ex)
    elseif Base.isexpr(ex, :call) || (Base.isexpr(ex, :(::)) && length(ex.args) == 2)
        return _TypedCallable_closure(ex)
    else
        error("""Invalid @TypedCallable expression: $(ex)
                 The supported syntax is:
                     @TypedCallable{(::T,::U,...)->RT} (to construct the type)
                     @TypedCallable f(x,::T,...)::RT (to construct the TypedCallable)
                 """)
    end
end

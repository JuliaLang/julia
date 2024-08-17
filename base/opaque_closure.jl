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
    esc(Expr(:opaque_closure, nothing, nothing, nothing, ex))
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
    return esc(Expr(:opaque_closure, AT, RT, RT, ex))
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

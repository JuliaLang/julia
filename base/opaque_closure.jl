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
    esc(Expr(:opaque_closure, ex))
end

macro opaque(ty, ex)
    esc(Expr(:opaque_closure, ty, ex))
end

# OpaqueClosure construction from pre-inferred CodeInfo/IRCode
using Core.Compiler: IRCode
using Core: CodeInfo

function compute_ir_rettype(ir::IRCode)
    rt = Union{}
    for i = 1:length(ir.stmts)
        stmt = ir.stmts[i][:inst]
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
                            do_compile::Bool = true)
    # NOTE: we need ir.argtypes[1] == typeof(env)
    ir = Core.Compiler.copy(ir)
    nargs = length(ir.argtypes)-1
    sig = compute_oc_signature(ir, nargs, isva)
    rt = compute_ir_rettype(ir)
    src = ccall(:jl_new_code_info_uninit, Ref{CodeInfo}, ())
    src.slotnames = fill(:none, nargs+1)
    src.slotflags = fill(zero(UInt8), length(ir.argtypes))
    src.slottypes = copy(ir.argtypes)
    src.rettype = rt
    src = Core.Compiler.ir_to_codeinf!(src, ir)
    return generate_opaque_closure(sig, Union{}, rt, src, nargs, isva, env...; do_compile)
end

function Core.OpaqueClosure(src::CodeInfo, @nospecialize env...)
    src.inferred || throw(ArgumentError("Expected inferred src::CodeInfo"))
    mi = src.parent::Core.MethodInstance
    sig = Base.tuple_type_tail(mi.specTypes)
    method = mi.def::Method
    nargs = method.nargs-1
    isva = method.isva
    return generate_opaque_closure(sig, Union{}, src.rettype, src, nargs, isva, env...)
end

function generate_opaque_closure(@nospecialize(sig), @nospecialize(rt_lb), @nospecialize(rt_ub),
                                 src::CodeInfo, nargs::Int, isva::Bool, @nospecialize env...;
                                 mod::Module=@__MODULE__,
                                 lineno::Int=0,
                                 file::Union{Nothing,Symbol}=nothing,
                                 do_compile::Bool=true)
    return ccall(:jl_new_opaque_closure_from_code_info, Any, (Any, Any, Any, Any, Any, Cint, Any, Cint, Cint, Any, Cint),
        sig, rt_lb, rt_ub, mod, src, lineno, file, nargs, isva, env, do_compile)
end

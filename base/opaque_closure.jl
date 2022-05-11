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

function Core.OpaqueClosure(ir::IRCode, env...;
        nargs::Int = length(ir.argtypes)-1,
        isva::Bool = false,
        rt = compute_ir_rettype(ir))
    if (isva && nargs > length(ir.argtypes)) || (!isva && nargs != length(ir.argtypes)-1)
        throw(ArgumentError("invalid argument count"))
    end
    src = ccall(:jl_new_code_info_uninit, Ref{CodeInfo}, ())
    src.slotflags = UInt8[]
    src.slotnames = fill(:none, nargs+1)
    src.slottypes = copy(ir.argtypes)
    Core.Compiler.replace_code_newstyle!(src, ir, nargs+1)
    Core.Compiler.widen_all_consts!(src)
    src.inferred = true
    # NOTE: we need ir.argtypes[1] == typeof(env)

    ccall(:jl_new_opaque_closure_from_code_info, Any, (Any, Any, Any, Any, Any, Cint, Any, Cint, Cint, Any),
          Tuple{ir.argtypes[2:end]...}, Union{}, rt, @__MODULE__, src, 0, nothing, nargs, isva, env)
end

function Core.OpaqueClosure(src::CodeInfo, env...)
    M = src.parent.def
    sig = Base.tuple_type_tail(src.parent.specTypes)

    ccall(:jl_new_opaque_closure_from_code_info, Any, (Any, Any, Any, Any, Any, Cint, Any, Cint, Cint, Any),
          sig, Union{}, src.rettype, @__MODULE__, src, 0, nothing, M.nargs - 1, M.isva, env)
end

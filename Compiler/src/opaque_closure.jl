# This file is a part of Julia. License is MIT: https://julialang.org/license

function compute_ir_rettype(ir::IRCode)
    rt = Union{}
    for i = 1:length(ir.stmts)
        stmt = ir[SSAValue(i)][:stmt]
        if isa(stmt, Core.ReturnNode) && isdefined(stmt, :val)
            rt = Compiler.tmerge(Compiler.argextype(stmt.val, ir), rt)
        end
    end
    return Compiler.widenconst(rt)
end

function compute_oc_signature(ir::IRCode, nargs::Int, isva::Bool)
    argtypes = Vector{Any}(undef, nargs)
    for i = 1:nargs
        argtypes[i] = Compiler.widenconst(ir.argtypes[i+1])
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
    src.min_world = ir.valid_worlds.min_world
    src.max_world = ir.valid_worlds.max_world
    src.isva = isva
    src.nargs = UInt(nargtypes)
    src = ir_to_codeinf!(src, ir)
    src.rettype = rt
    return Base.Experimental.generate_opaque_closure(sig, Union{}, rt, src, nargs, isva, env...; kwargs...)
end

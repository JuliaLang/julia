module CompilerDevTools

using Compiler
using Core.IR

include(joinpath(dirname(pathof(Compiler)), "..", "test", "newinterp.jl"))

@newinterp SplitCacheInterp

function generate_codeinst(world::UInt, #=source=#::LineNumberNode, this, fargtypes)
    tt = Base.to_tuple_type(fargtypes)
    match = Base._which(tt; raise=false, world)
    match === nothing && return nothing # method match failed – the fallback implementation will raise a proper MethodError
    mi = Compiler.specialize_method(match)
    interp = SplitCacheInterp(; world)
    new_compiler_ci = Compiler.typeinf_ext(interp, mi, Compiler.SOURCE_MODE_ABI)

    # Construct a CodeInstance that matches `with_new_compiler` and forwards
    # to new_compiler_ci

    src = ccall(:jl_new_code_info_uninit, Ref{CodeInfo}, ())
    src.slotnames = Symbol[:self, :args]
    src.slotflags = fill(zero(UInt8), 2)
    src.slottypes = Any[this, fargtypes]
    src.isva = true
    src.nargs = 2

    code = Any[]
    ssavaluetypes = Any[]
    ncalleeargs = length(fargtypes)
    for i = 1:ncalleeargs
        push!(code, Expr(:call, getfield, Core.Argument(2), i))
        push!(ssavaluetypes, fargtypes[i])
    end
    push!(code, Expr(:invoke, new_compiler_ci, (SSAValue(i) for i = 1:ncalleeargs)...))
    push!(ssavaluetypes, new_compiler_ci.rettype)
    push!(code, ReturnNode(SSAValue(ncalleeargs+1)))
    push!(ssavaluetypes, Union{})
    src.code = code
    src.ssavaluetypes = ssavaluetypes

    return CodeInstance(
        mi, nothing, new_compiler_ci.rettype, new_compiler_ci.exctype,
        isdefined(new_compiler_ci, :rettype_const) ? new_compiler_ci.rettype_const : nothing,
        src,
        isdefined(new_compiler_ci, :rettype_const) ? Int32(0x02) : Int32(0x00),
        new_compiler_ci.min_world, new_compiler_ci.max_world,
        new_compiler_ci.ipo_purity_bits, nothing, new_compiler_ci.relocatability,
        nothing, Core.svec(new_compiler_ci))
end

function refresh()
    @eval function with_new_compiler(args...)
        $(Expr(:meta, :generated_only))
        $(Expr(:meta, :generated, generate_codeinst))
    end
end
refresh()

export with_new_compiler

end

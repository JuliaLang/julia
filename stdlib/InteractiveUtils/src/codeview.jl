# This file is a part of Julia. License is MIT: https://julialang.org/license

# displaying type warnings

function warntype_type_printer(io::IO, @nospecialize(ty), used::Bool)
    used || return
    if ty isa Type && (!Base.isdispatchelem(ty) || ty == Core.Box)
        if ty isa Union && Base.is_expected_union(ty)
            Base.emphasize(io, "::$ty", Base.warn_color()) # more mild user notification
        else
            Base.emphasize(io, "::$ty")
        end
    else
        Base.printstyled(io, "::$ty", color=:cyan) # show the "good" type
    end
    nothing
end

"""
    code_warntype([io::IO], f, types; debuginfo=:default)

Prints lowered and type-inferred ASTs for the methods matching the given generic function
and type signature to `io` which defaults to `stdout`. The ASTs are annotated in such a way
as to cause "non-leaf" types to be emphasized (if color is available, displayed in red).
This serves as a warning of potential type instability. Not all non-leaf types are particularly
problematic for performance, so the results need to be used judiciously.
In particular, unions containing either [`missing`](@ref) or [`nothing`](@ref) are displayed in yellow, since
these are often intentional.

Keyword argument `debuginfo` may be one of `:source` or `:none` (default), to specify the verbosity of code comments.

See [`@code_warntype`](@ref man-code-warntype) for more information.
"""
function code_warntype(io::IO, @nospecialize(f), @nospecialize(t); debuginfo::Symbol=:default, optimize::Bool=false)
    debuginfo = Base.IRShow.debuginfo(debuginfo)
    lineprinter = Base.IRShow.__debuginfo[debuginfo]
    for (src, rettype) in code_typed(f, t, optimize=optimize)
        lambda_io::IOContext = io
        if src.slotnames !== nothing
            slotnames = Base.sourceinfo_slotnames(src)
            lambda_io = IOContext(lambda_io, :SOURCE_SLOTNAMES => slotnames)
            println(io, "Variables")
            slottypes = src.slottypes
            for i = 1:length(slotnames)
                print(io, "  ", slotnames[i])
                if isa(slottypes, Vector{Any})
                    warntype_type_printer(io, slottypes[i], true)
                end
                println(io)
            end
            println(io)
        end
        print(io, "Body")
        warntype_type_printer(io, rettype, true)
        println(io)
        # TODO: static parameter values
        Base.IRShow.show_ir(lambda_io, src, lineprinter(src), warntype_type_printer)
    end
    nothing
end
code_warntype(@nospecialize(f), @nospecialize(t); kwargs...) =
    code_warntype(stdout, f, t; kwargs...)

import Base.CodegenParams

# Printing code representations in IR and assembly
function _dump_function(@nospecialize(f), @nospecialize(t), native::Bool, wrapper::Bool,
                        strip_ir_metadata::Bool, dump_module::Bool, syntax::Symbol,
                        optimize::Bool, debuginfo::Symbol,
                        params::CodegenParams=CodegenParams())
    ccall(:jl_is_in_pure_context, Bool, ()) && error("code reflection cannot be used from generated functions")
    if isa(f, Core.Builtin)
        throw(ArgumentError("argument is not a generic function"))
    end
    # get the MethodInstance for the method match
    world = typemax(UInt)
    meth = which(f, t)
    t = to_tuple_type(t)
    tt = signature_type(f, t)
    (ti, env) = ccall(:jl_type_intersection_with_env, Any, (Any, Any), tt, meth.sig)::Core.SimpleVector
    meth = Base.func_for_method_checked(meth, ti, env)
    linfo = ccall(:jl_specializations_get_linfo, Ref{Core.MethodInstance}, (Any, Any, Any, UInt), meth, ti, env, world)
    # get the code for it
    if native
        str = _dump_function_linfo_native(linfo, world, wrapper, syntax, debuginfo)
    else
        str = _dump_function_linfo_llvm(linfo, world, wrapper, strip_ir_metadata, dump_module, optimize, debuginfo, params)
    end
    # TODO: use jl_is_cacheable_sig instead of isdispatchtuple
    isdispatchtuple(linfo.specTypes) || (str = "; WARNING: This code may not match what actually runs.\n" * str)
    return str
end

function _dump_function_linfo_native(linfo::Core.MethodInstance, world::UInt, wrapper::Bool, syntax::Symbol, debuginfo::Symbol)
    if syntax !== :att && syntax !== :intel
        throw(ArgumentError("'syntax' must be either :intel or :att"))
    end
    if debuginfo === :default
        debuginfo = :source
    elseif debuginfo !== :source && debuginfo !== :none
        throw(ArgumentError("'debuginfo' must be either :source or :none"))
    end
    str = ccall(:jl_dump_method_asm, Ref{String},
                (Any, UInt, Cint, Bool, Ptr{UInt8}, Ptr{UInt8}),
                linfo, world, 0, wrapper, syntax, debuginfo)
    return str
end

function _dump_function_linfo_llvm(
        linfo::Core.MethodInstance, world::UInt, wrapper::Bool,
        strip_ir_metadata::Bool, dump_module::Bool,
        optimize::Bool, debuginfo::Symbol,
        params::CodegenParams)
    if debuginfo === :default
        debuginfo = :source
    elseif debuginfo !== :source && debuginfo !== :none
        throw(ArgumentError("'debuginfo' must be either :source or :none"))
    end
    llvmf = ccall(:jl_get_llvmf_defn, Ptr{Cvoid}, (Any, UInt, Bool, Bool, CodegenParams), linfo, world, wrapper, optimize, params)
    llvmf == C_NULL && error("could not compile the specified method")
    str = ccall(:jl_dump_function_ir, Ref{String},
                (Ptr{Cvoid}, Bool, Bool, Ptr{UInt8}),
                llvmf, strip_ir_metadata, dump_module, debuginfo)
    return str
end

"""
    code_llvm([io=stdout,], f, types; raw=false, dump_module=false, optimize=true, debuginfo=:default)

Prints the LLVM bitcodes generated for running the method matching the given generic
function and type signature to `io`.

If the `optimize` keyword is unset, the code will be shown before LLVM optimizations.
All metadata and dbg.* calls are removed from the printed bitcode. For the full IR, set the `raw` keyword to true.
To dump the entire module that encapsulates the function (with declarations), set the `dump_module` keyword to true.
Keyword argument `debuginfo` may be one of source (default) or none, to specify the verbosity of code comments.
"""
code_llvm(io::IO, @nospecialize(f), @nospecialize(types), raw::Bool, dump_module::Bool=false, optimize::Bool=true, debuginfo::Symbol=:default) =
    print(io, _dump_function(f, types, false, false, !raw, dump_module, :att, optimize, debuginfo))
code_llvm(io::IO, @nospecialize(f), @nospecialize(types=Tuple); raw::Bool=false, dump_module::Bool=false, optimize::Bool=true, debuginfo::Symbol=:default) =
    code_llvm(io, f, types, raw, dump_module, optimize, debuginfo)
code_llvm(@nospecialize(f), @nospecialize(types=Tuple); raw=false, dump_module=false, optimize=true, debuginfo::Symbol=:default) =
    code_llvm(stdout, f, types; raw=raw, dump_module=dump_module, optimize=optimize, debuginfo=debuginfo)


"""
    code_native([io=stdout,], f, types; syntax=:att, debuginfo=:default)

Prints the native assembly instructions generated for running the method matching the given
generic function and type signature to `io`.
Switch assembly syntax using `syntax` symbol parameter set to `:att` for AT&T syntax or `:intel` for Intel syntax.
Keyword argument `debuginfo` may be one of source (default) or none, to specify the verbosity of code comments.
"""
code_native(io::IO, @nospecialize(f), @nospecialize(types=Tuple); syntax::Symbol=:att, debuginfo::Symbol=:default) =
    print(io, _dump_function(f, types, true, false, false, false, syntax, true, debuginfo))
code_native(@nospecialize(f), @nospecialize(types=Tuple); syntax::Symbol=:att, debuginfo::Symbol=:default) =
    code_native(stdout, f, types; syntax=syntax, debuginfo=debuginfo)
code_native(::IO, ::Any, ::Symbol) = error("illegal code_native call") # resolve ambiguous call

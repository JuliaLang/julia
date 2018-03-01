# This file is a part of Julia. License is MIT: https://julialang.org/license

# displaying type warnings

"""
    code_warntype([io::IO], f, types)

Prints lowered and type-inferred ASTs for the methods matching the given generic function
and type signature to `io` which defaults to `stdout`. The ASTs are annotated in such a way
as to cause "non-leaf" types to be emphasized (if color is available, displayed in red).
This serves as a warning of potential type instability. Not all non-leaf types are particularly
problematic for performance, so the results need to be used judiciously.
In particular, unions containing either [`missing`](@ref) or [`nothing`](@ref) are displayed in yellow, since
these are often intentional.
See [`@code_warntype`](@ref man-code-warntype) for more information.
"""
function code_warntype(io::IO, f, @nospecialize(t))
    function slots_used(ci, slotnames)
        used = falses(length(slotnames))
        scan_exprs!(used, ci.code)
        return used
    end

    function scan_exprs!(used, exprs)
        for ex in exprs
            if isa(ex, Core.Slot)
                used[ex.id] = true
            elseif isa(ex, Expr)
                scan_exprs!(used, ex.args)
            end
        end
    end

    emph_io = IOContext(io, :TYPEEMPHASIZE => true)
    for (src, rettype) in code_typed(f, t)
        println(emph_io, "Variables:")
        slotnames = Base.sourceinfo_slotnames(src)
        used_slotids = slots_used(src, slotnames)
        for i = 1:length(slotnames)
            if used_slotids[i]
                print(emph_io, "  ", slotnames[i])
                if isa(src.slottypes, Array)
                    show_expr_type(emph_io, src.slottypes[i], true)
                end
                print(emph_io, '\n')
            elseif !('#' in slotnames[i] || '@' in slotnames[i])
                print(emph_io, "  ", slotnames[i], "<optimized out>\n")
            end
        end
        print(emph_io, "\nBody:\n  ")
        body = Expr(:body)
        body.args = src.code
        body.typ = rettype
        # Fix slot names and types in function body
        show_unquoted(IOContext(emph_io, :SOURCEINFO => src, :SOURCE_SLOTNAMES => slotnames),
                      body, 2)
        print(emph_io, '\n')
    end
    nothing
end
code_warntype(f, @nospecialize(t)) = code_warntype(stdout, f, t)

import Base.CodegenParams

# Printing code representations in IR and assembly
function _dump_function(@nospecialize(f), @nospecialize(t), native::Bool, wrapper::Bool,
                        strip_ir_metadata::Bool, dump_module::Bool, syntax::Symbol=:att,
                        optimize::Bool=true, params::CodegenParams=CodegenParams())
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
    meth = Base.func_for_method_checked(meth, ti)
    linfo = ccall(:jl_specializations_get_linfo, Ref{Core.MethodInstance}, (Any, Any, Any, UInt), meth, ti, env, world)
    # get the code for it
    return _dump_function_linfo(linfo, world, native, wrapper, strip_ir_metadata, dump_module, syntax, optimize, params)
end

function _dump_function_linfo(linfo::Core.MethodInstance, world::UInt, native::Bool, wrapper::Bool,
                              strip_ir_metadata::Bool, dump_module::Bool, syntax::Symbol=:att,
                              optimize::Bool=true, params::CodegenParams=CodegenParams())
    if syntax != :att && syntax != :intel
        throw(ArgumentError("'syntax' must be either :intel or :att"))
    end
    if native
        llvmf = ccall(:jl_get_llvmf_decl, Ptr{Cvoid}, (Any, UInt, Bool, CodegenParams), linfo, world, wrapper, params)
    else
        llvmf = ccall(:jl_get_llvmf_defn, Ptr{Cvoid}, (Any, UInt, Bool, Bool, CodegenParams), linfo, world, wrapper, optimize, params)
    end
    if llvmf == C_NULL
        error("could not compile the specified method")
    end

    if native
        str = ccall(:jl_dump_function_asm, Ref{String},
                    (Ptr{Cvoid}, Cint, Ptr{UInt8}), llvmf, 0, syntax)
    else
        str = ccall(:jl_dump_function_ir, Ref{String},
                    (Ptr{Cvoid}, Bool, Bool), llvmf, strip_ir_metadata, dump_module)
    end

    # TODO: use jl_is_cacheable_sig instead of isdispatchtuple
    isdispatchtuple(linfo.specTypes) || (str = "; WARNING: This code may not match what actually runs.\n" * str)
    return str
end

"""
    code_llvm([io=stdout,], f, types)

Prints the LLVM bitcodes generated for running the method matching the given generic
function and type signature to `io`.

All metadata and dbg.* calls are removed from the printed bitcode. Use code_llvm_raw for the full IR.
"""
code_llvm(io::IO, @nospecialize(f), @nospecialize(types=Tuple), strip_ir_metadata=true, dump_module=false) =
    print(io, _dump_function(f, types, false, false, strip_ir_metadata, dump_module))
code_llvm(@nospecialize(f), @nospecialize(types=Tuple)) = code_llvm(stdout, f, types)
code_llvm_raw(@nospecialize(f), @nospecialize(types=Tuple)) = code_llvm(stdout, f, types, false)

"""
    code_native([io=stdout,], f, types; syntax = :att)

Prints the native assembly instructions generated for running the method matching the given
generic function and type signature to `io`.
Switch assembly syntax using `syntax` symbol parameter set to `:att` for AT&T syntax or `:intel` for Intel syntax.
"""
code_native(io::IO, @nospecialize(f), @nospecialize(types=Tuple); syntax::Symbol = :att) =
    print(io, _dump_function(f, types, true, false, false, false, syntax))
code_native(@nospecialize(f), @nospecialize(types=Tuple); syntax::Symbol = :att) = code_native(stdout, f, types, syntax = syntax)
code_native(::IO, ::Any, ::Symbol) = error("illegal code_native call") # resolve ambiguous call

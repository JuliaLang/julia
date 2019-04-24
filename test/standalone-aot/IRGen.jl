module IRGen

import Libdl

export irgen, dump_native, @jlrun

struct LLVMNativeCode    # thin wrapper
    p::Ptr{Cvoid}
end

"""
Returns an LLVMNativeCode object for the function call `f` with TupleTypes `tt`.
"""
function irgen(@nospecialize(f), @nospecialize(tt))
    # get the method instance
    world = typemax(UInt)
    meth = which(f, tt)
    sig_tt = Tuple{typeof(f), tt.parameters...}
    (ti, env) = ccall(:jl_type_intersection_with_env, Any,
                      (Any, Any), sig_tt, meth.sig)::Core.SimpleVector
    meth = Base.func_for_method_checked(meth, ti)
    linfo = ccall(:jl_specializations_get_linfo, Ref{Core.MethodInstance},
                  (Any, Any, Any, UInt), meth, ti, env, world)

    # set-up the compiler interface
    params = Base.CodegenParams(track_allocations=false,
                                code_coverage=false,
                                static_alloc=false,
                                prefer_specsig=true)

    # generate IR
    ccall(:jl_set_standalone_aot_mode, Nothing, ())
    local llvm_mod_ref
    try
        native_code = ccall(:jl_create_native, Ptr{Cvoid},
                            (Vector{Core.MethodInstance}, Base.CodegenParams), [linfo], params)
        ccall(:jl_clear_standalone_aot_mode, Nothing, ())
        @assert native_code != C_NULL
        return LLVMNativeCode(native_code)
    catch e
        ccall(:jl_clear_standalone_aot_mode, Nothing, ())
        throw(e)
    end
end

"""
Creates an object file from `x`.
"""
dump_native(x::LLVMNativeCode, filename) =
    ccall(:jl_dump_native_lib, Nothing, (Ptr{Cvoid}, Cstring), x.p, filename)

"""
Compiles function call provided and calls it with `ccall` using the shared library that was created.
"""
macro jlrun(e)
    fun = e.args[1]
    efun = esc(fun)
    args = length(e.args) > 1 ? e.args[2:end] : Any[]
    libpath = abspath(string("lib", fun, ".o"))
    dylibpath = abspath(string("lib", fun, ".so"))
    tt = Tuple{(typeof(eval(a)) for a in args)...}
    if length(e.args) > 1
        ct = code_typed(Base.eval(__module__, fun), tt)
    else
        ct = code_typed(Base.eval(__module__, fun))
    end
    rettype = ct[1][2]
    pkgdir = @__DIR__
    bindir = string(Sys.BINDIR, "/../tools")
    quote
        native = irgen($efun, $tt)
        dump_native(native, $libpath)
        run($(`$bindir/clang -shared -fpic $libpath -o $dylibpath -L$bindir/../lib -ljulia-debug -ldSFMT -lopenblas64_`), wait = true)
        dylib = Libdl.dlopen($dylibpath)
        ccall(Libdl.dlsym(dylib, "init_lib"), Cvoid, ()) 
        ccall(Libdl.dlsym(dylib, $(Meta.quot(fun))),
            #   $rettype, ($((typeof(eval(a)) for a in args)...),), $(args...))
              $rettype, ($((typeof(eval(a)) for a in args)...),), $(eval.(args)...))
    end
end

end   # module
# nothing

# This file is a part of Julia. License is MIT: https://julialang.org/license

# Patches to Base needed for trimming

@eval Core begin
    DomainError(@nospecialize(val), @nospecialize(msg::AbstractString)) = (@noinline; $(Expr(:new, :DomainError, :val, :msg)))
end

(f::Base.RedirectStdStream)(io::Core.CoreSTDOUT) = Base._redirect_io_global(io, f.unix_fd)

@eval Base begin
    depwarn(msg, funcsym; force::Bool=false) = nothing
    _assert_tostring(msg) = ""
    reinit_stdio() = nothing
    JuliaSyntax.enable_in_core!() = nothing
    init_active_project() = ACTIVE_PROJECT[] = nothing
    set_active_project(projfile::Union{AbstractString,Nothing}) = ACTIVE_PROJECT[] = projfile
    disable_library_threading() = nothing
    start_profile_listener() = nothing
    invokelatest_trimmed(f, args...; kwargs...) = f(args...; kwargs...)
    const invokelatest = invokelatest_trimmed
    function sprint(f::F, args::Vararg{Any,N}; context=nothing, sizehint::Integer=0) where {F<:Function,N}
        s = IOBuffer(sizehint=sizehint)
        if context isa Tuple
            f(IOContext(s, context...), args...)
        elseif context !== nothing
            f(IOContext(s, context), args...)
        else
            f(s, args...)
        end
        String(_unsafe_take!(s))
    end
    function show_typeish(io::IO, @nospecialize(T))
        if T isa Type
            show(io, T)
        elseif T isa TypeVar
            print(io, (T::TypeVar).name)
        else
            print(io, "?")
        end
    end
    function show(io::IO, T::Type)
        if T isa DataType
            print(io, T.name.name)
            if T !== T.name.wrapper && length(T.parameters) > 0
                print(io, "{")
                first = true
                for p in T.parameters
                    if !first
                        print(io, ", ")
                    end
                    first = false
                    if p isa Int
                        show(io, p)
                    elseif p isa Type
                        show(io, p)
                    elseif p isa Symbol
                        print(io, ":")
                        print(io, p)
                    elseif p isa TypeVar
                        print(io, p.name)
                    else
                        print(io, "?")
                    end
                end
                print(io, "}")
            end
        elseif T isa Union
            print(io, "Union{")
            show_typeish(io, T.a)
            print(io, ", ")
            show_typeish(io, T.b)
            print(io, "}")
        elseif T isa UnionAll
            print(io, T.body::Type)
            print(io, " where ")
            print(io, T.var.name)
        end
    end
    # this function is not `--trim`-compatible if it resolves to a Varargs{...} specialization
    # and since it only has 1-argument methods this happens too often by default (just 2-3 args)
    setfield!(typeof(throw_eachindex_mismatch_indices).name, :max_args, Int32(5), :monotonic)
end
@eval Base.Sys begin
    __init_build() = nothing # VersionNumber parsing is not supported yet
end
# Used for LinearAlgebre ldiv with SVD
for s in [:searchsortedfirst, :searchsortedlast, :searchsorted]
    @eval Base.Sort begin
        # identical to existing Base def. but specializes on `lt` / `by`
        $s(v::AbstractVector, x, o::Ordering) = $s(v,x,firstindex(v),lastindex(v),o)
        $s(v::AbstractVector, x;
            lt::T=isless, by::F=identity, rev::Union{Bool,Nothing}=nothing, order::Ordering=Forward) where {T,F} =
            $s(v,x,ord(lt,by,rev,order))
    end
end
@eval Base.GMP begin
    function __init__() # VersionNumber parsing is not supported yet
        try
            ccall((:__gmp_set_memory_functions, libgmp), Cvoid,
                (Ptr{Cvoid},Ptr{Cvoid},Ptr{Cvoid}),
                cglobal(:jl_gc_counted_malloc),
                cglobal(:jl_gc_counted_realloc_with_old_size),
                cglobal(:jl_gc_counted_free_with_size))
            ZERO.alloc, ZERO.size, ZERO.d = 0, 0, C_NULL
            ONE.alloc, ONE.size, ONE.d = 1, 1, pointer(_ONE)
        catch ex
            Base.showerror_nostdio(ex, "WARNING: Error during initialization of module GMP")
        end
        # This only works with a patched version of GMP, ignore otherwise
        try
            ccall((:__gmp_set_alloc_overflow_function, libgmp), Cvoid,
                (Ptr{Cvoid},),
                cglobal(:jl_throw_out_of_memory_error))
            ALLOC_OVERFLOW_FUNCTION[] = true
        catch ex
            # ErrorException("ccall: could not find function...")
            if typeof(ex) != ErrorException
                rethrow()
            end
        end
    end
end

@eval Base.CoreLogging begin
    # Disable logging (TypedCallable is required to support the existing dynamic
    # logger interface, but it's not implemented yet)
    @inline current_logger_for_env(std_level::LogLevel, group, _module) = nothing
end

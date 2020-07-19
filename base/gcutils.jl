# This file is a part of Julia. License is MIT: https://julialang.org/license

==(w::WeakRef, v::WeakRef) = isequal(w.value, v.value)
==(w::WeakRef, v) = isequal(w.value, v)
==(w, v::WeakRef) = isequal(w, v.value)

"""
    finalizer(f, x)

Register a function `f(x)` to be called when there are no program-accessible references to
`x`, and return `x`. The type of `x` must be a `mutable struct`, otherwise the behavior of
this function is unpredictable.

`f` must not cause a task switch, which excludes most I/O operations such as `println`.
Using the `@async` macro (to defer context switching to outside of the finalizer) or
`ccall` to directly invoke IO functions in C may be helpful for debugging purposes.

# Examples
```julia
finalizer(my_mutable_struct) do x
    @async println("Finalizing \$x.")
end

finalizer(my_mutable_struct) do x
    ccall(:jl_safe_printf, Cvoid, (Cstring, Cstring), "Finalizing %s.", repr(x))
end
```

A finalizer may be registered at object construction. In the following example note that
we implicitly rely on the finalizer returning the newly created mutable struct `x`.

# Example
```julia
mutable struct MyMutableStruct
    bar
    function MyMutableStruct(bar)
        x = new(bar)
        f(t) = @async println("Finalizing \$t.")
        finalizer(f, x)
    end
end
```
"""
function finalizer(@nospecialize(f), @nospecialize(o))
    if !ismutable(o)
        error("objects of type ", typeof(o), " cannot be finalized")
    end
    ccall(:jl_gc_add_finalizer_th, Cvoid, (Ptr{Cvoid}, Any, Any),
          Core.getptls(), o, f)
    return o
end

function finalizer(f::Ptr{Cvoid}, o::T) where T
    @_inline_meta
    if !ismutable(o)
        error("objects of type ", typeof(o), " cannot be finalized")
    end
    ccall(:jl_gc_add_ptr_finalizer, Cvoid, (Ptr{Cvoid}, Any, Ptr{Cvoid}),
          Core.getptls(), o, f)
    return o
end

"""
    finalize(x)

Immediately run finalizers registered for object `x`.
"""
finalize(@nospecialize(o)) = ccall(:jl_finalize_th, Cvoid, (Ptr{Cvoid}, Any,),
                                   Core.getptls(), o)

"""
    Base.GC

Module with garbage collection utilities.
"""
module GC

# mirrored from julia.h
const GC_AUTO = 0
const GC_FULL = 1
const GC_INCREMENTAL = 2

"""
    GC.gc([full=true])

Perform garbage collection. The argument `full` determines the kind of
collection: A full collection (default) sweeps all objects, which makes the
next GC scan much slower, while an incremental collection may only sweep
so-called young objects.

!!! warning
    Excessive use will likely lead to poor performance.
"""
gc(full::Bool=true) =
    ccall(:jl_gc_collect, Cvoid, (Cint,), full ? GC_FULL : GC_INCREMENTAL)

"""
    GC.enable(on::Bool)

Control whether garbage collection is enabled using a boolean argument (`true` for enabled,
`false` for disabled). Return previous GC state.

!!! warning
    Disabling garbage collection should be used only with caution, as it can cause memory
    use to grow without bound.
"""
enable(on::Bool) = ccall(:jl_gc_enable, Int32, (Int32,), on) != 0

"""
    GC.@preserve x1 x2 ... xn expr

Mark the objects `x1, x2, ...` as being *in use* during the evaluation of the
expression `expr`. This is only required in unsafe code where `expr`
*implicitly uses* memory or other resources owned by one of the `x`s.

*Implicit use* of `x` covers any indirect use of resources logically owned by
`x` which the compiler cannot see. Some examples:
* Accessing memory of an object directly via a `Ptr`
* Passing a pointer to `x` to `ccall`
* Using resources of `x` which would be cleaned up in the finalizer.

`@preserve` should generally not have any performance impact in typical use
cases where it briefly extends object lifetime. In implementation, `@preserve`
has effects such as protecting dynamically allocated objects from garbage
collection.

# Examples

When loading from a pointer with `unsafe_load`, the underlying object is
implicitly used, for example `x` is implicitly used by `unsafe_load(p)` in the
following:

```jldoctest
julia> let
           x = Ref{Int}(101)
           p = Base.unsafe_convert(Ptr{Int}, x)
           GC.@preserve x unsafe_load(p)
       end
101
```

When passing pointers to `ccall`, the pointed-to object is implicitly used and
should be preserved. (Note however that you should normally just pass `x`
directly to `ccall` which counts as an explicit use.)

```jldoctest
julia> let
           x = "Hello"
           p = pointer(x)
           GC.@preserve x @ccall strlen(p::Cstring)::Cint
           # Preferred alternative
           @ccall strlen(x::Cstring)::Cint
       end
5
```
"""
macro preserve(args...)
    syms = args[1:end-1]
    for x in syms
        isa(x, Symbol) || error("Preserved variable must be a symbol")
    end
    esc(Expr(:gc_preserve, args[end], syms...))
end

"""
    GC.safepoint()

Inserts a point in the program where garbage collection may run.
This can be useful in rare cases in multi-threaded programs where some threads
are allocating memory (and hence may need to run GC) but other threads are doing
only simple operations (no allocation, task switches, or I/O).
Calling this function periodically in non-allocating threads allows garbage
collection to run.

!!! compat "Julia 1.4"
    This function is available as of Julia 1.4.
"""
safepoint() = ccall(:jl_gc_safepoint, Cvoid, ())

end # module GC

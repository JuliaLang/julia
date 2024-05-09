# This file is a part of Julia. License is MIT: https://julialang.org/license


"""
    WeakRef(x)

`w = WeakRef(x)` constructs a [weak reference](https://en.wikipedia.org/wiki/Weak_reference)
to the Julia value `x`: although `w` contains a reference to `x`, it does not prevent `x` from being
garbage collected. `w.value` is either `x` (if `x` has not been garbage-collected yet) or `nothing`
(if `x` has been garbage-collected).

```jldoctest
julia> x = "a string"
"a string"

julia> w = WeakRef(x)
WeakRef("a string")

julia> GC.gc()

julia> w           # a reference is maintained via `x`
WeakRef("a string")

julia> x = nothing # clear reference

julia> GC.gc()

julia> w
WeakRef(nothing)
```
"""
WeakRef

==(w::WeakRef, v::WeakRef) = isequal(w.value, v.value)
==(w::WeakRef, v) = isequal(w.value, v)
==(w, v::WeakRef) = isequal(w, v.value)

# Used by `Base.finalizer` to validate mutability of an object being finalized.
function _check_mutable(@nospecialize(o)) @noinline
    if !ismutable(o)
        error("objects of type ", typeof(o), " cannot be finalized")
    end
end

"""
    finalizer(f, x)

Register a function `f(x)` to be called when there are no program-accessible references to
`x`, and return `x`. The type of `x` must be a `mutable struct`, otherwise the function
will throw.

`f` must not cause a task switch, which excludes most I/O operations such as `println`.
Using the `@async` macro (to defer context switching to outside of the finalizer) or
`ccall` to directly invoke IO functions in C may be helpful for debugging purposes.

Note that there is no guaranteed world age for the execution of `f`. It may be
called in the world age in which the finalizer was registered or any later world age.

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
    _check_mutable(o)
    Core.finalizer(f, o)
    return o
end

function finalizer(f::Ptr{Cvoid}, o::T) where T @inline
    _check_mutable(o)
    ccall(:jl_gc_add_ptr_finalizer, Cvoid, (Ptr{Cvoid}, Any, Ptr{Cvoid}),
          Core.getptls(), o, f)
    return o
end

"""
    finalize(x)

Immediately run finalizers registered for object `x`.
"""
finalize(@nospecialize(o)) = ccall(:jl_finalize_th, Cvoid, (Any, Any,),
                                   current_task(), o)

"""
    Base.GC

Module with garbage collection utilities.
"""
module GC

public gc, enable, @preserve, safepoint, enable_logging, logging_enabled

# mirrored from julia.h
const GC_AUTO = 0
const GC_FULL = 1
const GC_INCREMENTAL = 2

"""
    GC.gc([full=true])

Perform garbage collection. The argument `full` determines the kind of
collection: a full collection (default) traverses all live objects (i.e. full mark)
and should reclaim memory from all unreachable objects. An incremental collection only
reclaims memory from young objects which are not reachable.

The GC may decide to perform a full collection even if an incremental collection was
requested.

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
    GC.enable_finalizers(on::Bool)

Increment or decrement the counter that controls the running of finalizers on
the current Task. Finalizers will only run when the counter is at zero. (Set
`true` for enabling, `false` for disabling). They may still run concurrently on
another Task or thread.
"""
enable_finalizers(on::Bool) = on ? enable_finalizers() : disable_finalizers()

function enable_finalizers() @inline
    ccall(:jl_gc_enable_finalizers_internal, Cvoid, ())
    if Core.Intrinsics.atomic_pointerref(cglobal(:jl_gc_have_pending_finalizers, Cint), :monotonic) != 0
        ccall(:jl_gc_run_pending_finalizers, Cvoid, (Ptr{Cvoid},), C_NULL)
    end
end

function disable_finalizers() @inline
    ccall(:jl_gc_disable_finalizers_internal, Cvoid, ())
end

"""
    GC.in_finalizer()::Bool

Returns `true` if the current task is running a finalizer, returns `false`
otherwise. Will also return `false` within a finalizer which was inlined by the
compiler's eager finalization optimization, or if `finalize` is called on the
finalizer directly.

The result of this function may be useful, for example, when a finalizer must
wait on a resource to become available; instead of polling the resource in a
`yield` loop (which is not legal to execute within a task running finalizers),
busy polling or an `@async` continuation could be used instead.
"""
function in_finalizer() @inline
    ccall(:jl_gc_is_in_finalizer, Int8, ()) > 0
end

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
           Int(GC.@preserve x @ccall strlen(p::Cstring)::Csize_t)
           # Preferred alternative
           Int(@ccall strlen(x::Cstring)::Csize_t)
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

"""
    GC.enable_logging(on::Bool)

When turned on, print statistics about each GC to stderr.
"""
function enable_logging(on::Bool=true)
    ccall(:jl_enable_gc_logging, Cvoid, (Cint,), on)
end

"""
    GC.logging_enabled()

Return whether GC logging has been enabled via [`GC.enable_logging`](@ref).
"""
function logging_enabled()
    ccall(:jl_is_gc_logging_enabled, Cint, ()) != 0
end

end # module GC

# This file is a part of Julia. License is MIT: https://julialang.org/license

# Copied from package LightBoundsErrors.jl:
#
# * https://github.com/JuliaArrays/LightBoundsErrors.jl
module LightBoundsErrors
    export LightBoundsError, throw_lightboundserror, checkbounds_lightboundserror
    function print_comma_blank(io::IO)
        print(io, ',')
        print(io, ' ')
    end
    function show_splatted(io::IO, iterator)
        ei1 = Iterators.peel(iterator)
        if ei1 === nothing
            return  # `iterator` is empty, return without printing anything
        end
        (e1, i1) = ei1
        show(io, e1)
        ei2 = Iterators.peel(i1)
        if ei2 === nothing
            return  # `iterator` had only a single element, we already printed it, return now
        end
        (e2, i2) = ei2
        print_comma_blank(io)
        show(io, e2)
        for e ∈ i2
            print_comma_blank(io)
            show(io, e)
        end
    end
    """
        LightBoundsError

    A subtype of `Exception` similar to `BoundsError`, but more friendly to the compiler optimizer.

    Use [`throw_lightboundserror`](@ref) to throw `LightBoundsError` unconditionally.

    Use [`checkbounds_lightboundserror`](@ref) to throw `LightBoundsError` conditionally.
    """
    mutable struct LightBoundsError <: Exception
        const collection_type::DataType
        const collection_axes::Tuple
        const requested_indices::Tuple
        function LightBoundsError(; collection_type::DataType, collection_axes::Tuple, requested_indices::Tuple)
            new(collection_type, collection_axes, requested_indices)
        end
    end
    function Base.showerror(io::IO, ex::LightBoundsError)
        show(io, typeof(ex))
        print(io, ": out-of-bounds indexing: `collection[")
        show_splatted(io, ex.requested_indices)
        print(io, "]`, where:\n* `typeof(collection) == ")
        show(io, ex.collection_type)
        print(io, "`\n* `axes(collection) == ")
        show(io, ex.collection_axes)
        print(io, '`')
        nothing
    end
    @noinline function throw_lightboundserror_impl(collection_type::DataType, collection_axes::Tuple, requested_indices::Tuple)
        ex = LightBoundsError(; collection_type, collection_axes, requested_indices)
        throw(ex)
    end
    """
        throw_lightboundserror(x, requested_indices...)

    Throw [`LightBoundsError`](@ref) for the given indexable collection and indices.

    See also:

    * [`checkbounds_lightboundserror`](@ref)

    ### Example usage

    ```julia-repl
    julia> throw_lightboundserror(rand(2, 2), 7)
    ERROR: LightBoundsError: out-of-bounds indexing: `collection[7]`, where:
    * `typeof(collection) == Matrix{Float64}`
    * `axes(collection) == (Base.OneTo(2), Base.OneTo(2))`
    ```
    """
    function throw_lightboundserror(x, requested_indices...)
        collection_type = typeof(x)
        collection_axes = axes(x)
        @noinline throw_lightboundserror_impl(collection_type, collection_axes, requested_indices)
    end
    function checkbounds_lightboundserror_impl(checkbounds, x, requested_indices...)
        is_inbounds = @inline checkbounds(Bool, x, requested_indices...)
        if !is_inbounds
            @inline throw_lightboundserror(x, requested_indices...)
        end
        nothing
    end
    """
        checkbounds_lightboundserror(x, requested_indices...)

    Throw [`LightBoundsError`](@ref) if
    `!checkbounds(Bool, x, requested_indices...)`. Otherwise, return `nothing`.

    See also:

    * [`throw_lightboundserror`](@ref)

    ### Example usage

    ```julia-repl
    julia> checkbounds_lightboundserror(rand(2, 2), 2, 3)
    ERROR: LightBoundsError: out-of-bounds indexing: `collection[2, 3]`, where:
    * `typeof(collection) == Matrix{Float64}`
    * `axes(collection) == (Base.OneTo(2), Base.OneTo(2))`
    ```
    """
    function checkbounds_lightboundserror(x, requested_indices...)
        @inline checkbounds_lightboundserror_impl(checkbounds, x, requested_indices...)
    end
end

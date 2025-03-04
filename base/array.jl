# This file is a part of Julia. License is MIT: https://julialang.org/license

## array.jl: Dense arrays

"""
    DimensionMismatch([msg])

The objects called do not have matching dimensionality. Optional argument `msg` is a
descriptive error string.
"""
struct DimensionMismatch <: Exception
    msg::AbstractString
end
DimensionMismatch() = DimensionMismatch("")

## Type aliases for convenience ##
"""
    AbstractVector{T}

Supertype for one-dimensional arrays (or array-like types) with
elements of type `T`. Alias for [`AbstractArray{T,1}`](@ref).
"""
const AbstractVector{T} = AbstractArray{T,1}

"""
    AbstractMatrix{T}

Supertype for two-dimensional arrays (or array-like types) with
elements of type `T`. Alias for [`AbstractArray{T,2}`](@ref).
"""
const AbstractMatrix{T} = AbstractArray{T,2}

"""
    AbstractVecOrMat{T}

Union type of [`AbstractVector{T}`](@ref) and [`AbstractMatrix{T}`](@ref).
"""
const AbstractVecOrMat{T} = Union{AbstractVector{T}, AbstractMatrix{T}}
const RangeIndex = Union{<:BitInteger, AbstractRange{<:BitInteger}}
const DimOrInd = Union{Integer, AbstractUnitRange}
const IntOrInd = Union{Int, AbstractUnitRange}
const DimsOrInds{N} = NTuple{N,DimOrInd}
const NeedsShaping = Union{Tuple{Integer,Vararg{Integer}}, Tuple{OneTo,Vararg{OneTo}}}

"""
    Array{T,N} <: AbstractArray{T,N}

`N`-dimensional dense array with elements of type `T`.
"""
Array

"""
    Vector{T} <: AbstractVector{T}

One-dimensional dense array with elements of type `T`, often used to represent
a mathematical vector. Alias for [`Array{T,1}`](@ref).

See also [`empty`](@ref), [`similar`](@ref) and [`zero`](@ref) for creating vectors.
"""
const Vector{T} = Array{T,1}

"""
    Matrix{T} <: AbstractMatrix{T}

Two-dimensional dense array with elements of type `T`, often used to represent
a mathematical matrix. Alias for [`Array{T,2}`](@ref).

See also [`fill`](@ref), [`zeros`](@ref), [`undef`](@ref) and [`similar`](@ref)
for creating matrices.
"""
const Matrix{T} = Array{T,2}

"""
    VecOrMat{T}

Union type of [`Vector{T}`](@ref) and [`Matrix{T}`](@ref) which allows functions to accept either a Matrix or a Vector.

# Examples
```jldoctest
julia> Vector{Float64} <: VecOrMat{Float64}
true

julia> Matrix{Float64} <: VecOrMat{Float64}
true

julia> Array{Float64, 3} <: VecOrMat{Float64}
false
```
"""
const VecOrMat{T} = Union{Vector{T}, Matrix{T}}

"""
    DenseArray{T, N} <: AbstractArray{T,N}

`N`-dimensional dense array with elements of type `T`.
The elements of a dense array are stored contiguously in memory.
"""
DenseArray

"""
    DenseVector{T}

One-dimensional [`DenseArray`](@ref) with elements of type `T`. Alias for `DenseArray{T,1}`.
"""
const DenseVector{T} = DenseArray{T,1}

"""
    DenseMatrix{T}

Two-dimensional [`DenseArray`](@ref) with elements of type `T`. Alias for `DenseArray{T,2}`.
"""
const DenseMatrix{T} = DenseArray{T,2}

"""
    DenseVecOrMat{T}

Union type of [`DenseVector{T}`](@ref) and [`DenseMatrix{T}`](@ref).
"""
const DenseVecOrMat{T} = Union{DenseVector{T}, DenseMatrix{T}}

## Basic functions ##

"""
    @_safeindex

This internal macro converts:
- `getindex(xs::Tuple, i::Int)` -> `__safe_getindex(xs, i)`
- `setindex!(xs::Vector{T}, x, i::Int)` -> `__safe_setindex!(xs, x, i)`
to tell the compiler that indexing operations within the applied expression are always
inbounds and do not need to taint `:consistent` and `:nothrow`.
"""
macro _safeindex(ex)
    return esc(_safeindex(ex))
end
function _safeindex(ex)
    isa(ex, Expr) || return ex
    if ex.head === :(=)
        lhs = ex.args[1]
        if isa(lhs, Expr) && lhs.head === :ref # xs[i] = x
            rhs = ex.args[2]
            xs = lhs.args[1]
            args = Vector{Any}(undef, length(lhs.args)-1)
            for i = 2:length(lhs.args)
                args[i-1] = _safeindex(lhs.args[i])
            end
            return Expr(:call, GlobalRef(@__MODULE__, :__safe_setindex!), xs, _safeindex(rhs), args...)
        end
    elseif ex.head === :ref # xs[i]
        return Expr(:call, GlobalRef(@__MODULE__, :__safe_getindex), ex.args...)
    end
    args = Vector{Any}(undef, length(ex.args))
    for i = 1:length(ex.args)
        args[i] = _safeindex(ex.args[i])
    end
    return Expr(ex.head, args...)
end

vect() = Vector{Any}()
function vect(X::T...) where T
    @_terminates_locally_meta
    vec = Vector{T}(undef, length(X))
    @_safeindex for i = 1:length(X)
        vec[i] = X[i]
    end
    return vec
end

"""
    vect(X...)

Create a [`Vector`](@ref) with element type computed from the `promote_typeof` of the argument,
containing the argument list.

# Examples
```jldoctest
julia> a = Base.vect(UInt8(1), 2.5, 1//2)
3-element Vector{Float64}:
 1.0
 2.5
 0.5
```
"""
function vect(X...)
    T = promote_typeof(X...)
    return T[X...]
end

size(a::Array, d::Integer) = size(a, Int(d)::Int)
function size(a::Array, d::Int)
    d < 1 && error("arraysize: dimension out of range")
    sz = getfield(a, :size)
    return d > length(sz) ? 1 : getfield(sz, d, false) # @inbounds
end

asize_from(a::Array, n) = n > ndims(a) ? () : (size(a,n), asize_from(a, n+1)...)

allocatedinline(@nospecialize T::Type) = (@_total_meta; ccall(:jl_stored_inline, Cint, (Any,), T) != Cint(0))

"""
    Base.isbitsunion(::Type{T})

Return whether a type is an "is-bits" Union type, meaning each type included in a Union is [`isbitstype`](@ref).

# Examples
```jldoctest
julia> Base.isbitsunion(Union{Float64, UInt8})
true

julia> Base.isbitsunion(Union{Float64, String})
false
```
"""
isbitsunion(u::Type) = u isa Union && allocatedinline(u)

function _unsetindex!(A::Array, i::Int)
    @inline
    @boundscheck checkbounds(A, i)
    @inbounds _unsetindex!(memoryref(A.ref, i))
    return A
end


# TODO: deprecate this (aligned_sizeof and/or elsize and/or sizeof(Some{T}) are more correct)
elsize(::Type{A}) where {T,A<:Array{T}} = aligned_sizeof(T)
function elsize(::Type{Ptr{T}}) where T
    # this only must return something valid for values which satisfy is_valid_intrinsic_elptr(T),
    # which includes Any and most concrete datatypes
    T === Any && return sizeof(Ptr{Any})
    T isa DataType || sizeof(Any) # throws
    return LLT_ALIGN(Core.sizeof(T), datatype_alignment(T))
end
elsize(::Type{Union{}}, slurp...) = 0

sizeof(a::Array) = length(a) * elsize(typeof(a)) # n.b. this ignores bitsunion bytes, as a historical fact

function isassigned(a::Array, i::Int...)
    @inline
    @_noub_if_noinbounds_meta
    @boundscheck checkbounds(Bool, a, i...) || return false
    ii = _sub2ind(size(a), i...)
    return @inbounds isassigned(memoryrefnew(a.ref, ii, false))
end

function isassigned(a::Vector, i::Int) # slight compiler simplification for the most common case
    @inline
    @_noub_if_noinbounds_meta
    @boundscheck checkbounds(Bool, a, i) || return false
    return @inbounds isassigned(memoryrefnew(a.ref, i, false))
end


## copy ##

"""
    unsafe_copyto!(dest::Ptr{T}, src::Ptr{T}, N)

Copy `N` elements from a source pointer to a destination, with no checking. The size of an
element is determined by the type of the pointers.

The `unsafe` prefix on this function indicates that no validation is performed on the
pointers `dest` and `src` to ensure that they are valid. Incorrect usage may corrupt or
segfault your program, in the same manner as C.
"""
function unsafe_copyto!(dest::Ptr{T}, src::Ptr{T}, n) where T
    # Do not use this to copy data between pointer arrays.
    # It can't be made safe no matter how carefully you checked.
    memmove(dest, src, n * aligned_sizeof(T))
    return dest
end

"""
    unsafe_copyto!(dest::Array, doffs, src::Array, soffs, n)

Copy `n` elements from a source array to a destination, starting at the linear index `soffs` in the
source and `doffs` in the destination (1-indexed).

The `unsafe` prefix on this function indicates that no validation is performed to ensure
that n is inbounds on either array. Incorrect usage may corrupt or segfault your program, in
the same manner as C.
"""
function unsafe_copyto!(dest::Array, doffs, src::Array, soffs, n)
    n == 0 && return dest
    unsafe_copyto!(memoryref(dest.ref, doffs), memoryref(src.ref, soffs), n)
    return dest
end

"""
    copyto!(dest, doffs, src, soffs, n)

Copy `n` elements from collection `src` starting at the linear index `soffs`, to array `dest` starting at
the index `doffs`. Return `dest`.
"""
copyto!(dest::Array, doffs::Integer, src::Array, soffs::Integer, n::Integer) = _copyto_impl!(dest, doffs, src, soffs, n)
copyto!(dest::Array, doffs::Integer, src::Memory, soffs::Integer, n::Integer) = _copyto_impl!(dest, doffs, src, soffs, n)
copyto!(dest::Memory, doffs::Integer, src::Array, soffs::Integer, n::Integer) = _copyto_impl!(dest, doffs, src, soffs, n)

# this is only needed to avoid possible ambiguities with methods added in some packages
copyto!(dest::Array{T}, doffs::Integer, src::Array{T}, soffs::Integer, n::Integer) where {T} = _copyto_impl!(dest, doffs, src, soffs, n)

function _copyto_impl!(dest::Union{Array,Memory}, doffs::Integer, src::Union{Array,Memory}, soffs::Integer, n::Integer)
    n == 0 && return dest
    n > 0 || _throw_argerror("Number of elements to copy must be non-negative.")
    @boundscheck checkbounds(dest, doffs:doffs+n-1)
    @boundscheck checkbounds(src, soffs:soffs+n-1)
    @inbounds let dest = memoryref(dest isa Array ? getfield(dest, :ref) : dest, doffs),
                  src = memoryref(src isa Array ? getfield(src, :ref) : src, soffs)
        unsafe_copyto!(dest, src, n)
    end
    return dest
end


# Outlining this because otherwise a catastrophic inference slowdown
# occurs, see discussion in #27874.
# It is also mitigated by using a constant string.
_throw_argerror(s) = (@noinline; throw(ArgumentError(s)))

copyto!(dest::Array, src::Array) = copyto!(dest, 1, src, 1, length(src))

# also to avoid ambiguities in packages
copyto!(dest::Array{T}, src::Array{T}) where {T} = copyto!(dest, 1, src, 1, length(src))

# N.B: The generic definition in multidimensional.jl covers, this, this is just here
# for bootstrapping purposes.
function fill!(dest::Array{T}, x) where T
    @inline
    x = x isa T ? x : convert(T, x)::T
    return _fill!(dest, x)
end
function _fill!(dest::Array{T}, x::T) where T
    for i in eachindex(dest)
        @inbounds dest[i] = x
    end
    return dest
end

"""
    copy(x)

Create a shallow copy of `x`: the outer structure is copied, but not all internal values.
For example, copying an array produces a new array with identically-same elements as the
original.

See also [`copy!`](@ref Base.copy!), [`copyto!`](@ref), [`deepcopy`](@ref).
"""
copy

@eval function copy(a::Array)
    # `copy` only throws when the size exceeds the max allocation size,
    # but since we're copying an existing array, we're guaranteed that this will not happen.
    @_nothrow_meta
    ref = a.ref
    newmem = typeof(ref.mem)(undef, length(a))
    @inbounds unsafe_copyto!(memoryref(newmem), ref, length(a))
    return $(Expr(:new, :(typeof(a)), :(memoryref(newmem)), :(a.size)))
end

# a mutating version of copyto! that results in dst aliasing src afterwards
function _take!(dst::Array{T,N}, src::Array{T,N}) where {T,N}
    if getfield(dst, :ref) !== getfield(src, :ref)
        setfield!(dst, :ref, getfield(src, :ref))
    end
    if getfield(dst, :size) !== getfield(src, :size)
        setfield!(dst, :size, getfield(src, :size))
    end
    return dst
end

## Constructors ##

similar(a::Array{T,1}) where {T}                    = Vector{T}(undef, size(a,1))
similar(a::Array{T,2}) where {T}                    = Matrix{T}(undef, size(a,1), size(a,2))
similar(a::Array{T,1}, S::Type) where {T}           = Vector{S}(undef, size(a,1))
similar(a::Array{T,2}, S::Type) where {T}           = Matrix{S}(undef, size(a,1), size(a,2))
similar(a::Array{T}, m::Int) where {T}              = Vector{T}(undef, m)
similar(a::Array, T::Type, dims::Dims{N}) where {N} = Array{T,N}(undef, dims)
similar(a::Array{T}, dims::Dims{N}) where {T,N}     = Array{T,N}(undef, dims)

# T[x...] constructs Array{T,1}
"""
    getindex(type[, elements...])

Construct a 1-d array of the specified type. This is usually called with the syntax
`Type[]`. Element values can be specified using `Type[a,b,c,...]`.

# Examples
```jldoctest
julia> Int8[1, 2, 3]
3-element Vector{Int8}:
 1
 2
 3

julia> getindex(Int8, 1, 2, 3)
3-element Vector{Int8}:
 1
 2
 3
```
"""
function getindex(::Type{T}, vals...) where T
    @inline
    @_effect_free_terminates_locally_meta
    a = Vector{T}(undef, length(vals))
    if vals isa NTuple
        @_safeindex for i in 1:length(vals)
            a[i] = vals[i]
        end
    else
        # use afoldl to avoid type instability inside loop
        afoldl(1, vals...) do i, v
            @inbounds a[i] = v
            return i + 1
        end
    end
    return a
end

function getindex(::Type{Any}, @nospecialize vals...)
    @_effect_free_terminates_locally_meta
    a = Vector{Any}(undef, length(vals))
    @_safeindex for i = 1:length(vals)
        a[i] = vals[i]
    end
    return a
end
getindex(::Type{Any}) = Vector{Any}()

function fill!(a::Union{Array{UInt8}, Array{Int8}}, x::Integer)
    ref = a.ref
    t = @_gc_preserve_begin ref
    p = unsafe_convert(Ptr{Cvoid}, ref)
    memset(p, x isa eltype(a) ? x : convert(eltype(a), x), length(a) % UInt)
    @_gc_preserve_end t
    return a
end

to_dim(d::Integer) = d
to_dim(d::OneTo) = last(d)

"""
    fill(value, dims::Tuple)
    fill(value, dims...)

Create an array of size `dims` with every location set to `value`.

For example, `fill(1.0, (5,5))` returns a 5×5 array of floats,
with `1.0` in every location of the array.

The dimension lengths `dims` may be specified as either a tuple or a sequence of arguments.
An `N`-length tuple or `N` arguments following the `value` specify an `N`-dimensional
array. Thus, a common idiom for creating a zero-dimensional array with its only location
set to `x` is `fill(x)`.

Every location of the returned array is set to (and is thus [`===`](@ref) to)
the `value` that was passed; this means that if the `value` is itself modified,
all elements of the `fill`ed array will reflect that modification because they're
_still_ that very `value`. This is of no concern with `fill(1.0, (5,5))` as the
`value` `1.0` is immutable and cannot itself be modified, but can be unexpected
with mutable values like — most commonly — arrays.  For example, `fill([], 3)`
places _the very same_ empty array in all three locations of the returned vector:

```jldoctest
julia> v = fill([], 3)
3-element Vector{Vector{Any}}:
 []
 []
 []

julia> v[1] === v[2] === v[3]
true

julia> value = v[1]
Any[]

julia> push!(value, 867_5309)
1-element Vector{Any}:
 8675309

julia> v
3-element Vector{Vector{Any}}:
 [8675309]
 [8675309]
 [8675309]
```

To create an array of many independent inner arrays, use a [comprehension](@ref man-comprehensions) instead.
This creates a new and distinct array on each iteration of the loop:

```jldoctest
julia> v2 = [[] for _ in 1:3]
3-element Vector{Vector{Any}}:
 []
 []
 []

julia> v2[1] === v2[2] === v2[3]
false

julia> push!(v2[1], 8675309)
1-element Vector{Any}:
 8675309

julia> v2
3-element Vector{Vector{Any}}:
 [8675309]
 []
 []
```

See also: [`fill!`](@ref), [`zeros`](@ref), [`ones`](@ref), [`similar`](@ref).

# Examples
```jldoctest
julia> fill(1.0, (2,3))
2×3 Matrix{Float64}:
 1.0  1.0  1.0
 1.0  1.0  1.0

julia> fill(42)
0-dimensional Array{Int64, 0}:
42

julia> A = fill(zeros(2), 2) # sets both elements to the same [0.0, 0.0] vector
2-element Vector{Vector{Float64}}:
 [0.0, 0.0]
 [0.0, 0.0]

julia> A[1][1] = 42; # modifies the filled value to be [42.0, 0.0]

julia> A # both A[1] and A[2] are the very same vector
2-element Vector{Vector{Float64}}:
 [42.0, 0.0]
 [42.0, 0.0]
```
"""
function fill end

fill(v, dims::DimOrInd...) = fill(v, dims)
fill(v, dims::NTuple{N, Union{Integer, OneTo}}) where {N} = fill(v, map(to_dim, dims))
fill(v, dims::NTuple{N, Integer}) where {N} = (a=Array{typeof(v),N}(undef, dims); fill!(a, v); a)
fill(v, dims::NTuple{N, DimOrInd}) where {N} = (a=similar(Array{typeof(v),N}, dims); fill!(a, v); a)
fill(v, dims::Tuple{}) = (a=Array{typeof(v),0}(undef, dims); fill!(a, v); a)

"""
    zeros([T=Float64,] dims::Tuple)
    zeros([T=Float64,] dims...)

Create an `Array`, with element type `T`, of all zeros with size specified by `dims`.
See also [`fill`](@ref), [`ones`](@ref), [`zero`](@ref).

# Examples
```jldoctest
julia> zeros(1)
1-element Vector{Float64}:
 0.0

julia> zeros(Int8, 2, 3)
2×3 Matrix{Int8}:
 0  0  0
 0  0  0
```
"""
function zeros end

"""
    ones([T=Float64,] dims::Tuple)
    ones([T=Float64,] dims...)

Create an `Array`, with element type `T`, of all ones with size specified by `dims`.
See also [`fill`](@ref), [`zeros`](@ref).

# Examples
```jldoctest
julia> ones(1,2)
1×2 Matrix{Float64}:
 1.0  1.0

julia> ones(ComplexF64, 2, 3)
2×3 Matrix{ComplexF64}:
 1.0+0.0im  1.0+0.0im  1.0+0.0im
 1.0+0.0im  1.0+0.0im  1.0+0.0im
```
"""
function ones end

for (fname, felt) in ((:zeros, :zero), (:ones, :one))
    @eval begin
        $fname(dims::DimOrInd...) = $fname(dims)
        $fname(::Type{T}, dims::DimOrInd...) where {T} = $fname(T, dims)
        $fname(dims::Tuple{Vararg{DimOrInd}}) = $fname(Float64, dims)
        $fname(::Type{T}, dims::NTuple{N, Union{Integer, OneTo}}) where {T,N} = $fname(T, map(to_dim, dims))
        function $fname(::Type{T}, dims::NTuple{N, Integer}) where {T,N}
            a = Array{T,N}(undef, dims)
            fill!(a, $felt(T))
            return a
        end
        function $fname(::Type{T}, dims::Tuple{}) where {T}
            a = Array{T}(undef)
            fill!(a, $felt(T))
            return a
        end
        function $fname(::Type{T}, dims::NTuple{N, DimOrInd}) where {T,N}
            a = similar(Array{T,N}, dims)
            fill!(a, $felt(T))
            return a
        end
    end
end

## Conversions ##

convert(::Type{T}, a::AbstractArray) where {T<:Array} = a isa T ? a : T(a)::T

promote_rule(a::Type{Array{T,n}}, b::Type{Array{S,n}}) where {T,n,S} = el_same(promote_type(T,S), a, b)

## Constructors ##

# constructors should make copies
Array{T,N}(x::AbstractArray{S,N})         where {T,N,S} = copyto_axcheck!(Array{T,N}(undef, size(x)), x)
AbstractArray{T,N}(A::AbstractArray{S,N}) where {T,N,S} = copyto_axcheck!(similar(A,T), A)

## copying iterators to containers

"""
    collect(element_type, collection)

Return an `Array` with the given element type of all items in a collection or iterable.
The result has the same shape and number of dimensions as `collection`.

# Examples
```jldoctest
julia> collect(Float64, 1:2:5)
3-element Vector{Float64}:
 1.0
 3.0
 5.0
```
"""
collect(::Type{T}, itr) where {T} = _collect(T, itr, IteratorSize(itr))

_collect(::Type{T}, itr, isz::Union{HasLength,HasShape}) where {T} =
    copyto!(_array_for(T, isz, _similar_shape(itr, isz)), itr)
function _collect(::Type{T}, itr, isz::SizeUnknown) where T
    a = Vector{T}()
    for x in itr
        push!(a, x)
    end
    return a
end

# make a collection similar to `c` and appropriate for collecting `itr`
_similar_for(c, ::Type{T}, itr, isz, shp) where {T} = similar(c, T)

_similar_shape(itr, ::SizeUnknown) = nothing
_similar_shape(itr, ::HasLength) = length(itr)::Integer
_similar_shape(itr, ::HasShape) = axes(itr)

_similar_for(c::AbstractArray, ::Type{T}, itr, ::SizeUnknown, ::Nothing) where {T} =
    similar(c, T, 0)
_similar_for(c::AbstractArray, ::Type{T}, itr, ::HasLength, len::Integer) where {T} =
    similar(c, T, len)
_similar_for(c::AbstractArray, ::Type{T}, itr, ::HasShape, axs) where {T} =
    similar(c, T, axs)

# make a collection appropriate for collecting `itr::Generator`
_array_for(::Type{T}, ::SizeUnknown, ::Nothing) where {T} = Vector{T}(undef, 0)
_array_for(::Type{T}, ::HasLength, len::Integer) where {T} = Vector{T}(undef, Int(len))
_array_for(::Type{T}, ::HasShape{N}, axs) where {T,N} = similar(Array{T,N}, axs)

# used by syntax lowering for simple typed comprehensions
_array_for(::Type{T}, itr, isz) where {T} = _array_for(T, isz, _similar_shape(itr, isz))


"""
    collect(iterator)

Return an `Array` of all items in a collection or iterator. For dictionaries, returns
a `Vector` of `key=>value` [Pair](@ref Pair)s. If the argument is array-like or is an iterator
with the [`HasShape`](@ref IteratorSize) trait, the result will have the same shape
and number of dimensions as the argument.

Used by [comprehensions](@ref man-comprehensions) to turn a [generator expression](@ref man-generators)
into an `Array`. Thus, *on generators*, the square-brackets notation may be used instead of calling `collect`,
see second example.

The element type of the returned array is based on the types of the values collected. However, if the
iterator is empty then the element type of the returned (empty) array is determined by type inference.

# Examples

Collect items from a `UnitRange{Int64}` collection:

```jldoctest
julia> collect(1:3)
3-element Vector{Int64}:
 1
 2
 3
```

Collect items from a generator (same output as `[x^2 for x in 1:3]`):

```jldoctest
julia> collect(x^2 for x in 1:3)
3-element Vector{Int64}:
 1
 4
 9
```

Collecting an empty iterator where the result type depends on type inference:

```jldoctest
julia> [rand(Bool) ? 1 : missing for _ in []]
Union{Missing, Int64}[]
```

When the iterator is non-empty, the result type depends only on values:

```julia-repl
julia> [rand(Bool) ? 1 : missing for _ in [""]]
1-element Vector{Int64}:
 1
```
"""
collect(itr) = _collect(1:1 #= Array =#, itr, IteratorEltype(itr), IteratorSize(itr))

collect(A::AbstractArray) = _collect_indices(axes(A), A)

collect_similar(cont, itr) = _collect(cont, itr, IteratorEltype(itr), IteratorSize(itr))

_collect(cont, itr, ::HasEltype, isz::Union{HasLength,HasShape}) =
    copyto!(_similar_for(cont, eltype(itr), itr, isz, _similar_shape(itr, isz)), itr)

function _collect(cont, itr, ::HasEltype, isz::SizeUnknown)
    a = _similar_for(cont, eltype(itr), itr, isz, nothing)
    for x in itr
        push!(a,x)
    end
    return a
end

_collect_indices(::Tuple{}, A) = copyto!(Array{eltype(A),0}(undef), A)
_collect_indices(indsA::Tuple{Vararg{OneTo}}, A) =
    copyto!(Array{eltype(A)}(undef, length.(indsA)), A)
function _collect_indices(indsA, A)
    B = Array{eltype(A)}(undef, length.(indsA))
    copyto!(B, CartesianIndices(axes(B)), A, CartesianIndices(indsA))
end

# NOTE: this function is not meant to be called, only inferred, for the
# purpose of bounding the types of values generated by an iterator.
function _iterator_upper_bound(itr)
    x = iterate(itr)
    while x !== nothing
        val = getfield(x, 1)
        if inferencebarrier(nothing)
            return val
        end
        x = iterate(itr, getfield(x, 2))
    end
    throw(nothing)
end

# define this as a macro so that the call to Core.Compiler
# gets inlined into the caller before recursion detection
# gets a chance to see it, so that recursive calls to the caller
# don't trigger the inference limiter
macro default_eltype(itr)
    I = esc(itr)
    return quote
        if $I isa Generator && ($I).f isa Type
            T = ($I).f
        else
            T = Base._return_type(_iterator_upper_bound, Tuple{typeof($I)})
        end
        promote_typejoin_union(T)
    end
end

function collect(itr::Generator)
    isz = IteratorSize(itr.iter)
    et = @default_eltype(itr)
    if isa(isz, SizeUnknown)
        return grow_to!(Vector{et}(), itr)
    else
        shp = _similar_shape(itr, isz)
        y = iterate(itr)
        if y === nothing
            return _array_for(et, isz, shp)
        end
        v1, st = y
        dest = _array_for(typeof(v1), isz, shp)
        # The typeassert gives inference a helping hand on the element type and dimensionality
        # (work-around for #28382)
        et′ = et <: Type ? Type : et
        RT = dest isa AbstractArray ? AbstractArray{<:et′, ndims(dest)} : Any
        collect_to_with_first!(dest, v1, itr, st)::RT
    end
end

_collect(c, itr, ::EltypeUnknown, isz::SizeUnknown) =
    grow_to!(_similar_for(c, @default_eltype(itr), itr, isz, nothing), itr)

function _collect(c, itr, ::EltypeUnknown, isz::Union{HasLength,HasShape})
    et = @default_eltype(itr)
    shp = _similar_shape(itr, isz)
    y = iterate(itr)
    if y === nothing
        return _similar_for(c, et, itr, isz, shp)
    end
    v1, st = y
    dest = _similar_for(c, typeof(v1), itr, isz, shp)
    # The typeassert gives inference a helping hand on the element type and dimensionality
    # (work-around for #28382)
    et′ = et <: Type ? Type : et
    RT = dest isa AbstractArray ? AbstractArray{<:et′, ndims(dest)} : Any
    collect_to_with_first!(dest, v1, itr, st)::RT
end

function collect_to_with_first!(dest::AbstractArray, v1, itr, st)
    i1 = first(LinearIndices(dest))
    dest[i1] = v1
    return collect_to!(dest, itr, i1+1, st)
end

function collect_to_with_first!(dest, v1, itr, st)
    push!(dest, v1)
    return grow_to!(dest, itr, st)
end

function setindex_widen_up_to(dest::AbstractArray{T}, el, i) where T
    @inline
    new = similar(dest, promote_typejoin(T, typeof(el)))
    f = first(LinearIndices(dest))
    copyto!(new, first(LinearIndices(new)), dest, f, i-f)
    @inbounds new[i] = el
    return new
end

function collect_to!(dest::AbstractArray{T}, itr, offs, st) where T
    # collect to dest array, checking the type of each result. if a result does not
    # match, widen the result type and re-dispatch.
    i = offs
    while true
        y = iterate(itr, st)
        y === nothing && break
        el, st = y
        if el isa T
            @inbounds dest[i] = el
            i += 1
        else
            new = setindex_widen_up_to(dest, el, i)
            return collect_to!(new, itr, i+1, st)
        end
    end
    return dest
end

function grow_to!(dest, itr)
    y = iterate(itr)
    y === nothing && return dest
    dest2 = empty(dest, typeof(y[1]))
    push!(dest2, y[1])
    grow_to!(dest2, itr, y[2])
end

function push_widen(dest, el)
    @inline
    new = sizehint!(empty(dest, promote_typejoin(eltype(dest), typeof(el))), length(dest))
    if new isa AbstractSet
        # TODO: merge back these two branches when copy! is re-enabled for sets/vectors
        union!(new, dest)
    else
        append!(new, dest)
    end
    push!(new, el)
    return new
end

function grow_to!(dest, itr, st)
    T = eltype(dest)
    y = iterate(itr, st)
    while y !== nothing
        el, st = y
        if el isa T
            push!(dest, el)
        else
            new = push_widen(dest, el)
            return grow_to!(new, itr, st)
        end
        y = iterate(itr, st)
    end
    return dest
end

## Iteration ##

iterate(A::Array, i=1) = (@inline; (i - 1)%UInt < length(A)%UInt ? (@inbounds A[i], i + 1) : nothing)

## Indexing: getindex ##

"""
    getindex(collection, key...)

Retrieve the value(s) stored at the given key or index within a collection. The syntax
`a[i,j,...]` is converted by the compiler to `getindex(a, i, j, ...)`.

See also [`get`](@ref), [`keys`](@ref), [`eachindex`](@ref).

# Examples
```jldoctest
julia> A = Dict("a" => 1, "b" => 2)
Dict{String, Int64} with 2 entries:
  "b" => 2
  "a" => 1

julia> getindex(A, "a")
1
```
"""
function getindex end

function getindex(A::Array, i1::Int, i2::Int, I::Int...)
    @inline
    @boundscheck checkbounds(A, i1, i2, I...) # generally _to_linear_index requires bounds checking
    return @inbounds A[_to_linear_index(A, i1, i2, I...)]
end

# Faster contiguous indexing using copyto! for AbstractUnitRange and Colon
function getindex(A::Array, I::AbstractUnitRange{<:Integer})
    @inline
    @boundscheck checkbounds(A, I)
    lI = length(I)
    X = similar(A, axes(I))
    if lI > 0
        copyto!(X, firstindex(X), A, first(I), lI)
    end
    return X
end

# getindex for carrying out logical indexing for AbstractUnitRange{Bool} as Bool <: Integer
getindex(a::Array, r::AbstractUnitRange{Bool}) = getindex(a, to_index(r))

function getindex(A::Array, c::Colon)
    lI = length(A)
    X = similar(A, lI)
    if lI > 0
        unsafe_copyto!(X, 1, A, 1, lI)
    end
    return X
end

# This is redundant with the abstract fallbacks, but needed for bootstrap
function getindex(A::Array{S}, I::AbstractRange{Int}) where S
    return S[ A[i] for i in I ]
end

## Indexing: setindex! ##

"""
    setindex!(collection, value, key...)

Store the given value at the given key or index within a collection. The syntax `a[i,j,...] =
x` is converted by the compiler to `(setindex!(a, x, i, j, ...); x)`.

# Examples
```jldoctest
julia> a = Dict("a"=>1)
Dict{String, Int64} with 1 entry:
  "a" => 1

julia> setindex!(a, 2, "b")
Dict{String, Int64} with 2 entries:
  "b" => 2
  "a" => 1
```
"""
function setindex! end

function setindex!(A::Array{T}, x, i::Int) where {T}
    @_propagate_inbounds_meta
    x = x isa T ? x : convert(T, x)::T
    return _setindex!(A, x, i)
end
function _setindex!(A::Array{T}, x::T, i::Int) where {T}
    @_noub_if_noinbounds_meta
    @boundscheck (i - 1)%UInt < length(A)%UInt || throw_boundserror(A, (i,))
    memoryrefset!(memoryrefnew(A.ref, i, false), x, :not_atomic, false)
    return A
end
function setindex!(A::Array{T}, x, i1::Int, i2::Int, I::Int...) where {T}
    @_propagate_inbounds_meta
    x = x isa T ? x : convert(T, x)::T
    return _setindex!(A, x, i1, i2, I...)
end
function _setindex!(A::Array{T}, x::T, i1::Int, i2::Int, I::Int...) where {T}
    @inline
    @_noub_if_noinbounds_meta
    @boundscheck checkbounds(A, i1, i2, I...) # generally _to_linear_index requires bounds checking
    memoryrefset!(memoryrefnew(A.ref, _to_linear_index(A, i1, i2, I...), false), x, :not_atomic, false)
    return A
end

__safe_setindex!(A::Vector{Any}, @nospecialize(x), i::Int) = (@inline; @_nothrow_noub_meta;
    memoryrefset!(memoryrefnew(A.ref, i, false), x, :not_atomic, false); return A)
__safe_setindex!(A::Vector{T}, x::T, i::Int) where {T} = (@inline; @_nothrow_noub_meta;
    memoryrefset!(memoryrefnew(A.ref, i, false), x, :not_atomic, false); return A)
__safe_setindex!(A::Vector{T}, x,    i::Int) where {T} = (@inline;
    __safe_setindex!(A, convert(T, x)::T, i))

# This is redundant with the abstract fallbacks but needed and helpful for bootstrap
function setindex!(A::Array, X::AbstractArray, I::AbstractVector{Int})
    @_propagate_inbounds_meta
    @boundscheck setindex_shape_check(X, length(I))
    @boundscheck checkbounds(A, I)
    require_one_based_indexing(X)
    X′ = unalias(A, X)
    I′ = unalias(A, I)
    count = 1
    for i in I′
        @inbounds A[i] = X′[count]
        count += 1
    end
    return A
end

# Faster contiguous setindex! with copyto!
function setindex!(A::Array{T}, X::Array{T}, I::AbstractUnitRange{Int}) where T
    @inline
    @boundscheck checkbounds(A, I)
    lI = length(I)
    @boundscheck setindex_shape_check(X, lI)
    if lI > 0
        unsafe_copyto!(A, first(I), X, 1, lI)
    end
    return A
end
function setindex!(A::Array{T}, X::Array{T}, c::Colon) where T
    @inline
    lI = length(A)
    @boundscheck setindex_shape_check(X, lI)
    if lI > 0
        unsafe_copyto!(A, 1, X, 1, lI)
    end
    return A
end

# Pick new memory size for efficiently growing an array
# TODO: This should know about the size of our GC pools
# Specifically we are wasting ~10% of memory for small arrays
# by not picking memory sizes that max out a GC pool
function overallocation(maxsize)
    maxsize < 8 && return 8;
    # compute maxsize = maxsize + 4*maxsize^(7/8) + maxsize/8
    # for small n, we grow faster than O(n)
    # for large n, we grow at O(n/8)
    # and as we reach O(memory) for memory>>1MB,
    # this means we end by adding about 10% of memory each time
    exp2 = sizeof(maxsize) * 8 - Core.Intrinsics.ctlz_int(maxsize)
    maxsize += (1 << div(exp2 * 7, 8)) * 4 + div(maxsize, 8)
    return maxsize
end

array_new_memory(mem::Memory, newlen::Int) = typeof(mem)(undef, newlen) # when implemented, this should attempt to first expand mem

function _growbeg!(a::Vector, delta::Integer)
    @_noub_meta
    delta = Int(delta)
    delta == 0 && return # avoid attempting to index off the end
    delta >= 0 || throw(ArgumentError("grow requires delta >= 0"))
    ref = a.ref
    mem = ref.mem
    len = length(a)
    offset = memoryrefoffset(ref)
    newlen = len + delta
    setfield!(a, :size, (newlen,))
    # if offset is far enough advanced to fit data in existing memory without copying
    if delta <= offset - 1
        setfield!(a, :ref, @inbounds memoryref(ref, 1 - delta))
    else
        @noinline (function()
        @_terminates_locally_meta
        memlen = length(mem)
        if offset + len - 1 > memlen || offset < 1
            throw(ConcurrencyViolationError("Vector has invalid state. Don't modify internal fields incorrectly, or resize without correct locks"))
        end
        # since we will allocate the array in the middle of the memory we need at least 2*delta extra space
        # the +1 is because I didn't want to have an off by 1 error.
        newmemlen = max(overallocation(len), len + 2 * delta + 1)
        newoffset = div(newmemlen - newlen, 2) + 1
        # If there is extra data after the end of the array we can use that space so long as there is enough
        # space at the end that there won't be quadratic behavior with a mix of growth from both ends.
        # Specifically, we want to ensure that we will only do this operation once before
        # increasing the size of the array, and that we leave enough space at both the beginning and the end.
        if newoffset + newlen < memlen
            newoffset = div(memlen - newlen, 2) + 1
            newmem = mem
            unsafe_copyto!(newmem, newoffset + delta, mem, offset, len)
            for j in offset:newoffset+delta-1
                @inbounds _unsetindex!(mem, j)
            end
        else
            newmem = array_new_memory(mem, newmemlen)
            unsafe_copyto!(newmem, newoffset + delta, mem, offset, len)
        end
        if ref !== a.ref
            @noinline throw(ConcurrencyViolationError("Vector can not be resized concurrently"))
        end
        setfield!(a, :ref, @inbounds memoryref(newmem, newoffset))
        end)()
    end
    return
end

function _growend!(a::Vector, delta::Integer)
    @_noub_meta
    delta = Int(delta)
    delta >= 0 || throw(ArgumentError("grow requires delta >= 0"))
    ref = a.ref
    mem = ref.mem
    memlen = length(mem)
    len = length(a)
    newlen = len + delta
    offset = memoryrefoffset(ref)
    setfield!(a, :size, (newlen,))
    newmemlen = offset + newlen - 1
    if memlen < newmemlen
        @noinline (function()
        if offset + len - 1 > memlen || offset < 1
            throw(ConcurrencyViolationError("Vector has invalid state. Don't modify internal fields incorrectly, or resize without correct locks"))
        end

        if offset - 1 > div(5 * newlen, 4)
            # If the offset is far enough that we can copy without resizing
            # while maintaining proportional spacing on both ends of the array
            # note that this branch prevents infinite growth when doing combinations
            # of push! and popfirst! (i.e. when using a Vector as a queue)
            newmem = mem
            newoffset = div(newlen, 8) + 1
        else
            # grow either by our computed overallocation factor
            # or exactly the requested size, whichever is larger
            # TODO we should possibly increase the offset if the current offset is nonzero.
            newmemlen2 = max(overallocation(memlen), newmemlen)
            newmem = array_new_memory(mem, newmemlen2)
            newoffset = offset
        end
        newref = @inbounds memoryref(newmem, newoffset)
        unsafe_copyto!(newref, ref, len)
        if ref !== a.ref
            @noinline throw(ConcurrencyViolationError("Vector can not be resized concurrently"))
        end
        setfield!(a, :ref, newref)
        end)()
    end
    return
end

function _growat!(a::Vector, i::Integer, delta::Integer)
    @_terminates_globally_noub_meta
    delta = Int(delta)
    i = Int(i)
    i == 1 && return _growbeg!(a, delta)
    len = length(a)
    i == len + 1 && return _growend!(a, delta)
    delta >= 0 || throw(ArgumentError("grow requires delta >= 0"))
    1 < i <= len || throw(BoundsError(a, i))
    ref = a.ref
    mem = ref.mem
    memlen = length(mem)
    newlen = len + delta
    offset = memoryrefoffset(ref)
    setfield!(a, :size, (newlen,))
    newmemlen = offset + newlen - 1

    # which side would we rather grow into?
    prefer_start = i <= div(len, 2)
    # if offset is far enough advanced to fit data in beginning of the memory
    if prefer_start && delta <= offset - 1
        newref = @inbounds memoryref(mem, offset - delta)
        unsafe_copyto!(newref, ref, i)
        setfield!(a, :ref, newref)
        for j in i:i+delta-1
            @inbounds _unsetindex!(a, j)
        end
    elseif !prefer_start && memlen >= newmemlen
        unsafe_copyto!(mem, offset - 1 + delta + i, mem, offset - 1 + i, len - i + 1)
        for j in i:i+delta-1
            @inbounds _unsetindex!(a, j)
        end
    else
        # since we will allocate the array in the middle of the memory we need at least 2*delta extra space
        # the +1 is because I didn't want to have an off by 1 error.
        newmemlen = max(overallocation(memlen), len+2*delta+1)
        newoffset = (newmemlen - newlen) ÷ 2 + 1
        newmem = array_new_memory(mem, newmemlen)
        newref = @inbounds memoryref(newmem, newoffset)
        unsafe_copyto!(newref, ref, i-1)
        unsafe_copyto!(newmem, newoffset + delta + i - 1, mem, offset + i - 1, len - i + 1)
        setfield!(a, :ref, newref)
    end
end

# efficiently delete part of an array
function _deletebeg!(a::Vector, delta::Integer)
    delta = Int(delta)
    len = length(a)
    0 <= delta <= len || throw(ArgumentError("_deletebeg! requires delta in 0:length(a)"))
    for i in 1:delta
        @inbounds _unsetindex!(a, i)
    end
    newlen = len - delta
    if newlen != 0 # if newlen==0 we could accidentally index past the memory
        newref = @inbounds memoryref(a.ref, delta + 1)
        setfield!(a, :ref, newref)
    end
    setfield!(a, :size, (newlen,))
    return
end
function _deleteend!(a::Vector, delta::Integer)
    delta = Int(delta)
    len = length(a)
    0 <= delta <= len || throw(ArgumentError("_deleteend! requires delta in 0:length(a)"))
    newlen = len - delta
    for i in newlen+1:len
        @inbounds _unsetindex!(a, i)
    end
    setfield!(a, :size, (newlen,))
    return
end
function _deleteat!(a::Vector, i::Integer, delta::Integer)
    i = Int(i)
    len = length(a)
    0 <= delta || throw(ArgumentError("_deleteat! requires delta >= 0"))
    1 <= i <= len || throw(BoundsError(a, i))
    i + delta <= len + 1 || throw(BoundsError(a, i + delta - 1))
    newa = a
    if 2*i + delta <= len
        unsafe_copyto!(newa, 1 + delta, a, 1, i - 1)
        _deletebeg!(a, delta)
    else
        unsafe_copyto!(newa, i, a, i + delta, len + 1 - delta - i)
        _deleteend!(a, delta)
    end
    return
end
## Dequeue functionality ##

"""
    push!(collection, items...) -> collection

Insert one or more `items` in `collection`. If `collection` is an ordered container,
the items are inserted at the end (in the given order).

# Examples
```jldoctest
julia> push!([1, 2, 3], 4, 5, 6)
6-element Vector{Int64}:
 1
 2
 3
 4
 5
 6
```

If `collection` is ordered, use [`append!`](@ref) to add all the elements of another
collection to it. The result of the preceding example is equivalent to `append!([1, 2, 3], [4,
5, 6])`. For `AbstractSet` objects, [`union!`](@ref) can be used instead.

See [`sizehint!`](@ref) for notes about the performance model.

See also [`pushfirst!`](@ref).
"""
function push! end

function push!(a::Vector{T}, item) where T
    @inline
    # convert first so we don't grow the array if the assignment won't work
    # and also to avoid a dynamic dynamic dispatch in the common case that
    # `item` is poorly-typed and `a` is well-typed
    item = item isa T ? item : convert(T, item)::T
    return _push!(a, item)
end
function _push!(a::Vector{T}, item::T) where T
    _growend!(a, 1)
    @_safeindex a[length(a)] = item
    return a
end

# specialize and optimize the single argument case
function push!(a::Vector{Any}, @nospecialize x)
    _growend!(a, 1)
    @_safeindex a[length(a)] = x
    return a
end
function push!(a::Vector{Any}, @nospecialize x...)
    @_terminates_locally_meta
    na = length(a)
    nx = length(x)
    _growend!(a, nx)
    @_safeindex for i = 1:nx
        a[na+i] = x[i]
    end
    return a
end

"""
    append!(collection, collections...) -> collection.

For an ordered container `collection`, add the elements of each `collections`
to the end of it.

!!! compat "Julia 1.6"
    Specifying multiple collections to be appended requires at least Julia 1.6.

# Examples
```jldoctest
julia> append!([1], [2, 3])
3-element Vector{Int64}:
 1
 2
 3

julia> append!([1, 2, 3], [4, 5], [6])
6-element Vector{Int64}:
 1
 2
 3
 4
 5
 6
```

Use [`push!`](@ref) to add individual items to `collection` which are not already
themselves in another collection. The result of the preceding example is equivalent to
`push!([1, 2, 3], 4, 5, 6)`.

See [`sizehint!`](@ref) for notes about the performance model.

See also [`vcat`](@ref) for vectors, [`union!`](@ref) for sets,
and [`prepend!`](@ref) and [`pushfirst!`](@ref) for the opposite order.
"""
function append! end

function append!(a::Vector{T}, items::Union{AbstractVector{<:T},Tuple}) where T
    items isa Tuple && (items = map(x -> convert(T, x), items))
    n = length(items)
    _growend!(a, n)
    copyto!(a, length(a)-n+1, items, firstindex(items), n)
    return a
end

append!(a::AbstractVector, iter) = _append!(a, IteratorSize(iter), iter)
push!(a::AbstractVector, iter...) = append!(a, iter)
append!(a::AbstractVector, iter...) = (for v in iter; append!(a, v); end; return a)

function _append!(a::AbstractVector, ::Union{HasLength,HasShape}, iter)
    n = Int(length(iter))::Int
    i = lastindex(a)
    sizehint!(a, length(a) + n; shrink=false)
    for item in iter
        push!(a, item)
    end
    a
end
function _append!(a::AbstractVector, ::IteratorSize, iter)
    for item in iter
        push!(a, item)
    end
    a
end

"""
    prepend!(a::Vector, collections...) -> collection

Insert the elements of each `collections` to the beginning of `a`.

When `collections` specifies multiple collections, order is maintained:
elements of `collections[1]` will appear leftmost in `a`, and so on.

!!! compat "Julia 1.6"
    Specifying multiple collections to be prepended requires at least Julia 1.6.

# Examples
```jldoctest
julia> prepend!([3], [1, 2])
3-element Vector{Int64}:
 1
 2
 3

julia> prepend!([6], [1, 2], [3, 4, 5])
6-element Vector{Int64}:
 1
 2
 3
 4
 5
 6
```
"""
function prepend! end

function prepend!(a::Vector{T}, items::Union{AbstractVector{<:T},Tuple}) where T
    items isa Tuple && (items = map(x -> convert(T, x), items))
    n = length(items)
    _growbeg!(a, n)
    # in case of aliasing, the _growbeg might have shifted our data, so copy
    # just the last n elements instead of all of them from the first
    copyto!(a, 1, items, lastindex(items)-n+1, n)
    return a
end

prepend!(a::AbstractVector, iter) = _prepend!(a, IteratorSize(iter), iter)
pushfirst!(a::AbstractVector, iter...) = prepend!(a, iter)
prepend!(a::AbstractVector, iter...) = (for v = reverse(iter); prepend!(a, v); end; return a)

function _prepend!(a::Vector, ::Union{HasLength,HasShape}, iter)
    @_terminates_locally_meta
    require_one_based_indexing(a)
    n = Int(length(iter))::Int
    sizehint!(a, length(a) + n; first=true, shrink=false)
    n = 0
    for item in iter
        n += 1
        pushfirst!(a, item)
    end
    reverse!(a, 1, n)
    a
end
function _prepend!(a::Vector, ::IteratorSize, iter)
    n = 0
    for item in iter
        n += 1
        pushfirst!(a, item)
    end
    reverse!(a, 1, n)
    a
end

"""
    resize!(a::Vector, n::Integer) -> Vector

Resize `a` to contain `n` elements. If `n` is smaller than the current collection
length, the first `n` elements will be retained. If `n` is larger, the new elements are not
guaranteed to be initialized.

# Examples
```jldoctest
julia> resize!([6, 5, 4, 3, 2, 1], 3)
3-element Vector{Int64}:
 6
 5
 4

julia> a = resize!([6, 5, 4, 3, 2, 1], 8);

julia> length(a)
8

julia> a[1:6]
6-element Vector{Int64}:
 6
 5
 4
 3
 2
 1
```
"""
function resize!(a::Vector, nl::Integer)
    l = length(a)
    if nl > l
        _growend!(a, nl-l)
    elseif nl != l
        if nl < 0
            _throw_argerror("new length must be ≥ 0")
        end
        _deleteend!(a, l-nl)
    end
    return a
end

"""
    sizehint!(s, n; first::Bool=false, shrink::Bool=true) -> s

Suggest that collection `s` reserve capacity for at least `n` elements. That is, if
you expect that you're going to have to push a lot of values onto `s`, you can avoid
the cost of incremental reallocation by doing it once up front; this can improve
performance.

If `first` is `true`, then any additional space is reserved before the start of the collection.
This way, subsequent calls to `pushfirst!` (instead of `push!`) may become faster.
Supplying this keyword may result in an error if the collection is not ordered
or if `pushfirst!` is not supported for this collection.

If `shrink=true` (the default), the collection's capacity may be reduced if its current
capacity is greater than `n`.

See also [`resize!`](@ref).

# Notes on the performance model

For types that support `sizehint!`,

1. `push!` and `append!` methods generally may (but are not required to) preallocate extra
   storage. For types implemented in `Base`, they typically do, using a heuristic optimized for
   a general use case.

2. `sizehint!` may control this preallocation. Again, it typically does this for types in
   `Base`.

3. `empty!` is nearly costless (and O(1)) for types that support this kind of preallocation.

!!! compat "Julia 1.11"
    The `shrink` and `first` arguments were added in Julia 1.11.
"""
function sizehint! end

function sizehint!(a::Vector, sz::Integer; first::Bool=false, shrink::Bool=true)
    len = length(a)
    ref = a.ref
    mem = ref.mem
    memlen = length(mem)
    sz = max(Int(sz), len)
    inc = sz - len
    if sz <= memlen
        # if we don't save at least 1/8th memlen then its not worth it to shrink
        if !shrink || memlen - sz <= div(memlen, 8)
            return a
        end
        newmem = array_new_memory(mem, sz)
        if first
            newref = memoryref(newmem, inc + 1)
        else
            newref = memoryref(newmem)
        end
        unsafe_copyto!(newref, ref, len)
        setfield!(a, :ref, newref)
    elseif first
        _growbeg!(a, inc)
        newref = getfield(a, :ref)
        newref = memoryref(newref, inc + 1)
        setfield!(a, :size, (len,)) # undo the size change from _growbeg!
        setfield!(a, :ref, newref) # undo the offset change from _growbeg!
    else # last
        _growend!(a, inc)
        setfield!(a, :size, (len,)) # undo the size change from _growend!
    end
    a
end

# Fall-back implementation for non-shrinkable collections
# avoid defining this the normal way to avoid avoid infinite recursion
function Core.kwcall(kwargs::NamedTuple{names}, ::typeof(sizehint!), a, sz) where names
    get(kwargs, :first, false)::Bool
    get(kwargs, :shrink, true)::Bool
    isempty(diff_names(names, (:first, :shrink))) || kwerr(kwargs, sizehint!, a, sz)
    sizehint!(a, sz)
end

"""
    pop!(collection) -> item

Remove an item in `collection` and return it. If `collection` is an
ordered container, the last item is returned; for unordered containers,
an arbitrary element is returned.

See also: [`popfirst!`](@ref), [`popat!`](@ref), [`delete!`](@ref), [`deleteat!`](@ref), [`splice!`](@ref), and [`push!`](@ref).

# Examples
```jldoctest
julia> A=[1, 2, 3]
3-element Vector{Int64}:
 1
 2
 3

julia> pop!(A)
3

julia> A
2-element Vector{Int64}:
 1
 2

julia> S = Set([1, 2])
Set{Int64} with 2 elements:
  2
  1

julia> pop!(S)
2

julia> S
Set{Int64} with 1 element:
  1

julia> pop!(Dict(1=>2))
1 => 2
```
"""
function pop!(a::Vector)
    if isempty(a)
        _throw_argerror("array must be non-empty")
    end
    item = a[end]
    _deleteend!(a, 1)
    return item
end

"""
    popat!(a::Vector, i::Integer, [default])

Remove the item at the given `i` and return it. Subsequent items
are shifted to fill the resulting gap.
When `i` is not a valid index for `a`, return `default`, or throw an error if
`default` is not specified.

See also: [`pop!`](@ref), [`popfirst!`](@ref), [`deleteat!`](@ref), [`splice!`](@ref).

!!! compat "Julia 1.5"
    This function is available as of Julia 1.5.

# Examples
```jldoctest
julia> a = [4, 3, 2, 1]; popat!(a, 2)
3

julia> a
3-element Vector{Int64}:
 4
 2
 1

julia> popat!(a, 4, missing)
missing

julia> popat!(a, 4)
ERROR: BoundsError: attempt to access 3-element Vector{Int64} at index [4]
[...]
```
"""
function popat!(a::Vector, i::Integer)
    @_propagate_inbounds_meta
    x = a[i]
    _deleteat!(a, i, 1)
    x
end

function popat!(a::Vector, i::Integer, default)
    if 1 <= i <= length(a)
        x = @inbounds a[i]
        _deleteat!(a, i, 1)
        x
    else
        default
    end
end

"""
    pushfirst!(collection, items...) -> collection

Insert one or more `items` at the beginning of `collection`.

This function is called `unshift` in many other programming languages.

# Examples
```jldoctest
julia> pushfirst!([1, 2, 3, 4], 5, 6)
6-element Vector{Int64}:
 5
 6
 1
 2
 3
 4
```
"""
function pushfirst!(a::Vector{T}, item) where T
    @inline
    item = item isa T ? item : convert(T, item)::T
    return _pushfirst!(a, item)
end
function _pushfirst!(a::Vector{T}, item::T) where T
    _growbeg!(a, 1)
    @_safeindex a[1] = item
    return a
end

# specialize and optimize the single argument case
function pushfirst!(a::Vector{Any}, @nospecialize x)
    _growbeg!(a, 1)
    @_safeindex a[1] = x
    return a
end
function pushfirst!(a::Vector{Any}, @nospecialize x...)
    @_terminates_locally_meta
    na = length(a)
    nx = length(x)
    _growbeg!(a, nx)
    @_safeindex for i = 1:nx
        a[i] = x[i]
    end
    return a
end

"""
    popfirst!(collection) -> item

Remove the first `item` from `collection`.

This function is called `shift` in many other programming languages.

See also: [`pop!`](@ref), [`popat!`](@ref), [`delete!`](@ref).

# Examples
```jldoctest
julia> A = [1, 2, 3, 4, 5, 6]
6-element Vector{Int64}:
 1
 2
 3
 4
 5
 6

julia> popfirst!(A)
1

julia> A
5-element Vector{Int64}:
 2
 3
 4
 5
 6
```
"""
function popfirst!(a::Vector)
    if isempty(a)
        _throw_argerror("array must be non-empty")
    end
    item = a[1]
    _deletebeg!(a, 1)
    return item
end

"""
    insert!(a::Vector, index::Integer, item)

Insert an `item` into `a` at the given `index`. `index` is the index of `item` in
the resulting `a`.

See also: [`push!`](@ref), [`replace`](@ref), [`popat!`](@ref), [`splice!`](@ref).

# Examples
```jldoctest
julia> insert!(Any[1:6;], 3, "here")
7-element Vector{Any}:
 1
 2
  "here"
 3
 4
 5
 6
```
"""
function insert!(a::Array{T,1}, i::Integer, item) where T
    @_propagate_inbounds_meta
    item = item isa T ? item : convert(T, item)::T
    return _insert!(a, i, item)
end
function _insert!(a::Array{T,1}, i::Integer, item::T) where T
    @_noub_meta
    # Throw convert error before changing the shape of the array
    _growat!(a, i, 1)
    # :noub, because _growat! already did bound check
    @inbounds a[i] = item
    return a
end

"""
    deleteat!(a::Vector, i::Integer)

Remove the item at the given `i` and return the modified `a`. Subsequent items
are shifted to fill the resulting gap.

See also: [`keepat!`](@ref), [`delete!`](@ref), [`popat!`](@ref), [`splice!`](@ref).

# Examples
```jldoctest
julia> deleteat!([6, 5, 4, 3, 2, 1], 2)
5-element Vector{Int64}:
 6
 4
 3
 2
 1
```
"""
function deleteat!(a::Vector, i::Integer)
    i isa Bool && depwarn("passing Bool as an index is deprecated", :deleteat!)
    _deleteat!(a, i, 1)
    return a
end

function deleteat!(a::Vector, r::AbstractUnitRange{<:Integer})
    if eltype(r) === Bool
        return invoke(deleteat!, Tuple{Vector, AbstractVector{Bool}}, a, r)
    else
        n = length(a)
        f = first(r)
        f isa Bool && depwarn("passing Bool as an index is deprecated", :deleteat!)
        isempty(r) || _deleteat!(a, f, length(r))
        return a
    end
end

"""
    deleteat!(a::Vector, inds)

Remove the items at the indices given by `inds`, and return the modified `a`.
Subsequent items are shifted to fill the resulting gap.

`inds` can be either an iterator or a collection of sorted and unique integer indices,
or a boolean vector of the same length as `a` with `true` indicating entries to delete.

# Examples
```jldoctest
julia> deleteat!([6, 5, 4, 3, 2, 1], 1:2:5)
3-element Vector{Int64}:
 5
 3
 1

julia> deleteat!([6, 5, 4, 3, 2, 1], [true, false, true, false, true, false])
3-element Vector{Int64}:
 5
 3
 1

julia> deleteat!([6, 5, 4, 3, 2, 1], (2, 2))
ERROR: ArgumentError: indices must be unique and sorted
Stacktrace:
[...]
```
"""
deleteat!(a::Vector, inds) = _deleteat!(a, inds)
deleteat!(a::Vector, inds::AbstractVector) = _deleteat!(a, to_indices(a, (inds,))[1])

struct Nowhere; end
push!(::Nowhere, _) = nothing
_growend!(::Nowhere, _) = nothing

function _push_deleted!(dltd, a::Vector, ind)
    @_propagate_inbounds_meta
    if isassigned(a, ind)
        push!(dltd, a[ind])
    else
        _growend!(dltd, 1)
    end
end

function _copy_item!(a::Vector, p, q)
    @_propagate_inbounds_meta
    if isassigned(a, q)
        a[p] = a[q]
    else
        _unsetindex!(a, p)
    end
end

function _deleteat!(a::Vector, inds, dltd=Nowhere())
    n = length(a)
    y = iterate(inds)
    y === nothing && return a
    (p, s) = y
    checkbounds(a, p)
    @inbounds _push_deleted!(dltd, a, p)
    q = p+1
    while true
        y = iterate(inds, s)
        y === nothing && break
        (i,s) = y
        if !(q <= i <= n)
            if i < q
                _throw_argerror("indices must be unique and sorted")
            else
                throw(BoundsError())
            end
        end
        while q < i
            @inbounds _copy_item!(a, p, q)
            p += 1; q += 1
        end
        @inbounds _push_deleted!(dltd, a, i)
        q = i+1
    end
    while q <= n
        @inbounds _copy_item!(a, p, q)
        p += 1; q += 1
    end
    _deleteend!(a, n-p+1)
    return a
end

# Simpler and more efficient version for logical indexing
function deleteat!(a::Vector, inds::AbstractVector{Bool})
    n = length(a)
    length(inds) == n || throw(BoundsError(a, inds))
    p = 1
    for (q, i) in enumerate(inds)
        @inbounds _copy_item!(a, p, q)
        p += !i
    end
    _deleteend!(a, n-p+1)
    return a
end

const _default_splice = []

"""
    splice!(a::Vector, index::Integer, [replacement]) -> item

Remove the item at the given index, and return the removed item.
Subsequent items are shifted left to fill the resulting gap.
If specified, replacement values from an ordered
collection will be spliced in place of the removed item.

See also: [`replace`](@ref), [`delete!`](@ref), [`deleteat!`](@ref), [`pop!`](@ref), [`popat!`](@ref).

# Examples
```jldoctest
julia> A = [6, 5, 4, 3, 2, 1]; splice!(A, 5)
2

julia> A
5-element Vector{Int64}:
 6
 5
 4
 3
 1

julia> splice!(A, 5, -1)
1

julia> A
5-element Vector{Int64}:
  6
  5
  4
  3
 -1

julia> splice!(A, 1, [-1, -2, -3])
6

julia> A
7-element Vector{Int64}:
 -1
 -2
 -3
  5
  4
  3
 -1
```

To insert `replacement` before an index `n` without removing any items, use
`splice!(collection, n:n-1, replacement)`.
"""
function splice!(a::Vector, i::Integer, ins=_default_splice)
    v = a[i]
    m = length(ins)
    if m == 0
        _deleteat!(a, i, 1)
    elseif m == 1
        a[i] = only(ins)
    else
        _growat!(a, i, m-1)
        k = 1
        for x in ins
            a[i+k-1] = x
            k += 1
        end
    end
    return v
end

"""
    splice!(a::Vector, indices, [replacement]) -> items

Remove items at specified indices, and return a collection containing
the removed items.
Subsequent items are shifted left to fill the resulting gaps.
If specified, replacement values from an ordered collection will be spliced in
place of the removed items; in this case, `indices` must be a `AbstractUnitRange`.

To insert `replacement` before an index `n` without removing any items, use
`splice!(collection, n:n-1, replacement)`.

$(_DOCS_ALIASING_WARNING)

!!! compat "Julia 1.5"
    Prior to Julia 1.5, `indices` must always be a `UnitRange`.

!!! compat "Julia 1.8"
    Prior to Julia 1.8, `indices` must be a `UnitRange` if splicing in replacement values.

# Examples
```jldoctest
julia> A = [-1, -2, -3, 5, 4, 3, -1]; splice!(A, 4:3, 2)
Int64[]

julia> A
8-element Vector{Int64}:
 -1
 -2
 -3
  2
  5
  4
  3
 -1
```
"""
function splice!(a::Vector, r::AbstractUnitRange{<:Integer}, ins=_default_splice)
    v = a[r]
    m = length(ins)
    if m == 0
        deleteat!(a, r)
        return v
    end

    n = length(a)
    f = first(r)
    l = last(r)
    d = length(r)

    if m < d
        delta = d - m
        _deleteat!(a, (f - 1 < n - l) ? f : (l - delta + 1), delta)
    elseif m > d
        _growat!(a, (f - 1 < n - l) ? f : (l + 1), m - d)
    end

    k = 1
    for x in ins
        a[f+k-1] = x
        k += 1
    end
    return v
end

splice!(a::Vector, inds) = (dltds = eltype(a)[]; _deleteat!(a, inds, dltds); dltds)

function empty!(a::Vector)
    _deleteend!(a, length(a))
    return a
end

# use memcmp for cmp on byte arrays
function cmp(a::Array{UInt8,1}, b::Array{UInt8,1})
    aref = a.ref
    bref = b.ref
    ta = @_gc_preserve_begin aref
    tb = @_gc_preserve_begin bref
    pa = unsafe_convert(Ptr{Cvoid}, aref)
    pb = unsafe_convert(Ptr{Cvoid}, bref)
    c = memcmp(pa, pb, min(length(a),length(b)))
    @_gc_preserve_end ta
    @_gc_preserve_end tb
    return c < 0 ? -1 : c > 0 ? +1 : cmp(length(a),length(b))
end

const BitIntegerArray{N} = Union{map(T->Array{T,N}, BitInteger_types)...} where N
# use memcmp for == on bit integer types
function ==(a::Arr, b::Arr) where {Arr <: BitIntegerArray}
    if size(a) == size(b)
        aref = a.ref
        bref = b.ref
        ta = @_gc_preserve_begin aref
        tb = @_gc_preserve_begin bref
        pa = unsafe_convert(Ptr{Cvoid}, aref)
        pb = unsafe_convert(Ptr{Cvoid}, bref)
        c = memcmp(pa, pb, sizeof(eltype(Arr)) * length(a))
        @_gc_preserve_end ta
        @_gc_preserve_end tb
        return c == 0
    else
        return false
    end
end

function ==(a::Arr, b::Arr) where Arr <: BitIntegerArray{1}
    len = length(a)
    if len == length(b)
        aref = a.ref
        bref = b.ref
        ta = @_gc_preserve_begin aref
        tb = @_gc_preserve_begin bref
        T = eltype(Arr)
        pa = unsafe_convert(Ptr{T}, aref)
        pb = unsafe_convert(Ptr{T}, bref)
        c = memcmp(pa, pb, sizeof(T) * len)
        @_gc_preserve_end ta
        @_gc_preserve_end tb
        return c == 0
    else
        return false
    end
end

"""
    reverse(v [, start=firstindex(v) [, stop=lastindex(v) ]] )

Return a copy of `v` reversed from start to stop.  See also [`Iterators.reverse`](@ref)
for reverse-order iteration without making a copy, and in-place [`reverse!`](@ref).

# Examples
```jldoctest
julia> A = Vector(1:5)
5-element Vector{Int64}:
 1
 2
 3
 4
 5

julia> reverse(A)
5-element Vector{Int64}:
 5
 4
 3
 2
 1

julia> reverse(A, 1, 4)
5-element Vector{Int64}:
 4
 3
 2
 1
 5

julia> reverse(A, 3, 5)
5-element Vector{Int64}:
 1
 2
 5
 4
 3
```
"""
function reverse(A::AbstractVector, start::Integer, stop::Integer=lastindex(A))
    s, n = Int(start), Int(stop)
    B = similar(A)
    for i = firstindex(A):s-1
        B[i] = A[i]
    end
    for i = s:n
        B[i] = A[n+s-i]
    end
    for i = n+1:lastindex(A)
        B[i] = A[i]
    end
    return B
end

# 1d special cases of reverse(A; dims) and reverse!(A; dims):
for (f,_f) in ((:reverse,:_reverse), (:reverse!,:_reverse!))
    @eval begin
        $f(A::AbstractVector; dims=:) = $_f(A, dims)
        $_f(A::AbstractVector, ::Colon) = $f(A, firstindex(A), lastindex(A))
        $_f(A::AbstractVector, dim::Tuple{Integer}) = $_f(A, first(dim))
        function $_f(A::AbstractVector, dim::Integer)
            dim == 1 || _throw_argerror(LazyString("invalid dimension ", dim, " ≠ 1"))
            return $_f(A, :)
        end
    end
end

function reverseind(a::AbstractVector, i::Integer)
    li = LinearIndices(a)
    first(li) + last(li) - i
end

# This implementation of `midpoint` is performance-optimized but safe
# only if `lo <= hi`.
midpoint(lo::T, hi::T) where T<:Integer = lo + ((hi - lo) >>> 0x01)
midpoint(lo::Integer, hi::Integer) = midpoint(promote(lo, hi)...)

"""
    reverse!(v [, start=firstindex(v) [, stop=lastindex(v) ]]) -> v

In-place version of [`reverse`](@ref).

# Examples
```jldoctest
julia> A = Vector(1:5)
5-element Vector{Int64}:
 1
 2
 3
 4
 5

julia> reverse!(A);

julia> A
5-element Vector{Int64}:
 5
 4
 3
 2
 1
```
"""
function reverse!(v::AbstractVector, start::Integer, stop::Integer=lastindex(v))
    s, n = Int(start), Int(stop)
    if n > s # non-empty and non-trivial
        liv = LinearIndices(v)
        if !(first(liv) ≤ s ≤ last(liv))
            throw(BoundsError(v, s))
        elseif !(first(liv) ≤ n ≤ last(liv))
            throw(BoundsError(v, n))
        end
        r = n
        @inbounds for i in s:midpoint(s, n-1)
            v[i], v[r] = v[r], v[i]
            r -= 1
        end
    end
    return v
end

# concatenations of (in)homogeneous combinations of vectors, horizontal and vertical

vcat() = Vector{Any}()
hcat() = Vector{Any}()

function hcat(V::Vector{T}...) where T
    height = length(V[1])
    for j = 2:length(V)
        if length(V[j]) != height
            throw(DimensionMismatch("vectors must have same lengths"))
        end
    end
    return [ V[j][i]::T for i=1:length(V[1]), j=1:length(V) ]
end
hcat(A::Vector...) = cat(A...; dims=Val(2)) # more special than SparseArrays's hcat

function vcat(arrays::Vector{T}...) where T
    n = 0
    for a in arrays
        n += length(a)
    end
    arr = Vector{T}(undef, n)
    nd = 1
    for a in arrays
        na = length(a)
        @assert nd + na <= 1 + length(arr) # Concurrent modification of arrays?
        unsafe_copyto!(arr, nd, a, 1, na)
        nd += na
    end
    return arr
end
vcat(A::Vector...) = cat(A...; dims=Val(1)) # more special than SparseArrays's vcat

_cat(n::Integer, x::Integer...) = reshape([x...], (ntuple(Returns(1), n-1)..., length(x)))

## find ##

"""
    findnext(A, i)

Find the next index after or including `i` of a `true` element of `A`,
or `nothing` if not found.

Indices are of the same type as those returned by [`keys(A)`](@ref)
and [`pairs(A)`](@ref).

# Examples
```jldoctest
julia> A = [false, false, true, false]
4-element Vector{Bool}:
 0
 0
 1
 0

julia> findnext(A, 1)
3

julia> findnext(A, 4) # returns nothing, but not printed in the REPL

julia> A = [false false; true false]
2×2 Matrix{Bool}:
 0  0
 1  0

julia> findnext(A, CartesianIndex(1, 1))
CartesianIndex(2, 1)
```
"""
findnext(A, start) = findnext(identity, A, start)

"""
    findfirst(A)

Return the index or key of the first `true` value in `A`.
Return `nothing` if no such value is found.
To search for other kinds of values, pass a predicate as the first argument.

Indices or keys are of the same type as those returned by [`keys(A)`](@ref)
and [`pairs(A)`](@ref).

See also: [`findall`](@ref), [`findnext`](@ref), [`findlast`](@ref), [`searchsortedfirst`](@ref).

# Examples
```jldoctest
julia> A = [false, false, true, false]
4-element Vector{Bool}:
 0
 0
 1
 0

julia> findfirst(A)
3

julia> findfirst(falses(3)) # returns nothing, but not printed in the REPL

julia> A = [false false; true false]
2×2 Matrix{Bool}:
 0  0
 1  0

julia> findfirst(A)
CartesianIndex(2, 1)
```
"""
findfirst(A) = findfirst(identity, A)

# Needed for bootstrap, and allows defining only an optimized findnext method
findfirst(A::AbstractArray) = findnext(A, first(keys(A)))

"""
    findnext(predicate::Function, A, i)

Find the next index after or including `i` of an element of `A`
for which `predicate` returns `true`, or `nothing` if not found. This works for
Arrays, Strings, and most other collections that support [`getindex`](@ref),
[`keys(A)`](@ref), and [`nextind`](@ref).

Indices are of the same type as those returned by [`keys(A)`](@ref)
and [`pairs(A)`](@ref).

# Examples
```jldoctest
julia> A = [1, 4, 2, 2];

julia> findnext(isodd, A, 1)
1

julia> findnext(isodd, A, 2) # returns nothing, but not printed in the REPL

julia> A = [1 4; 2 2];

julia> findnext(isodd, A, CartesianIndex(1, 1))
CartesianIndex(1, 1)

julia> findnext(isspace, "a b c", 3)
4
```
"""
function findnext(testf::Function, A, start)
    i = oftype(first(keys(A)), start)
    l = last(keys(A))
    i > l && return nothing
    while true
        testf(A[i]) && return i
        i == l && break
        # nextind(A, l) can throw/overflow
        i = nextind(A, i)
    end
    return nothing
end

"""
    findfirst(predicate::Function, A)

Return the index or key of the first element of `A` for which `predicate` returns `true`.
Return `nothing` if there is no such element.

Indices or keys are of the same type as those returned by [`keys(A)`](@ref)
and [`pairs(A)`](@ref).

# Examples
```jldoctest
julia> A = [1, 4, 2, 2]
4-element Vector{Int64}:
 1
 4
 2
 2

julia> findfirst(iseven, A)
2

julia> findfirst(x -> x>10, A) # returns nothing, but not printed in the REPL

julia> findfirst(isequal(4), A)
2

julia> A = [1 4; 2 2]
2×2 Matrix{Int64}:
 1  4
 2  2

julia> findfirst(iseven, A)
CartesianIndex(2, 1)
```
"""
function findfirst(testf::Function, A)
    for (i, a) in pairs(A)
        testf(a) && return i
    end
    return nothing
end

# Needed for bootstrap, and allows defining only an optimized findnext method
findfirst(testf::Function, A::Union{AbstractArray, AbstractString}) =
    findnext(testf, A, first(keys(A)))

findfirst(p::Union{Fix2{typeof(isequal),T},Fix2{typeof(==),T}}, r::OneTo) where {T<:Integer} =
    1 <= p.x <= r.stop ? convert(keytype(r), p.x) : nothing

findfirst(::typeof(iszero), ::OneTo) = nothing
findfirst(::typeof(isone), r::OneTo) = isempty(r) ? nothing : oneunit(keytype(r))

function findfirst(p::Union{Fix2{typeof(isequal),T},Fix2{typeof(==),T}}, r::AbstractUnitRange{<:Integer}) where {T<:Integer}
    first(r) <= p.x <= last(r) || return nothing
    i1 = first(keys(r))
    return i1 + oftype(i1, p.x - first(r))
end

function findfirst(p::Union{Fix2{typeof(isequal),T},Fix2{typeof(==),T}}, r::StepRange{T,S}) where {T,S}
    isempty(r) && return nothing
    minimum(r) <= p.x <= maximum(r) || return nothing
    d = p.x - first(r)
    iszero(d % step(r)) || return nothing
    return convert(keytype(r), d ÷ step(r) + 1)
end

findfirst(::typeof(iszero), r::AbstractRange) = findfirst(==(zero(first(r))), r)
findfirst(::typeof(isone), r::AbstractRange) = findfirst(==(one(first(r))), r)

"""
    findprev(A, i)

Find the previous index before or including `i` of a `true` element of `A`,
or `nothing` if not found.

Indices are of the same type as those returned by [`keys(A)`](@ref)
and [`pairs(A)`](@ref).

See also: [`findnext`](@ref), [`findfirst`](@ref), [`findall`](@ref).

# Examples
```jldoctest
julia> A = [false, false, true, true]
4-element Vector{Bool}:
 0
 0
 1
 1

julia> findprev(A, 3)
3

julia> findprev(A, 1) # returns nothing, but not printed in the REPL

julia> A = [false false; true true]
2×2 Matrix{Bool}:
 0  0
 1  1

julia> findprev(A, CartesianIndex(2, 1))
CartesianIndex(2, 1)
```
"""
findprev(A, start) = findprev(identity, A, start)

"""
    findlast(A)

Return the index or key of the last `true` value in `A`.
Return `nothing` if there is no `true` value in `A`.

Indices or keys are of the same type as those returned by [`keys(A)`](@ref)
and [`pairs(A)`](@ref).

See also: [`findfirst`](@ref), [`findprev`](@ref), [`findall`](@ref).

# Examples
```jldoctest
julia> A = [true, false, true, false]
4-element Vector{Bool}:
 1
 0
 1
 0

julia> findlast(A)
3

julia> A = falses(2,2);

julia> findlast(A) # returns nothing, but not printed in the REPL

julia> A = [true false; true false]
2×2 Matrix{Bool}:
 1  0
 1  0

julia> findlast(A)
CartesianIndex(2, 1)
```
"""
findlast(A) = findlast(identity, A)

# Needed for bootstrap, and allows defining only an optimized findprev method
findlast(A::AbstractArray) = findprev(A, last(keys(A)))

"""
    findprev(predicate::Function, A, i)

Find the previous index before or including `i` of an element of `A`
for which `predicate` returns `true`, or `nothing` if not found. This works for
Arrays, Strings, and most other collections that support [`getindex`](@ref),
[`keys(A)`](@ref), and [`nextind`](@ref).

Indices are of the same type as those returned by [`keys(A)`](@ref)
and [`pairs(A)`](@ref).

# Examples
```jldoctest
julia> A = [4, 6, 1, 2]
4-element Vector{Int64}:
 4
 6
 1
 2

julia> findprev(isodd, A, 1) # returns nothing, but not printed in the REPL

julia> findprev(isodd, A, 3)
3

julia> A = [4 6; 1 2]
2×2 Matrix{Int64}:
 4  6
 1  2

julia> findprev(isodd, A, CartesianIndex(1, 2))
CartesianIndex(2, 1)

julia> findprev(isspace, "a b c", 3)
2
```
"""
function findprev(testf::Function, A, start)
    f = first(keys(A))
    i = oftype(f, start)
    i < f && return nothing
    while true
        testf(A[i]) && return i
        i == f && break
        # prevind(A, f) can throw/underflow
        i = prevind(A, i)
    end
    return nothing
end

"""
    findlast(predicate::Function, A)

Return the index or key of the last element of `A` for which `predicate` returns `true`.
Return `nothing` if there is no such element.

Indices or keys are of the same type as those returned by [`keys(A)`](@ref)
and [`pairs(A)`](@ref).

# Examples
```jldoctest
julia> A = [1, 2, 3, 4]
4-element Vector{Int64}:
 1
 2
 3
 4

julia> findlast(isodd, A)
3

julia> findlast(x -> x > 5, A) # returns nothing, but not printed in the REPL

julia> A = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> findlast(isodd, A)
CartesianIndex(2, 1)
```
"""
function findlast(testf::Function, A)
    for (i, a) in Iterators.reverse(pairs(A))
        testf(a) && return i
    end
    return nothing
end

# Needed for bootstrap, and allows defining only an optimized findprev method
findlast(testf::Function, A::Union{AbstractArray, AbstractString}) =
    findprev(testf, A, last(keys(A)))

# for monotonic ranges, there is a unique index corresponding to a value, so findfirst and findlast are identical
function findlast(p::Union{Fix2{typeof(isequal),<:Integer},Fix2{typeof(==),<:Integer},typeof(iszero),typeof(isone)},
        r::AbstractUnitRange{<:Integer})
    findfirst(p, r)
end

function findlast(p::Union{Fix2{typeof(isequal),T},Fix2{typeof(==),T},typeof(iszero),typeof(isone)},
        r::StepRange{T,S}) where {T,S}
    findfirst(p, r)
end

"""
    findall(f::Function, A)

Return a vector `I` of the indices or keys of `A` where `f(A[I])` returns `true`.
If there are no such elements of `A`, return an empty array.

Indices or keys are of the same type as those returned by [`keys(A)`](@ref)
and [`pairs(A)`](@ref).

# Examples
```jldoctest
julia> x = [1, 3, 4]
3-element Vector{Int64}:
 1
 3
 4

julia> findall(isodd, x)
2-element Vector{Int64}:
 1
 2

julia> A = [1 2 0; 3 4 0]
2×3 Matrix{Int64}:
 1  2  0
 3  4  0
julia> findall(isodd, A)
2-element Vector{CartesianIndex{2}}:
 CartesianIndex(1, 1)
 CartesianIndex(2, 1)

julia> findall(!iszero, A)
4-element Vector{CartesianIndex{2}}:
 CartesianIndex(1, 1)
 CartesianIndex(2, 1)
 CartesianIndex(1, 2)
 CartesianIndex(2, 2)

julia> d = Dict(:A => 10, :B => -1, :C => 0)
Dict{Symbol, Int64} with 3 entries:
  :A => 10
  :B => -1
  :C => 0

julia> findall(≥(0), d)
2-element Vector{Symbol}:
 :A
 :C

```
"""
function findall(testf::Function, A)
    gen = (first(p) for p in pairs(A) if testf(last(p)))
    @default_eltype(gen) === Union{} ? collect(@default_eltype(keys(A)), gen) : collect(gen)
end

# Broadcasting is much faster for small testf, and computing
# integer indices from logical index using findall has a negligible cost
findall(testf::F, A::AbstractArray) where {F<:Function} = findall(testf.(A))

"""
    findall(A)

Return a vector `I` of the `true` indices or keys of `A`.
If there are no such elements of `A`, return an empty array.
To search for other kinds of values, pass a predicate as the first argument.

Indices or keys are of the same type as those returned by [`keys(A)`](@ref)
and [`pairs(A)`](@ref).

See also: [`findfirst`](@ref), [`searchsorted`](@ref).

# Examples
```jldoctest
julia> A = [true, false, false, true]
4-element Vector{Bool}:
 1
 0
 0
 1

julia> findall(A)
2-element Vector{Int64}:
 1
 4

julia> A = [true false; false true]
2×2 Matrix{Bool}:
 1  0
 0  1

julia> findall(A)
2-element Vector{CartesianIndex{2}}:
 CartesianIndex(1, 1)
 CartesianIndex(2, 2)

julia> findall(falses(3))
Int64[]
```
"""
function findall(A)
    collect(first(p) for p in pairs(A) if last(p))
end

# Allocating result upfront is faster (possible only when collection can be iterated twice)
function findall(A::AbstractArray{Bool})
    n = count(A)
    I = Vector{eltype(keys(A))}(undef, n)
    cnt = 1
    for (i,a) in pairs(A)
        if a
            I[cnt] = i
            cnt += 1
        end
    end
    I
end

findall(x::Bool) = x ? [1] : Vector{Int}()
findall(testf::Function, x::Number) = testf(x) ? [1] : Vector{Int}()
findall(p::Fix2{typeof(in)}, x::Number) = x in p.x ? [1] : Vector{Int}()

# similar to Matlab's ismember
"""
    indexin(a, b)

Return an array containing the first index in `b` for
each value in `a` that is a member of `b`. The output
array contains `nothing` wherever `a` is not a member of `b`.

See also: [`sortperm`](@ref), [`findfirst`](@ref).

# Examples
```jldoctest
julia> a = ['a', 'b', 'c', 'b', 'd', 'a'];

julia> b = ['a', 'b', 'c'];

julia> indexin(a, b)
6-element Vector{Union{Nothing, Int64}}:
 1
 2
 3
 2
  nothing
 1

julia> indexin(b, a)
3-element Vector{Union{Nothing, Int64}}:
 1
 2
 3
```
"""
function indexin(a, b::AbstractArray)
    inds = keys(b)
    bdict = Dict{eltype(b),eltype(inds)}()
    for (val, ind) in zip(b, inds)
        get!(bdict, val, ind)
    end
    return Union{eltype(inds), Nothing}[
        get(bdict, i, nothing) for i in a
    ]
end

function _findin(a::Union{AbstractArray, Tuple}, b::AbstractSet)
    ind  = Vector{eltype(keys(a))}()
    @inbounds for (i,ai) in pairs(a)
        ai in b && push!(ind, i)
    end
    ind
end
_findin(a::Union{AbstractArray, Tuple}, b) = _findin(a, Set(b))

# If two collections are already sorted, _findin can be computed with
# a single traversal of the two collections. This is much faster than
# using a hash table (although it has the same complexity).
function _sortedfindin(v::Union{AbstractArray, Tuple}, w)
    viter, witer = keys(v), eachindex(w)
    out  = eltype(viter)[]
    vy, wy = iterate(viter), iterate(witer)
    if vy === nothing || wy === nothing
        return out
    end
    viteri, i = vy
    witerj, j = wy
    @inbounds begin
        vi, wj = v[viteri], w[witerj]
        while true
            if isless(vi, wj)
                vy = iterate(viter, i)
                if vy === nothing
                    break
                end
                viteri, i = vy
                vi        = v[viteri]
            elseif isless(wj, vi)
                wy = iterate(witer, j)
                if wy === nothing
                    break
                end
                witerj, j = wy
                wj        = w[witerj]
            else
                push!(out, viteri)
                vy = iterate(viter, i)
                if vy === nothing
                    break
                end
                # We only increment the v iterator because v can have
                # repeated matches to a single value in w
                viteri, i = vy
                vi        = v[viteri]
            end
        end
    end
    return out
end

function findall(pred::Fix2{typeof(in),<:Union{Array{<:Real},Real}}, x::Array{<:Real})
    if issorted(x, Sort.Forward) && issorted(pred.x, Sort.Forward)
        return _sortedfindin(x, pred.x)
    else
        return _findin(x, pred.x)
    end
end
# issorted fails for some element types so the method above has to be restricted
# to element with isless/< defined.
findall(pred::Fix2{typeof(in)}, x::AbstractArray) = _findin(x, pred.x)
findall(pred::Fix2{typeof(in)}, x::Tuple) = _findin(x, pred.x)

# Copying subregions
function indcopy(sz::Dims, I::Vector)
    n = length(I)
    s = sz[n]
    for i = n+1:length(sz)
        s *= sz[i]
    end
    dst = eltype(I)[_findin(I[i], i < n ? (1:sz[i]) : (1:s)) for i = 1:n]
    src = eltype(I)[I[i][_findin(I[i], i < n ? (1:sz[i]) : (1:s))] for i = 1:n]
    dst, src
end

function indcopy(sz::Dims, I::Tuple{Vararg{RangeIndex}})
    n = length(I)
    s = sz[n]
    for i = n+1:length(sz)
        s *= sz[i]
    end
    dst::typeof(I) = ntuple(i-> _findin(I[i], i < n ? (1:sz[i]) : (1:s)), n)::typeof(I)
    src::typeof(I) = ntuple(i-> I[i][_findin(I[i], i < n ? (1:sz[i]) : (1:s))], n)::typeof(I)
    dst, src
end

## Filter ##

"""
    filter(f, a)

Return a copy of collection `a`, removing elements for which `f` is `false`.
The function `f` is passed one argument.

!!! compat "Julia 1.4"
    Support for `a` as a tuple requires at least Julia 1.4.

See also: [`filter!`](@ref), [`Iterators.filter`](@ref).

# Examples
```jldoctest
julia> a = 1:10
1:10

julia> filter(isodd, a)
5-element Vector{Int64}:
 1
 3
 5
 7
 9
```
"""
function filter(f, a::Array{T, N}) where {T, N}
    j = 1
    b = Vector{T}(undef, length(a))
    for ai in a
        @inbounds b[j] = ai
        j = ifelse(f(ai)::Bool, j+1, j)
    end
    resize!(b, j-1)
    sizehint!(b, length(b))
    b
end

function filter(f, a::AbstractArray)
    (IndexStyle(a) != IndexLinear()) && return a[map(f, a)::AbstractArray{Bool}]

    j = 1
    idxs = Vector{Int}(undef, length(a))
    for idx in eachindex(a)
        @inbounds idxs[j] = idx
        ai = @inbounds a[idx]
        j = ifelse(f(ai)::Bool, j+1, j)
    end
    resize!(idxs, j-1)
    res = a[idxs]
    empty!(idxs)
    sizehint!(idxs, 0)
    return res
end

"""
    filter!(f, a)

Update collection `a`, removing elements for which `f` is `false`.
The function `f` is passed one argument.

# Examples
```jldoctest
julia> filter!(isodd, Vector(1:10))
5-element Vector{Int64}:
 1
 3
 5
 7
 9
```
"""
function filter!(f, a::AbstractVector)
    j = firstindex(a)
    for ai in a
        @inbounds a[j] = ai
        j = ifelse(f(ai)::Bool, nextind(a, j), j)
    end
    j > lastindex(a) && return a
    if a isa Vector
        resize!(a, j-1)
        sizehint!(a, j-1)
    else
        deleteat!(a, j:lastindex(a))
    end
    return a
end

"""
    filter(f)

Create a function that filters its arguments with function `f` using [`filter`](@ref), i.e.
a function equivalent to `x -> filter(f, x)`.

The returned function is of type `Base.Fix1{typeof(filter)}`, which can be
used to implement specialized methods.

# Examples
```jldoctest
julia> (1, 2, Inf, 4, NaN, 6) |> filter(isfinite)
(1, 2, 4, 6)

julia> map(filter(iseven), [1:3, 2:4, 3:5])
3-element Vector{Vector{Int64}}:
 [2]
 [2, 4]
 [4]
```
!!! compat "Julia 1.9"
    This method requires at least Julia 1.9.
"""
function filter(f)
    Fix1(filter, f)
end

"""
    keepat!(a::Vector, inds)
    keepat!(a::BitVector, inds)

Remove the items at all the indices which are not given by `inds`,
and return the modified `a`.
Items which are kept are shifted to fill the resulting gaps.

$(_DOCS_ALIASING_WARNING)

`inds` must be an iterator of sorted and unique integer indices.
See also [`deleteat!`](@ref).

!!! compat "Julia 1.7"
    This function is available as of Julia 1.7.

# Examples
```jldoctest
julia> keepat!([6, 5, 4, 3, 2, 1], 1:2:5)
3-element Vector{Int64}:
 6
 4
 2
```
"""
keepat!(a::Vector, inds) = _keepat!(a, inds)

"""
    keepat!(a::Vector, m::AbstractVector{Bool})
    keepat!(a::BitVector, m::AbstractVector{Bool})

The in-place version of logical indexing `a = a[m]`. That is, `keepat!(a, m)` on
vectors of equal length `a` and `m` will remove all elements from `a` for which
`m` at the corresponding index is `false`.

# Examples
```jldoctest
julia> a = [:a, :b, :c];

julia> keepat!(a, [true, false, true])
2-element Vector{Symbol}:
 :a
 :c

julia> a
2-element Vector{Symbol}:
 :a
 :c
```
"""
keepat!(a::Vector, m::AbstractVector{Bool}) = _keepat!(a, m)

# set-like operators for vectors
# These are moderately efficient, preserve order, and remove dupes.

_unique_filter!(pred::P, update!::U, state) where {P,U} = function (x)
    # P, U force specialization
    if pred(x, state)
        update!(state, x)
        true
    else
        false
    end
end

_grow_filter!(seen) = _unique_filter!(∉, push!, seen)
_shrink_filter!(keep) = _unique_filter!(∈, pop!, keep)

function _grow!(pred!, v::AbstractVector, itrs)
    filter!(pred!, v) # uniquify v
    for itr in itrs
        mapfilter(pred!, push!, itr, v)
    end
    return v
end

union!(v::AbstractVector{T}, itrs...) where {T} =
    _grow!(_grow_filter!(sizehint!(Set{T}(), length(v))), v, itrs)

symdiff!(v::AbstractVector{T}, itrs...) where {T} =
    _grow!(_shrink_filter!(symdiff!(Set{T}(), v, itrs...)), v, itrs)

function _shrink!(shrinker!::F, v::AbstractVector, itrs) where F
    seen = Set{eltype(v)}()
    filter!(_grow_filter!(seen), v)
    shrinker!(seen, itrs...)
    filter!(in(seen), v)
end

intersect!(v::AbstractVector, itrs...) = _shrink!(intersect!, v, itrs)
setdiff!(  v::AbstractVector, itrs...) = _shrink!(setdiff!, v, itrs)

vectorfilter(T::Type, f, v) = T[x for x in v if f(x)]

function _shrink(shrinker!::F, itr, itrs) where F
    T = promote_eltype(itr, itrs...)
    keep = shrinker!(Set{T}(itr), itrs...)
    vectorfilter(T, _shrink_filter!(keep), itr)
end

intersect(itr, itrs...) = _shrink(intersect!, itr, itrs)
setdiff(  itr, itrs...) = _shrink(setdiff!, itr, itrs)

function intersect(v::AbstractVector, r::AbstractRange)
    T = promote_eltype(v, r)
    common = Iterators.filter(in(r), v)
    seen = Set{T}(common)
    return vectorfilter(T, _shrink_filter!(seen), common)
end
intersect(r::AbstractRange, v::AbstractVector) = intersect(v, r)

# Here instead of range.jl for bootstrapping because `@propagate_inbounds` depends on Vectors.
@propagate_inbounds function getindex(v::AbstractRange, i::Integer)
    if i isa Bool # Not via dispatch to avoid ambiguities
        throw(ArgumentError("invalid index: $i of type Bool"))
    else
        _getindex(v, i)
    end
end

"""
    wrap(Array, m::Union{Memory{T}, MemoryRef{T}}, dims)

Create an array of size `dims` using `m` as the underlying memory. This can be thought of as a safe version
of [`unsafe_wrap`](@ref) utilizing `Memory` or `MemoryRef` instead of raw pointers.
"""
function wrap end

# validity checking for _wrap calls, separate from allocation of Array so that it can be more likely to inline into the caller
function _wrap(ref::MemoryRef{T}, dims::NTuple{N, Int}) where {T, N}
    mem = ref.mem
    mem_len = length(mem) + 1 - memoryrefoffset(ref)
    len = Core.checked_dims(dims...)
    @boundscheck mem_len >= len || invalid_wrap_err(mem_len, dims, len)
    return ref
end

@noinline invalid_wrap_err(len, dims, proddims) = throw(DimensionMismatch(LazyString(
    "Attempted to wrap a MemoryRef of length ", len, " with an Array of size dims=", dims,
    " which is invalid because prod(dims) = ", proddims, " > ", len,
    " so that the array would have more elements than the underlying memory can store.")))

@eval @propagate_inbounds function wrap(::Type{Array}, m::MemoryRef{T}, dims::NTuple{N, Integer}) where {T, N}
    dims = convert(Dims, dims)
    ref = _wrap(m, dims)
    $(Expr(:new, :(Array{T, N}), :ref, :dims))
end

@eval @propagate_inbounds function wrap(::Type{Array}, m::Memory{T}, dims::NTuple{N, Integer}) where {T, N}
    dims = convert(Dims, dims)
    ref = _wrap(memoryref(m), dims)
    $(Expr(:new, :(Array{T, N}), :ref, :dims))
end
@eval @propagate_inbounds function wrap(::Type{Array}, m::MemoryRef{T}, l::Integer) where {T}
    dims = (Int(l),)
    ref = _wrap(m, dims)
    $(Expr(:new, :(Array{T, 1}), :ref, :dims))
end
@eval @propagate_inbounds function wrap(::Type{Array}, m::Memory{T}, l::Integer) where {T}
    dims = (Int(l),)
    ref = _wrap(memoryref(m), (l,))
    $(Expr(:new, :(Array{T, 1}), :ref, :dims))
end
@eval @propagate_inbounds function wrap(::Type{Array}, m::Memory{T}) where {T}
    ref = memoryref(m)
    dims = (length(m),)
    $(Expr(:new, :(Array{T, 1}), :ref, :dims))
end

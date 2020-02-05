# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Generator(f, iter)

Given a function `f` and an iterator `iter`, construct an iterator that yields
the values of `f` applied to the elements of `iter`.
The syntax for constructing an instance of this type is `f(x) for x in iter [if cond(x)::Bool] `.
The `[if cond(x)::Bool]` expression is optional and acts as a "guard", effectively
filtering out values where the condition is false.

```jldoctest
julia> g = (abs2(x) for x in 1:5 if x != 3);

julia> for x in g
           println(x)
       end
1
4
16
25

julia> collect(g)
4-element Array{Int64,1}:
  1
  4
 16
 25
```
"""
struct Generator{I,F}
    f::F
    iter::I
end

Generator(f, I1, I2, Is...) = Generator(a->f(a...), zip(I1, I2, Is...))

Generator(::Type{T}, iter::I) where {T,I} = Generator{I,Type{T}}(T, iter)

Generator(::Type{T}, I1, I2, Is...) where {T} = Generator(a->T(a...), zip(I1, I2, Is...))

function iterate(g::Generator, s...)
    @_inline_meta
    y = iterate(g.iter, s...)
    y === nothing && return nothing
    y = y::Tuple{Any, Any} # try to give inference some idea of what to expect about the behavior of the next line
    return (g.f(y[1]), y[2])
end

length(g::Generator) = length(g.iter)
size(g::Generator) = size(g.iter)
axes(g::Generator) = axes(g.iter)
ndims(g::Generator) = ndims(g.iter)
keys(g::Generator) = keys(g.iter)


## iterator traits

abstract type IteratorSize end
struct SizeUnknown <: IteratorSize end
struct HasLength <: IteratorSize end
struct HasShape{N} <: IteratorSize end
struct IsInfinite <: IteratorSize end

"""
    IteratorSize(itertype::Type) -> IteratorSize

Given the type of an iterator, return one of the following values:

* `SizeUnknown()` if the length (number of elements) cannot be determined in advance.
* `HasLength()` if there is a fixed, finite length.
* `HasShape{N}()` if there is a known length plus a notion of multidimensional shape (as for an array).
   In this case `N` should give the number of dimensions, and the [`axes`](@ref) function is valid
   for the iterator.
* `IsInfinite()` if the iterator yields values forever.

The default value (for iterators that do not define this function) is `HasLength()`.
This means that most iterators are assumed to implement [`length`](@ref).

This trait is generally used to select between algorithms that pre-allocate space for their
result, and algorithms that resize their result incrementally.

```jldoctest
julia> Base.IteratorSize(1:5)
Base.HasShape{1}()

julia> Base.IteratorSize((2,3))
Base.HasLength()
```
"""
IteratorSize(x) = IteratorSize(typeof(x))
IteratorSize(::Type) = HasLength()  # HasLength is the default

IteratorSize(::Type{<:AbstractArray{<:Any,N}})  where {N} = HasShape{N}()
IteratorSize(::Type{Generator{I,F}}) where {I,F} = IteratorSize(I)

IteratorSize(::Type{Any}) = SizeUnknown()

haslength(iter) = IteratorSize(iter) isa Union{HasShape, HasLength}

abstract type IteratorEltype end
struct EltypeUnknown <: IteratorEltype end
struct HasEltype <: IteratorEltype end

"""
    IteratorEltype(itertype::Type) -> IteratorEltype

Given the type of an iterator, return one of the following values:

* `EltypeUnknown()` if the type of elements yielded by the iterator is not known in advance.
* `HasEltype()` if the element type is known, and [`eltype`](@ref) would return a meaningful value.

`HasEltype()` is the default, since iterators are assumed to implement [`eltype`](@ref).

This trait is generally used to select between algorithms that pre-allocate a specific
type of result, and algorithms that pick a result type based on the types of yielded
values.

```jldoctest
julia> Base.IteratorEltype(1:5)
Base.HasEltype()
```
"""
IteratorEltype(x) = IteratorEltype(typeof(x))
IteratorEltype(::Type) = HasEltype()  # HasEltype is the default

IteratorEltype(::Type{Generator{I,T}}) where {I,T} = EltypeUnknown()

IteratorEltype(::Type{Any}) = EltypeUnknown()

# This file is a part of Julia. License is MIT: http://julialang.org/license

"""
    Generator(f, iter)

Given a function `f` and an iterator `iter`, construct an iterator that yields
the values of `f` applied to the elements of `iter`.
The syntax `f(x) [if cond(x)::Bool] for x in iter` is syntax for constructing an instance of this
type. The `[if cond(x)::Bool]` expression is optional and acts as a "guard", effectively
filtering out values where the condition is false.
"""
immutable Generator{I,F}
    f::F
    iter::I
end

Generator(f, I1, I2, Is...) = Generator(a->f(a...), zip(I1, I2, Is...))

Generator{T,I}(::Type{T}, iter::I) = Generator{I,Type{T}}(T, iter)

start(g::Generator) = (@_inline_meta; start(g.iter))
done(g::Generator, s) = (@_inline_meta; done(g.iter, s))
function next(g::Generator, s)
    @_inline_meta
    v, s2 = next(g.iter, s)
    g.f(v), s2
end


## iterator traits

abstract IteratorSize
immutable SizeUnknown <: IteratorSize end
immutable HasLength <: IteratorSize end
immutable HasShape <: IteratorSize end
immutable IsInfinite <: IteratorSize end

"""
    iteratorsize(itertype::Type) -> IteratorSize

Given the type of an iterator, returns one of the following values:

* `SizeUnknown()` if the length (number of elements) cannot be determined in advance.
* `HasLength()` if there is a fixed, finite length.
* `HasShape()` if there is a known length plus a notion of multidimensional shape (as for an array).
  In this case the `size` function is valid for the iterator.
* `IsInfinite()` if the iterator yields values forever.

The default value (for iterators that do not define this function) is `HasLength()`.
This means that most iterators are assumed to implement `length`.

This trait is generally used to select between algorithms that pre-allocate space for their
result, and algorithms that resize their result incrementally.
"""
iteratorsize(x) = iteratorsize(typeof(x))
iteratorsize(::Type) = HasLength()  # HasLength is the default

abstract IteratorEltype
immutable EltypeUnknown <: IteratorEltype end
immutable HasEltype <: IteratorEltype end

"""
    iteratoreltype(itertype::Type) -> IteratorEltype

Given the type of an iterator, returns one of the following values:

* `EltypeUnknown()` if the type of elements yielded by the iterator is not known in advance.
* `HasEltype()` if the element type is known, and `eltype` would return a meaningful value.

`HasEltype()` is the default, since iterators are assumed to implement `eltype`.

This trait is generally used to select between algorithms that pre-allocate a specific
type of result, and algorithms that pick a result type based on the types of yielded
values.
"""
iteratoreltype(x) = iteratoreltype(typeof(x))
iteratoreltype(::Type) = HasEltype()  # HasEltype is the default

iteratorsize{T<:AbstractArray}(::Type{T}) = HasShape()
iteratorsize{I,F}(::Type{Generator{I,F}}) = iteratorsize(I)
length(g::Generator) = length(g.iter)
size(g::Generator) = size(g.iter)
indices(g::Generator) = indices(g.iter)
ndims(g::Generator) = ndims(g.iter)

iteratoreltype{I,T}(::Type{Generator{I,T}}) = EltypeUnknown()

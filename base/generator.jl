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

iteratorsize(x) = iteratorsize(typeof(x))
iteratorsize(::Type) = HasLength()  # HasLength is the default

and_iteratorsize{T}(isz::T, ::T) = isz
and_iteratorsize(::HasLength, ::HasShape) = HasLength()
and_iteratorsize(::HasShape, ::HasLength) = HasLength()
and_iteratorsize(a, b) = SizeUnknown()

abstract IteratorEltype
immutable EltypeUnknown <: IteratorEltype end
immutable HasEltype <: IteratorEltype end

iteratoreltype(x) = iteratoreltype(typeof(x))
iteratoreltype(::Type) = HasEltype()  # HasEltype is the default

and_iteratoreltype{T}(iel::T, ::T) = iel
and_iteratoreltype(a, b) = EltypeUnknown()

iteratorsize{T<:AbstractArray}(::Type{T}) = HasShape()
iteratorsize{I,F}(::Type{Generator{I,F}}) = iteratorsize(I)
length(g::Generator) = length(g.iter)
size(g::Generator) = size(g.iter)
ndims(g::Generator) = ndims(g.iter)

iteratoreltype{I,T}(::Type{Generator{I,T}}) = EltypeUnknown()

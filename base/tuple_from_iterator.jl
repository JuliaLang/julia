# This file is a part of Julia. License is MIT: https://julialang.org/license

module TupleFromIterator

import ..TypeIntersectExact as TIE

incremented(n::Int) = Core.Intrinsics.add_int(n, 1)
decremented(n::Int) = Core.Intrinsics.sub_int(n, 1)

function _totuple_err(@nospecialize T)
    @noinline
    throw(ArgumentError("too few or too many elements for tuple type $T"))
end

function iterator_to_ntuple_recur(::Type{Tuple{}}, iter, state::Union{Tuple{},Tuple{Any}})
    i = iterate(iter, state...)::Union{Nothing,Tuple{Any,Any}}
    isnothing(i) ||
        throw(ArgumentError("iterator has too many elements for the tuple type"))
    ()
end
function iterator_to_ntuple_recur(
    ::Type{Tuple{E,Vararg{E,lenm1}}}, iter, state::Union{Tuple{},Tuple{Any}}
) where {E,lenm1}
    T2 = Tuple{E,Any}
    ItUn = Union{Nothing,T2}
    i = iterate(iter, state...)::ItUn
    T = Tuple{E,Vararg{E,lenm1}}
    isnothing(i) &&
        throw(ArgumentError("iterator has too few elements for the tuple type"))
    (e, s) = i::T2
    Next = NTuple{lenm1,E}
    t = iterator_to_ntuple_recur(Next, iter, (s,))::Next
    (e, t...)::T
end

function iterator_to_ntuple(::Type{T}, iter) where {len,T<:NTuple{len,Any}}
    if len < 100
        iterator_to_ntuple_recur(T, iter, ())::T
    else
        # prevent stack overflow during type inference
        let
            f(i) = (collect(i)...,)
            f(i::Tuple) = i
            f(i::Union{NamedTuple,Core.SimpleVector,Array,Pair}) = (i...,)
            f(i::Array{<:Any,0}) = (first(i),)
            f(i::Union{AbstractArray{<:Any,0},Number,Ref}) = (first(i),)
            f(i::Pair) = (first(i), last(i))
            f(iter)::T
        end
    end::T
end

function iterator_to_tuple(::Type{R}, iter) where {R<:Tuple}
    f(i) = collect(i)
    f(i::Union{Tuple,NamedTuple,Core.SimpleVector,Array,AbstractArray{<:Any,0},Number,Ref,Pair}) = i

    c = f(iter)
    len = length(c)::Int
    E = eltype(c)::Type
    T = NTuple{len,E}
    r = iterator_to_ntuple(T, c)::Tuple{Vararg{E}}::T
    (r isa R) || _totuple_err(R)
    r::R
end

"""
  ntuple_any(::Type{<:NTuple{len,Any}}) where {len}

Like `ntuple(Returns(Any), Val(len))`.
"""
ntuple_any(::Type{Tuple{}}) = ()
function ntuple_any(::Type{<:Tuple{Any,Vararg{Any,lenm1}}}) where {lenm1}
    T = NTuple{lenm1,DataType}
    t = ntuple_any(T)::T
    (Any, t...)::Tuple{DataType,Vararg{DataType,lenm1}}
end

"""
  tuple_va_type_length(::Val)

Creates a `Vararg` `<:Tuple` type of specified minimal length.
"""
function tuple_va_type_length(::Val{len}) where {len}
    Tuple{ntuple_any(NTuple{len,DataType})...,Vararg}
end
tuple_va_type_length(n::Int) = tuple_va_type_length(Val(n))::Type{<:Tuple}

tuple_length_type_va(::Type{T}, ::Val{n}, ::Type{S}) where {T<:Tuple,n,S<:Tuple} = Val(decremented(n))
function tuple_length_type_va(::Type{T}, ::Val{n}, ::Type{S}) where {n,S<:Tuple,T<:S}
    v = Val(incremented(n))
    tuple_length_type_va(T, v, tuple_va_type_length(v)::Type{<:Tuple})::Val
end

"""
  tuple_length_type(::Type{<:Tuple})

Strips the element type information from a tuple type while keeping
the information about the length.
"""
function tuple_length_type(::Type{T}) where {T<:Tuple}
    v = Val(1)
    r = tuple_length_type_va(T, v, tuple_va_type_length(v)::Type{<:Tuple})::Val
    tuple_va_type_length(r)::Type{<:Tuple}
end
tuple_length_type(::Type{T}) where {len,T<:NTuple{len,Any}} = NTuple{len,Any}

"""
  fieldtype_typeintersect_ntuple(::Type{T}, ::Type{<:NTuple{len,Any}}, ::Val{ind}) where {T<:Tuple, len, ind}

Returns the type of the field `ind` in the type intersection of `T` with
`NTuple{len,Any}`. Failing to compute the exact intersection, the field `ind` of
`T` is returned.
"""
function fieldtype_typeintersect_ntuple(
    (@nospecialize T::Type{<:Tuple}), ::Type{<:NTuple{len,Any}}, ::Val{ind}
) where {len,ind}
    Core.@_foldable_meta
    (1 ≤ (ind::Int) ≤ len) || throw(ArgumentError("`ind` out of bounds"))
    S = NTuple{len,Any}
    ST = typeintersect(S, T)::Type
    TS = typeintersect(T, S)::Type
    X = fieldtype(T,  ind)::Type
    Y = fieldtype(ST, ind)::Type
    Z = fieldtype(TS, ind)::Type
    type_intrs = TIE.type_intersect_exact(X, Y, Z)
    TIE.get_result(X, type_intrs)::Type
end

function tuple_converted_elem(::Type{T}, t::NTuple{len,Any}, ::Val{ind}) where {T<:Tuple,len,ind}
    (1 ≤ (ind::Int) ≤ len) || throw(ArgumentError("`ind` out of bounds"))
    S = fieldtype_typeintersect_ntuple(T, NTuple{len,Any}, Val(ind))::Type
    e = t[ind]
    ((e isa S) ? e : convert(S, e))::S
end

function tuple_with_converted_elems_recur(::Type{T}, ::NTuple{len,Any}, r::NTuple{len,Any}) where {T<:Tuple,len}
    r::T
end
function tuple_with_converted_elems_recur(::Type{T}, t::NTuple{len,Any}, r::NTuple{n,Any}) where {T<:Tuple,len,n}
    (n < len) || throw(ArgumentError("`n` out of bounds"))
    m = incremented(n)
    s = (r..., tuple_converted_elem(T, t, Val(m)))::NTuple{m,Any}
    tuple_with_converted_elems_recur(T, t, s)::NTuple{len,Any}
end

function tuple_with_converted_elems(::Type{T}, t::NTuple{len,Any}) where {T<:Tuple,len}
    NT = NTuple{len,Any}
    type_intrs = TIE.type_intersect_exact(NT, T)
    S = TIE.get_result(T, type_intrs)::Type{<:Tuple}
    (S <: Union{}) && _totuple_err(T)
    tuple_with_converted_elems_recur(S, t, ())::T::NT::S
end

function iterator_to_tuple_with_element_types(::Type{T}, iter) where {T<:Tuple}
    R = tuple_length_type(T)::Type{<:Tuple}
    t = iterator_to_tuple(R, iter)::R
    tuple_with_converted_elems(T, t)::T
end

# As an optimization, special case some tuple types with no constraints on the
# types of the elements.
iterator_to_tuple_with_element_types(T::Type{NTuple{len,Any}}, i) where {len} = iterator_to_tuple(T, i)::T
iterator_to_tuple_with_element_types(T::Type{tuple_va_type_length(0)}, i) = iterator_to_tuple(T, i)::T
iterator_to_tuple_with_element_types(T::Type{tuple_va_type_length(1)}, i) = iterator_to_tuple(T, i)::T
iterator_to_tuple_with_element_types(T::Type{tuple_va_type_length(2)}, i) = iterator_to_tuple(T, i)::T
iterator_to_tuple_with_element_types(T::Type{tuple_va_type_length(3)}, i) = iterator_to_tuple(T, i)::T
iterator_to_tuple_with_element_types(T::Type{tuple_va_type_length(4)}, i) = iterator_to_tuple(T, i)::T

end

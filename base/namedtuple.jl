# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    NamedTuple

`NamedTuple`s are, as their name suggests, named [`Tuple`](@ref)s. That is, they're a
tuple-like collection of values, where each entry has a unique name, represented as a
[`Symbol`](@ref). Like `Tuple`s, `NamedTuple`s are immutable; neither the names nor the values
can be modified in place after construction.

A named tuple can be created as a tuple literal with keys, e.g. `(a=1, b=2)`,
or as a tuple literal with semicolon after the opening parenthesis, e.g. `(;
a=1, b=2)` (this form also accepts programmatically generated names as
described below), or using a `NamedTuple` type as constructor, e.g.
`NamedTuple{(:a, :b)}((1,2))`.

Accessing the value associated with a name in a named tuple can be done using field
access syntax, e.g. `x.a`, or using [`getindex`](@ref), e.g. `x[:a]` or `x[(:a, :b)]`.
A tuple of the names can be obtained using [`keys`](@ref), and a tuple of the values
can be obtained using [`values`](@ref).

!!! note
    Iteration over `NamedTuple`s produces the *values* without the names. (See example
    below.) To iterate over the name-value pairs, use the [`pairs`](@ref) function.

The [`@NamedTuple`](@ref) macro can be used for conveniently declaring `NamedTuple` types.

# Examples
```jldoctest
julia> x = (a=1, b=2)
(a = 1, b = 2)

julia> x.a
1

julia> x[:a]
1

julia> x[(:a,)]
(a = 1,)

julia> keys(x)
(:a, :b)

julia> values(x)
(1, 2)

julia> collect(x)
2-element Vector{Int64}:
 1
 2

julia> collect(pairs(x))
2-element Vector{Pair{Symbol, Int64}}:
 :a => 1
 :b => 2
```

In a similar fashion as to how one can define keyword arguments programmatically,
a named tuple can be created by giving pairs `name::Symbol => value` after a
semicolon inside a tuple literal. This and the `name=value` syntax can be mixed:

```jldoctest
julia> (; :a => 1, :b => 2, c=3)
(a = 1, b = 2, c = 3)
```

The name-value pairs can also be provided by splatting a named tuple or any
iterator that yields two-value collections holding each a symbol as first
value:

julia> keys = (:a, :b, :c); values = (1, 2, 3);

julia> NamedTuple{keys}(values)
(a = 1, b = 2, c = 3)

julia> (; (keys .=> values)...)
(a = 1, b = 2, c = 3)

julia> nt1 = (a=1, b=2);

julia> nt2 = (c=3, d=4);

julia> (; nt1..., nt2..., b=20) # the final b overwrites the value from nt1
(a = 1, b = 20, c = 3, d = 4)

julia> (; zip(keys, values)...) # zip yields tuples such as (:a, 1)
(a = 1, b = 2, c = 3)
```

As in keyword arguments, identifiers and dot expressions imply names:

```jldoctest
julia> x = 0
0

julia> t = (; x)
(x = 0,)

julia> (; t.x)
(x = 0,)
```

!!! compat "Julia 1.5"
    Implicit names from identifiers and dot expressions are available as of Julia 1.5.

!!! compat "Julia 1.7"
    Use of `getindex` methods with multiple `Symbol`s is available as of Julia 1.7.
"""
Core.NamedTuple

if nameof(@__MODULE__) === :Base

@eval function NamedTuple{names,T}(args::Tuple) where {names, T <: Tuple}
    if length(args) != length(names::Tuple)
        throw(ArgumentError("Wrong number of arguments to named tuple constructor."))
    end
    # Note T(args) might not return something of type T; e.g.
    # Tuple{Type{Float64}}((Float64,)) returns a Tuple{DataType}
    $(Expr(:splatnew, :(NamedTuple{names,T}), :(T(args))))
end

function NamedTuple{names, T}(nt::NamedTuple) where {names, T <: Tuple}
    if @generated
        Expr(:new, :(NamedTuple{names, T}),
             Any[ :(convert(fieldtype(T, $n), getfield(nt, $(QuoteNode(names[n]))))) for n in 1:length(names) ]...)
    else
        NamedTuple{names, T}(map(Fix1(getfield, nt), names))
    end
end

# Like NamedTuple{names, T} as a constructor, but omits the additional
# `convert` call, when the types are known to match the fields
@eval function _new_NamedTuple(T::Type{NamedTuple{NTN, NTT}} where {NTN, NTT}, args::Tuple)
    $(Expr(:splatnew, :T, :args))
end

function NamedTuple{names}(nt::NamedTuple) where {names}
    if @generated
        idx = Int[ fieldindex(nt, names[n]) for n in 1:length(names) ]
        types = Tuple{(fieldtype(nt, idx[n]) for n in 1:length(idx))...}
        Expr(:new, :(NamedTuple{names, $types}), Any[ :(getfield(nt, $(idx[n]))) for n in 1:length(idx) ]...)
    else
        length_names = length(names::Tuple)
        types = Tuple{(fieldtype(typeof(nt), names[n]) for n in 1:length_names)...}
        _new_NamedTuple(NamedTuple{names, types}, map(Fix1(getfield, nt), names))
    end
end

NamedTuple{names, T}(itr) where {names, T <: Tuple} = NamedTuple{names, T}(T(itr))
NamedTuple{names}(itr) where {names} = NamedTuple{names}(Tuple(itr))

NamedTuple(itr) = (; itr...)

# avoids invalidating Union{}(...)
NamedTuple{names, Union{}}(itr::Tuple) where {names} = throw(MethodError(NamedTuple{names, Union{}}, (itr,)))

end # if Base

length(t::NamedTuple) = nfields(t)
iterate(t::NamedTuple, iter=1) = iter > nfields(t) ? nothing : (getfield(t, iter), iter + 1)
rest(t::NamedTuple) = t
@inline rest(t::NamedTuple{names}, i::Int) where {names} = NamedTuple{rest(names,i)}(t)
firstindex(t::NamedTuple) = 1
lastindex(t::NamedTuple) = nfields(t)
getindex(t::NamedTuple, i::Int) = getfield(t, i)
getindex(t::NamedTuple, i::Symbol) = getfield(t, i)
getindex(t::NamedTuple, ::Colon) = t
@inline getindex(t::NamedTuple, idxs::Tuple{Vararg{Symbol}}) = NamedTuple{idxs}(t)
@inline getindex(t::NamedTuple, idxs::AbstractVector{Symbol}) = NamedTuple{Tuple(idxs)}(t)
indexed_iterate(t::NamedTuple, i::Int, state=1) = (getfield(t, i), i+1)
isempty(::NamedTuple{()}) = true
isempty(::NamedTuple) = false
empty(::NamedTuple) = NamedTuple()

prevind(@nospecialize(t::NamedTuple), i::Integer) = Int(i)-1
nextind(@nospecialize(t::NamedTuple), i::Integer) = Int(i)+1

convert(::Type{NT}, nt::NT) where {names, NT<:NamedTuple{names}} = nt
convert(::Type{NT}, nt::NT) where {names, T<:Tuple, NT<:NamedTuple{names,T}} = nt

function convert(::Type{NT}, nt::NamedTuple{names}) where {names, T<:Tuple, NT<:NamedTuple{names,T}}
    if !@isdefined T
        # converting abstract NT to an abstract Tuple type, to a concrete NT1, is not straightforward, so this could just be an error, but we define it anyways
        # _tuple_error(NT, nt)
        T1 = Tuple{ntuple(i -> fieldtype(NT, i), Val(length(names)))...}
        NT1 = NamedTuple{names, T1}
    else
        T1 = T
        NT1 = NT
    end
    return NT1(T1(nt))::NT1::NT
end

if nameof(@__MODULE__) === :Base
    Tuple(nt::NamedTuple) = (nt...,)
    (::Type{T})(nt::NamedTuple) where {T <: Tuple} = convert(T, Tuple(nt))
end

function show(io::IO, t::NamedTuple)
    n = nfields(t)
    for i = 1:n
        # if field types aren't concrete, show full type
        if typeof(getfield(t, i)) !== fieldtype(typeof(t), i)
            show(io, typeof(t))
            print(io, "(")
            show(io, Tuple(t))
            print(io, ")")
            return
        end
    end
    if n == 0
        print(io, "NamedTuple()")
    else
        typeinfo = get(io, :typeinfo, Any)
        print(io, "(")
        for i = 1:n
            show_sym(io, fieldname(typeof(t), i))
            print(io, " = ")
            show(IOContext(io, :typeinfo =>
                           t isa typeinfo <: NamedTuple ? fieldtype(typeinfo, i) : Any),
                 getfield(t, i))
            if n == 1
                print(io, ",")
            elseif i < n
                print(io, ", ")
            end
        end
        print(io, ")")
    end
end

eltype(::Type{T}) where T<:NamedTuple = nteltype(T)
nteltype(::Type) = Any
nteltype(::Type{NamedTuple{names,T}} where names) where {T} = eltype(T)

keytype(@nospecialize nt::NamedTuple) = keytype(typeof(nt))
keytype(@nospecialize T::Type{<:NamedTuple}) = Symbol

valtype(@nospecialize nt::NamedTuple) = valtype(typeof(nt))
valtype(@nospecialize T::Type{<:NamedTuple}) = eltype(T)

==(a::NamedTuple{n}, b::NamedTuple{n}) where {n} = Tuple(a) == Tuple(b)
==(a::NamedTuple, b::NamedTuple) = false

isequal(a::NamedTuple{n}, b::NamedTuple{n}) where {n} = isequal(Tuple(a), Tuple(b))
isequal(a::NamedTuple, b::NamedTuple) = false

_nt_names(::NamedTuple{names}) where {names} = names
_nt_names(::Type{T}) where {names,T<:NamedTuple{names}} = names

hash(x::NamedTuple, h::UInt) = xor(objectid(_nt_names(x)), hash(Tuple(x), h))

(<)(a::NamedTuple{n}, b::NamedTuple{n}) where {n} = Tuple(a) < Tuple(b)
isless(a::NamedTuple{n}, b::NamedTuple{n}) where {n} = isless(Tuple(a), Tuple(b))

same_names(::NamedTuple{names}...) where {names} = true
same_names(::NamedTuple...) = false

# NOTE: this method signature makes sure we don't define map(f)
function map(f, nt::NamedTuple{names}, nts::NamedTuple...) where names
    if !same_names(nt, nts...)
        throw(ArgumentError("Named tuple names do not match."))
    end
    NamedTuple{names}(map(f, map(Tuple, (nt, nts...))...))
end

@assume_effects :total function merge_names(an::Tuple{Vararg{Symbol}}, bn::Tuple{Vararg{Symbol}})
    @nospecialize an bn
    names = Symbol[an...]
    for n in bn
        if !sym_in(n, an)
            push!(names, n)
        end
    end
    (names...,)
end

@assume_effects :total function merge_types(names::Tuple{Vararg{Symbol}}, a::Type{<:NamedTuple}, b::Type{<:NamedTuple})
    @nospecialize names a b
    bn = _nt_names(b)
    return Tuple{Any[ fieldtype(sym_in(names[n], bn) ? b : a, names[n]) for n in 1:length(names) ]...}
end

@assume_effects :foldable function merge_fallback(@nospecialize(a::NamedTuple), @nospecialize(b::NamedTuple),
        @nospecialize(an::Tuple{Vararg{Symbol}}), @nospecialize(bn::Tuple{Vararg{Symbol}}))
    names = merge_names(an, bn)
    types = merge_types(names, typeof(a), typeof(b))
    n = length(names)
    A = Vector{Any}(undef, n)
    for i=1:n
        n = names[i]
        A[i] = getfield(sym_in(n, bn) ? b : a, n)
    end
    _new_NamedTuple(NamedTuple{names, types}, (A...,))
end

"""
    merge(a::NamedTuple, bs::NamedTuple...)

Construct a new named tuple by merging two or more existing ones, in a left-associative
manner. Merging proceeds left-to-right, between pairs of named tuples, and so the order of fields
present in both the leftmost and rightmost named tuples take the same position as they are found in the
leftmost named tuple. However, values are taken from matching fields in the rightmost named tuple that
contains that field. Fields present in only the rightmost named tuple of a pair are appended at the end.
A fallback is implemented for when only a single named tuple is supplied,
with signature `merge(a::NamedTuple)`.

!!! compat "Julia 1.1"
    Merging 3 or more `NamedTuple` requires at least Julia 1.1.

# Examples
```jldoctest
julia> merge((a=1, b=2, c=3), (b=4, d=5))
(a = 1, b = 4, c = 3, d = 5)
```

```jldoctest
julia> merge((a=1, b=2), (b=3, c=(d=1,)), (c=(d=2,),))
(a = 1, b = 3, c = (d = 2,))
```
"""
function merge(a::NamedTuple{an}, b::NamedTuple{bn}) where {an, bn}
    if @generated
        names = merge_names(an, bn)
        types = merge_types(names, a, b)
        vals = Any[ :(getfield($(sym_in(names[n], bn) ? :b : :a), $(QuoteNode(names[n])))) for n in 1:length(names) ]
        :( _new_NamedTuple(NamedTuple{$names,$types}, ($(vals...),)) )
    else
        merge_fallback(a, b, an, bn)
    end
end

merge(a::NamedTuple,     b::NamedTuple{()}) = a
merge(a::NamedTuple{()}, b::NamedTuple{()}) = a
merge(a::NamedTuple{()}, b::NamedTuple)     = b

merge(a::NamedTuple, b::Iterators.Pairs{<:Any,<:Any,<:Any,<:NamedTuple}) = merge(a, getfield(b, :data))

merge(a::NamedTuple, b::Iterators.Zip{<:Tuple{Any,Any}}) = merge(a, NamedTuple{Tuple(b.is[1])}(b.is[2]))

merge(a::NamedTuple, b::NamedTuple, cs::NamedTuple...) = merge(merge(a, b), cs...)

merge(a::NamedTuple) = a

"""
    merge(a::NamedTuple, iterable)

Interpret an iterable of key-value pairs as a named tuple, and perform a merge.

```jldoctest
julia> merge((a=1, b=2, c=3), [:b=>4, :d=>5])
(a = 1, b = 4, c = 3, d = 5)
```
"""
function merge(a::NamedTuple, itr)
    names = Symbol[]
    vals = Any[]
    inds = IdDict{Symbol,Int}()
    for (k, v) in itr
        k = k::Symbol
        oldind = get(inds, k, 0)
        if oldind > 0
            vals[oldind] = v
        else
            push!(names, k)
            push!(vals, v)
            inds[k] = length(names)
        end
    end
    merge(a, NamedTuple{(names...,)}((vals...,)))
end

keys(nt::NamedTuple{names}) where {names} = names::Tuple{Vararg{Symbol}}
values(nt::NamedTuple) = Tuple(nt)
haskey(nt::NamedTuple, key::Union{Integer, Symbol}) = isdefined(nt, key)
get(nt::NamedTuple, key::Union{Integer, Symbol}, default) = isdefined(nt, key) ? getfield(nt, key) : default
get(f::Callable, nt::NamedTuple, key::Union{Integer, Symbol}) = isdefined(nt, key) ? getfield(nt, key) : f()
tail(t::NamedTuple{names}) where names = NamedTuple{tail(names::Tuple)}(t)
front(t::NamedTuple{names}) where names = NamedTuple{front(names::Tuple)}(t)
reverse(nt::NamedTuple) = NamedTuple{reverse(keys(nt))}(reverse(values(nt)))

@assume_effects :total function diff_names(an::Tuple{Vararg{Symbol}}, bn::Tuple{Vararg{Symbol}})
    @nospecialize an bn
    names = Symbol[]
    for n in an
        if !sym_in(n, bn)
            push!(names, n)
        end
    end
    (names...,)
end

@assume_effects :foldable function diff_types(@nospecialize(a::NamedTuple), @nospecialize(names::Tuple{Vararg{Symbol}}))
    return Tuple{Any[ fieldtype(typeof(a), names[n]) for n in 1:length(names) ]...}
end

@assume_effects :foldable function diff_fallback(@nospecialize(a::NamedTuple), @nospecialize(an::Tuple{Vararg{Symbol}}), @nospecialize(bn::Tuple{Vararg{Symbol}}))
    names = diff_names(an, bn)
    isempty(names) && return (;)
    types = diff_types(a, names)
    n = length(names)
    A = Vector{Any}(undef, n)
    for i=1:n
        n = names[i]
        A[i] = getfield(a, n)
    end
    _new_NamedTuple(NamedTuple{names, types}, (A...,))
end

"""
    structdiff(a::NamedTuple, b::Union{NamedTuple,Type{NamedTuple}})

Construct a copy of named tuple `a`, except with fields that exist in `b` removed.
`b` can be a named tuple, or a type of the form `NamedTuple{field_names}`.
"""
function structdiff(a::NamedTuple{an}, b::Union{NamedTuple{bn}, Type{NamedTuple{bn}}}) where {an, bn}
    if @generated
        names = diff_names(an, bn)
        isempty(names) && return (;) # just a fast pass
        idx = Int[ fieldindex(a, names[n]) for n in 1:length(names) ]
        types = Tuple{Any[ fieldtype(a, idx[n]) for n in 1:length(idx) ]...}
        vals = Any[ :(getfield(a, $(idx[n]))) for n in 1:length(idx) ]
        return :( _new_NamedTuple(NamedTuple{$names,$types}, ($(vals...),)) )
    else
        return diff_fallback(a, an, bn)
    end
end

structdiff(a::NamedTuple{an}, b::Union{NamedTuple{an}, Type{NamedTuple{an}}}) where {an} = (;)

"""
    setindex(nt::NamedTuple, val, key::Symbol)

Constructs a new `NamedTuple` with the key `key` set to `val`.
If `key` is already in the keys of `nt`, `val` replaces the old value.

```jldoctest
julia> nt = (a = 3,)
(a = 3,)

julia> Base.setindex(nt, 33, :b)
(a = 3, b = 33)

julia> Base.setindex(nt, 4, :a)
(a = 4,)

julia> Base.setindex(nt, "a", :a)
(a = "a",)
```
"""
function setindex(nt::NamedTuple, v, idx::Symbol)
    merge(nt, (; idx => v))
end

"""
    @NamedTuple{key1::Type1, key2::Type2, ...}
    @NamedTuple begin key1::Type1; key2::Type2; ...; end

This macro gives a more convenient syntax for declaring `NamedTuple` types. It returns a `NamedTuple`
type with the given keys and types, equivalent to `NamedTuple{(:key1, :key2, ...), Tuple{Type1,Type2,...}}`.
If the `::Type` declaration is omitted, it is taken to be `Any`.   The `begin ... end` form allows the
declarations to be split across multiple lines (similar to a `struct` declaration), but is otherwise
equivalent. The `NamedTuple` macro is used when printing `NamedTuple` types to e.g. the REPL.

For example, the tuple `(a=3.1, b="hello")` has a type `NamedTuple{(:a, :b), Tuple{Float64, String}}`, which
can also be declared via `@NamedTuple` as:

```jldoctest
julia> @NamedTuple{a::Float64, b::String}
@NamedTuple{a::Float64, b::String}

julia> @NamedTuple begin
           a::Float64
           b::String
       end
@NamedTuple{a::Float64, b::String}
```

!!! compat "Julia 1.5"
    This macro is available as of Julia 1.5.
"""
macro NamedTuple(ex)
    Meta.isexpr(ex, :braces) || Meta.isexpr(ex, :block) ||
        throw(ArgumentError("@NamedTuple expects {...} or begin...end"))
    decls = filter(e -> !(e isa LineNumberNode), ex.args)
    all(e -> e isa Symbol || Meta.isexpr(e, :(::)), decls) ||
        throw(ArgumentError("@NamedTuple must contain a sequence of name or name::type expressions"))
    vars = [QuoteNode(e isa Symbol ? e : e.args[1]) for e in decls]
    types = [esc(e isa Symbol ? :Any : e.args[2]) for e in decls]
    return :(NamedTuple{($(vars...),), Tuple{$(types...)}})
end

@constprop :aggressive function split_rest(t::NamedTuple{names}, n::Int, st...) where {names}
    _check_length_split_rest(length(t), n)
    names_front, names_last_n = split_rest(names, n, st...)
    return NamedTuple{names_front}(t), NamedTuple{names_last_n}(t)
end

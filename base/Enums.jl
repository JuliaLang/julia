# This file is a part of Julia. License is MIT: https://julialang.org/license

module Enums

import Core.Intrinsics.bitcast
export Enum, @enum

function namemap end

"""
    Enum{T<:Integer}

The abstract supertype of all enumerated types defined with [`@enum`](@ref).
"""
abstract type Enum{T<:Integer} end

basetype(::Type{<:Enum{T}}) where {T<:Integer} = T

(::Type{T})(x::Enum{T2}) where {T<:Integer,T2<:Integer} = T(bitcast(T2, x))::T
Base.cconvert(::Type{T}, x::Enum{T2}) where {T<:Integer,T2<:Integer} = T(x)
Base.write(io::IO, x::Enum{T}) where {T<:Integer} = write(io, T(x))
Base.read(io::IO, ::Type{T}) where {T<:Enum} = T(read(io, basetype(T)))

Base.isless(x::T, y::T) where {T<:Enum} = isless(basetype(T)(x), basetype(T)(y))

Base.Symbol(x::Enum) = namemap(typeof(x))[Integer(x)]::Symbol

Base.print(io::IO, x::Enum) = print(io, Symbol(x))

function Base.show(io::IO, x::Enum)
    sym = Symbol(x)
    if !(get(io, :compact, false)::Bool)
        from = get(io, :module, Main)
        def = typeof(x).name.module
        if from === nothing || !Base.isvisible(sym, def, from)
            show(io, def)
            print(io, ".")
        end
    end
    print(io, sym)
end

function Base.show(io::IO, ::MIME"text/plain", x::Enum)
    print(io, x, "::")
    show(IOContext(io, :compact => true), typeof(x))
    print(io, " = ")
    show(io, Integer(x))
end

function Base.show(io::IO, ::MIME"text/plain", t::Type{<:Enum})
    print(io, "Enum ")
    Base.show_datatype(io, t)
    print(io, ":")
    for x in instances(t)
        print(io, "\n", Symbol(x), " = ")
        show(io, Integer(x))
    end
end

# generate code to test whether expr is in the given set of values
function membershiptest(expr, values)
    lo, hi = extrema(values)
    if length(values) == hi - lo + 1
        :($lo <= $expr <= $hi)
    elseif length(values) < 20
        foldl((x1,x2)->:($x1 || ($expr == $x2)), values[2:end]; init=:($expr == $(values[1])))
    else
        :($expr in $(Set(values)))
    end
end

# give Enum types scalar behavior in broadcasting
Base.broadcastable(x::Enum) = Ref(x)

@noinline enum_argument_error(typename, x) = throw(ArgumentError(string("invalid value for Enum $(typename): $x")))

"""
    @enum EnumName[::BaseType] value1[=x] value2[=y]

Create an `Enum{BaseType}` subtype with name `EnumName` and enum member values of
`value1` and `value2` with optional assigned values of `x` and `y`, respectively.
`EnumName` can be used just like other types and enum member values as regular values, such as

# Examples
```jldoctest fruitenum
julia> @enum Fruit apple=1 orange=2 kiwi=3

julia> f(x::Fruit) = "I'm a Fruit with value: \$(Int(x))"
f (generic function with 1 method)

julia> f(apple)
"I'm a Fruit with value: 1"

julia> Fruit(1)
apple::Fruit = 1
```

Values can also be specified inside a `begin` block, e.g.

```julia
@enum EnumName begin
    value1
    value2
end
```

`BaseType`, which defaults to [`Int32`](@ref), must be a primitive subtype of `Integer`.
Member values can be converted between the enum type and `BaseType`. `read` and `write`
perform these conversions automatically. In case, the default `BaseType` is not used,
the `Base.Enums.basetype(EnumName)` function returns the `BaseType` for `EnumName`.

To list all the instances of an enum use `instances`, e.g.

```jldoctest fruitenum
julia> instances(Fruit)
(apple, orange, kiwi)
```
"""
macro enum(T, syms...)
    if isempty(syms)
        throw(ArgumentError("no arguments given for Enum $T"))
    end
    basetype = Int32
    typename = T
    if isa(T, Expr) && T.head === :(::) && length(T.args) == 2 && isa(T.args[1], Symbol)
        typename = T.args[1]
        basetype = Core.eval(__module__, T.args[2])
        if !isa(basetype, DataType) || !(basetype <: Integer) || !isbitstype(basetype)
            throw(ArgumentError("invalid base type for Enum $typename, $T=::$basetype; base type must be an integer primitive type"))
        end
    elseif !isa(T, Symbol)
        throw(ArgumentError("invalid type expression for enum $T"))
    end
    values = basetype[]
    seen = Set{Symbol}()
    namemap = Dict{basetype,Symbol}()
    lo = hi = 0
    i = zero(basetype)
    hasexpr = false

    if length(syms) == 1 && syms[1] isa Expr && syms[1].head === :block
        syms = syms[1].args
    end
    for s in syms
        s isa LineNumberNode && continue
        if isa(s, Symbol)
            if i == typemin(basetype) && !isempty(values)
                throw(ArgumentError("overflow in value \"$s\" of Enum $typename"))
            end
        elseif isa(s, Expr) &&
               (s.head === :(=) || s.head === :kw) &&
               length(s.args) == 2 && isa(s.args[1], Symbol)
            i = Core.eval(__module__, s.args[2]) # allow exprs, e.g. uint128"1"
            if !isa(i, Integer)
                throw(ArgumentError("invalid value for Enum $typename, $s; values must be integers"))
            end
            i = convert(basetype, i)
            s = s.args[1]
            hasexpr = true
        else
            throw(ArgumentError(string("invalid argument for Enum ", typename, ": ", s)))
        end
        s = s::Symbol
        if !Base.isidentifier(s)
            throw(ArgumentError("invalid name for Enum $typename; \"$s\" is not a valid identifier"))
        end
        if hasexpr && haskey(namemap, i)
            throw(ArgumentError("both $s and $(namemap[i]) have value $i in Enum $typename; values must be unique"))
        end
        namemap[i] = s
        push!(values, i)
        if s in seen
            throw(ArgumentError("name \"$s\" in Enum $typename is not unique"))
        end
        push!(seen, s)
        if length(values) == 1
            lo = hi = i
        else
            lo = min(lo, i)
            hi = max(hi, i)
        end
        i += oneunit(i)
    end
    blk = quote
        # enum definition
        Base.@__doc__(primitive type $(esc(typename)) <: Enum{$(basetype)} $(sizeof(basetype) * 8) end)
        function $(esc(typename))(x::Integer)
            $(membershiptest(:x, values)) || enum_argument_error($(Expr(:quote, typename)), x)
            return bitcast($(esc(typename)), convert($(basetype), x))
        end
        Enums.namemap(::Type{$(esc(typename))}) = $(esc(namemap))
        Base.typemin(x::Type{$(esc(typename))}) = $(esc(typename))($lo)
        Base.typemax(x::Type{$(esc(typename))}) = $(esc(typename))($hi)
        let insts = (Any[ $(esc(typename))(v) for v in $values ]...,)
            Base.instances(::Type{$(esc(typename))}) = insts
        end
    end
    if isa(typename, Symbol)
        for (i, sym) in namemap
            push!(blk.args, :(const $(esc(sym)) = $(esc(typename))($i)))
        end
    end
    push!(blk.args, :nothing)
    blk.head = :toplevel
    return blk
end

end # module

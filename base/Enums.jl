# This file is a part of Julia. License is MIT: https://julialang.org/license

module Enums

import Core.Intrinsics.bitcast
export Enum, @enum

function basetype end

abstract type Enum{T<:Integer} end

(::Type{T})(x::Enum{T2}) where {T<:Integer,T2<:Integer} = T(bitcast(T2, x))::T
Base.cconvert(::Type{T}, x::Enum{T2}) where {T<:Integer,T2<:Integer} = T(x)
Base.write(io::IO, x::Enum{T}) where {T<:Integer} = write(io, T(x))
Base.read(io::IO, ::Type{T}) where {T<:Enum} = T(read(io, Enums.basetype(T)))

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
perform these conversions automatically.

To list all the instances of an enum use `instances`, e.g.

```jldoctest fruitenum
julia> instances(Fruit)
(apple::Fruit = 1, orange::Fruit = 2, kiwi::Fruit = 3)
```
"""
macro enum(T, syms...)
    if isempty(syms)
        throw(ArgumentError("no arguments given for Enum $T"))
    end
    basetype = Int32
    typename = T
    if isa(T, Expr) && T.head == :(::) && length(T.args) == 2 && isa(T.args[1], Symbol)
        typename = T.args[1]
        basetype = Core.eval(__module__, T.args[2])
        if !isa(basetype, DataType) || !(basetype <: Integer) || !isbitstype(basetype)
            throw(ArgumentError("invalid base type for Enum $typename, $T=::$basetype; base type must be an integer primitive type"))
        end
    elseif !isa(T, Symbol)
        throw(ArgumentError("invalid type expression for enum $T"))
    end
    vals = Vector{Tuple{Symbol,Integer}}()
    lo = hi = 0
    i = zero(basetype)
    hasexpr = false

    if length(syms) == 1 && syms[1] isa Expr && syms[1].head == :block
        syms = syms[1].args
    end
    for s in syms
        s isa LineNumberNode && continue
        if isa(s, Symbol)
            if i == typemin(basetype) && !isempty(vals)
                throw(ArgumentError("overflow in value \"$s\" of Enum $typename"))
            end
        elseif isa(s, Expr) &&
               (s.head == :(=) || s.head == :kw) &&
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
        if !Base.isidentifier(s)
            throw(ArgumentError("invalid name for Enum $typename; \"$s\" is not a valid identifier."))
        end
        push!(vals, (s,i))
        if length(vals) == 1
            lo = hi = i
        else
            lo = min(lo, i)
            hi = max(hi, i)
        end
        i += oneunit(i)
    end
    values = basetype[i[2] for i in vals]
    if hasexpr && values != unique(values)
        throw(ArgumentError("values for Enum $typename are not unique"))
    end
    blk = quote
        # enum definition
        Base.@__doc__(primitive type $(esc(typename)) <: Enum{$(basetype)} $(sizeof(basetype) * 8) end)
        function $(esc(typename))(x::Integer)
            $(membershiptest(:x, values)) || enum_argument_error($(Expr(:quote, typename)), x)
            return bitcast($(esc(typename)), convert($(basetype), x))
        end
        Enums.basetype(::Type{$(esc(typename))}) = $(esc(basetype))
        Base.typemin(x::Type{$(esc(typename))}) = $(esc(typename))($lo)
        Base.typemax(x::Type{$(esc(typename))}) = $(esc(typename))($hi)
        Base.isless(x::$(esc(typename)), y::$(esc(typename))) = isless($basetype(x), $basetype(y))
        let insts = ntuple(i->$(esc(typename))($values[i]), $(length(vals)))
            Base.instances(::Type{$(esc(typename))}) = insts
        end
        function Base.print(io::IO, x::$(esc(typename)))
            for (sym, i) in $vals
                if i == $(basetype)(x)
                    print(io, sym); break
                end
            end
        end
        function Base.show(io::IO, x::$(esc(typename)))
            if get(io, :compact, false)
                print(io, x)
            else
                print(io, x, "::")
                show(IOContext(io, :compact => true), typeof(x))
                print(io, " = ", $basetype(x))
            end
        end
        function Base.show(io::IO, t::Type{$(esc(typename))})
            Base.show_datatype(io, t)
        end
        function Base.show(io::IO, ::MIME"text/plain", t::Type{$(esc(typename))})
            print(io, "Enum ")
            Base.show_datatype(io, t)
            print(io, ":")
            for (sym, i) in $vals
                print(io, "\n", sym, " = ")
                show(io, i)
            end
        end
    end
    if isa(typename, Symbol)
        for (sym,i) in vals
            push!(blk.args, :(const $(esc(sym)) = $(esc(typename))($i)))
        end
    end
    push!(blk.args, :nothing)
    blk.head = :toplevel
    return blk
end

end # module

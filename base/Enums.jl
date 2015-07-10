# This file is a part of Julia. License is MIT: http://julialang.org/license

module Enums

using Base.Meta

export Enum, @enum

abstract Enum

Base.convert{T<:Integer}(::Type{T},x::Enum) = convert(T, x.val)

# generate code to test whether expr is in the given set of values
function membershiptest(expr, values)
    lo, hi = extrema(values)
    sv = sort(values)
    if sv == [lo:hi;]
        :($lo <= $expr <= $hi)
    elseif length(values) < 20
        foldl((x1,x2)->:($x1 || ($expr == $x2)), :($expr == $(values[1])), values[2:end])
    else
        :($expr in $(Set(values)))
    end
end

@noinline enum_argument_error(typename, x) = throw(ArgumentError(string("invalid value for Enum $(typename): $x")))

macro enum(T,syms...)
    isempty(syms) && throw(ArgumentError("no arguments given for Enum $T"))
    isa(T,Symbol) ? (typename = T) : throw(ArgumentError("invalid type expression for enum $T"))
    vals = Tuple{Symbol,Integer}[]
    lo = hi = 0
    i = -1
    enumT = typeof(i)
    hasexpr = false
    for s in syms
        isexpr(s, :kw) && (s.head = :(=))
        @match s begin
            s_Symbol          -> i == typemax(typeof(i)) ?
                                   (i = widen(i) + one(i)) :
                                   (i += one(i))
            (ss_Symbol = ii_) -> (s, i, hasexpr) = (ss, eval(ii), true)
            _                 -> throw(ArgumentError(string("invalid argument for Enum ", typename, ": ", s)))
        end
        isa(i, Integer) ||
            throw(ArgumentError("invalid value for Enum $typename, $s=$i; values must be integers"))
        Base.isidentifier(s) ||
            throw(ArgumentError("invalid name for Enum $typename; \"$s\" is not a valid identifier."))
        push!(vals, (s,i))
        I = typeof(i)
        if length(vals) == 1
            enumT = I
            lo = hi = i
        else
            enumT = promote_type(enumT,I)
            lo = min(lo, i)
            hi = max(hi, i)
        end
    end
    if !hasexpr
        n = length(vals)
        enumT = n <= typemax(Int8) ? Int8 :
                n <= typemax(Int16) ? Int16 :
                n <= typemax(Int32) ? Int32 : Int64
    end
    values = enumT[i[2] for i in vals]
    if hasexpr && values != unique(values)
        throw(ArgumentError("values for Enum $typename are not unique"))
    end
    lo = convert(enumT, lo)
    hi = convert(enumT, hi)
    vals = map(x->(x[1],convert(enumT,x[2])), vals)
    blk = quote
        # enum definition
        immutable $(esc(T)) <: Enum
            val::$enumT
            function Base.convert(::Type{$(esc(typename))}, x::Integer)
                $(membershiptest(:x, values)) || enum_argument_error($(Expr(:quote, typename)), x)
                new(x)
            end
        end
        Base.typemin(x::Type{$(esc(typename))}) = $(esc(typename))($lo)
        Base.typemax(x::Type{$(esc(typename))}) = $(esc(typename))($hi)
        Base.isless(x::$(esc(typename)), y::$(esc(typename))) = isless(x.val, y.val)
        let insts = ntuple(i->$(esc(typename))($values[i]), $(length(vals)))
            Base.instances(::Type{$(esc(typename))}) = insts
        end
        function Base.print(io::IO,x::$(esc(typename)))
            for (sym, i) in $vals
                if i == x.val
                    print(io, sym); break
                end
            end
        end
        Base.show(io::IO,x::$(esc(typename))) = print(io, x, "::", $(esc(typename)))
    end
    if isa(T,Symbol)
        for (sym,i) in vals
            push!(blk.args, :(const $(esc(sym)) = $(esc(T))($i)))
        end
    end
    push!(blk.args, :nothing)
    blk.head = :toplevel
    return blk
end

end # module

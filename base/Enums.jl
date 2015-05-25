# This file is a part of Julia. License is MIT: http://julialang.org/license

module Enums

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
    if isempty(syms)
        throw(ArgumentError("no arguments given for Enum $T"))
    end
    if isa(T,Symbol)
        typename = T
    else
        throw(ArgumentError("invalid type expression for enum $T"))
    end
    vals = Array(Tuple{Symbol,Integer},0)
    lo = hi = 0
    i = -1
    enumT = typeof(i)
    hasexpr = false
    for s in syms
        if isa(s,Symbol)
            if i == typemax(typeof(i))
                i = widen(i) + one(i)
            else
                i += one(i)
            end
        elseif isa(s,Expr) &&
               (s.head == :(=) || s.head == :kw) &&
               length(s.args) == 2 && isa(s.args[1],Symbol)
            i = eval(s.args[2]) # allow exprs, e.g. uint128"1"
            s = s.args[1]
            hasexpr = true
        else
            throw(ArgumentError(string("invalid argument for Enum ", typename, ": ", s)))
        end
        if !isa(i, Integer)
            throw(ArgumentError("invalid value for Enum $typename, $s=$i; values must be integers"))
        end
        if !Base.isidentifier(s)
            throw(ArgumentError("invalid name for Enum $typename; \"$s\" is not a valid identifier."))
        end
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
        Base.length(x::Type{$(esc(typename))}) = $(length(vals))
        Base.start(::Type{$(esc(typename))}) = 1
        Base.next(x::Type{$(esc(typename))},s) = ($(esc(typename))($values[s]),s+1)
        Base.done(x::Type{$(esc(typename))},s) = s > $(length(values))
        Base.names(x::Type{$(esc(typename))}) = [$(map(x->Expr(:quote, (x[1])), vals)...)]
        Base.isless(x::$(esc(typename)), y::$(esc(typename))) = isless(x.val, y.val)
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

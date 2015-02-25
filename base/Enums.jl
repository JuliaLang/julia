module Enums

export Enum, @enum

abstract Enum

Base.convert{T<:Integer}(::Type{T},x::Enum) = convert(T, x.val)
Base.convert{T<:Enum}(::Type{T},x::Integer) = T(x)
Base.start{T<:Enum}(::Type{T}) = 1
# next, done defined per Enum

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
    elseif isa(T,Expr) && T.head === :curly
        typename = T.args[1]
    else
        throw(ArgumentError("invalid type expression for enum $T"))
    end
    vals = Array((Symbol,Integer),0)
    lo = typemax(Int)
    hi = typemin(Int)
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
        enumT = length(vals) == 1 ? I : promote_type(enumT,I)
        lo = min(lo, i)
        hi = max(hi, i)
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
            function $(esc(typename))(x::Integer)
                $(membershiptest(:x, values)) || enum_argument_error($(Meta.quot(typename)), x)
                new(x)
            end
        end
        Base.typemin{E<:$(esc(typename))}(x::Type{E}) = E($lo)
        Base.typemax{E<:$(esc(typename))}(x::Type{E}) = E($hi)
        Base.length{E<:$(esc(typename))}(x::Type{E}) = $(length(vals))
        Base.next{E<:$(esc(typename))}(x::Type{E},s) = (E($values[s]),s+1)
        Base.done{E<:$(esc(typename))}(x::Type{E},s) = s > $(length(values))
        Base.names{E<:$(esc(typename))}(x::Type{E}) = [$(map(x->Meta.quot(x[1]), vals)...)]
        function Base.print{E<:$(esc(typename))}(io::IO,x::E)
            for (sym, i) in $vals
                if i == x.val
                    print(io, sym); break
                end
            end
        end
        Base.show{E<:$(esc(typename))}(io::IO,x::E) = print(io, x, "::", E)
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

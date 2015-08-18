# This file is a part of Julia. License is MIT: http://julialang.org/license

module Enums

export Enum, @enum

abstract Enum

Base.convert{T<:Integer}(::Type{T}, x::Enum) = convert(T, Intrinsics.box(Int32, x))

Base.write(io::IO, x::Enum) = write(io, Int32(x))
Base.read{T<:Enum}(io::IO, ::Type{T}) = T(read(io, Int32))

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
    if !isa(T,Symbol)
        throw(ArgumentError("invalid type expression for enum $T"))
    end
    typename = T
    vals = Array(Tuple{Symbol,Integer},0)
    lo = hi = 0
    i = Int32(-1)
    hasexpr = false
    for s in syms
        if isa(s,Symbol)
            if i == typemax(typeof(i))
                throw(ArgumentError("overflow in value \"$s\" of Enum $typename"))
            end
            i += one(i)
        elseif isa(s,Expr) &&
               (s.head == :(=) || s.head == :kw) &&
               length(s.args) == 2 && isa(s.args[1],Symbol)
            i = eval(current_module(),s.args[2]) # allow exprs, e.g. uint128"1"
            if !isa(i, Integer)
                throw(ArgumentError("invalid value for Enum $typename, $s=$i; values must be integers"))
            end
            i = convert(Int32, i)
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
    end
    values = Int32[i[2] for i in vals]
    if hasexpr && values != unique(values)
        throw(ArgumentError("values for Enum $typename are not unique"))
    end
    blk = quote
        # enum definition
        bitstype 32 $(esc(T)) <: Enum
        function Base.convert(::Type{$(esc(typename))}, x::Integer)
            $(membershiptest(:x, values)) || enum_argument_error($(Expr(:quote, typename)), x)
            Intrinsics.box($(esc(typename)), convert(Int32, x))
        end
        Base.typemin(x::Type{$(esc(typename))}) = $(esc(typename))($lo)
        Base.typemax(x::Type{$(esc(typename))}) = $(esc(typename))($hi)
        Base.isless(x::$(esc(typename)), y::$(esc(typename))) = isless(Int32(x), Int32(y))
        let insts = ntuple(i->$(esc(typename))($values[i]), $(length(vals)))
            Base.instances(::Type{$(esc(typename))}) = insts
        end
        function Base.print(io::IO,x::$(esc(typename)))
            for (sym, i) in $vals
                if i == Int32(x)
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

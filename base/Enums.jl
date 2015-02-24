module Enums

export Enum, @enum

abstract Enum

Base.convert{T<:Integer}(::Type{T},x::Enum) = convert(T, x.val)
Base.convert{T<:Enum}(::Type{T},x::Integer) = T(x)
Base.start{T<:Enum}(::Type{T}) = 1
Base.next{T<:Enum}(::Type{T},s) = Base.next(names(T),s)
Base.done{T<:Enum}(::Type{T},s) = Base.done(names(T),s)

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
    prev = -1
    first = true
    enumT = typeof(i)
    hasexpr = false
    contiguous = true
    for s in syms
        if isa(s,Symbol)
            if i == typemax(typeof(i))
                i = widen(i) + one(typeof(i))
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
            throw(ArgumentError(string("invalid argument for Enum ", typename, ", ", s)))
        end
        if !isa(i, Integer)
            throw(ArgumentError("Invalid value for Enum $typename, $s=$i. Enum values must be integers."))
        end
        if !Base.isidentifier(s)
            throw(ArgumentError("Invalid name for Enum $typename, $s is not a valid identifier."))
        end
        push!(vals, (s,i))
        I = typeof(i)
        enumT = length(vals) == 1 ? I : promote_type(enumT,I)
        lo = min(lo, i)
        hi = max(hi, i)
        if !first
            contiguous &= (i == prev+1)
        end
        first = false
        prev = i
    end
    if !hasexpr
        n = length(vals)
        enumT = n <= typemax(Int8) ? Int8 :
                n <= typemax(Int16) ? Int16 :
                n <= typemax(Int32) ? Int32 : Int64
    end
    vals = map(x->(x[1],convert(enumT,x[2])), vals)
    quotednames = map(x->Meta.quot(x[1]), vals)
    all_instances = map(x->Expr(:call, T, x[2]), vals)
    valueset = Set(map(x->x[2], vals))
    blk = quote
        # enum definition
        immutable $(esc(T)) <: Enum
            val::$enumT
            function $(esc(typename))(x::Integer)
                $(if contiguous
                    :($lo <= x <= $hi)
                else
                    :(x in $valueset)
                end) || throw(ArgumentError(string("invalid value for Enum ",$(Meta.quot(typename)),", ",x)))
                new(x)
            end
        end
        Base.typemin{E<:$(esc(typename))}(x::Type{E}) = E($lo)
        Base.typemax{E<:$(esc(typename))}(x::Type{E}) = E($hi)
        Base.length{E<:$(esc(typename))}(x::Type{E}) = $(length(vals))
        Base.names{E<:$(esc(typename))}(x::Type{E}) = tuple($(map(x->Expr(:call, :E, x[2]), vals)...))
        function Base.show{E<:$(esc(typename))}(io::IO,x::E)
            for (sym, i) in $vals
                if i == x.val
                    print(io, sym); break
                end
            end
            print(io, "::", E)
        end
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

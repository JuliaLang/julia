module Enums

export Enum, @enum

abstract Enum

Base.convert{T<:Integer}(::Type{T},x::Enum) = convert(T, x.val)
Base.convert(::Type{BigInt},x::Enum) = big(x.val)
function Base.convert{T<:Enum}(::Type{T},x::Integer)
    (x < typemin(T).val || x > typemax(T).val) && throw(InexactError())
    T(x)
end
Base.start{T<:Enum}(::Type{T}) = 1
# next, done defined per Enum

macro enum(T,syms...)
    if isempty(syms)
        throw(ArgumentError("no arguments given for Enum $T"))
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
            throw(ArgumentError(string("invalid argument for Enum ", T, ", ", s)))
        end
        if !isa(i, Integer) || !isbits(i)
            throw(ArgumentError("Invalid value for Enum $T, $s=$i. Enum values must be integer bits types."))
        end
        if !Base.isidentifier(s)
            throw(ArgumentError("Invalid name for Enum $T, $s is not a valid identifier."))
        end
        push!(vals, (s,i))
        I = typeof(i)
        enumT = length(vals) == 1 ? I : promote_type(enumT,I)
        lo = convert(enumT,min(lo, i))
        hi = convert(enumT,max(hi, i))
    end
    if hasexpr
        seen = Dict{Integer,Symbol}()
        for (sym,i) in vals
            if haskey(seen, i)
                i = convert(enumT,i)
                throw(ArgumentError("@enum argument values must be unique, $(seen[i])=$i and $sym=$i"))
            else
                seen[i] = sym
            end
        end
    else
        n = length(vals)
        enumT = n <= typemax(Int8) ? Int8 :
                n <= typemax(Int16) ? Int16 :
                n <= typemax(Int32) ? Int32 : Int64
    end
    values = enumT[i[2] for i in vals]
    if !hasexpr || length(lo:hi) == length(values)
        cond = :(x > $(lo - 1) && x < $(hi + 1))
    else
        val_set = Set{enumT}(values)
        cond = :(x in $val_set)
    end
    inner_constructor = :($(esc(T))(x::Integer) =  $cond ? new(convert($enumT,x)) : throw(ArgumentError("invalid value for Enum $($(esc(T))), $x")))
    quotedsyms = [Meta.quot(i[1]) for i in vals]
    blk = quote
        # enum definition
        immutable $(esc(T)) <: Enum
            val::$(esc(enumT))
            $inner_constructor
        end
        # enum traits
        $(esc(:(Base.typemin)))(x::Type{$(esc(T))}) = $(esc(T))($lo)
        $(esc(:(Base.typemax)))(x::Type{$(esc(T))}) = $(esc(T))($hi)
        $(esc(:(Base.length)))(x::Type{$(esc(T))}) = $(length(vals))
        $(esc(:(Base.names)))(x::Type{$(esc(T))}) = [$(quotedsyms...)]
        $(esc(:(Base.next)))(::Type{$(esc(T))},s) = ($(esc(T))($values[s]),s+1)
        $(esc(:(Base.done)))(::Type{$(esc(T))},s) = s > $(length(values))
        function $(esc(:(Base.show))){T<:$(esc(T))}(io::IO,x::T)
            vals = $vals
            for (sym, i) in vals
                i = convert($(esc(enumT)), i)
                if i == x.val
                    print(io, sym)
                    break
                end
            end
            print(io, "::", T.name)
        end
    end
    for (sym,i) in vals
        i = convert(enumT, i)
        # define enum member constants
        push!(blk.args, :(const $(esc(sym)) = $(esc(T))($i)))
    end
    push!(blk.args, :nothing)
    blk.head = :toplevel
    return blk
end

end # module

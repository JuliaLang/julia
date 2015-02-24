module Enums

export Enum, @enum

abstract Enum

function Base.convert{T<:Integer}(::Type{T},x::Enum)
    (x.val < typemin(T) || x.val > typemax(T)) && throw(InexactError())
    convert(T, x.val)
end
Base.convert(::Type{BigInt},x::Enum) = big(x.val)
function Base.convert{T<:Enum}(::Type{T},x::Integer)
    (x < typemin(T).val || x > typemax(T).val) && throw(InexactError())
    T(x)
end
Base.start{T<:Enum}(::Type{T}) = 1
Base.next{T<:Enum}(::Type{T},s) = Base.next(names(T),s)
Base.done{T<:Enum}(::Type{T},s) = Base.done(names(T),s)

# Pass Integer through to Enum constructor through Val{T}
call{T<:Enum}(::Type{T},x::Integer) = T(Val{convert(fieldtype(T,:val),x)})

# Catchall that errors when specific Enum(::Type{Val{n}}) hasn't been defined
call{T<:Enum,n}(::Type{T},::Type{Val{n}}) = throw(ArgumentError("invalid value for Enum $T, $n"))

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
                i = widen(i) + one(typeof(i))
            else
                i += one(typeof(i))
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
            throw(ArgumentError("invalid value for Enum $T, $s=$i. Enum values must be integer bits types."))
        end
        if !Base.isidentifier(s)
            throw(ArgumentError("invalid name for Enum $T, $s is not a valid identifier."))
        end
        push!(vals, (s,i))
        I = typeof(i)
        enumT = length(vals) == 1 ? I : promote_type(enumT,I)
        lo = min(lo, i)
        hi = max(hi, i)
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
    blk = quote
        # enum definition
        immutable $(esc(T)) <: $(esc(Enum))
            val::$(esc(enumT))
        end
        $(esc(:(Base.typemin)))(x::Type{$(esc(T))}) = $(esc(T))($lo)
        $(esc(:(Base.typemax)))(x::Type{$(esc(T))}) = $(esc(T))($hi)
        $(esc(:(Base.length)))(x::Type{$(esc(T))}) = $(length(vals))
        $(esc(:(Base.names)))(x::Type{$(esc(T))}) = $(esc(T))[]
        function $(esc(:(Base.show))){T<:$(esc(T))}(io::IO,x::T)
            vals = $vals
            for (sym, i) in vals
                i = convert($(esc(enumT)), i)
                i == x.val && print(io, sym)
            end
            print(io, "::", T.name)
        end
    end
    for (sym,i) in vals
        i = convert(enumT, i)
        # add inner constructors to Enum type definition for specific Val{T} values
        push!(blk.args[2].args[3].args, :($(esc(T))(::Type{Val{$i}}) = new($i)))
        # define enum member constants
        push!(blk.args, :(const $(esc(sym)) = $(esc(T))($i)))
        # add enum member value to names(T) function
        push!(blk.args[10].args[2].args[2].args, :($(esc(sym))))
    end
    push!(blk.args, :nothing)
    blk.head = :toplevel
    return blk
end

end # module

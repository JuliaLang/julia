abstract Enum

Base.typemin{T<:Enum}(x::Type{T}) = T(0)
function Base.show{T<:Enum}(io::IO, x::Type{T})
    if T == Enum
        print(io, "Enum")
    else
        print(io, string("enum ", T.name, ' ', '(', join(names(T), ", "), ')'))
    end
end
function Base.convert{T<:Integer}(::Type{T},x::Enum)
    if x.n < typemin(T) || x.n > typemax(T)
        error(InexactError())
    end
    convert(T, x.n)
end
function Base.convert{T<:Enum}(::Type{T},x::Integer)
    if x < typemin(T).n || x > typemax(T).n
        error(InexactError())
    end
    T(x)
end
function Base.show{T<:Enum}(io::IO, x::T)
    print(io, T.name, '(')
    first = true
    for s = T
        if s[2] == x.n
            if !first
                print(io, " or ")
            end
            print(io, s[1])
            first = false
        end
    end
    if first
        print(io, "invalid enum constant")
    end
    print(io, ')')    
end
Base.names{T<:Enum}(x::Type{T}) = [s[1] for s in T]

macro enum(T,syms...)
    assert(!isempty(syms))
    vals = Array((Symbol,Int64),0)
    i::Int64 = -1
    lo::Int64 = typemax(Int64)
    hi::Int64 = typemin(Int64)
    hasexpr = false
    signed = false
    for s in syms
        if i == typemax(Int64)
            error("@expr enum has a max size of Int64")
        end
        i += 1
        if isa(s,Symbol)
            push!(vals, (s,i))
        elseif isa(s,Expr) && s.head == :(=) && length(s.args) == 2 && isa(s.args[1],Symbol) && isa(s.args[2],Integer)
            if abs(s.args[2]) > typemax(Int64)
                error("@expr argument too large")
            end
            i = s.args[2]
            push!(vals, (s.args[1], i))
            hasexpr = true
        else
            error(string("invalid syntax in @expr macro call: ",s))
        end
        if i < 0 && !signed
            signed = true
        end
        if i < lo
            lo = i
        end
        if i > hi
            hi = i
        end
    end
    if signed
        if max(abs(lo),abs(hi)) < typemax(Int32)
            enumT = Int32
        else
            enumT = Int64
        end
    else
        if max(abs(lo),abs(hi)) < typemax(Uint32)
            enumT = Uint32
        else
            enumT = Uint64
        end
    end
    blk = quote
        immutable $(esc(T)) <: $(esc(Enum))
            n::$(enumT)
            $(esc(T))(n::Integer) = new(n)
        end
        if $lo != 0
            $(esc(:(Base.typemin)))(x::Type{$(esc(T))}) = $(esc(T))($lo)
        end
        $(esc(:(Base.typemax)))(x::Type{$(esc(T))}) = $(esc(T))($hi)
        $(esc(:(Base.start)))(x::Type{$(esc(T))}) = $(esc(:(Base.start)))($vals)
        $(esc(:(Base.next)))(x::Type{$(esc(T))},s) = $(esc(:(Base.next)))($vals,s)
        $(esc(:(Base.done)))(x::Type{$(esc(T))},s) = $(esc(:(Base.done)))($vals,s)
        $(esc(:(Base.length)))(x::Type{$(esc(T))}) = $(length(vals))
    end
    for (sym,i) in vals
        push!(blk.args, :(const $(esc(sym)) = $(esc(T))($i)))
    end
    push!(blk.args, :nothing)
    blk.head = :toplevel
    return blk
end

abstract EnumMask <: Enum

Base.(:&){T<:EnumMask}(a::T, b::T) = T(a.n & b.n)
Base.(:|){T<:EnumMask}(a::T, b::T) = T(a.n | b.n)
Base.(:~){T<:EnumMask}(a::T) = T(~a.n)
Base.(:(==)){T<:EnumMask}(a::T, b::T) = a.n == b.n
Base.(:!=){T<:EnumMask}(a::T, b::T) = a.n != b.n
function Base.show{T<:EnumMask}(io::IO, x::Type{T})
    if T == EnumMask
        print(io, "EnumMask")
    else
        print(io, string("enum mask ", T.name, ' ', '(', join(names(T), ", "), ')'))
    end
end
Base.show(io::IO, x::Type{EnumMask}) = print(io, "EnumMask")

macro enum_mask(T,syms...)
    if length(syms) <= 32
        enumT = Uint32
    elseif length(syms) <= 64
        enumT = Uint64
    else
        error("too many symbols for @enum_mask")
    end
    blk = quote
        immutable $(esc(T)) <: $(EnumMask)
            n::$(enumT)
            $(esc(T))(n::Integer) = new(n)
        end
        function $(esc(:(Base.show)))(io::IO, x::$(esc(T)))
            print(io, $(string(T)), '(')
            first = true
            if x.n == 0
                println("0")
            else
                for i = 0:$(length(syms)-1)
                    if x.n & (1<<i) != 0
                        if !first
                            print(io, '|')
                        else
                            first = false
                        end
                        print(io, $syms[i+1])
                    end
                end
            end
            println(')')
        end
        $(esc(:(Base.typemax)))(x::Type{$(esc(T))}) = $(esc(T))($(2^length(syms)-1))
        $(esc(:(Base.start)))(x::Type{$(esc(T))}) = ($(esc(:(Base.start)))($syms),1)
        $(esc(:(Base.next)))(x::Type{$(esc(T))},s) = (x=$(esc(:(Base.next)))($syms,s[1]); ((x[1],s[2]),(x[2],s[2]<<1)))
        $(esc(:(Base.done)))(x::Type{$(esc(T))},s) = $(esc(:(Base.done)))($syms,s[1])
        $(esc(:(Base.length)))(x::Type{$(esc(T))}) = $(length(syms))
    end
    for (i,sym) in enumerate(syms)
        push!(blk.args, :(const $(esc(sym)) = $(esc(T))($(1<<(i-1)))))
    end
    push!(blk.args, :nothing)
    blk.head = :toplevel
    return blk
end

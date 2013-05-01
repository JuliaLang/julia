module Enum
export @enum, @enum_mask

macro enum(T,syms...)
    blk = quote
        immutable $(esc(T))
            n::Uint32
            $(esc(T))(n::Integer) = new(n)
        end
        $(esc(:(Base.show)))(io::IO, x::$(esc(T))) = print(io, $syms[x.n+1])
        $(esc(:(Base.show)))(io::IO, x::Type{$(esc(T))}) = print(io, $(string("enum ", T, ' ', '(', join(syms, ", "), ')')))
    end
    for (i,sym) in enumerate(syms)
        push!(blk.args, :(const $(esc(sym)) = $(esc(T))($(i-1))))
    end
    push!(blk.args, :nothing)
    blk.head = :toplevel
    return blk
end

abstract EnumMask
Base.(:&){T<:EnumMask}(a::T, b::T) = T(a.n & b.n)
Base.(:|){T<:EnumMask}(a::T, b::T) = T(a.n | b.n)
Base.(:~){T<:EnumMask}(a::T) = T(~a.n)
Base.(:(==)){T<:EnumMask}(a::T, b::T) = T(a.n == b.n)
Base.(:!=){T<:EnumMask}(a::T, b::T) = T(a.n != b.n)

macro enum_mask(T,syms...)
    @assert length(syms) <= 32
    blk = quote
        immutable $(esc(T)) <: EnumMask
            n::Uint32
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
        $(esc(:(Base.show)))(io::IO, x::Type{$(esc(T))}) = print(io, $(string("enum mask ", T, ' ', '(', join(syms, ", "), ')')))
    end
    for (i,sym) in enumerate(syms)
        push!(blk.args, :(const $(esc(sym)) = $(esc(T))($(1<<(i-1)))))
    end
    push!(blk.args, :nothing)
    blk.head = :toplevel
    return blk
end

end

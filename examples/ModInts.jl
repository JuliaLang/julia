# This file is a part of Julia. License is MIT: https://julialang.org/license

module ModInts
export ModInt

import Base: +, -, *, /, inv

struct ModInt{n} <: Integer
    k::Int
    ModInt{n}(k) where {n} = new(mod(k,n))
end

Base.show(io::IO, k::ModInt{n}) where {n} =
    print(io, get(io, :typeinfo, Any) == typeof(k) ? k.k : "$(k.k) mod $n")

(+)(a::ModInt{n}, b::ModInt{n}) where {n} = ModInt{n}(a.k+b.k)
(-)(a::ModInt{n}, b::ModInt{n}) where {n} = ModInt{n}(a.k-b.k)
(*)(a::ModInt{n}, b::ModInt{n}) where {n} = ModInt{n}(a.k*b.k)
(-)(a::ModInt{n}) where {n} = ModInt{n}(-a.k)

inv(a::ModInt{n}) where {n} = ModInt{n}(invmod(a.k, n))
(/)(a::ModInt{n}, b::ModInt{n}) where {n} = a*inv(b) # broaden for non-coprime?

Base.promote_rule(::Type{ModInt{n}}, ::Type{Int}) where {n} = ModInt{n}
Base.convert(::Type{ModInt{n}}, i::Int) where {n} = ModInt{n}(i)

end # module

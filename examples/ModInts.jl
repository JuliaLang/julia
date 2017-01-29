# This file is a part of Julia. License is MIT: http://julialang.org/license

module ModInts
export ModInt

import Base: +, -, *, /, inv

immutable ModInt{n} <: Integer
    k::Int
    ModInt{n}(k) where n = new(mod(k,n))
end

Base.show{n}(io::IO, k::ModInt{n}) =
    print(io, get(io, :compact, false) ? k.k : "$(k.k) mod $n")

+{n}(a::ModInt{n}, b::ModInt{n}) = ModInt{n}(a.k+b.k)
-{n}(a::ModInt{n}, b::ModInt{n}) = ModInt{n}(a.k-b.k)
*{n}(a::ModInt{n}, b::ModInt{n}) = ModInt{n}(a.k*b.k)
-{n}(a::ModInt{n}) = ModInt{n}(-a.k)

inv{n}(a::ModInt{n}) = ModInt{n}(invmod(a.k, n))
/{n}(a::ModInt{n}, b::ModInt{n}) = a*inv(b) # broaden for non-coprime?

Base.promote_rule{n}(::Type{ModInt{n}}, ::Type{Int}) = ModInt{n}
Base.convert{n}(::Type{ModInt{n}}, i::Int) = ModInt{n}(i)

end # module

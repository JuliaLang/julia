importall Base

type ModInt{n} <: Integer
    k::Int
    ModInt(k) = new(mod(k,n))
end

-{n}(a::ModInt{n}) = ModInt{n}(-a.k)
+{n}(a::ModInt{n}, b::ModInt{n}) = ModInt{n}(a.k+b.k)
-{n}(a::ModInt{n}, b::ModInt{n}) = ModInt{n}(a.k-b.k)
*{n}(a::ModInt{n}, b::ModInt{n}) = ModInt{n}(a.k*b.k)

convert{n}(::Type{ModInt{n}}, i::Int) = ModInt{n}(i)
promote_rule{n}(::Type{ModInt{n}}, ::Type{Int}) = ModInt{n}

show{n}(io::IO, k::ModInt{n}) = print(io, "$(k.k) mod $n")
showcompact(io::IO, k::ModInt) = print(io, k.k)

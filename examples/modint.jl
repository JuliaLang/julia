type ModInt{N} <: Integer
    k::Int
    ModInt(k) = new(mod(k,N))
end

-{N}(a::ModInt{N}) = ModInt{N}(-a.k)
+{N}(a::ModInt{N}, b::ModInt{N}) = ModInt{N}(a.k+b.k)
-{N}(a::ModInt{N}, b::ModInt{N}) = ModInt{N}(a.k-b.k)
*{N}(a::ModInt{N}, b::ModInt{N}) = ModInt{N}(a.k*b.k)

convert{N}(::Type{ModInt{N}}, i::Int) = ModInt{N}(i)
promote_rule{N}(::Type{ModInt{N}}, ::Type{Int}) = ModInt{N}

show{n}(io, k::ModInt{n}) = print(io, "$(k.k) mod $n")
showcompact(io, k::ModInt) = print(io, k.k)

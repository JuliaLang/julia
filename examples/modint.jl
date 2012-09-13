type ModInt{N}
    k::Int
    ModInt(k) = new(k % N)
end

+{N}(a::ModInt{N}, b::ModInt{N}) = ModInt{N}(a.k+b.k)
*{N}(a::ModInt{N}, b::ModInt{N}) = ModInt{N}(a.k*b.k)

convert{N}(::Type{ModInt{N}}, i::Integer) = ModInt{N}(i)
show{n}(io, k::ModInt{n}) = println(io, "$(k.k) mod $n")
showcompact(io, k::ModInt) = print(io, k.k)

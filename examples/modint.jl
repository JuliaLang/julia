module ModInts
export ModInt

immutable ModInt{n} <: Integer
    k::Int
    ModInt(k) = new(mod(k,n))
end

-{n}(a::ModInt{n}) = ModInt{n}(-a.k)
+{n}(a::ModInt{n}, b::ModInt{n}) = ModInt{n}(a.k+b.k)
-{n}(a::ModInt{n}, b::ModInt{n}) = ModInt{n}(a.k-b.k)
*{n}(a::ModInt{n}, b::ModInt{n}) = ModInt{n}(a.k*b.k)

Base.convert{n}(::Type{ModInt{n}}, i::Int) = ModInt{n}(i)
Base.promote_rule{n}(::Type{ModInt{n}}, ::Type{Int}) = ModInt{n}

Base.show{n}(io::IO, k::ModInt{n}) = print(io, "$(k.k) mod $n")
Base.showcompact(io::IO, k::ModInt) = print(io, k.k)

function Base.inv{n}(a::ModInt{n})
    thegcd, x = gcdx(a.k, n)
    thegcd==1 ? ModInt{n}(x) : error("Inverse of $(a.k) mod $n doesn't exist")
end
    
end # module

type Buf <: IO b::Vector end
Base.write(b::Buf, v::Vector{UInt8}) = (b.b = v; length(v))

b = Buf([])
@test write(b, IOBuffer("abc")) == 3
@test b.b == b"abc"

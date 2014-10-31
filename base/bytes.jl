using Core.Intrinsics
importall Core
importall Base
import Base: show_comma_array

Bytes(a::Vector{Uint8}) = ccall(:jl_bytes, Bytes, (Ptr{Uint8}, Csize_t), a, length(a))
Bytes(s::String) = Bytes(bytestring(s).data)
Bytes(b::Uint8...) = Bytes(Uint8[b...])

eltype(b::Bytes) = Uint8
endof(b::Bytes) = length(b)
getindex(b::Bytes, i::Real) = ccall(:jl_bytesref, Uint8, (Bytes, Csize_t), b, i-1)
length(b::Bytes) = ifelse(b.neglen < 0, -b.neglen, b.neglen >>> ((sizeof(Int)-1) << 3))

start(b::Bytes) = 1
next(b::Bytes, i::Int) = (b[i], i+1)
done(b::Bytes, i::Int) = length(b) < i

function show(io::IO, b::Bytes)
	print(io, "Bytes")
	show_comma_array(io, b, '(', ')')
end

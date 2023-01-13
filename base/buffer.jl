size(a::Buffer) = (Core.Intrinsics.bufferlen(a),)
length(a::Buffer) = Core.Intrinsics.bufferlen(a)
sizeof(a::Buffer) = Core.sizeof(a)

function isassigned(a::Buffer, i::Int)
    @inline
    @boundscheck i < length(a) % UInt || return false
    ccall(:jl_buffer_isassigned, Cint, (Any, UInt), a, i) == 1
end

@eval getindex(A::Buffer, i1::Int) = Core.bufferref($(Expr(:boundscheck)), A, i1)
@eval setindex!(A::Buffer{T}, x, i1::Int) where {T} = Core.bufferset($(Expr(:boundscheck)), A, convert(T, x)::T, i1)

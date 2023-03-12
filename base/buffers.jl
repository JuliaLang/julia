

if !isdefined(Main, :Base)
    Buffer{T}(::UndefInitializer, len::Int) where {T} =
        ccall(:jl_new_buffer, Buffer{T}, (Any, Int), Buffer{T}, len)
    Buffer(a::AbstractArray{T}) where {T} = Buffer{T}(a)
    function Buffer{T}(a::AbstractArray) where {T}
        n = length(a)
        b = Buffer{T}(undef, n)
        i = 1
        for a_i in a
            @inbounds b[i] = a_i
            i += 1
        end
        return b
    end
    function isassigned(b::Buffer, i::Int)
        @inline
        @boundscheck 1 <= i <= length(b) || return false
        ccall(:jl_buffer_isassigned, Cint, (Any, UInt), b, i) == 1
    end
end

@eval getindex(b::Buffer{T}, i::Int) where {T} = Core.bufref($(Expr(:boundscheck)), b, i)
@eval setindex!(b::Buffer{T}, x, i::Int) where {T} =
    Core.bufset($(Expr(:boundscheck)), b, convert(T, x)::T, i)
elsize(@nospecialize T::Type{<:Buffer}) = aligned_sizeof(eltype(T))
length(b::Buffer) = Core.Intrinsics.bufferlen(b)
firstindex(b::Buffer) = 1
lastindex(b::Buffer) = length(b)
keys(b::Buffer) = OneTo(length(b))
axes(b::Buffer) = (OneTo(length(b)),)
axes(b::Buffer, d::Integer) = d <= 1 ? OneTo(length(b)) : OneTo(1)
iterate(v::Buffer, i=1) = (length(v) < i ? nothing : (v[i], i + 1))
isempty(b::Buffer) = (length(b) == 0)
function ==(v1::Buffer, v2::Buffer)
    length(v1)==length(v2) || return false
    for i in 1:length(v1)
        v1[i] == v2[i] || return false
    end
    return true
end

sizeof(b::Buffer) = Core.sizeof(b)

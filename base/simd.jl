module SIMD

import Base: VecElement, Memory, MemoryRef
import Base: @propagate_inbounds, @_propagate_inbounds_meta, @_boundscheck, @_noub_if_noinbounds_meta
import Base: memoryrefget, memoryrefnew, memoryrefset!

export Vec
export vload, vstore!, natural_vecwidth

# TODO: See C# and Co Vec type 
# TODO: Hardware portable vector types...

struct Vec{N, T}
    data::NTuple{N, VecElement{T}}
end

# Constructors
@inline Vec(v::NTuple{N, T}) where {N, T} = Vec(VecElement.(v))
@inline Vec(v::Vararg{T, N}) where {N, T} = Vec(v)
@inline Vec(v::Vec) = v

# Numbers defines this and it is needed in power_by_squaring...
Base.copy(v::Vec) = v

function Base.show(io::IO, v::Vec{N, T}) where {N, T}
    io = IOContext(io, :typeinfo => eltype(v))
    print(io, "<$N x $T>[")
    join(io, [sprint(show, x.value; context=io) for x in v.data], ", ")
    print(io, "]")
end

# Breaks with multi-versioning
natural_vecwidth(::Type{Float32}) = 8
natural_vecwidth(::Type{Float64}) = 4

import Base: +, -, *

# Mocked vload/vstore! relying on SLP

@inline function vload(::Type{Vec{N, T}}, A::Array{T}, i::Int) where {N, T}
    @_noub_if_noinbounds_meta
    # TODO: Alignment...; may need an intrinsic for vectorized loads.
    # Writting my own boundscheck loop since `inbounds` doesn't propagate through `ntuple` FFS
    @boundscheck checkbounds(A, i:(i+ N - 1))
    mem = A.ref
    data = ntuple(Val(N)) do j
        # why does `@inbounds  ref = memoryrefnew(mem, i + j - 1, @_boundscheck)` not work?
        ref = memoryrefnew(mem, i + j - 1, false)
        VecElement{T}(memoryrefget(ref, :not_atomic, false))
    end
    return Vec(data)
end

@inline function vstore!(A::Array{T}, v::Vec{N, T}, i::Int) where {N, T}
    @_noub_if_noinbounds_meta
    # TODO: Alignment...; may need an intrinsic for vectorized loads.
    # Writting my own boundscheck loop since `inbounds` doesn't propagate through `ntuple` FFS
    @boundscheck checkbounds(A, i:(i+ N - 1))
    mem = A.ref
    data = v.data
    ntuple(Val(N)) do j
        # why does `@inbounds  ref = memoryrefnew(mem, i + j - 1, @_boundscheck)` not work?
        ref = memoryrefnew(mem, i + j - 1, false)
        memoryrefset!(ref, data[j].value, :not_atomic, false)
        return nothing
    end
    return nothing
end

end # module
module SIMD

import Base: VecElement, Memory, MemoryRef, IEEEFloat
import Base: @propagate_inbounds, @_propagate_inbounds_meta, @_boundscheck, @_noub_if_noinbounds_meta
import Base: memoryrefget, memoryrefnew, memoryrefset!

import Core.Intrinsics: preferred_vector_width

export Vec
export vload, vstore!, preferred_vector, width

# TODO: See C# and Co Vec type 
# TODO: Hardware portable vector types...

# TODO: tfunc support for preferred_vector_width does allow for "constant prop"
#       but the intrinsic is not removed just yet during JIT, we should only need
#       it for AOT or on a machine with scaleable vector types...

struct Vec{N, T}
    data::NTuple{N, VecElement{T}}
end

width(::Type{<:Vec{N}}) where N = N
width(::Vec{N}) where N = N

function preferred_vector(::Type{T}) where T
    width = preferred_vector_width(T)
    if width === nothing
        error("$T has no preferred_vector_width")
    end
    return Vec{width, T}
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

import Base: +, -, *, /, muladd, promote_rule, widen
import Core.Intrinsics: add_float, sub_float, mul_float, div_float, muladd_float, neg_float

## floating point promotions ##
promote_rule(::Type{Vec{N, Float32}}, ::Type{Vec{N, Float16}}) where N = Vec{N, Float32}
promote_rule(::Type{Vec{N, Float64}}, ::Type{Vec{N, Float16}}) where N = Vec{N, Float64}
promote_rule(::Type{Vec{N, Float64}}, ::Type{Vec{N, Float32}}) where N = Vec{N, Float64}

widen(::Type{Vec{N, Float16}}) where N = Vec{N, Float16}
widen(::Type{Vec{N, Float32}}) where N = Vec{N, Float32}

## floating point arithmetic ##
-(x::Vec{N,T}) where {N,T<:IEEEFloat} = neg_float(x.data)

+(x::Vec{N,T}, y::Vec{N,T}) where {N,T<:IEEEFloat} = add_float(x.data, y.data)
-(x::Vec{N,T}, y::Vec{N,T}) where {N,T<:IEEEFloat} = sub_float(x.data, y.data)
*(x::Vec{N,T}, y::Vec{N,T}) where {N,T<:IEEEFloat} = mul_float(x.data, y.data)
/(x::Vec{N,T}, y::Vec{N,T}) where {N,T<:IEEEFloat} = div_float(x.data, y.data)

muladd(x::Vec{N,T}, y::Vec{N,T}, z::Vec{N,T}) where {N, T<:IEEEFloat} =
    muladd_float(x.data, y.data, z.data)

end # module
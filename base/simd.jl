module SIMD

import Base: VecElement, Memory, MemoryRef, IEEEFloat
import Base: @propagate_inbounds, @_propagate_inbounds_meta, @_boundscheck, @_noub_if_noinbounds_meta
import Base: memoryrefget, memoryrefnew, memoryrefset!

import Core.Intrinsics: preferred_vector_width

export Vec
export vload, vstore!, preferred_vector, width, select

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

# TODO: llvm.vp expects a mask of i1
const Mask{N} = Vec{N, Bool}

function Vec{N}(val) where N
    Vec(ntuple(_->VecElement(val),Val(N)))
end

# select(m::Mask{N}, a::Vec{N, T}, b::Vec{N,T}) where {N,T} = Core.ifelse(m.data, a.data, b.data)
# ERROR: TypeError: non-boolean (NTuple{4, VecElement{Bool}}) used in boolean context
# Mocked select, relying on SLP
function select(m::Mask{N}, a::Vec{N, T}, b::Vec{N,T}) where {N,T}
    data = ntuple(Val(N)) do j
        VecElement(Core.ifelse(m.data[j].value, a.data[j].value, b.data[j].value))
    end
    return Vec(data)
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
-(x::Vec{N,T}) where {N,T<:IEEEFloat} = Vec(neg_float(x.data))

+(x::Vec{N,T}, y::Vec{N,T}) where {N,T<:IEEEFloat} = Vec(add_float(x.data, y.data))
-(x::Vec{N,T}, y::Vec{N,T}) where {N,T<:IEEEFloat} = Vec(sub_float(x.data, y.data))
*(x::Vec{N,T}, y::Vec{N,T}) where {N,T<:IEEEFloat} = Vec(mul_float(x.data, y.data))
/(x::Vec{N,T}, y::Vec{N,T}) where {N,T<:IEEEFloat} = Vec(div_float(x.data, y.data))

muladd(x::Vec{N,T}, y::Vec{N,T}, z::Vec{N,T}) where {N, T<:IEEEFloat} =
    Vec(muladd_float(x.data, y.data, z.data))

## integer arithmetic ##
import Base: ÷, BitInteger, BitSigned, BitUnsigned
import Core.Intrinsics: add_int, sub_int, mul_int, sdiv_int, udiv_int, neg_int

+(x::Vec{N,T}, y::Vec{N,T}) where {N,T<:BitInteger} = Vec(add_int(x.data, y.data))
-(x::Vec{N,T}, y::Vec{N,T}) where {N,T<:BitInteger} = Vec(sub_int(x.data, y.data))
*(x::Vec{N,T}, y::Vec{N,T}) where {N,T<:BitInteger} = Vec(mul_int(x.data, y.data))
# TODO ought we implement div by zero?
÷(x::Vec{N,T}, y::Vec{N,T}) where {N,T<:BitSigned}   = Vec(sdiv_int(x.data, y.data))
÷(x::Vec{N,T}, y::Vec{N,T}) where {N,T<:BitUnsigned} = Vec(udiv_int(x.data, y.data))

## logical ops
import Base: xor, |, &
import Core.Intrinsics: xor_int, and_int, or_int
xor(x::Vec{N,T}, y::Vec{N,T}) where {N,T<:BitInteger} = Vec(xor_int(x.data, y.data))
(|)(x::Vec{N,T}, y::Vec{N,T}) where {N,T<:BitInteger} = Vec(and_int(x.data, y.data))
(&)(x::Vec{N,T}, y::Vec{N,T}) where {N,T<:BitInteger} = Vec(or_int(x.data, y.data))

## integer shifts
# unsigned shift counts always shift in the same direction
import Base: >>, <<, >>>
import Core.Intrinsics: ashr_int, lshr_int, shl_int, lshr_int
>>(x::Vec{N, <:BitSigned},   y::Vec{N, <:BitUnsigned}) where N = ashr_int(x, y)
>>(x::Vec{N, <:BitUnsigned}, y::Vec{N, <:BitUnsigned}) where N = lshr_int(x, y)
<<(x::Vec{N, <:BitInteger},  y::Vec{N, <:BitUnsigned}) where N = shl_int(x, y)
>>>(x::Vec{N, <:BitInteger}, y::Vec{N, <:BitUnsigned}) where N = lshr_int(x, y)

end # module
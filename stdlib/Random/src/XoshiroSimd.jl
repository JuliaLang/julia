# This file is a part of Julia. License is MIT: https://julialang.org/license

module XoshiroSimd
# Getting the xoroshiro RNG to reliably vectorize is somewhat of a hassle without Simd.jl.
import ..Random: rand!
using ..Random: TaskLocalRNG, rand, Xoshiro, CloseOpen01, UnsafeView, SamplerType, SamplerTrivial, getstate, setstate!, _uint2float
using Base: BitInteger_types
using Base.Libc: memcpy
using Core.Intrinsics: llvmcall

# Vector-width. Influences random stream.
xoshiroWidth() = Val(8)
# Simd threshold. Influences random stream.
simdThreshold(::Type{T}) where T = 64
simdThreshold(::Type{Bool}) = 640

@inline _rotl45(x::UInt64) = (x<<45)|(x>>19)
@inline _shl17(x::UInt64) = x<<17
@inline _rotl23(x::UInt64) = (x<<23)|(x>>41)
@inline _plus(x::UInt64,y::UInt64) = x+y
@inline _xor(x::UInt64,y::UInt64) = xor(x,y)
@inline _and(x::UInt64, y::UInt64) = x & y
@inline _or(x::UInt64, y::UInt64) = x | y
@inline _lshr(x, y::Int32) = _lshr(x, y % Int64)
@inline _lshr(x::UInt64, y::Int64) = llvmcall("""
    %res = lshr i64 %0, %1
    ret i64 %res
    """,
    UInt64,
    Tuple{UInt64, Int64},
    x, y)

# `_bits2float(x::UInt64, T)` takes `x::UInt64` as input, it splits it in `N` parts where
# `N = sizeof(UInt64) / sizeof(T)` (`N = 1` for `Float64`, `N = 2` for `Float32, etc...), it
# truncates each part to the unsigned type of the same size as `T`, scales all of these
# numbers to a value of type `T` in the range [0,1) with `_uint2float`, and then
# recomposes another `UInt64` using all these parts.
@inline _bits2float(x::UInt64, ::Type{Float64}) = reinterpret(UInt64,  _uint2float(x, Float64))
@inline function _bits2float(x::UInt64, ::Type{Float32})
    #=
    # this implementation uses more high bits, but is harder to vectorize
    x = x >>> 16  # discard low 16 bits
    u = Float32(x >>> 24) * Float32(0x1.0p-24)
    l = Float32(x & 0x00ffffff) * Float32(0x1.0p-24)
    =#
    ui = (x>>>32) % UInt32
    li = x % UInt32
    u = _uint2float(ui, Float32)
    l = _uint2float(li, Float32)
    (UInt64(reinterpret(UInt32, u)) << 32) | UInt64(reinterpret(UInt32, l))
end
@inline function _bits2float(x::UInt64, ::Type{Float16})
    i1 = (x>>>48) % UInt16
    i2 = (x>>>32) % UInt16
    i3 = (x>>>16) % UInt16
    i4 = x % UInt16
    f1 = _uint2float(i1, Float16)
    f2 = _uint2float(i2, Float16)
    f3 = _uint2float(i3, Float16)
    f4 = _uint2float(i4, Float16)
    return (UInt64(reinterpret(UInt16, f1)) << 48) | (UInt64(reinterpret(UInt16, f2)) << 32) | (UInt64(reinterpret(UInt16, f3)) << 16) | UInt64(reinterpret(UInt16, f4))
end

# required operations. These could be written more concisely with `ntuple`, but the compiler
# sometimes refuses to properly vectorize.
for N in [4,8,16]
    let code, s, fshl = "llvm.fshl.v$(N)i64",
        VT = :(NTuple{$N, VecElement{UInt64}})

        s = ntuple(_->VecElement(UInt64(45)), N)
        @eval @inline _rotl45(x::$VT) = ccall($fshl, llvmcall, $VT, ($VT, $VT, $VT), x, x, $s)

        s = ntuple(_->VecElement(UInt64(23)), N)
        @eval @inline _rotl23(x::$VT) = ccall($fshl, llvmcall, $VT, ($VT, $VT, $VT), x, x, $s)

        code = """
        %lshiftOp = shufflevector <1 x i64> <i64 17>, <1 x i64> undef, <$N x i32> zeroinitializer
        %res = shl <$N x i64> %0, %lshiftOp
        ret <$N x i64> %res
        """
        @eval @inline _shl17(x::$VT) = llvmcall($code, $VT, Tuple{$VT}, x)

        code = """
        %res = add <$N x i64> %1, %0
        ret <$N x i64> %res
        """
        @eval @inline _plus(x::$VT, y::$VT) = llvmcall($code, $VT, Tuple{$VT, $VT}, x, y)

        code = """
        %res = xor <$N x i64> %1, %0
        ret <$N x i64> %res
        """
        @eval @inline _xor(x::$VT, y::$VT) = llvmcall($code, $VT, Tuple{$VT, $VT}, x, y)

        code = """
        %res = and <$N x i64> %1, %0
        ret <$N x i64> %res
        """
        @eval @inline _and(x::$VT, y::$VT) = llvmcall($code, $VT, Tuple{$VT, $VT}, x, y)

        code = """
        %res = or <$N x i64> %1, %0
        ret <$N x i64> %res
        """
        @eval @inline _or(x::$VT, y::$VT) = llvmcall($code, $VT, Tuple{$VT, $VT}, x, y)

        code = """
        %tmp = insertelement <1 x i64> undef, i64 %1, i32 0
        %shift = shufflevector <1 x i64> %tmp, <1 x i64> %tmp, <$N x i32> zeroinitializer
        %res = lshr <$N x i64> %0, %shift
        ret <$N x i64> %res
        """
        @eval @inline _lshr(x::$VT, y::Int64) = llvmcall($code, $VT, Tuple{$VT, Int64}, x, y)

        code = """
        %shiftamt = shufflevector <1 x i64> <i64 11>, <1 x i64> undef, <$N x i32> zeroinitializer
        %sh = lshr <$N x i64> %0, %shiftamt
        %f = uitofp <$N x i64> %sh to <$N x double>
        %scale = shufflevector <1 x double> <double 0x3ca0000000000000>, <1 x double> undef, <$N x i32> zeroinitializer
        %m = fmul <$N x double> %f, %scale
        %i = bitcast <$N x double> %m to <$N x i64>
        ret <$N x i64> %i
        """
        @eval @inline _bits2float(x::$VT, ::Type{Float64}) = llvmcall($code, $VT, Tuple{$VT}, x)

        code = """
        %as32 = bitcast <$N x i64> %0 to <$(2N) x i32>
        %shiftamt = shufflevector <1 x i32> <i32 8>, <1 x i32> undef, <$(2N) x i32> zeroinitializer
        %sh = lshr <$(2N) x i32> %as32, %shiftamt
        %f = uitofp <$(2N) x i32> %sh to <$(2N) x float>
        %scale = shufflevector <1 x float> <float 0x3e70000000000000>, <1 x float> undef, <$(2N) x i32> zeroinitializer
        %m = fmul <$(2N) x float> %f, %scale
        %i = bitcast <$(2N) x float> %m to <$N x i64>
        ret <$N x i64> %i
        """
        @eval @inline _bits2float(x::$VT, ::Type{Float32}) = llvmcall($code, $VT, Tuple{$VT}, x)

        code = """
        %as16 = bitcast <$N x i64> %0 to <$(4N) x i16>
        %shiftamt = shufflevector <1 x i16> <i16 5>, <1 x i16> undef, <$(4N) x i32> zeroinitializer
        %sh = lshr <$(4N) x i16> %as16, %shiftamt
        %f = uitofp <$(4N) x i16> %sh to <$(4N) x half>
        %scale = shufflevector <1 x half> <half 0x3f40000000000000>, <1 x half> undef, <$(4N) x i32> zeroinitializer
        %m = fmul <$(4N) x half> %f, %scale
        %i = bitcast <$(4N) x half> %m to <$N x i64>
        ret <$N x i64> %i
        """
        @eval @inline _bits2float(x::$VT, ::Type{Float16}) = llvmcall($code, $VT, Tuple{$VT}, x)
    end
end


function forkRand(rng::Union{TaskLocalRNG, Xoshiro}, ::Val{N}) where N
    # constants have nothing up their sleeve. For more discussion, cf rng_split in task.c
    # 0x02011ce34bce797f == hash(UInt(1))|0x01
    # 0x5a94851fb48a6e05 == hash(UInt(2))|0x01
    # 0x3688cf5d48899fa7 == hash(UInt(3))|0x01
    # 0x867b4bb4c42e5661 == hash(UInt(4))|0x01
    s0 = ntuple(i->VecElement(0x02011ce34bce797f * rand(rng, UInt64)), Val(N))
    s1 = ntuple(i->VecElement(0x5a94851fb48a6e05 * rand(rng, UInt64)), Val(N))
    s2 = ntuple(i->VecElement(0x3688cf5d48899fa7 * rand(rng, UInt64)), Val(N))
    s3 = ntuple(i->VecElement(0x867b4bb4c42e5661 * rand(rng, UInt64)), Val(N))
    (s0, s1, s2, s3)
end

_id(x, T) = x

@inline function xoshiro_bulk(rng::Union{TaskLocalRNG, Xoshiro}, dst::Ptr{UInt8}, len::Int, T::Union{Type{UInt8}, Type{Bool}, Type{Float16}, Type{Float32}, Type{Float64}}, ::Val{N}, f::F = _id) where {N, F}
    if len >= simdThreshold(T)
        written = xoshiro_bulk_simd(rng, dst, len, T, Val(N), f)
        len -= written
        dst += written
    end
    if len != 0
        xoshiro_bulk_nosimd(rng, dst, len, T, f)
    end
    nothing
end

@noinline function xoshiro_bulk_nosimd(rng::Union{TaskLocalRNG, Xoshiro}, dst::Ptr{UInt8}, len::Int, ::Type{T}, f::F
                                       ) where {T, F}
    s0, s1, s2, s3 = getstate(rng)
    i = 0
    while i+8 <= len
        res = _plus(_rotl23(_plus(s0,s3)),s0)
        unsafe_store!(reinterpret(Ptr{UInt64}, dst + i), f(res, T))
        t = _shl17(s1)
        s2 = _xor(s2, s0)
        s3 = _xor(s3, s1)
        s1 = _xor(s1, s2)
        s0 = _xor(s0, s3)
        s2 = _xor(s2, t)
        s3 = _rotl45(s3)
        i += 8
    end
    if i < len
        res = _plus(_rotl23(_plus(s0,s3)),s0)
        t = _shl17(s1)
        s2 = _xor(s2, s0)
        s3 = _xor(s3, s1)
        s1 = _xor(s1, s2)
        s0 = _xor(s0, s3)
        s2 = _xor(s2, t)
        s3 = _rotl45(s3)
        ref = Ref(f(res, T))
        # TODO: This may make the random-stream dependent on system endianness
        GC.@preserve ref memcpy(dst+i, Base.unsafe_convert(Ptr{Cvoid}, ref), len-i)
    end
    setstate!(rng, (s0, s1, s2, s3, nothing))
    nothing
end

@noinline function xoshiro_bulk_nosimd(rng::Union{TaskLocalRNG, Xoshiro}, dst::Ptr{UInt8}, len::Int, ::Type{Bool}, f)
    s0, s1, s2, s3 = getstate(rng)
    i = 0
    while i+8 <= len
        res = _plus(_rotl23(_plus(s0,s3)),s0)
        shift = 0
        while i+8 <= len && shift < 8
            resLoc = _and(_lshr(res, shift), 0x0101010101010101)
            unsafe_store!(reinterpret(Ptr{UInt64}, dst + i), resLoc)
            i += 8
            shift += 1
        end

        t = _shl17(s1)
        s2 = _xor(s2, s0)
        s3 = _xor(s3, s1)
        s1 = _xor(s1, s2)
        s0 = _xor(s0, s3)
        s2 = _xor(s2, t)
        s3 = _rotl45(s3)
    end
    if i < len
        # we may overgenerate some bytes here, if len mod 64 <= 56 and len mod 8 != 0
        res = _plus(_rotl23(_plus(s0,s3)),s0)
        resLoc = _and(res, 0x0101010101010101)
        ref = Ref(resLoc)
        GC.@preserve ref memcpy(dst+i, Base.unsafe_convert(Ptr{Cvoid}, ref), len-i)
        t = _shl17(s1)
        s2 = _xor(s2, s0)
        s3 = _xor(s3, s1)
        s1 = _xor(s1, s2)
        s0 = _xor(s0, s3)
        s2 = _xor(s2, t)
        s3 = _rotl45(s3)
    end
    setstate!(rng, (s0, s1, s2, s3, nothing))
    nothing
end


@noinline function xoshiro_bulk_simd(rng::Union{TaskLocalRNG, Xoshiro}, dst::Ptr{UInt8}, len::Int, ::Type{T}, ::Val{N}, f::F) where {T,N,F}
    s0, s1, s2, s3 = forkRand(rng, Val(N))

    i = 0
    while i + 8*N <= len
        res = _plus(_rotl23(_plus(s0,s3)),s0)
        t = _shl17(s1)
        s2 = _xor(s2, s0)
        s3 = _xor(s3, s1)
        s1 = _xor(s1, s2)
        s0 = _xor(s0, s3)
        s2 = _xor(s2, t)
        s3 = _rotl45(s3)
        unsafe_store!(reinterpret(Ptr{NTuple{N,VecElement{UInt64}}}, dst + i), f(res, T))
        i += 8*N
    end
    return i
end

@noinline function xoshiro_bulk_simd(rng::Union{TaskLocalRNG, Xoshiro}, dst::Ptr{UInt8}, len::Int, ::Type{Bool}, ::Val{N}, f) where {N}
    s0, s1, s2, s3 = forkRand(rng, Val(N))
    msk = ntuple(i->VecElement(0x0101010101010101), Val(N))
    i = 0
    while i + 64*N <= len
        res = _plus(_rotl23(_plus(s0,s3)),s0)
        t = _shl17(s1)
        s2 = _xor(s2, s0)
        s3 = _xor(s3, s1)
        s1 = _xor(s1, s2)
        s0 = _xor(s0, s3)
        s2 = _xor(s2, t)
        s3 = _rotl45(s3)
        for k=0:7
            tmp = _lshr(res, k)
            toWrite = _and(tmp, msk)
            unsafe_store!(reinterpret(Ptr{NTuple{N,VecElement{UInt64}}}, dst + i + k*N*8), toWrite)
        end
        i += 64*N
    end
    return i
end

const MutableDenseArray = Union{Base.MutableDenseArrayType{T}, UnsafeView{T}} where {T}

function rand!(rng::Union{TaskLocalRNG, Xoshiro}, dst::MutableDenseArray{T}, ::SamplerTrivial{CloseOpen01{T}}) where {T<:Union{Float16,Float32,Float64}}
    GC.@preserve dst xoshiro_bulk(rng, convert(Ptr{UInt8}, pointer(dst)), length(dst)*sizeof(T), T, xoshiroWidth(), _bits2float)
    dst
end

for T in BitInteger_types
    @eval function rand!(rng::Union{TaskLocalRNG, Xoshiro}, dst::MutableDenseArray{$T}, ::SamplerType{$T})
        GC.@preserve dst xoshiro_bulk(rng, convert(Ptr{UInt8}, pointer(dst)), length(dst)*sizeof($T), UInt8, xoshiroWidth())
        dst
    end
end

function rand!(rng::Union{TaskLocalRNG, Xoshiro}, dst::MutableDenseArray{Bool}, ::SamplerType{Bool})
    GC.@preserve dst xoshiro_bulk(rng, convert(Ptr{UInt8}, pointer(dst)), length(dst), Bool, xoshiroWidth())
    dst
end

end # module

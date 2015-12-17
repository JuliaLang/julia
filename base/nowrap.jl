# This file is a part of Julia. License is MIT: http://julialang.org/license

# Support for unchecked (unsafe) non-wrapping integer arithmetic

module NoWrap

import Core.Intrinsics: box, unbox,
       unchecked_sneg_int, unchecked_sadd_int, unchecked_ssub_int,
       unchecked_smul_int, unchecked_sdiv_int, unchecked_srem_int,
       unchecked_uneg_int, unchecked_uadd_int, unchecked_usub_int,
       unchecked_umul_int, unchecked_udiv_int, unchecked_urem_int

typealias SignedInt Union{Int8,Int16,Int32,Int64,Int128}
typealias UnsignedInt Union{UInt8,UInt16,UInt32,UInt64,UInt128}

# LLVM has several code generation bugs for unchecked integer arithmetic (see
# e.g. #4905). We thus distinguish between operations that can be implemented
# via intrinsics, and operations for which we have to provide work-arounds.

# Note: As far as this code has been tested, most unchecked_* functions are
# working fine in LLVM. (Note that division is still handled via `base/int.jl`,
# which always checks for overflow, and which provides its own sets of
# work-arounds for LLVM codegen bugs.) However, the comments in `base/int.jl`
# and in issue #4905 are more pessimistic. For the time being, we thus retain
# the ability to handle codegen bugs in LLVM, until the code here has been
# tested on more systems and architectures. It also seems that things depend on
# which compiler that was used to build LLVM (i.e. either gcc or clang).

# These unions are used for most unchecked functions:
#     BrokenSignedInt
#     BrokenUnsignedInt
# These unions are used for unsafe_nowrap_{mul,div,rem}:
#     BrokenSignedIntMul
#     BrokenUnsignedIntMul

typealias BrokenSignedInt Union{}
typealias BrokenUnsignedInt Union{}
if WORD_SIZE == 32
    typealias BrokenSignedIntMul Int128
    typealias BrokenUnsignedIntMul UInt128
else
    typealias BrokenSignedIntMul Union{}
    typealias BrokenUnsignedIntMul Union{}
end
# Use these definitions to test the non-LLVM implementations
# typealias BrokenSignedInt SignedInt
# typealias BrokenUnsignedInt UnsignedInt
# typealias BrokenSignedIntMul SignedInt
# typealias BrokenUnsignedIntMul UnsignedInt

"""
    Base.unsafe_nowrap_neg(x)

Calculates `-x` without any overflow checking. It is the caller's
responsiblity to ensure that there is no overflow, and the compiler is free to
optimize the code assuming there is no overflow.
"""
function unsafe_nowrap_neg end

function unsafe_nowrap_neg{T<:SignedInt}(x::T)
    box(T, unchecked_sneg_int(unbox(T,x)))
end
function unsafe_nowrap_neg{T<:UnsignedInt}(x::T)
    box(T, unchecked_uneg_int(unbox(T,x)))
end
unsafe_nowrap_neg{T<:BrokenSignedInt}(x::T) = -x
unsafe_nowrap_neg{T<:BrokenUnsignedInt}(x::T) = T(0)

"""
    Base.unsafe_nowrap_abs(x)

Calculates `abs(x)` without any overflow checking. It is the caller's
responsiblity to ensure that there is no overflow, and the compiler is free to
optimize the code assuming there is no overflow.
"""
function unsafe_nowrap_abs end

unsafe_nowrap_abs(x::SignedInt) = ifelse(x<0, -x, x)
unsafe_nowrap_abs(x::UnsignedInt) = x

"""
    Base.unsafe_nowrap_add(x, y)

Calculates `x+y` without any overflow checking. It is the caller's
responsiblity to ensure that there is no overflow, and the compiler is free to
optimize the code assuming there is no overflow.
"""
function unsafe_nowrap_add end

unsafe_nowrap_add(x::Integer, y::Integer) = unsafe_nowrap_add(promote(x,y)...)
unsafe_nowrap_add{T<:Integer}(x::T, y::T) = x+y
function unsafe_nowrap_add{T<:SignedInt}(x::T, y::T)
    box(T, unchecked_sadd_int(unbox(T,x), unbox(T,y)))
end
function unsafe_nowrap_add{T<:UnsignedInt}(x::T, y::T)
    box(T, unchecked_uadd_int(unbox(T,x), unbox(T,y)))
end
unsafe_nowrap_add{T<:BrokenSignedInt}(x::T, y::T) = x+y
unsafe_nowrap_add{T<:BrokenUnsignedInt}(x::T, y::T) = x+y

# Handle multiple arguments
unsafe_nowrap_add(x) = x
unsafe_nowrap_add{T}(x1::T, x2::T, x3::T) =
    unsafe_nowrap_add(unsafe_nowrap_add(x1, x2), x3)
unsafe_nowrap_add{T}(x1::T, x2::T, x3::T, x4::T) =
    unsafe_nowrap_add(unsafe_nowrap_add(x1, x2), x3, x4)
unsafe_nowrap_add{T}(x1::T, x2::T, x3::T, x4::T, x5::T) =
    unsafe_nowrap_add(unsafe_nowrap_add(x1, x2), x3, x4, x5)
unsafe_nowrap_add{T}(x1::T, x2::T, x3::T, x4::T, x5::T, x6::T) =
    unsafe_nowrap_add(unsafe_nowrap_add(x1, x2), x3, x4, x5, x6)
unsafe_nowrap_add{T}(x1::T, x2::T, x3::T, x4::T, x5::T, x6::T, x7::T) =
    unsafe_nowrap_add(unsafe_nowrap_add(x1, x2), x3, x4, x5, x6, x7)
unsafe_nowrap_add{T}(x1::T, x2::T, x3::T, x4::T, x5::T, x6::T, x7::T, x8::T) =
    unsafe_nowrap_add(unsafe_nowrap_add(x1, x2), x3, x4, x5, x6, x7, x8)

"""
    Base.unsafe_nowrap_sub(x, y)

Calculates `x-y` without any overflow checking. It is the caller's
responsiblity to ensure that there is no overflow, and the compiler is free to
optimize the code assuming there is no overflow.
"""
function unsafe_nowrap_sub end

unsafe_nowrap_sub(x::Integer, y::Integer) = unsafe_nowrap_sub(promote(x,y)...)
unsafe_nowrap_sub{T<:Integer}(x::T, y::T) = x-y
function unsafe_nowrap_sub{T<:SignedInt}(x::T, y::T)
    box(T, unchecked_ssub_int(unbox(T,x), unbox(T,y)))
end
function unsafe_nowrap_sub{T<:UnsignedInt}(x::T, y::T)
    box(T, unchecked_usub_int(unbox(T,x), unbox(T,y)))
end
unsafe_nowrap_sub{T<:BrokenSignedInt}(x::T, y::T) = x-y
unsafe_nowrap_sub{T<:BrokenUnsignedInt}(x::T, y::T) = x-y

"""
    Base.unsafe_nowrap_mul(x, y)

Calculates `x*y` without any overflow checking. It is the caller's
responsiblity to ensure that there is no overflow, and the compiler is free to
optimize the code assuming there is no overflow.
"""
function unsafe_nowrap_mul end

unsafe_nowrap_mul(x::Integer, y::Integer) = unsafe_nowrap_mul(promote(x,y)...)
unsafe_nowrap_mul{T<:Integer}(x::T, y::T) = x*y
function unsafe_nowrap_mul{T<:SignedInt}(x::T, y::T)
    box(T, unchecked_smul_int(unbox(T,x), unbox(T,y)))
end
function unsafe_nowrap_mul{T<:UnsignedInt}(x::T, y::T)
    box(T, unchecked_umul_int(unbox(T,x), unbox(T,y)))
end
unsafe_nowrap_mul{T<:BrokenSignedIntMul}(x::T, y::T) = x*y
unsafe_nowrap_mul{T<:BrokenUnsignedIntMul}(x::T, y::T) = x*y

# Handle multiple arguments
unsafe_nowrap_mul(x) = x
unsafe_nowrap_mul{T}(x1::T, x2::T, x3::T) =
    unsafe_nowrap_mul(unsafe_nowrap_mul(x1, x2), x3)
unsafe_nowrap_mul{T}(x1::T, x2::T, x3::T, x4::T) =
    unsafe_nowrap_mul(unsafe_nowrap_mul(x1, x2), x3, x4)
unsafe_nowrap_mul{T}(x1::T, x2::T, x3::T, x4::T, x5::T) =
    unsafe_nowrap_mul(unsafe_nowrap_mul(x1, x2), x3, x4, x5)
unsafe_nowrap_mul{T}(x1::T, x2::T, x3::T, x4::T, x5::T, x6::T) =
    unsafe_nowrap_mul(unsafe_nowrap_mul(x1, x2), x3, x4, x5, x6)
unsafe_nowrap_mul{T}(x1::T, x2::T, x3::T, x4::T, x5::T, x6::T, x7::T) =
    unsafe_nowrap_mul(unsafe_nowrap_mul(x1, x2), x3, x4, x5, x6, x7)
unsafe_nowrap_mul{T}(x1::T, x2::T, x3::T, x4::T, x5::T, x6::T, x7::T, x8::T) =
    unsafe_nowrap_mul(unsafe_nowrap_mul(x1, x2), x3, x4, x5, x6, x7, x8)

"""
    Base.unsafe_nowrap_div(x, y)

Calculates `div(x,y)` without any overflow checking. It is the caller's
responsiblity to ensure that there is no overflow, and the compiler is free to
optimize the code assuming there is no overflow.
"""
function unsafe_nowrap_div end

unsafe_nowrap_div(x::Integer, y::Integer) = unsafe_nowrap_div(promote(x,y)...)
unsafe_nowrap_div{T<:Integer}(x::T, y::T) = div(x, y)
function unsafe_nowrap_div{T<:SignedInt}(x::T, y::T)
    box(T, unchecked_sdiv_int(unbox(T,x), unbox(T,y)))
end
function unsafe_nowrap_div{T<:UnsignedInt}(x::T, y::T)
    box(T, unchecked_udiv_int(unbox(T,x), unbox(T,y)))
end
unsafe_nowrap_div{T<:BrokenSignedIntMul}(x::T, y::T) = div(x, y)
unsafe_nowrap_div{T<:BrokenUnsignedIntMul}(x::T, y::T) = div(x, y)

"""
    Base.unsafe_nowrap_rem(x, y)

Calculates `rem(x,y)` without any overflow checking. It is the caller's
responsiblity to ensure that there is no overflow, and the compiler is free to
optimize the code assuming there is no overflow.
"""
function unsafe_nowrap_rem end

unsafe_nowrap_rem(x::Integer, y::Integer) = unsafe_nowrap_rem(promote(x,y)...)
unsafe_nowrap_rem{T<:Integer}(x::T, y::T) = rem(x, y)
function unsafe_nowrap_rem{T<:SignedInt}(x::T, y::T)
    y == -1 && return T(0)   # avoid overflow
    box(T, unchecked_srem_int(unbox(T,x), unbox(T,y)))
end
function unsafe_nowrap_rem{T<:UnsignedInt}(x::T, y::T)
    box(T, unchecked_urem_int(unbox(T,x), unbox(T,y)))
end
unsafe_nowrap_rem{T<:BrokenSignedIntMul}(x::T, y::T) = rem(x, y)
unsafe_nowrap_rem{T<:BrokenUnsignedIntMul}(x::T, y::T) = rem(x, y)

"""
    Base.unsafe_nowrap_fld(x, y)

Calculates `fld(x,y)` without any overflow checking. It is the caller's
responsiblity to ensure that there is no overflow, and the compiler is free to
optimize the code assuming there is no overflow.
"""
function unsafe_nowrap_fld end

unsafe_nowrap_fld(x::Integer, y::Integer) = unsafe_nowrap_fld(promote(x,y)...)
unsafe_nowrap_fld{T<:Integer}(x::T, y::T) = fld(x, y)
function unsafe_nowrap_fld{T<:SignedInt}(x::T, y::T)
    d = unsafe_nowrap_div(x,y)
    d - (((x<0) != (y<0)) & (d*y!=x))
end
unsafe_nowrap_fld{T<:UnsignedInt}(x::T, y::T) = unsafe_nowrap_div(x, y)

"""
    Base.unsafe_nowrap_mod(x, y)

Calculates `mod(x,y)` without any overflow checking. It is the caller's
responsiblity to ensure that there is no overflow, and the compiler is free to
optimize the code assuming there is no overflow.
"""
function unsafe_nowrap_mod end

unsafe_nowrap_mod(x::Integer, y::Integer) = unsafe_nowrap_mod(promote(x,y)...)
unsafe_nowrap_mod{T<:Integer}(x::T, y::T) = mod(x, y)
function unsafe_nowrap_mod{T<:SignedInt}(x::T, y::T)
    y == -1 && return T(0)   # avoid potential overflow in fld
    x - unsafe_nowrap_fld(x,y)*y
end
unsafe_nowrap_mod{T<:UnsignedInt}(x::T, y::T) = unsafe_nowrap_rem(x, y)

"""
    Base.unsafe_nowrap_cld(x, y)

Calculates `cld(x,y)` without any overflow checking. It is the caller's
responsiblity to ensure that there is no overflow, and the compiler is free to
optimize the code assuming there is no overflow.
"""
function unsafe_nowrap_cld end

unsafe_nowrap_cld(x::Integer, y::Integer) = unsafe_nowrap_cld(promote(x,y)...)
unsafe_nowrap_cld{T<:Integer}(x::T, y::T) = cld(x, y)
function unsafe_nowrap_cld{T<:SignedInt}(x::T, y::T)
    d = unsafe_nowrap_div(x,y)
    d + (((x>0) == (y>0)) & (d*y!=x))
end
function unsafe_nowrap_cld{T<:UnsignedInt}(x::T, y::T)
    d = unsafe_nowrap_div(x,y)
    d + (d*y!=x)
end

end

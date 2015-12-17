# This file is a part of Julia. License is MIT: http://julialang.org/license

# Support for checked integer arithmetic

module Checked

import Base: checked_neg, checked_abs, checked_add, checked_sub, checked_mul,
       checked_div, checked_rem, checked_fld, checked_mod, checked_cld
export checked_neg, checked_abs, checked_add, checked_sub, checked_mul,
       checked_div, checked_rem, checked_fld, checked_mod, checked_cld

import Core.Intrinsics: box, unbox,
       checked_sadd_int, checked_ssub_int, checked_smul_int, checked_sdiv_int,
       checked_srem_int,
       checked_uadd_int, checked_usub_int, checked_umul_int, checked_udiv_int,
       checked_urem_int

typealias SignedInt Union{Int8,Int16,Int32,Int64,Int128}
typealias UnsignedInt Union{UInt8,UInt16,UInt32,UInt64,UInt128}

# LLVM has several code generation bugs for checked integer arithmetic (see e.g.
# #4905). We thus distinguish between operations that can be implemented via
# intrinsics, and operations for which we have to provide work-arounds.

# Note: As far as this code has been tested, most checked_* functions are
# working fine in LLVM. (Note that division is still handled via `base/int.jl`,
# which always checks for overflow, and which provides its own sets of
# work-arounds for LLVM codegen bugs.) However, the comments in `base/int.jl`
# and in issue #4905 are more pessimistic. For the time being, we thus retain
# the ability to handle codegen bugs in LLVM, until the code here has been
# tested on more systems and architectures. It also seems that things depend on
# which compiler that was used to build LLVM (i.e. either gcc or clang).

# These unions are used for most checked functions:
#     BrokenSignedInt
#     BrokenUnsignedInt
# These unions are used for checked_{mul,div,rem}:
#     BrokenSignedIntMul
#     BrokenUnsignedIntMul

# This code runs early during bootstrap, and we can't use Julia's version
# strings yet
const llvm_version = Int(ccall(:jl_get_LLVM_VERSION, UInt32, ()))

brokenSignedInt = Union{}
brokenUnsignedInt = Union{}
brokenSignedIntMul = Int128
brokenUnsignedIntMul = UInt128
if WORD_SIZE == 32
    brokenSignedIntMul = Union{brokenSignedIntMul, Int64}
    brokenUnsignedIntMul = Union{brokenUnsignedIntMul, UInt64}
end
if llvm_version < 30500
    brokenSignedIntMul = Union{brokenSignedIntMul, Int8}
    brokenUnsignedIntMul = Union{brokenUnsignedIntMul, UInt8}
end
typealias BrokenSignedInt brokenSignedInt
typealias BrokenUnsignedInt brokenUnsignedInt
typealias BrokenSignedIntMul brokenSignedIntMul
typealias BrokenUnsignedIntMul brokenUnsignedIntMul
# Use these definitions to test the non-LLVM implementations
# typealias BrokenSignedInt SignedInt
# typealias BrokenUnsignedInt UnsignedInt
# typealias BrokenSignedIntMul SignedInt
# typealias BrokenUnsignedIntMul UnsignedInt

"""
    Base.checked_neg(x)

Calculates `-x`, checking for overflow errors where applicable. For
example, standard two's complement signed integers (e.g. `Int`) cannot
represent `-typemin(Int)`, thus leading to an overflow.

The overflow protection may impose a perceptible performance penalty.
"""
function checked_neg end

function checked_neg{T<:SignedInt}(x::T)
    checked_sub(T(0), x)
end
function checked_neg{T<:UnsignedInt}(x::T)
    checked_sub(T(0), x)
end
function checked_neg{T<:BrokenSignedInt}(x::T)
    r = -x
    (x<0) & (r<0) && throw(OverflowError())
    r
end
function checked_neg{T<:BrokenUnsignedInt}(x::T)
    x != 0 && throw(OverflowError())
    T(0)
end

"""
    Base.checked_abs(x)

Calculates `abs(x)`, checking for overflow errors where applicable.
For example, standard two's complement signed integers (e.g. `Int`)
cannot represent `abs(typemin(Int))`, thus leading to an overflow.

The overflow protection may impose a perceptible performance penalty.
"""
function checked_abs end

function checked_abs(x::SignedInt)
    r = ifelse(x<0, -x, x)
    r<0 && throw(OverflowError())
    r
 end
checked_abs(x::UnsignedInt) = x

"""
    Base.checked_add(x, y)

Calculates `x+y`, checking for overflow errors where applicable.

The overflow protection may impose a perceptible performance penalty.
"""
function checked_add end

function checked_add{T<:SignedInt}(x::T, y::T)
    box(T, checked_sadd_int(unbox(T,x), unbox(T,y)))
end
function checked_add{T<:UnsignedInt}(x::T, y::T)
    box(T, checked_uadd_int(unbox(T,x), unbox(T,y)))
end
function checked_add{T<:BrokenSignedInt}(x::T, y::T)
    r = x + y
    # x and y have the same sign, and the result has a different sign
    (x<0) == (y<0) != (r<0) && throw(OverflowError())
    r
end
function checked_add{T<:BrokenUnsignedInt}(x::T, y::T)
    # x + y > typemax(T)
    # Note: ~y == -y-1
    x > ~y && throw(OverflowError())
    x + y
end

# Handle multiple arguments
checked_add(x) = x
checked_add{T}(x1::T, x2::T, x3::T) =
    checked_add(checked_add(x1, x2), x3)
checked_add{T}(x1::T, x2::T, x3::T, x4::T) =
    checked_add(checked_add(x1, x2), x3, x4)
checked_add{T}(x1::T, x2::T, x3::T, x4::T, x5::T) =
    checked_add(checked_add(x1, x2), x3, x4, x5)
checked_add{T}(x1::T, x2::T, x3::T, x4::T, x5::T, x6::T) =
    checked_add(checked_add(x1, x2), x3, x4, x5, x6)
checked_add{T}(x1::T, x2::T, x3::T, x4::T, x5::T, x6::T, x7::T) =
    checked_add(checked_add(x1, x2), x3, x4, x5, x6, x7)
checked_add{T}(x1::T, x2::T, x3::T, x4::T, x5::T, x6::T, x7::T, x8::T) =
    checked_add(checked_add(x1, x2), x3, x4, x5, x6, x7, x8)

"""
    Base.checked_sub(x, y)

Calculates `x-y`, checking for overflow errors where applicable.

The overflow protection may impose a perceptible performance penalty.
"""
function checked_sub end

function checked_sub{T<:SignedInt}(x::T, y::T)
    box(T, checked_ssub_int(unbox(T,x), unbox(T,y)))
end
function checked_sub{T<:UnsignedInt}(x::T, y::T)
    box(T, checked_usub_int(unbox(T,x), unbox(T,y)))
end
function checked_sub{T<:BrokenSignedInt}(x::T, y::T)
    r = x - y
    # x and y have different signs, and the result has a different sign than x
    (x<0) != (y<0) == (r<0) && throw(OverflowError())
    r
end
function checked_sub{T<:BrokenUnsignedInt}(x::T, y::T)
    # x - y < 0
    x < y && throw(OverflowError())
    x - y
end

"""
    Base.checked_mul(x, y)

Calculates `x*y`, checking for overflow errors where applicable.

The overflow protection may impose a perceptible performance penalty.
"""
function checked_mul end

function checked_mul{T<:SignedInt}(x::T, y::T)
    box(T, checked_smul_int(unbox(T,x), unbox(T,y)))
end
function checked_mul{T<:UnsignedInt}(x::T, y::T)
    box(T, checked_umul_int(unbox(T,x), unbox(T,y)))
end
function checked_mul{T<:BrokenSignedIntMul}(x::T, y::T)
    r = widemul(x, y)
    r % T != r && throw(OverflowError())
    r % T
end
function checked_mul{T<:BrokenUnsignedIntMul}(x::T, y::T)
    r = widemul(x, y)
    r % T != r && throw(OverflowError())
    r % T
end
if Int128 <: BrokenSignedIntMul
    # Avoid BigInt
    function checked_mul{T<:Int128}(x::T, y::T)
        if y > 0
            # x * y > typemax(T)
            # x * y < typemin(T)
            x > fld(typemax(T), y) && throw(OverflowError())
            x < cld(typemin(T), y) && throw(OverflowError())
        elseif y < 0
            # x * y > typemax(T)
            # x * y < typemin(T)
            x < cld(typemax(T), y) && throw(OverflowError())
            # y == -1 can overflow fld
            y != -1 && x > fld(typemin(T), y) && throw(OverflowError())
        end
        x * y
    end
end
if UInt128 <: BrokenUnsignedIntMul
    # Avoid BigInt
    function checked_mul{T<:UInt128}(x::T, y::T)
        # x * y > typemax(T)
        y > 0 && x > fld(typemax(T), y) && throw(OverflowError())
        x * y
    end
end

# Handle multiple arguments
checked_mul(x) = x
checked_mul{T}(x1::T, x2::T, x3::T) =
    checked_mul(checked_mul(x1, x2), x3)
checked_mul{T}(x1::T, x2::T, x3::T, x4::T) =
    checked_mul(checked_mul(x1, x2), x3, x4)
checked_mul{T}(x1::T, x2::T, x3::T, x4::T, x5::T) =
    checked_mul(checked_mul(x1, x2), x3, x4, x5)
checked_mul{T}(x1::T, x2::T, x3::T, x4::T, x5::T, x6::T) =
    checked_mul(checked_mul(x1, x2), x3, x4, x5, x6)
checked_mul{T}(x1::T, x2::T, x3::T, x4::T, x5::T, x6::T, x7::T) =
    checked_mul(checked_mul(x1, x2), x3, x4, x5, x6, x7)
checked_mul{T}(x1::T, x2::T, x3::T, x4::T, x5::T, x6::T, x7::T, x8::T) =
    checked_mul(checked_mul(x1, x2), x3, x4, x5, x6, x7, x8)

"""
    Base.checked_div(x, y)

Calculates `div(x,y)`, checking for overflow errors where applicable.

The overflow protection may impose a perceptible performance penalty.
"""
function checked_div end

# Base.div already checks; nothing to do here
checked_div{T<:SignedInt}(x::T, y::T) = div(x, y)
checked_div{T<:UnsignedInt}(x::T, y::T) = div(x, y)

"""
    Base.checked_rem(x, y)

Calculates `x%y`, checking for overflow errors where applicable.

The overflow protection may impose a perceptible performance penalty.
"""
function checked_rem end

# Base.rem already checks; nothing to do here
checked_rem{T<:SignedInt}(x::T, y::T) = rem(x, y)
checked_rem{T<:UnsignedInt}(x::T, y::T) = rem(x, y)

"""
    Base.checked_fld(x, y)

Calculates `fld(x,y)`, checking for overflow errors where applicable.

The overflow protection may impose a perceptible performance penalty.
"""
function checked_fld end

# Base.fld already checks; nothing to do here
checked_fld{T<:SignedInt}(x::T, y::T) = fld(x, y)
checked_fld{T<:UnsignedInt}(x::T, y::T) = fld(x, y)

"""
    Base.checked_mod(x, y)

Calculates `mod(x,y)`, checking for overflow errors where applicable.

The overflow protection may impose a perceptible performance penalty.
"""
function checked_mod end

# Base.mod already checks; nothing to do here
checked_mod{T<:SignedInt}(x::T, y::T) = mod(x, y)
checked_mod{T<:UnsignedInt}(x::T, y::T) = mod(x, y)

"""
    Base.checked_cld(x, y)

Calculates `cld(x,y)`, checking for overflow errors where applicable.

The overflow protection may impose a perceptible performance penalty.
"""
function checked_cld end

# Base.cld already checks; nothing to do here
checked_cld{T<:SignedInt}(x::T, y::T) = cld(x, y)
checked_cld{T<:UnsignedInt}(x::T, y::T) = cld(x, y)

end

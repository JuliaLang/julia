# This file is a part of Julia. License is MIT: https://julialang.org/license

# Support for checked integer arithmetic

module Checked

export checked_neg, checked_abs, checked_add, checked_sub, checked_mul,
       checked_div, checked_rem, checked_fld, checked_mod, checked_cld,
       add_with_overflow, sub_with_overflow, mul_with_overflow

import Core.Intrinsics:
       checked_sadd_int, checked_ssub_int, checked_smul_int, checked_sdiv_int,
       checked_srem_int,
       checked_uadd_int, checked_usub_int, checked_umul_int, checked_udiv_int,
       checked_urem_int
import ..no_op_err, ..@_inline_meta, ..@_noinline_meta

# define promotion behavior for checked operations
checked_add(x::Integer, y::Integer) = checked_add(promote(x,y)...)
checked_sub(x::Integer, y::Integer) = checked_sub(promote(x,y)...)
checked_mul(x::Integer, y::Integer) = checked_mul(promote(x,y)...)
checked_div(x::Integer, y::Integer) = checked_div(promote(x,y)...)
checked_rem(x::Integer, y::Integer) = checked_rem(promote(x,y)...)
checked_fld(x::Integer, y::Integer) = checked_fld(promote(x,y)...)
checked_mod(x::Integer, y::Integer) = checked_mod(promote(x,y)...)
checked_cld(x::Integer, y::Integer) = checked_cld(promote(x,y)...)

# fallback catchall rules to prevent infinite recursion if promotion succeeds,
# but no method exists to handle those types
checked_abs(x::T) where {T<:Integer} = no_op_err("checked_abs", T)

const SignedInt = Union{Int8,Int16,Int32,Int64,Int128}
const UnsignedInt = Union{UInt8,UInt16,UInt32,UInt64,UInt128}

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
if Core.sizeof(Ptr{Cvoid}) == 4
    brokenSignedIntMul = Union{brokenSignedIntMul, Int64}
    brokenUnsignedIntMul = Union{brokenUnsignedIntMul, UInt64}
end
const BrokenSignedInt = brokenSignedInt
const BrokenUnsignedInt = brokenUnsignedInt
const BrokenSignedIntMul = brokenSignedIntMul
const BrokenUnsignedIntMul = brokenUnsignedIntMul
# Use these definitions to test the non-LLVM implementations
# const BrokenSignedInt = SignedInt
# const BrokenUnsignedInt = UnsignedInt
# const BrokenSignedIntMul = SignedInt
# const BrokenUnsignedIntMul = UnsignedInt

"""
    Base.checked_neg(x)

Calculates `-x`, checking for overflow errors where applicable. For
example, standard two's complement signed integers (e.g. `Int`) cannot
represent `-typemin(Int)`, thus leading to an overflow.

The overflow protection may impose a perceptible performance penalty.
"""
function checked_neg(x::T) where T<:Integer
    checked_sub(T(0), x)
end
throw_overflowerr_negation(x) = (@_noinline_meta;
    throw(OverflowError(Base.invokelatest(string, "checked arithmetic: cannot compute -x for x = ", x, "::", typeof(x)))))
if BrokenSignedInt != Union{}
function checked_neg(x::BrokenSignedInt)
    r = -x
    (x<0) & (r<0) && throw_overflowerr_negation(x)
    r
end
end
if BrokenUnsignedInt != Union{}
function checked_neg(x::T) where T<:BrokenUnsignedInt
    x != 0 && throw_overflowerr_negation(x)
    T(0)
end
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
    r<0 && throw(OverflowError(string("checked arithmetic: cannot compute |x| for x = ", x, "::", typeof(x))))
    r
 end
checked_abs(x::UnsignedInt) = x
checked_abs(x::Bool) = x



"""
    Base.add_with_overflow(x, y) -> (r, f)

Calculates `r = x+y`, with the flag `f` indicating whether overflow has occurred.
"""
function add_with_overflow end
add_with_overflow(x::T, y::T) where {T<:SignedInt}   = checked_sadd_int(x, y)
add_with_overflow(x::T, y::T) where {T<:UnsignedInt} = checked_uadd_int(x, y)
add_with_overflow(x::Bool, y::Bool) = (x+y, false)

if BrokenSignedInt != Union{}
function add_with_overflow(x::T, y::T) where T<:BrokenSignedInt
    r = x + y
    # x and y have the same sign, and the result has a different sign
    f = (x<0) == (y<0) != (r<0)
    r, f
end
end
if BrokenUnsignedInt != Union{}
function add_with_overflow(x::T, y::T) where T<:BrokenUnsignedInt
    # x + y > typemax(T)
    # Note: ~y == -y-1
    x + y, x > ~y
end
end


throw_overflowerr_binaryop(op, x, y) = (@_noinline_meta;
    throw(OverflowError(Base.invokelatest(string, x, " ", op, " ", y, " overflowed for type ", typeof(x)))))

"""
    Base.checked_add(x, y)

Calculates `x+y`, checking for overflow errors where applicable.

The overflow protection may impose a perceptible performance penalty.
"""
function checked_add(x::T, y::T) where T<:Integer
    @_inline_meta
    z, b = add_with_overflow(x, y)
    b && throw_overflowerr_binaryop(:+, x, y)
    z
end

# Handle multiple arguments
checked_add(x) = x
checked_add(x::Bool) = +x

checked_add(x1::T, x2::T, x3::T) where {T} =
    checked_add(checked_add(x1, x2), x3)
checked_add(x1::T, x2::T, x3::T, x4::T) where {T} =
    checked_add(checked_add(x1, x2), x3, x4)
checked_add(x1::T, x2::T, x3::T, x4::T, x5::T) where {T} =
    checked_add(checked_add(x1, x2), x3, x4, x5)
checked_add(x1::T, x2::T, x3::T, x4::T, x5::T, x6::T) where {T} =
    checked_add(checked_add(x1, x2), x3, x4, x5, x6)
checked_add(x1::T, x2::T, x3::T, x4::T, x5::T, x6::T, x7::T) where {T} =
    checked_add(checked_add(x1, x2), x3, x4, x5, x6, x7)
checked_add(x1::T, x2::T, x3::T, x4::T, x5::T, x6::T, x7::T, x8::T) where {T} =
    checked_add(checked_add(x1, x2), x3, x4, x5, x6, x7, x8)


"""
    Base.sub_with_overflow(x, y) -> (r, f)

Calculates `r = x-y`, with the flag `f` indicating whether overflow has occurred.
"""
function sub_with_overflow end
sub_with_overflow(x::T, y::T) where {T<:SignedInt}   = checked_ssub_int(x, y)
sub_with_overflow(x::T, y::T) where {T<:UnsignedInt} = checked_usub_int(x, y)
sub_with_overflow(x::Bool, y::Bool) = (x-y, false)

if BrokenSignedInt != Union{}
function sub_with_overflow(x::T, y::T) where T<:BrokenSignedInt
    r = x - y
    # x and y have different signs, and the result has a different sign than x
    f = (x<0) != (y<0) == (r<0)
    r, f
end
end
if BrokenUnsignedInt != Union{}
function sub_with_overflow(x::T, y::T) where T<:BrokenUnsignedInt
    # x - y < 0
    x - y, x < y
end
end

"""
    Base.checked_sub(x, y)

Calculates `x-y`, checking for overflow errors where applicable.

The overflow protection may impose a perceptible performance penalty.
"""
function checked_sub(x::T, y::T) where T<:Integer
    @_inline_meta
    z, b = sub_with_overflow(x, y)
    b && throw_overflowerr_binaryop(:-, x, y)
    z
end


"""
    Base.mul_with_overflow(x, y) -> (r, f)

Calculates `r = x*y`, with the flag `f` indicating whether overflow has occurred.
"""
function mul_with_overflow end
mul_with_overflow(x::T, y::T) where {T<:SignedInt}   = checked_smul_int(x, y)
mul_with_overflow(x::T, y::T) where {T<:UnsignedInt} = checked_umul_int(x, y)
mul_with_overflow(x::Bool, y::Bool) = (x*y, false)

if BrokenSignedIntMul != Union{} && BrokenSignedIntMul != Int128
function mul_with_overflow(x::T, y::T) where T<:BrokenSignedIntMul
    r = widemul(x, y)
    f = r % T != r
    r % T, f
end
end
if BrokenUnsignedIntMul != Union{} && BrokenUnsignedIntMul != UInt128
function mul_with_overflow(x::T, y::T) where T<:BrokenUnsignedIntMul
    r = widemul(x, y)
    f = r % T != r
    r % T, f
end
end
if Int128 <: BrokenSignedIntMul
    # Avoid BigInt
    function mul_with_overflow(x::T, y::T) where T<:Int128
        f = if y > 0
            # x * y > typemax(T)
            # x * y < typemin(T)
            x > fld(typemax(T), y) || x < cld(typemin(T), y)
        elseif y < 0
            # x * y > typemax(T)
            # x * y < typemin(T)
            # y == -1 can overflow fld
            x < cld(typemax(T), y) || y != -1 && x > fld(typemin(T), y)
        else
            false
        end
        x*y, f
    end
end
if UInt128 <: BrokenUnsignedIntMul
    # Avoid BigInt
    function mul_with_overflow(x::T, y::T) where T<:UInt128
        # x * y > typemax(T)
        x * y, y > 0 && x > fld(typemax(T), y)
    end
end

"""
    Base.checked_mul(x, y)

Calculates `x*y`, checking for overflow errors where applicable.

The overflow protection may impose a perceptible performance penalty.
"""
function checked_mul(x::T, y::T) where T<:Integer
    @_inline_meta
    z, b = mul_with_overflow(x, y)
    b && throw_overflowerr_binaryop(:*, x, y)
    z
end

# Handle multiple arguments
checked_mul(x) = x
checked_mul(x1::T, x2::T, x3::T) where {T} =
    checked_mul(checked_mul(x1, x2), x3)
checked_mul(x1::T, x2::T, x3::T, x4::T) where {T} =
    checked_mul(checked_mul(x1, x2), x3, x4)
checked_mul(x1::T, x2::T, x3::T, x4::T, x5::T) where {T} =
    checked_mul(checked_mul(x1, x2), x3, x4, x5)
checked_mul(x1::T, x2::T, x3::T, x4::T, x5::T, x6::T) where {T} =
    checked_mul(checked_mul(x1, x2), x3, x4, x5, x6)
checked_mul(x1::T, x2::T, x3::T, x4::T, x5::T, x6::T, x7::T) where {T} =
    checked_mul(checked_mul(x1, x2), x3, x4, x5, x6, x7)
checked_mul(x1::T, x2::T, x3::T, x4::T, x5::T, x6::T, x7::T, x8::T) where {T} =
    checked_mul(checked_mul(x1, x2), x3, x4, x5, x6, x7, x8)

"""
    Base.checked_div(x, y)

Calculates `div(x,y)`, checking for overflow errors where applicable.

The overflow protection may impose a perceptible performance penalty.
"""
checked_div(x::T, y::T) where {T<:Integer} = div(x, y) # Base.div already checks

"""
    Base.checked_rem(x, y)

Calculates `x%y`, checking for overflow errors where applicable.

The overflow protection may impose a perceptible performance penalty.
"""
checked_rem(x::T, y::T) where {T<:Integer} = rem(x, y) # Base.rem already checks

"""
    Base.checked_fld(x, y)

Calculates `fld(x,y)`, checking for overflow errors where applicable.

The overflow protection may impose a perceptible performance penalty.
"""
checked_fld(x::T, y::T) where {T<:Integer} = fld(x, y) # Base.fld already checks

"""
    Base.checked_mod(x, y)

Calculates `mod(x,y)`, checking for overflow errors where applicable.

The overflow protection may impose a perceptible performance penalty.
"""
checked_mod(x::T, y::T) where {T<:Integer} = mod(x, y) # Base.mod already checks

"""
    Base.checked_cld(x, y)

Calculates `cld(x,y)`, checking for overflow errors where applicable.

The overflow protection may impose a perceptible performance penalty.
"""
checked_cld(x::T, y::T) where {T<:Integer} = cld(x, y) # Base.cld already checks

end

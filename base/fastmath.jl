# This file is a part of Julia. License is MIT: https://julialang.org/license

# Support for @fastmath

# This module provides versions of math functions that may violate
# strict IEEE semantics.

# This allows the following transformations. For more information see
# http://llvm.org/docs/LangRef.html#fast-math-flags:
# nnan: No NaNs - Allow optimizations to assume the arguments and
#       result are not NaN. Such optimizations are required to retain
#       defined behavior over NaNs, but the value of the result is
#       undefined.
# ninf: No Infs - Allow optimizations to assume the arguments and
#       result are not +/-Inf. Such optimizations are required to
#       retain defined behavior over +/-Inf, but the value of the
#       result is undefined.
# nsz:  No Signed Zeros - Allow optimizations to treat the sign of a
#       zero argument or result as insignificant.
# arcp: Allow Reciprocal - Allow optimizations to use the reciprocal
#       of an argument rather than perform division.

module FastMath

export @fastmath

import Core.Intrinsics: sqrt_llvm, neg_float_fast,
    add_float_fast, sub_float_fast, mul_float_fast, div_float_fast, rem_float_fast,
    eq_float_fast, ne_float_fast, lt_float_fast, le_float_fast

const fast_op =
    Dict(# basic arithmetic
         :+ => :add_fast,
         :- => :sub_fast,
         :* => :mul_fast,
         :/ => :div_fast,
         :(==) => :eq_fast,
         :!= => :ne_fast,
         :< => :lt_fast,
         :<= => :le_fast,
         :abs => :abs_fast,
         :abs2 => :abs2_fast,
         :cmp => :cmp_fast,
         :conj => :conj_fast,
         :inv => :inv_fast,
         :rem => :rem_fast,
         :sign => :sign_fast,
         :isfinite => :isfinite_fast,
         :isinf => :isinf_fast,
         :isnan => :isnan_fast,
         :issubnormal => :issubnormal_fast,
         # math functions
         :^ => :pow_fast,
         :acos => :acos_fast,
         :acosh => :acosh_fast,
         :angle => :angle_fast,
         :asin => :asin_fast,
         :asinh => :asinh_fast,
         :atan => :atan_fast,
         :atan2 => :atan2_fast,
         :atanh => :atanh_fast,
         :cbrt => :cbrt_fast,
         :cis => :cis_fast,
         :cos => :cos_fast,
         :cosh => :cosh_fast,
         :exp10 => :exp10_fast,
         :exp2 => :exp2_fast,
         :exp => :exp_fast,
         :expm1 => :expm1_fast,
         :hypot => :hypot_fast,
         :lgamma => :lgamma_fast,
         :log10 => :log10_fast,
         :log1p => :log1p_fast,
         :log2 => :log2_fast,
         :log => :log_fast,
         :max => :max_fast,
         :min => :min_fast,
         :minmax => :minmax_fast,
         :sin => :sin_fast,
         :sincos => :sincos_fast,
         :sinh => :sinh_fast,
         :sqrt => :sqrt_fast,
         :tan => :tan_fast,
         :tanh => :tanh_fast)

const rewrite_op =
    Dict(:+= => :+,
         :-= => :-,
         :*= => :*,
         :/= => :/,
         :^= => :^)

function make_fastmath(expr::Expr)
    if expr.head === :quote
        return expr
    elseif expr.head == :call && expr.args[1] == :^ && expr.args[3] isa Integer
        # mimic Julia's literal_pow lowering of literal integer powers
        return Expr(:call, :(Base.FastMath.pow_fast), make_fastmath(expr.args[2]), Val{expr.args[3]}())
    end
    op = get(rewrite_op, expr.head, :nothing)
    if op !== :nothing
        var = expr.args[1]
        rhs = expr.args[2]
        if isa(var, Symbol)
            # simple assignment
            expr = :($var = $op($var, $rhs))
        elseif isa(var, Expr) && var.head === :ref
            # array reference
            arr = var.args[1]
            inds = tuple(var.args[2:end]...)
            arrvar = gensym()
            indvars = tuple([gensym() for i in inds]...)
            expr = quote
                $(Expr(:(=), arrvar, arr))
                $(Expr(:(=), Expr(:tuple, indvars...), Expr(:tuple, inds...)))
                $arrvar[$(indvars...)] = $op($arrvar[$(indvars...)], $rhs)
            end
        end
    end
    Expr(make_fastmath(expr.head), map(make_fastmath, expr.args)...)
end
function make_fastmath(symb::Symbol)
    fast_symb = get(fast_op, symb, :nothing)
    if fast_symb === :nothing
        return symb
    end
    :(Base.FastMath.$fast_symb)
end
make_fastmath(expr) = expr

"""
    @fastmath expr

Execute a transformed version of the expression, which calls functions that
may violate strict IEEE semantics. This allows the fastest possible operation,
but results are undefined -- be careful when doing this, as it may change numerical
results.

This sets the [LLVM Fast-Math flags](http://llvm.org/docs/LangRef.html#fast-math-flags),
and corresponds to the `-ffast-math` option in clang. See [the notes on performance
annotations](@ref man-performance-annotations) for more details.

# Examples
```jldoctest
julia> @fastmath 1+2
3

julia> @fastmath(sin(3))
0.1411200080598672
```
"""
macro fastmath(expr)
    make_fastmath(esc(expr))
end


# Basic arithmetic

const FloatTypes = Union{Float32,Float64}

sub_fast(x::FloatTypes) = neg_float_fast(x)

add_fast(x::T, y::T) where {T<:FloatTypes} = add_float_fast(x, y)
sub_fast(x::T, y::T) where {T<:FloatTypes} = sub_float_fast(x, y)
mul_fast(x::T, y::T) where {T<:FloatTypes} = mul_float_fast(x, y)
div_fast(x::T, y::T) where {T<:FloatTypes} = div_float_fast(x, y)
rem_fast(x::T, y::T) where {T<:FloatTypes} = rem_float_fast(x, y)

add_fast(x::T, y::T, zs::T...) where {T<:FloatTypes} =
    add_fast(add_fast(x, y), zs...)
mul_fast(x::T, y::T, zs::T...) where {T<:FloatTypes} =
    mul_fast(mul_fast(x, y), zs...)

@fastmath begin
    cmp_fast(x::T, y::T) where {T<:FloatTypes} = ifelse(x==y, 0, ifelse(x<y, -1, +1))
    log_fast(b::T, x::T) where {T<:FloatTypes} = log_fast(x)/log_fast(b)
end

eq_fast(x::T, y::T) where {T<:FloatTypes} = eq_float_fast(x, y)
ne_fast(x::T, y::T) where {T<:FloatTypes} = ne_float_fast(x, y)
lt_fast(x::T, y::T) where {T<:FloatTypes} = lt_float_fast(x, y)
le_fast(x::T, y::T) where {T<:FloatTypes} = le_float_fast(x, y)

isinf_fast(x) = false
isfinite_fast(x) = true
isnan_fast(x) = false
issubnormal_fast(x) = false

# complex numbers

ComplexTypes = Union{ComplexF32, ComplexF64}

@fastmath begin
    abs_fast(x::ComplexTypes) = hypot(real(x), imag(x))
    abs2_fast(x::ComplexTypes) = real(x)*real(x) + imag(x)*imag(x)
    conj_fast(x::T) where {T<:ComplexTypes} = T(real(x), -imag(x))
    inv_fast(x::ComplexTypes) = conj(x) / abs2(x)
    sign_fast(x::ComplexTypes) = x == 0 ? float(zero(x)) : x/abs(x)

    add_fast(x::T, y::T) where {T<:ComplexTypes} =
        T(real(x)+real(y), imag(x)+imag(y))
    add_fast(x::Complex{T}, b::T) where {T<:FloatTypes} =
        Complex{T}(real(x)+b, imag(x))
    add_fast(a::T, y::Complex{T}) where {T<:FloatTypes} =
        Complex{T}(a+real(y), imag(y))

    sub_fast(x::T, y::T) where {T<:ComplexTypes} =
        T(real(x)-real(y), imag(x)-imag(y))
    sub_fast(x::Complex{T}, b::T) where {T<:FloatTypes} =
        Complex{T}(real(x)-b, imag(x))
    sub_fast(a::T, y::Complex{T}) where {T<:FloatTypes} =
        Complex{T}(a-real(y), -imag(y))

    mul_fast(x::T, y::T) where {T<:ComplexTypes} =
        T(real(x)*real(y) - imag(x)*imag(y),
          real(x)*imag(y) + imag(x)*real(y))
    mul_fast(x::Complex{T}, b::T) where {T<:FloatTypes} =
        Complex{T}(real(x)*b, imag(x)*b)
    mul_fast(a::T, y::Complex{T}) where {T<:FloatTypes} =
        Complex{T}(a*real(y), a*imag(y))

    @inline div_fast(x::T, y::T) where {T<:ComplexTypes} =
        T(real(x)*real(y) + imag(x)*imag(y),
          imag(x)*real(y) - real(x)*imag(y)) / abs2(y)
    div_fast(x::Complex{T}, b::T) where {T<:FloatTypes} =
        Complex{T}(real(x)/b, imag(x)/b)
    div_fast(a::T, y::Complex{T}) where {T<:FloatTypes} =
        Complex{T}(a*real(y), -a*imag(y)) / abs2(y)

    eq_fast(x::T, y::T) where {T<:ComplexTypes} =
        (real(x)==real(y)) & (imag(x)==imag(y))
    eq_fast(x::Complex{T}, b::T) where {T<:FloatTypes} =
        (real(x)==b) & (imag(x)==T(0))
    eq_fast(a::T, y::Complex{T}) where {T<:FloatTypes} =
        (a==real(y)) & (T(0)==imag(y))

    ne_fast(x::T, y::T) where {T<:ComplexTypes} = !(x==y)
end

# fall-back implementations and type promotion

for op in (:abs, :abs2, :conj, :inv, :sign)
    op_fast = fast_op[op]
    @eval begin
        # fall-back implementation for non-numeric types
        $op_fast(xs...) = $op(xs...)
    end
end

for op in (:+, :-, :*, :/, :(==), :!=, :<, :<=, :cmp, :rem)
    op_fast = fast_op[op]
    @eval begin
        # fall-back implementation for non-numeric types
        $op_fast(xs...) = $op(xs...)
        # type promotion
        $op_fast(x::Number, y::Number, zs::Number...) =
            $op_fast(promote(x,y,zs...)...)
        # fall-back implementation that applies after promotion
        $op_fast(x::T,ys::T...) where {T<:Number} = $op(x,ys...)
    end
end


# Math functions

# builtins

pow_fast(x::Float32, y::Integer) = ccall("llvm.powi.f32", llvmcall, Float32, (Float32, Int32), x, y)
pow_fast(x::Float64, y::Integer) = ccall("llvm.powi.f64", llvmcall, Float64, (Float64, Int32), x, y)
pow_fast(x::FloatTypes, ::Val{p}) where {p} = pow_fast(x, p) # inlines already via llvm.powi
@inline pow_fast(x, v::Val) = Base.literal_pow(^, x, v)

sqrt_fast(x::FloatTypes) = sqrt_llvm(x)

# libm

const libm = Base.libm_name

for f in (:acosh, :asinh, :atanh, :cbrt, :cos,
          :cosh, :exp2, :expm1, :lgamma, :log10, :log1p, :log2,
          :log, :sin, :sinh, :tan, :tanh)
    f_fast = fast_op[f]
    @eval begin
        $f_fast(x::Float32) =
            ccall(($(string(f,"f")),libm), Float32, (Float32,), x)
        $f_fast(x::Float64) =
            ccall(($(string(f)),libm), Float64, (Float64,), x)
    end
end

pow_fast(x::Float32, y::Float32) =
    ccall(("powf",libm), Float32, (Float32,Float32), x, y)
pow_fast(x::Float64, y::Float64) =
    ccall(("pow",libm), Float64, (Float64,Float64), x, y)

atan2_fast(x::Float32, y::Float32) =
    ccall(("atan2f",libm), Float32, (Float32,Float32), x, y)
atan2_fast(x::Float64, y::Float64) =
    ccall(("atan2",libm), Float64, (Float64,Float64), x, y)

asin_fast(x::FloatTypes) = asin(x)
acos_fast(x::FloatTypes) = acos(x)

# explicit implementations

@inline function sincos_fast(v::Float64)
     s = Ref{Cdouble}()
     c = Ref{Cdouble}()
     ccall((:sincos, libm), Cvoid, (Cdouble, Ptr{Cdouble}, Ptr{Cdouble}), v, s, c)
     return (s[], c[])
end

@inline function sincos_fast(v::Float32)
     s = Ref{Cfloat}()
     c = Ref{Cfloat}()
     ccall((:sincosf, libm), Cvoid, (Cfloat, Ptr{Cfloat}, Ptr{Cfloat}), v, s, c)
     return (s[], c[])
end

@inline function sincos_fast(v::Float16)
    s, c = sincos_fast(Float32(v))
    return Float16(s), Float16(c)
end

sincos_fast(v::AbstractFloat) = (sin_fast(v), cos_fast(v))
sincos_fast(v::Real) = sincos_fast(float(v)::AbstractFloat)
sincos_fast(v) = (sin_fast(v), cos_fast(v))

@fastmath begin
    hypot_fast(x::T, y::T) where {T<:FloatTypes} = sqrt(x*x + y*y)

    # Note: we use the same comparison for min, max, and minmax, so
    # that the compiler can convert between them
    max_fast(x::T, y::T) where {T<:FloatTypes} = ifelse(y > x, y, x)
    min_fast(x::T, y::T) where {T<:FloatTypes} = ifelse(y > x, x, y)
    minmax_fast(x::T, y::T) where {T<:FloatTypes} = ifelse(y > x, (x,y), (y,x))

    # complex numbers

    function cis_fast(x::T) where {T<:FloatTypes}
        s, c = sincos_fast(x)
        Complex{T}(c, s)
    end

    # See <http://en.cppreference.com/w/cpp/numeric/complex>
    pow_fast(x::T, y::T) where {T<:ComplexTypes} = exp(y*log(x))
    pow_fast(x::T, y::Complex{T}) where {T<:FloatTypes} = exp(y*log(x))
    pow_fast(x::Complex{T}, y::T) where {T<:FloatTypes} = exp(y*log(x))
    acos_fast(x::T) where {T<:ComplexTypes} =
        convert(T,π)/2 + im*log(im*x + sqrt(1-x*x))
    acosh_fast(x::ComplexTypes) = log(x + sqrt(x+1) * sqrt(x-1))
    angle_fast(x::ComplexTypes) = atan2(imag(x), real(x))
    asin_fast(x::ComplexTypes) = -im*asinh(im*x)
    asinh_fast(x::ComplexTypes) = log(x + sqrt(1+x*x))
    atan_fast(x::ComplexTypes) = -im*atanh(im*x)
    atanh_fast(x::T) where {T<:ComplexTypes} = convert(T,1)/2*(log(1+x) - log(1-x))
    cis_fast(x::ComplexTypes) = exp(-imag(x)) * cis(real(x))
    cos_fast(x::ComplexTypes) = cosh(im*x)
    cosh_fast(x::T) where {T<:ComplexTypes} = convert(T,1)/2*(exp(x) + exp(-x))
    exp10_fast(x::T) where {T<:ComplexTypes} =
        exp10(real(x)) * cis(imag(x)*log(convert(T,10)))
    exp2_fast(x::T) where {T<:ComplexTypes} =
        exp2(real(x)) * cis(imag(x)*log(convert(T,2)))
    exp_fast(x::ComplexTypes) = exp(real(x)) * cis(imag(x))
    expm1_fast(x::ComplexTypes) = exp(x)-1
    log10_fast(x::T) where {T<:ComplexTypes} = log(x) / log(convert(T,10))
    log1p_fast(x::ComplexTypes) = log(1+x)
    log2_fast(x::T) where {T<:ComplexTypes} = log(x) / log(convert(T,2))
    log_fast(x::T) where {T<:ComplexTypes} = T(log(abs2(x))/2, angle(x))
    log_fast(b::T, x::T) where {T<:ComplexTypes} = T(log(x)/log(b))
    sin_fast(x::ComplexTypes) = -im*sinh(im*x)
    sinh_fast(x::T) where {T<:ComplexTypes} = convert(T,1)/2*(exp(x) - exp(-x))
    sqrt_fast(x::ComplexTypes) = sqrt(abs(x)) * cis(angle(x)/2)
    tan_fast(x::ComplexTypes) = -im*tanh(im*x)
    tanh_fast(x::ComplexTypes) = (a=exp(x); b=exp(-x); (a-b)/(a+b))
end

# fall-back implementations and type promotion

for f in (:acos, :acosh, :angle, :asin, :asinh, :atan, :atanh, :cbrt,
          :cis, :cos, :cosh, :exp10, :exp2, :exp, :expm1, :lgamma,
          :log10, :log1p, :log2, :log, :sin, :sinh, :sqrt, :tan,
          :tanh)
    f_fast = fast_op[f]
    @eval begin
        $f_fast(x) = $f(x)
    end
end

for f in (:^, :atan2, :hypot, :max, :min, :minmax, :log)
    f_fast = fast_op[f]
    @eval begin
        # fall-back implementation for non-numeric types
        $f_fast(x, y) = $f(x, y)
        # type promotion
        $f_fast(x::Number, y::Number) = $f_fast(promote(x, y)...)
        # fall-back implementation that applies after promotion
        $f_fast(x::T, y::T) where {T<:Number} = $f(x, y)
    end
end

end

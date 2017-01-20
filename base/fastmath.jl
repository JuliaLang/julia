# This file is a part of Julia. License is MIT: http://julialang.org/license

# Support for @fastmath

# This module provides versions of math functions that may violate
# strict IEEE semantics.

# This allows the following transformations:
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

import Core.Intrinsics: powi_llvm, sqrt_llvm_fast, neg_float_fast,
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
         :mod => :mod_fast,
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

macro fastmath(expr)
    make_fastmath(esc(expr))
end


# Basic arithmetic

FloatTypes = Union{Float32, Float64}

sub_fast{T<:FloatTypes}(x::T) = neg_float_fast(x)

add_fast{T<:FloatTypes}(x::T, y::T) = add_float_fast(x, y)
sub_fast{T<:FloatTypes}(x::T, y::T) = sub_float_fast(x, y)
mul_fast{T<:FloatTypes}(x::T, y::T) = mul_float_fast(x, y)
div_fast{T<:FloatTypes}(x::T, y::T) = div_float_fast(x, y)
rem_fast{T<:FloatTypes}(x::T, y::T) = rem_float_fast(x, y)

add_fast{T<:FloatTypes}(x::T, y::T, zs::T...) =
    add_fast(add_fast(x, y), zs...)
mul_fast{T<:FloatTypes}(x::T, y::T, zs::T...) =
    mul_fast(mul_fast(x, y), zs...)

@fastmath begin
    cmp_fast{T<:FloatTypes}(x::T, y::T) = ifelse(x==y, 0, ifelse(x<y, -1, +1))
    function mod_fast{T<:FloatTypes}(x::T, y::T)
        r = rem(x,y)
        ifelse((r > 0) ⊻ (y > 0), r+y, r)
    end
end

eq_fast{T<:FloatTypes}(x::T, y::T) = eq_float_fast(x, y)
ne_fast{T<:FloatTypes}(x::T, y::T) = ne_float_fast(x, y)
lt_fast{T<:FloatTypes}(x::T, y::T) = lt_float_fast(x, y)
le_fast{T<:FloatTypes}(x::T, y::T) = le_float_fast(x, y)

isinf_fast(x) = false
isfinite_fast(x) = true
isnan_fast(x) = false
issubnormal_fast(x) = false

# complex numbers

ComplexTypes = Union{Complex64, Complex128}

@fastmath begin
    abs_fast{T<:ComplexTypes}(x::T) = hypot(real(x), imag(x))
    abs2_fast{T<:ComplexTypes}(x::T) = real(x)*real(x) + imag(x)*imag(x)
    conj_fast{T<:ComplexTypes}(x::T) = T(real(x), -imag(x))
    inv_fast{T<:ComplexTypes}(x::T) = conj(x) / abs2(x)
    sign_fast{T<:ComplexTypes}(x::T) = x == 0 ? float(zero(x)) : x/abs(x)

    add_fast{T<:ComplexTypes}(x::T, y::T) =
        T(real(x)+real(y), imag(x)+imag(y))
    add_fast{T<:FloatTypes}(x::Complex{T}, b::T) =
        Complex{T}(real(x)+b, imag(x))
    add_fast{T<:FloatTypes}(a::T, y::Complex{T}) =
        Complex{T}(a+real(y), imag(y))

    sub_fast{T<:ComplexTypes}(x::T, y::T) =
        T(real(x)-real(y), imag(x)-imag(y))
    sub_fast{T<:FloatTypes}(x::Complex{T}, b::T) =
        Complex{T}(real(x)-b, imag(x))
    sub_fast{T<:FloatTypes}(a::T, y::Complex{T}) =
        Complex{T}(a-real(y), -imag(y))

    mul_fast{T<:ComplexTypes}(x::T, y::T) =
        T(real(x)*real(y) - imag(x)*imag(y),
          real(x)*imag(y) + imag(x)*real(y))
    mul_fast{T<:FloatTypes}(x::Complex{T}, b::T) =
        Complex{T}(real(x)*b, imag(x)*b)
    mul_fast{T<:FloatTypes}(a::T, y::Complex{T}) =
        Complex{T}(a*real(y), a*imag(y))

    @inline div_fast{T<:ComplexTypes}(x::T, y::T) =
        T(real(x)*real(y) + imag(x)*imag(y),
          imag(x)*real(y) - real(x)*imag(y)) / abs2(y)
    div_fast{T<:FloatTypes}(x::Complex{T}, b::T) =
        Complex{T}(real(x)/b, imag(x)/b)
    div_fast{T<:FloatTypes}(a::T, y::Complex{T}) =
        Complex{T}(a*real(y), -a*imag(y)) / abs2(y)

    eq_fast{T<:ComplexTypes}(x::T, y::T) =
        (real(x)==real(y)) & (imag(x)==imag(y))
    eq_fast{T<:FloatTypes}(x::Complex{T}, b::T) =
        (real(x)==b) & (imag(x)==T(0))
    eq_fast{T<:FloatTypes}(a::T, y::Complex{T}) =
        (a==real(y)) & (T(0)==imag(y))

    ne_fast{T<:ComplexTypes}(x::T, y::T) = !(x==y)
end

# fall-back implementations and type promotion

for op in (:abs, :abs2, :conj, :inv, :sign)
    op_fast = fast_op[op]
    @eval begin
        # fall-back implementation for non-numeric types
        $op_fast(xs...) = $op(xs...)
    end
end

for op in (:+, :-, :*, :/, :(==), :!=, :<, :<=, :cmp, :mod, :rem)
    op_fast = fast_op[op]
    @eval begin
        # fall-back implementation for non-numeric types
        $op_fast(xs...) = $op(xs...)
        # type promotion
        $op_fast(x::Number, y::Number, zs::Number...) =
            $op_fast(promote(x,y,zs...)...)
        # fall-back implementation that applies after promotion
        $op_fast{T<:Number}(x::T,ys::T...) = $op(x,ys...)
    end
end


# Math functions

# builtins

pow_fast{T<:FloatTypes}(x::T, y::Integer) = pow_fast(x, Int32(y))
pow_fast{T<:FloatTypes}(x::T, y::Int32) = Base.powi_llvm(x, y)

# TODO: Change sqrt_llvm intrinsic to avoid nan checking; add nan
# checking to sqrt in math.jl; remove sqrt_llvm_fast intrinsic
sqrt_fast{T<:FloatTypes}(x::T) = sqrt_llvm_fast(x)

# libm

const libm = Base.libm_name

for f in (:acos, :acosh, :asin, :asinh, :atan, :atanh, :cbrt, :cos,
          :cosh, :exp2, :exp, :expm1, :lgamma, :log10, :log1p, :log2,
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

# explicit implementations

@fastmath begin
    exp10_fast{T<:FloatTypes}(x::T) = exp2(log2(T(10))*x)
    exp10_fast(x::Integer) = exp10(float(x))

    hypot_fast{T<:FloatTypes}(x::T, y::T) = sqrt(x*x + y*y)

    # Note: we use the same comparison for min, max, and minmax, so
    # that the compiler can convert between them
    max_fast{T<:FloatTypes}(x::T, y::T) = ifelse(y > x, y, x)
    min_fast{T<:FloatTypes}(x::T, y::T) = ifelse(y > x, x, y)
    minmax_fast{T<:FloatTypes}(x::T, y::T) = ifelse(y > x, (x,y), (y,x))

    # complex numbers

    cis_fast{T<:FloatTypes}(x::T) = Complex{T}(cos(x), sin(x))

    # See <http://en.cppreference.com/w/cpp/numeric/complex>
    pow_fast{T<:ComplexTypes}(x::T, y::T) = exp(y*log(x))
    pow_fast{T<:FloatTypes}(x::T, y::Complex{T}) = exp(y*log(x))
    pow_fast{T<:FloatTypes}(x::Complex{T}, y::T) = exp(y*log(x))
    acos_fast{T<:ComplexTypes}(x::T) =
        convert(T,π)/2 + im*log(im*x + sqrt(1-x*x))
    acosh_fast{T<:ComplexTypes}(x::T) = log(x + sqrt(x+1) * sqrt(x-1))
    angle_fast{T<:ComplexTypes}(x::T) = atan2(imag(x), real(x))
    asin_fast{T<:ComplexTypes}(x::T) = -im*asinh(im*x)
    asinh_fast{T<:ComplexTypes}(x::T) = log(x + sqrt(1+x*x))
    atan_fast{T<:ComplexTypes}(x::T) = -im*atanh(im*x)
    atanh_fast{T<:ComplexTypes}(x::T) = convert(T,1)/2*(log(1+x) - log(1-x))
    cis_fast{T<:ComplexTypes}(x::T) = exp(-imag(x)) * cis(real(x))
    cos_fast{T<:ComplexTypes}(x::T) = cosh(im*x)
    cosh_fast{T<:ComplexTypes}(x::T) = convert(T,1)/2*(exp(x) + exp(-x))
    exp10_fast{T<:ComplexTypes}(x::T) =
        exp10(real(x)) * cis(imag(x)*log(convert(T,10)))
    exp2_fast{T<:ComplexTypes}(x::T) =
        exp2(real(x)) * cis(imag(x)*log(convert(T,2)))
    exp_fast{T<:ComplexTypes}(x::T) = exp(real(x)) * cis(imag(x))
    expm1_fast{T<:ComplexTypes}(x::T) = exp(x)-1
    log10_fast{T<:ComplexTypes}(x::T) = log(x) / log(convert(T,10))
    log1p_fast{T<:ComplexTypes}(x::T) = log(1+x)
    log2_fast{T<:ComplexTypes}(x::T) = log(x) / log(convert(T,2))
    log_fast{T<:ComplexTypes}(x::T) = T(log(abs2(x))/2, angle(x))
    sin_fast{T<:ComplexTypes}(x::T) = -im*sinh(im*x)
    sinh_fast{T<:ComplexTypes}(x::T) = convert(T,1)/2*(exp(x) - exp(-x))
    sqrt_fast{T<:ComplexTypes}(x::T) = sqrt(abs(x)) * cis(angle(x)/2)
    tan_fast{T<:ComplexTypes}(x::T) = -im*tanh(im*x)
    tanh_fast{T<:ComplexTypes}(x::T) = (a=exp(x); b=exp(-x); (a-b)/(a+b))
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

for f in (:^, :atan2, :hypot, :max, :min, :minmax)
    f_fast = fast_op[f]
    @eval begin
        # fall-back implementation for non-numeric types
        $f_fast(x, y) = $f(x, y)
        # type promotion
        $f_fast(x::Number, y::Number) = $f_fast(promote(x, y)...)
        # fall-back implementation that applies after promotion
        $f_fast{T<:Number}(x::T, y::T) = $f(x, y)
    end
end

end

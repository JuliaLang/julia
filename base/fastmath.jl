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

import Core.Intrinsics: box, unbox, powi_llvm, sqrt_llvm_fast

# generic fallback
fastmath(x) = x

fastmath(::typeof(+)) = add_fast
fastmath(::typeof(-)) = sub_fast
fastmath(::typeof(*)) = mul_fast
fastmath(::typeof(/)) = div_fast
fastmath(::typeof(==)) = eq_fast
fastmath(::typeof(!=)) = ne_fast
fastmath(::typeof(<)) = lt_fast
fastmath(::typeof(<=)) = le_fast
fastmath(::typeof(abs)) = abs_fast
fastmath(::typeof(abs2)) = abs2_fast
fastmath(::typeof(cmp)) = cmp_fast
fastmath(::typeof(conj)) = conj_fast
fastmath(::typeof(inv)) = inv_fast
fastmath(::typeof(mod)) = mod_fast
fastmath(::typeof(rem)) = rem_fast
fastmath(::typeof(sign)) = sign_fast
fastmath(::typeof(isfinite)) = isfinite_fast
fastmath(::typeof(isinf)) = isinf_fast
fastmath(::typeof(isnan)) = isnan_fast
fastmath(::typeof(issubnormal)) = issubnormal_fast
fastmath(::typeof(^)) = pow_fast
fastmath(::typeof(acos)) = acos_fast
fastmath(::typeof(acosh)) = acosh_fast
fastmath(::typeof(angle)) = angle_fast
fastmath(::typeof(asin)) = asin_fast
fastmath(::typeof(asinh)) = asinh_fast
fastmath(::typeof(atan)) = atan_fast
fastmath(::typeof(atan2)) = atan2_fast
fastmath(::typeof(atanh)) = atanh_fast
fastmath(::typeof(cbrt)) = cbrt_fast
fastmath(::typeof(cis)) = cis_fast
fastmath(::typeof(cos)) = cos_fast
fastmath(::typeof(cosh)) = cosh_fast
fastmath(::typeof(exp10)) = exp10_fast
fastmath(::typeof(exp2)) = exp2_fast
fastmath(::typeof(exp)) = exp_fast
fastmath(::typeof(expm1)) = expm1_fast
fastmath(::typeof(hypot)) = hypot_fast
fastmath(::typeof(lgamma)) = lgamma_fast
fastmath(::typeof(log10)) = log10_fast
fastmath(::typeof(log1p)) = log1p_fast
fastmath(::typeof(log2)) = log2_fast
fastmath(::typeof(log)) = log_fast
fastmath(::typeof(max)) = max_fast
fastmath(::typeof(min)) = min_fast
fastmath(::typeof(minmax)) = minmax_fast
fastmath(::typeof(sin)) = sin_fast
fastmath(::typeof(sinh)) = sinh_fast
fastmath(::typeof(sqrt)) = sqrt_fast
fastmath(::typeof(tan)) = tan_fast
fastmath(::typeof(tanh)) = tanh_fast

macro fastmath(expr::Expr)
    if expr.head === :quote
        return expr
    elseif expr.head === :(=) || expr.head === :function || expr.head === :->
        expr.args[2] = :(Base.FastMath.@fastmath $(expr.args[2]))
    elseif (h = string(expr.head); length(h) == 2 && h[2] == '=')
        op = Symbol(h[1])
        lhs = deepcopy(expr.args[1])
        expr = :($lhs = (Base.FastMath.fastmath($op))(Base.FastMath.@fastmath($(expr.args[1])), Base.FastMath.@fastmath($(expr.args[2]))))
    elseif expr.head === :.
        expr = :(Base.FastMath.fastmath($expr))
    else
        for i in 1:endof(expr.args)
            expr.args[i] = :(Base.FastMath.@fastmath $(expr.args[i]))
        end
    end
    esc(expr)
end
macro fastmath(x::Symbol)
    esc(:(Base.FastMath.fastmath($x)))
end
macro fastmath(x)
    x
end


# Basic arithmetic

FloatTypes = Union{Float32, Float64}

sub_fast{T<:FloatTypes}(x::T) = box(T,Base.neg_float_fast(unbox(T,x)))

add_fast{T<:FloatTypes}(x::T, y::T) =
    box(T,Base.add_float_fast(unbox(T,x), unbox(T,y)))
sub_fast{T<:FloatTypes}(x::T, y::T) =
    box(T,Base.sub_float_fast(unbox(T,x), unbox(T,y)))
mul_fast{T<:FloatTypes}(x::T, y::T) =
    box(T,Base.mul_float_fast(unbox(T,x), unbox(T,y)))
div_fast{T<:FloatTypes}(x::T, y::T) =
    box(T,Base.div_float_fast(unbox(T,x), unbox(T,y)))
rem_fast{T<:FloatTypes}(x::T, y::T) =
    box(T,Base.rem_float_fast(unbox(T,x), unbox(T,y)))

add_fast{T<:FloatTypes}(x::T, y::T, zs::T...) =
    add_fast(add_fast(x, y), zs...)
mul_fast{T<:FloatTypes}(x::T, y::T, zs::T...) =
    mul_fast(mul_fast(x, y), zs...)

@fastmath begin
    cmp_fast{T<:FloatTypes}(x::T, y::T) = ifelse(x==y, 0, ifelse(x<y, -1, +1))
    function mod_fast{T<:FloatTypes}(x::T, y::T)
        r = rem(x,y)
        ifelse((r > 0) $ (y > 0), r+y, r)
    end
end

eq_fast{T<:FloatTypes}(x::T, y::T) =
    Base.eq_float_fast(unbox(T,x),unbox(T,y))
ne_fast{T<:FloatTypes}(x::T, y::T) =
    Base.ne_float_fast(unbox(T,x),unbox(T,y))
lt_fast{T<:FloatTypes}(x::T, y::T) =
    Base.lt_float_fast(unbox(T,x),unbox(T,y))
le_fast{T<:FloatTypes}(x::T, y::T) =
    Base.le_float_fast(unbox(T,x),unbox(T,y))

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
    op_fast = Symbol(string(op,"_fast"))
    @eval begin
        # fall-back implementation for non-numeric types
        $op_fast(xs...) = $op(xs...)
    end
end

for (op,op_fast) in ((:+,:add_fast), (:-,:sub_fast), (:*,:mul_fast), (:/,:div_fast), (:(==),:eq_fast), (:!=,:ne_fast), (:<,:lt_fast), (:<=,:le_fast))
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

for op in (:cmp, :mod, :rem)
    op_fast = Symbol(string(op,"_fast"))
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
pow_fast{T<:FloatTypes}(x::T, y::Int32) =
    box(T, Base.powi_llvm(unbox(T,x), unbox(Int32,y)))

# TODO: Change sqrt_llvm intrinsic to avoid nan checking; add nan
# checking to sqrt in math.jl; remove sqrt_llvm_fast intrinsic
sqrt_fast{T<:FloatTypes}(x::T) = box(T, Base.sqrt_llvm_fast(unbox(T,x)))

# libm

const libm = Base.libm_name

for f in (:acos, :acosh, :asin, :asinh, :atan, :atanh, :cbrt, :cos,
          :cosh, :exp2, :exp, :expm1, :lgamma, :log10, :log1p, :log2,
          :log, :sin, :sinh, :tan, :tanh)
    f_fast = Symbol(string(f,"_fast"))
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
    f_fast = Symbol(string(f,"_fast"))
    @eval begin
        $f_fast(x) = $f(x)
    end
end

for f in (:atan2, :hypot, :max, :min, :minmax)
    f_fast = Symbol(string(f,"_fast"))
    @eval begin
        # fall-back implementation for non-numeric types
        $f_fast(x, y) = $f(x, y)
        # type promotion
        $f_fast(x::Number, y::Number) = $f_fast(promote(x, y)...)
        # fall-back implementation that applies after promotion
        $f_fast{T<:Number}(x::T, y::T) = $f(x, y)
    end
end
f = :^; f_fast = :pow_fast
@eval begin
    # fall-back implementation for non-numeric types
    $f_fast(x, y) = $f(x, y)
    # type promotion
    $f_fast(x::Number, y::Number) = $f_fast(promote(x, y)...)
    # fall-back implementation that applies after promotion
    $f_fast{T<:Number}(x::T, y::T) = $f(x, y)
end

end

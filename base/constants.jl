## general machinery for mathematical constants

immutable MathConst{sym} <: Real end

show{sym}(io::IO, ::MathConst{sym}) = print(io, sym)

promote_rule{s}(::Type{MathConst{s}}, ::Type) = Float64
promote_rule{s}(::Type{MathConst{s}}, ::Type{Float32}) = Float32
promote_rule{s}(::Type{MathConst{s}}, ::Type{BigInt}) = BigFloat
promote_rule{s}(::Type{MathConst{s}}, ::Type{BigFloat}) = BigFloat
promote_rule{s}(::Type{MathConst{s}}, ::Type{ImaginaryUnit}) = Complex{Float64}
promote_rule{s}(::Type{ImaginaryUnit}, ::Type{MathConst{s}}) = Complex{Float64}
promote_rule{s,T<:Real}(::Type{MathConst{s}}, ::Type{Complex{T}}) =
    Complex{promote_type(MathConst{s},T)}

convert(::Type{FloatingPoint}, x::MathConst) = float64(x)
convert{T<:Real}(::Type{Complex{T}}, x::MathConst) = convert(Complex{T}, float64(x))
convert{T<:Integer}(::Type{Rational{T}}, x::MathConst) = convert(Rational{T}, float64(x))

-(x::MathConst) = -float64(x)
+(x::MathConst, y::MathConst) = float64(x) + float64(y)
-(x::MathConst, y::MathConst) = float64(x) - float64(y)
*(x::MathConst, y::MathConst) = float64(x) * float64(y)
/(x::MathConst, y::MathConst) = float64(x) / float64(y)
^(x::MathConst, y::MathConst) = float64(x) ^ float64(y)

*(x::MathConst, i::ImaginaryUnit) = float64(x)*i
*(i::ImaginaryUnit, x::MathConst) = i*float64(x)

## specific mathematical constants

const π = MathConst{:π}()
const e = MathConst{:e}()
const γ = MathConst{:γ}()

for (sym, name) in ((:π, :pi), (:γ, :euler))
    qsym = Expr(:quote, sym)
    @eval begin
        function convert(::Type{BigFloat}, ::MathConst{$qsym})
            c = BigFloat()
            ccall(($(string("mpfr_const_", name)), :libmpfr),
                  Cint, (Ptr{BigFloat}, Int32),
                  &c, MPFR.ROUNDING_MODE[end])
            return c
        end
    end
end

convert(::Type{BigFloat}, ::MathConst{:e}) = exp(big(1))

for sym in (:π, :e, :γ)
    x = eval(sym)
    qsym = Expr(:quote, sym)
    @eval begin
        convert(::Type{Float64}, ::MathConst{$qsym}) = $(float64(convert(BigFloat,x)))
        convert(::Type{Float32}, ::MathConst{$qsym}) = $(float32(convert(BigFloat,x)))
    end
end

# aliases
const pi = π
const eulergamma = γ

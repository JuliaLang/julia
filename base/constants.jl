## general machinery for mathematical constants

immutable MathConst{sym} <: Real end

show{sym}(io::IO, x::MathConst{sym}) = print(io, "$sym = $(string(float(x))[1:15])...")

promote_rule{s}(::Type{MathConst{s}}, ::Type{Float32}) = Float32
promote_rule{s,t}(::Type{MathConst{s}}, ::Type{MathConst{t}}) = Float64
promote_rule{s,T<:Number}(::Type{MathConst{s}}, ::Type{T}) = promote_type(Float64,T)

convert(::Type{FloatingPoint}, x::MathConst) = float64(x)
convert(::Type{Float16}, x::MathConst) = float16(float32(x))
convert{T<:Real}(::Type{Complex{T}}, x::MathConst) = convert(Complex{T}, convert(T,x))
convert{T<:Integer}(::Type{Rational{T}}, x::MathConst) = convert(Rational{T}, float64(x))

stagedfunction call{T<:Union(Float32,Float64),s}(t::Type{T},c::MathConst{s},r::RoundingMode)
    f = T(big(c()),r())
    :($f)
end

=={s}(::MathConst{s}, ::MathConst{s}) = true
==(::MathConst, ::MathConst) = false

hash(x::MathConst, h::UInt) = hash(object_id(x), h)

-(x::MathConst) = -float64(x)
for op in Symbol[:+, :-, :*, :/, :^]
    @eval $op(x::MathConst, y::MathConst) = $op(float64(x),float64(y))
end

macro math_const(sym, val, def)
    esym = esc(sym)
    qsym = esc(Expr(:quote, sym))
    bigconvert = isa(def,Symbol) ? quote
        function Base.convert(::Type{BigFloat}, ::MathConst{$qsym})
            c = BigFloat()
            ccall(($(string("mpfr_const_", def)), :libmpfr),
                  Cint, (Ptr{BigFloat}, Int32),
                  &c, MPFR.ROUNDING_MODE[end])
            return c
        end
    end : quote
        Base.convert(::Type{BigFloat}, ::MathConst{$qsym}) = $(esc(def))
    end
    quote
        const $esym = MathConst{$qsym}()
        $bigconvert
        Base.convert(::Type{Float64}, ::MathConst{$qsym}) = $val
        Base.convert(::Type{Float32}, ::MathConst{$qsym}) = $(float32(val))
        @assert isa(big($esym), BigFloat)
        @assert float64($esym) == float64(big($esym))
        @assert float32($esym) == float32(big($esym))
    end
end

big(x::MathConst) = convert(BigFloat,x)

## specific mathematical constants

@math_const π        3.14159265358979323846  pi
@math_const e        2.71828182845904523536  exp(big(1))
@math_const γ        0.57721566490153286061  euler
@math_const catalan  0.91596559417721901505  catalan
@math_const φ        1.61803398874989484820  (1+sqrt(big(5)))/2

# aliases
const pi = π
const eu = e
const eulergamma = γ
const golden = φ

# special behaviors

# use exp for e^x or e.^x, as in
#    ^(::MathConst{:e}, x::Number) = exp(x)
#    .^(::MathConst{:e}, x) = exp(x)
# but need to loop over types to prevent ambiguity with generic rules for ^(::Number, x) etc.
for T in (MathConst, Rational, Integer, Number)
    ^(::MathConst{:e}, x::T) = exp(x)
end
for T in (Range, BitArray, SparseMatrixCSC, StridedArray, AbstractArray)
    .^(::MathConst{:e}, x::T) = exp(x)
end
^(::MathConst{:e}, x::AbstractMatrix) = expm(x)

log(::MathConst{:e}) = 1 # use 1 to correctly promote expressions like log(x)/log(e)
log(::MathConst{:e}, x) = log(x)

#Align along = for nice Array printing
function alignment(x::MathConst)
    m = match(r"^(.*?)(=.*)$", sprint(showcompact_lim, x))
    m == nothing ? (length(sprint(showcompact_lim, x)), 0) :
    (length(m.captures[1]), length(m.captures[2]))
end

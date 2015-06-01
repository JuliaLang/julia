# This file is a part of Julia. License is MIT: http://julialang.org/license

## floating-point functions ##

copysign(x::Float64, y::Float64) = box(Float64,copysign_float(unbox(Float64,x),unbox(Float64,y)))
copysign(x::Float32, y::Float32) = box(Float32,copysign_float(unbox(Float32,x),unbox(Float32,y)))
copysign(x::Float32, y::Real) = copysign(x, Float32(y))
copysign(x::Float64, y::Real) = copysign(x, Float64(y))
@vectorize_2arg Real copysign

flipsign(x::Float64, y::Float64) = box(Float64,xor_int(unbox(Float64,x),and_int(unbox(Float64,y),0x8000000000000000)))
flipsign(x::Float32, y::Float32) = box(Float32,xor_int(unbox(Float32,x),and_int(unbox(Float32,y),0x80000000)))
flipsign(x::Float32, y::Real) = flipsign(x, Float32(y))
flipsign(x::Float64, y::Real) = flipsign(x, Float64(y))
@vectorize_2arg Real flipsign

signbit(x::Float64) = signbit(reinterpret(Int64,x))
signbit(x::Float32) = signbit(reinterpret(Int32,x))
signbit(x::Float16) = signbit(reinterpret(Int16,x))

maxintfloat(::Type{Float64}) = 9007199254740992.
maxintfloat(::Type{Float32}) = Float32(16777216.)
maxintfloat(::Type{Float16}) = Float16(2048f0)
maxintfloat{T<:FloatingPoint}(x::T)  = maxintfloat(T)
maxintfloat() = maxintfloat(Float64)

isinteger(x::FloatingPoint) = (trunc(x)==x)&isfinite(x)

num2hex(x::Float16) = hex(reinterpret(UInt16,x), 4)
num2hex(x::Float32) = hex(box(UInt32,unbox(Float32,x)),8)
num2hex(x::Float64) = hex(box(UInt64,unbox(Float64,x)),16)

function hex2num(s::AbstractString)
    if length(s) <= 8
        return box(Float32,unbox(UInt32,parse(UInt32,s,16)))
    end
    return box(Float64,unbox(UInt64,parse(UInt64,s,16)))
end

@vectorize_1arg Number abs
@vectorize_1arg Number abs2
@vectorize_1arg Number angle

@vectorize_1arg Number isnan
@vectorize_1arg Number isinf
@vectorize_1arg Number isfinite


round(x::Real, ::RoundingMode{:ToZero}) = trunc(x)
round(x::Real, ::RoundingMode{:Up}) = ceil(x)
round(x::Real, ::RoundingMode{:Down}) = floor(x)
# C-style round
function round(x::FloatingPoint, ::RoundingMode{:NearestTiesAway})
    y = trunc(x)
    ifelse(x==y,y,trunc(2*x-y))
end
# Java-style round
function round(x::FloatingPoint, ::RoundingMode{:NearestTiesUp})
    y = floor(x)
    ifelse(x==y,y,copysign(floor(2*x-y),x))
end
round{T<:Integer}(::Type{T}, x::FloatingPoint, r::RoundingMode) = trunc(T,round(x,r))

@vectorize_1arg Real trunc
@vectorize_1arg Real floor
@vectorize_1arg Real ceil
@vectorize_1arg Real round

for f in (:trunc,:floor,:ceil,:round)
    @eval begin
        function ($f){T,R}(::Type{T}, x::AbstractArray{R,1})
            [ ($f)(T, x[i])::T for i = 1:length(x) ]
        end
        function ($f){T,R}(::Type{T}, x::AbstractArray{R,2})
            [ ($f)(T, x[i,j])::T for i = 1:size(x,1), j = 1:size(x,2) ]
        end
        function ($f){T}(::Type{T}, x::AbstractArray)
            reshape([ ($f)(T, x[i])::T for i in eachindex(x) ], size(x))
        end
        function ($f){R}(x::AbstractArray{R,1}, digits::Integer, base::Integer=10)
            [ ($f)(x[i], digits, base) for i = 1:length(x) ]
        end
        function ($f){R}(x::AbstractArray{R,2}, digits::Integer, base::Integer=10)
            [ ($f)(x[i,j], digits, base) for i = 1:size(x,1), j = 1:size(x,2) ]
        end
        function ($f)(x::AbstractArray, digits::Integer, base::Integer=10)
            reshape([ ($f)(x[i], digits, base) for i in eachindex(x) ], size(x))
        end
    end
end

function round{R}(x::AbstractArray{R,1}, r::RoundingMode)
    [ round(x[i], r) for i = 1:length(x) ]
end
function round{R}(x::AbstractArray{R,2}, r::RoundingMode)
    [ round(x[i,j], r) for i = 1:size(x,1), j = 1:size(x,2) ]
end
function round(x::AbstractArray, r::RoundingMode)
    reshape([ round(x[i], r) for i in eachindex(x) ], size(x))
end

function round{T,R}(::Type{T}, x::AbstractArray{R,1}, r::RoundingMode)
    [ round(T, x[i], r)::T for i = 1:length(x) ]
end
function round{T,R}(::Type{T}, x::AbstractArray{R,2}, r::RoundingMode)
    [ round(T, x[i,j], r)::T for i = 1:size(x,1), j = 1:size(x,2) ]
end
function round{T}(::Type{T}, x::AbstractArray, r::RoundingMode)
    reshape([ round(T, x[i], r)::T for i in eachindex(x) ], size(x))
end

# adapted from Matlab File Exchange roundsd: http://www.mathworks.com/matlabcentral/fileexchange/26212
# for round, og is the power of 10 relative to the decimal point
# for signif, og is the absolute power of 10
# digits and base must be integers, x must be convertable to float

function _signif_og(x, digits, base)
    if base == 10
        e = floor(log10(abs(x)) - digits + 1.)
        og = oftype(x, exp10(abs(e)))
    elseif base == 2
        e = exponent(abs(x)) - digits + 1.
        og = oftype(x, exp2(abs(e)))
    else
        e = floor(log(base, abs(x)) - digits + 1.)
        og = oftype(x, float(base) ^ abs(e))
    end
    return og, e
end

function signif(x::Real, digits::Integer, base::Integer=10)
    digits < 1 && throw(DomainError())

    x = float(x)
    (x == 0 || !isfinite(x)) && return x
    og, e = _signif_og(x, digits, base)
    if e >= 0 # for numeric stability
        r = round(x/og)*og
    else
        r = round(x*og)/og
    end
    !isfinite(r) ? x : r
end

for f in (:round, :ceil, :floor, :trunc)
    @eval begin
        function ($f)(x::Real, digits::Integer, base::Integer=10)
            x = float(x)
            og = convert(eltype(x),base)^digits
            r = ($f)(x * og) / og

            if !isfinite(r)
                if digits > 0
                    return x
                elseif x > 0
                    return $(:ceil == f ? :(convert(eltype(x), Inf)) : :(zero(x)))
                elseif x < 0
                    return $(:floor == f ? :(-convert(eltype(x), Inf)) : :(-zero(x)))
                else
                    return x
                end
            end
            return r
        end
    end
end

# isapprox: Tolerant comparison of floating point numbers
function isapprox(x::FloatingPoint, y::FloatingPoint; rtol::Real=rtoldefault(x,y), atol::Real=atoldefault(x,y))
    (isinf(x) || isinf(y)) ? x == y : abs(x-y) <= atol + rtol.*max(abs(x), abs(y))
end

# promotion of non-floats
isapprox(x::Real, y::FloatingPoint; rtol::Real=rtoldefault(x, y), atol::Real=atoldefault(x, y)) = isapprox(promote(x, y)...; rtol=rtol, atol=atol)
isapprox(x::FloatingPoint, y::Real; rtol::Real=rtoldefault(x, y), atol::Real=atoldefault(x, y)) = isapprox(promote(x, y)...; rtol=rtol, atol=atol)

# other real numbers
isapprox(x::Real, y::Real; rtol::Real=0, atol::Real=0) = abs(x-y) <= atol

# complex numbers
isapprox(z::Complex, w::Complex; rtol::Real=rtoldefault(abs(z), abs(w)), atol::Real=atoldefault(abs(z), abs(w))) = abs(z-w) <= atol + rtol*max(abs(z), abs(w))

# real-complex combinations
isapprox(x::Real, z::Complex; rtol::Real=rtoldefault(x, abs(z)), atol::Real=atoldefault(x, abs(z))) = isapprox(complex(x), z; rtol=rtol, atol=atol)
isapprox(z::Complex, x::Real; rtol::Real=rtoldefault(x, abs(z)), atol::Real=atoldefault(x, abs(z))) = isapprox(complex(x), z; rtol=rtol, atol=atol)

# default tolerance arguments
rtoldefault(x::FloatingPoint, y::FloatingPoint) = cbrt(max(eps(x), eps(y)))
atoldefault(x::FloatingPoint, y::FloatingPoint) = sqrt(max(eps(x), eps(y)))

# promotion of non-floats
for fun in (:rtoldefault, :atoldefault)
    @eval begin
        ($fun)(x::Real, y::FloatingPoint) = ($fun)(promote(x,y)...)
        ($fun)(x::FloatingPoint, y::Real) = ($fun)(promote(x,y)...)
    end
end

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
maxintfloat{T<:AbstractFloat}(x::T)  = maxintfloat(T)
maxintfloat() = maxintfloat(Float64)

isinteger(x::AbstractFloat) = (trunc(x)==x)&isfinite(x)

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
function round(x::AbstractFloat, ::RoundingMode{:NearestTiesAway})
    y = trunc(x)
    ifelse(x==y,y,trunc(2*x-y))
end
# Java-style round
function round(x::AbstractFloat, ::RoundingMode{:NearestTiesUp})
    y = floor(x)
    ifelse(x==y,y,copysign(floor(2*x-y),x))
end
round{T<:Integer}(::Type{T}, x::AbstractFloat, r::RoundingMode) = trunc(T,round(x,r))

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

# isapprox: approximate equality of numbers
function isapprox(x::Number, y::Number; rtol::Real=rtoldefault(x,y), atol::Real=0)
    x == y || (isfinite(x) && isfinite(y) && abs(x-y) <= atol + rtol*max(abs(x), abs(y)))
end

const ≈ = isapprox
≉(x,y) = !(x ≈ y)

# default tolerance arguments
rtoldefault{T<:AbstractFloat}(::Type{T}) = sqrt(eps(T))
rtoldefault{T<:Real}(::Type{T}) = 0
rtoldefault{T<:Number,S<:Number}(x::Union{T,Type{T}}, y::Union{S,Type{S}}) = rtoldefault(promote_type(real(T),real(S)))

# fused multiply-add
fma_libm(x::Float32, y::Float32, z::Float32) =
    ccall(("fmaf", libm_name), Float32, (Float32,Float32,Float32), x, y, z)
fma_libm(x::Float64, y::Float64, z::Float64) =
    ccall(("fma", libm_name), Float64, (Float64,Float64,Float64), x, y, z)
fma_llvm(x::Float32, y::Float32, z::Float32) =
    box(Float32,fma_float(unbox(Float32,x),unbox(Float32,y),unbox(Float32,z)))
fma_llvm(x::Float64, y::Float64, z::Float64) =
    box(Float64,fma_float(unbox(Float64,x),unbox(Float64,y),unbox(Float64,z)))
# Disable LLVM's fma if it is incorrect, e.g. because LLVM falls back
# onto a broken system libm; if so, use openlibm's fma instead
# 1.0000305f0 = 1 + 1/2^15
# 1.0000000009313226 = 1 + 1/2^30
# If fma_llvm() clobbers the rounding mode, the result of 0.1 + 0.2 will be 0.3
# instead of the properly-rounded 0.30000000000000004; check after calling fma
if (ARCH != :i686 && fma_llvm(1.0000305f0, 1.0000305f0, -1.0f0) == 6.103609f-5 &&
    (fma_llvm(1.0000000009313226, 1.0000000009313226, -1.0) ==
     1.8626451500983188e-9) && 0.1 + 0.2 == 0.30000000000000004)
    fma(x::Float32, y::Float32, z::Float32) = fma_llvm(x,y,z)
    fma(x::Float64, y::Float64, z::Float64) = fma_llvm(x,y,z)
else
    fma(x::Float32, y::Float32, z::Float32) = fma_libm(x,y,z)
    fma(x::Float64, y::Float64, z::Float64) = fma_libm(x,y,z)
end
# This is necessary at least on 32-bit Intel Linux, since fma_llvm may
# have called glibc, and some broken glibc fma implementations don't
# properly restore the rounding mode
Rounding.setrounding_raw(Float32, Rounding.JL_FE_TONEAREST)
Rounding.setrounding_raw(Float64, Rounding.JL_FE_TONEAREST)

# This file is a part of Julia. License is MIT: https://julialang.org/license

# (This is part of the FFTW module.)

export dct, idct, dct!, idct!, plan_dct, plan_idct, plan_dct!, plan_idct!

# Discrete cosine transforms (type II/III) via FFTW's r2r transforms;
# we follow the Matlab convention and adopt a unitary normalization here.
# Unlike Matlab we compute the multidimensional transform by default,
# similar to the Julia fft functions.

mutable struct DCTPlan{T<:fftwNumber,K,inplace} <: Plan{T}
    plan::r2rFFTWPlan{T}
    r::Array{UnitRange{Int}} # array of indices for rescaling
    nrm::Float64 # normalization factor
    region::Dims # dimensions being transformed
    pinv::DCTPlan{T}
    DCTPlan{T,K,inplace}(plan,r,nrm,region) where {T<:fftwNumber,K,inplace} = new(plan,r,nrm,region)
end

size(p::DCTPlan) = size(p.plan)

function show(io::IO, p::DCTPlan{T,K,inplace}) where {T,K,inplace}
    print(io, inplace ? "FFTW in-place " : "FFTW ",
          K == REDFT10 ? "DCT (DCT-II)" : "IDCT (DCT-III)", " plan for ")
    showfftdims(io, p.plan.sz, p.plan.istride, eltype(p))
end

for (pf, pfr, K, inplace) in ((:plan_dct, :plan_r2r, REDFT10, false),
                              (:plan_dct!, :plan_r2r!, REDFT10, true),
                              (:plan_idct, :plan_r2r, REDFT01, false),
                              (:plan_idct!, :plan_r2r!, REDFT01, true))
    @eval function $pf(X::StridedArray{T}, region; kws...) where T<:fftwNumber
        r = [1:n for n in size(X)]
        nrm = sqrt(0.5^length(region) * normalization(X,region))
        DCTPlan{T,$K,$inplace}($pfr(X, $K, region; kws...), r, nrm,
                               ntuple(i -> Int(region[i]), length(region)))
    end
end

function plan_inv(p::DCTPlan{T,K,inplace}) where {T,K,inplace}
    X = Array{T}(p.plan.sz)
    iK = inv_kind[K]
    DCTPlan{T,iK,inplace}(inplace ?
                          plan_r2r!(X, iK, p.region, flags=p.plan.flags) :
                          plan_r2r(X, iK, p.region, flags=p.plan.flags),
                          p.r, p.nrm, p.region)
end

for f in (:dct, :dct!, :idct, :idct!)
    pf = Symbol("plan_", f)
    @eval begin
        $f(x::AbstractArray{<:fftwNumber}) = $pf(x) * x
        $f(x::AbstractArray{<:fftwNumber}, region) = $pf(x, region) * x
        $pf(x::AbstractArray; kws...) = $pf(x, 1:ndims(x); kws...)
        $f(x::AbstractArray{<:Real}, region=1:ndims(x)) = $f(fftwfloat(x), region)
        $pf(x::AbstractArray{<:Real}, region; kws...) = $pf(fftwfloat(x), region; kws...)
        $pf(x::AbstractArray{<:Complex}, region; kws...) = $pf(fftwcomplex(x), region; kws...)
    end
end

const sqrthalf = sqrt(0.5)
const sqrt2 = sqrt(2.0)
const onerange = 1:1

function A_mul_B!(y::StridedArray{T}, p::DCTPlan{T,REDFT10},
                  x::StridedArray{T}) where T
    assert_applicable(p.plan, x, y)
    unsafe_execute!(p.plan, x, y)
    scale!(y, p.nrm)
    r = p.r
    for d in p.region
        oldr = r[d]
        r[d] = onerange
        y[r...] *= sqrthalf
        r[d] = oldr
    end
    return y
end

# note: idct changes input data
function A_mul_B!(y::StridedArray{T}, p::DCTPlan{T,REDFT01},
                  x::StridedArray{T}) where T
    assert_applicable(p.plan, x, y)
    scale!(x, p.nrm)
    r = p.r
    for d in p.region
        oldr = r[d]
        r[d] = onerange
        x[r...] *= sqrt2
        r[d] = oldr
    end
    unsafe_execute!(p.plan, x, y)
    return y
end

*(p::DCTPlan{T,REDFT10,false}, x::StridedArray{T}) where {T} =
    A_mul_B!(Array{T}(p.plan.osz), p, x)

*(p::DCTPlan{T,REDFT01,false}, x::StridedArray{T}) where {T} =
    A_mul_B!(Array{T}(p.plan.osz), p, copy(x)) # need copy to preserve input

*(p::DCTPlan{T,K,true}, x::StridedArray{T}) where {T,K} = A_mul_B!(x, p, x)

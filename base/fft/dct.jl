# This file is a part of Julia. License is MIT: http://julialang.org/license

# (This is part of the FFTW module.)

export dct, idct, dct!, idct!, plan_dct, plan_idct, plan_dct!, plan_idct!

# Discrete cosine transforms (type II/III) via FFTW's r2r transforms;
# we follow the Matlab convention and adopt a unitary normalization here.
# Unlike Matlab we compute the multidimensional transform by default,
# similar to the Julia fft functions.

type DCTPlan{T<:fftwNumber,K,inplace} <: Plan{T}
    plan::r2rFFTWPlan{T}
    r::Array{UnitRange{Int}} # array of indices for rescaling
    nrm::Float64 # normalization factor
    region::Dims # dimensions being transformed
    pinv::DCTPlan{T}
    DCTPlan(plan,r,nrm,region) = new(plan,r,nrm,region)
end

size(p::DCTPlan) = size(p.plan)

function show{T,K,inplace}(io::IO, p::DCTPlan{T,K,inplace})
    print(io, inplace ? "FFTW in-place " : "FFTW ",
          K == REDFT10 ? "DCT (DCT-II)" : "IDCT (DCT-III)", " plan for ")
    showfftdims(io, p.plan.sz, p.plan.istride, eltype(p))
end

for (pf, pfr, K, inplace) in ((:plan_dct, :plan_r2r, REDFT10, false),
                              (:plan_dct!, :plan_r2r!, REDFT10, true),
                              (:plan_idct, :plan_r2r, REDFT01, false),
                              (:plan_idct!, :plan_r2r!, REDFT01, true))
    @eval function $pf{T<:fftwNumber}(X::StridedArray{T}, region; kws...)
        r = [1:n for n in size(X)]
        nrm = sqrt(0.5^length(region) * normalization(X,region))
        DCTPlan{T,$K,$inplace}($pfr(X, $K, region; kws...), r, nrm,
                               ntuple(i -> Int(region[i]), length(region)))
    end
end

function plan_inv{T,K,inplace}(p::DCTPlan{T,K,inplace})
    X = Array(T, p.plan.sz)
    iK = inv_kind[K]
    DCTPlan{T,iK,inplace}(inplace ?
                          plan_r2r!(X, iK, p.region, flags=p.plan.flags) :
                          plan_r2r(X, iK, p.region, flags=p.plan.flags),
                          p.r, p.nrm, p.region)
end

for f in (:dct, :dct!, :idct, :idct!)
    pf = symbol(string("plan_", f))
    @eval begin
        $f{T<:fftwNumber}(x::AbstractArray{T}) = $pf(x) * x
        $f{T<:fftwNumber}(x::AbstractArray{T}, region) = $pf(x, region) * x
        $pf(x::AbstractArray; kws...) = $pf(x, 1:ndims(x); kws...)
        $f{T<:Real}(x::AbstractArray{T}, region=1:ndims(x)) = $f(fftwfloat(x), region)
        $pf{T<:Real}(x::AbstractArray{T}, region; kws...) = $pf(fftwfloat(x), region; kws...)
        $pf{T<:Complex}(x::AbstractArray{T}, region; kws...) = $pf(fftwcomplex(x), region; kws...)
    end
end

const sqrthalf = sqrt(0.5)
const sqrt2 = sqrt(2.0)
const onerange = 1:1

function A_mul_B!{T}(y::StridedArray{T}, p::DCTPlan{T,REDFT10},
                     x::StridedArray{T})
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
function A_mul_B!{T}(y::StridedArray{T}, p::DCTPlan{T,REDFT01},
                     x::StridedArray{T})
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

*{T}(p::DCTPlan{T,REDFT10,false}, x::StridedArray{T}) =
    A_mul_B!(Array(T, p.plan.osz), p, x)

*{T}(p::DCTPlan{T,REDFT01,false}, x::StridedArray{T}) =
    A_mul_B!(Array(T, p.plan.osz), p, copy(x)) # need copy to preserve input

*{T,K}(p::DCTPlan{T,K,true}, x::StridedArray{T}) = A_mul_B!(x, p, x)

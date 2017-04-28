# This file is a part of Julia. License is MIT: https://julialang.org/license

module DFT

# DFT plan where the inputs are an array of eltype T
abstract type Plan{T} end

import Base: show, summary, size, ndims, length, eltype,
             *, A_mul_B!, inv, \, A_ldiv_B!

eltype(::Type{Plan{T}}) where {T} = T

# size(p) should return the size of the input array for p
size(p::Plan, d) = size(p)[d]
ndims(p::Plan) = length(size(p))
length(p::Plan) = prod(size(p))::Int

##############################################################################
export fft, ifft, bfft, fft!, ifft!, bfft!,
       plan_fft, plan_ifft, plan_bfft, plan_fft!, plan_ifft!, plan_bfft!,
       rfft, irfft, brfft, plan_rfft, plan_irfft, plan_brfft

const FFTWFloat = Union{Float32,Float64}
fftwfloat(x) = _fftwfloat(float(x))
_fftwfloat(::Type{T}) where {T<:FFTWFloat} = T
_fftwfloat(::Type{Float16}) = Float32
_fftwfloat(::Type{Complex{T}}) where {T} = Complex{_fftwfloat(T)}
_fftwfloat(::Type{T}) where {T} = error("type $T not supported")
_fftwfloat(x::T) where {T} = _fftwfloat(T)(x)

complexfloat(x::StridedArray{Complex{<:FFTWFloat}}) = x
realfloat(x::StridedArray{<:FFTWFloat}) = x

# return an Array, rather than similar(x), to avoid an extra copy for FFTW
# (which only works on StridedArray types).
complexfloat(x::AbstractArray{T}) where {T<:Complex} = copy1(typeof(fftwfloat(zero(T))), x)
complexfloat(x::AbstractArray{T}) where {T<:Real} = copy1(typeof(complex(fftwfloat(zero(T)))), x)

realfloat(x::AbstractArray{T}) where {T<:Real} = copy1(typeof(fftwfloat(zero(T))), x)

# copy to a 1-based array, using circular permutation
function copy1(::Type{T}, x) where T
    y = Array{T}(map(length, indices(x)))
    Base.circcopy!(y, x)
end

to1(x::AbstractArray) = _to1(indices(x), x)
_to1(::Tuple{Base.OneTo,Vararg{Base.OneTo}}, x) = x
_to1(::Tuple, x) = copy1(eltype(x), x)

# implementations only need to provide plan_X(x, region)
# for X in (:fft, :bfft, ...):
for f in (:fft, :bfft, :ifft, :fft!, :bfft!, :ifft!, :rfft)
    pf = Symbol("plan_", f)
    @eval begin
        $f(x::AbstractArray) = (y = to1(x); $pf(y) * y)
        $f(x::AbstractArray, region) = (y = to1(x); $pf(y, region) * y)
        $pf(x::AbstractArray; kws...) = (y = to1(x); $pf(y, 1:ndims(y); kws...))
    end
end

"""
    plan_ifft(A [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

Same as [`plan_fft`](@ref), but produces a plan that performs inverse transforms
[`ifft`](@ref).
"""
plan_ifft

"""
    plan_ifft!(A [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

Same as [`plan_ifft`](@ref), but operates in-place on `A`.
"""
plan_ifft!

"""
    plan_bfft!(A [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

Same as [`plan_bfft`](@ref), but operates in-place on `A`.
"""
plan_bfft!

"""
    plan_bfft(A [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

Same as [`plan_fft`](@ref), but produces a plan that performs an unnormalized
backwards transform [`bfft`](@ref).
"""
plan_bfft

"""
    plan_fft(A [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

Pre-plan an optimized FFT along given dimensions (`dims`) of arrays matching the shape and
type of `A`.  (The first two arguments have the same meaning as for [`fft`](@ref).)
Returns an object `P` which represents the linear operator computed by the FFT, and which
contains all of the information needed to compute `fft(A, dims)` quickly.

To apply `P` to an array `A`, use `P * A`; in general, the syntax for applying plans is much
like that of matrices.  (A plan can only be applied to arrays of the same size as the `A`
for which the plan was created.)  You can also apply a plan with a preallocated output array `Â`
by calling `A_mul_B!(Â, plan, A)`.  (For `A_mul_B!`, however, the input array `A` must
be a complex floating-point array like the output `Â`.) You can compute the inverse-transform plan by `inv(P)`
and apply the inverse plan with `P \\ Â` (the inverse plan is cached and reused for
subsequent calls to `inv` or `\\`), and apply the inverse plan to a pre-allocated output
array `A` with `A_ldiv_B!(A, P, Â)`.

The `flags` argument is a bitwise-or of FFTW planner flags, defaulting to `FFTW.ESTIMATE`.
e.g. passing `FFTW.MEASURE` or `FFTW.PATIENT` will instead spend several seconds (or more)
benchmarking different possible FFT algorithms and picking the fastest one; see the FFTW
manual for more information on planner flags.  The optional `timelimit` argument specifies a
rough upper bound on the allowed planning time, in seconds. Passing `FFTW.MEASURE` or
`FFTW.PATIENT` may cause the input array `A` to be overwritten with zeros during plan
creation.

[`plan_fft!`](@ref) is the same as [`plan_fft`](@ref) but creates a
plan that operates in-place on its argument (which must be an array of complex
floating-point numbers). [`plan_ifft`](@ref) and so on are similar but produce
plans that perform the equivalent of the inverse transforms [`ifft`](@ref) and so on.
"""
plan_fft

"""
    plan_fft!(A [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

Same as [`plan_fft`](@ref), but operates in-place on `A`.
"""
plan_fft!

"""
    rfft(A [, dims])

Multidimensional FFT of a real array `A`, exploiting the fact that the transform has
conjugate symmetry in order to save roughly half the computational time and storage costs
compared with [`fft`](@ref). If `A` has size `(n_1, ..., n_d)`, the result has size
`(div(n_1,2)+1, ..., n_d)`.

The optional `dims` argument specifies an iterable subset of one or more dimensions of `A`
to transform, similar to [`fft`](@ref). Instead of (roughly) halving the first
dimension of `A` in the result, the `dims[1]` dimension is (roughly) halved in the same way.
"""
rfft

"""
    ifft!(A [, dims])

Same as [`ifft`](@ref), but operates in-place on `A`.
"""
ifft!

"""
    ifft(A [, dims])

Multidimensional inverse FFT.

A one-dimensional inverse FFT computes

```math
\\operatorname{IDFT}(A)[k] = \\frac{1}{\\operatorname{length}(A)}
\\sum_{n=1}^{\\operatorname{length}(A)} \\exp\\left(+i\\frac{2\\pi (n-1)(k-1)}
{\\operatorname{length}(A)} \\right) A[n].
```

A multidimensional inverse FFT simply performs this operation along each transformed dimension of `A`.
"""
ifft

"""
    fft!(A [, dims])

Same as [`fft`](@ref), but operates in-place on `A`, which must be an array of
complex floating-point numbers.
"""
fft!

"""
    bfft(A [, dims])

Similar to [`ifft`](@ref), but computes an unnormalized inverse (backward)
transform, which must be divided by the product of the sizes of the transformed dimensions
in order to obtain the inverse. (This is slightly more efficient than [`ifft`](@ref)
because it omits a scaling step, which in some applications can be combined with other
computational steps elsewhere.)

```math
\\operatorname{BDFT}(A)[k] = \\operatorname{length}(A) \\operatorname{IDFT}(A)[k]
```
"""
bfft

"""
    bfft!(A [, dims])

Same as [`bfft`](@ref), but operates in-place on `A`.
"""
bfft!

# promote to a complex floating-point type (out-of-place only),
# so implementations only need Complex{Float} methods
for f in (:fft, :bfft, :ifft)
    pf = Symbol("plan_", f)
    @eval begin
        $f(x::AbstractArray{<:Real}, region=1:ndims(x)) = $f(complexfloat(x), region)
        $pf(x::AbstractArray{<:Real}, region; kws...) = $pf(complexfloat(x), region; kws...)
        $f(x::AbstractArray{<:Complex{<:Union{Integer,Rational}}}, region=1:ndims(x)) = $f(complexfloat(x), region)
        $pf(x::AbstractArray{<:Complex{<:Union{Integer,Rational}}}, region; kws...) = $pf(complexfloat(x), region; kws...)
    end
end
rfft(x::AbstractArray{<:Union{Integer,Rational}}, region=1:ndims(x)) = rfft(realfloat(x), region)
plan_rfft(x::AbstractArray, region; kws...) = plan_rfft(realfloat(x), region; kws...)

# only require implementation to provide *(::Plan{T}, ::Array{T})
*(p::Plan{T}, x::AbstractArray) where {T} = p * copy1(T, x)

# Implementations should also implement A_mul_B!(Y, plan, X) so as to support
# pre-allocated output arrays.  We don't define * in terms of A_mul_B!
# generically here, however, because of subtleties for in-place and rfft plans.

##############################################################################
# To support inv, \, and A_ldiv_B!(y, p, x), we require Plan subtypes
# to have a pinv::Plan field, which caches the inverse plan, and which
# should be initially undefined.  They should also implement
# plan_inv(p) to construct the inverse of a plan p.

# hack from @simonster (in #6193) to compute the return type of plan_inv
# without actually calling it or even constructing the empty arrays.
_pinv_type(p::Plan) = typeof([plan_inv(x) for x in typeof(p)[]])
pinv_type(p::Plan) = eltype(_pinv_type(p))

inv(p::Plan) =
    isdefined(p, :pinv) ? p.pinv::pinv_type(p) : (p.pinv = plan_inv(p))
\(p::Plan, x::AbstractArray) = inv(p) * x
A_ldiv_B!(y::AbstractArray, p::Plan, x::AbstractArray) = A_mul_B!(y, inv(p), x)

##############################################################################
# implementations only need to provide the unnormalized backwards FFT,
# similar to FFTW, and we do the scaling generically to get the ifft:

mutable struct ScaledPlan{T,P,N} <: Plan{T}
    p::P
    scale::N # not T, to avoid unnecessary promotion to Complex
    pinv::Plan
    ScaledPlan{T,P,N}(p, scale) where {T,P,N} = new(p, scale)
end
ScaledPlan{T}(p::P, scale::N) where {T,P,N} = ScaledPlan{T,P,N}(p, scale)
ScaledPlan(p::Plan{T}, scale::Number) where {T} = ScaledPlan{T}(p, scale)
ScaledPlan(p::ScaledPlan, α::Number) = ScaledPlan(p.p, p.scale * α)

size(p::ScaledPlan) = size(p.p)

show(io::IO, p::ScaledPlan) = print(io, p.scale, " * ", p.p)
summary(p::ScaledPlan) = string(p.scale, " * ", summary(p.p))

*(p::ScaledPlan, x::AbstractArray) = scale!(p.p * x, p.scale)

*(α::Number, p::Plan) = ScaledPlan(p, α)
*(p::Plan, α::Number) = ScaledPlan(p, α)
*(I::UniformScaling, p::ScaledPlan) = ScaledPlan(p, I.λ)
*(p::ScaledPlan, I::UniformScaling) = ScaledPlan(p, I.λ)
*(I::UniformScaling, p::Plan) = ScaledPlan(p, I.λ)
*(p::Plan, I::UniformScaling) = ScaledPlan(p, I.λ)

# Normalization for ifft, given unscaled bfft, is 1/prod(dimensions)
normalization(T, sz, region) = one(T) / Int(prod([sz...][[region...]]))
normalization(X, region) = normalization(real(eltype(X)), size(X), region)

plan_ifft(x::AbstractArray, region; kws...) =
    ScaledPlan(plan_bfft(x, region; kws...), normalization(x, region))
plan_ifft!(x::AbstractArray, region; kws...) =
    ScaledPlan(plan_bfft!(x, region; kws...), normalization(x, region))

plan_inv(p::ScaledPlan) = ScaledPlan(plan_inv(p.p), inv(p.scale))

A_mul_B!(y::AbstractArray, p::ScaledPlan, x::AbstractArray) =
    scale!(p.scale, A_mul_B!(y, p.p, x))

##############################################################################
# Real-input DFTs are annoying because the output has a different size
# than the input if we want to gain the full factor-of-two(ish) savings
# For backward real-data transforms, we must specify the original length
# of the first dimension, since there is no reliable way to detect this
# from the data (we can't detect whether the dimension was originally even
# or odd).

for f in (:brfft, :irfft)
    pf = Symbol("plan_", f)
    @eval begin
        $f(x::AbstractArray, d::Integer) = $pf(x, d) * x
        $f(x::AbstractArray, d::Integer, region) = $pf(x, d, region) * x
        $pf(x::AbstractArray, d::Integer;kws...) = $pf(x, d, 1:ndims(x);kws...)
    end
end

for f in (:brfft, :irfft)
    @eval begin
        $f(x::AbstractArray{<:Real}, d::Integer, region=1:ndims(x)) = $f(complexfloat(x), d, region)
        $f(x::AbstractArray{<:Complex{<:Union{Integer,Rational}}}, d::Integer, region=1:ndims(x)) = $f(complexfloat(x), d, region)
    end
end

"""
    irfft(A, d [, dims])

Inverse of [`rfft`](@ref): for a complex array `A`, gives the corresponding real
array whose FFT yields `A` in the first half. As for [`rfft`](@ref), `dims` is an
optional subset of dimensions to transform, defaulting to `1:ndims(A)`.

`d` is the length of the transformed real array along the `dims[1]` dimension, which must
satisfy `div(d,2)+1 == size(A,dims[1])`. (This parameter cannot be inferred from `size(A)`
since both `2*size(A,dims[1])-2` as well as `2*size(A,dims[1])-1` are valid sizes for the
transformed real array.)
"""
irfft

"""
    brfft(A, d [, dims])

Similar to [`irfft`](@ref) but computes an unnormalized inverse transform (similar
to [`bfft`](@ref)), which must be divided by the product of the sizes of the
transformed dimensions (of the real output array) in order to obtain the inverse transform.
"""
brfft

function rfft_output_size(x::AbstractArray, region)
    d1 = first(region)
    osize = [size(x)...]
    osize[d1] = osize[d1]>>1 + 1
    return osize
end

function brfft_output_size(x::AbstractArray, d::Integer, region)
    d1 = first(region)
    osize = [size(x)...]
    @assert osize[d1] == d>>1 + 1
    osize[d1] = d
    return osize
end

plan_irfft(x::AbstractArray{Complex{T}}, d::Integer, region; kws...) where {T} =
    ScaledPlan(plan_brfft(x, d, region; kws...),
               normalization(T, brfft_output_size(x, d, region), region))

"""
    plan_irfft(A, d [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

Pre-plan an optimized inverse real-input FFT, similar to [`plan_rfft`](@ref)
except for [`irfft`](@ref) and [`brfft`](@ref), respectively. The first
three arguments have the same meaning as for [`irfft`](@ref).
"""
plan_irfft

##############################################################################

export fftshift, ifftshift

fftshift(x) = circshift(x, div.([size(x)...],2))

"""
    fftshift(x)

Swap the first and second halves of each dimension of `x`.
"""
fftshift(x)

function fftshift(x,dim)
    s = zeros(Int,ndims(x))
    for i in dim
        s[i] = div(size(x,i),2)
    end
    circshift(x, s)
end

"""
    fftshift(x,dim)

Swap the first and second halves of the given dimension or iterable of dimensions of array `x`.
"""
fftshift(x,dim)

ifftshift(x) = circshift(x, div.([size(x)...],-2))

"""
    ifftshift(x, [dim])

Undoes the effect of `fftshift`.
"""
ifftshift

function ifftshift(x,dim)
    s = zeros(Int,ndims(x))
    for i in dim
        s[i] = -div(size(x,i),2)
    end
    circshift(x, s)
end

##############################################################################

# FFTW module (may move to an external package at some point):
"""
    fft(A [, dims])

Performs a multidimensional FFT of the array `A`. The optional `dims` argument specifies an
iterable subset of dimensions (e.g. an integer, range, tuple, or array) to transform along.
Most efficient if the size of `A` along the transformed dimensions is a product of small
primes; see `nextprod()`. See also `plan_fft()` for even greater efficiency.

A one-dimensional FFT computes the one-dimensional discrete Fourier transform (DFT) as
defined by

```math
\\operatorname{DFT}(A)[k] =
  \\sum_{n=1}^{\\operatorname{length}(A)}
  \\exp\\left(-i\\frac{2\\pi
  (n-1)(k-1)}{\\operatorname{length}(A)} \\right) A[n].
```

A multidimensional FFT simply performs this operation along each transformed dimension of `A`.

!!! note
    * Julia starts FFTW up with 1 thread by default. Higher performance is usually possible by
      increasing number of threads. Use `FFTW.set_num_threads(Sys.CPU_CORES)` to use as many
      threads as cores on your system.

    * This performs a multidimensional FFT by default. FFT libraries in other languages such as
      Python and Octave perform a one-dimensional FFT along the first non-singleton dimension
      of the array. This is worth noting while performing comparisons. For more details,
      refer to the [Noteworthy Differences from other Languages](@ref)
      section of the manual.
"""
fft

"""
    plan_rfft(A [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

Pre-plan an optimized real-input FFT, similar to [`plan_fft`](@ref) except for
[`rfft`](@ref) instead of [`fft`](@ref). The first two arguments, and the
size of the transformed result, are the same as for [`rfft`](@ref).
"""
plan_rfft

"""
    plan_brfft(A, d [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

Pre-plan an optimized real-input unnormalized transform, similar to
[`plan_rfft`](@ref) except for [`brfft`](@ref) instead of
[`rfft`](@ref). The first two arguments and the size of the transformed result, are
the same as for [`brfft`](@ref).
"""
plan_brfft

module FFTW
    import ..DFT: fft, bfft, ifft, rfft, brfft, irfft, plan_fft, plan_bfft, plan_ifft,
        plan_rfft, plan_brfft, plan_irfft, fft!, bfft!, ifft!, plan_fft!, plan_bfft!, plan_ifft!,
        Plan, rfft_output_size, brfft_output_size, plan_inv, normalization, ScaledPlan

    export r2r, r2r!, plan_r2r, plan_r2r!

    """
        plan_dct!(A [, dims [, flags [, timelimit]]])

    Same as [`plan_dct`](@ref), but operates in-place on `A`.
    """
    function plan_dct! end

    """
        plan_idct(A [, dims [, flags [, timelimit]]])

    Pre-plan an optimized inverse discrete cosine transform (DCT), similar to
    [`plan_fft`](@ref) except producing a function that computes
    [`idct`](@ref). The first two arguments have the same meaning as for
    [`idct`](@ref).
    """
    function plan_idct end

    """
        plan_dct(A [, dims [, flags [, timelimit]]])

    Pre-plan an optimized discrete cosine transform (DCT), similar to
    [`plan_fft`](@ref) except producing a function that computes
    [`dct`](@ref). The first two arguments have the same meaning as for
    [`dct`](@ref).
    """
    function plan_dct end

    """
        plan_idct!(A [, dims [, flags [, timelimit]]])

    Same as [`plan_idct`](@ref), but operates in-place on `A`.
    """
    function plan_idct! end

    """
        dct(A [, dims])

    Performs a multidimensional type-II discrete cosine transform (DCT) of the array `A`, using
    the unitary normalization of the DCT. The optional `dims` argument specifies an iterable
    subset of dimensions (e.g. an integer, range, tuple, or array) to transform along.  Most
    efficient if the size of `A` along the transformed dimensions is a product of small primes;
    see [`nextprod`](@ref). See also [`plan_dct`](@ref) for even greater
    efficiency.
    """
    function dct end

    """
        idct(A [, dims])

    Computes the multidimensional inverse discrete cosine transform (DCT) of the array `A`
    (technically, a type-III DCT with the unitary normalization). The optional `dims` argument
    specifies an iterable subset of dimensions (e.g. an integer, range, tuple, or array) to
    transform along.  Most efficient if the size of `A` along the transformed dimensions is a
    product of small primes; see [`nextprod`](@ref).  See also
    [`plan_idct`](@ref) for even greater efficiency.
    """
    function idct end

    """
        dct!(A [, dims])

    Same as [`dct!`](@ref), except that it operates in-place on `A`, which must be an
    array of real or complex floating-point values.
    """
    function dct! end

    """
        idct!(A [, dims])

    Same as [`idct!`](@ref), but operates in-place on `A`.
    """
    function idct! end

    """
        r2r(A, kind [, dims])

    Performs a multidimensional real-input/real-output (r2r) transform
    of type `kind` of the array `A`, as defined in the FFTW manual.
    `kind` specifies either a discrete cosine transform of various types
    (`FFTW.REDFT00`, `FFTW.REDFT01`, `FFTW.REDFT10`, or
    `FFTW.REDFT11`), a discrete sine transform of various types
    (`FFTW.RODFT00`, `FFTW.RODFT01`, `FFTW.RODFT10`, or
    `FFTW.RODFT11`), a real-input DFT with halfcomplex-format output
    (`FFTW.R2HC` and its inverse `FFTW.HC2R`), or a discrete
    Hartley transform (`FFTW.DHT`).  The `kind` argument may be
    an array or tuple in order to specify different transform types
    along the different dimensions of `A`; `kind[end]` is used
    for any unspecified dimensions.  See the FFTW manual for precise
    definitions of these transform types, at http://www.fftw.org/doc.

    The optional `dims` argument specifies an iterable subset of
    dimensions (e.g. an integer, range, tuple, or array) to transform
    along. `kind[i]` is then the transform type for `dims[i]`,
    with `kind[end]` being used for `i > length(kind)`.

    See also [`plan_r2r`](@ref) to pre-plan optimized r2r transforms.
    """
    function r2r end

    """
        r2r!(A, kind [, dims])

    Same as [`r2r`](@ref), but operates in-place on `A`, which must be
    an array of real or complex floating-point numbers.
    """
    function r2r! end

    """
        plan_r2r!(A, kind [, dims [, flags [, timelimit]]])

    Similar to [`plan_fft`](@ref), but corresponds to [`r2r!`](@ref).
    """
    function plan_r2r! end

    """
        plan_r2r(A, kind [, dims [, flags [, timelimit]]])

    Pre-plan an optimized r2r transform, similar to [`plan_fft`](@ref)
    except that the transforms (and the first three arguments)
    correspond to [`r2r`](@ref) and [`r2r!`](@ref), respectively.
    """
    function plan_r2r end

    (Base.USE_GPL_LIBS || Base.fftw_vendor() == :mkl) && include(joinpath("fft", "FFTW.jl"))
end

importall .FFTW
export FFTW, dct, idct, dct!, idct!, plan_dct, plan_idct, plan_dct!, plan_idct!

##############################################################################

end

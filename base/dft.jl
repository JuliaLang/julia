# This file is a part of Julia. License is MIT: http://julialang.org/license

module DFT

# DFT plan where the inputs are an array of eltype T
abstract Plan{T}

import Base: show, summary, size, ndims, length, eltype,
             *, mul!, inv, \, A_ldiv_B!

eltype{T}(::Type{Plan{T}}) = T

# size(p) should return the size of the input array for p
size(p::Plan, d) = size(p)[d]
ndims(p::Plan) = length(size(p))
length(p::Plan) = prod(size(p))::Int

##############################################################################
export fft, ifft, bfft, fft!, ifft!, bfft!,
       plan_fft, plan_ifft, plan_bfft, plan_fft!, plan_ifft!, plan_bfft!,
       rfft, irfft, brfft, plan_rfft, plan_irfft, plan_brfft

complexfloat{T<:AbstractFloat}(x::AbstractArray{Complex{T}}) = x

# return an Array, rather than similar(x), to avoid an extra copy for FFTW
# (which only works on StridedArray types).
complexfloat{T<:Complex}(x::AbstractArray{T}) = copy!(Array(typeof(float(one(T))), size(x)), x)
complexfloat{T<:AbstractFloat}(x::AbstractArray{T}) = copy!(Array(typeof(complex(one(T))), size(x)), x)
complexfloat{T<:Real}(x::AbstractArray{T}) = copy!(Array(typeof(complex(float(one(T)))), size(x)), x)

# implementations only need to provide plan_X(x, region)
# for X in (:fft, :bfft, ...):
for f in (:fft, :bfft, :ifft, :fft!, :bfft!, :ifft!, :rfft)
    pf = symbol(string("plan_", f))
    @eval begin
        $f(x::AbstractArray) = $pf(x) * x
        $f(x::AbstractArray, region) = $pf(x, region) * x
        $pf(x::AbstractArray; kws...) = $pf(x, 1:ndims(x); kws...)
    end
end

doc"""
```rst
..  plan_ifft(A [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

Same as :func:`plan_fft`, but produces a plan that performs inverse transforms
:func:`ifft`.
```
"""
plan_ifft

doc"""
```rst
..  plan_ifft!(A [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

Same as :func:`plan_ifft`, but operates in-place on ``A``.
```
"""
plan_ifft!

doc"""
```rst
..  plan_bfft!(A [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

Same as :func:`plan_bfft`, but operates in-place on ``A``.
```
"""
plan_bfft!

doc"""
```rst
..  plan_bfft(A [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

Same as :func:`plan_fft`, but produces a plan that performs an unnormalized
backwards transform :func:`bfft`.
```
"""
plan_bfft

doc"""
```rst
..  plan_fft(A [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

Pre-plan an optimized FFT along given dimensions (``dims``) of arrays
matching the shape and type of ``A``.  (The first two arguments have
the same meaning as for :func:`fft`.)  Returns an object ``P`` which
represents the linear operator computed by the FFT, and which contains
all of the information needed to compute ``fft(A, dims)`` quickly.

To apply ``P`` to an array ``A``, use ``P * A``; in general, the
syntax for applying plans is much like that of matrices.  (A plan
can only be applied to arrays of the same size as the ``A`` for
which the plan was created.)  You can also apply a plan with a
preallocated output array ``Â`` by calling ``A_mul_B!(Â, plan,
A)``.  You can compute the inverse-transform plan by ``inv(P)`` and
apply the inverse plan with ``P \ Â`` (the inverse plan is cached
and reused for subsequent calls to ``inv`` or ``\``), and apply the
inverse plan to a pre-allocated output array ``A`` with
``A_ldiv_B!(A, P, Â)``.

The ``flags`` argument is a bitwise-or of FFTW planner flags, defaulting
to ``FFTW.ESTIMATE``.  e.g. passing ``FFTW.MEASURE`` or ``FFTW.PATIENT``
will instead spend several seconds (or more) benchmarking different
possible FFT algorithms and picking the fastest one; see the FFTW manual
for more information on planner flags.  The optional ``timelimit`` argument
specifies a rough upper bound on the allowed planning time, in seconds.
Passing ``FFTW.MEASURE`` or ``FFTW.PATIENT`` may cause the input array ``A``
to be overwritten with zeros during plan creation.

:func:`plan_fft!` is the same as :func:`plan_fft` but creates a plan
that operates in-place on its argument (which must be an array of
complex floating-point numbers).  :func:`plan_ifft` and so on
are similar but produce plans that perform the equivalent of
the inverse transforms :func:`ifft` and so on.
```
"""
plan_fft

doc"""
```rst
..  plan_fft!(A [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

Same as :func:`plan_fft`, but operates in-place on ``A``.
```
"""
plan_fft!

doc"""
```rst
..  rfft(A [, dims])

Multidimensional FFT of a real array ``A``, exploiting the fact that
the transform has conjugate symmetry in order to save roughly half
the computational time and storage costs compared with :func:`fft`.
If ``A`` has size ``(n_1, ..., n_d)``, the result has size
``(div(n_1,2)+1, ..., n_d)``.

The optional ``dims`` argument specifies an iterable subset of one or
more dimensions of ``A`` to transform, similar to :func:`fft`.  Instead
of (roughly) halving the first dimension of ``A`` in the result, the
``dims[1]`` dimension is (roughly) halved in the same way.
```
"""
rfft

doc"""
```rst
..  ifft!(A [, dims])

Same as :func:`ifft`, but operates in-place on ``A``.
```
"""
ifft!

doc"""
    ifft(A [, dims])

Multidimensional inverse FFT.

A one-dimensional inverse FFT computes

$$\operatorname{IDFT}(A)[k] = \frac{1}{\operatorname{length}(A)}
\sum_{n=1}^{\operatorname{length}(A)} \exp\left(+i\frac{2\pi (n-1)(k-1)}
{\operatorname{length}(A)} \right) A[n].$$

A multidimensional inverse FFT simply performs this operation along each transformed dimension of `A`.
"""
ifft

doc"""
```rst
..  fft!(A [, dims])

Same as :func:`fft`, but operates in-place on ``A``,
which must be an array of complex floating-point numbers.
```
"""
fft!

doc"""
```rst
..  bfft(A [, dims])

Similar to :func:`ifft`, but computes an unnormalized inverse (backward)
transform, which must be divided by the product of the sizes of the
transformed dimensions in order to obtain the inverse. (This is slightly
more efficient than :func:`ifft` because it omits a scaling step, which in
some applications can be combined with other computational steps elsewhere.)

.. math::

   \operatorname{BDFT}(A)[k] = \operatorname{length}(A) \operatorname{IDFT}(A)[k]
```
"""
bfft

doc"""
```rst
..  bfft!(A [, dims])

Same as :func:`bfft`, but operates in-place on ``A``.
```
"""
bfft!

# promote to a complex floating-point type (out-of-place only),
# so implementations only need Complex{Float} methods
for f in (:fft, :bfft, :ifft)
    pf = symbol(string("plan_", f))
    @eval begin
        $f{T<:Real}(x::AbstractArray{T}, region=1:ndims(x)) = $f(complexfloat(x), region)
        $pf{T<:Real}(x::AbstractArray{T}, region; kws...) = $pf(complexfloat(x), region; kws...)
        $f{T<:Union{Integer,Rational}}(x::AbstractArray{Complex{T}}, region=1:ndims(x)) = $f(complexfloat(x), region)
        $pf{T<:Union{Integer,Rational}}(x::AbstractArray{Complex{T}}, region; kws...) = $pf(complexfloat(x), region; kws...)
    end
end
rfft{T<:Union{Integer,Rational}}(x::AbstractArray{T}, region=1:ndims(x)) = rfft(float(x), region)
plan_rfft{T<:Union{Integer,Rational}}(x::AbstractArray{T}, region; kws...) = plan_rfft(float(x), region; kws...)

# only require implementation to provide *(::Plan{T}, ::Array{T})
*{T}(p::Plan{T}, x::AbstractArray) = p * copy!(Array(T, size(x)), x)

# Implementations should also implement mul!(Y, plan, X) so as to support
# pre-allocated output arrays.  We don't define * in terms of mul!
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
A_ldiv_B!(y::AbstractArray, p::Plan, x::AbstractArray) = mul!(y, inv(p), x)

##############################################################################
# implementations only need to provide the unnormalized backwards FFT,
# similar to FFTW, and we do the scaling generically to get the ifft:

type ScaledPlan{T,P,N} <: Plan{T}
    p::P
    scale::N # not T, to avoid unnecessary promotion to Complex
    pinv::Plan
    ScaledPlan(p, scale) = new(p, scale)
end
call{T,P,N}(::Type{ScaledPlan{T}}, p::P, scale::N) = ScaledPlan{T,P,N}(p, scale)
ScaledPlan{T}(p::Plan{T}, scale::Number) = ScaledPlan{T}(p, scale)
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
normalization(T, sz, region) = (one(T) / prod([sz...][[region...]]))::T
normalization(X, region) = normalization(real(eltype(X)), size(X), region)

plan_ifft(x::AbstractArray, region; kws...) =
    ScaledPlan(plan_bfft(x, region; kws...), normalization(x, region))
plan_ifft!(x::AbstractArray, region; kws...) =
    ScaledPlan(plan_bfft!(x, region; kws...), normalization(x, region))

plan_inv(p::ScaledPlan) = ScaledPlan(plan_inv(p.p), inv(p.scale))

mul!(y::AbstractArray, p::ScaledPlan, x::AbstractArray) =
    scale!(p.scale, mul!(y, p.p, x))

##############################################################################
# Real-input DFTs are annoying because the output has a different size
# than the input if we want to gain the full factor-of-two(ish) savings
# For backward real-data transforms, we must specify the original length
# of the first dimension, since there is no reliable way to detect this
# from the data (we can't detect whether the dimension was originally even
# or odd).

for f in (:brfft, :irfft)
    pf = symbol(string("plan_", f))
    @eval begin
        $f(x::AbstractArray, d::Integer) = $pf(x, d) * x
        $f(x::AbstractArray, d::Integer, region) = $pf(x, d, region) * x
        $pf(x::AbstractArray, d::Integer;kws...) = $pf(x, d, 1:ndims(x);kws...)
    end
end

for f in (:brfft, :irfft)
    @eval begin
        $f{T<:Real}(x::AbstractArray{T}, d::Integer, region=1:ndims(x)) = $f(complexfloat(x), d, region)
        $f{T<:Union{Integer,Rational}}(x::AbstractArray{Complex{T}}, d::Integer, region=1:ndims(x)) = $f(complexfloat(x), d, region)
    end
end

doc"""
```rst
..  irfft(A, d [, dims])

Inverse of :func:`rfft`: for a complex array ``A``, gives the
corresponding real array whose FFT yields ``A`` in the first half.
As for :func:`rfft`, ``dims`` is an optional subset of dimensions
to transform, defaulting to ``1:ndims(A)``.

``d`` is the length of the transformed real array along the ``dims[1]``
dimension, which must satisfy ``div(d,2)+1 == size(A,dims[1])``.
(This parameter cannot be inferred from ``size(A)`` since both
``2*size(A,dims[1])-2`` as well as ``2*size(A,dims[1])-1`` are valid sizes
for the transformed real array.)
```
"""
irfft

doc"""
```rst
..  brfft(A, d [, dims])

Similar to :func:`irfft` but computes an unnormalized inverse transform
(similar to :func:`bfft`), which must be divided by the product
of the sizes of the transformed dimensions (of the real output array)
in order to obtain the inverse transform.
```
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

plan_irfft{T}(x::AbstractArray{Complex{T}}, d::Integer, region; kws...) =
    ScaledPlan(plan_brfft(x, d, region; kws...),
               normalization(T, brfft_output_size(x, d, region), region))

doc"""
```rst
..  plan_irfft(A, d [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

Pre-plan an optimized inverse real-input FFT, similar to :func:`plan_rfft`
except for :func:`irfft` and :func:`brfft`, respectively.  The first
three arguments have the same meaning as for :func:`irfft`.
```
"""
plan_irfft

##############################################################################

export fftshift, ifftshift

fftshift(x) = circshift(x, div([size(x)...],2))

doc"""
    fftshift(x)

Swap the first and second halves of each dimension of `x`.
"""
fftshift(x)

function fftshift(x,dim)
    s = zeros(Int,ndims(x))
    s[dim] = div(size(x,dim),2)
    circshift(x, s)
end

doc"""
    fftshift(x,dim)

Swap the first and second halves of the given dimension of array `x`.
"""
fftshift(x,dim)

ifftshift(x) = circshift(x, div([size(x)...],-2))

doc"""
    ifftshift(x, [dim])

Undoes the effect of `fftshift`.
"""
ifftshift

function ifftshift(x,dim)
    s = zeros(Int,ndims(x))
    s[dim] = -div(size(x,dim),2)
    circshift(x, s)
end

##############################################################################

# FFTW module (may move to an external package at some point):
if Base.USE_GPL_LIBS
    include("fft/FFTW.jl")
    importall .FFTW
    export FFTW, dct, idct, dct!, idct!, plan_dct, plan_idct, plan_dct!, plan_idct!
end

##############################################################################

end

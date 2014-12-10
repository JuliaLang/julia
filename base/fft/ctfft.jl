# 1d Cooley-Tukey FFTs, using an FFTW-like (version 1) approach: automatic
# generation of fixed-size FFT kernels (with and without twiddle factors)
# which are combined to make arbitrary-size FFTs (plus generic base
# cases for large prime factors).

#############################################################################
# Combining pregenerated kernels into generic-size FFT plans:

# now, we define a CTPlan (Cooley-Tukey plan) as a sequence of twiddle steps
# followed by a nontwiddle step:

abstract TwiddleStep{T}
abstract NontwiddleStep{T}

type CTPlan{T,forward,Tt,Tn} <: Plan{T}
    n::Int
    tsteps::Tt # a tuple of TwiddleSteps
    nstep::Tn # a NontwiddleStep
    pinv::ScaledPlan{T}
    CTPlan(n,tsteps,nstep) = new(n,tsteps,nstep)
end

summary{T,forw}(p::CTPlan{T,forw}) = string(forw ? "for" : "back",
                                            "ward CTPlan{$T} of size ", p.n)
function show(io::IO, p::CTPlan)
    if p.n == 0
        print(io, "(no transform)") # untransformed dims in multi-dim plans
    else
        ns = length(p.tsteps)
        print(io, summary(p), ":\n    ", length(p.tsteps),
              " Cooley-Tukey step", ns == 0 ? "s." : ns == 1 ? ": " : "s: ",
              join(map(string, p.tsteps), ", "), "\n    base case: ", p.nstep)
    end
end

size(p::CTPlan) = (p.n,)

# unscaled inverse:
invCT{T,forward,Tt,Tn}(p::CTPlan{T,forward,Tt,Tn}) =
    CTPlan{T,!forward,map(inv,Tt),inv(Tn)}(p.n, map(inv, p.tsteps), inv(p.nstep))

plan_inv{T}(p::CTPlan{T}) =
    ScaledPlan(invCT(p), normalization(real(T), p.n, 1))

# steps for pregenerated kernels:
immutable TwiddleKernelStep{T,r,forward} <: TwiddleStep{T} # radix-r
    m::Int # n / r
    W::Matrix{T}
    TwiddleKernelStep(m::Int, W::Matrix{T}) = new(m, W)
    function TwiddleKernelStep(n::Int)
        m = div(n, r)
        Tr = promote_type(Float64, real(T))
        twopi_n = -2(π/convert(Tr,n))
        W = T[exp((twopi_n*mod(j1*k2,n))*im) for j1=1:r-1, k2=0:m-1]
        new(m, W)
    end
end
length{T,r}(ts::TwiddleKernelStep{T,r}) = r
applystep{T}(ts::TwiddleKernelStep{T}, y::AbstractArray{T}, y0, ys) =
    applystep(ts, ts.m, y, y0, ts.m * ys, ys, ts.W)
inv{T,r,forward}(ts::TwiddleKernelStep{T,r,forward}) =
    TwiddleKernelStep{T,r,!forward}(ts.m, ts.W)
inv{T,r,forward}(::Type{TwiddleKernelStep{T,r,forward}}) =
    TwiddleKernelStep{T,r,!forward}
show{T,r}(io::IO, ts::TwiddleKernelStep{T,r}) = print(io, "radix ", r)

immutable NontwiddleKernelStep{T,n,forward} <: NontwiddleStep{T} end
length{T,n}(ns::NontwiddleKernelStep{T,n}) = n
applystep{T,n}(ns::NontwiddleKernelStep{T,n}, r::Integer,
               x::AbstractArray{T}, x0::Integer, xs::Integer,
               y::AbstractArray{T}, y0::Integer, ys::Integer) =
    applystep(ns, r, x,x0,xs*r,xs, y,y0,ys,ys*n)
inv{T,n,forward}(ns::NontwiddleKernelStep{T,n,forward}) =
    NontwiddleKernelStep{T,n,!forward}()
inv{T,n,forward}(::Type{NontwiddleKernelStep{T,n,forward}}) =
    NontwiddleKernelStep{T,n,!forward}
show{T,n}(io::IO, ns::NontwiddleKernelStep{T,n}) =
    print(io, "size ", n, " kernel")

# create null plans for untransformed dimensions and 0-size arrays
immutable NullNontwiddleStep{T} <: NontwiddleStep{T} end
applystep(::NullNontwiddleStep, r, x, x0, xs, y, y0, ys) = nothing
show(io::IO, ::NullNontwiddleStep) = print(io, "null transform")
inv(s::NullNontwiddleStep) = s
inv(::Type{NullNontwiddleStep}) = NullNontwiddleStep
CTPlan(T,forward) =
    CTPlan{T,forward,(),NullNontwiddleStep{T}}(0, (), NullNontwiddleStep{T}())

function CTPlan{Tr<:FloatingPoint}(::Type{Complex{Tr}}, forward::Bool, n::Int)
    T = Complex{Tr}
    n == 0 && return CTPlan(T,forward)
    factors = fft_factors(T, n)
    m = n
    tsteps = Array(TwiddleStep{T}, length(factors)-1)
    for i = 1:length(tsteps)
        tsteps[i] = Twiddle(T, m, factors[i], forward)
        m = tsteps[i].m
    end
    @assert m == factors[end]
    tsteps_ = tuple(tsteps...)
    nt = Nontwiddle(T, m, forward)
    CTPlan{T,forward,map(typeof,tsteps_),typeof(nt)}(n, tsteps_, nt)
end

plan_fft{Tr<:FloatingPoint}(x::AbstractVector{Complex{Tr}}) =
    CTPlan(Complex{Tr}, true, length(x))
plan_bfft{Tr<:FloatingPoint}(x::AbstractVector{Complex{Tr}}) =
    CTPlan(Complex{Tr}, false, length(x))

function applystep{T}(p::CTPlan{T},
                      x::AbstractArray{T}, x0, xs,
                      y::AbstractArray{T}, y0, ys,
                      step::Int)
    nsteps = length(p.tsteps)
    if step > nsteps
        applystep(p.nstep, 1, x,x0,xs, y,y0,ys)
    else
        # decimation in time: perform r DFTs of length m
        tstep = p.tsteps[step]
        m = tstep.m
        r = length(tstep)
        if step == nsteps
            applystep(p.nstep, r, x,x0,xs, y,y0,ys)
        else
            xs_ = xs*r
            x0_ = x0
            y0_ = y0
            for i = 1:r-1
                applystep(p, x,x0_,xs_, y,y0_,ys, step+1)
                x0_ += xs
                y0_ += m
            end
            applystep(p, x,x0_,xs_, y,y0_,ys, step+1)
        end
        # combine sub-transforms with twiddle step:
        applystep(tstep, y,y0,ys)
    end
end

function A_mul_B!{T}(y::AbstractVector{T}, p::CTPlan{T}, x::AbstractVector{T})
    p.n == length(y) == length(x) || throw(BoundsError())
    applystep(p, x,1,1, y,1,1, 1)
    return y
end

*{T}(p::CTPlan{T}, x::AbstractVector{T}) = A_mul_B!(similar(x), p, x)

#############################################################################
# FFT code generation:

# Choose the default radix for pregenerated FFT of length `n`.  To get
# a cache-oblivious algorithm (for register usage), we will choose the
# biggest factor of `n` that is $\le \sqrt{n}$.  However, we also prefer
# relatively prime factors since they allow us to use PFA.
function choose_radix(n)
    # biggest relatively prime factor
    for r = isqrt(n):-1:2
        if n % r == 0 && gcd(r, div(n, r)) == 1
            return r
        end
    end
    # biggest factor <= sqrt(n)
    for r = isqrt(n):-1:2
        if n % r == 0
            return r
        end
    end
    return 1
end

# `twiddle(T, forward, n, k, x)` multiplies the expression `x` by
# $\omega_n^k$, where $\omega_n$ is the `n`-th primitive root of unity
# for the field `T`, returning a new expression.  For complex `T`,
# $\omega_n = \exp(s*2\pi i/n)$, where `s=-1` for `forward=true` and
# `s=+1` for `forward=false`.  (These are traditionally called
# "twiddle factors" in FFT algorithms.) Note also that algebraic
# simplifications occur for several `k`.
#
# (In FFTW's generator, we first generate all expressions in terms of
# real arithmetic and then apply a simplifier to eliminate
# multiplications by `1` and similar.  That is a more powerful
# approach, but this is quicker to implement.)
function twiddle{Tr<:FloatingPoint}(T::Type{Complex{Tr}}, forward::Bool, n::Integer, k::Integer, x)
    k == 0 && return x
    2k == n && return :(-$x)
    if 4k == n || 4k == 3n
        tmpvar = gensym("twiddle")
        if (4k == n && !forward) || (4k == 3n && forward) # ω = +im
            return :(let $tmpvar=$x; complex(-imag($tmpvar), real($tmpvar)); end)
        else  # ω = -im
            return :(let $tmpvar=$x; complex(imag($tmpvar), -real($tmpvar)); end)
        end
    end
    if 8k == n || 8k == 3n || 8k == 5n || 8k == 7n # ω = [(1,-1), (-1,-1), (-1,1), or (1,1)] * sqrt(0.5)
        sqrthalf = sqrt(one(Tr)/2)
        tmpvar = gensym("twiddle")
        if (8k == n && forward) || (8k == 7n && !forward)
            return :(let $tmpvar=$x; $sqrthalf*complex(real($tmpvar)+imag($tmpvar), imag($tmpvar)-real($tmpvar)); end)
        elseif (8k == 3n && forward) || (8k == 5n && !forward)
            return :(let $tmpvar=$x; $sqrthalf*complex(imag($tmpvar)-real($tmpvar), -real($tmpvar)-imag($tmpvar)); end)
        elseif (8k == 5n && forward) || (8k == 3n && !forward)
            return :(let $tmpvar=$x; $sqrthalf*complex(-real($tmpvar)-imag($tmpvar), real($tmpvar)-imag($tmpvar)); end)
        elseif (8k == 7n && forward) || (8k == n && !forward)
            return :(let $tmpvar=$x; $sqrthalf*complex(real($tmpvar)-imag($tmpvar), real($tmpvar)+imag($tmpvar)); end)
        end
    end
    # make sure trig factors are correctly rounded:
    c, s = with_bigfloat_precision(2*precision(one(Tr))) do
        φ = (big(2) * k) / n
        convert(Tr, cospi(φ)), convert(Tr, sinpi(φ))
    end
    ω = complex(c, forward ? -s : s)
    return :($ω * $x)
end

# Like `fftgen`, below, but generates the naïve $\Theta(n^2)$ DFT algorithm:
function dftgen(T, forward::Bool, n::Integer, x, y)
    n == 1 && return :($(y(0)) = $(x(0)))
    tmpvars = Symbol[ gensym(string("dftgen_", j)) for j in 0:n-1 ]
    n == 2 && return Expr(:let, Expr(:block, :($(y(0)) = $(tmpvars[1]) + $(tmpvars[2])), :($(y(1)) = $(tmpvars[1]) - $(tmpvars[2]))), :($(tmpvars[1]) = $(x(0))), :($(tmpvars[2]) = $(x(1))))
    Expr(:let,
         Expr(:block, [:($(y(k)) = $(Expr(:call, :+, [twiddle(T, forward, n, j*k, tmpvars[j+1]) for j in 0:n-1]...))) for k=0:n-1]...),
         [:($(tmpvars[j+1]) = $(x(j))) for j = 0:n-1]...)
end

# `fftgen(n, true, x, y)` generates an expression (`Expr`) for an FFT
# of length `n` with inputs `x(i)` and outputs `y(i)` for `i=0:n-1`.
# For `forward=false`, the unscaled backward transform is returned.
# Note that `x` and `y` are *functions* returning expressions.  `T` is
# the type of the field over which we perform the DFT,
# e.g. `Complex{Float64}`.
function fftgen(T, forward::Bool, n::Integer, x, y)
    r = choose_radix(n)
    r == 1 && return dftgen(T, forward, n, x, y)
    m = div(n, r)

    # store results of first r sub-transforms in r*m temporary variables
    z = Symbol[ gensym(string("fftgen_", j1, "_", k2))
               for j1 = 0:r-1, k2 = 0:m-1 ]

    if gcd(r,m) == 1 # radix-r Prime-Factor (PFA) step
        ri = invmod(r, m)*r
        mi = invmod(m, r)*m
        # get expressions to perform r sub-FFTs of length m
        Fm = Expr[ fftgen(T, forward, m,
                          j2 -> x(mod(r*j2+m*j1, n)),
                          k2 -> z[j1+1,k2+1])
                   for j1 = 0:r-1 ]
        # get expressions to perform m sub-FFTs of length r
        Fr = Expr[ fftgen(T, forward, r,
                          j1 -> z[j1+1,k2+1],
                          k1 -> y(mod(mi*k1+ri*k2, n)))
                   for k2 in 0:m-1 ]
    else # radix-r Cooley-Tukey step
        # get expressions to perform r sub-FFTs of length m
        Fm = Expr[ fftgen(T, forward, m,
                          j2 -> x(r*j2+j1), k2 -> z[j1+1,k2+1])
                   for j1 = 0:r-1 ]
        # get expressions to perform m sub-FFTs of length r
        Fr = Expr[ fftgen(T, forward, r,
                          j1 -> twiddle(T, forward, n, j1*k2, z[j1+1,k2+1]),
                          k1 -> y(m*k1+k2))
                   for k2 in 0:m-1 ]
    end
    Expr(:block, [Expr(:local, Z) for Z in z]..., Fm..., Fr...)
end
fftgen(T, forward::Bool, n::Integer, X::Symbol, Y::Symbol) = fftgen(T, forward, n, j -> :($X[$(j+1)]), k -> :($Y[$(k+1)]))

# Analogous to FFTW's nontwiddle codelets (direct solvers), we
# generate a bunch of solvers for small fixed sizes.  Each solver is
# of the form `fft_N(vn, X, x0, xs, xvs, Y, y0, ys, yvs)` and
# computes `i in 0:vn-1` transforms, with the `i`-th transform
# performing `X[x0 + xvs*i + (0:N-1)*xs] = fft(Y[x0 + yvs*i +
# (0:N-1)*ys])`.  Each such solver is generated by the
# `@nontwiddle(T,forward,n)` macro:
macro nontwiddle(T, forward, n)
    quote
        function $(esc(:applystep)){T<:$T}(ns::NontwiddleKernelStep{Complex{T},$n,$forward},
                                  vn::Integer,
                                  X::AbstractArray{Complex{T}},
                                  x0::Integer, xs::Integer, xvs::Integer,
                                  Y::AbstractArray{Complex{T}},
                                  y0::Integer, ys::Integer, yvs::Integer)
            @inbounds @simd for i in 0:vn-1
                $(fftgen(Complex{eval(T)}, forward, n,
                         j -> :(X[(x0 + xvs*i) + xs*$j]),
                         k -> :(Y[(y0 + yvs*i) + ys*$k])))
            end
            Y
        end
    end
end

# compute conj(z) * w
mulconj(z::Complex, w::Complex) =
    Complex(real(z) * real(w) + imag(z) * imag(w),
            real(z) * imag(w) - imag(z) * real(w))

# Analogous to FFTW's twiddle codelets, we also generate solvers that
# perform *in-place* FFTs of small fixed sizes where the data is
# pre-multipled by a precomputed 2d array `W[j+1,i+1]` of twiddle
# factors (with `W[1,_] = 1`).  These are of the form `twiddle_N(vn, X,
# x0, xs, xvs, W)`, and the meaning of the parameter is otherwise
# identical to the nontwiddle codelets with `Y=X`.   The twiddle array
# is the same for both forward and backward transforms, and is conjugated
# as needed.
macro twiddle(T, forward, n)
    quote
        function  $(esc(:applystep)){T<:$T}(ts::TwiddleKernelStep{Complex{T},$n,$forward},
                                   vn::Integer,
                                   X::AbstractArray{Complex{T}},
                                   x0::Integer, xs::Integer, xvs::Integer,
                                   W::AbstractMatrix{Complex{T}})
            @inbounds @simd for i in 0:vn-1
                $(fftgen(Complex{eval(T)}, forward, n,
                         j -> j == 0 ? :(X[(x0 + xvs*i) + xs*$j]) :
                              forward ? :(W[$j,i+1] * X[(x0 + xvs*i) + xs*$j]) : :(mulconj(W[$j,i+1], X[(x0 + xvs*i) + xs*$j])),
                         j -> :(X[(x0 + xvs*i) + xs*$j])))
            end
            X
        end
    end
end

# Now, we will generate nontwiddle and twiddle kernels for a set of
# fixed sizes, to be composed to build an arbitrary-$n$ FFT algorithm.
const fft_kernel_sizes = Set([3, 5:10..., 12, 14, 15, 16, 20, 25, 32])
for forward in (true,false)
    for n in fft_kernel_sizes
        for T in (Float32, Float64)
            @eval @nontwiddle($T, $forward, $n)
            @eval @twiddle($T, $forward, $n)
        end
    end
end
const fft_kernel_sizes_sorted = sort!(Int[n for n in fft_kernel_sizes],
                                      rev=true)

typealias CTComplex Union(Complex64,Complex128) # types of pregenerated kernels

# The above kernels are only for single- and double-precision, since they
# include hard-coded transcendental constants.  The following kernels
# are generic to any floating-point type.
const generic_kernel_sizes = Set([1,2,4])
for n in (1,2,4)
    @eval @nontwiddle(FloatingPoint, true, $n)
    @eval @nontwiddle(FloatingPoint, false, $n)
end
for forward in (true,false)
    @eval @twiddle(FloatingPoint, $forward, 2)
    @eval @twiddle(FloatingPoint, $forward, 4)
end

#############################################################################
# Generic solver for arbitrary prime (or nonprime) factors using
# Bluestein's algorithm.  For a reasonable description of this
# algorithm (written mostly by SGJ), see e.g.
#      http://en.wikipedia.org/wiki/Bluestein%27s_FFT_algorithm
# We follow the same notation for the arrays a and b as in that article.

# compute the b_j = b[j+1] array used in Bluestein's algorithm
function bluestein_b(T, forward, n, n2)
    b = zeros(T, n2)
    Tr = promote_type(Float64, real(T))
    pi_n = forward ? π/convert(Tr,n) : -(π/convert(Tr,n))
    # embed inverse-DFT scaling factor 1/n2 in b array:
    scale = inv(cbrt(convert(Tr,n2)))
    b[1] = scale
    for i = 1:n-1
        b[i+1] = b[n2-i+1] = exp((pi_n * mod(i*i,2n))*im) * scale
    end
    return b
end

immutable NontwiddleBluesteinStep{T} <: NontwiddleStep{T}
    n::Int   # DFT size to be computed
    n2::Int  # nextpow2(2n-1)
    p::Plan{T}  # plan for DFT of length n2

    # the following arrays are of length n2, used to compute the convolution
    a::Vector{T} # storage for x[j] * b[j]', zero-padded
    A::Vector{T} # storage to be used for DFT of a
    b::Vector{T} # b[j+1] = exp(πi j^2 / n), wrapped symmetrically
    B::Vector{T} # DFT(b)

    forward::Bool

    function NontwiddleBluesteinStep(n::Int, forward::Bool)
        n2 = nextpow2(2n-1)
        a = Array(T, n2)
        p = plan_fft(a)
        A = Array(T, n2)
        b = bluestein_b(T, forward, n, n2)
        B = p * b
        new(n, n2, p, a, A, b, B, forward)
    end
end

length(s::NontwiddleBluesteinStep) = s.n

inv{T}(s::NontwiddleBluesteinStep{T}) =
    NontwiddleBluesteinStep{T}(s.n, !s.forward)
inv{T}(S::Type{NontwiddleBluesteinStep{T}}) = S

show(io::IO, s::NontwiddleBluesteinStep) =
    print(io, "size ", s.n, " Bluestein-", s.n2)

function applystep{T}(ns::NontwiddleBluesteinStep{T}, r,
                      x::AbstractArray{T}, x0, xs,
                      y::AbstractArray{T}, y0, ys)
    a = ns.a
    b = ns.b
    A = ns.A
    B = ns.B
    z = zero(T)
    xs_ = xs*r
    ys_ = ys*ns.n
    for i = 1:r
        @inbounds for j = 1:ns.n
            a[j] = x[x0 + xs_*(j-1)] * b[j]'
        end
        @inbounds for j = ns.n+1:ns.n2
            a[j] = z
        end
        # conv(a,b) = ifft(fft(a) .* B), where ifft -> bfft because
        # the 1/n2 scaling was included in b and B, and we use the
        # identity bfft(x) = conj(fft(conj(x))) so that we can re-use ns.p:
        A_mul_B!(A, ns.p, a)
        @inbounds for j = 1:ns.n2
            A[j] = (A[j] * B[j])'
        end
        A_mul_B!(a, ns.p, A)
        @inbounds for j = 1:ns.n
            y[y0 + ys*(j-1)] = (b[j] * a[j])'
        end
        x0 += xs
        y0 += ys_
    end
end

# Similar to above, but a Bluestein-based twiddle step:

immutable TwiddleBluesteinStep{T} <: TwiddleStep{T}
    r::Int # radix
    m::Int # n / r
    W::Array{T} # twiddle factors

    r2::Int  # nextpow2(2r-1)
    p::Plan{T}  # plan for DFT of length r2

    # the following arrays are of length r2, used to compute the convolution
    a::Vector{T} # storage for x[j] * b[j]', zero-padded
    A::Vector{T} # storage to be used for DFT of a
    b::Vector{T} # b[j+1] = exp(πi j^2 / n), wrapped symmetrically
    B::Vector{T} # DFT(b)

    forward::Bool

    function TwiddleBluesteinStep(n::Int, r::Int, forward::Bool)
        m = div(n, r)
        Tr = promote_type(Float64, real(T))
        twopi_n = forward ? -2(π/convert(Tr,n)) : 2(π/convert(Tr,n))
        r2 = nextpow2(2r-1)
        a = Array(T, r2)
        p = plan_fft(a)
        A = Array(T, r2)
        b = bluestein_b(T, forward, r, r2)
        B = p * b
        new(r, m,
            T[exp((twopi_n*mod(j1*k2,n))*im) for j1=1:r-1, k2=0:m-1],
            r2, p, a, A, b, B, forward)
    end
end

length(s::TwiddleBluesteinStep) = s.r

show(io::IO, s::TwiddleBluesteinStep) =
    print(io, "radix ", s.r, " Bluestein-", s.r2)

inv{T}(s::TwiddleBluesteinStep{T}) =
    TwiddleBluesteinStep{T}(s.r*s.m, s.r, !s.forward)
inv{T}(S::Type{TwiddleBluesteinStep{T}}) = S

function applystep{T}(ts::TwiddleBluesteinStep{T}, y::AbstractArray{T}, y0,ys)
    W = ts.W
    a = ts.a
    b = ts.b
    A = ts.A
    B = ts.B
    z = zero(T)
    ys_ = ys*ts.m
    for i = 1:ts.m
        a[1] = y[y0] * b[1]'
        @inbounds for j = 2:ts.r
            a[j] = W[j-1,i] * y[y0 + ys_*(j-1)] * b[j]'
        end
        @inbounds for j = ts.r+1:ts.r2
            a[j] = z
        end
        # conv(a,b) = ifft(fft(a) .* B), where ifft -> bfft because
        # the 1/n2 scaling was included in b and B, and we use the
        # identity bfft(x) = conj(fft(conj(x))) so that we can re-use ts.p:
        A_mul_B!(A, ts.p, a)
        @inbounds for j = 1:ts.r2
            A[j] = (A[j] * B[j])'
        end
        A_mul_B!(a, ts.p, A)
        @inbounds for j = 1:ts.r
            y[y0 + ys_*(j-1)] = (b[j] * a[j])'
        end
        y0 += ys
    end
end

#############################################################################
# Step selection for CTPlan:

Nontwiddle(T, n::Int, forw::Bool) = (T <: CTComplex && n in fft_kernel_sizes) || n in generic_kernel_sizes ? NontwiddleKernelStep{T,n,forw}() : NontwiddleBluesteinStep{T}(n, forw)

Twiddle(T, n::Int, r::Int, forw::Bool) = (T <: CTComplex && r in fft_kernel_sizes) || r == 4 || r == 2 ? TwiddleKernelStep{T,r,forw}(n) : TwiddleBluesteinStep{T}(n, r, forw)

# Break n into a series of factors for type T, avoiding small kernels
# if possible
function fft_factors(T::Type, n::Integer)
    factors = Int[]
    if n == 1
        push!(factors, 1)
    else
        m = n
        if T <: CTComplex
            for r in fft_kernel_sizes_sorted
                while m % r == 0
                    push!(factors, r)
                    m = div(m, r)
                end
                m == 1 && break
            end
        end
        # generic radix-4 and radix-2 twiddle kernels:
        while m % 4 == 0
            push!(factors, 4)
            m = div(m, 4)
        end
        if iseven(m)
            push!(factors, 2)
            m = div(m, 2)
        end
        # sometimes there will be a small factor (e.g. 2) left over at the end;
        # try to combine this with larger earlier factors if possible:
        if length(factors) > 1
            for i = 1:length(factors)-1
                factors[end] >= 16 && break
                while factors[i] % 2 == 0 && div(factors[i], 2) > factors[end]
                    factors[i] = div(factors[i], 2)
                    factors[end] *= 2
                end
            end
        end
        # get any leftover prime factors:
        for (f,k) in factor(m)
            for i=1:k
                push!(factors, f)
            end
        end
    end
    factors
end

#############################################################################

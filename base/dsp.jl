module DSP

importall Base.FFTW
import Base.FFTW.normalization

export FFTW, filt, filt!, deconv, conv, conv2, xcorr, fftshift, ifftshift,
       dct, idct, dct!, idct!, plan_dct, plan_idct, plan_dct!, plan_idct!,
       # the rest are defined imported from FFTW:
       fft, bfft, ifft, rfft, brfft, irfft,
       plan_fft, plan_bfft, plan_ifft, plan_rfft, plan_brfft, plan_irfft,
       fft!, bfft!, ifft!, plan_fft!, plan_bfft!, plan_ifft!

function filt{T<:Number}(b::Union(AbstractVector{T}, T), a::Union(AbstractVector{T}, T), x::AbstractVector{T})
    filt!(b, a, copy!(Array(T, size(x)), x))
end
function filt{T<:Number}(b::Union(AbstractVector{T}, T), a::Union(AbstractVector{T}, T), x::AbstractVector{T}, si::AbstractVector{T})
    filt!(b, a, copy!(Array(T, size(x)), x), copy!(Array(T, size(si)), si))
end

# in-place filtering; both the input and filter state are modified in-place
function filt!{T<:Number}(b::Union(AbstractVector{T}, T), a::Union(AbstractVector{T}, T),
                          x::AbstractVector{T}, si::AbstractVector{T}=zeros(T, max(length(a), length(b))-1))
    if isempty(b); error("b must be non-empty"); end
    if isempty(a); error("a must be non-empty"); end
    if a[1]==0; error("a[1] must be nonzero"); end

    as = length(a)
    bs = length(b)
    sz = max(as, bs)

    if sz == 1
        # Simple scaling without memory; quick exit
        return scale!(x, b[1]/a[1])
    end

    if bs<sz
        # Ensure b has at least as many elements as a
        newb = zeros(T,sz)
        newb[1:bs] = b
        b = newb
    end

    xs = size(x,1)
    silen = sz-1
    size(si) == (silen,) || error("the vector of initial conditions must have exactly max(length(a),length(b))-1 elements")

    if a[1] != 1
        # Normalize the coefficients such that a[1] == 1
        norml = a[1]
        a ./= norml
        b ./= norml
    end

    if as > 1
        if as<sz
            # Pad a to be the same length as b
            newa = zeros(T,sz)
            newa[1:as] = a
            a = newa
        end

        @inbounds begin
            for i=1:xs
                val = si[1] + b[1]*x[i]
                for j=1:(silen-1)
                    si[j] = si[j+1] + b[j+1]*x[i] - a[j+1]*val
                end
                si[silen] = b[silen+1]*x[i] - a[silen+1]*val
                x[i] = val
            end
        end
    else
        @inbounds begin
            for i=1:xs
                val = si[1] + b[1]*x[i]
                for j=1:(silen-1)
                    si[j] = si[j+1] + b[j+1]*x[i]
                end
                si[silen] = b[silen+1]*x[i]
                x[i] = val
            end
        end
    end
    return x
end

function deconv{T}(b::StridedVector{T}, a::StridedVector{T})
    lb = size(b,1)
    la = size(a,1)
    if lb < la
        return [zero(T)]
    end
    lx = lb-la+1
    x = zeros(T, lx)
    x[1] = 1
    filt(b, a, x)
end

function conv{T<:Base.LinAlg.BlasFloat}(u::StridedVector{T}, v::StridedVector{T})
    nu = length(u)
    nv = length(v)
    n = nu + nv - 1
    np2 = n > 1024 ? nextprod([2,3,5], n) : nextpow2(n)
    upad = [u, zeros(T, np2 - nu)]
    vpad = [v, zeros(T, np2 - nv)]
    if T <: Real
        p = plan_rfft(upad)
        y = irfft(p(upad).*p(vpad), np2)
    else
        p = plan_fft!(upad)
        y = ifft!(p(upad).*p(vpad))
    end
    return y[1:n]
end
conv{T<:Integer}(u::StridedVector{T}, v::StridedVector{T}) = int(conv(float(u), float(v)))
conv{T<:Integer, S<:Base.LinAlg.BlasFloat}(u::StridedVector{T}, v::StridedVector{S}) = conv(float(u), v)
conv{T<:Integer, S<:Base.LinAlg.BlasFloat}(u::StridedVector{S}, v::StridedVector{T}) = conv(u, float(v))

function conv2{T}(u::StridedVector{T}, v::StridedVector{T}, A::StridedMatrix{T})
    m = length(u)+size(A,1)-1
    n = length(v)+size(A,2)-1
    B = zeros(T, m, n)
    B[1:size(A,1),1:size(A,2)] = A
    u = fft([u;zeros(T,m-length(u))])
    v = fft([v;zeros(T,n-length(v))])
    C = ifft(fft(B) .* (u * v.'))
    if T <: Real
        return real(C)
    end
    return C
end

function conv2{T}(A::StridedMatrix{T}, B::StridedMatrix{T})
    sa, sb = size(A), size(B)
    At = zeros(T, sa[1]+sb[1]-1, sa[2]+sb[2]-1)
    Bt = zeros(T, sa[1]+sb[1]-1, sa[2]+sb[2]-1)
    At[1:sa[1], 1:sa[2]] = A
    Bt[1:sb[1], 1:sb[2]] = B
    p = plan_fft(At)
    C = ifft(p(At).*p(Bt))
    if T <: Real
        return real(C)
    end
    return C
end
conv2{T<:Integer}(A::StridedMatrix{T}, B::StridedMatrix{T}) = int(conv2(float(A), float(B)))
conv2{T<:Integer}(u::StridedVector{T}, v::StridedVector{T}, A::StridedMatrix{T}) = int(conv2(float(u), float(v), float(A)))

function xcorr(u, v)
    su = size(u,1); sv = size(v,1)
    if su < sv
        u = [u;zeros(eltype(u),sv-su)]
    elseif sv < su
        v = [v;zeros(eltype(v),su-sv)]
    end
    flipud(conv(flipud(u), v))
end

fftshift(x) = circshift(x, div([size(x)...],2))

function fftshift(x,dim)
    s = zeros(Int,ndims(x))
    s[dim] = div(size(x,dim),2)
    circshift(x, s)
end

ifftshift(x) = circshift(x, div([size(x)...],-2))

function ifftshift(x,dim)
    s = zeros(Int,ndims(x))
    s[dim] = -div(size(x,dim),2)
    circshift(x, s)
end

# Discrete cosine and sine transforms via FFTW's r2r transforms;
# we follow the Matlab convention and adopt a unitary normalization here.
# Unlike Matlab we compute the multidimensional transform by default,
# similar to the Julia fft functions.

fftwcopy{T<:fftwNumber}(X::StridedArray{T}) = copy(X)
fftwcopy{T<:Real}(X::StridedArray{T}) = float(X)
fftwcopy{T<:Complex}(X::StridedArray{T}) = complex128(X)

for (f, fr2r, Y, Tx) in ((:dct, :r2r, :Y, :Number), 
                         (:dct!, :r2r!, :X, :fftwNumber))
    plan_f = symbol(string("plan_",f))
    plan_fr2r = symbol(string("plan_",fr2r))
    fi = symbol(string("i",f))
    plan_fi = symbol(string("plan_",fi))
    Ycopy = Y == :X ? 0 : :(Y = fftwcopy(X))
    @eval begin
        function $f{T<:$Tx}(X::StridedArray{T}, region)
            $Y = $fr2r(X, REDFT10, region)
            scale!($Y, sqrt(0.5^length(region) * normalization(X,region)))
            sqrthalf = sqrt(0.5)
            r = map(n -> 1:n, [size(X)...])
            for d in region
                r[d] = 1:1
                $Y[r...] *= sqrthalf
                r[d] = 1:size(X,d)
            end
            return $Y
        end

        function $plan_f{T<:$Tx}(X::StridedArray{T}, region,
                                 flags::Unsigned, timelimit::Real)
            p = $plan_fr2r(X, REDFT10, region, flags, timelimit)
            sqrthalf = sqrt(0.5)
            r = map(n -> 1:n, [size(X)...])
            nrm = sqrt(0.5^length(region) * normalization(X,region))
            return X::StridedArray{T} -> begin
                $Y = p(X)
                scale!($Y, nrm)
                for d in region
                    r[d] = 1:1
                    $Y[r...] *= sqrthalf
                    r[d] = 1:size(X,d)
                end
                return $Y
            end
        end

        function $fi{T<:$Tx}(X::StridedArray{T}, region)
            $Ycopy
            scale!($Y, sqrt(0.5^length(region) * normalization(X, region)))
            sqrt2 = sqrt(2)
            r = map(n -> 1:n, [size(X)...])
            for d in region
                r[d] = 1:1
                $Y[r...] *= sqrt2
                r[d] = 1:size(X,d)
            end
            return r2r!($Y, REDFT01, region)
        end

        function $plan_fi{T<:$Tx}(X::StridedArray{T}, region,
                                 flags::Unsigned, timelimit::Real)
            p = $plan_fr2r(X, REDFT01, region, flags, timelimit)
            sqrt2 = sqrt(2)
            r = map(n -> 1:n, [size(X)...])
            nrm = sqrt(0.5^length(region) * normalization(X,region))
            return X::StridedArray{T} -> begin
                $Ycopy
                scale!($Y, nrm)
                for d in region
                    r[d] = 1:1
                    $Y[r...] *= sqrt2
                    r[d] = 1:size(X,d)
                end
                return p($Y)
            end
        end

    end
    for (g,plan_g) in ((f,plan_f), (fi, plan_fi))
        @eval begin
            $g{T<:$Tx}(X::StridedArray{T}) = $g(X, 1:ndims(X))
            
            $plan_g(X, region, flags::Unsigned) =
              $plan_g(X, region, flags, NO_TIMELIMIT)
            $plan_g(X, region) =
              $plan_g(X, region, ESTIMATE, NO_TIMELIMIT)
            $plan_g{T<:$Tx}(X::StridedArray{T}) =
              $plan_g(X, 1:ndims(X), ESTIMATE, NO_TIMELIMIT)
        end
    end
end

# DCT of scalar is just the identity:
dct(x::Number, dims) = length(dims) == 0 || dims[1] == 1 ? x : throw(BoundsError())
idct(x::Number, dims) = dct(x, dims)
dct(x::Number) = x
idct(x::Number) = x
plan_dct(x::Number, dims, flags, tlim) = length(dims) == 0 || dims[1] == 1 ? y::Number -> y : throw(BoundsError())
plan_idct(x::Number, dims, flags, tlim) = plan_dct(x, dims)
plan_dct(x::Number) = y::Number -> y
plan_idct(x::Number) = y::Number -> y

end # module

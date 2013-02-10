module DSP

importall Base.FFTW
import Base.FFTW.normalization

export FFTW, filt, deconv, conv, conv2, xcorr, fftshift, ifftshift,
       dct, idct, dct!, idct!, plan_dct, plan_idct, plan_dct!, plan_idct!,
       # the rest are defined imported from FFTW:
       fft, bfft, ifft, rfft, brfft, irfft,
       plan_fft, plan_bfft, plan_ifft, plan_rfft, plan_brfft, plan_irfft,
       fft!, bfft!, ifft!, plan_fft!, plan_bfft!, plan_ifft!

function filt(b,a,x)
    if a[1]==0
        error("filt: a[1] must be nonzero")
    end

    sz = max(size(a,1),size(b,1))

    if sz == 1
        return (b[1]/a[1]).*x
    end

    if size(a,1)<sz
        newa = zeros(eltype(a),sz)
        newa[1:size(a,1)] = a
        a = newa
    end
    if size(b,1)<sz
        newb = zeros(eltype(b),sz)
        newb[1:size(b,1)] = b
        b = newb
    end

    xs = size(x,1)
    y = Array(eltype(a), xs)
    silen = sz-1
    si = zeros(eltype(a), silen)

    if a[1] != 1
        norml = a[1]
        a ./= norml
        b ./= norml
    end

    if sz > 1
        for i=1:xs
            y[i] = si[1] + b[1]*x[i]
            for j=1:(silen-1)
                si[j] = si[j+1] + b[j+1]*x[i] - a[j+1]*y[i]
            end
            si[silen] = b[silen+1]*x[i] - a[silen+1]*y[i]
        end
    else
        for i=1:xs
            y[i] = si[1] + b[1]*x[i]
            for j=1:(silen-1)
                si[j] = si[j+1] + b[j+1]*x[i]
            end
            si[silen] = b[silen+1]*x[i]
        end
    end
    return y
end

function deconv{T}(b::Vector{T}, a::Vector{T})
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

function conv{T}(u::Vector{T}, v::Vector{T})
    n = size(u,1)+size(v,1)-1
    u, v = [u;zeros(T,size(v,1)-1)], [v;zeros(T,size(u,1)-1)]
    p = plan_fft(u)
    y = ifft(p(u).*p(v))
    if T <: Real
        return real(y)
    end
    return y
end

function conv2{T}(y::Vector{T}, x::Vector{T}, A::Matrix{T})
    m = length(y)+size(A,1)-1
    n = length(x)+size(A,2)-1
    B = zeros(T, m, n)
    B[1:size(A,1),1:size(A,2)] = A
    y = fft([y;zeros(T,m-length(y))])
    x = fft([x;zeros(T,n-length(x))])
    C = ifft(fft(B) .* (y * x.'))
    if T <: Real
        return real(C)
    end
    return C
end

function conv2{T}(A::Matrix{T}, B::Matrix{T})
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
dct(x::Number, dims) = length(dims) == 0 || dims[1] == 1 ? x : throw(BoundsError())x
idct(x::Number, dims) = dct(x, dims)
dct(x::Number) = x
idct(x::Number) = x
plan_dct(x::Number, dims, flags, tlim) = length(dims) == 0 || dims[1] == 1 ? y::Number -> y : throw(BoundsError())
plan_idct(x::Number, dims, flags, tlim) = plan_dct(x, dims)
plan_dct(x::Number) = y::Number -> y
plan_idct(x::Number) = y::Number -> y

end # module

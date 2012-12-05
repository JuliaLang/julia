module DSP

using Base.FFTW

export FFTW, filt, deconv, conv, conv2, xcorr, fftshift, ifftshift,
       # the rest are defined imported from FFTW:
       bfft, bfftn, brfft, brfftn, fft, fft2, fft3, fftn,
       ifft, ifft2, ifft3, ifftn, irfft, irfftn, rfft, rfftn

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
    y = ifft(fft(u).*fft(v))
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
    C = ifft2(fft2(B) .* (y * x.'))
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
    C = ifft2(fft2(At).*fft2(Bt))
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

end # module

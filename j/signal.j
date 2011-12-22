function filter(b,a,x)
    if a[1]==0
        error("filter: a[1] must be nonzero")
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
    filter(b, a, x)
end

function conv{T}(u::Vector{T}, v::Vector{T})
    n = size(u,1)+size(v,1)-1
    u, v = [u;zeros(T,size(v,1)-1)], [v;zeros(T,size(u,1)-1)]
    y = ifft(fft(u).*fft(v))./n
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
    y = fft([y;zeros(T,m-length(y))])./m
    Y = repmat(reshape(y,m,1), 1, n)
    x = fft([x;zeros(T,n-length(x))])./n
    X = repmat(reshape(x,1,n), m, 1)
    C = ifft2(fft(fft(B,(),2).*X,(),1).*Y)
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

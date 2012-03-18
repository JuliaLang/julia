mean(v::AbstractArray) = sum(v) / numel(v)
mean(v::AbstractArray, dim::Int) = sum(v,dim) / size(v,dim)

function std(v::AbstractVector)
    n = numel(v)
    m = mean(v)
    s = 0.0
    for i=1:n
        s += (v[i]-m)^2
    end
    return sqrt(s/(n-1))
end

median(v::AbstractVector) = select(v, div(numel(v)+1,2))

## hist ##

function hist(v::StridedVector, nbins::Integer)
    h = zeros(Int, nbins)
    if nbins == 0
        return h
    end
    lo, hi = min(v), max(v)
    if lo == hi
        lo = lo - div(nbins,2)
        hi = hi + div(nbins,2)
    end
    binsz = (hi-lo)/nbins
    for x in v
        if isfinite(x)
            i = iround((x-lo+binsz/2)/binsz)
            h[i > nbins ? nbins : i] += 1
        end
    end
    h
end

hist(x) = hist(x, 10)

function hist(A::StridedMatrix, nbins::Integer)
    m, n = size(A)
    h = Array(Int, nbins, n)
    for j=1:n
        i = 1+(j-1)*m
        h[:,j] = hist(sub(A, i:(i+m-1)), nbins)
    end
    h
end

function histc(v::StridedVector, edg)
    n = length(edg)
    h = zeros(Int, n)
    first = edg[1]
    last = edg[n]
    for x in v
        if !isless(last, x) && !isless(x, first)
            i = searchsorted(edg, x)
            while isless(x, edg[i])
                i -= 1
            end
            h[i] += 1
        end
    end
    h
end

function histc(A::StridedMatrix, edg)
    m, n = size(A)
    h = Array(Int, length(edg), n)
    for j=1:n
        i = 1+(j-1)*m
        h[:,j] = histc(sub(A, i:(i+m-1)), edg)
    end
    h
end


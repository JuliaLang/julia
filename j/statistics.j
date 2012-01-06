mean(V::AbstractVector) = sum(V) / length(V)

mean(V::AbstractArray, dim::Int) = sum(V,dim) / size(V,dim)

function std(V::AbstractVector)
    n = numel(V)
    m = mean(V)
    s = 0.0
    for i=1:n
        s += (V[i] - m)^2
    end
    return sqrt(s/(n-1))
end

median(V::AbstractVector) = select(V, div(numel(V),2))

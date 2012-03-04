mean(v::AbstractVector) = sum(v) / length(v)
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

median(v::AbstractVector) = select(v, div(numel(v),2))

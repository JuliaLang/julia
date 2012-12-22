function cell_1d(xs::ANY...)
    n = length(xs)
    a = Array(Any,n)
    for i=1:n
        arrayset(a,xs[i],i)
    end
    a
end

function cell_2d(nr, nc, xs::ANY...)
    a = Array(Any,nr,nc)
    for i=1:(nr*nc)
        arrayset(a,xs[i],i)
    end
    a
end

# map cell array
map(f, a::Array{Any,1}) = { f(a[i]) for i=1:length(a) }
map(f, a::Array{Any,1}, b::Array{Any,1}) =
    { f(a[i],b[i]) for i=1:length_checked_equal(a, b) }
function map(f, as::Array{Any,1}...)
    n = length_checked_equal(as...)
    { f(map(a->a[i],as)...) for i=1:n }
end

cell(dims::(Integer...))   = Array(Any, map(int, dims))
cell(dims::Integer...)     = Array(Any, map(int, dims))
cell(d1::Integer)          = Array(Any, int(d1))
cell(d1::Integer, d2::Integer) = Array(Any, int(d1), int(d2))

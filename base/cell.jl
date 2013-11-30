# map cell array
map(f::Callable, a::Array{Any,1}) = { f(a[i]) for i=1:length(a) }
map(f::Callable, a::Array{Any,1}, b::Array{Any,1}) =
    { f(a[i],b[i]) for i=1:length_checked_equal(a, b) }
function map(f::Callable, as::Array{Any,1}...)
    n = length_checked_equal(as...)
    { f(map(a->a[i],as)...) for i=1:n }
end

cell(dims::(Integer...))   = Array(Any, map(int, dims))
cell(dims::Integer...)     = Array(Any, map(int, dims))
cell(d1::Integer)          = Array(Any, int(d1))
cell(d1::Integer, d2::Integer) = Array(Any, int(d1), int(d2))

setindex!(A::Array{Any}, x::ANY, i::Real) = arrayset(A, x, to_index(i))

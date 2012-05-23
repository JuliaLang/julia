# these are used by the front end to implement {} and backquote
function append(a1::Array{Any,1}, as::Array{Any,1}...)
    n = arraylen(a1)
    for i = 1:length(as)
        n += arraylen(as[i])
    end
    a = Array(Any,n)
    for i = 1:arraylen(a1)
        arrayset(a,i,arrayref(a1,i))
    end
    i = arraylen(a1)+1
    for x in as
        for j = 1:length(x)
            arrayset(a,i,x[j])
            i += 1
        end
    end
    a
end

function cell_1d(xs::ANY...)
    n = length(xs)
    a = Array(Any,n)
    for i=1:n
        arrayset(a,i,xs[i])
    end
    a
end

function cell_2d(nr, nc, xs::ANY...)
    a = Array(Any,nr,nc)
    for i=1:(nr*nc)
        arrayset(a,i,xs[i])
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

struct Array[T,ndims] <: Tensor[T,ndims]
    dims::Buffer[Size]
    data::Buffer[T]
end

typealias Vector[T] Tensor[T,1]
typealias Matrix[T] Tensor[T,2]

## Basic functions
numel(a::Array) = length(a.data)

length[T](v::Array[T,1]) = length(v.data)
length[T](a::Array[T,2]) = max(a.dims)

size(a::Array) = a.dims
size(a::Array, d) = a.dims[d]

zeros(sz...) = jl_make_array(sz...)
zeros(T::Type, sz...) = jl_make_array(T, sz...)

ones(m::Size) = [ 1 | i=1:m ]
ones(m::Size, n::Size) = [ 1 | i=1:m, j=1:n ]

rand(m::Size) = [ rand() | i=1:m ]
rand(m::Size, n::Size) = [ rand() | i=1:m, j=1:n ]

(+)[T](x::Array[T,1], y::Array[T,1]) = [ x[i] + y[i] | i=1:numel(x) ]
(+)[T](x::Array[T,2], y::Array[T,2]) = [ x[i,j] + y[i,j] | i=1:x.dims[1], j=1:x.dims[2] ]

(==)(x::Array, y::Array) = x.dims == y.dims && x.data == y.data

transpose[T](x::Array[T,2]) = [ x[j,i] | i=1:x.dims[2], j=1:x.dims[1] ]
ctranspose[T](x::Array[T,2]) = [ conj(x[j,i]) | i=1:x.dims[2], j=1:x.dims[1] ]

function jl_make_array(eltype::Type, dim...)
    ndims = length(dim)
    dims = Buffer[Size].new(ndims)
    numel = 1
    for i=1:ndims; dims[i] = dim[i]; numel = numel*dim[i]; end
    data = Buffer[eltype].new(numel)
    Array[eltype,ndims].new(dims, data)
end

jl_make_array(dim...) = jl_make_array(Float64, dim...)

function colon(start::Int32, stop::Int32, stride::Int32)
    len = div((stop-start),stride) + 1
    x = zeros(Int32, len)
    ind = 1
    for i=start:stride:stop
        x[ind] = i
        ind = ind+1
    end
    return x
end

## Indexing: ref()
ref[T](a::Array[T,1], i::Index) = a.data[i]
ref[T](a::Array[T,2], i::Index, j::Index) = a.data[(j-1)*a.dims[1] + i]

jl_fill_endpts(A, n, R::RangeBy) = range(1, R.step, size(A, n))
jl_fill_endpts(A, n, R::RangeFrom) = range(R.start, R.step, size(A, n))
jl_fill_endpts(A, n, R::RangeTo) = range(1, R.step, R.stop)
jl_fill_endpts(A, n, R) = R

ref[T](A::Array[T,1], I) = [ A[i] | i = jl_fill_endpts(A, 1, I) ]
ref[T](A::Array[T,2], I, J) = [ A[i, j] | i = jl_fill_endpts(A, 1, I),
                                          j = jl_fill_endpts(A, 2, J)  ]

function ref(a::Array, I::Index...) 
    # TODO: Need out-of-bounds checks
    data = a.data
    dims = a.dims
    ndims = length(I) - 1

    index = I[1]
    stride = 1
    for k=2:ndims
        stride = stride * dims[k-1]
        index += (I[k]-1) * stride
    end

    return data[index]
end

# Indexing: set()
set[T](a::Array[T,1], x, i::Index) = do (a.data[i] = x, a)
set[T](a::Array[T,2], x, i::Index, j::Index) = do (a.data[(j-1)*a.dims[1] + i] = x, a)

function set(a::Array, x, I::Index...)
    # TODO: Need to take care of growing
    data = a.data
    dims = a.dims
    ndims = length(I)

    index = I[1]
    stride = 1
    for k=2:ndims
        stride = stride * dims[k-1]
        index += (I[k]-1) * stride
    end

    data[index] = x
    return a
end

function set[T](A::Array[T,1], X, I)
    # TODO: Need to take care of growing
    I = jl_fill_endpts(A, 1, I)

    if isscalar(X)
        for i=I; A[i] = X; end;
    end

    return A
end

function set[T](A::Array[T,2], X, I, J)
    # TODO: Need to take care of growing
    I = jl_fill_endpts(A, 1, I)
    J = jl_fill_endpts(A, 2, J)

    if isscalar(X)
        for i=I; for j=J; A[i,j] = X; end; end
    end

    return A
end

# Concatenation
cat(x::Scalar...) = [ x[i] | i=1:length(x) ]
hcat(x::Scalar...) = [ x[j] | i=1, j=1:length(x) ]
vcat(x::Scalar...) = [ x[i] | i=1:length(x), j=1 ]

vector[T](elts::T...) = cat(elts...)

# iterate arrays by iterating data
start(a::Array) = start(a.data)
next(a::Array,i) = next(a.data,i)
done(a::Array,i) = done(a.data,i)

# Print arrays
function print[T](a::Array[T,1])
    n = a.dims[1]

    if n < 10
        for i=1:n; print(a[i]); print("\n"); end
    else
        for i=1:3; print(a[i]); print("\n"); end
        print(":\n");
        for i=n-2:n; print(a[i]); print("\n"); end
    end
end

function printcols(a, start, stop, i)
    for j=start:stop; print(a[i,j]); print(" "); end
end

function print[T](a::Array[T,2])

    m = a.dims[1]
    n = a.dims[2]

    print_hdots = false
    print_vdots = false
    if 10 < m; print_vdots = true; end
    if 10 < n; print_hdots = true; end

    if !print_vdots && !print_hdots
        for i=1:m
            printcols(a, 1, n, i)
            print("\n")
        end
        return ()
    elseif print_vdots && !print_hdots
        for i=1:3
            printcols(a, 1, n, i)
            print("\n")
        end
        print(":\n")
        for i=m-2:m
            printcols(a, 1, n, i)
            print("\n")
        end
        return ()
    elseif !print_vdots && print_hdots
        for i=1:m
            printcols (a, 1, 3, i)
            if i == 1 || i == m; print(": "); else; print("  "); end
            printcols (a, n-2, n, i)
            print("\n")
        end
    else
        for i=1:3
            printcols (a, 1, 3, i)
            if i == 1; print(": "); else; print("  "); end
            printcols (a, n-2, n, i)
            print("\n")
        end
        print(":\n")
        for i=m-2:m
            printcols (a, 1, 3, i)
            if i == m; print(": "); else; print("  "); end
            printcols (a, n-2, n, i)
            print("\n")
        end
    end
end # print()

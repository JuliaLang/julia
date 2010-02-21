struct Array[T,ndims] <: Tensor[T,ndims]
    dims:: Buffer[Size]
    data:: Buffer[T]
end

typealias Vector[T] Tensor[T,1]
typealias Matrix[T] Tensor[T,2]

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

function make_array(eltype::Type, dim...)
    ndims = length(dim)
    dims = Buffer[Size].new(ndims)
    numel = 1
    for i=1:ndims; dims[i] = dim[i]; numel = numel*dim[i]; end
    data = Buffer[eltype].new(numel)
    Array[eltype,ndims].new(dims, data)
end

make_array(dim...) = make_array(Float64, dim...)

# This is a temp version to get an integer array of zeros
# until convert() is in place
function zeros_int(dim...)
    ndims = length(dim)
    dims = Buffer[Size].new(ndims)
    numel = 1
    for i=1:ndims; dims[i] = dim[i]; numel = numel*dim[i]; end
    data = Buffer[Int32].new(numel)
    array = Array[Int32,ndims].new(dims, data)
end

size(a::Array) = a.dims

# colon

function colon(start::Int32, stop::Int32, stride::Int32)
    len = div((stop-start),stride) + 1
    x = zeros_int(len)
    ind = 1
    for i=start:stride:stop
        x[ind] = i
        ind = ind+1
    end
    return x
end

## Scalar indexing

ref(a::Array, i::Index) = a.data[i]

ref(a::Array, i::Index, j::Index) = a.data[(j-1)*a.dims[1] + i]

# TODO: Need out-of-bounds checks
function ref(a::Array, I::Index...)
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


function set(a::Array, i::Index, x)
    a.data[i] = x
    return x
end

function set(a::Array, i::Index, j::Index, x)
    m = a.dims[1]
    a.data[(j-1)*m + i] = x
    return x
end

function set(a::Array, I...)
    data = a.data
    dims = a.dims
    ndims = length(I) - 1
    x = I[ndims+1]

    index = I[1]
    stride = 1
    for k=2:ndims
        stride = stride * dims[k-1]
        index += (I[k]-1) * stride
    end

    data[index] = x
    return x
end

## Vector indexing
ref(a::Array, I::Array) = [ a[i] | (i=I) ]


## Other functions
numel(a::Array) = a.data.length
length[T](v::Array[T,1]) = v.data.length
length[T](v::Array[T,2]) = v.data.length
zeros(sz...) = a = make_array(sz...)
ones(m::Size) = [ 1 | (i=1:m) ]
ones(m::Size, n::Size) = [ 1 | (i=1:m), (j=1:n) ]
ones(m::Size, n::Size, o::Size) = [ 1 | (i=1:m), (j=1:n), (k=1:o) ]
rand(m::Size) = [ rand() | (i=1:m) ]
rand(m::Size, n::Size) = [ rand() | (i=1:m), (j=1:n) ]

(+)[T](x::Array[T,1], y::Array[T,1]) = [ x[i] + y[i] | (i=1:numel(x)) ]
(+)[T](x::Array[T,2], y::Array[T,2]) = [ x[i,j] + y[i,j] | (i=1:x.dims[1]), (j=1:x.dims[2]) ]
(+)[T](x::Array[T,3], y::Array[T,3]) = [ x[i,j,k] + y[i,j,k]  | (i=1:x.dims[1]), (j=1:x.dims[2]), (k=1:x.dims[3]) ]

(==)(x::Array, y::Array) = x.dims == y.dims && x.data == y.data

transpose[T](x::Array[T,2]) = [ x[j,i] | (i=1:x.dims[2]), (j=1:x.dims[1]) ]
ctranspose[T](x::Array[T,2]) = [ conj(x[j,i]) | (i=1:x.dims[2]), (j=1:x.dims[1]) ]

function hcat[T](elts::T...)
    n = length(elts)
    if n == 0
        return make_array(0)
    end
    a = make_array(T, n)
    for i=1:n
        a[i] = elts[i]
    end
    a
end

function vector[T](elts::T...)
    v = make_array(T,length(elts))
    for i = 1:length(elts)
        v[i] = elts[i]
    end
    return v
end

function compute_dims(r...)
    n = length(r)
    for i=1:n
        print(length(r[i]))
    end
end

# iterating over vectors
start(v::Vector) = 1
done(v::Vector, i) = (i > numel(v))
next(v::Vector, i) = (v[i], i+1)

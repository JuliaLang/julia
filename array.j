type Array[`T,`ndims] < Tensor[`T,`ndims]
    dims:: Buffer[Size]
    data:: Buffer[`T]
end

typealias Vector Tensor[`T,1]
typealias Matrix Tensor[`T,2]

function print(a::Array[`T,1])
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

function print(a::Array[`T,2])

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
    dims = new(Buffer[Size], ndims)
    numel = 1
    for i=1:ndims; dims[i] = dim[i]; numel = numel*dim[i]; end
    data = new(Buffer[eltype], numel)
    new(Array[eltype,ndims], dims, data)
end

function make_array(dim...)
    return make_array(Double, dim...)
end

# This is a temp version to get an integer array of zeros
# until convert() is in place
function zeros_int(dim...)
    ndims = length(dim)
    dims = new(Buffer[Size], ndims)
    numel = 1
    for i=1:ndims; dims[i] = dim[i]; numel = numel*dim[i]; end
    data = new(Buffer[Int32], numel)
    array = new(Array[Int32,ndims], dims, data)
end

function size(a::Array)
    return a.dims
end

# colon

function colon(start::Int32, stop::Int32, stride::Int32)
    len = div((stop-start),stride) + 1
    x = zeros_int(1, len)
    ind = 1
    for i=start:stride:stop
        x[ind] = i
        ind = ind+1
    end
    return x
end

## One based indexing

function ref(a::Array, i::Index)
    return a.data[i] 
end

function ref(a::Array, I::Array)
    return [ a[i] | (i=I) ]
end

function ref(a::Array, i::Index, j::Index)
    m = a.dims[1]
    return a.data[(j-1)*m + i] 
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

function numel(a::Array)
    return a.data.length
end

function zeros(sz...)
    a = make_array(sz...)
end

function ones(m::Size)
    [ 1 | (i=1:m) ]
end

function ones(m::Size, n::Size)
    [ 1 | (i=1:m), (j=1:n) ]
end

function rand(m::Size)
    [ rand() | (i=1:m) ]
end

function rand(m::Size, n::Size)
    [ rand() | (i=1:m), (j=1:n) ]
end

function +(x::Array[`T,1], y::Array[`T,1])
    n = numel(x)
    return [ x[i] + y[i] | (i=1:n) ]
end

function +(x::Array[`T,2], y::Array[`T,2])
    m = x.dims[1]
    n = x.dims[2]
    return [ x[i,j] + y[i,j] | (i=1:m), (j=1:n) ]
end

function transpose(x::Array[`T,2])
    m = x.dims[1]
    n = x.dims[2]
    return [ x[j,i] | (i=1:n), (j=1:m) ]
end

function ctranspose(x::Array[`T,2])
    return transpose(x)
end

function hcat(elts::`T...)
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

function vector(elts::`T...)
    v = make_array(T,1,length(elts))
    for i = 1:length(elts)
        v[i] = elts[i]
    end
    return v
end

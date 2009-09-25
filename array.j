type Array(T,ndims) < Tensor(T,ndims)
    dims: buffer(size)
    data: buffer(T)
end

typealias Vector Tensor(T,1)
typealias Matrix Tensor(T,2)

function print(a:Array)
    for i=1:a.dims[0]
        for j=1:a.dims[1]
            print(a[i,j])
            print(" ")
        end
        print("\n")
    end
    return ()
end

function make_array(m:int32)
    dims = new(buffer(int32), 1)
    dims[0] = m
    data = new(buffer(double), m)
    array = new(Array(double,1), dims, data)
    return array
end

function make_array(m:int32, n:int32)
    dims = new(buffer(int32), 2)
    dims[0] = m
    dims[1] = n
    data = new(buffer(double), m*n)
    array = new(Array(double,2), dims, data)
    return array
end

## One based indexing

function ref(a:Array, i:int32)
    return a.data[i-1] 
end

function ref(a:Array, i:int32, j:int32)
    m = a.dims[0]
    return a.data[(j-1)*m + (i-1)] 
end

function set(a:Array, i:int32, x)
    bufferset(a.data, unbox(i-1), x) 
    return x
end

function set(a:Array, i:int32, j:int32, x)
    m = a.dims[0]
    pos = (j-1)*m + (i-1)
    bufferset(a.data, unbox(pos), x)
    return x
end

function numel(a:Array)
    return a.data.length
end

function zeros(m:int32)
    a = make_array(m)
    return a
end

function zeros(m:int32, n:int32)
    a = make_array(m, n)
    return a
end

function ones(m:int32)
    return [ 1 | (i=1:m) ]
end

function ones(m:int32, n:int32)
    return [ 1 | (i=1:m), (j=1:n) ]
end

function `+`(x:Array(T,1), y:Array(T,1))
    n = numel(x)
    return [ x[i] + y[i] | (i=1:n) ]
end

function `+`(x:Array(T,2), y:Array(T,2))
    m = x.dims[0]
    n = x.dims[1]
    return [ x[i] + y[i] | (i=1:m), (j=1:n) ]
end


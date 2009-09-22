type Array(T,n) < Tensor(T,n)
    dims: buffer(size)
    data: buffer(T)
end

typealias Vector Tensor(T,1)
typealias Matrix Tensor(T,2)

function make_array(m:int32)
    dims = new(buffer(int32), 1)
    dims[0] = m
    data = new(buffer(double), m)
    array = new(Array(double,1), dims, data)
    return array
end

function ref(a:Array, i:int32)
    return a.data[i-1] # 1-based indexing
end

function set(a:Array, i:int32, x)
    bufferset(a.data, unbox(i-1), x) # 1-based indexing
    return x
end

function ones(m:int32)
    a = make_array(m)
    for i=1:m
        a[i] = 1
    end
    return a
end

function `+`(x:Array, y:Array)
    numel = x.data.length
    b = make_array(numel)
    for i=1:numel
        b[i] = x[i] + y[i]
    end
    return b
end

type Array(t)
    dims: buffer(size)
    data: buffer(t)
end

function make_array(m:int32)
    dims = new(buffer(int32), 1)
    data = new(buffer(double), m)
    array = new(Array(double), dims, data)
    return array
end

function ref(a:Array(t), i:int32)
    return a.data[i-1] # 1-based indexing
end

function set(a:Array(t), i:int32, x)
    bufferset(a.data, unbox(i-1), x) # 1-based indexing
    return x
end

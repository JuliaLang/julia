# ranges
@assert size(10:1:0) == (0,)
@assert length(1:.2:2) == 6
@assert length(1.:.2:2.) == 6
@assert length(2:-.2:1) == 6
@assert length(2.:-.2:1.) == 6
@assert length(2:.2:1) == 0
@assert length(2.:.2:1.) == 0

@assert length(1:0) == 0
@assert length(0.0:-0.5) == 0
@assert length(1:2:0) == 0
L32 = linspace(int32(1), int32(4), 4)
L64 = linspace(int64(1), int64(4), 4)
@assert L32[1] == 1 && L64[1] == 1
@assert L32[2] == 2 && L64[2] == 2
@assert L32[3] == 3 && L64[3] == 3
@assert L32[4] == 4 && L64[4] == 4

r = [5:-1:1]
@assert r[1]==5
@assert r[2]==4
@assert r[3]==3
@assert r[4]==2
@assert r[5]==1

# comprehensions
X = [ i+2j | i=1:5, j=1:5 ]
@assert X[2,3] == 8
@assert X[4,5] == 14
@assert isequal(ones(2,3) * ones(2,3)', [3. 3.; 3. 3.])
@assert isequal([ [1,2] | i=1:2, : ], [1 2; 1 2])
# where element type is a Union. try to confuse type inference.
foo32_64(x) = (x<2) ? int32(x) : int64(x)
boo32_64() = [ foo32_64(i) | i=1:2 ]
let a36 = boo32_64()
    @assert a36[1]==1 && a36[2]==2
end
@assert isequal([1,2,3], [b | (a,b) in enumerate(2:4)])
@assert isequal([2,3,4], [a | (a,b) in enumerate(2:4)])

@assert (10.^[-1])[1] == 0.1
@assert (10.^[-1.])[1] == 0.1

# dict
h = Dict()
for i=1:10000
    h[i] = i+1
end
for i=1:10000
    @assert (h[i] == i+1)
end
for i=1:2:10000
    del(h, i)
end
for i=1:2:10000
    h[i] = i+1
end
for i=1:10000
    @assert (h[i] == i+1)
end
for i=1:10000
    del(h, i)
end
@assert isempty(h)
h[77] = 100
@assert h[77]==100
for i=1:10000
    h[i] = i+1
end
for i=1:2:10000
    del(h, i)
end
for i=10001:20000
    h[i] = i+1
end
for i=2:2:10000
    @assert h[i]==i+1
end
for i=10000:20000
    @assert h[i]==i+1
end
h = {"a" => 3}
@assert h["a"] == 3

let
    z = Dict()
    get_KeyError = false
    try
        z["a"]
    catch _e123_
        get_KeyError = isa(_e123_,KeyError)
    end
    @assert get_KeyError
end

# ranges
@assert size(10:1:0) == (0,)
@assert length(1:.2:2) == 6
@assert length(Range(1.,.2,2.)) == 6
@assert length(2:-.2:1) == 6
@assert length(Range(2.,-.2,1.)) == 6
@assert length(2:.2:1) == 0
@assert length(Range(2.,.2,1.)) == 0

# comprehensions
X = [ i+2j | i=1:5, j=1:5 ]
@assert X[2,3] == 8
@assert X[4,5] == 14
@assert ones(2,3) * ones(2,3)' == [3. 3.; 3. 3.]
@assert [ [1,2] | i=1:2, : ] == [1 2; 1 2]
# where element type is a Union. try to confuse type inference.
foo32_64(x) = (x<2) ? int32(x) : int64(x)
boo32_64() = [ foo32_64(i) | i=1:2 ]
let a36 = boo32_64()
    @assert a36[1]==1 && a36[2]==2
end

# hash table
h = HashTable()
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

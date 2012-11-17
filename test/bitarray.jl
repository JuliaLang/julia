macro check_bit_operation(func, RetT, args)
    quote
        r1 = eval(($func)(($args)...))
        r2 = eval(($func)(map(x->(isa(x, BitArray) ? bitunpack(x) : x), $args)...))
        @assert isa(r1, $RetT)
        @assert isequal(r1, convert($RetT, r2))
    end
end

macro timesofar(str)
    return # no-op, comment to see timings
    global t0
    local t1 = gensym()
    quote
        $t1 = time()
        println($str, ": ", $t1-t0, " seconds")
        t0 = $t1
    end
end

cd("../extras") do
require("linalg_bitarray")

TT = Uint8
S = promote_type(TT, Int)

# vectors size
v1 = 260
# matrices size
n1 = 17
n2 = 20
# arrays size
s1 = 5
s2 = 8
s3 = 3
s4 = 4

t0 = time()

## Conversions ##

b1 = bitrand(TT, n1, n2)
@assert isequal(bitpack(bitunpack(b1)), b1)
i1 = randi(2, n1, n2) - 1
@assert isequal(bitunpack(bitpack(i1)), i1)

@timesofar "conversions"

## utility functions ##

@check_bit_operation length Int (b1,)
@check_bit_operation ndims Int (b1,)
@check_bit_operation numel Int (b1,)
@check_bit_operation size (Int...) (b1,)

@assert isequal(bitunpack(bitones(TT, n1, n2)), ones(TT, n1, n2))
@assert isequal(bitunpack(bitzeros(TT, n1, n2)), zeros(TT, n1, n2))

@assert isequal(fill!(b1, one(TT)), bitones(TT, size(b1)))
@assert isequal(fill!(b1, zero(TT)), bitzeros(TT, size(b1)))

@timesofar "utils"

## Indexing ##

b1 = bitrand(TT, n1, n2)
m1 = randi(n1)
m2 = randi(n2)
b2 = bitrand(TT, m1, m2)
@check_bit_operation copy_to BitArray{TT} (b1, b2)
@check_bit_operation ref BitArray{TT} (b1, 1:m1, m2:n2)
b2 = bitrand(TT, m1, m2)
@check_bit_operation assign BitArray{TT} (b1, b2, 1:m1, n2-m2+1:n2)

for p1 = [randi(v1) 1 63 64 65 191 192 193]
    for p2 = [randi(v1) 1 63 64 65 191 192 193]
        for n = 0 : min(v1 - p1 + 1, v1 - p2 + 1)
            b1 = bitrand(TT, v1)
            b2 = bitrand(TT, v1)
            @check_bit_operation copy_to BitArray{TT} (b1, p1, b2, p2, n)
        end
    end
end

# logical indexing
b1 = bitrand(TT, n1, n2)
t1 = bitrand(Bool, n1, n2)
@assert isequal(bitunpack(b1[t1]), bitunpack(b1)[t1])
@assert isequal(bitunpack(b1[t1]), bitunpack(b1)[bitunpack(t1)])
t1 = bitrand(Bool, n1)
t2 = bitrand(Bool, n2)
@assert isequal(bitunpack(b1[t1, t2]), bitunpack(b1)[t1, t2])
@assert isequal(bitunpack(b1[t1, t2]), bitunpack(b1)[bitunpack(t1), bitunpack(t2)])

@timesofar "indexing"

## Dequeue functionality ##

b1 = BitArray()
i1 = Int[]
for m = 1 : v1
    x = randi(2) - 1
    push(b1, x)
    push(i1, x)
    @assert isequal(bitunpack(b1), i1)
end

for m1 = 0 : v1
    for m2 = [0, 1, 63, 64, 65, 127, 128, 129]
        b1 = bitrand(TT, m1)
        b2 = bitrand(TT, m2)
        i1 = bitunpack(b1)
        i2 = bitunpack(b2)
        @assert isequal(bitunpack(append!(b1, b2)), append!(i1, i2))
    end
end

b1 = bitrand(TT, v1)
i1 = bitunpack(b1)
for m = 1 : v1
    jb = pop(b1)
    ji = pop(i1)
    @assert jb == ji
    @assert isequal(bitunpack(b1), i1)
end
@assert length(b1) == 0


b1 = BitArray()
i1 = Int[]
for m = 1 : v1
    x = randi(2) - 1
    enqueue(b1, x)
    enqueue(i1, x)
    @assert isequal(bitunpack(b1), i1)
end


b1 = bitrand(TT, v1)
i1 = bitunpack(b1)
for m = 1 : v1
    jb = shift(b1)
    ji = shift(i1)
    @assert jb == ji
    @assert isequal(bitunpack(b1), i1)
end
@assert length(b1) == 0

b1 = BitArray()
i1 = bitunpack(b1)
for m = 1 : v1
    j = randi(m)
    x = randi(2) - 1
    insert(b1, j, x)
    insert(i1, j, x)
    @assert isequal(bitunpack(b1), i1)
end

b1 = bitrand(TT, v1)
i1 = bitunpack(b1)
for j in [63, 64, 65, 127, 128, 129, 191, 192, 193]
    x = randi(2) - 1
    insert(b1, j, x)
    insert(i1, j, x)
    @assert isequal(bitunpack(b1), i1)
end

b1 = bitrand(TT, v1)
i1 = bitunpack(b1)
for m = v1 : -1 : 1
    j = randi(m)
    del(b1, j)
    del(i1, j)
    @assert isequal(bitunpack(b1), i1)
end
@assert length(b1) == 0

b1 = bitrand(TT, v1)
i1 = bitunpack(b1)
for j in [63, 64, 65, 127, 128, 129, 191, 192, 193]
    del(b1, j)
    del(i1, j)
    @assert isequal(bitunpack(b1), i1)
end

b1 = bitrand(TT, v1)
i1 = bitunpack(b1)
for m1 = 1 : v1
    for m2 = m1 : v1
        b2 = copy(b1)
        i2 = copy(i1)
        del(b2, m1:m2)
        del(i2, m1:m2)
        @assert isequal(bitunpack(b2), i2)
    end
end

b1 = bitrand(TT, v1)
i1 = bitunpack(b1)
del_all(b1)
del_all(i1)
@assert isequal(bitunpack(b1), i1)

@timesofar "dequeue"

## Unary operators ##

b1 = bitrand(TT, n1, n2)
@check_bit_operation (~) Array{TT} (b1,)
@check_bit_operation (-) Array{TT} (b1,)
@check_bit_operation sign BitArray{TT} (b1,)
@check_bit_operation real BitArray{TT} (b1,)
@check_bit_operation imag BitArray{TT} (b1,)
@check_bit_operation conj BitArray{TT} (b1,)

b1 = bitrand(Bool, n1, n2)
@check_bit_operation (~) BitArray{Bool} (b1,)
@check_bit_operation (!) BitArray{Bool} (b1,)
@check_bit_operation (-) BitArray{Bool} (b1,)
@check_bit_operation sign BitArray{Int} (b1,)

@timesofar "unary arithmetic"

## Binary arithmetic operators ##

b1 = bitrand(TT, n1, n2)
b2 = bitrand(TT, n1, n2)
@check_bit_operation (&) BitArray{TT} (b1, b2)
@check_bit_operation (|) BitArray{TT} (b1, b2)
@check_bit_operation ($) BitArray{TT} (b1, b2)
@check_bit_operation (-) Array{TT} (b1, b2)
@check_bit_operation (.*) BitArray{TT} (b1, b2)
@check_bit_operation (./) Array{Float64} (b1, b2)
@check_bit_operation (.^) Array{Float64} (b1, b2)

b2 = bitones(TT, n1, n2)
@check_bit_operation div Array{TT} (b1, b2)
@check_bit_operation mod Array{TT} (b1, b2)

while true
    global b1
    b1 = bitrand(TT, n1, n1)
    if abs(det(float64(b1))) > 1e-6
        break
    end
end
b2 = bitrand(TT, n1, n1)

@check_bit_operation (*) Array{TT} (b1, b2)
@check_bit_operation (/) Array{Float64} (b1, b1)
@check_bit_operation (\) Array{Float64} (b1, b1)

b1 = bitrand(TT, n1, n2)
b2 = randi(10, n1, n2)
@check_bit_operation (&) Array{S} (b1, b2)
@check_bit_operation (|) Array{S} (b1, b2)
@check_bit_operation ($) Array{S} (b1, b2)
@check_bit_operation (-) Array{S} (b1, b2)
@check_bit_operation (.*) Array{S} (b1, b2)
@check_bit_operation (./) Array{Float64} (b1, b2)
@check_bit_operation (.^) Array{Float64} (b1, b2)
@check_bit_operation div Array{S} (b1, b2)
@check_bit_operation mod Array{S} (b1, b2)

while true
    global b1
    b1 = bitrand(TT, n1, n1)
    if abs(det(float64(b1))) > 1e-6
        break
    end
end
b2 = bitunpack(b1)
@check_bit_operation (*) Array{TT} (b1, b2)
@check_bit_operation (/) Array{Float64} (b1, b2)
@check_bit_operation (\) Array{Float64} (b1, b2)

b1 = bitrand(Bool, n1, n2)
b2 = bitrand(Bool, n1, n2)
@check_bit_operation (&) BitArray{Bool} (b1, b2)
@check_bit_operation (|) BitArray{Bool} (b1, b2)
@check_bit_operation ($) BitArray{Bool} (b1, b2)
@check_bit_operation (.*) BitArray{Bool} (b1, b2)
@check_bit_operation (*) BitArray{Bool} (b1, b1')

@timesofar "binary arithmetic"

## Binary comparison operators ##

b1 = bitrand(TT, n1, n2)
b2 = bitrand(TT, n1, n2)
@check_bit_operation (.==) BitArray{Bool} (b1, b2)
@check_bit_operation (.!=) BitArray{Bool} (b1, b2)
@check_bit_operation (.<) BitArray{Bool} (b1, b2)
@check_bit_operation (.<=) BitArray{Bool} (b1, b2)

@timesofar "binary comparison"

## Data movement ##

b1 = bitrand(TT, s1, s2, s3, s4)
for d = 1 : 4
    j = randi(size(b1, d))
    #for j = 1 : size(b1, d)
        @check_bit_operation slicedim BitArray{TT} (b1, d, j)
    #end
    @check_bit_operation flipdim BitArray{TT} (b1, d)
end

b1 = bitrand(TT, n1, n2)
for k = 1 : 4
    @check_bit_operation rotl90 BitArray{TT} (b1, k)
end

for m = 0 : v1
    b1 = bitrand(TT, m)
    @check_bit_operation reverse BitArray{TT} (b1,)
end

b1 = bitrand(TT, v1)
for m = [randi(v1)-1 0 1 63 64 65 191 192 193 v1-1]
    @assert isequal(b1 << m, [ b1[m+1:end]; bitzeros(TT, m) ])
    @assert isequal(b1 >>> m, [ bitzeros(TT, m); b1[1:end-m] ])
    @assert isequal(rotl(b1, m), [ b1[m+1:end]; b1[1:m] ])
    @assert isequal(rotr(b1, m), [ b1[end-m+1:end]; b1[1:end-m] ])
end

@timesofar "datamove"

## nnz & find ##

b1 = bitrand(TT, v1)
@check_bit_operation nnz Int (b1,)
@check_bit_operation find Array{Int} (b1,)

b1 = bitrand(TT, n1, n2)
@check_bit_operation findn_nzs (Array{Int}, Array{Int}, BitArray{TT}) (b1,)

@timesofar "nnz&find"

## Reductions ##

b1 = bitrand(TT, s1, s2, s3, s4)
m1 = randi(s1)
m2 = randi(s3)
@check_bit_operation max BitArray{TT} (b1, (), (m1, m2))
@check_bit_operation min BitArray{TT} (b1, (), (m1, m2))
@check_bit_operation sum Array{Int} (b1, (m1, m2))
@check_bit_operation prod BitArray{TT} (b1, (m1, m2))

@timesofar "reductions"

## map over bitarrays ##

# TODO (not implemented)

## Filter ##

# TODO

## Transpose ##

b1 = bitrand(TT, v1)
@check_bit_operation transpose BitArray{TT} (b1,)

for m1 = 0 : n1
    for m2 = 0 : n2
        b1 = bitrand(TT, m1, m2)
        @check_bit_operation transpose BitArray{TT} (b1,)
    end
end

@timesofar "transpose"

## Permute ##

b1 = bitrand(TT, s1, s2, s3, s4)
p = randperm(4)
@check_bit_operation permute BitArray{TT} (b1, p)

@timesofar "permute"

## Concatenation ##

b1 = bitrand(TT, v1)
b2 = bitrand(TT, v1)
@check_bit_operation hcat BitArray{TT} (b1, b2)
for m = 1 : v1 - 1
    @check_bit_operation vcat BitArray{TT} (b1[1:m], b1[m+1:end])
end

b1 = bitrand(TT, n1, n2)
b2 = bitrand(TT, n1)
b3 = bitrand(TT, n1, n2)
b4 = bitrand(TT, 1, n2)
@check_bit_operation hcat BitArray{TT} (b1, b2, b3)
@check_bit_operation vcat BitArray{TT} (b1, b4, b3)

b1 = bitrand(TT, s1, s2, s3, s4)
b2 = bitrand(TT, s1, s3, s3, s4)
b3 = bitrand(TT, s1, s2, s3, s1)
@check_bit_operation cat BitArray{TT} (2, b1, b2)
@check_bit_operation cat BitArray{TT} (4, b1, b3)
@check_bit_operation cat BitArray{TT} (6, b1, b1)

b1 = bitrand(TT, 1, v1, 1)
@check_bit_operation cat Array{S} (2, 0, b1, 1, 1, b1)
@check_bit_operation cat Array{S} (2, 3, b1, 4, 5, b1)

b1 = bitrand(Bool, 1, v1, 1)
@check_bit_operation cat BitArray{Bool} (2, false, b1, true, true, b1)
@check_bit_operation cat Array{Int} (2, 3, b1, 4, 5, b1)

b1 = bitrand(TT, n1, n2)
for m1 = 1 : n1 - 1
    for m2 = 1 : n2 - 1
        @assert isequal([b1[1:m1,1:m2] b1[1:m1,m2+1:end]; b1[m1+1:end,1:m2] b1[m1+1:end,m2+1:end]], b1)
    end
end

@timesofar "cat"

# Linear algebra

b1 = bitrand(TT, v1)
b2 = bitrand(TT, v1)
@check_bit_operation dot typeof(one(TT) * one(TT)) (b1, b2)
b2 = bitrand(Bool, v1)
@check_bit_operation dot typeof(one(TT) * true) (b1, b2)

b1 = bitrand(TT, n1, n2)
for k = -max(n1,n2) : max(n1,n2)
    @check_bit_operation tril BitArray{TT} (b1, k)
    @check_bit_operation triu BitArray{TT} (b1, k)
end

#b1 = bitrand(TT, v1)
#@check_bit_operation diff Array{S} (b1,)
#b1 = bitrand(TT, n1, n2)
#@check_bit_operation diff Array{S} (b1,)

@timesofar "linalg"

end # do

load("../extras/bitarray.jl")

macro check_bit_operation1(func, RetT, b1, args...)
    quote
        r1 = eval(($func)($b1, $args...))
        r2 = eval(($func)(int($b1), $args...))
        @assert isequal(convert($RetT, r1), r2)
    end
end

macro check_bit_operation2(func, RetT, b1, b2, args...)
    quote
        r1 = eval(($func)($b1, $b2, $args...))
        r2 = eval(($func)(int($b1), int($b2), $args...))
        @assert isequal(convert($RetT, r1), r2)
    end
end

macro check_bit_operation3(func, RetT, b1, b2, b3, args...)
    quote
        r1 = eval(($func)($b1, $b2, $b3, $args...))
        r2 = eval(($func)(int($b1), int($b2), int($b3), $args...))
        @assert isequal(convert($RetT, r1), r2)
    end
end

macro check_bit_operation_allint(func, RetT, args)
    quote
        r1 = eval(($func)(($args)...))
        r2 = eval(($func)(map(x->int(x), $args)...))
        @assert isequal(convert($RetT, r1), r2)
    end
end

macro timesofar(str)
    return # no-op
    global t0
    local t1 = gensym()
    quote
        $t1 = time()
        println($str, ": ", $t1-t0, " seconds")
        t0 = $t1
    end
end


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

b1 = bitrand(n1, n2)
@assert isequal(convert(BitArray, convert(Array{Int}, b1)), b1)
i1 = randbit(n1, n2)
@assert isequal(convert(Array{Int}, convert(BitArray, i1)), i1)

@timesofar "conversions"

## utility functions ##

@check_bit_operation1 length Int b1
@check_bit_operation1 ndims Int b1
@check_bit_operation1 numel Int b1
@check_bit_operation1 size (Int...) b1

@assert int(bitones(n1, n2)) == ones(Int, n1, n2)
@assert int(bitzeros(n1, n2)) == zeros(Int, n1, n2)

@assert fill(b1, 1) == bitones(size(b1))
@assert fill(b1, 0) == bitzeros(size(b1))

@timesofar "utils"

## Indexing ##

m1 = randi(n1)
m2 = randi(n2)
b2 = bitrand(m1, m2)
@check_bit_operation2 copy_to Array{Int} b1 b2
@check_bit_operation1 ref Array{Int} b1 1:m1 m2:n2
b2 = bitrand(m1, m2)
@check_bit_operation2 assign Array{Int} b1 b2 1:m1 n2-m2+1:n2

# logical indexing
b2 = bitrand(n1, n2)
t2 = convert(Array{Bool}, b2)
@assert isequal(int(b1[t2]), int(b1)[t2])

@timesofar "indexing"

## Dequeue functionality ##

b1 = BitArray()
i1 = Int[]
for m = 1 : v1
    x = randi(2) - 1
    push(b1, x)
    push(i1, x)
    @assert isequal(int(b1), i1)
    @assert isequal(b1, convert(BitArray, i1))
end

for m1 = 0 : v1
    for m2 = [0, 1, 63, 64, 65, 127, 128, 129]
        b1 = bitrand(m1)
        b2 = bitrand(m2)
        i1 = int(b1)
        i2 = int(b2)
        @assert isequal(int(append!(b1, b2)), append!(i1, i2))
    end
end

b1 = bitrand(v1)
i1 = int(b1)
for m = 1 : v1
    jb = pop(b1)
    ji = pop(i1)
    @assert jb == ji
    @assert isequal(int(b1), i1)
end
@assert length(b1) == 0


b1 = BitArray()
i1 = Int[]
for m = 1 : v1
    x = randi(2) - 1
    enqueue(b1, x)
    enqueue(i1, x)
    @assert isequal(int(b1), i1)
end


b1 = bitrand(v1)
i1 = int(b1)
for m = 1 : v1
    jb = shift(b1)
    ji = shift(i1)
    @assert jb == ji
    @assert isequal(int(b1), i1)
end
@assert length(b1) == 0

b1 = BitArray()
i1 = int(b1)
for m = 1 : v1
    j = randi(m)
    x = randi(2) - 1
    insert(b1, j, x)
    insert(i1, j, x)
    @assert isequal(int(b1), i1)
end

b1 = bitrand(v1)
i1 = int(b1)
for j in [63, 64, 65, 127, 128, 129, 191, 192, 193]
    x = randi(2) - 1
    insert(b1, j, x)
    insert(i1, j, x)
    @assert isequal(int(b1), i1)
end

b1 = bitrand(v1)
i1 = int(b1)
for m = v1 : -1 : 1
    j = randi(m)
    del(b1, j)
    del(i1, j)
    @assert isequal(int(b1), i1)
end
@assert length(b1) == 0

b1 = bitrand(v1)
i1 = int(b1)
for j in [63, 64, 65, 127, 128, 129, 191, 192, 193]
    del(b1, j)
    del(i1, j)
    @assert isequal(int(b1), i1)
end

b1 = bitrand(v1)
i1 = int(b1)
for m1 = 1 : v1
    for m2 = m1 : v1
        b2 = copy(b1)
        i2 = copy(i1)
        del(b2, m1:m2)
        del(i2, m1:m2)
        @assert isequal(int(b2), i2)
    end
end

b1 = bitrand(v1)
i1 = int(b1)
del_all(b1)
del_all(i1)
@assert isequal(int(b1), i1)

@timesofar "dequeue"

## Unary operators ##

b1 = bitrand(n1, n2)
@assert isequal(int(~b1), 1 - int(b1))
@check_bit_operation1 (-) Array{Int} b1
@check_bit_operation1 sign Array{Int} b1
@check_bit_operation1 real Array{Int} b1
@check_bit_operation1 imag Array{Int} b1
@check_bit_operation1 conj Array{Int} b1

@timesofar "unary arithmetic"

## Binary arithmetic operators ##

b2 = bitrand(n1, n2)
@check_bit_operation2 (&) Array{Int} b1 b2
@check_bit_operation2 (|) Array{Int} b1 b2
@check_bit_operation2 ($) Array{Int} b1 b2
@check_bit_operation2 (-) Array{Int} b1 b2
@check_bit_operation2 (.*) Array{Int} b1 b2
@check_bit_operation2 (./) Array{Float64} b1 b2
@check_bit_operation2 (.^) Array{Float64} b1 b2

b2 = bitones(n1, n2)
@check_bit_operation2 div Array{Int} b1 b2
@check_bit_operation2 mod Array{Int} b1 b2

while true
    global b1
    b1 = bitrand(n1, n1)
    if abs(det(float64(b1))) > 1e-6
        break
    end
end
b2 = bitrand(n1, n1)

@check_bit_operation2 (*) Array{Int} b1 b2
@check_bit_operation2 (/) Array{Float64} b1 b1
@check_bit_operation2 (\) Array{Float64} b1 b1

b1 = bitrand(n1, n2)
b2 = randi(10, n1, n2)
@check_bit_operation2 (&) Array{Int} b1 b2
@check_bit_operation2 (|) Array{Int} b1 b2
@check_bit_operation2 ($) Array{Int} b1 b2
@check_bit_operation2 (-) Array{Int} b1 b2
@check_bit_operation2 (.*) Array{Int} b1 b2
@check_bit_operation2 (./) Array{Float64} b1 b2
@check_bit_operation2 (.^) Array{Float64} b1 b2
@check_bit_operation2 div Array{Int} b1 b2
@check_bit_operation2 mod Array{Int} b1 b2

while true
    global b1
    b1 = bitrand(n1, n1)
    if abs(det(float64(b1))) > 1e-6
        break
    end
end
b2 = int(b1)
@check_bit_operation2 (*) Array{Int} b1 b2
@check_bit_operation2 (/) Array{Float64} b1 b2
@check_bit_operation2 (\) Array{Float64} b1 b2

@timesofar "binary arithmetic"

## Binary comparison operators ##

b1 = bitrand(n1, n2)
b2 = bitrand(n1, n2)
@check_bit_operation2 (==) Array{Int} b1 b2
@check_bit_operation2 (!=) Array{Int} b1 b2
@check_bit_operation2 (<) Array{Int} b1 b2
@check_bit_operation2 (<=) Array{Int} b1 b2

@timesofar "binary comparison"

## Data movement ##

b1 = bitrand(s1, s2, s3, s4)
for d = 1 : 4
    j = randi(size(b1, d))
    #for j = 1 : size(b1, d)
        @check_bit_operation1 slicedim Array{Int} b1 d j
    #end
    @check_bit_operation1 flipdim Array{Int} b1 d
end

b1 = bitrand(n1, n2)
for k = 1 : 4
    @check_bit_operation1 rotl90 Array{Int} b1 k
end

b1 = bitrand(v1)
@check_bit_operation1 reverse Array{Int} b1

@timesofar "datamove"

## nnz & find ##

b1 = bitrand(v1)
@check_bit_operation1 nnz Int b1
@check_bit_operation1 find Array{Int} b1

b1 = bitrand(n1, n2)
@check_bit_operation1 findn_nzs (Array{Int}, Array{Int}, Array{Int}) b1

@timesofar "nnz&find"

## Reductions ##

b1 = bitrand(s1, s2, s3, s4)
m1 = randi(s1)
m2 = randi(s3)
@check_bit_operation1 max Array{Int} b1 () (m1, m2)
@check_bit_operation1 min Array{Int} b1 () (m1, m2)
@check_bit_operation1 sum Array{Int} b1 (m1, m2)
@check_bit_operation1 prod Array{Int} b1 (m1, m2)

@timesofar "reductions"

## map over bitarrays ##

# TODO (not implemented)

## Filter ##

# TODO

## Transpose ##

b1 = bitrand(v1)
@check_bit_operation1 transpose Array{Int} b1

b1 = bitrand(n1, n2)
@check_bit_operation1 transpose Array{Int} b1

@timesofar "transpose"

## Permute ##

b1 = bitrand(s1, s2, s3, s4)
p = randperm(4)
@check_bit_operation1 permute Array{Int} b1 p

@timesofar "permute"

## Concatenation ##

b1 = bitrand(v1)
b2 = bitrand(v1)
@check_bit_operation2 hcat Array{Int} b1 b2
for m = 1 : v1 - 1
    @check_bit_operation2 vcat Array{Int} b1[1:m] b1[m+1:end]
end

b1 = bitrand(n1, n2)
b2 = bitrand(n1)
b3 = bitrand(n1, n2)
b4 = bitrand(1, n2)
@check_bit_operation3 hcat Array{Int} b1 b2 b3
@check_bit_operation3 vcat Array{Int} b1 b4 b3

b1 = bitrand(s1, s2, s3, s4)
b2 = bitrand(s1, s3, s3, s4)
b3 = bitrand(s1, s2, s3, s1)
@check_bit_operation_allint cat Array{Int} (2, b1, b2)
@check_bit_operation_allint cat Array{Int} (4, b1, b3)

b1 = bitrand(n1, 1, 1)
@check_bit_operation_allint cat Array{Int} (1, b1, 0, 1, b1)
@check_bit_operation_allint cat Array{Int} (1, b1, 3, 5, b1)

b1 = bitrand(n1, n2)
for m1 = 1 : n1 - 1
    for m2 = 1 : n2 - 1
        @assert isequal([b1[1:m1,1:m2] b1[1:m1,m2+1:end]; b1[m1+1:end,1:m2] b1[m1+1:end,m2+1:end]], b1)
    end
end

@timesofar "cat"

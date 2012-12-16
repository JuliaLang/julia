# for now, manually import necessary bit array functions:

macro check_bit_operation(func, RetT, args)
    quote
        r1 = ($func)($(args.args...))
        r2 = ($func)(map(x->(isa(x, BitArray) ? bitunpack(x) : x), $args)...)
        @test isa(r1, $RetT)
        @test isequal(r1, convert($RetT, r2))
    end
end

let t0 = time()
    global timesofar
    function timesofar(str)
        return # no-op, comment to see timings
        t1 = time()
        println(str, ": ", t1-t0, " seconds")
        t0 = t1
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

## Conversions ##

b1 = randbool(n1, n2)
@test isequal(bitpack(bitunpack(b1)), b1)
i1 = randi(2, n1, n2) - 1
@test isequal(bitunpack(bitpack(i1)), i1)

timesofar("conversions")

## utility functions ##

@check_bit_operation length Int (b1,)
@check_bit_operation ndims Int (b1,)
@check_bit_operation numel Int (b1,)
@check_bit_operation size (Int...) (b1,)

@test isequal(bitunpack(trues(n1, n2)), ones(Bool, n1, n2))
@test isequal(bitunpack(falses(n1, n2)), zeros(Bool, n1, n2))

@test isequal(fill!(b1, true), trues(size(b1)))
@test isequal(fill!(b1, false), falses(size(b1)))

timesofar("utils")

## Indexing ##

b1 = randbool(n1, n2)
m1 = randi(n1)
m2 = randi(n2)
b2 = randbool(m1, m2)
@check_bit_operation copy_to BitMatrix (b1, b2)
@check_bit_operation ref BitMatrix (b1, 1:m1, m2:n2)
@check_bit_operation ref BitVector (b1, 1:m1, m2)
b2 = randbool(m1, m2)
@check_bit_operation assign BitMatrix (b1, b2, 1:m1, n2-m2+1:n2)
b2 = randbool(m1)
@check_bit_operation assign BitMatrix (b1, b2, 1:m1, m2)

for p1 = [randi(v1) 1 63 64 65 191 192 193]
    for p2 = [randi(v1) 1 63 64 65 191 192 193]
        for n = 0 : min(v1 - p1 + 1, v1 - p2 + 1)
            b1 = randbool(v1)
            b2 = randbool(v1)
            @check_bit_operation copy_to BitVector (b1, p1, b2, p2, n)
        end
    end
end

# logical indexing
b1 = randbool(n1, n2)
t1 = randbool(n1, n2)
@test isequal(bitunpack(b1[t1]), bitunpack(b1)[t1])
@test isequal(bitunpack(b1[t1]), bitunpack(b1)[bitunpack(t1)])
t1 = randbool(n1)
t2 = randbool(n2)
@test isequal(bitunpack(b1[t1, t2]), bitunpack(b1)[t1, t2])
@test isequal(bitunpack(b1[t1, t2]), bitunpack(b1)[bitunpack(t1), bitunpack(t2)])

timesofar("indexing")

## Dequeue functionality ##

b1 = BitArray()
i1 = Bool[]
for m = 1 : v1
    x = randbool()
    push(b1, x)
    push(i1, x)
    @test isequal(bitunpack(b1), i1)
end

for m1 = 0 : v1
    for m2 = [0, 1, 63, 64, 65, 127, 128, 129]
        b1 = randbool(m1)
        b2 = randbool(m2)
        i1 = bitunpack(b1)
        i2 = bitunpack(b2)
        @test isequal(bitunpack(append!(b1, b2)), append!(i1, i2))
    end
end

b1 = randbool(v1)
i1 = bitunpack(b1)
for m = 1 : v1
    jb = pop(b1)
    ji = pop(i1)
    @test jb == ji
    @test isequal(bitunpack(b1), i1)
end
@test length(b1) == 0


b1 = BitArray()
i1 = Bool[]
for m = 1 : v1
    x = randbool()
    enqueue(b1, x)
    enqueue(i1, x)
    @test isequal(bitunpack(b1), i1)
end


b1 = randbool(v1)
i1 = bitunpack(b1)
for m = 1 : v1
    jb = shift(b1)
    ji = shift(i1)
    @test jb == ji
    @test isequal(bitunpack(b1), i1)
end
@test length(b1) == 0

b1 = BitArray()
i1 = bitunpack(b1)
for m = 1 : v1
    j = randi(m)
    x = randbool()
    insert(b1, j, x)
    insert(i1, j, x)
    @test isequal(bitunpack(b1), i1)
end

b1 = randbool(v1)
i1 = bitunpack(b1)
for j in [63, 64, 65, 127, 128, 129, 191, 192, 193]
    x = randi(2) - 1
    insert(b1, j, x)
    insert(i1, j, x)
    @test isequal(bitunpack(b1), i1)
end

b1 = randbool(v1)
i1 = bitunpack(b1)
for m = v1 : -1 : 1
    j = randi(m)
    del(b1, j)
    del(i1, j)
    @test isequal(bitunpack(b1), i1)
end
@test length(b1) == 0

b1 = randbool(v1)
i1 = bitunpack(b1)
for j in [63, 64, 65, 127, 128, 129, 191, 192, 193]
    del(b1, j)
    del(i1, j)
    @test isequal(bitunpack(b1), i1)
end

b1 = randbool(v1)
i1 = bitunpack(b1)
for m1 = 1 : v1
    for m2 = m1 : v1
        b2 = copy(b1)
        i2 = copy(i1)
        del(b2, m1:m2)
        del(i2, m1:m2)
        @test isequal(bitunpack(b2), i2)
    end
end

b1 = randbool(v1)
i1 = bitunpack(b1)
del_all(b1)
del_all(i1)
@test isequal(bitunpack(b1), i1)

timesofar("dequeue")

## Unary operators ##

b1 = randbool(n1, n2)
@check_bit_operation (~) BitMatrix (b1,)
@check_bit_operation (!) BitMatrix (b1,)
#@check_bit_operation (-) Matrix{Int} (b1,)
@check_bit_operation sign BitMatrix (b1,)
@check_bit_operation real BitMatrix (b1,)
@check_bit_operation imag BitMatrix (b1,)
@check_bit_operation conj BitMatrix (b1,)

b0 = falses(0)
@check_bit_operation (~) BitVector (b0,)
@check_bit_operation (!) BitVector (b0,)
#@check_bit_operation (-) Vector{Int} (b0,)
@check_bit_operation sign BitVector (b0,)

timesofar("unary arithmetic")

## Binary arithmetic operators ##

b1 = randbool(n1, n2)
b2 = randbool(n1, n2)
@check_bit_operation (&) BitMatrix (b1, b2)
@check_bit_operation (|) BitMatrix (b1, b2)
@check_bit_operation ($) BitMatrix (b1, b2)
@check_bit_operation (-) Matrix{Int} (b1, b2)
@check_bit_operation (.*) BitMatrix (b1, b2)
@check_bit_operation (./) Matrix{Float64} (b1, b2)
@check_bit_operation (.^) Matrix{Float64} (b1, b2)

b2 = trues(n1, n2)
@check_bit_operation div Matrix{Int} (b1, b2)
@check_bit_operation mod Matrix{Bool} (b1, b2)

while true
    global b1
    b1 = randbool(n1, n1)
    if abs(det(float64(b1))) > 1e-6
        break
    end
end
b2 = randbool(n1, n1)

@check_bit_operation (*) BitMatrix (b1, b2)
@check_bit_operation (/) Matrix{Float64} (b1, b1)
@check_bit_operation (\) Matrix{Float64} (b1, b1)

b1 = randbool(n1, n2)
b2 = randi(10, n1, n2)
@check_bit_operation (&) Matrix{Int} (b1, b2)
@check_bit_operation (|) Matrix{Int} (b1, b2)
@check_bit_operation ($) Matrix{Int} (b1, b2)
@check_bit_operation (-) Matrix{Int} (b1, b2)
@check_bit_operation (.*) Matrix{Int} (b1, b2)
@check_bit_operation (./) Matrix{Float64} (b1, b2)
@check_bit_operation (.^) Matrix{Float64} (b1, b2)
@check_bit_operation div Matrix{Int} (b1, b2)
@check_bit_operation mod Matrix{Int} (b1, b2)

b1 = randbool(n1, n2)
b2 = randbool(n1, n2)
@check_bit_operation (&) BitMatrix (b1, b2)
@check_bit_operation (|) BitMatrix (b1, b2)
@check_bit_operation ($) BitMatrix (b1, b2)
@check_bit_operation (.*) BitMatrix (b1, b2)
@check_bit_operation (*) BitMatrix (b1, b1')

b0 = falses(0)
@check_bit_operation (&) BitVector (b0, b0)
@check_bit_operation (|) BitVector (b0, b0)
@check_bit_operation ($) BitVector (b0, b0)
@check_bit_operation (.*) BitVector (b0, b0)
@check_bit_operation (*) BitMatrix (b0, b0')

timesofar("binary arithmetic")

## Binary comparison operators ##

b1 = randbool(n1, n2)
b2 = randbool(n1, n2)
@check_bit_operation (.==) BitMatrix (b1, b2)
@check_bit_operation (.!=) BitMatrix (b1, b2)
@check_bit_operation (.<) BitMatrix (b1, b2)
@check_bit_operation (.<=) BitMatrix (b1, b2)

timesofar("binary comparison")

## Data movement ##

b1 = randbool(s1, s2, s3, s4)
for d = 1 : 4
    j = randi(size(b1, d))
    #for j = 1 : size(b1, d)
        @check_bit_operation slicedim BitArray{4} (b1, d, j)
    #end
    @check_bit_operation flipdim BitArray{4} (b1, d)
end

b1 = randbool(n1, n2)
for k = 1 : 4
    @check_bit_operation rotl90 BitMatrix (b1, k)
end

for m = 0 : v1
    b1 = randbool(m)
    @check_bit_operation reverse BitVector (b1,)
end

b1 = randbool(v1)
for m = [randi(v1)-1 0 1 63 64 65 191 192 193 v1-1]
    @test isequal(b1 << m, [ b1[m+1:end]; falses(m) ])
    @test isequal(b1 >>> m, [ falses(m); b1[1:end-m] ])
    @test isequal(rotl(b1, m), [ b1[m+1:end]; b1[1:m] ])
    @test isequal(rotr(b1, m), [ b1[end-m+1:end]; b1[1:end-m] ])
end

timesofar("datamove")

## nnz & find ##

b1 = randbool(v1)
@check_bit_operation nnz Int (b1,)

@check_bit_operation findfirst Int (b1,)
@check_bit_operation findfirst Int (trues(v1),)
@check_bit_operation findfirst Int (falses(v1),)

@check_bit_operation findfirst Int (b1, true)
@check_bit_operation findfirst Int (b1, false)
@check_bit_operation findfirst Int (b1, 3)

@check_bit_operation findfirst Int (x->x, b1)
@check_bit_operation findfirst Int (x->!x, b1)
@check_bit_operation findfirst Int (x->true, b1)
@check_bit_operation findfirst Int (x->false, b1)

@check_bit_operation find Vector{Int} (b1,)

b1 = randbool(n1, n2)
@check_bit_operation findn_nzs (Vector{Int}, Vector{Int}, BitArray) (b1,)

timesofar("nnz&find")

## Reductions ##

b1 = randbool(s1, s2, s3, s4)
m1 = 1
m2 = 3
@check_bit_operation max BitArray{4} (b1, (), (m1, m2))
@check_bit_operation min BitArray{4} (b1, (), (m1, m2))
#@check_bit_operationV any BitArray{4} (b1, (m1, m2)) # ??? fails
#@check_bit_operation all BitArray{4} (b1, (m1, m2)) # ??? fails
@check_bit_operation sum Array{Int,4} (b1, (m1, m2))

@check_bit_operation max Bool (b1,)
@check_bit_operation min Bool (b1,)
@check_bit_operation any Bool (b1,)
@check_bit_operation all Bool (b1,)
@check_bit_operation sum Int (b1,)

b0 = falses(0)
@check_bit_operation max Bool (b0,)
@check_bit_operation min Bool (b0,)
@check_bit_operation any Bool (b0,)
@check_bit_operation all Bool (b0,)
@check_bit_operation sum Int (b0,)

timesofar("reductions")

## map over bitarrays ##

# TODO (not implemented)

## Filter ##

# TODO

## Transpose ##

b1 = randbool(v1)
@check_bit_operation transpose BitMatrix (b1,)

for m1 = 0 : n1
    for m2 = 0 : n2
        b1 = randbool(m1, m2)
        @check_bit_operation transpose BitMatrix (b1,)
    end
end

timesofar("transpose")

## Permute ##

b1 = randbool(s1, s2, s3, s4)
p = randperm(4)
@check_bit_operation permute BitArray{4} (b1, p)

timesofar("permute")

## Concatenation ##

b1 = randbool(v1)
b2 = randbool(v1)
@check_bit_operation hcat BitMatrix (b1, b2)
for m = 1 : v1 - 1
    @check_bit_operation vcat BitVector (b1[1:m], b1[m+1:end])
end

b1 = randbool(n1, n2)
b2 = randbool(n1)
b3 = randbool(n1, n2)
b4 = randbool(1, n2)
@check_bit_operation hcat BitMatrix (b1, b2, b3)
@check_bit_operation vcat BitMatrix (b1, b4, b3)

b1 = randbool(s1, s2, s3, s4)
b2 = randbool(s1, s3, s3, s4)
b3 = randbool(s1, s2, s3, s1)
@check_bit_operation cat BitArray{4} (2, b1, b2)
@check_bit_operation cat BitArray{4} (4, b1, b3)
@check_bit_operation cat BitArray{6} (6, b1, b1)

b1 = randbool(1, v1, 1)
@check_bit_operation cat Array{Int,3} (2, 0, b1, 1, 1, b1)
@check_bit_operation cat Array{Int,3} (2, 3, b1, 4, 5, b1)
@check_bit_operation cat BitArray{3} (2, false, b1, true, true, b1)

b1 = randbool(n1, n2)
for m1 = 1 : n1 - 1
    for m2 = 1 : n2 - 1
        @test isequal([b1[1:m1,1:m2] b1[1:m1,m2+1:end]; b1[m1+1:end,1:m2] b1[m1+1:end,m2+1:end]], b1)
    end
end

timesofar("cat")

# Linear algebra

b1 = randbool(v1)
b2 = randbool(v1)
@check_bit_operation dot Int (b1, b2)

b1 = randbool(n1, n2)
for k = -max(n1,n2) : max(n1,n2)
    @check_bit_operation tril BitMatrix (b1, k)
    @check_bit_operation triu BitMatrix (b1, k)
end

#b1 = randbool(v1)
#@check_bit_operation diff Vector{Int} (b1,)
#b1 = randbool(n1, n2)
#@check_bit_operation diff Vector{Int} (b1,)

timesofar("linalg")

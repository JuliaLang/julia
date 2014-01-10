function tc(r1, r2)
    if isa(r1, Tuple) && isa(r2, Tuple) && length(r1) == length(r2)
        return all(map(x->tc(x...), [zip(r1,r2)...]))
    elseif isa(r1,BitArray)
        return isa(r2, Union(BitArray,Array{Bool}))
    else
        return typeof(r1) == typeof(r2)
    end
end

function check_bitop(func, RetT, args)
    r1 = func(args...)
    r2 = func(map(x->(isa(x, BitArray) ? bitunpack(x) : x), args)...)
    @test isa(r1, RetT)
    @test tc(r1, r2)
    @test isequal(r1, convert(RetT, r2))
end

macro check_bit_operation(func, RetT, args)
    :(check_bitop($(esc(func)), $(esc(RetT)), $(esc(args))))
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
i1 = rand(false:true, n1, n2)
@test isequal(bitunpack(bitpack(i1)), i1)

timesofar("conversions")

## utility functions ##

@check_bit_operation length Int (b1,)
@check_bit_operation ndims Int (b1,)
@check_bit_operation size (Int...) (b1,)

@test isequal(bitunpack(trues(n1, n2)), ones(Bool, n1, n2))
@test isequal(bitunpack(falses(n1, n2)), zeros(Bool, n1, n2))

@test isequal(fill!(b1, true), trues(size(b1)))
@test isequal(fill!(b1, false), falses(size(b1)))

timesofar("utils")

## Indexing ##

b1 = randbool(n1, n2)
m1 = rand(1:n1)
m2 = rand(1:n2)
b2 = randbool(m1, m2)
@check_bit_operation copy! BitMatrix (b1, b2)
@check_bit_operation getindex BitMatrix (b1, 1:m1, m2:n2)
@check_bit_operation getindex BitVector (b1, 1:m1, m2)
@check_bit_operation getindex BitMatrix (b1, 1:m1, [n2,m2,1])
b2 = randbool(m1, m2)
@check_bit_operation setindex! BitMatrix (b1, b2, 1:m1, n2-m2+1:n2)
k1 = randperm(m1)
k2 = randperm(m2)
@check_bit_operation setindex! BitMatrix (b1, b2, 1:m1, k2)
@check_bit_operation setindex! BitMatrix (b1, b2, k1, k2)
b2 = randbool(m1)
@check_bit_operation setindex! BitMatrix (b1, b2, 1:m1, m2)

for p1 = [rand(1:v1) 1 63 64 65 191 192 193]
    for p2 = [rand(1:v1) 1 63 64 65 191 192 193]
        for n = 0 : min(v1 - p1 + 1, v1 - p2 + 1)
            b1 = randbool(v1)
            b2 = randbool(v1)
            @check_bit_operation copy! BitVector (b1, p1, b2, p2, n)
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

b1 = BitArray(0)
i1 = Bool[]
for m = 1 : v1
    x = randbool()
    push!(b1, x)
    push!(i1, x)
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

for m1 = 0 : v1
    for m2 = [0, 1, 63, 64, 65, 127, 128, 129]
        b1 = randbool(m1)
        b2 = randbool(m2)
        i1 = bitunpack(b1)
        i2 = bitunpack(b2)
        @test isequal(bitunpack(prepend!(b1, b2)), prepend!(i1, i2))
    end
end

b1 = randbool(v1)
i1 = bitunpack(b1)
for m = 1 : v1
    jb = pop!(b1)
    ji = pop!(i1)
    @test jb == ji
    @test isequal(bitunpack(b1), i1)
end
@test length(b1) == 0


b1 = BitArray(0)
i1 = Bool[]
for m = 1 : v1
    x = randbool()
    unshift!(b1, x)
    unshift!(i1, x)
    @test isequal(bitunpack(b1), i1)
end


b1 = randbool(v1)
i1 = bitunpack(b1)
for m = 1 : v1
    jb = shift!(b1)
    ji = shift!(i1)
    @test jb == ji
    @test isequal(bitunpack(b1), i1)
end
@test length(b1) == 0

b1 = BitArray(0)
i1 = bitunpack(b1)
for m = 1 : v1
    j = rand(1:m)
    x = randbool()
    insert!(b1, j, x)
    insert!(i1, j, x)
    @test isequal(bitunpack(b1), i1)
end

b1 = randbool(v1)
i1 = bitunpack(b1)
for j in [63, 64, 65, 127, 128, 129, 191, 192, 193]
    x = rand(1:2) - 1
    insert!(b1, j, x)
    insert!(i1, j, x)
    @test isequal(bitunpack(b1), i1)
end

b1 = randbool(v1)
i1 = bitunpack(b1)
for m = v1 : -1 : 1
    j = rand(1:m)
    splice!(b1, j)
    splice!(i1, j)
    @test isequal(bitunpack(b1), i1)
end
@test length(b1) == 0

b1 = randbool(v1)
i1 = bitunpack(b1)
for j in [63, 64, 65, 127, 128, 129, 191, 192, 193]
    splice!(b1, j)
    splice!(i1, j)
    @test isequal(bitunpack(b1), i1)
end

b1 = randbool(v1)
i1 = bitunpack(b1)
for m1 = 1 : v1
    for m2 = m1 : v1
        b2 = copy(b1)
        i2 = copy(i1)
        splice!(b2, m1:m2)
        splice!(i2, m1:m2)
        @test isequal(bitunpack(b2), i2)
    end
end

b1 = randbool(v1)
i1 = bitunpack(b1)
for m1 = 1 : v1 + 1
    for m2 = m1 - 1 : v1
        for v2 = [0, 1, 63, 64, 65, 127, 128, 129, 191, 192, 193, rand(1:v1)]
            b2 = copy(b1)
            i2 = copy(i1)
            b3 = randbool(v2)
            i3 = bitunpack(b3)
            splice!(b2, m1:m2, b3)
            splice!(i2, m1:m2, i3)
            @test isequal(bitunpack(b2), i2)
        end
    end
end

b1 = randbool(v1)
i1 = bitunpack(b1)
empty!(b1)
empty!(i1)
@test isequal(bitunpack(b1), i1)

timesofar("dequeue")

## Unary operators ##

b1 = randbool(n1, n2)
@check_bit_operation (~) BitMatrix (b1,)
@check_bit_operation (!) BitMatrix (b1,)
@check_bit_operation (-) Matrix{Int} (b1,)
@check_bit_operation sign BitMatrix (b1,)
@check_bit_operation real BitMatrix (b1,)
@check_bit_operation imag BitMatrix (b1,)
@check_bit_operation conj BitMatrix (b1,)

b0 = falses(0)
@check_bit_operation (~) BitVector (b0,)
@check_bit_operation (!) BitVector (b0,)
@check_bit_operation (-) Vector{Int} (b0,)
@check_bit_operation sign BitVector (b0,)

timesofar("unary arithmetic")

## Binary arithmetic operators ##

# Matrix{Bool}/Matrix{Bool}

b1 = randbool(n1, n2)
b2 = randbool(n1, n2)
@check_bit_operation (&) BitMatrix (b1, b2)
@check_bit_operation (|) BitMatrix (b1, b2)
@check_bit_operation ($) BitMatrix (b1, b2)
@check_bit_operation (+) Matrix{Int} (b1, b2)
@check_bit_operation (-) Matrix{Int} (b1, b2)
@check_bit_operation (.*) BitMatrix (b1, b2)
@check_bit_operation (./) Matrix{Float32} (b1, b2)
@check_bit_operation (.^) BitMatrix (b1, b2)

b2 = trues(n1, n2)
@check_bit_operation div BitMatrix (b1, b2)
@check_bit_operation mod BitMatrix (b1, b2)

while true
    global b1
    b1 = randbool(n1, n1)
    if abs(det(float64(b1))) > 1e-6
        break
    end
end
b2 = randbool(n1, n1)

@check_bit_operation (*) Matrix{Int} (b1, b2)
@check_bit_operation (/) Matrix{Float32} (b1, b1)
@check_bit_operation (\) Matrix{Float32} (b1, b1)

b0 = falses(0)
@check_bit_operation (&) BitVector (b0, b0)
@check_bit_operation (|) BitVector (b0, b0)
@check_bit_operation ($) BitVector (b0, b0)
@check_bit_operation (.*) BitVector (b0, b0)
@check_bit_operation (*) Matrix{Int} (b0, b0')

# Matrix{Bool}/Matrix{Int}
b1 = randbool(n1, n2)
i2 = rand(1:10, n1, n2)
@check_bit_operation (&) Matrix{Int} (b1, i2)
@check_bit_operation (|) Matrix{Int} (b1, i2)
@check_bit_operation ($) Matrix{Int} (b1, i2)
@check_bit_operation (+) Matrix{Int} (b1, i2)
@check_bit_operation (-) Matrix{Int} (b1, i2)
@check_bit_operation (.*) Matrix{Int} (b1, i2)
@check_bit_operation (./) Matrix{Float64} (b1, i2)
@check_bit_operation (.^) BitMatrix (b1, i2)
@check_bit_operation div Matrix{Int} (b1, i2)
@check_bit_operation mod Matrix{Int} (b1, i2)

# Matrix{Bool}/Matrix{Float64}
b1 = randbool(n1, n2)
f2 = 1.0 + rand(n1, n2)
@check_bit_operation (.*) Matrix{Float64} (b1, f2)
@check_bit_operation (./) Matrix{Float64} (b1, f2)
@check_bit_operation (.^) Matrix{Float64} (b1, f2)
@check_bit_operation div Matrix{Float64} (b1, f2)
@check_bit_operation mod Matrix{Float64} (b1, f2)

# Number/Matrix
b2 = randbool(n1, n2)
i1 = rand(1:10)
u1 = uint8(i1)
f1 = float64(i1)
ci1 = complex(i1)
cu1 = complex(u1)
cf1 = complex(f1)

@check_bit_operation (&) Matrix{Int} (i1, b2)
@check_bit_operation (|) Matrix{Int} (i1, b2)
@check_bit_operation ($) Matrix{Int} (i1, b2)
@check_bit_operation (+) Matrix{Int} (i1, b2)
@check_bit_operation (-) Matrix{Int} (i1, b2)
@check_bit_operation (.*) Matrix{Int} (i1, b2)

@check_bit_operation (&) Matrix{Uint8} (u1, b2)
@check_bit_operation (|) Matrix{Uint8} (u1, b2)
@check_bit_operation ($) Matrix{Uint8} (u1, b2)
@check_bit_operation (+) Matrix{Uint8} (u1, b2)
@check_bit_operation (-) Matrix{Uint8} (u1, b2)
@check_bit_operation (.*) Matrix{Uint8} (u1, b2)

for (x1,t1) = {(f1, Float64),
               (ci1, Complex{Int}),
               (cu1, Complex{Uint8}),
               (cf1, Complex128)}
    @check_bit_operation (+) Matrix{t1} (x1, b2)
    @check_bit_operation (-) Matrix{t1} (x1, b2)
    @check_bit_operation (.*) Matrix{t1} (x1, b2)
end

b2 = trues(n1, n2)
@check_bit_operation (./) Matrix{Float32} (true, b2)
@check_bit_operation div BitMatrix (true, b2)
@check_bit_operation mod BitMatrix (true, b2)
@check_bit_operation (./) Matrix{Float32} (false, b2)
@check_bit_operation div BitMatrix (false, b2)
@check_bit_operation mod BitMatrix (false, b2)

@check_bit_operation (./) Matrix{Float64} (i1, b2)
@check_bit_operation div Matrix{Int} (i1, b2)
@check_bit_operation mod Matrix{Int} (i1, b2)

@check_bit_operation (./) Matrix{Float32} (u1, b2)
@check_bit_operation div Matrix{Uint8} (u1, b2)
@check_bit_operation mod Matrix{Uint8} (u1, b2)

@check_bit_operation (./) Matrix{Float64} (f1, b2)
@check_bit_operation div Matrix{Float64} (f1, b2)
@check_bit_operation mod Matrix{Float64} (f1, b2)

@check_bit_operation (./) Matrix{Complex128} (ci1, b2)
@check_bit_operation (./) Matrix{Complex64 } (cu1, b2)
@check_bit_operation (./) Matrix{Complex128} (cf1, b2)

b2 = randbool(n1, n2)
@check_bit_operation (.^) BitMatrix (false, b2)
@check_bit_operation (.^) BitMatrix (true, b2)
@check_bit_operation (.^) Matrix{Uint8} (0x0, b2)
@check_bit_operation (.^) Matrix{Uint8} (0x1, b2)
@check_bit_operation (.^) Matrix{Int} (-1, b2)
@check_bit_operation (.^) Matrix{Int} (0, b2)
@check_bit_operation (.^) Matrix{Int} (1, b2)
@check_bit_operation (.^) Matrix{Float64} (0.0, b2)
@check_bit_operation (.^) Matrix{Float64} (1.0, b2)
@check_bit_operation (.^) Matrix{Complex128} (0.0im, b2)
@check_bit_operation (.^) Matrix{Complex128} (1.0im, b2)
@check_bit_operation (.^) Matrix{Complex{Int}} (0im, b2)
@check_bit_operation (.^) Matrix{Complex{Int}} (1im, b2)
@check_bit_operation (.^) Matrix{Complex{Uint8}} (0x0im, b2)
@check_bit_operation (.^) Matrix{Complex{Uint8}} (0x1im, b2)

# Matrix/Number
b1 = randbool(n1, n2)
i2 = rand(1:10)
u2 = uint8(i2)
f2 = float64(i2)
ci2 = complex(i2)
cu2 = complex(u2)
cf2 = complex(f2)

@check_bit_operation (&) BitMatrix (b1, true)
@check_bit_operation (&) BitMatrix (b1, false)
@check_bit_operation (|) BitMatrix (b1, true)
@check_bit_operation (|) BitMatrix (b1, false)
@check_bit_operation ($) BitMatrix (b1, true)
@check_bit_operation ($) BitMatrix (b1, false)
@check_bit_operation (+) Matrix{Int} (b1, true)
@check_bit_operation (+) Matrix{Int} (b1, false)
@check_bit_operation (-) Matrix{Int} (b1, true)
@check_bit_operation (-) Matrix{Int} (b1, false)
@check_bit_operation (.*) BitMatrix (b1, true)
@check_bit_operation (.*) BitMatrix (b1, false)
@check_bit_operation (./) Matrix{Float32} (b1, true)
@check_bit_operation (./) Matrix{Float32} (b1, false)
@check_bit_operation div BitMatrix (b1, true)
@check_bit_operation mod BitMatrix (b1, true)

@check_bit_operation (&) Matrix{Int} (b1, i2)
@check_bit_operation (|) Matrix{Int} (b1, i2)
@check_bit_operation ($) Matrix{Int} (b1, i2)
@check_bit_operation (+) Matrix{Int} (b1, i2)
@check_bit_operation (-) Matrix{Int} (b1, i2)
@check_bit_operation (.*) Matrix{Int} (b1, i2)
@check_bit_operation (./) Matrix{Float64} (b1, i2)
@check_bit_operation div Matrix{Int} (b1, i2)
@check_bit_operation mod Matrix{Int} (b1, i2)

@check_bit_operation (&) Matrix{Uint8} (b1, u2)
@check_bit_operation (|) Matrix{Uint8} (b1, u2)
@check_bit_operation ($) Matrix{Uint8} (b1, u2)
@check_bit_operation (+) Matrix{Uint8} (b1, u2)
@check_bit_operation (-) Matrix{Uint8} (b1, u2)
@check_bit_operation (.*) Matrix{Uint8} (b1, u2)
@check_bit_operation (./) Matrix{Float32} (b1, u2)
@check_bit_operation div Matrix{Uint8} (b1, u2)
@check_bit_operation mod Matrix{Uint8} (b1, u2)

@check_bit_operation (+) Matrix{Float64} (b1, f2)
@check_bit_operation (-) Matrix{Float64} (b1, f2)
@check_bit_operation (.*) Matrix{Float64} (b1, f2)
@check_bit_operation (./) Matrix{Float64} (b1, f2)
@check_bit_operation div Matrix{Float64} (b1, f2)
@check_bit_operation mod Matrix{Float64} (b1, f2)

@check_bit_operation (+) Matrix{Complex{Int}} (b1, ci2)
@check_bit_operation (-) Matrix{Complex{Int}} (b1, ci2)
@check_bit_operation (.*) Matrix{Complex{Int}} (b1, ci2)
@check_bit_operation (./) Matrix{Complex128} (b1, ci2)

@check_bit_operation (+) Matrix{Complex{Uint8}} (b1, cu2)
@check_bit_operation (-) Matrix{Complex{Uint8}} (b1, cu2)
@check_bit_operation (.*) Matrix{Complex{Uint8}} (b1, cu2)
@check_bit_operation (./) Matrix{Complex64} (b1, cu2)

@check_bit_operation (+) Matrix{Complex128} (b1, cf2)
@check_bit_operation (-) Matrix{Complex128} (b1, cf2)
@check_bit_operation (.*) Matrix{Complex128} (b1, cf2)
@check_bit_operation (./) Matrix{Complex128} (b1, cf2)

@check_bit_operation (.^) BitMatrix (b1, false)
@check_bit_operation (.^) BitMatrix (b1, true)
@check_bit_operation (.^) BitMatrix (b1, 0x0)
@check_bit_operation (.^) BitMatrix (b1, 0x1)
@check_bit_operation (.^) BitMatrix (b1, 0)
@check_bit_operation (.^) BitMatrix (b1, 1)
@check_bit_operation (.^) Matrix{Float64} (b1, -1.0)
@check_bit_operation (.^) Matrix{Float64} (b1, 0.0)
@check_bit_operation (.^) Matrix{Float64} (b1, 1.0)
@check_bit_operation (.^) Matrix{Complex128} (b1, 0.0im)
@check_bit_operation (.^) Matrix{Complex64 } (b1, 0x0im)
@check_bit_operation (.^) Matrix{Complex128} (b1, 0im)

b1 = trues(n1, n2)
@check_bit_operation (.^) Matrix{Complex128} (b1, -1.0im)
@check_bit_operation (.^) Matrix{Complex128} (b1, 1.0im)
@check_bit_operation (.^) Matrix{Complex128} (b1, -1im)
@check_bit_operation (.^) Matrix{Complex128} (b1, 1im)
@check_bit_operation (.^) Matrix{Complex64 } (b1, 0x1im)

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
    j = rand(1:size(b1, d))
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
for m = [rand(1:v1)-1 0 1 63 64 65 191 192 193 v1-1]
    @test isequal(b1 << m, [ b1[m+1:end]; falses(m) ])
    @test isequal(b1 >>> m, [ falses(m); b1[1:end-m] ])
    @test isequal(rol(b1, m), [ b1[m+1:end]; b1[1:m] ])
    @test isequal(ror(b1, m), [ b1[end-m+1:end]; b1[1:end-m] ])
    @test isequal(ror(b1, m), rol(b1, -m))
    @test isequal(rol(b1, m), ror(b1, -m))
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

b1 = trues(v1)
for i = 0:v1-1
    @test findfirst(b1 >> i) == i+1
    @test Base.findfirstnot(~(b1 >> i)) == i+1
end

for i = 3:v1-1
    for j = 2:i
        submask = b1 << (v1-j+1)
        @test findnext((b1 >> i) | submask,j) == i+1
        @test Base.findnextnot((~(b1 >> i)) $ submask,j) == i+1
    end
end

b1 = randbool(n1, n2)
@check_bit_operation findnz (Vector{Int}, Vector{Int}, BitArray) (b1,)

timesofar("nnz&find")

## Reductions ##

b1 = randbool(s1, s2, s3, s4)
m1 = 1
m2 = 3
@check_bit_operation maximum BitArray{4} (b1, (m1, m2))
@check_bit_operation minimum BitArray{4} (b1, (m1, m2))
@check_bit_operation sum Array{Int,4} (b1, (m1, m2))

@check_bit_operation maximum Bool (b1,)
@check_bit_operation minimum Bool (b1,)
@check_bit_operation any Bool (b1,)
@check_bit_operation all Bool (b1,)
@check_bit_operation sum Int (b1,)

b0 = falses(0)
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

## Permutedims ##

b1 = randbool(s1, s2, s3, s4)
p = randperm(4)
@check_bit_operation permutedims BitArray{4} (b1, p)
@check_bit_operation permutedims BitArray{4} (b1, tuple(p...))

timesofar("permutedims")

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

b1 = randbool(n1, n1)
@check_bit_operation istril Bool (b1,)
b1 = randbool(n1, n2)
@check_bit_operation istril Bool (b1,)
b1 = randbool(n2, n1)
@check_bit_operation istril Bool (b1,)

b1 = tril(randbool(n1, n1))
@check_bit_operation istril Bool (b1,)
b1 = tril(randbool(n1, n2))
@check_bit_operation istril Bool (b1,)
b1 = tril(randbool(n2, n1))
@check_bit_operation istril Bool (b1,)

b1 = randbool(n1, n1)
@check_bit_operation istriu Bool (b1,)
b1 = randbool(n1, n2)
@check_bit_operation istriu Bool (b1,)
b1 = randbool(n2, n1)
@check_bit_operation istriu Bool (b1,)

b1 = triu(randbool(n1, n1))
@check_bit_operation istriu Bool (b1,)
b1 = triu(randbool(n1, n2))
@check_bit_operation istriu Bool (b1,)
b1 = triu(randbool(n2, n1))
@check_bit_operation istriu Bool (b1,)

b1 = randbool(n1)
b2 = randbool(n2)
@check_bit_operation kron BitVector (b1, b2)

b1 = randbool(s1, s2)
b2 = randbool(s3, s4)
@check_bit_operation kron BitMatrix (b1, b2)

#b1 = randbool(v1)
#@check_bit_operation diff Vector{Int} (b1,)
#b1 = randbool(n1, n2)
#@check_bit_operation diff Vector{Int} (b1,)

timesofar("linalg")

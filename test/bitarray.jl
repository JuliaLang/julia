tc{N}(r1::NTuple{N}, r2::NTuple{N}) = all(map(x->tc(x...), [zip(r1,r2)...]))
tc{N}(r1::BitArray{N}, r2::Union(BitArray{N},Array{Bool,N})) = true
tc{T}(r1::T, r2::T) = true
tc(r1,r2) = false

function check_bitop(ret_type, func, args...)
    r1 = func(args...)
    r2 = func(map(x->(isa(x, BitArray) ? bitunpack(x) : x), args)...)
    @test isa(r1, ret_type)
    @test tc(r1, r2)
    @test isequal(r1, convert(ret_type, r2))
end

macro check_bit_operation(ex, ret_type)
    @assert Meta.isexpr(ex, :call)
    Expr(:call, :check_bitop, esc(ret_type), map(esc,ex.args)...)
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
n1, n2 = 17, 20
# arrays size
s1, s2, s3, s4 = 5, 8, 3, 4

allsizes = [((), BitArray{0}), ((v1,), BitVector),
            ((n1,n2), BitMatrix), ((s1,s2,s3,s4), BitArray{4})]

## Conversions ##

for (sz,T) in allsizes
    b1 = rand!(falses(sz...))
    @test isequal(bitpack(bitunpack(b1)), b1)
    @test isequal(convert(Array{Float64,ndims(b1)}, b1),
                  convert(Array{Float64,ndims(b1)}, bitunpack(b1)))
    @test isequal(convert(AbstractArray{Float64,ndims(b1)}, b1),
                  convert(AbstractArray{Float64,ndims(b1)}, bitunpack(b1)))

    i1 = rand!(zeros(Bool, sz...), false:true)
    @test isequal(bitunpack(bitpack(i1)), i1)
end

timesofar("conversions")

## utility functions ##

b1 = randbool(v1)
@test isequal(fill!(b1, true), trues(size(b1)))
@test isequal(fill!(b1, false), falses(size(b1)))

for (sz,T) in allsizes
    @test isequal(bitunpack(trues(sz...)), ones(Bool, sz...))
    @test isequal(bitunpack(falses(sz...)), zeros(Bool, sz...))

    b1 = rand!(falses(sz...))
    @test isa(b1, T)

    @check_bit_operation length(b1) Int
    @check_bit_operation ndims(b1)  Int
    @check_bit_operation size(b1)   (Int...)

    b2 = similar(b1)
    @check_bit_operation copy!(b2, b1) T
end

timesofar("utils")

## Indexing ##

# 0d
for (sz,T) in allsizes
    b1 = rand!(falses(sz...))
    @check_bit_operation getindex(b1)         Bool
    @check_bit_operation setindex!(b1, true)  T
    @check_bit_operation setindex!(b1, false) T
    @check_bit_operation setindex!(b1, 1.0)  T
end

# linear
for (sz,T) in allsizes[2:end]
    l = *(sz...)
    b1 = rand!(falses(sz...))
    for j = 1:l
        @check_bit_operation getindex(b1, j) Bool
    end
    @check_bit_operation getindex(b1, 100.0) Bool

    for j in [0, 1, 63, 64, 65, 127, 128, 129, 191, 192, 193, l-1, l]
        @check_bit_operation getindex(b1, 1:j)   BitVector
        @check_bit_operation getindex(b1, j+1:l) BitVector
    end
    for j in [1, 63, 64, 65, 127, 128, 129, div(l,2)]
        m1 = j:(l-j)
        @check_bit_operation getindex(b1, m1) BitVector
    end
    @check_bit_operation getindex(b1, 1.0:100.0) BitVector

    t1 = find(randbool(l))
    @check_bit_operation getindex(b1, t1)        BitVector
    @check_bit_operation getindex(b1, float(t1)) BitVector

    for j = 1:l
        x = randbool()
        @check_bit_operation setindex!(b1, x, j) T
    end

    x = randbool()
    @check_bit_operation setindex!(b1, x, 100.0) T
    y = rand(0.0:1.0)
    @check_bit_operation setindex!(b1, y, 100) T
    y = rand(0.0:1.0)
    @check_bit_operation setindex!(b1, y, 100.0) T

    for j in [1, 63, 64, 65, 127, 128, 129, 191, 192, 193, l-1]
        x = randbool()
        @check_bit_operation setindex!(b1, x, 1:j) T
        b2 = randbool(j)
        @check_bit_operation setindex!(b1, b2, 1:j) T
        x = randbool()
        @check_bit_operation setindex!(b1, x, j+1:l) T
        b2 = randbool(l-j)
        @check_bit_operation setindex!(b1, b2, j+1:l) T
    end
    for j in [1, 63, 64, 65, 127, 128, 129, div(l,2)]
        m1 = j:(l-j)
        x = randbool()
        @check_bit_operation setindex!(b1, x, m1) T
        b2 = randbool(length(m1))
        @check_bit_operation setindex!(b1, b2, m1) T
    end
    x = randbool()
    @check_bit_operation setindex!(b1, x, 1.0:100.0) T
    b2 = randbool(100)
    @check_bit_operation setindex!(b1, b2, 1.0:100.0) T

    y = rand(0.0:1.0)
    @check_bit_operation setindex!(b1, y, 1:100) T
    f2 = float(randbool(100))
    @check_bit_operation setindex!(b1, f2, 1:100) T
    f2 = float(randbool(100))
    @check_bit_operation setindex!(b1, f2, 1.0:100.0) T

    t1 = find(randbool(l))
    x = randbool()
    @check_bit_operation setindex!(b1, x, t1) T
    b2 = randbool(length(t1))
    @check_bit_operation setindex!(b1, b2, t1) T

    y = rand(0.0:1.0)
    @check_bit_operation setindex!(b1, y, t1) T
    f2 = float(randbool(length(t1)))
    @check_bit_operation setindex!(b1, f2, t1) T

    ft1 = float(t1)
    x = randbool()
    @check_bit_operation setindex!(b1, x, ft1) T
    b2 = randbool(length(t1))
    @check_bit_operation setindex!(b1, b2, ft1) T

    y = rand(0.0:1.0)
    @check_bit_operation setindex!(b1, y, ft1) T
    f2 = float(randbool(length(t1)))
    @check_bit_operation setindex!(b1, f2, ft1) T
end

# multidimensional

rand_m1m2() = rand(1:n1), rand(1:n2)

b1 = randbool(n1, n2)

m1, m2 = rand_m1m2()
b2 = randbool(m1, m2)
@check_bit_operation copy!(b1, b2) BitMatrix

function gen_getindex_data()
    m1, m2 = rand_m1m2()
    produce((m1, m2, Bool))
    m1, m2 = rand_m1m2()
    produce((m1, 1:m2, BitMatrix))
    m1, m2 = rand_m1m2()
    produce((m1, randperm(m2), BitMatrix))
    m1, m2 = rand_m1m2()
    produce((1:m1, m2, BitVector))
    m1, m2 = rand_m1m2()
    produce((1:m1, 1:m2, BitMatrix))
    m1, m2 = rand_m1m2()
    produce((1:m1, randperm(m2), BitMatrix))
    m1, m2 = rand_m1m2()
    produce((randperm(m1), m2, BitVector))
    m1, m2 = rand_m1m2()
    produce((randperm(m1), 1:m2, BitMatrix))
    m1, m2 = rand_m1m2()
    produce((randperm(m1), randperm(m2), BitMatrix))
end

for (k1, k2, T) in Task(gen_getindex_data)
    # println(typeof(k1), " ", typeof(k2), " ", T) # uncomment to debug
    @check_bit_operation getindex(b1, k1, k2) T
    @check_bit_operation getindex(b1, k1, k2, 1) T

    #@check_bit_operation getindex(b1, float(k1), k2) T
    #@check_bit_operation getindex(b1, k1, float(k2)) T

    @check_bit_operation getindex(b1, float(k1), float(k2)) T

    @check_bit_operation getindex(b1, k1, k2, 1.0) T
    #@check_bit_operation getindex(b1, float(k1), float(k2), 1.0) T
end

function gen_setindex_data()
    m1, m2 = rand_m1m2()
    produce((randbool(), m1, m2))
    m1, m2 = rand_m1m2()
    produce((randbool(), m1, 1:m2))
    produce((randbool(m2), m1, 1:m2))
    m1, m2 = rand_m1m2()
    produce((randbool(), m1, randperm(m2)))
    produce((randbool(m2), m1, randperm(m2)))
    m1, m2 = rand_m1m2()
    produce((randbool(), 1:m1, m2))
    produce((randbool(m1), 1:m1, m2))
    m1, m2 = rand_m1m2()
    produce((randbool(), 1:m1, 1:m2))
    produce((randbool(m1, m2), 1:m1, 1:m2))
    m1, m2 = rand_m1m2()
    produce((randbool(), 1:m1, randperm(m2)))
    produce((randbool(m1, m2), 1:m1, randperm(m2)))
    m1, m2 = rand_m1m2()
    produce((randbool(), randperm(m1), m2))
    produce((randbool(m1), randperm(m1), m2))
    m1, m2 = rand_m1m2()
    produce((randbool(), randperm(m1), 1:m2))
    produce((randbool(m1,m2), randperm(m1), 1:m2))
    m1, m2 = rand_m1m2()
    produce((randbool(), randperm(m1), randperm(m2)))
    produce((randbool(m1,m2), randperm(m1), randperm(m2)))
end

for (b2, k1, k2) in Task(gen_setindex_data)
    # println(typeof(b2), " ", typeof(k1), " ", typeof(k2)) # uncomment to debug
    @check_bit_operation setindex!(b1, b2, k1, k2) BitMatrix

    @check_bit_operation setindex!(b1, float(b2), k1, k2) BitMatrix
    @check_bit_operation setindex!(b1, b2, float(k1), k2) BitMatrix
    #@check_bit_operation setindex!(b1, b2, k1, float(k2)) BitMatrix

    #@check_bit_operation setindex!(b1, float(b2), float(k1), k2) BitMatrix
    #@check_bit_operation setindex!(b1, float(b2), k1, float(k2)) BitMatrix
    #@check_bit_operation setindex!(b1, b2, float(k1), float(k2)) BitMatrix

    @check_bit_operation setindex!(b1, float(b2), float(k1), float(k2)) BitMatrix
end

m1, m2 = rand_m1m2()
b2 = randbool(1, 1, m2)
@check_bit_operation setindex!(b1, b2, m1, 1:m2) BitMatrix
x = randbool()
b2 = randbool(1, m2, 1)
@check_bit_operation setindex!(b1, x, m1, 1:m2, 1)  BitMatrix
@check_bit_operation setindex!(b1, b2, m1, 1:m2, 1) BitMatrix

for p1 = [rand(1:v1) 1 63 64 65 191 192 193]
    for p2 = [rand(1:v1) 1 63 64 65 191 192 193]
        for n = 0 : min(v1 - p1 + 1, v1 - p2 + 1)
            b1 = randbool(v1)
            b2 = randbool(v1)
            @check_bit_operation copy!(b1, p1, b2, p2, n) BitVector
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


b1 = randbool(n1, n2)
t1 = randbool(n1, n2)
@check_bit_operation setindex!(b1, true, t1) BitMatrix

t1 = randbool(n1, n2)
b2 = randbool(countnz(t1))
@check_bit_operation setindex!(b1, b2, t1) BitMatrix

m1 = rand(1:n1)
m2 = rand(1:n2)

t1 = randbool(n1)
b2 = randbool(countnz(t1), m2)
k2 = randperm(m2)
@check_bit_operation setindex!(b1, b2, t1, 1:m2)       BitMatrix
@check_bit_operation setindex!(b1, b2, t1, n2-m2+1:n2) BitMatrix
@check_bit_operation setindex!(b1, b2, t1, k2)         BitMatrix

t2 = randbool(n2)
b2 = randbool(m1, countnz(t2))
k1 = randperm(m1)
@check_bit_operation setindex!(b1, b2, 1:m1, t2)       BitMatrix
@check_bit_operation setindex!(b1, b2, n1-m1+1:n1, t2) BitMatrix
@check_bit_operation setindex!(b1, b2, k1, t2)         BitMatrix

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
@test_throws BoundsError insert!(b1, 2, false)
@test_throws BoundsError insert!(b1, 0, false)
i1 = bitunpack(b1)
for m = 1 : v1
    j = rand(1:m)
    x = randbool()
    @test insert!(b1, j, x) === b1
    insert!(i1, j, x)
    @test isequal(bitunpack(b1), i1)
end

b1 = randbool(v1)
i1 = bitunpack(b1)
for j in [63, 64, 65, 127, 128, 129, 191, 192, 193]
    x = rand(0:1)
    @test insert!(b1, j, x) === b1
    insert!(i1, j, x)
    @test isequal(bitunpack(b1), i1)
end

b1 = randbool(v1)
i1 = bitunpack(b1)
for m = v1 : -1 : 1
    j = rand(1:m)
    b = splice!(b1, j)
    i = splice!(i1, j)
    @test isequal(bitunpack(b1), i1)
    @test b == i
end
@test length(b1) == 0

b1 = randbool(v1)
i1 = bitunpack(b1)
for m = v1 : -1 : 1
    j = rand(1:m)
    deleteat!(b1, j)
    deleteat!(i1, j)
    @test isequal(bitunpack(b1), i1)
end
@test length(b1) == 0

b1 = randbool(v1)
i1 = bitunpack(b1)
for j in [63, 64, 65, 127, 128, 129, 191, 192, 193]
    b = splice!(b1, j)
    i = splice!(i1, j)
    @test isequal(bitunpack(b1), i1)
    @test b == i
end

b1 = randbool(v1)
i1 = bitunpack(b1)
for j in [63, 64, 65, 127, 128, 129, 191, 192, 193]
    deleteat!(b1, j)
    deleteat!(i1, j)
    @test isequal(bitunpack(b1), i1)
end

b1 = randbool(v1)
i1 = bitunpack(b1)
for m1 = 1 : v1
    for m2 = m1 : v1
        b2 = copy(b1)
        i2 = copy(i1)
        b = splice!(b2, m1:m2)
        i = splice!(i2, m1:m2)
        @test isequal(bitunpack(b2), i2)
        @test b == i
    end
end

b1 = randbool(v1)
i1 = bitunpack(b1)
for m1 = 1 : v1
    for m2 = m1 : v1
        b2 = copy(b1)
        i2 = copy(i1)
        deleteat!(b2, m1:m2)
        deleteat!(i2, m1:m2)
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
            b = splice!(b2, m1:m2, b3)
            i = splice!(i2, m1:m2, i3)
            @test isequal(bitunpack(b2), i2)
            @test b == i
            b2 = copy(b1)
            i2 = copy(i1)
            i3 = int(randbool(v2))
            b = splice!(b2, m1:m2, i3)
            i = splice!(i2, m1:m2, i3)
            @test isequal(bitunpack(b2), i2)
            @test b == i
            b2 = copy(b1)
            i2 = copy(i1)
            i3 = [j => rand(0:3) for j = 1:v2]
            b = splice!(b2, m1:m2, values(i3))
            i = splice!(i2, m1:m2, values(i3))
            @test isequal(bitunpack(b2), i2)
            @test b == i
        end
    end
end

b1 = randbool(v1)
i1 = bitunpack(b1)
for m1 = 1 : v1
    for v2 = [0, 1, 63, 64, 65, 127, 128, 129, 191, 192, 193, rand(1:v1)]
        b2 = copy(b1)
        i2 = copy(i1)
        b3 = randbool(v2)
        i3 = bitunpack(b3)
        b = splice!(b2, m1, b3)
        i = splice!(i2, m1, i3)
        @test isequal(bitunpack(b2), i2)
        @test b == i
        b2 = copy(b1)
        i2 = copy(i1)
        i3 = int(randbool(v2))
        b = splice!(b2, m1:m2, i3)
        i = splice!(i2, m1:m2, i3)
        @test isequal(bitunpack(b2), i2)
        @test b == i
        b2 = copy(b1)
        i2 = copy(i1)
        i3 = [j => rand(0:3) for j = 1:v2]
        b = splice!(b2, m1:m2, values(i3))
        i = splice!(i2, m1:m2, values(i3))
        @test isequal(bitunpack(b2), i2)
        @test b == i
    end
end

b1 = randbool(v1)
i1 = bitunpack(b1)
for m1 = 1 : v1 - 1
    for m2 = m1 + 1 : v1
        locs = randbool(m2-m1+1)
        m = [m1:m2...][locs]
        b2 = copy(b1)
        i2 = copy(i1)
        deleteat!(b2, m)
        deleteat!(i2, m)
        @test isequal(bitunpack(b2), i2)
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
@check_bit_operation (~)(b1)  BitMatrix
@check_bit_operation (!)(b1)  BitMatrix
@check_bit_operation (-)(b1)  Matrix{Int}
@check_bit_operation sign(b1) BitMatrix
@check_bit_operation real(b1) BitMatrix
@check_bit_operation imag(b1) BitMatrix
@check_bit_operation conj(b1) BitMatrix

b0 = falses(0)
@check_bit_operation (~)(b0)  BitVector
@check_bit_operation (!)(b0)  BitVector
@check_bit_operation (-)(b0)  Vector{Int}
@check_bit_operation sign(b0) BitVector

timesofar("unary arithmetic")

## Binary arithmetic operators ##

# Matrix{Bool}/Matrix{Bool}

b1 = randbool(n1, n2)
b2 = randbool(n1, n2)
@check_bit_operation (&)(b1, b2)  BitMatrix
@check_bit_operation (|)(b1, b2)  BitMatrix
@check_bit_operation ($)(b1, b2)  BitMatrix
@check_bit_operation (+)(b1, b2)  Matrix{Int}
@check_bit_operation (-)(b1, b2)  Matrix{Int}
@check_bit_operation (.*)(b1, b2) BitMatrix
@check_bit_operation (./)(b1, b2) Matrix{Float64}
@check_bit_operation (.^)(b1, b2) BitMatrix

b2 = trues(n1, n2)
@check_bit_operation div(b1, b2) BitMatrix
@check_bit_operation mod(b1, b2) BitMatrix

while true
    global b1
    b1 = randbool(n1, n1)
    if abs(det(float64(b1))) > 1e-6
        break
    end
end
b2 = randbool(n1, n1)

@check_bit_operation (*)(b1, b2) Matrix{Int}
@check_bit_operation (/)(b1, b1) Matrix{Float64}
@check_bit_operation (\)(b1, b1) Matrix{Float64}

b0 = falses(0)
@check_bit_operation (&)(b0, b0)  BitVector
@check_bit_operation (|)(b0, b0)  BitVector
@check_bit_operation ($)(b0, b0)  BitVector
@check_bit_operation (.*)(b0, b0) BitVector
@check_bit_operation (*)(b0, b0') Matrix{Int}

# Matrix{Bool}/Matrix{Int}
b1 = randbool(n1, n2)
i2 = rand(1:10, n1, n2)
@check_bit_operation (&)(b1, i2)  Matrix{Int}
@check_bit_operation (|)(b1, i2)  Matrix{Int}
@check_bit_operation ($)(b1, i2)  Matrix{Int}
@check_bit_operation (+)(b1, i2)  Matrix{Int}
@check_bit_operation (-)(b1, i2)  Matrix{Int}
@check_bit_operation (.*)(b1, i2) Matrix{Int}
@check_bit_operation (./)(b1, i2) Matrix{Float64}
@check_bit_operation (.^)(b1, i2) BitMatrix
@check_bit_operation div(b1, i2)  Matrix{Int}
@check_bit_operation mod(b1, i2)  Matrix{Int}

# Matrix{Bool}/Matrix{Float64}
b1 = randbool(n1, n2)
f2 = 1.0 .+ rand(n1, n2)
@check_bit_operation (.*)(b1, f2) Matrix{Float64}
@check_bit_operation (./)(b1, f2) Matrix{Float64}
@check_bit_operation (.^)(b1, f2) Matrix{Float64}
@check_bit_operation div(b1, f2)  Matrix{Float64}
@check_bit_operation mod(b1, f2)  Matrix{Float64}

# Number/Matrix
b2 = randbool(n1, n2)
i1 = rand(1:10)
u1 = uint8(i1)
f1 = float64(i1)
ci1 = complex(i1)
cu1 = complex(u1)
cf1 = complex(f1)

@check_bit_operation (&)(i1, b2)  Matrix{Int}
@check_bit_operation (|)(i1, b2)  Matrix{Int}
@check_bit_operation ($)(i1, b2)  Matrix{Int}
@check_bit_operation (.+)(i1, b2)  Matrix{Int}
@check_bit_operation (.-)(i1, b2)  Matrix{Int}
@check_bit_operation (.*)(i1, b2) Matrix{Int}

@check_bit_operation (&)(u1, b2)  Matrix{UInt8}
@check_bit_operation (|)(u1, b2)  Matrix{UInt8}
@check_bit_operation ($)(u1, b2)  Matrix{UInt8}
@check_bit_operation (.+)(u1, b2)  Matrix{UInt8}
@check_bit_operation (.-)(u1, b2)  Matrix{UInt8}
@check_bit_operation (.*)(u1, b2) Matrix{UInt8}

for (x1,t1) = [(f1, Float64),
              (ci1, Complex{Int}),
              (cu1, Complex{UInt8}),
              (cf1, Complex128)]
    @check_bit_operation (.+)(x1, b2)  Matrix{t1}
    @check_bit_operation (.-)(x1, b2)  Matrix{t1}
    @check_bit_operation (.*)(x1, b2) Matrix{t1}
end

b2 = trues(n1, n2)
@check_bit_operation (./)(true, b2)  Matrix{Float64}
@check_bit_operation div(true, b2)   BitMatrix
@check_bit_operation mod(true, b2)   BitMatrix
@check_bit_operation (./)(false, b2) Matrix{Float64}
@check_bit_operation div(false, b2)  BitMatrix
@check_bit_operation mod(false, b2)  BitMatrix

@check_bit_operation (./)(i1, b2) Matrix{Float64}
@check_bit_operation div(i1, b2)  Matrix{Int}
@check_bit_operation mod(i1, b2)  Matrix{Int}

@check_bit_operation (./)(u1, b2) Matrix{Float64}
@check_bit_operation div(u1, b2)  Matrix{UInt8}
@check_bit_operation mod(u1, b2)  Matrix{UInt8}

@check_bit_operation (./)(f1, b2) Matrix{Float64}
@check_bit_operation div(f1, b2)  Matrix{Float64}
@check_bit_operation mod(f1, b2)  Matrix{Float64}

@check_bit_operation (./)(ci1, b2) Matrix{Complex128}
@check_bit_operation (./)(cu1, b2) Matrix{Complex128}
@check_bit_operation (./)(cf1, b2) Matrix{Complex128}

b2 = randbool(n1, n2)
@check_bit_operation (.^)(false, b2) BitMatrix
@check_bit_operation (.^)(true, b2)  BitMatrix
@check_bit_operation (.^)(0x0, b2)   Matrix{UInt8}
@check_bit_operation (.^)(0x1, b2)   Matrix{UInt8}
@check_bit_operation (.^)(-1, b2)    Matrix{Int}
@check_bit_operation (.^)(0, b2)     Matrix{Int}
@check_bit_operation (.^)(1, b2)     Matrix{Int}
@check_bit_operation (.^)(0.0, b2)   Matrix{Float64}
@check_bit_operation (.^)(1.0, b2)   Matrix{Float64}
@check_bit_operation (.^)(0.0im, b2) Matrix{Complex128}
@check_bit_operation (.^)(1.0im, b2) Matrix{Complex128}
@check_bit_operation (.^)(0im, b2)   Matrix{Complex{Int}}
@check_bit_operation (.^)(1im, b2)   Matrix{Complex{Int}}
@check_bit_operation (.^)(0x0im, b2) Matrix{Complex{UInt8}}
@check_bit_operation (.^)(0x1im, b2) Matrix{Complex{UInt8}}

# Matrix/Number
b1 = randbool(n1, n2)
i2 = rand(1:10)
u2 = uint8(i2)
f2 = float64(i2)
ci2 = complex(i2)
cu2 = complex(u2)
cf2 = complex(f2)

@check_bit_operation (&)(b1, true)   BitMatrix
@check_bit_operation (&)(b1, false)  BitMatrix
@check_bit_operation (|)(b1, true)   BitMatrix
@check_bit_operation (|)(b1, false)  BitMatrix
@check_bit_operation ($)(b1, true)   BitMatrix
@check_bit_operation ($)(b1, false)  BitMatrix
@check_bit_operation (.+)(b1, true)   Matrix{Int}
@check_bit_operation (.+)(b1, false)  Matrix{Int}
@check_bit_operation (.-)(b1, true)   Matrix{Int}
@check_bit_operation (.-)(b1, false)  Matrix{Int}
@check_bit_operation (.*)(b1, true)  BitMatrix
@check_bit_operation (.*)(b1, false) BitMatrix
@check_bit_operation (./)(b1, true)  Matrix{Float64}
@check_bit_operation (./)(b1, false) Matrix{Float64}
@check_bit_operation div(b1, true)   BitMatrix
@check_bit_operation mod(b1, true)   BitMatrix

@check_bit_operation (&)(b1, i2)  Matrix{Int}
@check_bit_operation (|)(b1, i2)  Matrix{Int}
@check_bit_operation ($)(b1, i2)  Matrix{Int}
@check_bit_operation (.+)(b1, i2)  Matrix{Int}
@check_bit_operation (.-)(b1, i2)  Matrix{Int}
@check_bit_operation (.*)(b1, i2) Matrix{Int}
@check_bit_operation (./)(b1, i2) Matrix{Float64}
@check_bit_operation div(b1, i2)  Matrix{Int}
@check_bit_operation mod(b1, i2)  Matrix{Int}

@check_bit_operation (&)(b1, u2)  Matrix{UInt8}
@check_bit_operation (|)(b1, u2)  Matrix{UInt8}
@check_bit_operation ($)(b1, u2)  Matrix{UInt8}
@check_bit_operation (.+)(b1, u2)  Matrix{UInt8}
@check_bit_operation (.-)(b1, u2)  Matrix{UInt8}
@check_bit_operation (.*)(b1, u2) Matrix{UInt8}
@check_bit_operation (./)(b1, u2) Matrix{Float64}
@check_bit_operation div(b1, u2)  Matrix{UInt8}
@check_bit_operation mod(b1, u2)  Matrix{UInt8}

@check_bit_operation (.+)(b1, f2)  Matrix{Float64}
@check_bit_operation (.-)(b1, f2)  Matrix{Float64}
@check_bit_operation (.*)(b1, f2) Matrix{Float64}
@check_bit_operation (./)(b1, f2) Matrix{Float64}
@check_bit_operation div(b1, f2)  Matrix{Float64}
@check_bit_operation mod(b1, f2)  Matrix{Float64}

@check_bit_operation (.+)(b1, ci2)  Matrix{Complex{Int}}
@check_bit_operation (.-)(b1, ci2)  Matrix{Complex{Int}}
@check_bit_operation (.*)(b1, ci2) Matrix{Complex{Int}}
@check_bit_operation (./)(b1, ci2) Matrix{Complex128}

@check_bit_operation (.+)(b1, cu2)  Matrix{Complex{UInt8}}
@check_bit_operation (.-)(b1, cu2)  Matrix{Complex{UInt8}}
@check_bit_operation (.*)(b1, cu2) Matrix{Complex{UInt8}}
@check_bit_operation (./)(b1, cu2) Matrix{Complex128}

@check_bit_operation (.+)(b1, cf2)  Matrix{Complex128}
@check_bit_operation (.-)(b1, cf2)  Matrix{Complex128}
@check_bit_operation (.*)(b1, cf2) Matrix{Complex128}
@check_bit_operation (./)(b1, cf2) Matrix{Complex128}

@check_bit_operation (.^)(b1, false) BitMatrix
@check_bit_operation (.^)(b1, true)  BitMatrix
@check_bit_operation (.^)(b1, 0x0)   BitMatrix
@check_bit_operation (.^)(b1, 0x1)   BitMatrix
@check_bit_operation (.^)(b1, 0)     BitMatrix
@check_bit_operation (.^)(b1, 1)     BitMatrix
@check_bit_operation (.^)(b1, -1.0)  Matrix{Float64}
@check_bit_operation (.^)(b1, 0.0)   Matrix{Float64}
@check_bit_operation (.^)(b1, 1.0)   Matrix{Float64}
@check_bit_operation (.^)(b1, 0.0im) Matrix{Complex128}
@check_bit_operation (.^)(b1, 0x0im) Matrix{Complex128}
@check_bit_operation (.^)(b1, 0im)   Matrix{Complex128}

b1 = trues(n1, n2)
@check_bit_operation (.^)(b1, -1.0im) Matrix{Complex128}
@check_bit_operation (.^)(b1, 1.0im)  Matrix{Complex128}
@check_bit_operation (.^)(b1, -1im)   Matrix{Complex128}
@check_bit_operation (.^)(b1, 1im)    Matrix{Complex128}
@check_bit_operation (.^)(b1, 0x1im)  Matrix{Complex128}

timesofar("binary arithmetic")

## Binary comparison operators ##

b1 = randbool(n1, n2)
b2 = randbool(n1, n2)
@check_bit_operation (.==)(b1, b2) BitMatrix
@check_bit_operation (.!=)(b1, b2) BitMatrix
@check_bit_operation (.<)(b1, b2) BitMatrix
@check_bit_operation (.<=)(b1, b2) BitMatrix

timesofar("binary comparison")

## Data movement ##

b1 = randbool(s1, s2, s3, s4)
for d = 1 : 4
    j = rand(1:size(b1, d))
    #for j = 1 : size(b1, d)
        @check_bit_operation slicedim(b1, d, j) BitArray{4}
    #end
    @check_bit_operation flipdim(b1, d) BitArray{4}
end

b1 = randbool(n1, n2)
for k = 1 : 4
    @check_bit_operation rotl90(b1, k) BitMatrix
end

for m = 0 : v1
    b1 = randbool(m)
    @check_bit_operation reverse(b1) BitVector
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

## countnz & find ##

for m = 0:v1, b1 in Any[randbool(m), trues(m), falses(m)]
    @check_bit_operation countnz(b1) Int

    @check_bit_operation findfirst(b1) Int

    @check_bit_operation findfirst(b1, true)  Int
    @check_bit_operation findfirst(b1, false) Int
    @check_bit_operation findfirst(b1, 3)     Int

    @check_bit_operation findfirst(x->x, b1)     Int
    @check_bit_operation findfirst(x->!x, b1)    Int
    @check_bit_operation findfirst(x->true, b1)  Int
    @check_bit_operation findfirst(x->false, b1) Int

    @check_bit_operation find(b1) Vector{Int}
end

b1 = trues(v1)
for i = 0:v1-1
    @test findfirst(b1 >> i) == i+1
    @test Base.findfirstnot(~(b1 >> i)) == i+1
end

for i = 3:v1-1
    for j = 2:i
        submask = b1 << (v1-j+1)
        @test findnext((b1 >> i) | submask, j) == i+1
        @test Base.findnextnot((~(b1 >> i)) $ submask, j) == i+1
    end
end

b1 = randbool(n1, n2)
@check_bit_operation findnz(b1) (Vector{Int}, Vector{Int}, BitArray)

timesofar("nnz&find")

## Reductions ##

b1 = randbool(s1, s2, s3, s4)
m1 = 1
m2 = 3
@check_bit_operation maximum(b1, (m1, m2)) BitArray{4}
@check_bit_operation minimum(b1, (m1, m2)) BitArray{4}
@check_bit_operation sum(b1, (m1, m2)) Array{Int,4}

@check_bit_operation maximum(b1) Bool
@check_bit_operation minimum(b1) Bool
@check_bit_operation any(b1) Bool
@check_bit_operation all(b1) Bool
@check_bit_operation sum(b1) Int

b0 = falses(0)
@check_bit_operation any(b0) Bool
@check_bit_operation all(b0) Bool
@check_bit_operation sum(b0) Int

timesofar("reductions")

## map over bitarrays ##

# TODO (not implemented)

## Filter ##

# TODO

## Transpose ##

b1 = randbool(v1)
@check_bit_operation transpose(b1) BitMatrix

for m1 = 0 : n1
    for m2 = 0 : n2
        b1 = randbool(m1, m2)
        @check_bit_operation transpose(b1) BitMatrix
    end
end

timesofar("transpose")

## Permutedims ##

b1 = randbool(s1, s2, s3, s4)
p = randperm(4)
@check_bit_operation permutedims(b1, p) BitArray{4}
@check_bit_operation permutedims(b1, tuple(p...)) BitArray{4}

timesofar("permutedims")

## Concatenation ##

b1 = randbool(v1)
b2 = randbool(v1)
@check_bit_operation hcat(b1, b2) BitMatrix
for m = 1 : v1 - 1
    @check_bit_operation vcat(b1[1:m], b1[m+1:end]) BitVector
end

b1 = randbool(n1, n2)
b2 = randbool(n1)
b3 = randbool(n1, n2)
b4 = randbool(1, n2)
@check_bit_operation hcat(b1, b2, b3) BitMatrix
@check_bit_operation vcat(b1, b4, b3) BitMatrix

b1 = randbool(s1, s2, s3, s4)
b2 = randbool(s1, s3, s3, s4)
b3 = randbool(s1, s2, s3, s1)
@check_bit_operation cat(2, b1, b2) BitArray{4}
@check_bit_operation cat(4, b1, b3) BitArray{4}
@check_bit_operation cat(6, b1, b1) BitArray{6}

b1 = randbool(1, v1, 1)
@check_bit_operation cat(2, 0, b1, 1, 1, b1) Array{Int,3}
@check_bit_operation cat(2, 3, b1, 4, 5, b1) Array{Int,3}
@check_bit_operation cat(2, false, b1, true, true, b1) BitArray{3}

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
@check_bit_operation dot(b1, b2) Int

b1 = randbool(n1, n2)
for k = -max(n1,n2) : max(n1,n2)
    @check_bit_operation tril(b1, k) BitMatrix
    @check_bit_operation triu(b1, k) BitMatrix
end

b1 = randbool(n1, n1)
@check_bit_operation istril(b1) Bool
b1 = randbool(n1, n2)
@check_bit_operation istril(b1) Bool
b1 = randbool(n2, n1)
@check_bit_operation istril(b1) Bool

b1 = tril(randbool(n1, n1))
@check_bit_operation istril(b1) Bool
b1 = tril(randbool(n1, n2))
@check_bit_operation istril(b1) Bool
b1 = tril(randbool(n2, n1))
@check_bit_operation istril(b1) Bool

b1 = randbool(n1, n1)
@check_bit_operation istriu(b1) Bool
b1 = randbool(n1, n2)
@check_bit_operation istriu(b1) Bool
b1 = randbool(n2, n1)
@check_bit_operation istriu(b1) Bool

b1 = triu(randbool(n1, n1))
@check_bit_operation istriu(b1) Bool
b1 = triu(randbool(n1, n2))
@check_bit_operation istriu(b1) Bool
b1 = triu(randbool(n2, n1))
@check_bit_operation istriu(b1) Bool

b1 = randbool(n1,n1)
b1 |= b1.'
@check_bit_operation issym(b1) Bool

b1 = randbool(n1)
b2 = randbool(n2)
@check_bit_operation kron(b1, b2) BitVector

b1 = randbool(s1, s2)
b2 = randbool(s3, s4)
@check_bit_operation kron(b1, b2) BitMatrix

#b1 = randbool(v1)
#@check_bit_operation diff(b1) Vector{Int}
#b1 = randbool(n1, n2)
#@check_bit_operation diff(b1) Vector{Int}

timesofar("linalg")

# issue #7515
@test sizeof(BitArray(64)) == 8
@test sizeof(BitArray(65)) == 16

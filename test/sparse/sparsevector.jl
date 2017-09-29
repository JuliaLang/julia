# This file is a part of Julia. License is MIT: https://julialang.org/license

### Data

spv_x1 = SparseVector(8, [2, 5, 6], [1.25, -0.75, 3.5])

@test isa(spv_x1, SparseVector{Float64,Int})

x1_full = zeros(length(spv_x1))
x1_full[SparseArrays.nonzeroinds(spv_x1)] = nonzeros(spv_x1)

### Basic Properties

let x = spv_x1
    @test eltype(x) == Float64
    @test ndims(x) == 1
    @test length(x) == 8
    @test size(x) == (8,)
    @test size(x,1) == 8
    @test size(x,2) == 1
    @test !isempty(x)

    @test count(!iszero, x) == 3
    @test nnz(x) == 3
    @test SparseArrays.nonzeroinds(x) == [2, 5, 6]
    @test nonzeros(x) == [1.25, -0.75, 3.5]
end

@test count(SparseVector(8, [2, 5, 6], [true,false,true])) == 2

# full

for (x, xf) in [(spv_x1, x1_full)]
    @test isa(Array(x), Vector{Float64})
    @test Array(x) == xf
end

### Show

@test contains(string(spv_x1), "1.25")
@test contains(string(spv_x1), "-0.75")
@test contains(string(spv_x1), "3.5")

### Other Constructors

### Comparison helper to ensure exact equality with internal structure
function exact_equal(x::AbstractSparseVector, y::AbstractSparseVector)
    eltype(x) == eltype(y) &&
    eltype(SparseArrays.nonzeroinds(x)) == eltype(SparseArrays.nonzeroinds(y)) &&
    length(x) == length(y) &&
    SparseArrays.nonzeroinds(x) == SparseArrays.nonzeroinds(y) &&
    nonzeros(x) == nonzeros(y)
end

# construct empty sparse vector

@test exact_equal(spzeros(Float64, 8), SparseVector(8, Int[], Float64[]))

# from list of indices and values

@test exact_equal(
    sparsevec(Int[], Float64[], 8),
    SparseVector(8, Int[], Float64[]))

@test exact_equal(
    sparsevec(Int[], Float64[]),
    SparseVector(0, Int[], Float64[]))

@test exact_equal(
    sparsevec([3, 3], [5.0, -5.0], 8),
    SparseVector(8, [3], [0.0]))

@test exact_equal(
    sparsevec([2, 3, 6], [12.0, 18.0, 25.0]),
    SparseVector(6, [2, 3, 6], [12.0, 18.0, 25.0]))

let x0 = SparseVector(8, [2, 3, 6], [12.0, 18.0, 25.0])
    @test exact_equal(
        sparsevec([2, 3, 6], [12.0, 18.0, 25.0], 8), x0)

    @test exact_equal(
        sparsevec([3, 6, 2], [18.0, 25.0, 12.0], 8), x0)

    @test exact_equal(
        sparsevec([2, 3, 4, 4, 6], [12.0, 18.0, 5.0, -5.0, 25.0], 8),
        SparseVector(8, [2, 3, 4, 6], [12.0, 18.0, 0.0, 25.0]))

    @test exact_equal(
        sparsevec([1, 1, 1, 2, 3, 3, 6], [2.0, 3.0, -5.0, 12.0, 10.0, 8.0, 25.0], 8),
        SparseVector(8, [1, 2, 3, 6], [0.0, 12.0, 18.0, 25.0]))

    @test exact_equal(
        sparsevec([2, 3, 6, 7, 7], [12.0, 18.0, 25.0, 5.0, -5.0], 8),
        SparseVector(8, [2, 3, 6, 7], [12.0, 18.0, 25.0, 0.0]))
end

@test exact_equal(
    sparsevec(Any[1, 3], [1, 1]),
    sparsevec([1, 3], [1, 1]))

@test exact_equal(
    sparsevec(Any[1, 3], [1, 1], 5),
    sparsevec([1, 3], [1, 1], 5))

# from dictionary

function my_intmap(x)
    a = Dict{Int,eltype(x)}()
    for i in SparseArrays.nonzeroinds(x)
        a[i] = x[i]
    end
    return a
end

let x = spv_x1
    a = my_intmap(x)
    xc = sparsevec(a, 8)
    @test exact_equal(x, xc)

    xc = sparsevec(a)
    @test exact_equal(xc, SparseVector(6, [2, 5, 6], [1.25, -0.75, 3.5]))

    d = Dict{Int, Float64}((1 => 0.0, 2 => 1.0, 3 => 2.0))
    @test exact_equal(sparsevec(d), SparseVector(3, [1, 2, 3], [0.0, 1.0, 2.0]))
end

# spones - copies structure, but replaces nzvals with ones
let x = SparseVector(8, [2, 3, 6], [12.0, 18.0, 25.0])
    y = spones(x)
    @test (x .!= 0) == (y .!= 0)
    @test y == SparseVector(8, [2, 3, 6], [1.0, 1.0, 1.0])
end

# sprand & sprandn

let xr = sprand(1000, 0.9)
    @test isa(xr, SparseVector{Float64,Int})
    @test length(xr) == 1000
    @test all(nonzeros(xr) .>= 0.0)
end

let xr = sprand(Float32, 1000, 0.9)
    @test isa(xr, SparseVector{Float32,Int})
    @test length(xr) == 1000
    @test all(nonzeros(xr) .>= 0.0)
end

let xr = sprandn(1000, 0.9)
    @test isa(xr, SparseVector{Float64,Int})
    @test length(xr) == 1000
    if !isempty(nonzeros(xr))
        @test any(nonzeros(xr) .> 0.0) && any(nonzeros(xr) .< 0.0)
    end
end

let xr = sprand(Bool, 1000, 0.9)
    @test isa(xr, SparseVector{Bool,Int})
    @test length(xr) == 1000
    @test all(nonzeros(xr))
end

let r1 = MersenneTwister(0), r2 = MersenneTwister(0)
    @test sprand(r1, 100, .9) == sprand(r2, 100, .9)
    @test sprandn(r1, 100, .9) == sprandn(r2, 100, .9)
    @test sprand(r1, Bool, 100, .9) == sprand(r2,  Bool, 100, .9)
end

### Element access

# getindex

# single integer index
for (x, xf) in [(spv_x1, x1_full)]
    for i = 1:length(x)
        @test x[i] == xf[i]
    end
end

# generic array index
let x = sprand(100, 0.5)
    I = rand(1:length(x), 20)
    r = x[I]
    @test isa(r, SparseVector{Float64,Int})
    @test all(nonzeros(r) .!= 0.0)
    @test Array(r) == Array(x)[I]
end

# boolean array index
let x = sprand(10, 10, 0.5)
    I = rand(1:size(x, 2), 10)
    bI = falses(size(x, 2))
    bI[I] = true
    r = x[1,bI]
    @test isa(r, SparseVector{Float64,Int})
    @test all(nonzeros(r) .!= 0.0)
    @test Array(r) == Array(x)[1,bI]
end

let x = sprand(10, 0.5)
    I = rand(1:length(x), 5)
    bI = falses(length(x))
    bI[I] = true
    r = x[bI]
    @test isa(r, SparseVector{Float64,Int})
    @test all(nonzeros(r) .!= 0.0)
    @test Array(r) == Array(x)[bI]
end

# setindex

let xc = spzeros(Float64, 8)
    xc[3] = 2.0
    @test exact_equal(xc, SparseVector(8, [3], [2.0]))
end

let xc = copy(spv_x1)
    xc[5] = 2.0
    @test exact_equal(xc, SparseVector(8, [2, 5, 6], [1.25, 2.0, 3.5]))
end

let xc = copy(spv_x1)
    xc[3] = 4.0
    @test exact_equal(xc, SparseVector(8, [2, 3, 5, 6], [1.25, 4.0, -0.75, 3.5]))

    xc[1] = 6.0
    @test exact_equal(xc, SparseVector(8, [1, 2, 3, 5, 6], [6.0, 1.25, 4.0, -0.75, 3.5]))

    xc[8] = -1.5
    @test exact_equal(xc, SparseVector(8, [1, 2, 3, 5, 6, 8], [6.0, 1.25, 4.0, -0.75, 3.5, -1.5]))
end

let xc = copy(spv_x1)
    xc[5] = 0.0
    @test exact_equal(xc, SparseVector(8, [2, 5, 6], [1.25, 0.0, 3.5]))

    xc[6] = 0.0
    @test exact_equal(xc, SparseVector(8, [2, 5, 6], [1.25, 0.0, 0.0]))

    xc[2] = 0.0
    @test exact_equal(xc, SparseVector(8, [2, 5, 6], [0.0, 0.0, 0.0]))

    xc[1] = 0.0
    @test exact_equal(xc, SparseVector(8, [2, 5, 6], [0.0, 0.0, 0.0]))
end

## dropstored! tests
let x = SparseVector(10, [2, 7, 9], [2.0, 7.0, 9.0])
    # Test argument bounds checking for dropstored!(x, i)
    @test_throws BoundsError Base.SparseArrays.dropstored!(x, 0)
    @test_throws BoundsError Base.SparseArrays.dropstored!(x, 11)
    # Test behavior of dropstored!(x, i)
    # --> Test dropping a single stored entry
    @test Base.SparseArrays.dropstored!(x, 2) == SparseVector(10, [7, 9], [7.0, 9.0])
    # --> Test dropping a single nonstored entry
    @test Base.SparseArrays.dropstored!(x, 5) == SparseVector(10, [7, 9], [7.0, 9.0])
end

# find and findnz tests
@test find(!iszero, spv_x1) == find(!iszero, x1_full)
@test find(spv_x1 .> 1) == find(x1_full .> 1)
@test find(x->x>1, spv_x1) == find(x->x>1, x1_full)
@test findnz(spv_x1) == (find(!iszero, x1_full), filter(x->x!=0, x1_full))
let xc = SparseVector(8, [2, 3, 5], [1.25, 0, -0.75]), fc = Array(xc)
    @test find(!iszero, xc) == find(!iszero, fc)
    @test findnz(xc) == ([2, 5], [1.25, -0.75])
end

### Array manipulation

# copy

let x = spv_x1
    xc = copy(x)
    @test isa(xc, SparseVector{Float64,Int})
    @test x.nzind !== xc.nzval
    @test x.nzval !== xc.nzval
    @test exact_equal(x, xc)
end

let a = SparseVector(8, [2, 5, 6], Int32[12, 35, 72])
    # vec
    @test vec(a) == a

    # reinterpret
    au = reinterpret(UInt32, a)
    @test isa(au, SparseVector{UInt32,Int})
    @test exact_equal(au, SparseVector(8, [2, 5, 6], UInt32[12, 35, 72]))

    # float
    af = float(a)
    @test float(af) == af
    @test isa(af, SparseVector{Float64,Int})
    @test exact_equal(af, SparseVector(8, [2, 5, 6], [12., 35., 72.]))
    @test sparsevec(transpose(transpose(af))) == af

    # complex
    acp = complex(af)
    @test complex(acp) == acp
    @test isa(acp, SparseVector{Complex128,Int})
    @test exact_equal(acp, SparseVector(8, [2, 5, 6], complex([12., 35., 72.])))
    @test sparsevec(adjoint(adjoint(acp))) == acp
end

let x1 = SparseVector(8, [2, 5, 6], [12.2, 1.4, 5.0])
    x2 = SparseVector(8, [3, 4], [1.2, 3.4])
    copy!(x2, x1)
    @test x2 == x1
    x2 = SparseVector(8, [2, 4, 8], [10.3, 7.4, 3.1])
    copy!(x2, x1)
    @test x2 == x1
    x2 = SparseVector(8, [1, 3, 4, 7], [0.3, 1.2, 3.4, 0.1])
    copy!(x2, x1)
    @test x2 == x1
    x2 = SparseVector(10, [3, 4], [1.2, 3.4])
    copy!(x2, x1)
    @test x2[1:8] == x1
    @test x2[9:10] == spzeros(2)
    x2 = SparseVector(10, [3, 4, 9], [1.2, 3.4, 17.8])
    copy!(x2, x1)
    @test x2[1:8] == x1
    @test x2[9] == 17.8
    @test x2[10] == 0
    x2 = SparseVector(10, [3, 4, 5, 6, 9], [8.3, 7.2, 1.2, 3.4, 17.8])
    copy!(x2, x1)
    @test x2[1:8] == x1
    @test x2[9] == 17.8
    @test x2[10] == 0
    x2 = SparseVector(6, [3, 4], [1.2, 3.4])
    @test_throws BoundsError copy!(x2, x1)
end

let x1 = sparse([2, 1, 2], [1, 3, 3], [12.2, 1.4, 5.0], 2, 4)
    x2 = SparseVector(8, [3, 4], [1.2, 3.4])
    copy!(x2, x1)
    @test x2[:] == x1[:]
    x2 = SparseVector(8, [2, 4, 8], [10.3, 7.4, 3.1])
    copy!(x2, x1)
    @test x2[:] == x1[:]
    x2 = SparseVector(8, [1, 3, 4, 7], [0.3, 1.2, 3.4, 0.1])
    copy!(x2, x1)
    @test x2[:] == x1[:]
    x2 = SparseVector(10, [3, 4], [1.2, 3.4])
    copy!(x2, x1)
    @test x2[1:8] == x1[:]
    @test x2[9:10] == spzeros(2)
    x2 = SparseVector(10, [3, 4, 9], [1.2, 3.4, 17.8])
    copy!(x2, x1)
    @test x2[1:8] == x1[:]
    @test x2[9] == 17.8
    @test x2[10] == 0
    x2 = SparseVector(10, [3, 4, 5, 6, 9], [8.3, 7.2, 1.2, 3.4, 17.8])
    copy!(x2, x1)
    @test x2[1:8] == x1[:]
    @test x2[9] == 17.8
    @test x2[10] == 0
    x2 = SparseVector(6, [3, 4], [1.2, 3.4])
    @test_throws BoundsError copy!(x2, x1)
end

let x1 = SparseVector(8, [2, 5, 6], [12.2, 1.4, 5.0])
    x2 = sparse([1, 2], [2, 2], [1.2, 3.4], 2, 4)
    copy!(x2, x1)
    @test x2[:] == x1[:]
    x2 = sparse([2, 2, 2], [1, 3, 4], [10.3, 7.4, 3.1], 2, 4)
    copy!(x2, x1)
    @test x2[:] == x1[:]
    x2 = sparse([1, 1, 2, 1], [1, 2, 2, 4], [0.3, 1.2, 3.4, 0.1], 2, 4)
    copy!(x2, x1)
    @test x2[:] == x1[:]
    x2 = sparse([1, 2], [2, 2], [1.2, 3.4], 2, 5)
    copy!(x2, x1)
    @test x2[1:8] == x1
    @test x2[9:10] == spzeros(2)
    x2 = sparse([1, 2, 1], [2, 2, 5], [1.2, 3.4, 17.8], 2, 5)
    copy!(x2, x1)
    @test x2[1:8] == x1
    @test x2[9] == 17.8
    @test x2[10] == 0
    x2 = sparse([1, 2, 1, 2, 1], [2, 2, 3, 3, 5], [8.3, 7.2, 1.2, 3.4, 17.8], 2, 5)
    copy!(x2, x1)
    @test x2[1:8] == x1
    @test x2[9] == 17.8
    @test x2[10] == 0
    x2 = sparse([1, 2], [2, 2], [1.2, 3.4], 2, 3)
    @test_throws BoundsError copy!(x2, x1)
end

### Type conversion

let x = convert(SparseVector, sparse([2, 5, 6], [1, 1, 1], [1.25, -0.75, 3.5], 8, 1))
    @test isa(x, SparseVector{Float64,Int})
    @test exact_equal(x, spv_x1)
end

let x = spv_x1, xf = x1_full
    xc = convert(SparseVector, xf)
    @test isa(xc, SparseVector{Float64,Int})
    @test exact_equal(xc, x)

    xc = convert(SparseVector{Float32,Int}, x)
    xf32 = SparseVector(8, [2, 5, 6], [1.25f0, -0.75f0, 3.5f0])
    @test isa(xc, SparseVector{Float32,Int})
    @test exact_equal(xc, xf32)

    xc = convert(SparseVector{Float32}, x)
    @test isa(xc, SparseVector{Float32,Int})
    @test exact_equal(xc, xf32)

    xm = convert(SparseMatrixCSC, x)
    @test isa(xm, SparseMatrixCSC{Float64,Int})
    @test Array(xm) == reshape(xf, 8, 1)

    xm = convert(SparseMatrixCSC{Float32}, x)
    @test isa(xm, SparseMatrixCSC{Float32,Int})
    @test Array(xm) == reshape(convert(Vector{Float32}, xf), 8, 1)
end


### Concatenation

let m = 80, n = 100
    A = Array{SparseVector{Float64,Int}}(n)
    tnnz = 0
    for i = 1:length(A)
        A[i] = sprand(m, 0.3)
        tnnz += nnz(A[i])
    end

    H = hcat(A...)
    @test isa(H, SparseMatrixCSC{Float64,Int})
    @test size(H) == (m, n)
    @test nnz(H) == tnnz
    Hr = zeros(m, n)
    for j = 1:n
        Hr[:,j] = Array(A[j])
    end
    @test Array(H) == Hr

    V = vcat(A...)
    @test isa(V, SparseVector{Float64,Int})
    @test length(V) == m * n
    Vr = vec(Hr)
    @test Array(V) == Vr
end

# Test that concatenations of combinations of sparse vectors with various other
# matrix/vector types yield sparse arrays
let N = 4
    spvec = spzeros(N)
    spmat = spzeros(N, 1)
    densevec = ones(N)
    densemat = ones(N, 1)
    diagmat = Diagonal(ones(4))
    # Test that concatenations of pairwise combinations of sparse vectors with dense
    # vectors/matrices, sparse matrices, or special matrices yield sparse arrays
    for othervecormat in (densevec, densemat, spmat)
        @test issparse(vcat(spvec, othervecormat))
        @test issparse(vcat(othervecormat, spvec))
    end
    for othervecormat in (densevec, densemat, spmat, diagmat)
        @test issparse(hcat(spvec, othervecormat))
        @test issparse(hcat(othervecormat, spvec))
        @test issparse(hvcat((2,), spvec, othervecormat))
        @test issparse(hvcat((2,), othervecormat, spvec))
        @test issparse(cat((1,2), spvec, othervecormat))
        @test issparse(cat((1,2), othervecormat, spvec))
    end
    # The preceding tests should cover multi-way combinations of those types, but for good
    # measure test a few multi-way combinations involving those types
    @test issparse(vcat(spvec, densevec, spmat, densemat))
    @test issparse(vcat(densevec, spvec, densemat, spmat))
    @test issparse(hcat(spvec, densemat, spmat, densevec, diagmat))
    @test issparse(hcat(densemat, spmat, spvec, densevec, diagmat))
    @test issparse(hvcat((5,), diagmat, densevec, spvec, densemat, spmat))
    @test issparse(hvcat((5,), spvec, densemat, diagmat, densevec, spmat))
    @test issparse(cat((1,2), densemat, diagmat, spmat, densevec, spvec))
    @test issparse(cat((1,2), spvec, diagmat, densevec, spmat, densemat))
end
@testset "vertical concatenation of SparseVectors with different el- and ind-type (#22225)" begin
    spv6464 = SparseVector(0, Int64[], Int64[])
    @test isa(vcat(spv6464, SparseVector(0, Int64[], Int32[])), SparseVector{Int64,Int64})
    @test isa(vcat(spv6464, SparseVector(0, Int32[], Int64[])), SparseVector{Int64,Int64})
    @test isa(vcat(spv6464, SparseVector(0, Int32[], Int32[])), SparseVector{Int64,Int64})
end

## sparsemat: combinations with sparse matrix

let S = sprand(4, 8, 0.5)
    Sf = Array(S)
    @assert isa(Sf, Matrix{Float64})

    # get a single column
    for j = 1:size(S,2)
        col = S[:, j]
        @test isa(col, SparseVector{Float64,Int})
        @test length(col) == size(S,1)
        @test Array(col) == Sf[:,j]
    end

    # Get a reshaped vector
    v = S[:]
    @test isa(v, SparseVector{Float64,Int})
    @test length(v) == length(S)
    @test Array(v) == Sf[:]

    # Get a linear subset
    for i=0:length(S)
        v = S[1:i]
        @test isa(v, SparseVector{Float64,Int})
        @test length(v) == i
        @test Array(v) == Sf[1:i]
    end
    for i=1:length(S)+1
        v = S[i:end]
        @test isa(v, SparseVector{Float64,Int})
        @test length(v) == length(S) - i + 1
        @test Array(v) == Sf[i:end]
    end
    for i=0:div(length(S),2)
        v = S[1+i:end-i]
        @test isa(v, SparseVector{Float64,Int})
        @test length(v) == length(S) - 2i
        @test Array(v) == Sf[1+i:end-i]
    end
end

let r = [1,10], S = sparse(r, r, r)
    Sf = Array(S)
    @assert isa(Sf, Matrix{Int})

    inds = [1,1,1,1,1,1]
    v = S[inds]
    @test isa(v, SparseVector{Int,Int})
    @test length(v) == length(inds)
    @test Array(v) == Sf[inds]

    inds = [2,2,2,2,2,2]
    v = S[inds]
    @test isa(v, SparseVector{Int,Int})
    @test length(v) == length(inds)
    @test Array(v) == Sf[inds]

    # get a single column
    for j = 1:size(S,2)
        col = S[:, j]
        @test isa(col, SparseVector{Int,Int})
        @test length(col) == size(S,1)
        @test Array(col) == Sf[:,j]
    end

    # Get a reshaped vector
    v = S[:]
    @test isa(v, SparseVector{Int,Int})
    @test length(v) == length(S)
    @test Array(v) == Sf[:]

    # Get a linear subset
    for i=0:length(S)
        v = S[1:i]
        @test isa(v, SparseVector{Int,Int})
        @test length(v) == i
        @test Array(v) == Sf[1:i]
    end
    for i=1:length(S)+1
        v = S[i:end]
        @test isa(v, SparseVector{Int,Int})
        @test length(v) == length(S) - i + 1
        @test Array(v) == Sf[i:end]
    end
    for i=0:div(length(S),2)
        v = S[1+i:end-i]
        @test isa(v, SparseVector{Int,Int})
        @test length(v) == length(S) - 2i
        @test Array(v) == Sf[1+i:end-i]
    end
end

## math

### Data

rnd_x0 = sprand(50, 0.6)
rnd_x0f = Array(rnd_x0)

rnd_x1 = sprand(50, 0.7) * 4.0
rnd_x1f = Array(rnd_x1)

spv_x1 = SparseVector(8, [2, 5, 6], [1.25, -0.75, 3.5])
spv_x2 = SparseVector(8, [1, 2, 6, 7], [3.25, 4.0, -5.5, -6.0])

### Arithmetic operations

let x = spv_x1, x2 = spv_x2
    # negate
    @test exact_equal(-x, SparseVector(8, [2, 5, 6], [-1.25, 0.75, -3.5]))

    # abs and abs2
    @test exact_equal(abs.(x), SparseVector(8, [2, 5, 6], abs.([1.25, -0.75, 3.5])))
    @test exact_equal(abs2.(x), SparseVector(8, [2, 5, 6], abs2.([1.25, -0.75, 3.5])))

    # plus and minus
    xa = SparseVector(8, [1,2,5,6,7], [3.25,5.25,-0.75,-2.0,-6.0])

    @test exact_equal(x + x, x * 2)
    @test exact_equal(x + x2, xa)
    @test exact_equal(x2 + x, xa)

    xb = SparseVector(8, [1,2,5,6,7], [-3.25,-2.75,-0.75,9.0,6.0])
    @test exact_equal(x - x, SparseVector(8, Int[], Float64[]))
    @test exact_equal(x - x2, xb)
    @test exact_equal(x2 - x, -xb)

    @test Array(x) + x2 == Array(xa)
    @test Array(x) - x2 == Array(xb)
    @test x + Array(x2) == Array(xa)
    @test x - Array(x2) == Array(xb)

    # multiplies
    xm = SparseVector(8, [2, 6], [5.0, -19.25])
    @test exact_equal(x .* x, abs2.(x))
    @test exact_equal(x .* x2, xm)
    @test exact_equal(x2 .* x, xm)

    @test Array(x) .* x2 == Array(xm)
    @test x .* Array(x2) == Array(xm)

    # max & min
    @test exact_equal(max.(x, x), x)
    @test exact_equal(min.(x, x), x)
    @test exact_equal(max.(x, x2),
        SparseVector(8, Int[1, 2, 6], Float64[3.25, 4.0, 3.5]))
    @test exact_equal(min.(x, x2),
        SparseVector(8, Int[2, 5, 6, 7], Float64[1.25, -0.75, -5.5, -6.0]))
end

### Complex

let x = spv_x1, x2 = spv_x2
    # complex
    @test exact_equal(complex.(x, x),
        SparseVector(8, [2,5,6], [1.25+1.25im, -0.75-0.75im, 3.5+3.5im]))
    @test exact_equal(complex.(x, x2),
        SparseVector(8, [1,2,5,6,7], [3.25im, 1.25+4.0im, -0.75+0.0im, 3.5-5.5im, -6.0im]))
    @test exact_equal(complex.(x2, x),
        SparseVector(8, [1,2,5,6,7], [3.25+0.0im, 4.0+1.25im, -0.75im, -5.5+3.5im, -6.0+0.0im]))

    # real, imag and conj

    @test real(x) === x
    @test exact_equal(imag(x), spzeros(Float64, length(x)))
    @test conj(x) === x

    xcp = complex.(x, x2)
    @test exact_equal(real(xcp), x)
    @test exact_equal(imag(xcp), x2)
    @test exact_equal(conj(xcp), complex.(x, -x2))
end

### Zero-preserving math functions: sparse -> sparse

x1operations = (floor, ceil, trunc, round)
x0operations = (log1p,  expm1,  sinpi,
                sin,    tan,    sind,   tand,
                asin,   atan,   asind,  atand,
                sinh,   tanh,   asinh,  atanh)

for (spvec, densevec, operations) in (
        (rnd_x0, rnd_x0f, x0operations),
        (rnd_x1, rnd_x1f, x1operations) )
    for op in operations
        spresvec = op.(spvec)
        @test spresvec == op.(densevec)
        @test all(!iszero, spresvec.nzval)
        resvaltype = typeof(op(zero(eltype(spvec))))
        resindtype = Base.SparseArrays.indtype(spvec)
        @test isa(spresvec, SparseVector{resvaltype,resindtype})
    end
end

### Non-zero-preserving math functions: sparse -> dense

for op in (exp, exp2, exp10, log, log2, log10,
        cos, cosd, acos, cosh, cospi,
        csc, cscd, acot, csch, acsch,
        cot, cotd, acosd, coth,
        sec, secd, acotd, sech, asech)
    spvec = rnd_x0
    densevec = rnd_x0f
    spresvec = op.(spvec)
    @test spresvec == op.(densevec)
    resvaltype = typeof(op(zero(eltype(spvec))))
    resindtype = Base.SparseArrays.indtype(spvec)
    @test isa(spresvec, SparseVector{resvaltype,resindtype})
end


### Reduction

# sum, vecnorm

let x = spv_x1
    @test sum(x) == 4.0
    @test sum(abs, x) == 5.5
    @test sum(abs2, x) == 14.375

    @test vecnorm(x) == sqrt(14.375)
    @test vecnorm(x, 1) == 5.5
    @test vecnorm(x, 2) == sqrt(14.375)
    @test vecnorm(x, Inf) == 3.5
end

# maximum, minimum

let x = spv_x1
    @test maximum(x) == 3.5
    @test minimum(x) == -0.75
    @test maximum(abs, x) == 3.5
    @test minimum(abs, x) == 0.0
end

let x = abs.(spv_x1)
    @test maximum(x) == 3.5
    @test minimum(x) == 0.0
end

let x = -abs.(spv_x1)
    @test maximum(x) == 0.0
    @test minimum(x) == -3.5
end

let x = SparseVector(3, [1, 2, 3], [-4.5, 2.5, 3.5])
    @test maximum(x) == 3.5
    @test minimum(x) == -4.5
    @test maximum(abs, x) == 4.5
    @test minimum(abs, x) == 2.5
end

let x = spzeros(Float64, 8)
    @test maximum(x) == 0.0
    @test minimum(x) == 0.0
    @test maximum(abs, x) == 0.0
    @test minimum(abs, x) == 0.0
end


### linalg

### BLAS Level-1

let x = sprand(16, 0.5), x2 = sprand(16, 0.4)
    xf = Array(x)
    xf2 = Array(x2)

    # axpy!
    for c in [1.0, -1.0, 2.0, -2.0]
        y = Array(x)
        @test Base.axpy!(c, x2, y) === y
        @test y == Array(x2 * c + x)
    end

    # scale
    let α = 2.5, sx = SparseVector(x.n, x.nzind, x.nzval * α)
        @test exact_equal(x * α, sx)
        @test exact_equal(x * (α + 0.0*im), complex(sx))
        @test exact_equal(α * x, sx)
        @test exact_equal((α + 0.0*im) * x, complex(sx))
        @test exact_equal(x * α, sx)
        @test exact_equal(α * x, sx)
        @test exact_equal(x .* α, sx)
        @test exact_equal(α .* x, sx)
        @test exact_equal(x / α, SparseVector(x.n, x.nzind, x.nzval / α))

        xc = copy(x)
        @test scale!(xc, α) === xc
        @test exact_equal(xc, sx)
        xc = copy(x)
        @test scale!(α, xc) === xc
        @test exact_equal(xc, sx)
        xc = copy(x)
        @test scale!(xc, complex(α, 0.0)) === xc
        @test exact_equal(xc, sx)
        xc = copy(x)
        @test scale!(complex(α, 0.0), xc) === xc
        @test exact_equal(xc, sx)
    end

    # dot
    let dv = dot(xf, xf2)
        @test dot(x, x) == sum(abs2, x)
        @test dot(x2, x2) == sum(abs2, x2)
        @test dot(x, x2) ≈ dv
        @test dot(x2, x) ≈ dv
        @test dot(Array(x), x2) ≈ dv
        @test dot(x, Array(x2)) ≈ dv
    end
end

let x = complex.(sprand(32, 0.6), sprand(32, 0.6)),
    y = complex.(sprand(32, 0.6), sprand(32, 0.6))
    xf = Array(x)::Vector{Complex128}
    yf = Array(y)::Vector{Complex128}
    @test dot(x, x) ≈ dot(xf, xf)
    @test dot(x, y) ≈ dot(xf, yf)
end


### BLAS Level-2:

## dense A * sparse x -> dense y

let A = randn(9, 16), x = sprand(16, 0.7)
    xf = Array(x)
    for α in [0.0, 1.0, 2.0], β in [0.0, 0.5, 1.0]
        y = rand(9)
        rr = α*A*xf + β*y
        @test A_mul_B!(α, A, x, β, y) === y
        @test y ≈ rr
    end
    y = A*x
    @test isa(y, Vector{Float64})
    @test A*x ≈ A*xf
end

let A = randn(16, 9), x = sprand(16, 0.7)
    xf = Array(x)
    for α in [0.0, 1.0, 2.0], β in [0.0, 0.5, 1.0]
        y = rand(9)
        rr = α*A'xf + β*y
        @test At_mul_B!(α, A, x, β, y) === y
        @test y ≈ rr
    end
    y = At_mul_B(A, x)
    @test isa(y, Vector{Float64})
    @test y ≈ At_mul_B(A, xf)
end

## sparse A * sparse x -> dense y

let A = sprandn(9, 16, 0.5), x = sprand(16, 0.7)
    Af = Array(A)
    xf = Array(x)
    for α in [0.0, 1.0, 2.0], β in [0.0, 0.5, 1.0]
        y = rand(9)
        rr = α*Af*xf + β*y
        @test A_mul_B!(α, A, x, β, y) === y
        @test y ≈ rr
    end
    y = SparseArrays.densemv(A, x)
    @test isa(y, Vector{Float64})
    @test y ≈ Af*xf
end

let A = sprandn(16, 9, 0.5), x = sprand(16, 0.7)
    Af = Array(A)
    xf = Array(x)
    for α in [0.0, 1.0, 2.0], β in [0.0, 0.5, 1.0]
        y = rand(9)
        rr = α*Af'xf + β*y
        @test At_mul_B!(α, A, x, β, y) === y
        @test y ≈ rr
    end
    y = SparseArrays.densemv(A, x; trans='T')
    @test isa(y, Vector{Float64})
    @test y ≈ At_mul_B(Af, xf)
end

let A = complex.(sprandn(7, 8, 0.5), sprandn(7, 8, 0.5)),
    x = complex.(sprandn(8, 0.6), sprandn(8, 0.6)),
    x2 = complex.(sprandn(7, 0.75), sprandn(7, 0.75))
    Af = Array(A)
    xf = Array(x)
    x2f = Array(x2)
    @test SparseArrays.densemv(A, x; trans='N') ≈ Af * xf
    @test SparseArrays.densemv(A, x2; trans='T') ≈ Af.' * x2f
    @test SparseArrays.densemv(A, x2; trans='C') ≈ Af'x2f
    @test_throws ArgumentError SparseArrays.densemv(A, x; trans='D')
end

## sparse A * sparse x -> sparse y

let A = sprandn(9, 16, 0.5), x = sprand(16, 0.7), x2 = sprand(9, 0.7)
    Af = Array(A)
    xf = Array(x)
    x2f = Array(x2)

    y = A*x
    @test isa(y, SparseVector{Float64,Int})
    @test all(nonzeros(y) .!= 0.0)
    @test Array(y) ≈ Af * xf

    y = At_mul_B(A, x2)
    @test isa(y, SparseVector{Float64,Int})
    @test all(nonzeros(y) .!= 0.0)
    @test Array(y) ≈ Af'x2f
end

let A = complex.(sprandn(7, 8, 0.5), sprandn(7, 8, 0.5)),
    x = complex.(sprandn(8, 0.6), sprandn(8, 0.6)),
    x2 = complex.(sprandn(7, 0.75), sprandn(7, 0.75))
    Af = Array(A)
    xf = Array(x)
    x2f = Array(x2)

    y = A*x
    @test isa(y, SparseVector{Complex128,Int})
    @test Array(y) ≈ Af * xf

    y = At_mul_B(A, x2)
    @test isa(y, SparseVector{Complex128,Int})
    @test Array(y) ≈ Af.' * x2f

    y = Ac_mul_B(A, x2)
    @test isa(y, SparseVector{Complex128,Int})
    @test Array(y) ≈ Af'x2f
end

# left-division operations involving triangular matrices and sparse vectors (#14005)
let m = 10
    sparsefloatvecs = SparseVector[sprand(m, 0.4) for k in 1:3]
    sparseintvecs = SparseVector[SparseVector(m, sprvec.nzind, round.(Int, sprvec.nzval*10)) for sprvec in sparsefloatvecs]
    sparsecomplexvecs = SparseVector[SparseVector(m, sprvec.nzind, complex.(sprvec.nzval, sprvec.nzval)) for sprvec in sparsefloatvecs]

    sprmat = sprand(m, m, 0.2)
    sparsefloatmat = speye(m) + sprmat/(2m)
    sparsecomplexmat = speye(m) + SparseMatrixCSC(m, m, sprmat.colptr, sprmat.rowval, complex.(sprmat.nzval, sprmat.nzval)/(4m))
    sparseintmat = speye(Int, m)*10m + SparseMatrixCSC(m, m, sprmat.colptr, sprmat.rowval, round.(Int, sprmat.nzval*10))

    denseintmat = eye(Int, m)*10m + rand(1:m, m, m)
    densefloatmat = eye(m) + randn(m, m)/(2m)
    densecomplexmat = eye(m) + complex.(randn(m, m), randn(m, m))/(4m)

    inttypes = (Int32, Int64, BigInt)
    floattypes = (Float32, Float64, BigFloat)
    complextypes = (Complex{Float32}, Complex{Float64})
    eltypes = (inttypes..., floattypes..., complextypes...)

    for eltypemat in eltypes
        (densemat, sparsemat) = eltypemat in inttypes ? (denseintmat, sparseintmat) :
                                eltypemat in floattypes ? (densefloatmat, sparsefloatmat) :
                                eltypemat in complextypes && (densecomplexmat, sparsecomplexmat)
        densemat = convert(Matrix{eltypemat}, densemat)
        sparsemat = convert(SparseMatrixCSC{eltypemat}, sparsemat)
        trimats = (LowerTriangular(densemat), UpperTriangular(densemat),
                   LowerTriangular(sparsemat), UpperTriangular(sparsemat) )
        unittrimats = (Base.LinAlg.UnitLowerTriangular(densemat), Base.LinAlg.UnitUpperTriangular(densemat),
                       Base.LinAlg.UnitLowerTriangular(sparsemat), Base.LinAlg.UnitUpperTriangular(sparsemat) )

        for eltypevec in eltypes
            spvecs = eltypevec in inttypes ? sparseintvecs :
                     eltypevec in floattypes ? sparsefloatvecs :
                     eltypevec in complextypes && sparsecomplexvecs
            spvecs = SparseVector[SparseVector(m, spvec.nzind, convert(Vector{eltypevec}, spvec.nzval)) for spvec in spvecs]

            for spvec in spvecs
                fspvec = convert(Array, spvec)
                # test out-of-place left-division methods
                for mat in (trimats..., unittrimats...), func in (\, At_ldiv_B, Ac_ldiv_B)
                    @test func(mat, spvec) ≈ func(mat, fspvec)
                end
                # test in-place left-division methods not involving quotients
                if eltypevec == typeof(zero(eltypemat)*zero(eltypevec) + zero(eltypemat)*zero(eltypevec))
                    for mat in unittrimats, func in (A_ldiv_B!, Base.LinAlg.At_ldiv_B!, Base.LinAlg.Ac_ldiv_B!)
                        @test func(mat, copy(spvec)) ≈ func(mat, copy(fspvec))
                    end
                end
                # test in-place left-division methods involving quotients
                if eltypevec == typeof((zero(eltypemat)*zero(eltypevec) + zero(eltypemat)*zero(eltypevec))/one(eltypemat))
                    for mat in trimats, func in (A_ldiv_B!, Base.LinAlg.At_ldiv_B!, Base.LinAlg.Ac_ldiv_B!)
                        @test func(mat, copy(spvec)) ≈ func(mat, copy(fspvec))
                    end
                end
            end
        end
    end
end
# The preceding tests miss the edge case where the sparse vector is empty (#16716)
let origmat = [-1.5 -0.7; 0.0 1.0]
    transmat = transpose(origmat)
    utmat = UpperTriangular(origmat)
    ltmat = LowerTriangular(transmat)
    uutmat = Base.LinAlg.UnitUpperTriangular(origmat)
    ultmat = Base.LinAlg.UnitLowerTriangular(transmat)

    zerospvec = spzeros(Float64, 2)
    zerodvec = zeros(Float64, 2)

    for mat in (utmat, ltmat, uutmat, ultmat)
        for func in (\, At_ldiv_B, Ac_ldiv_B)
            @test isequal((func)(mat, zerospvec), zerodvec)
        end
        for ipfunc in (A_ldiv_B!, Base.LinAlg.At_ldiv_B!, Base.LinAlg.Ac_ldiv_B!)
            @test isequal((ipfunc)(mat, copy(zerospvec)), zerospvec)
        end
    end
end

# kron
let testdims = ((5,10), (20,12), (25,30))
    for (m,n) in testdims
        x = sprand(m, 0.4)
        y = sprand(n, 0.3)
        @test Vector(kron(x,y)) == kron(Vector(x), Vector(y))
        @test Vector(kron(Vector(x),y)) == kron(Vector(x), Vector(y))
        @test Vector(kron(x,Vector(y))) == kron(Vector(x), Vector(y))
    end
end

# fkeep!
let x = sparsevec(1:7, [3., 2., -1., 1., -2., -3., 3.], 7)
    # droptol
    xdrop = Base.droptol!(copy(x), 1.5)
    @test exact_equal(xdrop, SparseVector(7, [1, 2, 5, 6, 7], [3., 2., -2., -3., 3.]))
    Base.droptol!(xdrop, 2.5)
    @test exact_equal(xdrop, SparseVector(7, [1, 6, 7], [3., -3., 3.]))
    Base.droptol!(xdrop, 3.)
    @test exact_equal(xdrop, SparseVector(7, Int[], Float64[]))

    xdrop = copy(x)
    # This will keep index 1, 3, 4, 7 in xdrop
    f_drop(i, x) = (abs(x) == 1.) || (i in [1, 7])
    Base.SparseArrays.fkeep!(xdrop, f_drop)
    @test exact_equal(xdrop, SparseVector(7, [1, 3, 4, 7], [3., -1., 1., 3.]))
end

# dropzeros[!]
let testdims = (10, 20, 30), nzprob = 0.4, targetnumposzeros = 5, targetnumnegzeros = 5
    for m in testdims
        v = sprand(m, nzprob)
        struczerosv = find(x -> x == 0, v)
        poszerosinds = unique(rand(struczerosv, targetnumposzeros))
        negzerosinds = unique(rand(struczerosv, targetnumnegzeros))
        vposzeros = setindex!(copy(v), 2, poszerosinds)
        vnegzeros = setindex!(copy(v), -2, negzerosinds)
        vbothsigns = setindex!(copy(vposzeros), -2, negzerosinds)
        map!(x -> x == 2 ? 0.0 : x, vposzeros.nzval, vposzeros.nzval)
        map!(x -> x == -2 ? -0.0 : x, vnegzeros.nzval, vnegzeros.nzval)
        map!(x -> x == 2 ? 0.0 : x == -2 ? -0.0 : x, vbothsigns.nzval, vbothsigns.nzval)
        for vwithzeros in (vposzeros, vnegzeros, vbothsigns)
            # Basic functionality / dropzeros!
            @test dropzeros!(copy(vwithzeros)) == v
            @test dropzeros!(copy(vwithzeros), false) == v
            # Basic functionality / dropzeros
            @test dropzeros(vwithzeros) == v
            @test dropzeros(vwithzeros, false) == v
            # Check trimming works as expected
            @test length(dropzeros!(copy(vwithzeros)).nzval) == length(v.nzval)
            @test length(dropzeros!(copy(vwithzeros)).nzind) == length(v.nzind)
            @test length(dropzeros!(copy(vwithzeros), false).nzval) == length(vwithzeros.nzval)
            @test length(dropzeros!(copy(vwithzeros), false).nzind) == length(vwithzeros.nzind)
        end
    end
    # original dropzeros! test
    xdrop = sparsevec(1:7, [3., 2., -1., 1., -2., -3., 3.], 7)
    xdrop.nzval[[2, 4, 6]] = 0.0
    Base.SparseArrays.dropzeros!(xdrop)
    @test exact_equal(xdrop, SparseVector(7, [1, 3, 5, 7], [3, -1., -2., 3.]))
end

# It's tempting to share data between a SparseVector and a SparseMatrix,
# but if that's done, then modifications to one or the other will cause
# an inconsistent state:
sv = sparse(1:10)
sm = convert(SparseMatrixCSC, sv)
sv[1] = 0
@test Array(sm)[2:end] == collect(2:10)

# Ensure that sparsevec with all-zero values returns an array of zeros
@test sparsevec([1,2,3],[0,0,0]) == [0,0,0]

# Compare stored zero semantics between SparseVector and SparseMatrixCSC
let S = SparseMatrixCSC(10,1,[1,6],[1,3,5,6,7],[0,1,2,0,3]), x = SparseVector(10,[1,3,5,6,7],[0,1,2,0,3])
    @test nnz(S) == nnz(x) == 5
    for I = (:, 1:10, collect(1:10))
        @test S[I,1] == S[I] == x[I] == x
        @test nnz(S[I,1]) == nnz(S[I]) == nnz(x[I]) == nnz(x)
    end
    for I = (2:9, 1:2, 9:10, [3,6,1], [10,9,8], [])
        @test S[I,1] == S[I] == x[I]
        @test nnz(S[I,1]) == nnz(S[I]) == nnz(x[I])
    end
    @test S[[1 3 5; 2 4 6]] == x[[1 3 5; 2 4 6]]
    @test nnz(S[[1 3 5; 2 4 6]]) == nnz(x[[1 3 5; 2 4 6]])
end


# Issue 14013
s14013 = sparse([10.0 0.0 30.0; 0.0 1.0 0.0])
a14013 = [10.0 0.0 30.0; 0.0 1.0 0.0]
@test s14013 == a14013
@test vec(s14013) == s14013[:] == a14013[:]
@test Array(s14013)[1,:] == s14013[1,:] == a14013[1,:] == [10.0, 0.0, 30.0]
@test Array(s14013)[2,:] == s14013[2,:] == a14013[2,:] == [0.0, 1.0, 0.0]

# Issue 14046
s14046 = sprand(5, 1.0)
@test spzeros(5) + s14046 == s14046
@test 2*s14046 == s14046 + s14046

# Issue 14589
# test vectors with no zero elements
let x = sparsevec(1:7, [3., 2., -1., 1., -2., -3., 3.], 7)
    @test collect(sort(x)) == sort(collect(x))
end
# test vectors with all zero elements
let x = sparsevec(Int64[], Float64[], 7)
    @test collect(sort(x)) == sort(collect(x))
end
# test vector with sparsity approx 1/2
let x = sparsevec(1:7, [3., 2., -1., 1., -2., -3., 3.], 15)
    @test collect(sort(x)) == sort(collect(x))
    # apply three distinct tranformations where zeros sort into start/middle/end
    @test collect(sort(x, by=abs)) == sort(collect(x), by=abs)
    @test collect(sort(x, by=sign)) == sort(collect(x), by=sign)
    @test collect(sort(x, by=inv)) == sort(collect(x), by=inv)
end

# fill!
for Tv in [Float32, Float64, Int64, Int32, Complex128]
    for Ti in [Int16, Int32, Int64, BigInt]
        sptypes = (SparseMatrixCSC{Tv, Ti}, SparseVector{Tv, Ti})
        sizes = [(3, 4), (3,)]
        for (siz, Sp) in zip(sizes, sptypes)
            arr = rand(Tv, siz...)
            sparr = Sp(arr)
            fillval = rand(Tv)
            fill!(sparr, fillval)
            @test Array(sparr) == fillval * ones(arr)
            fill!(sparr, 0)
            @test Array(sparr) == zeros(arr)
        end
    end
end

# ref 13130 and 16661
@test issparse([sprand(10,10,.1) sprand(10,.1)])
@test issparse([sprand(10,1,.1); sprand(10,.1)])

@test issparse([sprand(10,10,.1) rand(10)])
@test issparse([sprand(10,1,.1)  rand(10)])
@test issparse([sprand(10,2,.1) sprand(10,1,.1) rand(10)])
@test issparse([sprand(10,1,.1); rand(10)])

@test issparse([sprand(10,.1)  rand(10)])
@test issparse([sprand(10,.1); rand(10)])


mutable struct t20488 end

@testset "similar" begin
    x = sparsevec(rand(3) .+ 0.1)
    @test length(similar(x, t20488).nzval) == 3
    @test typeof(similar(x, Float32, Int32)) == SparseVector{Float32, Int32}
    @test typeof(similar(x, Float32)) == SparseVector{Float32, Int}
end

@testset "show" begin
    io = IOBuffer()
    show(io, MIME"text/plain"(), sparsevec(Int64[1], [1.0]))
    @test String(take!(io)) == "1-element SparseVector{Float64,Int64} with 1 stored entry:\n  [1]  =  1.0"
    show(io, MIME"text/plain"(),  spzeros(Float64, Int64, 2))
    @test String(take!(io)) == "2-element SparseVector{Float64,Int64} with 0 stored entries"
    show(io, similar(sparsevec(rand(3) .+ 0.1), t20488))
    @test String(take!(io)) == "  [1]  =  #undef\n  [2]  =  #undef\n  [3]  =  #undef"
end

@testset "spzeros with index type" begin
    @test typeof(spzeros(Float32, Int16, 3)) == SparseVector{Float32,Int16}
end

@testset "corner cases of broadcast arithmetic operations with scalars (#21515)" begin
    # test both scalar literals and variables
    areequal(a, b, c) = isequal(a, b) && isequal(b, c)
    inf, zeroh, zv, spzv = Inf, 0.0, zeros(1), spzeros(1)
    @test areequal(spzv .* Inf,  spzv .* inf,    sparsevec(zv .* Inf))
    @test areequal(Inf .* spzv,  inf .* spzv,    sparsevec(Inf .* zv))
    @test areequal(spzv ./ 0.0,  spzv ./ zeroh,  sparsevec(zv ./ 0.0))
    @test areequal(0.0 .\ spzv,  zeroh .\ spzv,  sparsevec(0.0 .\ zv))
end

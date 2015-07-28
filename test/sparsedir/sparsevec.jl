# helpers

function exact_equal(x::SparseVector, y::SparseVector)
    x.n == y.n && x.nzind == y.nzind && x.nzval == y.nzval
end

# empty sparse vectors

x0 = SparseVector(8)
@test isa(x0, SparseVector{Float64,Int})
@test length(x0) == 8
@test nnz(x0) == 0

x0 = SparseVector(Float32, 8)
@test isa(x0, SparseVector{Float32,Int})
@test length(x0) == 8
@test nnz(x0) == 0

x0 = SparseVector(Float32, Int32, 8)
@test isa(x0, SparseVector{Float32, Int32})
@test length(x0) == 8
@test nnz(x0) == 0


# construction

x = SparseVector(8, [2, 5, 6], [1.25, -0.75, 3.5])
x2 = SparseVector(8, [1, 2, 6, 7], [3.25, 4.0, -5.5, -6.0])

@test eltype(x) == Float64
@test ndims(x) == 1
@test length(x) == 8
@test size(x) == (8,)
@test size(x,1) == 8
@test size(x,2) == 1
@test !isempty(x)

@test countnz(x) == 3
@test nnz(x) == 3
@test nonzeros(x) == [1.25, -0.75, 3.5]

dct = Dict{Int,Float64}()
dct[2] = 1.25
dct[5] = -0.75
dct[6] = 3.5
xc = SparseVector(8, dct)
@test isa(xc, SparseVector{Float64,Int})
@test exact_equal(x, xc)

# full

xf = zeros(8)
xf[2] = 1.25
xf[5] = -0.75
xf[6] = 3.5
@test isa(full(x), Vector{Float64})
@test full(x) == xf

xf2 = zeros(8)
xf2[1] = 3.25
xf2[2] = 4.0
xf2[6] = -5.5
xf2[7] = -6.0
@test isa(full(x2), Vector{Float64})
@test full(x2) == xf2


# conversion

xc = convert(SparseVector, xf)
@test isa(xc, SparseVector{Float64,Int})
@test exact_equal(x, xc)

xc = convert(SparseVector{Float32,Int}, x)
@test isa(xc, SparseVector{Float32,Int})
@test exact_equal(x, xc)

xc = convert(SparseVector{Float32}, x)
@test isa(xc, SparseVector{Float32,Int})
@test exact_equal(x, xc)

# copy

xc = copy(x)
@test isa(xc, SparseVector{Float64,Int})
@test !is(x.nzind, xc.nzval)
@test !is(x.nzval, xc.nzval)
@test exact_equal(x, xc)

# getindex

for i = 1:length(x)
    @test x[i] == xf[i]
end

# setindex

xc = SparseVector(8)
xc[3] = 2.0
@test exact_equal(xc, SparseVector(8, [3], [2.0]))

xc = copy(x)
xc[5] = 2.0
@test exact_equal(xc, SparseVector(8, [2, 5, 6], [1.25, 2.0, 3.5]))

xc = copy(x)
xc[3] = 4.0
@test exact_equal(xc, SparseVector(8, [2, 3, 5, 6], [1.25, 4.0, -0.75, 3.5]))

xc[1] = 6.0
@test exact_equal(xc, SparseVector(8, [1, 2, 3, 5, 6], [6.0, 1.25, 4.0, -0.75, 3.5]))

xc[8] = -1.5
@test exact_equal(xc, SparseVector(8, [1, 2, 3, 5, 6, 8], [6.0, 1.25, 4.0, -0.75, 3.5, -1.5]))

xc = copy(x)
xc[5] = 0.0
@test exact_equal(xc, SparseVector(8, [2, 6], [1.25, 3.5]))

xc[6] = 0.0
@test exact_equal(xc, SparseVector(8, [2], [1.25]))

xc[2] = 0.0
@test exact_equal(xc, SparseVector(8, Int[], Float64[]))


# sprand

xr = sprand(1000, 0.3)
@test isa(xr, SparseVector{Float64,Int})
@test length(xr) == 1000
@test all(nonzeros(xr) .>= 0.0)

xr = sprand(1000, 0.3, Float32)
@test isa(xr, SparseVector{Float32,Int})
@test length(xr) == 1000
@test all(nonzeros(xr) .>= 0.0)

xr = sprandn(1000, 0.3)
@test isa(xr, SparseVector{Float64,Int})
@test length(xr) == 1000
@test any(nonzeros(xr) .> 0.0) && any(nonzeros(xr) .< 0.0)

# abs and abs2

@test exact_equal(abs(x), SparseVector(8, [2, 5, 6], abs([1.25, -0.75, 3.5])))
@test exact_equal(abs2(x), SparseVector(8, [2, 5, 6], abs2([1.25, -0.75, 3.5])))

# plus and minus

xa = SparseVector(8, [1,2,5,6,7], [3.25,5.25,-0.75,-2.0,-6.0])

@test exact_equal(x + x, SparseVector(8, [2,5,6], [2.5,-1.5,7.0]))
@test exact_equal(x + x2, xa)

xb = SparseVector(8, [1,2,5,6,7], [-3.25,-2.75,-0.75,9.0,6.0])

@test exact_equal(x - x, SparseVector(8, Int[], Float64[]))
@test exact_equal(x - x2, xb)

@test full(x) + x2 == full(xa)
@test full(x) - x2 == full(xb)
@test x + full(x2) == full(xa)
@test x - full(x2) == full(xb)


# reduction

@test sum(x) == 4.0
@test sumabs(x) == 5.5
@test sumabs2(x) == 14.375

@test vecnorm(x) == sqrt(14.375)
@test vecnorm(x, 1) == 5.5
@test vecnorm(x, 2) == sqrt(14.375)
@test vecnorm(x, Inf) == 3.5

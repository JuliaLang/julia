## sparsevec.jl

### Data

spv_x1 = SparseVector(8, [2, 5, 6], [1.25, -0.75, 3.5])
_x2 = SparseVector(8, [1, 2, 6, 7], [3.25, 4.0, -5.5, -6.0])
spv_x2 = view(_x2)

@test isa(spv_x1, SparseVector{Float64,Int})
@test isa(spv_x2, SparseVectorView{Float64,Int})

x1_full = zeros(length(spv_x1))
x1_full[nonzeroinds(spv_x1)] = nonzeros(spv_x1)

x2_full = zeros(length(spv_x2))
x2_full[nonzeroinds(spv_x2)] = nonzeros(spv_x2)


### Basic Properties

let x = spv_x1
    @test eltype(x) == Float64
    @test ndims(x) == 1
    @test length(x) == 8
    @test size(x) == (8,)
    @test size(x,1) == 8
    @test size(x,2) == 1
    @test !isempty(x)

    @test countnz(x) == 3
    @test nnz(x) == 3
    @test nonzeroinds(x) == [2, 5, 6]
    @test nonzeros(x) == [1.25, -0.75, 3.5]
end

let x = spv_x2
    @test eltype(x) == Float64
    @test ndims(x) == 1
    @test length(x) == 8
    @test size(x) == (8,)
    @test size(x,1) == 8
    @test size(x,2) == 1
    @test !isempty(x)

    @test countnz(x) == 4
    @test nnz(x) == 4
    @test nonzeroinds(x) == [1, 2, 6, 7]
    @test nonzeros(x) == [3.25, 4.0, -5.5, -6.0]
end

# full

for (x, xf) in [(spv_x1, x1_full), (spv_x2, x2_full)]
    @test isa(full(x), Vector{Float64})
    @test full(x) == xf
end

### Show

@test string(spv_x1) == "Sparse vector, length = 8, with 3 Float64 entries:\n" *
"  [2]  =  1.25\n" *
"  [5]  =  -0.75\n" *
"  [6]  =  3.5\n"


### Other Constructors

# construct empty sparse vector

@test exact_equal(sparsevector(Float64, 8), SparseVector(8, Int[], Float64[]))

# from list of indices and values

@test exact_equal(
    sparsevector(Int[], Float64[], 8),
    SparseVector(8, Int[], Float64[]))

@test exact_equal(
    sparsevector(Int[], Float64[]),
    SparseVector(0, Int[], Float64[]))

@test exact_equal(
    sparsevector([3, 3], [5.0, -5.0], 8),
    sparsevector(Float64, 8))

@test exact_equal(
    sparsevector([2, 3, 6], [12.0, 18.0, 25.0]),
    SparseVector(6, [2, 3, 6], [12.0, 18.0, 25.0]))

let x0 = SparseVector(8, [2, 3, 6], [12.0, 18.0, 25.0])
    @test exact_equal(
        sparsevector([2, 3, 6], [12.0, 18.0, 25.0], 8), x0)

    @test exact_equal(
        sparsevector([3, 6, 2], [18.0, 25.0, 12.0], 8), x0)

    @test exact_equal(
        sparsevector([2, 3, 4, 4, 6], [12.0, 18.0, 5.0, -5.0, 25.0], 8),
        x0)

    @test exact_equal(
        sparsevector([1, 1, 1, 2, 3, 3, 6], [2.0, 3.0, -5.0, 12.0, 10.0, 8.0, 25.0], 8),
        x0)

    @test exact_equal(
        sparsevector([2, 3, 6, 7, 7], [12.0, 18.0, 25.0, 5.0, -5.0], 8), x0)
end

# from dictionary

function my_intmap(x)
    a = Dict{Int,eltype(x)}()
    for i in nonzeroinds(x)
        a[i] = x[i]
    end
    return a
end

let x = spv_x1
    a = my_intmap(x)
    xc = sparsevector(a, 8)
    @test exact_equal(x, xc)

    xc = sparsevector(a)
    @test exact_equal(xc, SparseVector(6, [2, 5, 6], [1.25, -0.75, 3.5]))
end

# sprand & sprandn

let xr = sprand(1000, 0.3)
    @test isa(xr, SparseVector{Float64,Int})
    @test length(xr) == 1000
    @test all(nonzeros(xr) .>= 0.0)
end

let xr = sprand(1000, 0.3, Float32)
    @test isa(xr, SparseVector{Float32,Int})
    @test length(xr) == 1000
    @test all(nonzeros(xr) .>= 0.0)
end

let xr = sprandn(1000, 0.3)
    @test isa(xr, SparseVector{Float64,Int})
    @test length(xr) == 1000
    @test any(nonzeros(xr) .> 0.0) && any(nonzeros(xr) .< 0.0)
end


### Element access

# getindex

# single integer index
for (x, xf) in [(spv_x1, x1_full), (spv_x2, x2_full)]
    for i = 1:length(x)
        @test x[i] == xf[i]
    end
end

# range index
let x = spv_x2
    # range that contains no non-zeros
    @test exact_equal(x[3:2], sparsevector(Float64, 0))
    @test exact_equal(x[3:3], sparsevector(Float64, 1))
    @test exact_equal(x[3:5], sparsevector(Float64, 3))

    # range with non-zeros
    @test exact_equal(x[1:length(x)], x)
    @test exact_equal(x[1:5], SparseVector(5, [1,2], [3.25, 4.0]))
    @test exact_equal(x[2:6], SparseVector(5, [1,5], [4.0, -5.5]))
    @test exact_equal(x[2:8], SparseVector(7, [1,5,6], [4.0, -5.5, -6.0]))
end

# generic array index
let x = sprand(100, 0.5)
    I = rand(1:length(x), 20)
    @which x[I]
    r = x[I]
    @test isa(r, SparseVector{Float64,Int})
    @test all(nonzeros(r) .!= 0.0)
    @test full(r) == full(x)[I]
end

# setindex

let xc = sparsevector(Float64, 8)
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
    @test exact_equal(xc, SparseVector(8, [2, 6], [1.25, 3.5]))

    xc[6] = 0.0
    @test exact_equal(xc, SparseVector(8, [2], [1.25]))

    xc[2] = 0.0
    @test exact_equal(xc, SparseVector(8, Int[], Float64[]))
end


### Array manipulation

# copy

let x = spv_x1
    xc = copy(x)
    @test isa(xc, SparseVector{Float64,Int})
    @test !is(x.nzind, xc.nzval)
    @test !is(x.nzval, xc.nzval)
    @test exact_equal(x, xc)
end

let a = SparseVector(8, [2, 5, 6], Int32[12, 35, 72])
    # reinterpret
    au = reinterpret(UInt32, a)
    @test isa(au, SparseVector{UInt32,Int})
    @test exact_equal(au, SparseVector(8, [2, 5, 6], UInt32[12, 35, 72]))

    # float
    af = float(a)
    @test isa(af, SparseVector{Float64,Int})
    @test exact_equal(af, SparseVector(8, [2, 5, 6], [12., 35., 72.]))

    # complex
    acp = complex(af)
    @test isa(acp, SparseVector{Complex128,Int})
    @test exact_equal(acp, SparseVector(8, [2, 5, 6], complex([12., 35., 72.])))
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
    @test full(xm) == reshape(xf, 8, 1)

    xm = convert(SparseMatrixCSC{Float32}, x)
    @test isa(xm, SparseMatrixCSC{Float32,Int})
    @test full(xm) == reshape(convert(Vector{Float32}, xf), 8, 1)
end


### Concatenation

let m = 80, n = 100
    A = Array(SparseVector{Float64,Int}, n)
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
        Hr[:,j] = full(A[j])
    end
    @test full(H) == Hr

    V = vcat(A...)
    @test isa(V, SparseVector{Float64,Int})
    @test length(V) == m * n
    Vr = vec(Hr)
    @test full(V) == Vr
end


## sparsemat.jl

let S = sprand(4, 8, 0.5)
    Sf = full(S)
    @assert isa(Sf, Matrix{Float64})

    # get a single column
    for j = 1:size(S,2)
        col = getcol(S, j)
        @test isa(col, SparseVector{Float64,Int})
        @test length(col) == size(S,1)
        @test full(col) == Sf[:,j]
    end

    # column views
    for j = 1:size(S,2)
        col = view(S, :, j)
        @test isa(col, SparseVectorView{Float64,Int})
        @test length(col) == size(S,1)
        @test full(col) == Sf[:,j]
    end

    # column-range views

    # non-empty range
    V = unsafe_colrange(S, 2:6)
    @test isa(V, SparseMatrixCSC{Float64,Int})
    @test size(V) == (4, 5)
    @test full(V) == Sf[:, 2:6]
    @test !isempty(V)

    # empty range
    V0 = unsafe_colrange(S, 2:1)
    @test isa(V0, SparseMatrixCSC{Float64,Int})
    @test size(V0) == (4, 0)
    @test isempty(V0)

end


## math.jl

### Data

rnd_x0 = sprand(50, 0.6)
rnd_x0f = full(rnd_x0)

rnd_x1 = sprand(50, 0.7) * 4.0
rnd_x1f = full(rnd_x1)

spv_x1 = SparseVector(8, [2, 5, 6], [1.25, -0.75, 3.5])
_x2 = SparseVector(8, [1, 2, 6, 7], [3.25, 4.0, -5.5, -6.0])
spv_x2 = view(_x2)


### Arithmetic operations

let x = spv_x1, x2 = spv_x2
    # negate
    @test exact_equal(-x, SparseVector(8, [2, 5, 6], [-1.25, 0.75, -3.5]))

    # abs and abs2
    @test exact_equal(abs(x), SparseVector(8, [2, 5, 6], abs([1.25, -0.75, 3.5])))
    @test exact_equal(abs2(x), SparseVector(8, [2, 5, 6], abs2([1.25, -0.75, 3.5])))

    # plus and minus
    xa = SparseVector(8, [1,2,5,6,7], [3.25,5.25,-0.75,-2.0,-6.0])

    @test exact_equal(x + x, x * 2)
    @test exact_equal(x + x2, xa)
    @test exact_equal(x2 + x, xa)

    xb = SparseVector(8, [1,2,5,6,7], [-3.25,-2.75,-0.75,9.0,6.0])
    @test exact_equal(x - x, SparseVector(8, Int[], Float64[]))
    @test exact_equal(x - x2, xb)
    @test exact_equal(x2 - x, -xb)

    @test full(x) + x2 == full(xa)
    @test full(x) - x2 == full(xb)
    @test x + full(x2) == full(xa)
    @test x - full(x2) == full(xb)

    # multiplies
    xm = SparseVector(8, [2, 6], [5.0, -19.25])
    @test exact_equal(x .* x, abs2(x))
    @test exact_equal(x .* x2, xm)
    @test exact_equal(x2 .* x, xm)

    @test full(x) .* x2 == full(xm)
    @test x .* full(x2) == full(xm)

    # max & min
    @test exact_equal(max(x, x), x)
    @test exact_equal(min(x, x), x)
    @test exact_equal(max(x, x2),
        SparseVector(8, Int[1, 2, 6], Float64[3.25, 4.0, 3.5]))
    @test exact_equal(min(x, x2),
        SparseVector(8, Int[2, 5, 6, 7], Float64[1.25, -0.75, -5.5, -6.0]))
end

### Complex

let x = spv_x1, x2 = spv_x2
    # complex
    @test exact_equal(complex(x, x),
        SparseVector(8, [2,5,6], [1.25+1.25im, -0.75-0.75im, 3.5+3.5im]))
    @test exact_equal(complex(x, x2),
        SparseVector(8, [1,2,5,6,7], [3.25im, 1.25+4.0im, -0.75+0.im, 3.5-5.5im, -6.0im]))
    @test exact_equal(complex(x2, x),
        SparseVector(8, [1,2,5,6,7], [3.25+0.im, 4.0+1.25im, -0.75im, -5.5+3.5im, -6.0+0.im]))

    # real & imag

    @test is(real(x), x)
    @test exact_equal(imag(x), sparsevector(Float64, length(x)))

    xcp = complex(x, x2)
    @test exact_equal(real(xcp), x)
    @test exact_equal(imag(xcp), x2)
end

### Zero-preserving math functions: sparse -> sparse

function check_nz2z_z2z{T}(f::Function, x::SparseVector{T}, xf::Vector{T})
    R = typeof(f(zero(T)))
    r = f(x)
    isa(r, AbstractSparseVector) || error("$f(x) is not a sparse vector.")
    eltype(r) == R || error("$f(x) results in eltype = $(eltype(r)), expect $R")
    all(r.nzval .!= 0) || error("$f(x) contains zeros in nzval.")
    full(r) == f(xf) || error("Incorrect results found in $f(x).")
end

for f in [floor, ceil, trunc, round]
    check_nz2z_z2z(f, rnd_x1, rnd_x1f)
end

for f in [log1p, expm1,
          sin, tan, sinpi, sind, tand,
          asin, atan, asind, atand,
          sinh, tanh, asinh, atanh]
    check_nz2z_z2z(f, rnd_x0, rnd_x0f)
end

### Non-zero-preserving math functions: sparse -> dense

function check_z2nz{T}(f::Function, x::SparseVector{T}, xf::Vector{T})
    R = typeof(f(zero(T)))
    r = f(x)
    isa(r, Vector) || error("$f(x) is not a dense vector.")
    eltype(r) == R || error("$f(x) results in eltype = $(eltype(r)), expect $R")
    r == f(xf) || error("Incorrect results found in $f(x).")
end

for f in [exp, exp2, exp10, log, log2, log10,
          cos, csc, cot, sec, cospi,
          cosd, cscd, cotd, secd,
          acos, acot, acosd, acotd,
          cosh, csch, coth, sech, acsch, asech]
    check_z2nz(f, rnd_x0, rnd_x0f)
end


### Reduction

# sum, sumabs, sumabs2, vecnorm

let x = spv_x1
    @test sum(x) == 4.0
    @test sumabs(x) == 5.5
    @test sumabs2(x) == 14.375

    @test vecnorm(x) == sqrt(14.375)
    @test vecnorm(x, 1) == 5.5
    @test vecnorm(x, 2) == sqrt(14.375)
    @test vecnorm(x, Inf) == 3.5
end

# maximum, minimum, maxabs, minabs

let x = spv_x1
    @test maximum(x) == 3.5
    @test minimum(x) == -0.75
    @test maxabs(x) == 3.5
    @test minabs(x) == 0.0
end

let x = abs(spv_x1)
    @test maximum(x) == 3.5
    @test minimum(x) == 0.0
end

let x = -abs(spv_x1)
    @test maximum(x) == 0.0
    @test minimum(x) == -3.5
end

let x = SparseVector(3, [1, 2, 3], [-4.5, 2.5, 3.5])
    @test maximum(x) == 3.5
    @test minimum(x) == -4.5
    @test maxabs(x) == 4.5
    @test minabs(x) == 2.5
end

let x = sparsevector(Float64, 8)
    @test maximum(x) == 0.0
    @test minimum(x) == 0.0
    @test maxabs(x) == 0.0
    @test minabs(x) == 0.0
end


### linalg.jl

### BLAS Level-1

let x = sprand(16, 0.5), x2 = sprand(16, 0.4)
    xf = full(x)
    xf2 = full(x2)

    # axpy!
    for c in [1.0, -1.0, 2.0, -2.0]
        y = full(x)
        @test is(axpy!(c, x2, y), y)
        @test y == full(x2 * c + x)
    end

    # scale
    let sx = SparseVector(x.n, x.nzind, x.nzval * 2.5)
        @test exact_equal(scale(x, 2.5), sx)
        @test exact_equal(scale(2.5, x), sx)
        @test exact_equal(x * 2.5, sx)
        @test exact_equal(2.5 * x, sx)
        @test exact_equal(x .* 2.5, sx)
        @test exact_equal(2.5 .* x, sx)

        xc = copy(x)
        @test is(scale!(xc, 2.5), xc)
        @test exact_equal(xc, sx)
    end

    # dot
    let dv = dot(xf, xf2)
        @test dot(x, x) == sumabs2(x)
        @test dot(x2, x2) == sumabs2(x2)
        @test_approx_eq dot(x, x2) dv
        @test_approx_eq dot(x2, x) dv
        @test_approx_eq dot(full(x), x2) dv
        @test_approx_eq dot(x, full(x2)) dv
    end
end

let x = complex(sprand(32, 0.6), sprand(32, 0.6)),
    y = complex(sprand(32, 0.6), sprand(32, 0.6))
    xf = full(x)::Vector{Complex128}
    yf = full(y)::Vector{Complex128}
    @test_approx_eq dot(x, x) dot(xf, xf)
    @test_approx_eq dot(x, y) dot(xf, yf)
end



### BLAS Level-2:

## dense A * sparse x -> dense y

let A = randn(9, 16), x = sprand(16, 0.7)
    xf = full(x)
    for α in [0.0, 1.0, 2.0], β in [0.0, 0.5, 1.0]
        y = rand(9)
        rr = α * A * xf + β * y
        @test is(A_mul_B!(α, A, x, β, y), y)
        @test_approx_eq y rr
    end
    y = A * x
    @test isa(y, Vector{Float64})
    @test_approx_eq A * x A * xf
end

let A = randn(16, 9), x = sprand(16, 0.7)
    xf = full(x)
    for α in [0.0, 1.0, 2.0], β in [0.0, 0.5, 1.0]
        y = rand(9)
        rr = α * A'xf + β * y
        @test is(At_mul_B!(α, A, x, β, y), y)
        @test_approx_eq y rr
    end
    y = At_mul_B(A, x)
    @test isa(y, Vector{Float64})
    @test_approx_eq y At_mul_B(A, xf)
end

## sparse A * sparse x -> dense y

let A = sprandn(9, 16, 0.5), x = sprand(16, 0.7)
    Af = full(A)
    xf = full(x)
    for α in [0.0, 1.0, 2.0], β in [0.0, 0.5, 1.0]
        y = rand(9)
        rr = α * Af * xf + β * y
        @test is(A_mul_B!(α, A, x, β, y), y)
        @test_approx_eq y rr
    end
    y = densemv(A, x)
    @test isa(y, Vector{Float64})
    @test_approx_eq y Af * xf
end

let A = sprandn(16, 9, 0.5), x = sprand(16, 0.7)
    Af = full(A)
    xf = full(x)
    for α in [0.0, 1.0, 2.0], β in [0.0, 0.5, 1.0]
        y = rand(9)
        rr = α * Af'xf + β * y
        @test is(At_mul_B!(α, A, x, β, y), y)
        @test_approx_eq y rr
    end
    y = densemv(A, x; trans='T')
    @test isa(y, Vector{Float64})
    @test_approx_eq y At_mul_B(Af, xf)
end

let A = complex(sprandn(7, 8, 0.5), sprandn(7, 8, 0.5)),
    x = complex(sprandn(8, 0.6), sprandn(8, 0.6)),
    x2 = complex(sprandn(7, 0.75), sprandn(7, 0.75))
    Af = full(A)
    xf = full(x)
    x2f = full(x2)
    @test_approx_eq densemv(A, x; trans='N') Af * xf
    @test_approx_eq densemv(A, x2; trans='T') Af.' * x2f
    @test_approx_eq densemv(A, x2; trans='C') Af'x2f
end

## sparse A * sparse x -> sparse y

let A = sprandn(9, 16, 0.5), x = sprand(16, 0.7), x2 = sprand(9, 0.7)
    Af = full(A)
    xf = full(x)
    x2f = full(x2)

    y = A * x
    @test isa(y, SparseVector{Float64,Int})
    @test all(nonzeros(y) .!= 0.0)
    @test_approx_eq full(y) Af * xf

    y = At_mul_B(A, x2)
    @test isa(y, SparseVector{Float64,Int})
    @test all(nonzeros(y) .!= 0.0)
    @test_approx_eq full(y) Af'x2f
end

let A = complex(sprandn(7, 8, 0.5), sprandn(7, 8, 0.5)),
    x = complex(sprandn(8, 0.6), sprandn(8, 0.6)),
    x2 = complex(sprandn(7, 0.75), sprandn(7, 0.75))
    Af = full(A)
    xf = full(x)
    x2f = full(x2)

    y = A * x
    @test isa(y, SparseVector{Complex128,Int})
    @test_approx_eq full(y) Af * xf

    y = At_mul_B(A, x2)
    @test isa(y, SparseVector{Complex128,Int})
    @test_approx_eq full(y) Af.' * x2f

    y = Ac_mul_B(A, x2)
    @test isa(y, SparseVector{Complex128,Int})
    @test_approx_eq full(y) Af'x2f
end

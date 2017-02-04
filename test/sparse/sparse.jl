# This file is a part of Julia. License is MIT: http://julialang.org/license

@testset "issparse" begin
    @test issparse(sparse(ones(5,5)))
    @test !issparse(ones(5,5))
end

@testset "indtype" begin
    @test Base.SparseArrays.indtype(sparse(ones(Int8,2),ones(Int8,2),rand(2))) == Int8
end

@testset "sparse matrix construction" begin
    @test isequal(Array(sparse(complex.(ones(5,5), ones(5,5)))), complex.(ones(5,5), ones(5,5)))
    @test_throws ArgumentError sparse([1,2,3], [1,2], [1,2,3], 3, 3)
    @test_throws ArgumentError sparse([1,2,3], [1,2,3], [1,2], 3, 3)
    @test_throws ArgumentError sparse([1,2,3], [1,2,3], [1,2,3], 0, 1)
    @test_throws ArgumentError sparse([1,2,3], [1,2,3], [1,2,3], 1, 0)
    @test_throws ArgumentError sparse([1,2,4], [1,2,3], [1,2,3], 3, 3)
    @test_throws ArgumentError sparse([1,2,3], [1,2,4], [1,2,3], 3, 3)
    @test isequal(sparse(Int[], Int[], Int[], 0, 0), SparseMatrixCSC(0, 0, Int[1], Int[], Int[]))
    @test sparse(Any[1,2,3], Any[1,2,3], Any[1,1,1]) == sparse([1,2,3], [1,2,3], [1,1,1])
    @test sparse(Any[1,2,3], Any[1,2,3], Any[1,1,1], 5, 4) == sparse([1,2,3], [1,2,3], [1,1,1], 5, 4)
end

se33 = speye(3)
do33 = ones(3)

@testset "sparse binary operations" begin
    @test isequal(se33 * se33, se33)

    @test all(Array(se33 + convert(SparseMatrixCSC{Float32,Int32}, se33)) == 2*eye(3))
    @test all(Array(se33 * convert(SparseMatrixCSC{Float32,Int32}, se33)) == eye(3))

    @testset "shape checks for sparse elementwise binary operations equivalent to map" begin
        sqrfloatmat, colfloatmat = sprand(4, 4, 0.5), sprand(4, 1, 0.5)
        @test_throws DimensionMismatch (+)(sqrfloatmat, colfloatmat)
        @test_throws DimensionMismatch (-)(sqrfloatmat, colfloatmat)
        @test_throws DimensionMismatch map(min, sqrfloatmat, colfloatmat)
        @test_throws DimensionMismatch map(max, sqrfloatmat, colfloatmat)
        sqrboolmat, colboolmat = sprand(Bool, 4, 4, 0.5), sprand(Bool, 4, 1, 0.5)
        @test_throws DimensionMismatch map(&, sqrboolmat, colboolmat)
        @test_throws DimensionMismatch map(|, sqrboolmat, colboolmat)
        @test_throws DimensionMismatch map(xor, sqrboolmat, colboolmat)
    end
end

@testset "concatenation tests" begin
    @testset "horizontal concatenation" begin
        @test all([se33 se33] == sparse([1, 2, 3, 1, 2, 3], [1, 2, 3, 4, 5, 6], ones(6)))
    end

    @testset "vertical concatenation" begin
        @test all([se33; se33] == sparse([1, 4, 2, 5, 3, 6], [1, 1, 2, 2, 3, 3], ones(6)))
        se33_32bit = convert(SparseMatrixCSC{Float32,Int32}, se33)
        @test all([se33; se33_32bit] == sparse([1, 4, 2, 5, 3, 6], [1, 1, 2, 2, 3, 3], ones(6)))
    end

    se44 = speye(4)
    sz42 = spzeros(4, 2)
    sz41 = spzeros(4, 1)
    sz34 = spzeros(3, 4)
    se77 = speye(7)
    @testset "h+v concatenation" begin
        @test all([se44 sz42 sz41; sz34 se33] == se77)
    end

    @testset "blkdiag concatenation" begin
        @test all(blkdiag(se33, se33) == sparse([1, 2, 3, 4, 5, 6], [1, 2, 3, 4, 5, 6], ones(6)))
    end

    @testset "concatenation promotion" begin
        sz41_f32 = spzeros(Float32, 4, 1)
        se33_i32 = speye(Int32, 3, 3)
        @test all([se44 sz42 sz41_f32; sz34 se33_i32] == se77)
    end

    @testset "mixed sparse-dense concatenation" begin
        sz33 = spzeros(3, 3)
        de33 = eye(3)
        @test  all([se33 de33; sz33 se33] == Array([se33 se33; sz33 se33 ]))
    end

    # check splicing + concatenation on random instances, with nested vcat and also side-checks sparse ref
    @testset "splicing + concatenation on random instances" begin
        for i = 1 : 10
            a = sprand(5, 4, 0.5)
            @test all([a[1:2,1:2] a[1:2,3:4]; a[3:5,1] [a[3:4,2:4]; a[5:5,2:4]]] == a)
        end
    end
end

a116 = copy(reshape(1:16, 4, 4))
s116 = sparse(a116)

@testset "sparse ref" begin
    p = [4, 1, 2, 3, 2]
    @test Array(s116[p,:]) == a116[p,:]
    @test Array(s116[:,p]) == a116[:,p]
    @test Array(s116[p,p]) == a116[p,p]
end

@testset "sparse assignment" begin
    p = [4, 1, 3]
    a116[p, p] = -1
    s116[p, p] = -1
    @test a116 == s116

    p = [2, 1, 4]
    a116[p, p] = reshape(1:9, 3, 3)
    s116[p, p] = reshape(1:9, 3, 3)
    @test a116 == s116
end

@testset "squeeze" begin
    for i = 1:5
        am = sprand(20, 1, 0.2)
        av = squeeze(am, 2)
        @test ndims(av) == 1
        @test all(av.==am)
        am = sprand(1, 20, 0.2)
        av = squeeze(am, 1)
        @test ndims(av) == 1
        @test all(av.'.==am)
    end
end

@testset "matrix-vector multiplication (non-square)" begin
    for i = 1:5
        a = sprand(10, 5, 0.5)
        b = rand(5)
        @test maximum(abs.(a*b - Array(a)*b)) < 100*eps()
    end
end

@testset "sparse matrix * BitArray" begin
    A = sprand(5,5,0.2)
    B = trues(5)
    @test A*B ≈ Array(A)*B
    B = trues(5,5)
    @test A*B ≈ Array(A)*B
    @test B*A ≈ B*Array(A)
end

@testset "complex matrix-vector multiplication and left-division" begin
    if Base.USE_GPL_LIBS
    for i = 1:5
        a = speye(5) + 0.1*sprandn(5, 5, 0.2)
        b = randn(5,3) + im*randn(5,3)
        c = randn(5) + im*randn(5)
        d = randn(5) + im*randn(5)
        α = rand(Complex128)
        β = rand(Complex128)
        @test (maximum(abs.(a*b - Array(a)*b)) < 100*eps())
        @test (maximum(abs.(A_mul_B!(similar(b), a, b) - Array(a)*b)) < 100*eps()) # for compatibility with present matmul API. Should go away eventually.
        @test (maximum(abs.(A_mul_B!(similar(c), a, c) - Array(a)*c)) < 100*eps()) # for compatibility with present matmul API. Should go away eventually.
        @test (maximum(abs.(At_mul_B!(similar(b), a, b) - Array(a).'*b)) < 100*eps()) # for compatibility with present matmul API. Should go away eventually.
        @test (maximum(abs.(At_mul_B!(similar(c), a, c) - Array(a).'*c)) < 100*eps()) # for compatibility with present matmul API. Should go away eventually.
        @test (maximum(abs.(a'b - Array(a)'b)) < 100*eps())
        @test (maximum(abs.(a.'b - Array(a).'b)) < 100*eps())
        @test (maximum(abs.(a\b - Array(a)\b)) < 1000*eps())
        @test (maximum(abs.(a'\b - Array(a')\b)) < 1000*eps())
        @test (maximum(abs.(a.'\b - Array(a.')\b)) < 1000*eps())
        @test (maximum(abs.((a'*c + d) - (Array(a)'*c + d))) < 1000*eps())
        @test (maximum(abs.((α*a.'*c + β*d) - (α*Array(a).'*c + β*d))) < 1000*eps())
        @test (maximum(abs.((a.'*c + d) - (Array(a).'*c + d))) < 1000*eps())
        c = randn(6) + im*randn(6)
        @test_throws DimensionMismatch α*a.'*c + β*c
        @test_throws DimensionMismatch α*a.'*ones(5) + β*c

        a = speye(5) + 0.1*sprandn(5, 5, 0.2) + 0.1*im*sprandn(5, 5, 0.2)
        b = randn(5,3)
        @test (maximum(abs.(a*b - Array(a)*b)) < 100*eps())
        @test (maximum(abs.(a'b - Array(a)'b)) < 100*eps())
        @test (maximum(abs.(a.'b - Array(a).'b)) < 100*eps())
        @test (maximum(abs.(a\b - Array(a)\b)) < 1000*eps())
        @test (maximum(abs.(a'\b - Array(a')\b)) < 1000*eps())
        @test (maximum(abs.(a.'\b - Array(a.')\b)) < 1000*eps())

        a = speye(5) + tril(0.1*sprandn(5, 5, 0.2))
        b = randn(5,3) + im*randn(5,3)
        @test (maximum(abs.(a*b - Array(a)*b)) < 100*eps())
        @test (maximum(abs.(a'b - Array(a)'b)) < 100*eps())
        @test (maximum(abs.(a.'b - Array(a).'b)) < 100*eps())
        @test (maximum(abs.(a\b - Array(a)\b)) < 1000*eps())
        @test (maximum(abs.(a'\b - Array(a')\b)) < 1000*eps())
        @test (maximum(abs.(a.'\b - Array(a.')\b)) < 1000*eps())

        a = speye(5) + tril(0.1*sprandn(5, 5, 0.2) + 0.1*im*sprandn(5, 5, 0.2))
        b = randn(5,3)
        @test (maximum(abs.(a*b - Array(a)*b)) < 100*eps())
        @test (maximum(abs.(a'b - Array(a)'b)) < 100*eps())
        @test (maximum(abs.(a.'b - Array(a).'b)) < 100*eps())
        @test (maximum(abs.(a\b - Array(a)\b)) < 1000*eps())
        @test (maximum(abs.(a'\b - Array(a')\b)) < 1000*eps())
        @test (maximum(abs.(a.'\b - Array(a.')\b)) < 1000*eps())

        a = speye(5) + triu(0.1*sprandn(5, 5, 0.2))
        b = randn(5,3) + im*randn(5,3)
        @test (maximum(abs.(a*b - Array(a)*b)) < 100*eps())
        @test (maximum(abs.(a'b - Array(a)'b)) < 100*eps())
        @test (maximum(abs.(a.'b - Array(a).'b)) < 100*eps())
        @test (maximum(abs.(a\b - Array(a)\b)) < 1000*eps())
        @test (maximum(abs.(a'\b - Array(a')\b)) < 1000*eps())
        @test (maximum(abs.(a.'\b - Array(a.')\b)) < 1000*eps())

        a = speye(5) + triu(0.1*sprandn(5, 5, 0.2) + 0.1*im*sprandn(5, 5, 0.2))
        b = randn(5,3)
        @test (maximum(abs.(a*b - Array(a)*b)) < 100*eps())
        @test (maximum(abs.(a'b - Array(a)'b)) < 100*eps())
        @test (maximum(abs.(a.'b - Array(a).'b)) < 100*eps())
        @test (maximum(abs.(a\b - Array(a)\b)) < 1000*eps())
        @test (maximum(abs.(a'\b - Array(a')\b)) < 1000*eps())
        @test (maximum(abs.(a.'\b - Array(a.')\b)) < 1000*eps())

        a = speye(5) + triu(0.1*sprandn(5, 5, 0.2))
        b = randn(5,3) + im*randn(5,3)
        @test (maximum(abs.(a*b - Array(a)*b)) < 100*eps())
        @test (maximum(abs.(a'b - Array(a)'b)) < 100*eps())
        @test (maximum(abs.(a.'b - Array(a).'b)) < 100*eps())
        @test (maximum(abs.(a\b - Array(a)\b)) < 1000*eps())
        @test (maximum(abs.(a'\b - Array(a')\b)) < 1000*eps())
        @test (maximum(abs.(a.'\b - Array(a.')\b)) < 1000*eps())

        a = spdiagm(randn(5)) + im*spdiagm(randn(5))
        b = randn(5,3)
        @test (maximum(abs.(a*b - Array(a)*b)) < 100*eps())
        @test (maximum(abs.(a'b - Array(a)'b)) < 100*eps())
        @test (maximum(abs.(a.'b - Array(a).'b)) < 100*eps())
        @test (maximum(abs.(a\b - Array(a)\b)) < 1000*eps())
        @test (maximum(abs.(a'\b - Array(a')\b)) < 1000*eps())
        @test (maximum(abs.(a.'\b - Array(a.')\b)) < 1000*eps())

        b = randn(5,3) + im*randn(5,3)
        @test (maximum(abs.(a*b - Array(a)*b)) < 100*eps())
        @test (maximum(abs.(a'b - Array(a)'b)) < 100*eps())
        @test (maximum(abs.(a.'b - Array(a).'b)) < 100*eps())
        @test (maximum(abs.(a\b - Array(a)\b)) < 1000*eps())
        @test (maximum(abs.(a'\b - Array(a')\b)) < 1000*eps())
        @test (maximum(abs.(a.'\b - Array(a.')\b)) < 1000*eps())
    end
    end
end

@testset "matrix multiplication and kron" begin
    for i = 1:5
        a = sprand(10, 5, 0.7)
        b = sprand(5, 15, 0.3)
        @test maximum(abs.(a*b - Array(a)*Array(b))) < 100*eps()
        @test maximum(abs.(Base.SparseArrays.spmatmul(a,b,sortindices=:sortcols) - Array(a)*Array(b))) < 100*eps()
        @test maximum(abs.(Base.SparseArrays.spmatmul(a,b,sortindices=:doubletranspose) - Array(a)*Array(b))) < 100*eps()
        @test Array(kron(a,b)) == kron(Array(a), Array(b))
        @test Array(kron(Array(a),b)) == kron(Array(a), Array(b))
        @test Array(kron(a,Array(b))) == kron(Array(a), Array(b))
        c = sparse(rand(Float32,5,5))
        d = sparse(rand(Float64,5,5))
        @test Array(kron(c,d)) == kron(Array(c),Array(d))
        f = Diagonal(rand(5))
        @test Array(a*f) == Array(a)*f
        @test Array(f*b) == f*Array(b)
    end
end

sA = sprandn(3, 7, 0.5)
sC = similar(sA)
dA = Array(sA)
b = randn(7)

@testset "scale and scale!" begin
    @test dA * Diagonal(b) == sA * Diagonal(b)
    @test dA * Diagonal(b) == scale!(sC, sA, b)
    @test dA * Diagonal(b) == scale!(copy(sA), b)
    b = randn(3)
    @test Diagonal(b) * dA == Diagonal(b) * sA
    @test Diagonal(b) * dA == scale!(sC, b, sA)
    @test Diagonal(b) * dA == scale!(b, copy(sA))

    @test dA * 0.5            == sA * 0.5
    @test dA * 0.5            == scale!(sC, sA, 0.5)
    @test dA * 0.5            == scale!(copy(sA), 0.5)
    @test 0.5 * dA            == 0.5 * sA
    @test 0.5 * dA            == scale!(sC, sA, 0.5)
    @test 0.5 * dA            == scale!(0.5, copy(sA))
    @test scale!(sC, 0.5, sA) == scale!(sC, sA, 0.5)
end

@testset "copy!" begin
    A = sprand(5, 5, 0.2)
    B = sprand(5, 5, 0.2)
    copy!(A, B)
    @test A == B
    @test pointer(A.nzval) != pointer(B.nzval)
    @test pointer(A.rowval) != pointer(B.rowval)
    @test pointer(A.colptr) != pointer(B.colptr)
    # Test size(A) != size(B), but length(A) == length(B)
    B = sprand(25, 1, 0.2)
    copy!(A, B)
    @test A[:] == B[:]
    # Test various size(A) / size(B) combinations
    for mA in [5, 10, 20], nA in [5, 10, 20], mB in [5, 10, 20], nB in [5, 10, 20]
        A = sprand(mA,nA,0.4)
        Aorig = copy(A)
        B = sprand(mB,nB,0.4)
        if mA*nA >= mB*nB
            copy!(A,B)
            @assert(A[1:length(B)] == B[:])
            @assert(A[length(B)+1:end] == Aorig[length(B)+1:end])
        else
            @test_throws BoundsError copy!(A,B)
        end
    end
    # Test eltype(A) != eltype(B), size(A) != size(B)
    A = sprand(5, 5, 0.2)
    Aorig = copy(A)
    B = sparse(rand(Float32, 3, 3))
    copy!(A, B)
    @test A[1:9] == B[:]
    @test A[10:end] == Aorig[10:end]
    # Test eltype(A) != eltype(B), size(A) == size(B)
    A = sparse(rand(Float64, 3, 3))
    B = sparse(rand(Float32, 3, 3))
    copy!(A, B)
    @test A == B
end

@testset "conj" begin
    cA = sprandn(5,5,0.2) + im*sprandn(5,5,0.2)
    @test Array(conj.(cA)) == conj(Array(cA))
    @test Array(conj!(copy(cA))) == conj(Array(cA))
end

@testset "SparseMatrixCSC [c]transpose[!] and permute[!]" begin
    smalldim = 5
    largedim = 10
    nzprob = 0.4
    (m, n) = (smalldim, smalldim)
    A = sprand(m, n, nzprob)
    X = similar(A)
    C = transpose(A)
    p = randperm(m)
    q = randperm(n)
    @testset "common error checking of [c]transpose! methods (ftranspose!)" begin
        @test_throws DimensionMismatch transpose!(A[:, 1:(smalldim - 1)], A)
        @test_throws DimensionMismatch transpose!(A[1:(smalldim - 1), 1], A)
        @test_throws ArgumentError transpose!((B = similar(A); resize!(B.rowval, nnz(A) - 1); B), A)
        @test_throws ArgumentError transpose!((B = similar(A); resize!(B.nzval, nnz(A) - 1); B), A)
    end
    @testset "common error checking of permute[!] methods / source-perm compat" begin
        @test_throws DimensionMismatch permute(A, p[1:(end - 1)], q)
        @test_throws DimensionMismatch permute(A, p, q[1:(end - 1)])
    end
    @testset "common error checking of permute[!] methods / source-dest compat" begin
        @test_throws DimensionMismatch permute!(A[1:(m - 1), :], A, p, q)
        @test_throws DimensionMismatch permute!(A[:, 1:(m - 1)], A, p, q)
        @test_throws ArgumentError permute!((Y = copy(X); resize!(Y.rowval, nnz(A) - 1); Y), A, p, q)
        @test_throws ArgumentError permute!((Y = copy(X); resize!(Y.nzval, nnz(A) - 1); Y), A, p, q)
    end
    @testset "common error checking of permute[!] methods / source-workmat compat" begin
        @test_throws DimensionMismatch permute!(X, A, p, q, C[1:(m - 1), :])
        @test_throws DimensionMismatch permute!(X, A, p, q, C[:, 1:(m - 1)])
        @test_throws ArgumentError permute!(X, A, p, q, (D = copy(C); resize!(D.rowval, nnz(A) - 1); D))
        @test_throws ArgumentError permute!(X, A, p, q, (D = copy(C); resize!(D.nzval, nnz(A) - 1); D))
    end
    @testset "common error checking of permute[!] methods / source-workcolptr compat" begin
        @test_throws DimensionMismatch permute!(A, p, q, C, Vector{eltype(A.rowval)}(length(A.colptr) - 1))
    end
    @testset "common error checking of permute[!] methods / permutation validity" begin
        @test_throws ArgumentError permute!(A, (r = copy(p); r[2] = r[1]; r), q)
        @test_throws ArgumentError permute!(A, (r = copy(p); r[2] = m + 1; r), q)
        @test_throws ArgumentError permute!(A, p, (r = copy(q); r[2] = r[1]; r))
        @test_throws ArgumentError permute!(A, p, (r = copy(q); r[2] = n + 1; r))
    end
    @testset "overall functionality of [c]transpose[!] and permute[!]" begin
        for (m, n) in ((smalldim, smalldim), (smalldim, largedim), (largedim, smalldim))
            A = sprand(m, n, nzprob)
            At = transpose(A)
            # transpose[!]
            fullAt = transpose(Array(A))
            @test transpose(A) == fullAt
            @test transpose!(similar(At), A) == fullAt
            # ctranspose[!]
            C = A + im*A/2
            fullCh = ctranspose(Array(C))
            @test ctranspose(C) == fullCh
            @test ctranspose!(similar(sparse(fullCh)), C) == fullCh
            # permute[!]
            p = randperm(m)
            q = randperm(n)
            fullPAQ = Array(A)[p,q]
            @test permute(A, p, q) == sparse(Array(A[p,q]))
            @test permute!(similar(A), A, p, q) == fullPAQ
            @test permute!(similar(A), A, p, q, similar(At)) == fullPAQ
            @test permute!(copy(A), p, q) == fullPAQ
            @test permute!(copy(A), p, q, similar(At)) == fullPAQ
            @test permute!(copy(A), p, q, similar(At), similar(A.colptr)) == fullPAQ
        end
    end
end

@testset "transpose of SubArrays" begin
    A = view(sprandn(10, 10, 0.3), 1:4, 1:4)
    @test  transpose(Array(A)) == Array(transpose(A))
    @test ctranspose(Array(A)) == Array(ctranspose(A))
end

@testset "exp" begin
    A = sprandn(5,5,0.2)
    @test e.^A ≈ e.^Array(A)
end

@testset "reductions" begin
    pA = sparse(rand(3, 7))

    for arr in (se33, sA, pA)
        for f in (sum, prod, minimum, maximum, var)
            farr = Array(arr)
            @test f(arr) ≈ f(farr)
            @test f(arr, 1) ≈ f(farr, 1)
            @test f(arr, 2) ≈ f(farr, 2)
            @test f(arr, (1, 2)) ≈ [f(farr)]
            @test isequal(f(arr, 3), f(farr, 3))
        end
    end

    for f in (sum, prod, minimum, maximum)
        # Test with a map function that maps to non-zero
        for arr in (se33, sA, pA)
            @test f(x->x+1, arr) ≈ f(arr+1)
        end

        # case where f(0) would throw
        @test f(x->sqrt(x-1), pA+1) ≈ f(sqrt.(pA))
        # these actually throw due to #10533
        # @test f(x->sqrt(x-1), pA+1, 1) ≈ f(sqrt(pA), 1)
        # @test f(x->sqrt(x-1), pA+1, 2) ≈ f(sqrt(pA), 2)
        # @test f(x->sqrt(x-1), pA+1, 3) ≈ f(pA)
    end

    @testset "empty cases" begin
        @test sum(sparse(Int[])) === 0
        @test prod(sparse(Int[])) === 1
        @test_throws ArgumentError minimum(sparse(Int[]))
        @test_throws ArgumentError maximum(sparse(Int[]))
        @test var(sparse(Int[])) === NaN

        for f in (sum, prod, minimum, maximum, var)
            @test isequal(f(spzeros(0, 1), 1), f(Array{Int}(0, 1), 1))
            @test isequal(f(spzeros(0, 1), 2), f(Array{Int}(0, 1), 2))
            @test isequal(f(spzeros(0, 1), (1, 2)), f(Array{Int}(0, 1), (1, 2)))
            @test isequal(f(spzeros(0, 1), 3), f(Array{Int}(0, 1), 3))
        end
    end
end

@testset "construction of diagonal SparseMatrixCSCs" begin
    @test Array(spdiagm((ones(2), ones(2)), (0, -1), 3, 3)) ==
                           [1.0  0.0  0.0; 1.0  1.0  0.0;  0.0  1.0  0.0]
    @test Array(spdiagm(ones(2), -1, 3, 3)) == diagm(ones(2), -1)
end

@testset "issue #4986, reinterpret" begin
    sfe22 = speye(Float64, 2)
    mfe22 = eye(Float64, 2)
    @test reinterpret(Int64, sfe22) == reinterpret(Int64, mfe22)
end

@testset "issue #5190" begin
    @test_throws ArgumentError sparsevec([3,5,7],[0.1,0.0,3.2],4)
end

@testset "issue #5386" begin
    K,J,V = findnz(SparseMatrixCSC(2,1,[1,3],[1,2],[1.0,0.0]))
    @test length(K) == length(J) == length(V) == 1
end

@testset "issue described in https://groups.google.com/d/msg/julia-users/Yq4dh8NOWBQ/GU57L90FZ3EJ" begin
    A = speye(Bool, 5)
    @test find(A) == find(x -> x == true, A) == find(Array(A))
end

@testset "issue #5824" begin
    @test sprand(4,5,0.5).^0 == sparse(ones(4,5))
end

@testset "issue #5985" begin
    @test sprand(Bool, 4, 5, 0.0) == sparse(zeros(Bool, 4, 5))
    @test sprand(Bool, 4, 5, 1.00) == sparse(ones(Bool, 4, 5))
    sprb45nnzs = zeros(5)
    for i=1:5
        sprb45 = sprand(Bool, 4, 5, 0.5)
        @test length(sprb45) == 20
        sprb45nnzs[i] = sum(sprb45)[1]
    end
    @test 4 <= mean(sprb45nnzs) <= 16
end

@testset "issue #5853, sparse diff" begin
    for i=1:2, a=Any[[1 2 3], reshape([1, 2, 3],(3,1)), eye(3)]
        @test all(diff(sparse(a),i) == diff(a,i))
    end
end

@testset "access to undefined error types that initially allocate elements as #undef" begin
    @test all(sparse(1:2, 1:2, Number[1,2])^2 == sparse(1:2, 1:2, [1,4]))
    sd1 = diff(sparse([1,1,1], [1,2,3], Number[1,2,3]), 1)
end

@testset "issue #6036" begin
    P = spzeros(Float64, 3, 3)
    for i = 1:3
        P[i,i] = i
    end

    @test minimum(P) === 0.0
    @test maximum(P) === 3.0
    @test minimum(-P) === -3.0
    @test maximum(-P) === 0.0

    @test maximum(P, (1,)) == [1.0 2.0 3.0]
    @test maximum(P, (2,)) == reshape([1.0,2.0,3.0],3,1)
    @test maximum(P, (1,2)) == reshape([3.0],1,1)

    @test maximum(sparse(-ones(3,3))) == -1
    @test minimum(sparse(ones(3,3))) == 1
end

@testset "unary functions" begin
    A = sprand(5, 15, 0.5)
    C = A + im*A
    Afull = Array(A)
    Cfull = Array(C)
    # Test representatives of [unary functions that map zeros to zeros and may map nonzeros to zeros]
    @test sin.(Afull) == Array(sin.(A))
    @test tan.(Afull) == Array(tan.(A)) # should be redundant with sin test
    @test ceil.(Afull) == Array(ceil.(A))
    @test floor.(Afull) == Array(floor.(A)) # should be redundant with ceil test
    @test real.(Afull) == Array(real.(A))
    @test imag.(Afull) == Array(imag.(A))
    @test real.(Cfull) == Array(real.(C))
    @test imag.(Cfull) == Array(imag.(C))
    # Test representatives of [unary functions that map zeros to zeros and nonzeros to nonzeros]
    @test expm1.(Afull) == Array(expm1.(A))
    @test abs.(Afull) == Array(abs.(A))
    @test abs2.(Afull) == Array(abs2.(A))
    @test abs.(Cfull) == Array(abs.(C))
    @test abs2.(Cfull) == Array(abs2.(C))
    # Test representatives of [unary functions that map both zeros and nonzeros to nonzeros]
    @test cos.(Afull) == Array(cos.(A))
    # Test representatives of remaining vectorized-nonbroadcast unary functions
    @test ceil.(Int, Afull) == Array(ceil.(Int, A))
    @test floor.(Int, Afull) == Array(floor.(Int, A))
    # Tests of real, imag, abs, and abs2 for SparseMatrixCSC{Int,X}s previously elsewhere
    for T in (Int, Float16, Float32, Float64, BigInt, BigFloat)
        R = rand(T[1:100;], 2, 2)
        I = rand(T[1:100;], 2, 2)
        D = R + I*im
        S = sparse(D)
        @test R == real.(S)
        @test I == imag.(S)
        @test real.(sparse(R)) == R
        @test nnz(imag.(sparse(R))) == 0
        @test abs.(S) == abs.(D)
        @test abs2.(S) == abs2.(D)
    end
end

@testset "getindex" begin
    ni = 23
    nj = 32
    a116 = reshape(1:(ni*nj), ni, nj)
    s116 = sparse(a116)

    ad116 = diagm(diag(a116))
    sd116 = sparse(ad116)

    for (aa116, ss116) in [(a116, s116), (ad116, sd116)]
        ij=11; i=3; j=2
        @test ss116[ij] == aa116[ij]
        @test ss116[(i,j)] == aa116[i,j]
        @test ss116[i,j] == aa116[i,j]
        @test ss116[i-1,j] == aa116[i-1,j]
        ss116[i,j] = 0
        @test ss116[i,j] == 0
        ss116 = sparse(aa116)

        @test ss116[:,:] == copy(ss116)

        # range indexing
        @test Array(ss116[i,:]) == aa116[i,:]
        @test Array(ss116[:,j]) == aa116[:,j]
        @test Array(ss116[i,1:2:end]) == aa116[i,1:2:end]
        @test Array(ss116[1:2:end,j]) == aa116[1:2:end,j]
        @test Array(ss116[i,end:-2:1]) == aa116[i,end:-2:1]
        @test Array(ss116[end:-2:1,j]) == aa116[end:-2:1,j]
        # float-range indexing is not supported

        # sorted vector indexing
        @test Array(ss116[i,[3:2:end-3;]]) == aa116[i,[3:2:end-3;]]
        @test Array(ss116[[3:2:end-3;],j]) == aa116[[3:2:end-3;],j]
        @test Array(ss116[i,[end-3:-2:1;]]) == aa116[i,[end-3:-2:1;]]
        @test Array(ss116[[end-3:-2:1;],j]) == aa116[[end-3:-2:1;],j]

        # unsorted vector indexing with repetition
        p = [4, 1, 2, 3, 2, 6]
        @test Array(ss116[p,:]) == aa116[p,:]
        @test Array(ss116[:,p]) == aa116[:,p]
        @test Array(ss116[p,p]) == aa116[p,p]

        # bool indexing
        li = bitrand(size(aa116,1))
        lj = bitrand(size(aa116,2))
        @test Array(ss116[li,j]) == aa116[li,j]
        @test Array(ss116[li,:]) == aa116[li,:]
        @test Array(ss116[i,lj]) == aa116[i,lj]
        @test Array(ss116[:,lj]) == aa116[:,lj]
        @test Array(ss116[li,lj]) == aa116[li,lj]

        # empty indices
        for empty in (1:0, Int[])
            @test Array(ss116[empty,:]) == aa116[empty,:]
            @test Array(ss116[:,empty]) == aa116[:,empty]
            @test Array(ss116[empty,lj]) == aa116[empty,lj]
            @test Array(ss116[li,empty]) == aa116[li,empty]
            @test Array(ss116[empty,empty]) == aa116[empty,empty]
        end

        # out of bounds indexing
        @test_throws BoundsError ss116[0, 1]
        @test_throws BoundsError ss116[end+1, 1]
        @test_throws BoundsError ss116[1, 0]
        @test_throws BoundsError ss116[1, end+1]
        for j in (1, 1:size(s116,2), 1:1, Int[1], trues(size(s116, 2)), 1:0, Int[])
            @test_throws BoundsError ss116[0:1, j]
            @test_throws BoundsError ss116[[0, 1], j]
            @test_throws BoundsError ss116[end:end+1, j]
            @test_throws BoundsError ss116[[end, end+1], j]
        end
        for i in (1, 1:size(s116,1), 1:1, Int[1], trues(size(s116, 1)), 1:0, Int[])
            @test_throws BoundsError ss116[i, 0:1]
            @test_throws BoundsError ss116[i, [0, 1]]
            @test_throws BoundsError ss116[i, end:end+1]
            @test_throws BoundsError ss116[i, [end, end+1]]
        end
    end

    # workaround issue #7197: comment out let-block
    #let S = SparseMatrixCSC(3, 3, UInt8[1,1,1,1], UInt8[], Int64[])
    S1290 = SparseMatrixCSC(3, 3, UInt8[1,1,1,1], UInt8[], Int64[])
        S1290[1,1] = 1
        S1290[5] = 2
        S1290[end] = 3
        @test S1290[end] == (S1290[1] + S1290[2,2])
        @test 6 == sum(diag(S1290))
        @test Array(S1290)[[3,1],1] == Array(S1290[[3,1],1])
    # end
end

@testset "setindex" begin
    a = spzeros(Int, 10, 10)
    @test countnz(a) == 0
    a[1,:] = 1
    @test countnz(a) == 10
    @test a[1,:] == sparse(ones(Int,10))
    a[:,2] = 2
    @test countnz(a) == 19
    @test a[:,2] == 2*sparse(ones(Int,10))
    b = copy(a)

    # Zero-assignment behavior of setindex!(A, v, i, j)
    a[1,3] = 0
    @test nnz(a) == 19
    @test countnz(a) == 18
    a[2,1] = 0
    @test nnz(a) == 19
    @test countnz(a) == 18

    # Zero-assignment behavior of setindex!(A, v, I, J)
    a[1,:] = 0
    @test nnz(a) == 19
    @test countnz(a) == 9
    a[2,:] = 0
    @test nnz(a) == 19
    @test countnz(a) == 8
    a[:,1] = 0
    @test nnz(a) == 19
    @test countnz(a) == 8
    a[:,2] = 0
    @test nnz(a) == 19
    @test countnz(a) == 0
    a = copy(b)
    a[:,:] = 0
    @test nnz(a) == 19
    @test countnz(a) == 0

    # Zero-assignment behavior of setindex!(A, B::SparseMatrixCSC, I, J)
    a = copy(b)
    a[1:2,:] = spzeros(2, 10)
    @test nnz(a) == 19
    @test countnz(a) == 8
    a[1:2,1:3] = sparse([1 0 1; 0 0 1])
    @test nnz(a) == 20
    @test countnz(a) == 11
    a = copy(b)
    a[1:2,:] = let c = sparse(ones(2,10)); fill!(c.nzval, 0); c; end
    @test nnz(a) == 19
    @test countnz(a) == 8
    a[1:2,1:3] = let c = sparse(ones(2,3)); c[1,2] = c[2,1] = c[2,2] = 0; c; end
    @test nnz(a) == 20
    @test countnz(a) == 11

    a[1,:] = 1:10
    @test a[1,:] == sparse([1:10;])
    a[:,2] = 1:10
    @test a[:,2] == sparse([1:10;])

    a[1,1:0] = []
    @test a[1,:] == sparse([1; 1; 3:10])
    a[1:0,2] = []
    @test a[:,2] == sparse([1:10;])
    a[1,1:0] = 0
    @test a[1,:] == sparse([1; 1; 3:10])
    a[1:0,2] = 0
    @test a[:,2] == sparse([1:10;])
    a[1,1:0] = 1
    @test a[1,:] == sparse([1; 1; 3:10])
    a[1:0,2] = 1
    @test a[:,2] == sparse([1:10;])

    @test_throws BoundsError a[:,11] = spzeros(10,1)
    @test_throws BoundsError a[11,:] = spzeros(1,10)
    @test_throws BoundsError a[:,-1] = spzeros(10,1)
    @test_throws BoundsError a[-1,:] = spzeros(1,10)
    @test_throws BoundsError a[0:9] = spzeros(1,10)
    @test_throws BoundsError a[:,11] = 0
    @test_throws BoundsError a[11,:] = 0
    @test_throws BoundsError a[:,-1] = 0
    @test_throws BoundsError a[-1,:] = 0
    @test_throws BoundsError a[0:9] = 0
    @test_throws BoundsError a[:,11] = 1
    @test_throws BoundsError a[11,:] = 1
    @test_throws BoundsError a[:,-1] = 1
    @test_throws BoundsError a[-1,:] = 1
    @test_throws BoundsError a[0:9] = 1

    @test_throws DimensionMismatch a[1:2,1:2] = 1:3
    @test_throws DimensionMismatch a[1:2,1] = 1:3
    @test_throws DimensionMismatch a[1,1:2] = 1:3
    @test_throws DimensionMismatch a[1:2] = 1:3

    A = spzeros(Int, 10, 20)
    A[1:5,1:10] = 10
    A[1:5,1:10] = 10
    @test countnz(A) == 50
    @test A[1:5,1:10] == 10 * ones(Int, 5, 10)
    A[6:10,11:20] = 0
    @test countnz(A) == 50
    A[6:10,11:20] = 20
    @test countnz(A) == 100
    @test A[6:10,11:20] == 20 * ones(Int, 5, 10)
    A[4:8,8:16] = 15
    @test countnz(A) == 121
    @test A[4:8,8:16] == 15 * ones(Int, 5, 9)

    ASZ = 1000
    TSZ = 800
    A = sprand(ASZ, 2*ASZ, 0.0001)
    B = copy(A)
    nA = countnz(A)
    x = A[1:TSZ, 1:(2*TSZ)]
    nx = countnz(x)
    A[1:TSZ, 1:(2*TSZ)] = 0
    nB = countnz(A)
    @test nB == (nA - nx)
    A[1:TSZ, 1:(2*TSZ)] = x
    @test countnz(A) == nA
    @test A == B
    A[1:TSZ, 1:(2*TSZ)] = 10
    @test countnz(A) == nB + 2*TSZ*TSZ
    A[1:TSZ, 1:(2*TSZ)] = x
    @test countnz(A) == nA
    @test A == B

    A = speye(Int, 5)
    I=1:10
    X=reshape([trues(10); falses(15)],5,5)
    @test A[I] == A[X] == [1,0,0,0,0,0,1,0,0,0]
    A[I] = [1:10;]
    @test A[I] == A[X] == collect(1:10)
    A[I] = zeros(Int, 10)
    @test nnz(A) == 13
    @test countnz(A) == 3
    @test A[I] == A[X] == zeros(Int, 10)
    c = collect(11:20); c[1] = c[3] = 0
    A[I] = c
    @test nnz(A) == 13
    @test countnz(A) == 11
    @test A[I] == A[X] == c
    A = speye(Int, 5)
    A[I] = c
    @test nnz(A) == 12
    @test countnz(A) == 11
    @test A[I] == A[X] == c

    S = sprand(50, 30, 0.5, x -> round.(Int, rand(x) * 100))
    I = sprand(Bool, 50, 30, 0.2)
    FS = Array(S)
    FI = Array(I)
    @test sparse(FS[FI]) == S[I] == S[FI]
    @test sum(S[FI]) + sum(S[!FI]) == sum(S)
    @test countnz(I) == count(I)

    sumS1 = sum(S)
    sumFI = sum(S[FI])
    nnzS1 = nnz(S)
    S[FI] = 0
    sumS2 = sum(S)
    cnzS2 = countnz(S)
    @test sum(S[FI]) == 0
    @test nnz(S) == nnzS1
    @test (sum(S) + sumFI) == sumS1

    S[FI] = 10
    nnzS3 = nnz(S)
    @test sum(S) == sumS2 + 10*sum(FI)
    S[FI] = 0
    @test sum(S) == sumS2
    @test nnz(S) == nnzS3
    @test countnz(S) == cnzS2

    S[FI] = [1:sum(FI);]
    @test sum(S) == sumS2 + sum(1:sum(FI))

    S = sprand(50, 30, 0.5, x -> round.(Int, rand(x) * 100))
    N = length(S) >> 2
    I = randperm(N) .* 4
    J = randperm(N)
    sumS1 = sum(S)
    sumS2 = sum(S[I])
    S[I] = 0
    @test sum(S) == (sumS1 - sumS2)
    S[I] = J
    @test sum(S) == (sumS1 - sumS2 + sum(J))
end

@testset "dropstored!" begin
    A = spzeros(Int, 10, 10)
    # Introduce nonzeros in row and column two
    A[1,:] = 1
    A[:,2] = 2
    @test nnz(A) == 19

    # Test argument bounds checking for dropstored!(A, i, j)
    @test_throws BoundsError Base.SparseArrays.dropstored!(A, 0, 1)
    @test_throws BoundsError Base.SparseArrays.dropstored!(A, 1, 0)
    @test_throws BoundsError Base.SparseArrays.dropstored!(A, 1, 11)
    @test_throws BoundsError Base.SparseArrays.dropstored!(A, 11, 1)

    # Test argument bounds checking for dropstored!(A, I, J)
    @test_throws BoundsError Base.SparseArrays.dropstored!(A, 0:1, 1:1)
    @test_throws BoundsError Base.SparseArrays.dropstored!(A, 1:1, 0:1)
    @test_throws BoundsError Base.SparseArrays.dropstored!(A, 10:11, 1:1)
    @test_throws BoundsError Base.SparseArrays.dropstored!(A, 1:1, 10:11)

    # Test behavior of dropstored!(A, i, j)
    # --> Test dropping a single stored entry
    Base.SparseArrays.dropstored!(A, 1, 2)
    @test nnz(A) == 18
    # --> Test dropping a single nonstored entry
    Base.SparseArrays.dropstored!(A, 2, 1)
    @test nnz(A) == 18

    # Test behavior of dropstored!(A, I, J) and derivs.
    # --> Test dropping a single row including stored and nonstored entries
    Base.SparseArrays.dropstored!(A, 1, :)
    @test nnz(A) == 9
    # --> Test dropping a single column including stored and nonstored entries
    Base.SparseArrays.dropstored!(A, :, 2)
    @test nnz(A) == 0
    # --> Introduce nonzeros in rows one and two and columns two and three
    A[1:2,:] = 1
    A[:,2:3] = 2
    @test nnz(A) == 36
    # --> Test dropping multiple rows containing stored and nonstored entries
    Base.SparseArrays.dropstored!(A, 1:3, :)
    @test nnz(A) == 14
    # --> Test dropping multiple columns containing stored and nonstored entries
    Base.SparseArrays.dropstored!(A, :, 2:4)
    @test nnz(A) == 0
    # --> Introduce nonzeros in every other row
    A[1:2:9, :] = 1
    @test nnz(A) == 50
    # --> Test dropping a block of the matrix towards the upper left
    Base.SparseArrays.dropstored!(A, 2:5, 2:5)
    @test nnz(A) == 42
end

@testset "issue #7507" begin
    @test (i7507=sparsevec(Dict{Int64, Float64}(), 10))==spzeros(10)
end

@testset "issue #7650" begin
    S = spzeros(3, 3)
    @test size(reshape(S, 9, 1)) == (9,1)
end

@testset "sparsevec from matrices" begin
    X = eye(5)
    M = rand(5,4)
    C = spzeros(3,3)
    SX = sparse(X); SM = sparse(M)
    VX = vec(X); VSX = vec(SX)
    VM = vec(M); VSM1 = vec(SM); VSM2 = sparsevec(M)
    VC = vec(C)
    @test VX == VSX
    @test VM == VSM1
    @test VM == VSM2
    @test size(VC) == (9,)
    @test nnz(VC) == 0
    @test nnz(VSX) == 5
end

@testset "issue #7677" begin
    A = sprand(5,5,0.5,(n)->rand(Float64,n))
    ACPY = copy(A)
    B = reshape(A,25,1)
    @test A == ACPY
    C = reinterpret(Int64, A, (25, 1))
    @test A == ACPY
    D = reinterpret(Int64, copy(B))
    @test C == D
end

@testset "issue #8225" begin
    @test_throws ArgumentError sparse([0],[-1],[1.0],2,2)
end

@testset "issue #8363" begin
    @test_throws ArgumentError sparsevec(Dict(-1=>1,1=>2))
end

@testset "issue #8976" begin
    @test conj.(sparse([1im])) == sparse(conj([1im]))
    @test conj!(sparse([1im])) == sparse(conj!([1im]))
end

@testset "issue #9525" begin
    @test_throws ArgumentError sparse([3], [5], 1.0, 3, 3)
end

@testset "indmax, indmin, findmax, findmin" begin
    S = sprand(100,80, 0.5)
    A = Array(S)
    @test indmax(S) == indmax(A)
    @test indmin(S) == indmin(A)
    @test findmin(S) == findmin(A)
    @test findmax(S) == findmax(A)
    for region in [(1,), (2,), (1,2)], m in [findmax, findmin]
        @test m(S, region) == m(A, region)
    end

    S = spzeros(10,8)
    A = Array(S)
    @test indmax(S) == indmax(A) == 1
    @test indmin(S) == indmin(A) == 1

    A = Array{Int}(0,0)
    S = sparse(A)
    iA = try indmax(A) end
    iS = try indmax(S) end
    @test iA === iS === nothing
    iA = try indmin(A) end
    iS = try indmin(S) end
    @test iA === iS === nothing
end

@testset "findn" begin
    b = findn( speye(4) )
    @test (length(b[1]) == 4)
    @test (length(b[2]) == 4)
end

@testset "rotations" begin
    a = sparse( [1,1,2,3], [1,3,4,1], [1,2,3,4] )

    @test rot180(a,2) == a
    @test rot180(a,1) == sparse( [3,3,2,1], [4,2,1,4], [1,2,3,4] )
    @test rotr90(a,1) == sparse( [1,3,4,1], [3,3,2,1], [1,2,3,4] )
    @test rotl90(a,1) == sparse( [4,2,1,4], [1,1,2,3], [1,2,3,4] )
    @test rotl90(a,2) == rot180(a)
    @test rotr90(a,2) == rot180(a)
    @test rotl90(a,3) == rotr90(a)
    @test rotr90(a,3) == rotl90(a)

    #ensure we have preserved the correct dimensions!

    a = speye(3,5)
    @test size(rot180(a)) == (3,5)
    @test size(rotr90(a)) == (5,3)
    @test size(rotl90(a)) == (5,3)
end

function test_getindex_algs{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, I::AbstractVector, J::AbstractVector, alg::Int)
    # Sorted vectors for indexing rows.
    # Similar to getindex_general but without the transpose trick.
    (m, n) = size(A)
    !isempty(I) && ((I[1] < 1) || (I[end] > m)) && BoundsError()
    if !isempty(J)
        minj, maxj = extrema(J)
        ((minj < 1) || (maxj > n)) && BoundsError()
    end

    (alg == 0) ? Base.SparseArrays.getindex_I_sorted_bsearch_A(A, I, J) :
    (alg == 1) ? Base.SparseArrays.getindex_I_sorted_bsearch_I(A, I, J) :
    Base.SparseArrays.getindex_I_sorted_linear(A, I, J)
end

@testset "test_getindex_algs" begin
    M=2^14
    N=2^4
    Irand = randperm(M)
    Jrand = randperm(N)
    SA = [sprand(M, N, d) for d in [1., 0.1, 0.01, 0.001, 0.0001, 0.]]
    IA = [sort(Irand[1:round(Int,n)]) for n in [M, M*0.1, M*0.01, M*0.001, M*0.0001, 0.]]
    debug = false

    if debug
        println("row sizes: $([round(Int,nnz(S)/S.n) for S in SA])")
        println("I sizes: $([length(I) for I in IA])")
        @printf("    S    |    I    | binary S | binary I |  linear  | best\n")
    end

    J = Jrand
    for I in IA
        for S in SA
            res = Any[1,2,3]
            times = Float64[0,0,0]
            best = [typemax(Float64), 0]
            for searchtype in [0, 1, 2]
                gc()
                tres = @timed test_getindex_algs(S, I, J, searchtype)
                res[searchtype+1] = tres[1]
                times[searchtype+1] = tres[2]
                if best[1] > tres[2]
                    best[1] = tres[2]
                    best[2] = searchtype
                end
            end

            if debug
                @printf(" %7d | %7d | %4.2e | %4.2e | %4.2e | %s\n", round(Int,nnz(S)/S.n), length(I), times[1], times[2], times[3],
                            (0 == best[2]) ? "binary S" : (1 == best[2]) ? "binary I" : "linear")
            end
            if res[1] != res[2]
                println("1 and 2")
            elseif res[2] != res[3]
                println("2, 3")
            end
            @test res[1] == res[2] == res[3]
        end
    end

    M = 2^8
    N=2^3
    Irand = randperm(M)
    Jrand = randperm(N)
    I = sort([Irand; Irand; Irand])
    J = [Jrand; Jrand]

    SA = [sprand(M, N, d) for d in [1., 0.1, 0.01, 0.001, 0.0001, 0.]]
    for S in SA
        res = Any[1,2,3]
        for searchtype in [0, 1, 2]
            res[searchtype+1] = test_getindex_algs(S, I, J, searchtype)
        end

        @test res[1] == res[2] == res[3]
    end

    M = 2^14
    N=2^4
    I = randperm(M)
    J = randperm(N)
    Jsorted = sort(J)

    SA = [sprand(M, N, d) for d in [1., 0.1, 0.01, 0.001, 0.0001, 0.]]
    IA = [I[1:round(Int,n)] for n in [M, M*0.1, M*0.01, M*0.001, M*0.0001, 0.]]
    debug = false
    if debug
        @printf("         |         |         |        times        |        memory       |\n")
        @printf("    S    |    I    |    J    |  sorted  | unsorted |  sorted  | unsorted |\n")
    end
    for I in IA
        Isorted = sort(I)
        for S in SA
            gc()
            ru = @timed S[I, J]
            gc()
            rs = @timed S[Isorted, Jsorted]
            if debug
                @printf(" %7d | %7d | %7d | %4.2e | %4.2e | %4.2e | %4.2e |\n", round(Int,nnz(S)/S.n), length(I), length(J), rs[2], ru[2], rs[3], ru[3])
            end
        end
    end
end

@testset "getindex bounds checking" begin
    S = sprand(10, 10, 0.1)
    @test_throws BoundsError S[[0,1,2], [1,2]]
    @test_throws BoundsError S[[1,2], [0,1,2]]
    @test_throws BoundsError S[[0,2,1], [1,2]]
    @test_throws BoundsError S[[2,1], [0,1,2]]
end

@testset "test that sparse / sparsevec constructors work for AbstractMatrix subtypes" begin
    D = Diagonal(ones(10,10))
    sm = sparse(D)
    sv = sparsevec(D)

    @test countnz(sm) == 10
    @test countnz(sv) == 10

    @test countnz(sparse(Diagonal(Int[]))) == 0
    @test countnz(sparsevec(Diagonal(Int[]))) == 0
end

@testset "explicit zeros" begin
    if Base.USE_GPL_LIBS
        a = SparseMatrixCSC(2, 2, [1, 3, 5], [1, 2, 1, 2], [1.0, 0.0, 0.0, 1.0])
        @test lufact(a)\[2.0, 3.0] ≈ [2.0, 3.0]
        @test cholfact(a)\[2.0, 3.0] ≈ [2.0, 3.0]
    end
end

@testset "issue #9917" begin
    @test sparse([]') == reshape(sparse([]), 1, 0)
    @test Array(sparse([])) == zeros(0)
    @test_throws BoundsError sparse([])[1]
    @test_throws BoundsError sparse([])[1] = 1
    x = speye(100)
    @test_throws BoundsError x[-10:10]
end

@testset "issue #10407" begin
    @test maximum(spzeros(5, 5)) == 0.0
    @test minimum(spzeros(5, 5)) == 0.0
end

@testset "issue #10411" begin
    for (m,n) in ((2,-2),(-2,2),(-2,-2))
        @test_throws ArgumentError spzeros(m,n)
        @test_throws ArgumentError speye(m,n)
        @test_throws ArgumentError sprand(m,n,0.2)
    end
end

@testset "issue #10837, sparse constructors from special matrices" begin
    T = Tridiagonal(randn(4),randn(5),randn(4))
    S = sparse(T)
    @test norm(Array(T) - Array(S)) == 0.0
    T = SymTridiagonal(randn(5),rand(4))
    S = sparse(T)
    @test norm(Array(T) - Array(S)) == 0.0
    B = Bidiagonal(randn(5),randn(4),true)
    S = sparse(B)
    @test norm(Array(B) - Array(S)) == 0.0
    B = Bidiagonal(randn(5),randn(4),false)
    S = sparse(B)
    @test norm(Array(B) - Array(S)) == 0.0
end

@testset "spdiagm promotion" begin
    @test spdiagm(([1,2],[3.5],[4+5im]), (0,1,-1), 2,2) == [1 3.5; 4+5im 2]
end

@testset "error conditions for reinterpret, reshape, and squeeze" begin
    A = sprand(Bool, 5,5,0.2)
    @test_throws ArgumentError reinterpret(Complex128,A)
    @test_throws ArgumentError reinterpret(Complex128,A,(5,5))
    @test_throws DimensionMismatch reinterpret(Int8,A,(20,))
    @test_throws DimensionMismatch reshape(A,(20,2))
    @test_throws ArgumentError squeeze(A,(1,1))
end

@testset "similar with type conversion" begin
    A = speye(5)
    @test size(similar(A,Complex128,Int)) == (5,5)
    @test typeof(similar(A,Complex128,Int)) == SparseMatrixCSC{Complex128,Int}
    @test size(similar(A,Complex128,Int8)) == (5,5)
    @test typeof(similar(A,Complex128,Int8)) == SparseMatrixCSC{Complex128,Int8}
    @test similar(A,Complex128,(6,6)) == spzeros(Complex128,6,6)
    @test convert(Matrix,A) == Array(A)
end

@testset "float" begin
    A = sprand(Bool, 5,5,0.0)
    @test eltype(float(A)) == Float64  # issue #11658
    A = sprand(Bool, 5,5,0.2)
    @test float(A) == float(Array(A))
end

@testset "sparsevec" begin
    A = sparse(ones(5,5))
    @test all(Array(sparsevec(A)) .== ones(25))
    @test all(Array(sparsevec([1:5;],1)) .== ones(5))
    @test_throws ArgumentError sparsevec([1:5;], [1:4;])
end

@testset "sparse" begin
    A = sparse(ones(5,5))
    @test sparse(A) == A
    @test sparse([1:5;],[1:5;],1) == speye(5)
end

@testset "speye and one" begin
    A = sparse(ones(5,5))
    @test speye(A) == speye(5)
    @test eye(A) == speye(5)
    @test one(A) == speye(5)
    @test_throws DimensionMismatch one(sprand(5,6,0.2))
    @test eltype(speye(Real, 5, 5)) === Real
end

@testset "istriu/istril" begin
    A = sparse(triu(rand(5,5)))
    @test istriu(A)
    @test !istriu(sparse(ones(5,5)))
    A = sparse(tril(rand(5,5)))
    @test istril(A)
    @test !istril(sparse(ones(5,5)))
end

@testset "droptol" begin
    srand(1234321)
    A = triu(sprand(10,10,0.2))
    @test Base.droptol!(A,0.01).colptr == [1,1,1,2,2,3,4,6,6,7,9]
    @test isequal(Base.droptol!(sparse([1], [1], [1]), 1), SparseMatrixCSC(1,1,Int[1,1],Int[],Int[]))
end

@testset "dropzeros[!]" begin
    smalldim = 5
    largedim = 10
    nzprob = 0.4
    targetnumposzeros = 5
    targetnumnegzeros = 5
    for (m, n) in ((largedim, largedim), (smalldim, largedim), (largedim, smalldim))
        A = sprand(m, n, nzprob)
        struczerosA = find(x -> x == 0, A)
        poszerosinds = unique(rand(struczerosA, targetnumposzeros))
        negzerosinds = unique(rand(struczerosA, targetnumnegzeros))
        Aposzeros = setindex!(copy(A), 2, poszerosinds)
        Anegzeros = setindex!(copy(A), -2, negzerosinds)
        Abothsigns = setindex!(copy(Aposzeros), -2, negzerosinds)
        map!(x -> x == 2 ? 0.0 : x, Aposzeros.nzval, Aposzeros.nzval)
        map!(x -> x == -2 ? -0.0 : x, Anegzeros.nzval, Anegzeros.nzval)
        map!(x -> x == 2 ? 0.0 : x == -2 ? -0.0 : x, Abothsigns.nzval, Abothsigns.nzval)
        for Awithzeros in (Aposzeros, Anegzeros, Abothsigns)
            # Basic functionality / dropzeros!
            @test dropzeros!(copy(Awithzeros)) == A
            @test dropzeros!(copy(Awithzeros), false) == A
            # Basic functionality / dropzeros
            @test dropzeros(Awithzeros) == A
            @test dropzeros(Awithzeros, false) == A
            # Check trimming works as expected
            @test length(dropzeros!(copy(Awithzeros)).nzval) == length(A.nzval)
            @test length(dropzeros!(copy(Awithzeros)).rowval) == length(A.rowval)
            @test length(dropzeros!(copy(Awithzeros), false).nzval) == length(Awithzeros.nzval)
            @test length(dropzeros!(copy(Awithzeros), false).rowval) == length(Awithzeros.rowval)
        end
    end
    # original lone dropzeros test
    A = sparse([1 2 3; 4 5 6; 7 8 9])
    A.nzval[2] = A.nzval[6] = A.nzval[7] = 0
    @test dropzeros!(A).colptr == [1, 3, 5, 7]
    # test for issue #5169, modified for new behavior following #15242/#14798
    @test nnz(sparse([1, 1], [1, 2], [0.0, -0.0])) == 2
    @test nnz(dropzeros!(sparse([1, 1], [1, 2], [0.0, -0.0]))) == 0
    # test for issue #5437, modified for new behavior following #15242/#14798
    @test nnz(sparse([1, 2, 3], [1, 2, 3], [0.0, 1.0, 2.0])) == 3
    @test nnz(dropzeros!(sparse([1, 2, 3],[1, 2, 3],[0.0, 1.0, 2.0]))) == 2
end

@testset "trace" begin
    @test_throws DimensionMismatch trace(sparse(ones(5,6)))
    @test trace(speye(5)) == 5
end

@testset "diagm on a matrix" begin
    @test_throws DimensionMismatch diagm(sparse(ones(5,2)))
    @test_throws DimensionMismatch diagm(sparse(ones(2,5)))
    @test diagm(sparse(ones(1,5))) == speye(5)
    @test diagm(sparse(ones(5,1))) == speye(5)
end

@testset "expandptr" begin
    A = speye(5)
    @test Base.SparseArrays.expandptr(A.colptr) == collect(1:5)
    A[1,2] = 1
    @test Base.SparseArrays.expandptr(A.colptr) == [1; 2; 2; 3; 4; 5]
    @test_throws ArgumentError Base.SparseArrays.expandptr([2; 3])
end

@testset "triu/tril" begin
    A = sprand(5,5,0.2)
    AF = Array(A)
    @test Array(triu(A,1)) == triu(AF,1)
    @test Array(tril(A,1)) == tril(AF,1)
    @test Array(triu!(copy(A), 2)) == triu(AF,2)
    @test Array(tril!(copy(A), 2)) == tril(AF,2)
    @test_throws BoundsError tril(A,6)
    @test_throws BoundsError tril(A,-6)
    @test_throws BoundsError triu(A,6)
    @test_throws BoundsError triu(A,-6)
    @test_throws ArgumentError tril!(sparse([1,2,3], [1,2,3], [1,2,3], 3, 4), 4)
    @test_throws ArgumentError tril!(sparse([1,2,3], [1,2,3], [1,2,3], 3, 4), -3)
    @test_throws ArgumentError triu!(sparse([1,2,3], [1,2,3], [1,2,3], 3, 4), 4)
    @test_throws ArgumentError triu!(sparse([1,2,3], [1,2,3], [1,2,3], 3, 4), -3)

    # fkeep trim option
    @test isequal(length(tril!(sparse([1,2,3], [1,2,3], [1,2,3], 3, 4), -1).rowval), 0)
end

@testset "norm" begin
    A = sparse(Int[],Int[],Float64[],0,0)
    @test norm(A) == zero(eltype(A))
    A = sparse([1.0])
    @test norm(A) == 1.0
    @test_throws ArgumentError norm(sprand(5,5,0.2),3)
    @test_throws ArgumentError norm(sprand(5,5,0.2),2)
end

@testset "ishermitian/issymmetric" begin
    # real matrices
    A = speye(5,5)
    @test ishermitian(A) == true
    @test issymmetric(A) == true
    A[1,3] = 1.0
    @test ishermitian(A) == false
    @test issymmetric(A) == false
    A[3,1] = 1.0
    @test ishermitian(A) == true
    @test issymmetric(A) == true

    # complex matrices
    A = speye(5,5) + im*speye(5,5)
    @test ishermitian(A) == false
    @test issymmetric(A) == true
    A[1,4] = 1.0 + im
    @test ishermitian(A) == false
    @test issymmetric(A) == false

    A = speye(Complex128, 5,5)
    A[3,2] = 1.0 + im
    @test ishermitian(A) == false
    @test issymmetric(A) == false
    A[2,3] = 1.0 - im
    @test ishermitian(A) == true
    @test issymmetric(A) == false

    A = sparse(zeros(5,5))
    @test ishermitian(A) == true
    @test issymmetric(A) == true

    # explicit zeros
    A = speye(Complex128, 5,5)
    A[3,1] = 2
    A.nzval[2] = 0.0
    @test ishermitian(A) == true
    @test issymmetric(A) == true

    # 15504
    m = n = 5
    colptr = [1, 5, 9, 13, 13, 17]
    rowval = [1, 2, 3, 5, 1, 2, 3, 5, 1, 2, 3, 5, 1, 2, 3, 5]
    nzval = [0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 1.0, 1.0, 1.0, 0.0, 1.0, 1.0, 1.0]
    A = SparseMatrixCSC(m, n, colptr, rowval, nzval)
    @test issymmetric(A) == true
    A.nzval[end - 3]  = 2.0
    @test issymmetric(A) == false

    # 16521
    @test issymmetric(sparse([0 0; 1 0])) == false
    @test issymmetric(sparse([0 1; 0 0])) == false
    @test issymmetric(sparse([0 0; 1 1])) == false
    @test issymmetric(sparse([1 0; 1 0])) == false
    @test issymmetric(sparse([0 1; 1 0])) == true
    @test issymmetric(sparse([1 1; 1 0])) == true
end

@testset "equality ==" begin
    A1 = speye(10)
    A2 = speye(10)
    nonzeros(A1)[end]=0
    @test A1!=A2
    nonzeros(A1)[end]=1
    @test A1==A2
    A1[1:4,end] = 1
    @test A1!=A2
    nonzeros(A1)[end-4:end-1]=0
    @test A1==A2
    A2[1:4,end-1] = 1
    @test A1!=A2
    nonzeros(A2)[end-5:end-2]=0
    @test A1==A2
    A2[2:3,1] = 1
    @test A1!=A2
    nonzeros(A2)[2:3]=0
    @test A1==A2
    A1[2:5,1] = 1
    @test A1!=A2
    nonzeros(A1)[2:5]=0
    @test A1==A2
    @test sparse([1,1,0])!=sparse([0,1,1])
end

@testset "UniformScaling" begin
    A = sprandn(10,10,0.5)
    @test A + I == Array(A) + I
    @test I + A == I + Array(A)
    @test A - I == Array(A) - I
    @test I - A == I - Array(A)
end

@testset "issue #12177, error path if triplet vectors are not all the same length" begin
    @test_throws ArgumentError sparse([1,2,3], [1,2], [1,2,3], 3, 3)
    @test_throws ArgumentError sparse([1,2,3], [1,2,3], [1,2], 3, 3)
end

@testset "issue #12118: sparse matrices are closed under +, -, min, max" begin
    A12118 = sparse([1,2,3,4,5], [1,2,3,4,5], [1,2,3,4,5])
    B12118 = sparse([1,2,4,5],   [1,2,3,5],   [2,1,-1,-2])

    @test A12118 + B12118 == sparse([1,2,3,4,4,5], [1,2,3,3,4,5], [3,3,3,-1,4,3])
    @test typeof(A12118 + B12118) == SparseMatrixCSC{Int,Int}

    @test A12118 - B12118 == sparse([1,2,3,4,4,5], [1,2,3,3,4,5], [-1,1,3,1,4,7])
    @test typeof(A12118 - B12118) == SparseMatrixCSC{Int,Int}

    @test max.(A12118, B12118) == sparse([1,2,3,4,5], [1,2,3,4,5], [2,2,3,4,5])
    @test typeof(max.(A12118, B12118)) == SparseMatrixCSC{Int,Int}

    @test min.(A12118, B12118) == sparse([1,2,4,5], [1,2,3,5], [1,1,-1,-2])
    @test typeof(min.(A12118, B12118)) == SparseMatrixCSC{Int,Int}
end

@testset "sparse matrix norms" begin
    Ac = sprandn(10,10,.1) + im* sprandn(10,10,.1)
    Ar = sprandn(10,10,.1)
    Ai = ceil.(Int,Ar*100)
    @test norm(Ac,1) ≈ norm(Array(Ac),1)
    @test norm(Ac,Inf) ≈ norm(Array(Ac),Inf)
    @test vecnorm(Ac) ≈ vecnorm(Array(Ac))
    @test norm(Ar,1) ≈ norm(Array(Ar),1)
    @test norm(Ar,Inf) ≈ norm(Array(Ar),Inf)
    @test vecnorm(Ar) ≈ vecnorm(Array(Ar))
    @test norm(Ai,1) ≈ norm(Array(Ai),1)
    @test norm(Ai,Inf) ≈ norm(Array(Ai),Inf)
    @test vecnorm(Ai) ≈ vecnorm(Array(Ai))
    Ai = trunc.(Int, Ar*100)
    @test norm(Ai,1) ≈ norm(Array(Ai),1)
    @test norm(Ai,Inf) ≈ norm(Array(Ai),Inf)
    @test vecnorm(Ai) ≈ vecnorm(Array(Ai))
    Ai = round.(Int, Ar*100)
    @test norm(Ai,1) ≈ norm(Array(Ai),1)
    @test norm(Ai,Inf) ≈ norm(Array(Ai),Inf)
    @test vecnorm(Ai) ≈ vecnorm(Array(Ai))
end

@testset "sparse matrix cond" begin
    A = sparse(reshape([1.0],1,1))
    Ac = sprandn(20,20,.5) + im* sprandn(20,20,.5)
    Ar = sprandn(20,20,.5)
    @test cond(A,1) == 1.0
    # For a discussion of the tolerance, see #14778
    if Base.USE_GPL_LIBS
        @test 0.99 <= cond(Ar, 1) \ norm(Ar, 1) * norm(inv(Array(Ar)), 1) < 3
        @test 0.99 <= cond(Ac, 1) \ norm(Ac, 1) * norm(inv(Array(Ac)), 1) < 3
        @test 0.99 <= cond(Ar, Inf) \ norm(Ar, Inf) * norm(inv(Array(Ar)), Inf) < 3
        @test 0.99 <= cond(Ac, Inf) \ norm(Ac, Inf) * norm(inv(Array(Ac)), Inf) < 3
    end
    @test_throws ArgumentError cond(A,2)
    @test_throws ArgumentError cond(A,3)
    Arect = spzeros(10, 6)
    @test_throws DimensionMismatch cond(Arect, 1)
    @test_throws ArgumentError cond(Arect,2)
    @test_throws DimensionMismatch cond(Arect, Inf)
end

@testset "sparse matrix normestinv" begin
    Ac = sprandn(20,20,.5) + im* sprandn(20,20,.5)
    Aci = ceil.(Int64, 100*sprand(20,20,.5)) + im*ceil.(Int64, sprand(20,20,.5))
    Ar = sprandn(20,20,.5)
    Ari = ceil.(Int64, 100*Ar)
    if Base.USE_GPL_LIBS
        @test Base.SparseArrays.normestinv(Ac,3) ≈ norm(inv(Array(Ac)),1) atol=1e-4
        @test Base.SparseArrays.normestinv(Aci,3) ≈ norm(inv(Array(Aci)),1) atol=1e-4
        @test Base.SparseArrays.normestinv(Ar) ≈ norm(inv(Array(Ar)),1) atol=1e-4
        @test_throws ArgumentError Base.SparseArrays.normestinv(Ac,0)
        @test_throws ArgumentError Base.SparseArrays.normestinv(Ac,21)
    end
    @test_throws DimensionMismatch Base.SparseArrays.normestinv(sprand(3,5,.9))
end

@testset "issue #13008" begin
    @test_throws ArgumentError sparse(collect(1:100), collect(1:100), fill(5,100), 5, 5)
    @test_throws ArgumentError sparse(Int[], collect(1:5), collect(1:5))
end

@testset "issue #13024" begin
    A13024 = sparse([1,2,3,4,5], [1,2,3,4,5], fill(true,5))
    B13024 = sparse([1,2,4,5],   [1,2,3,5],   fill(true,4))

    @test broadcast(&, A13024, B13024) == sparse([1,2,5], [1,2,5], fill(true,3))
    @test typeof(broadcast(&, A13024, B13024)) == SparseMatrixCSC{Bool,Int}

    @test broadcast(|, A13024, B13024) == sparse([1,2,3,4,4,5], [1,2,3,3,4,5], fill(true,6))
    @test typeof(broadcast(|, A13024, B13024)) == SparseMatrixCSC{Bool,Int}

    @test broadcast(⊻, A13024, B13024) == sparse([3,4,4], [3,3,4], fill(true,3), 5, 5)
    @test typeof(broadcast(⊻, A13024, B13024)) == SparseMatrixCSC{Bool,Int}

    @test broadcast(max, A13024, B13024) == sparse([1,2,3,4,4,5], [1,2,3,3,4,5], fill(true,6))
    @test typeof(broadcast(max, A13024, B13024)) == SparseMatrixCSC{Bool,Int}

    @test broadcast(min, A13024, B13024) == sparse([1,2,5], [1,2,5], fill(true,3))
    @test typeof(broadcast(min, A13024, B13024)) == SparseMatrixCSC{Bool,Int}

    for op in (+, -)
        @test op(A13024, B13024) == op(Array(A13024), Array(B13024))
    end
    for op in (max, min, &, |, xor)
        @test op.(A13024, B13024) == op.(Array(A13024), Array(B13024))
    end
end

@testset "spones" begin
    A = 2. * speye(5,5)
    @test Array(spones(A)) == eye(Array(A))
end

@testset "factorization" begin
    A = spdiagm(rand(5)) + sprandn(5,5,0.2) + im*sprandn(5,5,0.2)
    A = A + A'
    @test !Base.USE_GPL_LIBS || abs(det(factorize(Hermitian(A)))) ≈ abs(det(factorize(Array(A))))
    A = spdiagm(rand(5)) + sprandn(5,5,0.2) + im*sprandn(5,5,0.2)
    A = A*A'
    @test !Base.USE_GPL_LIBS || abs(det(factorize(Hermitian(A)))) ≈ abs(det(factorize(Array(A))))
    A = spdiagm(rand(5)) + sprandn(5,5,0.2)
    A = A + A.'
    @test !Base.USE_GPL_LIBS || abs(det(factorize(Symmetric(A)))) ≈ abs(det(factorize(Array(A))))
    A = spdiagm(rand(5)) + sprandn(5,5,0.2)
    A = A*A.'
    @test !Base.USE_GPL_LIBS || abs(det(factorize(Symmetric(A)))) ≈ abs(det(factorize(Array(A))))
    @test factorize(triu(A)) == triu(A)
    @test isa(factorize(triu(A)), UpperTriangular{Float64, SparseMatrixCSC{Float64, Int}})
    @test factorize(tril(A)) == tril(A)
    @test isa(factorize(tril(A)), LowerTriangular{Float64, SparseMatrixCSC{Float64, Int}})
    @test !Base.USE_GPL_LIBS || factorize(A[:,1:4])\ones(size(A,1)) ≈ Array(A[:,1:4])\ones(size(A,1))
    @test_throws ErrorException chol(A)
    @test_throws ErrorException lu(A)
    @test_throws ErrorException eig(A)
    @test_throws ErrorException inv(A)
end

@testset "issue #13792, use sparse triangular solvers for sparse triangular solves" begin
    n = 100
    A = sprandn(n, n, 0.5) + sqrt(n)*I
    x = LowerTriangular(A)*ones(n)
    @test LowerTriangular(A)\x ≈ ones(n)
    x = UpperTriangular(A)*ones(n)
    @test UpperTriangular(A)\x ≈ ones(n)
    A[2,2] = 0
    dropzeros!(A)
    @test_throws LinAlg.SingularException LowerTriangular(A)\ones(n)
    @test_throws LinAlg.SingularException UpperTriangular(A)\ones(n)
end

@testset "issue described in https://groups.google.com/forum/#!topic/julia-dev/QT7qpIpgOaA" begin
    @test sparse([1,1], [1,1], [true, true]) == sparse([1,1], [1,1], [true, true], 1, 1) == fill(true, 1, 1)
    @test sparsevec([1,1], [true, true]) == sparsevec([1,1], [true, true], 1) == fill(true, 1)
end

@testset "issparse for specialized matrix types" begin
    m = sprand(10, 10, 0.1)
    @test issparse(Symmetric(m))
    @test issparse(Hermitian(m))
    @test issparse(LowerTriangular(m))
    @test issparse(LinAlg.UnitLowerTriangular(m))
    @test issparse(UpperTriangular(m))
    @test issparse(LinAlg.UnitUpperTriangular(m))
    @test issparse(Symmetric(Array(m))) == false
    @test issparse(Hermitian(Array(m))) == false
    @test issparse(LowerTriangular(Array(m))) == false
    @test issparse(LinAlg.UnitLowerTriangular(Array(m))) == false
    @test issparse(UpperTriangular(Array(m))) == false
    @test issparse(LinAlg.UnitUpperTriangular(Array(m))) == false
end

@testset "test created type of sprand{T}(::Type{T}, m::Integer, n::Integer, density::AbstractFloat)" begin
    m = sprand(Float32, 10, 10, 0.1)
    @test eltype(m) == Float32
    m = sprand(Float64, 10, 10, 0.1)
    @test eltype(m) == Float64
    m = sprand(Int32, 10, 10, 0.1)
    @test eltype(m) == Int32
end

@testset "issue #16073" begin
    @inferred sprand(1, 1, 1.0)
    @inferred sprand(1, 1, 1.0, rand, Float64)
    @inferred sprand(1, 1, 1.0, x -> round.(Int, rand(x) * 100))
end

# Test that concatenations of combinations of sparse matrices with sparse matrices or dense
# matrices/vectors yield sparse arrays
@testset "sparse and dense concatenations" begin
    N = 4
    densevec = ones(N)
    densemat = diagm(ones(N))
    spmat = spdiagm(ones(N))
    # Test that concatenations of pairs of sparse matrices yield sparse arrays
    @test issparse(vcat(spmat, spmat))
    @test issparse(hcat(spmat, spmat))
    @test issparse(hvcat((2,), spmat, spmat))
    @test issparse(cat((1,2), spmat, spmat))
    # Test that concatenations of a sparse matrice with a dense matrix/vector yield sparse arrays
    @test issparse(vcat(spmat, densemat))
    @test issparse(vcat(densemat, spmat))
    for densearg in (densevec, densemat)
        @test issparse(hcat(spmat, densearg))
        @test issparse(hcat(densearg, spmat))
        @test issparse(hvcat((2,), spmat, densearg))
        @test issparse(hvcat((2,), densearg, spmat))
        @test issparse(cat((1,2), spmat, densearg))
        @test issparse(cat((1,2), densearg, spmat))
    end
end

@testset "issue #14816" begin
    m = 5
    intmat = fill(1, m, m)
    ltintmat = LowerTriangular(rand(1:5, m, m))
    @test At_ldiv_B(ltintmat, sparse(intmat)) ≈ At_ldiv_B(ltintmat, intmat)
end

# Test temporary fix for issue #16548 in PR #16979. Brittle. Expect to remove with `\` revisions.
# This is broken by the introduction of RowVector... see brittle comment above.
#@testset "issue #16548" begin
#    @test which(\, (SparseMatrixCSC, AbstractVecOrMat)).module == Base.SparseArrays
#end

@testset "row indexing a SparseMatrixCSC with non-Int integer type" begin
    A = sparse(UInt32[1,2,3], UInt32[1,2,3], [1.0,2.0,3.0])
    @test A[1,1:3] == A[1,:] == [1,0,0]
end

# Check that `broadcast` methods specialized for unary operations over `SparseMatrixCSC`s
# are called. (Issue #18705.) EDIT: #19239 unified broadcast over a single sparse matrix,
# eliminating the former operation classes.
@testset "issue #18705" begin
    @test isa(sin.(spdiagm(1.0:5.0)), SparseMatrixCSC)
end

@testset "issue #19225" begin
    X = sparse([1 -1; -1 1])
    for T in (Symmetric, Hermitian)
        Y = T(copy(X))
        _Y = similar(Y)
        copy!(_Y, Y)
        @test _Y == Y

        W = T(copy(X), :L)
        copy!(W, Y)
        @test W.data == Y.data
        @test W.uplo != Y.uplo

        W[1,1] = 4
        @test W == T(sparse([4 -1; -1 1]))
        @test_throws ArgumentError (W[1,2] = 2)

        @test Y + I == T(sparse([2 -1; -1 2]))
        @test Y - I == T(sparse([0 -1; -1 0]))
        @test Y * I == Y

        @test Y + 1 == T(sparse([2 0; 0 2]))
        @test Y - 1 == T(sparse([0 -2; -2 0]))
        @test Y * 2 == T(sparse([2 -2; -2 2]))
        @test Y / 1 == Y
    end
end

@testset "issue #19304" begin
    @inferred hcat(sparse(rand(2,1)), eye(2,2))
end

# Check that `broadcast` methods specialized for unary operations over
# `SparseMatrixCSC`s determine a reasonable return type.
@testset "issue #18974" begin
    @test eltype(sin.(spdiagm(Int64(1):Int64(4)))) == Float64
end

# Check calling of unary minus method specialized for SparseMatrixCSCs
@testset "issue #19503" begin
    @test which(-, (SparseMatrixCSC,)).module == Base.SparseArrays
end

@testset "issue #14398" begin
    @test transpose(view(speye(10), 1:5, 1:5)) ≈ eye(5,5)
end

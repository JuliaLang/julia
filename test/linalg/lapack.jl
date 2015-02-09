using Base.Test
using Base.LAPACK.bdsqr!

let # syevr
    srand(123)
    Ainit = randn(5,5)
    for elty in (Float32, Float64, Complex64, Complex128)
        if elty == Complex64 || elty == Complex128
            A = complex(Ainit, Ainit)
        else
            A = Ainit
        end
        A = convert(Array{elty, 2}, A)
        Asym = A'A
        vals, Z = LAPACK.syevr!('V', copy(Asym))
        @test_approx_eq Z*scale(vals, Z') Asym
        @test all(vals .> 0.0)
        @test_approx_eq LAPACK.syevr!('N','V','U',copy(Asym),0.0,1.0,4,5,-1.0)[1] vals[vals .< 1.0]
        @test_approx_eq LAPACK.syevr!('N','I','U',copy(Asym),0.0,1.0,4,5,-1.0)[1] vals[4:5]
        @test_approx_eq vals LAPACK.syev!('N','U',copy(Asym))
    end
end

let # Test gglse
    for elty in (Float32, Float64, Complex64, Complex128)
        A = convert(Array{elty, 2}, [1 1 1 1; 1 3 1 1; 1 -1 3 1; 1 1 1 3; 1 1 1 -1])
        c = convert(Array{elty, 1}, [2, 1, 6, 3, 1])
        B = convert(Array{elty, 2}, [1 1 1 -1; 1 -1 1 1; 1 1 -1 1])
        d = convert(Array{elty, 1}, [1, 3, -1])
        @test_approx_eq LAPACK.gglse!(A, c, B, d)[1] convert(Array{elty}, [0.5, -0.5, 1.5, 0.5])
    end
end

let # xbdsqr
    n = 10
    for elty in (Float32, Float64)
        d, e = convert(Vector{elty}, randn(n)), convert(Vector{elty}, randn(n - 1))
        U, Vt, C = eye(elty, n), eye(elty, n), eye(elty, n)
        s, _ = bdsqr!('U', copy(d), copy(e), Vt, U, C)
        @test_approx_eq full(Bidiagonal(d, e, true)) U*Diagonal(s)*Vt

        @test_throws ArgumentError bdsqr!('A', d, e, Vt, U, C)
        @test_throws DimensionMismatch bdsqr!('U', d, [e, 1], Vt, U, C)
        @test_throws DimensionMismatch bdsqr!('U', d, e, Vt[1:end - 1, :], U, C)
        @test_throws DimensionMismatch bdsqr!('U', d, e, Vt, U[:,1:end - 1], C)
        @test_throws DimensionMismatch bdsqr!('U', d, e, Vt, U, C[1:end - 1, :])
    end
end

let # Issue #7886
    x, r = LAPACK.gelsy!([0 1; 0 2; 0 3.], [2, 4, 6.])
    @test_approx_eq x [0,2]
    @test r == 1
end

using Test

const GD = GenericDimensionful

@testset "GenericDimensionful" begin
    x = GD(4)
    @test x isa GD{1,Int}
    @test_throws DomainError GD{2+3im}(3)
    @test GD{2}(x) === GD{2,Int}(4)
    @test GD{2,Float64}(x) === GD{2}(4.0)
    @test Real(x) === Integer(x) === 4

    @test convert(GD{1}, x) === x
    @test convert(GD{1,Float64}, x) === GD(4.0)
    @test convert(GD{0}, 3) === GD{0}(3)
    @test convert(GD{0,Float64}, 3) === GD{0}(3.0)
    @test convert(GD{1.0}, x) === convert(GD{1.0,Int}, x) === convert(GD{1.0}, GD{1//1}(4)) === GD{1.0}(4)
    @test_throws DimensionMismatch convert(GD{1}, 3)
    @test_throws DimensionMismatch convert(GD{1,Float64}, 3)
    @test_throws DimensionMismatch convert(GD{2}, x)
    @test_throws DimensionMismatch convert(GD{2,Float64}, x)

    @test one(x) === 1
    @test oneunit(x) === GD(1)
    @test zero(x) === zero(GD{1,Int}) === GD(0)
    @test iszero(GD(0)) && !iszero(x)
    @test float(x) === GD(4.0)
    @test eps(GD(4.0)) == GD(eps(4.0))
    @test eps(GD{1,Float64}) === eps(Float64)
    for f in (floatmin, floatmax)
        @test f(GD{1,Float64}) == f(GD(4.0)) === GD(f(Float64))
    end

    @test abs2(x) === GD{2}(16) === abs2(GD{1.0}(4))
    @test inv(x) === GD{-1}(0.25)

    for f in (isfinite, isnan, isreal, isinf, isunordered), y in (x, GD(1.0), GD(Inf), GD(NaN))
        @test f(y) === f(y.val)
    end
    for f in (abs,conj,real,imag,complex,+,-), y in (3+4im, 4.25)
        @test f(GD(y)) === GD(f(y))
    end

    for op in (+, -)
        @test op(x, GD(1.5)) === GD(op(4,1.5))
        @test_throws DimensionMismatch op(x, GD{2}(1.5))
        @test_throws DimensionMismatch op(x, 1.5)
        @test_throws DimensionMismatch op(1.5, x)
        @test op(GD{0}(5), 1.5) === op(5, GD{0}(1.5)) === GD{0}(op(5, 1.5))
    end

    for op in (==, !=, <, <=, isless, isequal), y in (GD(3), x, GD(5))
        @test op(x, y) === op(x.val, y.val)
    end
    @test_throws DimensionMismatch x == GD{2}(4)
    @test_throws DimensionMismatch x == 3

    @test x*x*x === x * GD{2}(4^2) === GD{3}(4^3)
    for op in (/, //)
        @test op(x, GD(8)) === op(4, 8) == GD{0}(op(4, 8))
        @test op(x, GD{3}(8)) === GD{-2}(op(4, 8))
        @test op(x, 8) === GD(op(4, 8))
    end
    @test x ÷ GD(3) === 1
    @test x % GD(-3) === GD(1)
    @test mod(x, GD(-3)) === GD(-2)

    @test sqrt(x) === GD{1//2}(2.0)

    let p = 2 # to avoid literal_pow path
        @test x^GD{0}(p) == x^2 == x^p == GD{2}(4^p)
    end
    @test x^GD{0}(2.5) == x^2.5 == GD{2.5}(4^2.5)
    @test x^(5//2) == GD{5//2}(4^2.5)
    @test_throws DimensionMismatch 2^x
    @test_throws DimensionMismatch x^x
end
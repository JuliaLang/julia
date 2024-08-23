using Base.TypeDomainIntegers
using Test

const max_tested_number = 7

@testset "TypeDomainNaturalNumbers.jl" begin
    @testset "subtyping" begin
        @test PositiveInteger <: NonnegativeInteger <: TypeDomainInteger <: Integer
        @test NegativeInteger <: TypeDomainInteger
        @test !(NonnegativeInteger <: PositiveInteger)
        @test PositiveInteger <: PositiveIntegerUpperBound
        @test !(NonnegativeInteger <: PositiveIntegerUpperBound)
        @test PositiveInteger == typeintersect(NonnegativeInteger,PositiveIntegerUpperBound)
    end
    @testset "zero" begin
        n = @inferred zero(NonnegativeInteger)
        @test @inferred iszero(n)
        @test n isa NonnegativeInteger
        @test !(n isa PositiveInteger)
        @test Base.issingletontype(typeof(n))
    end
    @testset "positive integers" begin
        n = @inferred zero(NonnegativeInteger)
        for _ ∈ 1:max_tested_number
            n = @inferred natural_successor(n)
            @test !(@inferred iszero(n))
            @test n isa NonnegativeInteger
            @test n isa PositiveInteger
            @test n isa PositiveIntegerUpperBound
        end
    end
    @testset "successor, predecessor" begin
        m = @inferred zero(NonnegativeInteger)
        @test_throws MethodError natural_predecessor(m)
        n = @inferred natural_successor(m)
        for _ ∈ 1:max_tested_number
            @test m === @inferred natural_predecessor(n)
            @test n === @inferred natural_successor(m)
        end
    end
    @testset "conversion to/from `Bool`" begin
        z = @inferred zero(NonnegativeInteger)
        o = @inferred natural_successor(z)
        t = @inferred natural_successor(o)
        @test z === convert(NonnegativeInteger, false)
        @test o === convert(NonnegativeInteger, true)
        @test false === @inferred convert(Bool, z)
        @test  true === @inferred convert(Bool, o)
        @test_throws InexactError convert(Bool, t)
    end
    @testset "conversion to/from `Int`" begin
        for i ∈ 0:max_tested_number
            @test i === convert(Int, convert(NonnegativeInteger, i))
            @test convert(NonnegativeInteger, i) isa NonnegativeInteger
        end
        @testset "negative" begin
            for i ∈ -5:-1
                @test_throws Exception convert(NonnegativeInteger, i)
            end
        end
    end
    @testset "identity conversion" begin
        for i ∈ -max_tested_number:max_tested_number
            n = convert(TypeDomainInteger, i)
            @test n === @inferred convert(TypeDomainInteger, n)
        end
        for i ∈ 0:max_tested_number
            n = convert(NonnegativeInteger, i)
            @test n === @inferred convert(NonnegativeInteger, n)
        end
    end
    @testset "constructors" begin
        @testset "`Bool`" begin
            for i ∈ 0:1
                b = Bool(i)
                n = convert(NonnegativeInteger, i)
                @test b === @inferred Bool(n)
                @test n === NonnegativeInteger(b)
            end
            @testset "failure" begin
                t = convert(NonnegativeInteger, 2)
                @test_throws InexactError Bool(t)
            end
        end
        @testset "`Int`" begin
            for i ∈ 0:max_tested_number
                n = convert(NonnegativeInteger, i)
                @test i === @inferred Int(n)
                @test n === NonnegativeInteger(i)
            end
            for i ∈ -max_tested_number:max_tested_number
                n = convert(TypeDomainInteger, i)
                @test i === @inferred Int(n)
                @test n === TypeDomainInteger(i)
            end
        end
    end
    @testset "instance zero" begin
        for i ∈ 0:max_tested_number
            @test zero(NonnegativeInteger) === zero(convert(NonnegativeInteger, i))
        end
        for i ∈ -max_tested_number:max_tested_number
            @test zero(TypeDomainInteger) === zero(convert(TypeDomainInteger, i))
        end
    end
    @testset "properties" begin
        for i ∈ -max_tested_number:max_tested_number
            n = convert(TypeDomainInteger, i)
            @test () === @inferred propertynames(n)
            @test () === @inferred propertynames(n, false)
        end
    end
    @testset "`iseven`, `isodd`" begin
        for i ∈ -max_tested_number:max_tested_number
            n = convert(TypeDomainInteger, i)
            @test iseven(n) === iseven(i)
            @test isodd(n) === isodd(i)
        end
    end
    @testset "comparisons" begin
        @testset "between type domain integers" begin
            for a ∈ -max_tested_number:max_tested_number
                l = convert(TypeDomainInteger, a)
                for b ∈ -max_tested_number:max_tested_number
                    r = convert(TypeDomainInteger, b)
                    for op ∈ (==, <, isequal, isless)
                        @test op(l, r) isa Bool
                        @test op(l, r) == op(a, b)
                    end
                end
            end
        end
        @testset "between a type domain integer and a `Number`" begin
            z = @inferred zero(NonnegativeInteger)
            o = @inferred natural_successor(z)
            t = @inferred natural_successor(o)
            for i ∈ -max_tested_number:max_tested_number
                lesser_numbers = (
                    prevfloat(Float64(i)), prevfloat(Float32(i)), prevfloat(Float16(i)),
                    i - true, Float64(i) - true, Float32(i) - true, Float16(i) - true,
                    -Inf16, -Inf32, -Inf64,
                )
                greater_numbers = (
                    nextfloat(Float64(i)), nextfloat(Float32(i)), nextfloat(Float16(i)),
                    i + true, Float64(i) + true, Float32(i) + true, Float16(i) + true,
                    Inf16, Inf32, Inf64,
                )
                unequal_numbers = (lesser_numbers..., greater_numbers...)
                n = convert(TypeDomainInteger, i)
                @testset "`==`, `isequal`" begin
                    for op ∈ (==, isequal)
                        for x ∈ (i, Float64(i), Float32(i), Float16(i))
                            @test op(n, x)
                            @test op(x, n)
                        end
                        for x ∈ unequal_numbers
                            @test !op(n, x)
                            @test !op(x, n)
                        end
                    end
                    @testset "`missing`" begin
                        @test ismissing(n == missing)
                        @test ismissing(missing == n)
                        @test !isequal(n, missing)
                        @test !isequal(missing, n)
                    end
                end
                @testset "`<`, `isless`" begin
                    for op ∈ (<, isless)
                        for x ∈ (i, Float64(i), Float32(i), Float16(i))
                            @test !op(n, x)
                            @test !op(x, n)
                        end
                        for x ∈ greater_numbers
                            @test op(n, x)
                            @test !op(x, n)
                        end
                        for x ∈ lesser_numbers
                            @test !op(n, x)
                            @test op(x, n)
                        end
                    end
                    @testset "`missing`" begin
                        @test ismissing(n < missing)
                        @test ismissing(missing < n)
                        @test isless(n, missing)
                        @test !isless(missing, n)
                    end
                end
            end
        end
    end
    @testset "addition and subtraction" begin
        @testset "identity" begin
            id = zero(NonnegativeInteger)
            for i ∈ -max_tested_number:max_tested_number
                n = convert(TypeDomainInteger, i)
                @test (@inferred (n + id)) === (id + n) === (n - id) === n
                @test (@inferred (n - n)) === id
            end
        end
        @testset "special cases" begin
            z = zero(NonnegativeInteger)
            o = @inferred natural_successor(z)
            t = @inferred natural_successor(o)
            @test -o === (z - o)
            @test -t === (z - t)
            @test -o === (o - t)
            @test t === @inferred (o + o)
            @test o === @inferred (t - o)
        end
        @testset "systematic" begin
            for a ∈ -max_tested_number:max_tested_number
                l = convert(TypeDomainInteger, a)
                for b ∈ -max_tested_number:max_tested_number
                    r = convert(TypeDomainInteger, b)
                    @test convert(Int, l + r) === (a + b)
                end
            end
        end
    end
    @testset "multiplication" begin
        z = zero(NonnegativeInteger)
        o = @inferred natural_successor(z)
        t = @inferred natural_successor(o)
        @testset "`one`, `isone`" begin
            @test o === one(TypeDomainInteger) === one(NonnegativeInteger) === one(z) === one(o) === one(t)
            @test !isone(z)
            @test isone(o)
            @test !isone(t)
        end
        for a ∈ -max_tested_number:max_tested_number
            l = convert(TypeDomainInteger, a)
            @test l === (l * o) === (o * l)
            for b ∈ -max_tested_number:max_tested_number
                r = convert(TypeDomainInteger, b)
                @test convert(Int, l * r) === (a * b)
            end
        end
    end
    @testset "some identities" begin
        z = @inferred zero(NonnegativeInteger)
        o = @inferred natural_successor(z)
        for c ∈ (π, ℯ, 7, 0x7, 7.0, true)
            @test (z + c) === c === (c + z) === (c - z) === (o * c) === (c * o)
        end
    end
    @testset "heterogeneous `+` `-` `*`" begin
        for i ∈ -max_tested_number:max_tested_number
            n = convert(TypeDomainInteger, i)
            for x ∈ (((-1):5)..., ((-3):0.1:3)...)
                for op ∈ (+, -, *)
                    @test op(n, x) == op(i, x)
                    @test op(x, n) == op(x, i)
                end
            end
        end
    end
end

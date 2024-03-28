
@testset "StaticInt" begin
    @test static(UInt(8)) === StaticInt(UInt(8)) === StaticInt{8}()
    @test iszero(StaticInt(0))
    @test !iszero(StaticInt(1))
    @test !isone(StaticInt(0))
    @test isone(StaticInt(1))
    @test @inferred(one(StaticInt(1))) === StaticInt(1)
    @test @inferred(zero(StaticInt(1))) === StaticInt(0)
    @test @inferred(one(StaticInt)) === StaticInt(1)
    @test @inferred(zero(StaticInt)) === StaticInt(0) === StaticInt(StaticInt(Val(0)))
    @test eltype(one(StaticInt)) <: Int

    x = StaticInt(1)
    @test @inferred(Bool(x)) isa Bool
    @test @inferred(BigInt(x)) isa BigInt
    @test @inferred(Integer(x)) === x
    @test @inferred(%(x, Integer)) === 1
    # test for ambiguities and correctness
    for i ∈ Any[StaticInt(0), StaticInt(1), StaticInt(2), 3]
        for j ∈ Any[StaticInt(0), StaticInt(1), StaticInt(2), 3]
            i === j === 3 && continue
            for f ∈ [+, -, *, ÷, %, <<, >>, >>>, &, |, ⊻, ==, ≤, ≥]
                (iszero(j) && ((f === ÷) || (f === %))) && continue # integer division error
                @test convert(Int, @inferred(f(i,j))) == f(convert(Int, i), convert(Int, j))
            end
        end
        i == 3 && break
        for f ∈ [+, -, *, /, ÷, %, ==, ≤, ≥]
            w = f(convert(Int, i), 1.4)
            x = f(1.4, convert(Int, i))
            @test convert(typeof(w), @inferred(f(i, 1.4))) === w
            @test convert(typeof(x), @inferred(f(1.4, i))) === x # if f is division and i === StaticInt(0), returns `NaN`; hence use of ==== in check.
            (((f === ÷) || (f === %)) && (i === StaticInt(0))) && continue
            y = f(convert(Int, i), 2 // 7)
            z = f(2 // 7, convert(Int, i))
            @test convert(typeof(y), @inferred(f(i, 2 // 7))) === y
            @test convert(typeof(z), @inferred(f(2 // 7, i))) === z 
        end
    end

    @test UnitRange{Int16}(StaticInt(-9), 17) === Int16(-9):Int16(17)
    @test UnitRange{Int16}(-7, StaticInt(19)) === Int16(-7):Int16(19)
    @test UnitRange(-11, StaticInt(15)) === -11:15
    @test UnitRange(StaticInt(-11), 15) === -11:15
    @test UnitRange{Int}(StaticInt(-11), StaticInt(15)) === -11:15
    @test UnitRange(StaticInt(-11), StaticInt(15)) === -11:15
    @test float(StaticInt(8)) === static(8.0)

    # test specific promote rules to ensure we don't cause ambiguities
    SI = StaticInt{1}
    IR = typeof(1//1)
    PI = typeof(pi)
    @test @inferred(convert(SI, SI())) === SI()
    @test @inferred(promote_rule(SI, PI)) <: promote_type(Int, PI)
    @test @inferred(promote_rule(SI, IR)) <: promote_type(Int, IR)
    @test @inferred(promote_rule(SI, SI)) <: Int
    @test @inferred(promote_rule(Missing, SI)) <: promote_type(Missing, Int)
    @test @inferred(promote_rule(Nothing, SI)) <: promote_type(Nothing, Int)
    @test @inferred(promote_rule(SI, Missing)) <: promote_type(Int, Missing)
    @test @inferred(promote_rule(SI, Nothing)) <: promote_type(Int, Nothing)
    @test @inferred(promote_rule(Union{Missing,Int}, SI)) <: promote_type(Union{Missing,Int}, Int)
    @test @inferred(promote_rule(Union{Nothing,Int}, SI)) <: promote_type(Union{Nothing,Int}, Int)
    @test @inferred(promote_rule(Union{Nothing,Missing,Int}, SI)) <: Union{Nothing,Missing,Int}
    @test @inferred(promote_rule(Union{Nothing,Missing}, SI)) <: promote_type(Union{Nothing,Missing}, Int)
    @test @inferred(promote_rule(SI, Missing)) <: promote_type(Int, Missing)
    @test @inferred(promote_rule(Base.TwicePrecision{Int}, StaticInt{1})) <: Base.TwicePrecision{Int}

    @test static(Int8(-18)) === static(-18)
    @test static(0xef) === static(239)
    @test static(Int16(-18)) === static(-18)
    @test static(0xffef) === static(65519)
    if sizeof(Int) == 8
        @test static(Int32(-18)) === static(-18)
        @test static(0xffffffef) === static(4294967279)
    end
end


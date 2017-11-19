# This file is a part of Julia. License is MIT: https://julialang.org/license

@testset "MissingException" begin
    @test sprint(showerror, MissingException("test")) == "MissingException: test"
end

@testset "convert" begin
    @test convert(Union{Int, Missing}, 1) === 1
    @test convert(Union{Int, Missing}, 1.0) === 1
    @test_throws MethodError convert(Missing, 1)
    @test_throws MethodError convert(Union{Int, Missing}, "a")
end

@testset "promote rules" begin
    @test promote_type(Missing, Missing) == Missing
    @test promote_type(Missing, Int) == Union{Missing, Int}
    @test promote_type(Int, Missing) == Union{Missing, Int}
    @test promote_type(Int, Any) == Any
    @test promote_type(Any, Any) == Any
    @test promote_type(Missing, Any) == Any
    @test promote_type(Any, Missing) == Any
    @test promote_type(Union{Int, Missing}, Missing) == Union{Int, Missing}
    @test promote_type(Missing, Union{Int, Missing}) == Union{Int, Missing}
    @test promote_type(Union{Int, Missing}, Int) == Union{Int, Missing}
    @test promote_type(Int, Union{Int, Missing}) == Union{Int, Missing}
    @test promote_type(Any, Union{Int, Missing}) == Any
    @test promote_type(Union{Int, Missing}, Union{Int, Missing}) == Union{Int, Missing}
    @test promote_type(Union{Float64, Missing}, Union{String, Missing}) == Any
    @test promote_type(Union{Float64, Missing}, Union{Int, Missing}) == Union{Float64, Missing}
    @test_broken promote_type(Union{Void, Missing, Int}, Float64) == Any
end

@testset "comparison operators" begin
    @test (missing == missing) === missing
    @test (1 == missing) === missing
    @test (missing == 1) === missing
    @test (missing != missing) === missing
    @test (1 != missing) === missing
    @test (missing != 1) === missing
    @test isequal(missing, missing)
    @test !isequal(1, missing)
    @test !isequal(missing, 1)
    @test (missing < missing) === missing
    @test (missing < 1) === missing
    @test (1 < missing) === missing
    @test (missing <= missing) === missing
    @test (missing <= 1) === missing
    @test (1 <= missing) === missing
    @test !isless(missing, missing)
    @test !isless(missing, 1)
    @test isless(1, missing)
end

@testset "arithmetic operators" begin
    arithmetic_operators = [+, -, *, /, ^, Base.div, Base.mod, Base.fld, Base.rem]

    # All unary operators return missing when evaluating missing
    for f in [!, +, -]
        @test ismissing(f(missing))
    end

    # All arithmetic operators return missing when operating on two missing's
    # All arithmetic operators return missing when operating on a scalar and an missing
    # All arithmetic operators return missing when operating on an missing and a scalar
    for f in arithmetic_operators
        @test ismissing(f(missing, missing))
        @test ismissing(f(1, missing))
        @test ismissing(f(missing, 1))
    end
end

@testset "bit operators" begin
    bit_operators = [&, |, ⊻]

    # All bit operators return missing when operating on two missing's
    for f in bit_operators
        @test ismissing(f(missing, missing))
    end
end

@testset "boolean operators" begin
    @test ismissing(missing & true)
    @test ismissing(true & missing)
    @test !(missing & false)
    @test !(false & missing)
    @test ismissing(missing | false)
    @test ismissing(false | missing)
    @test missing | true
    @test true | missing
    @test ismissing(xor(missing, true))
    @test ismissing(xor(true, missing))
    @test ismissing(xor(missing, false))
    @test ismissing(xor(false, missing))

    @test ismissing(missing & 1)
    @test ismissing(1 & missing)
    @test ismissing(missing | 1)
    @test ismissing(1 | missing)
    @test ismissing(xor(missing, 1))
    @test ismissing(xor(1, missing))
end

@testset "* string concatenation" begin
    @test ismissing("a" * missing)
    @test ismissing(missing * "a")
end

@testset "elementary functions" begin
    elementary_functions = [abs, abs2, sign,
                            acos, acosh, asin, asinh, atan, atanh, sin, sinh,
                            conj, cos, cosh, tan, tanh,
                            exp, exp2, expm1, log, log10, log1p, log2,
                            exponent, sqrt, gamma, lgamma,
                            identity, zero,
                            iseven, isodd, ispow2,
                            isfinite, isinf, isnan, iszero,
                            isinteger, isreal, isempty, transpose, float]

    # All elementary functions return missing when evaluating missing
    for f in elementary_functions
        @test ismissing(f(missing))
    end

    @test zero(Union{Int, Missing}) === 0
    @test zero(Union{Float64, Missing}) === 0.0
    @test_throws MethodError zero(Any)
    @test_throws MethodError zero(String)
    @test_throws MethodError zero(Union{String, Missing})
end

@testset "rounding functions" begin
    rounding_functions = [ceil, floor, round, trunc]

    # All rounding functions return missing when evaluating missing as first argument
    for f in rounding_functions
        @test ismissing(f(missing))
        @test ismissing(f(missing, 1))
        @test ismissing(f(missing, 1, 1))
        @test ismissing(f(Union{Int, Missing}, missing))
        @test_throws MissingException f(Int, missing)
    end
end

@testset "printing" begin
    @test sprint(show, missing) == "missing"
    @test sprint(showcompact, missing) == "missing"
    @test sprint(show, [missing]) == "$Missing[missing]"
    @test sprint(show, [1 missing]) == "$(Union{Int, Missing})[1 missing]"
    b = IOBuffer()
    display(TextDisplay(b), [missing])
    @test String(take!(b)) == "1-element Array{$Missing,1}:\n missing"
    b = IOBuffer()
    display(TextDisplay(b), [1 missing])
    @test String(take!(b)) == "1×2 Array{$(Union{Int, Missing}),2}:\n 1  missing"
end

@testset "arrays with missing values" begin
    x = convert(Vector{Union{Int, Missing}}, [1.0, missing])
    @test isa(x, Vector{Union{Int, Missing}})
    @test isequal(x, [1, missing])
    x = convert(Vector{Union{Int, Missing}}, [1.0])
    @test isa(x, Vector{Union{Int, Missing}})
    @test x == [1]
    x = convert(Vector{Union{Int, Missing}}, [missing])
    @test isa(x, Vector{Union{Int, Missing}})
    @test isequal(x, [missing])
end

@testset "== and != on arrays" begin
    @test ismissing([1, missing] == [1, missing])
    @test ismissing(["a", missing] == ["a", missing])
    @test ismissing(Any[1, missing] == Any[1, missing])
    @test ismissing(Any[missing] == Any[missing])
    @test ismissing([missing] == [missing])
    @test ismissing(Any[missing, 2] == Any[1, missing])
    @test ismissing([missing, false] == BitArray([true, false]))
    @test ismissing(Any[missing, false] == BitArray([true, false]))
    @test Union{Int, Missing}[1] == Union{Float64, Missing}[1.0]
    @test Union{Int, Missing}[1] == [1.0]
    @test Union{Bool, Missing}[true] == BitArray([true])
    @test !(Union{Int, Missing}[1] == [2])
    @test !([1] == Union{Int, Missing}[2])
    @test !(Union{Int, Missing}[1] == Union{Int, Missing}[2])

    @test ismissing([1, missing] != [1, missing])
    @test ismissing(["a", missing] != ["a", missing])
    @test ismissing(Any[1, missing] != Any[1, missing])
    @test ismissing(Any[missing] != Any[missing])
    @test ismissing([missing] != [missing])
    @test ismissing(Any[missing, 2] != Any[1, missing])
    @test ismissing([missing, false] != BitArray([true, false]))
    @test ismissing(Any[missing, false] != BitArray([true, false]))
    @test !(Union{Int, Missing}[1] != Union{Float64, Missing}[1.0])
    @test !(Union{Int, Missing}[1] != [1.0])
    @test !(Union{Bool, Missing}[true] != BitArray([true]))
    @test Union{Int, Missing}[1] != [2]
    @test [1] != Union{Int, Missing}[2]
    @test Union{Int, Missing}[1] != Union{Int, Missing}[2]
end

@testset "any & all" begin
    @test any([true, missing])
    @test any(x -> x == 1, [1, missing])
    @test ismissing(any([false, missing]))
    @test ismissing(any(x -> x == 1, [2, missing]))
    @test ismissing(all([true, missing]))
    @test ismissing(all(x -> x == 1, [1, missing]))
    @test !all([false, missing])
    @test !all(x -> x == 1, [2, missing])
    @test 1 in [1, missing]
    @test ismissing(2 in [1, missing])
    @test ismissing(missing in [1, missing])
end

@testset "float" begin
    @test isequal(float([1, missing]), [1, missing])
    @test float([1, missing]) isa Vector{Union{Float64, Missing}}
    @test isequal(float(Union{Int, Missing}[missing]), [missing])
    @test float(Union{Int, Missing}[missing]) isa Vector{Union{Float64, Missing}}
    @test float(Union{Int, Missing}[1]) == [1]
    @test float(Union{Int, Missing}[1]) isa Vector{Union{Float64, Missing}}
    @test isequal(float([missing]), [missing])
    @test float([missing]) isa Vector{Missing}
end

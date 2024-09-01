# Branching

@testset "branching" begin

test_mod = Module()

Base.eval(test_mod, quote
    using JuliaLowering: JuliaLowering, @ast, @chk
    using JuliaSyntax
end)

Base.eval(test_mod, quote
    function var"@label"(__context__::JuliaLowering.MacroContext, ex)
        @chk kind(ex) == JuliaSyntax.K"Identifier"
        @ast __context__ ex ex=>JuliaSyntax.K"symbolic_label"
    end

    function var"@goto"(__context__::JuliaLowering.MacroContext, ex)
        @chk kind(ex) == JuliaSyntax.K"Identifier"
        @ast __context__ ex ex=>JuliaSyntax.K"symbolic_goto"
    end
end)

#-------------------------------------------------------------------------------
@testset "Tail position" begin

@test JuliaLowering.include_string(test_mod, """
let a = true
    if a
        1
    end
end
""") === 1

@test JuliaLowering.include_string(test_mod, """
let a = false
    if a
        1
    end
end
""") === nothing

@test JuliaLowering.include_string(test_mod, """
let a = true
    if a
        1
    else
        2
    end
end
""") === 1

@test JuliaLowering.include_string(test_mod, """
let a = false
    if a
        1
    else
        2
    end
end
""") === 2

@test JuliaLowering.include_string(test_mod, """
let a = false, b = true
    if a
        1
    elseif b
        2
    else
        3
    end
end
""") === 2

@test JuliaLowering.include_string(test_mod, """
let a = false, b = false
    if a
        1
    elseif b
        2
    else
        3
    end
end
""") === 3

end

#-------------------------------------------------------------------------------
@testset "Value required but not tail position" begin

@test JuliaLowering.include_string(test_mod, """
let a = true
    x = if a
        1
    end
    x
end
""") === 1

@test JuliaLowering.include_string(test_mod, """
let a = false
    x = if a
        1
    end
    x
end
""") === nothing

@test JuliaLowering.include_string(test_mod, """
let a = true
    x = if a
        1
    else
        2
    end
    x
end
""") === 1

@test JuliaLowering.include_string(test_mod, """
let a = false
    x = if a
        1
    else
        2
    end
    x
end
""") === 2

@test JuliaLowering.include_string(test_mod, """
let a = false, b = true
    x = if a
        1
    elseif b
        2
    else
        3
    end
    x
end
""") === 2

@test JuliaLowering.include_string(test_mod, """
let a = false, b = false
    x = if a
        1
    elseif b
        2
    else
        3
    end
    x
end
""") === 3

end

#-------------------------------------------------------------------------------
@testset "Side effects (not value or tail position)" begin

@test JuliaLowering.include_string(test_mod, """
let a = true
    x = nothing
    if a
        x = 1
    end
    x
end
""") === 1

@test JuliaLowering.include_string(test_mod, """
let a = false
    x = nothing
    if a
        x = 1
    end
    x
end
""") === nothing

@test JuliaLowering.include_string(test_mod, """
let a = true
    x = nothing
    if a
        x = 1
    else
        x = 2
    end
    x
end
""") === 1

@test JuliaLowering.include_string(test_mod, """
let a = false
    x = nothing
    if a
        x = 1
    else
        x = 2
    end
    x
end
""") === 2

@test JuliaLowering.include_string(test_mod, """
let a = false, b = true
    x = nothing
    if a
        x = 1
    elseif b
        x = 2
    else
        x = 3
    end
    x
end
""") === 2

@test JuliaLowering.include_string(test_mod, """
let a = false, b = false
    x = nothing
    if a
        x = 1
    elseif b
        x = 2
    else
        x = 3
    end
    x
end
""") === 3

end

#-------------------------------------------------------------------------------
@testset "`&&` and `||` chains" begin

@test JuliaLowering.include_string(test_mod, """
true && "hi"
""") == "hi"

@test JuliaLowering.include_string(test_mod, """
true && true && "hi"
""") == "hi"

@test JuliaLowering.include_string(test_mod, """
false && "hi"
""") == false

@test JuliaLowering.include_string(test_mod, """
true && false && "hi"
""") == false

@test JuliaLowering.include_string(test_mod, """
begin
    z = true && "hi"
    z
end
""") == "hi"

@test JuliaLowering.include_string(test_mod, """
begin
    z = false && "hi"
    z
end
""") == false


@test JuliaLowering.include_string(test_mod, """
true || "hi"
""") == true

@test JuliaLowering.include_string(test_mod, """
true || true || "hi"
""") == true

@test JuliaLowering.include_string(test_mod, """
false || "hi"
""") == "hi"

@test JuliaLowering.include_string(test_mod, """
false || true || "hi"
""") == true

@test JuliaLowering.include_string(test_mod, """
false || false || "hi"
""") == "hi"

@test JuliaLowering.include_string(test_mod, """
begin
    z = false || "hi"
    z
end
""") == "hi"

@test JuliaLowering.include_string(test_mod, """
begin
    z = true || "hi"
    z
end
""") == true

end

@testset "symbolic goto/label" begin
    JuliaLowering.include_string(test_mod, """
    let
        a = []
        i = 1
        @label foo
        push!(a, i)
        i = i + 1
        if i <= 2
            @goto foo
        end
        a
    end
    """) == [1,2]

    @test_throws LoweringError JuliaLowering.include_string(test_mod, """
    begin
        @goto foo
    end
    """)

    @test_throws LoweringError JuliaLowering.include_string(test_mod, """
    begin
        @label foo
        @label foo
    end
    """)

    @test_throws LoweringError JuliaLowering.include_string(test_mod, """
    x = @label foo
    """)
end

#-------------------------------------------------------------------------------
@testset "Branching IR" begin
    test_ir_cases(joinpath(@__DIR__,"branching_ir.jl"))
end

end

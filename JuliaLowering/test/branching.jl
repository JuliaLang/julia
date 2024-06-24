# Branching

@testset "branching" begin

test_mod = Module()

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

#-------------------------------------------------------------------------------
@testset "Detailed lowering tests" begin

@test ir_as_text(test_mod, """
begin
    local a, b
    if a
        b
    end
end
""") == """
slot.₁/a
(gotoifnot ssa.₁ label.₅)
slot.₂/b
(return ssa.₃)
core.nothing
(return ssa.₅)"""

@test ir_as_text(test_mod, """
begin
    local a, b, c
    if a
        b
    end
    c
end
""") == """
slot.₁/a
(gotoifnot ssa.₁ label.₄)
slot.₂/b
slot.₃/c
(return ssa.₄)"""

@test ir_as_text(test_mod, """
begin
    local a, b, c
    if a
        b
    else
        c
    end
end
""") == """
slot.₁/a
(gotoifnot ssa.₁ label.₅)
slot.₂/b
(return ssa.₃)
slot.₃/c
(return ssa.₅)"""

@test ir_as_text(test_mod, """
begin
    local a, b, c, d
    if a
        b
    else
        c
    end
    d
end
""") == """
slot.₁/a
(gotoifnot ssa.₁ label.₅)
slot.₂/b
(goto label.₆)
slot.₃/c
slot.₄/d
(return ssa.₆)"""

# Blocks compile directly to branches
@test ir_as_text(test_mod, """
begin
   local a, b, c, d
   if (a; b && c)
       d
   end
end
""") == """
slot.₁/a
slot.₂/b
(gotoifnot ssa.₂ label.₈)
slot.₃/c
(gotoifnot ssa.₄ label.₈)
slot.₄/d
(return ssa.₆)
core.nothing
(return ssa.₈)"""

end

end

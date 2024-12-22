
@testset "Functions" begin

test_mod = Module()

# Capture assigned before closure
@test JuliaLowering.include_string(test_mod, """
let
    x = 1
    f(y) = x+y
    f(2), f(3)
end
""") == (3,4)

# Capture assigned after closure
@test JuliaLowering.include_string(test_mod, """
let
    f(y) = x+y
    x = 1
    f(2)
end
""") == 3

# Capture assigned inside closure
@test JuliaLowering.include_string(test_mod, """
let
    x = 1
    function f(y)
        x = y
    end
    f(100)
    x
end
""") == 100

# Anon function syntax
@test JuliaLowering.include_string(test_mod, """
begin
    call_it(f, arg) = f(arg)
    local y = 2
    call_it(x->x+y, 3)
end
""") == 5

# Anon function syntax with `where`
@test JuliaLowering.include_string(test_mod, """
begin
    call_it(f, arg) = f(arg)
    local y = 2
    call_it((x::T where {T<:Integer})->x+y, 3)
end
""") == 5


# Attempt to reference capture which is not assigned
@test_throws UndefVarError(:x, :local) JuliaLowering.include_string(test_mod, """
let
    function f()
        x
    end
    f()
    x = 1
end
""")


end

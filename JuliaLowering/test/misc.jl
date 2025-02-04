@testset "Miscellanous" begin

test_mod = Module()

# Blocks
@test JuliaLowering.include_string(test_mod, """
begin
end
""") == nothing

# Placeholders
@test JuliaLowering.include_string(test_mod, """_ = 10""") == 10

# GC.@preserve
@test JuliaLowering.include_string(test_mod, """
let x = [1,2]
    GC.@preserve x begin
        x
    end
end
""") == [1,2]

@test JuliaLowering.include_string(test_mod, """
let x=11
    20x
end
""") == 220

# ccall
@test JuliaLowering.include_string(test_mod, """
ccall(:strlen, Csize_t, (Cstring,), "asdfg")
""") == 5

# cfunction
JuliaLowering.include_string(test_mod, """
function f_ccallable(x, y)
    x + y * 10
end
""")
cf_int = JuliaLowering.include_string(test_mod, """
@cfunction(f_ccallable, Int, (Int,Int))
""")
@test @ccall($cf_int(2::Int, 3::Int)::Int) == 32
cf_float = JuliaLowering.include_string(test_mod, """
@cfunction(f_ccallable, Float64, (Float64,Float64))
""")
@test @ccall($cf_float(2::Float64, 3::Float64)::Float64) == 32.0

end

@testset "Array syntax" begin

test_mod = Module()

# vect
@test JuliaLowering.include_string(test_mod, """
[1,2,3]
""") == [1,2,3]

# hcat
@test JuliaLowering.include_string(test_mod, """
[1 2 3]
""") == [1 2 3]

@test JuliaLowering.include_string(test_mod, """
let
    xs = (1,2)
    [xs...; xs...]
end
""") == [1,2,1,2]

# hvcat
@test JuliaLowering.include_string(test_mod, """
[1 2 3; 4 5 6]
""") == [1 2 3;
         4 5 6]

# hvcat_rows
@test JuliaLowering.include_string(test_mod, """
let
    xs = (1,2)
    [xs... 3; 4 xs...]
end
""") == [1 2 3;
         4 1 2]

end

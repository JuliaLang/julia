@testset "Generators" begin

test_mod = Module()

@test JuliaLowering.include_string(test_mod, """
collect(x^2 for x in 1:3)
""") == [1,4,9]

@test JuliaLowering.include_string(test_mod, """
collect(x for x in 1:5 if isodd(x))
""") == [1,3,5]

@test JuliaLowering.include_string(test_mod, """
collect((y,x) for (x,y) in zip(1:3, 2:4) if y != 3)
""") == [(2,1), (4,3)]

# product iterator
@test JuliaLowering.include_string(test_mod, """
collect((x,y) for x in 1:3, y in 1:2)
""") == [(1,1)  (1,2)
         (2,1)  (2,2)
         (3,1)  (3,2)]

# flattened iterator
@test JuliaLowering.include_string(test_mod, """
collect((x,y,z) for x in 1:3, y in 4:5 for z in 6:7)
""") == [
    (1,4,6)
    (1,4,7)
    (2,4,6)
    (2,4,7)
    (3,4,6)
    (3,4,7)
    (1,5,6)
    (1,5,7)
    (2,5,6)
    (2,5,7)
    (3,5,6)
    (3,5,7)
]

# Duplicate iteration variables - body sees only innermost
@test JuliaLowering.include_string(test_mod, """
collect(x for x in 1:3 for x in 1:2)
""") == [1, 2, 1, 2, 1, 2]

# Outer iteration variables are protected from mutation
@test JuliaLowering.include_string(test_mod, """
collect((z=y; y=100; z) for y in 1:3 for x in 1:2)
""") == [1, 1, 2, 2, 3, 3]

# Simple typed comprehension lowered to for loops
@test JuliaLowering.include_string(test_mod, """
Tuple{Int,Int}[(x,y) for x in 1:2, y in 1:3]
""") == [(1,1) (1,2) (1,3)
         (2,1) (2,2) (2,3)]

end

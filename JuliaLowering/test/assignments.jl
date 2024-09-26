@testset "assignments" begin

test_mod = Module()

Base.include_string(test_mod,
"""
mutable struct X
    a
    b
end
""")

# TODO: Desugaring of assignment done, but needs `where` lowering
@test_broken JuliaLowering.include_string(test_mod, """
MyVector{T} = Array{1,T}
""") == 42

# Chained assignment
@test JuliaLowering.include_string(test_mod, """
let
    a = b = 42
end
""") == 42

@test JuliaLowering.include_string(test_mod, """
let
    x = []
    a = b = (push!(x, 1); 42)
    (a,b,x)
end
""") == (42,42,[1])

# setproperty!
@test JuliaLowering.include_string(test_mod, """
let
    x = X(1,2)
    x.a = 10
    (x.a, x.b)
end
""") == (10,2)

# Lowering of ref
@test JuliaLowering.include_string(test_mod, """
let
    as = [0,0,0,0]
    as[begin] = 1
    as[2] = 2
    as[end] = 4
    as
end
""") == [1, 2, 0, 4]

@test JuliaLowering.include_string(test_mod, """
let
    as = zeros(Int, 2,3)
    as[begin, end] = 1
    as[end, begin] = 2
    js = (2,)
    as[js..., end] = 3
    as
end
""") == [0 0 1;
         2 0 3]

# Declarations
@test JuliaLowering.include_string(test_mod, """
let
    x::Int = 1
    x = 10.0
    x
end
""") === 10


end

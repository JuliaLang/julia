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
JuliaLowering.include_string(test_mod, """
MyVector{T} = Array{1,T}
""")
@test test_mod.MyVector{Int} == Array{1,Int}

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

# Declarations
@test JuliaLowering.include_string(test_mod, """
let
    x::Int = 1
    x = 10.0
    x
end
""") === 10


end

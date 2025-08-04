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

# Assignment in value but not tail position
@test JuliaLowering.include_string(test_mod, """
let
    x = begin
        y = 42
    end
    x
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

# Updating assignments
@test JuliaLowering.include_string(test_mod, """
let x = "hi"
    x *= " ho"
    x
end
""") == "hi ho"

@test JuliaLowering.include_string(test_mod, """
let x = [1,3]
    x .-= [0,1]
    x
end
""") == [1,2]

@test JuliaLowering.include_string(test_mod, """
let x = [1 2; 3 4]
    x[begin, 1:end] .-= 1
    x
end
""") == [0 1 ; 3 4]

# Test that side effects of computing indices in left hand side only occur
# once.
@test JuliaLowering.include_string(test_mod, """
let
    x = [1, 2]
    n_calls = 0
    the_index() = (n_calls = n_calls + 1; 1)
    x[the_index()] += 1
    x[the_index()]::Int += 1
    x[the_index():end] .+= 1
    n_calls
end
""") == 3

end

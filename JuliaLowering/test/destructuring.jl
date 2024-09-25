@testset "Destructuring" begin

test_mod = Module()

@testset "Destructuring via iteration" begin

@test JuliaLowering.include_string(test_mod, """
let
    as = [1,2,3]
    (x,y) = as
    (x,y)
end
""") == (1,2)

@test JuliaLowering.include_string(test_mod, """
let
    as = [1,2,3]
    (x,ys...) = as
    (x,ys)
end
""") == (1, [2,3])

@test JuliaLowering.include_string(test_mod, """
let
    as = [1,2,3,4]
    (x,ys...,z) = as
    (x,ys,z)
end
""") == (1, [2, 3], 4)

@test JuliaLowering.include_string(test_mod, """
let
    as = [1,2,3,4]
    (xs...,y) = as
    (xs,y)
end
""") == ([1, 2, 3], 4)

# Case where indexed_iterate is just iteration
@test JuliaLowering.include_string(test_mod, """
let
    (x,ys...,z) = "aβcδe"
    (x,ys,z)
end
""") == ('a', "βcδ", 'e')


# Use in value position yeilds rhs
@test JuliaLowering.include_string(test_mod, """
let
    as = [1,2]
    zs = begin
        (x,y) = as
    end
    (x,y, as === zs)
end
""") == (1, 2, true)

# lhs variable name in rhs
@test JuliaLowering.include_string(test_mod, """
let
    x = (1,2)
    (x,y) = x
    (x,y)
end
""") == (1, 2)

@test JuliaLowering.include_string(test_mod, """
let
    x = (1,2)
    (x...,y) = x
    (x,y)
end
""") == ((1,), 2)

@test JuliaLowering.include_string(test_mod, """
let
    zs = [(1,2), (3,(4,5))]
    ((a,b), (c,(d,e))) = zs
    (a,b,c,d,e)
end
""") == (1,2,3,4,5)

@test JuliaLowering.include_string(test_mod, """
let
    zs = [[1,2,3], 4]
    ((a,bs...), c) = zs
    (a, bs, c)
end
""") == (1, [2,3], 4)

@test_throws LoweringError JuliaLowering.include_string(test_mod, """
(xs..., ys...) = x
""")

end


@testset "Tuples on both sides" begin

# lhs variable name in rhs
@test JuliaLowering.include_string(test_mod, """
let
    x = 1
    y = 2
    (x,y) = (y,x)
    (x,y)
end
""") == (2, 1)

# dotted rhs in last place
@test JuliaLowering.include_string(test_mod, """
let
    rh = (2, 3)
    (x,y,z) = (1,rh...)
    (x,y,z)
end
""") == (1, 2, 3)
# in value position
@test JuliaLowering.include_string(test_mod, """
let
    rh = (2, 3)
    (x,y) = (1,rh...)
end
""") == (1, 2, 3)

end


@testset "Property destructuring" begin

# TODO: Move named tuple inside test case once we can lower it
Base.eval(test_mod, :(some_named_tuple = (a=1,b=2)))
@test JuliaLowering.include_string(test_mod, """
let
    (; a, b) = some_named_tuple
    (a, b)
end
""") == (1, 2)

@test_throws LoweringError JuliaLowering.include_string(test_mod, "(x ; a, b) = rhs")
@test_throws LoweringError JuliaLowering.include_string(test_mod, "(; a=1, b) = rhs")

end

end

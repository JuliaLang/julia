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

end


@testset "Tuple elimination with tuples on both sides" begin

# Simple case
@test JuliaLowering.include_string(test_mod, """
let a = 1, b = 2
    (x,y) = (a,b)
    (x,y)
end
""") == (1, 2)

# lhs variable name in rhs
@test JuliaLowering.include_string(test_mod, """
let x = 1, y = 2
    (x,y) = (y,x)
    (x,y)
end
""") == (2, 1)

# Slurps and splats

@test JuliaLowering.include_string(test_mod, """
let a = 1, b = 2, c = 3
    (x, ys..., z) = (a, b, c)
    (x, ys, z)
end
""") == (1, (2,), 3)

@test JuliaLowering.include_string(test_mod, """
let a = 1, b = 2, cs = (3,4)
    (x, ys...) = (a, b, cs...)
    (x, ys)
end
""") == (1, (2,3,4))

@test JuliaLowering.include_string(test_mod, """
let a = 1, bs = (2,3), c = 4
    (x, ys...) = (a, bs..., c)
    (x, ys)
end
""") == (1, (2,3,4))

@test JuliaLowering.include_string(test_mod, """
let a = 1, b = 2, cs = (3,4)
    (x, ys..., z) = (a, b, cs...)
    (x, ys, z)
end
""") == (1, (2,3), 4)

@test JuliaLowering.include_string(test_mod, """
let a = 1
    (x, ys...) = (a,)
    (x, ys)
end
""") == (1, ())

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

# Side effects in the right hand tuple can affect the previous left hand side
# bindings, for example, `x`, below. In this case we need to ensure `f()` is
# called before `x` is assigned the value from the right hand side.
# (the flisp implementation fails this test.)
@test JuliaLowering.include_string(test_mod, """
let
   function f()
       x=100
       2
   end
   (x,y) = (1,f())
   x,y
end
""") == (1,2)

# `x` is not assigned and no side effect from `f()` happens when the right hand
# side throws an UndefVarError
@test JuliaLowering.include_string(test_mod, """
let x=1, y=2, z=3, side_effect=false, a
    exc = try
        function f()
            side_effect=true
        end
        (x,y,z) = (100, a, f())
    catch e
        e
    end
    (x, y, z, side_effect, exc.var)
end
""") == (1, 2, 3, false, :a)

# Require that rhs is evaluated before any assignments, thus `x` is not defined
# here because accessing `a` first throws an UndefVarError
@test JuliaLowering.include_string(test_mod, """
let x, y, a
    try
        (x, y) = (1, a)
    catch
    end
    @isdefined(x)
end
""") == false

end


@testset "Property destructuring" begin

@test JuliaLowering.include_string(test_mod, """
let
    ab = (a=1, b=2)
    (; a, b) = ab
    (a, b)
end
""") == (1, 2)

end

end

# This file is a part of Julia. License is MIT: http://julialang.org/license

## indexing ##
@test length(()) === 0
@test length((1,)) === 1
@test length((1,2)) === 2

@test endof(()) === 0
@test endof((1,)) === 1
@test endof((1,2)) === 2

@test size((), 1) === 0
@test size((1,), 1) === 1
@test size((1,2), 1) === 2

# @test_throws ArgumentError size((), 2)
# @test_throws ArgumentError size((1,), 2)
# @test_throws ArgumentError size((1,2), 2)

@test getindex((1,), 1) === 1
@test getindex((1,2), 2) === 2
@test_throws BoundsError getindex((), 1)
@test_throws BoundsError getindex((1,2), 0)
@test_throws BoundsError getindex((1,2), -1)

@test getindex((1,), 1.0) === 1
@test getindex((1,2), 2.0) === 2
@test_throws BoundsError getindex((), 1.0)
@test_throws BoundsError getindex((1,2), 0.0)
@test_throws BoundsError getindex((1,2), -1.0)

@test getindex((5,6,7,8), [1,2,3]) === (5,6,7)
@test_throws BoundsError getindex((1,2), [3,4])

@test getindex((5,6,7,8), [true, false, false, true]) === (5,8)
@test_throws BoundsError getindex((5,6,7,8), [true, false, false, true, true])

@test getindex((5,6,7,8), []) === ()

## iterating ##
@test start((1,2,3)) === 1

@test done((), 1)
@test !done((1,2,3), 3)
@test done((1,2,3), 4)

@test next((5,6,7), 1) === (5, 2)
@test next((5,6,7), 3) === (7, 4)
@test_throws BoundsError next((5,6,7), 0)
@test_throws BoundsError next((), 1)



## eltype ##
@test eltype((1,2,3)) === Int
@test eltype((1.0,2.0,3.0)) <: FloatingPoint
@test eltype((true, false)) === Bool
@test eltype((1,2.0, false)) === Any
@test eltype(()) === Any


## mapping ##
foo() = 2
foo(x) = 2x
foo(x, y) = x + y
foo(x, y, z) = x + y + z

# 0 argument
@test map(foo) === 2

# 1 argument
@test map(foo, ()) === ()
@test map(foo, (1,)) === (2,)
@test map(foo, (1,2)) === (2,4)
@test map(foo, (1,2,3,4)) === (2,4,6,8)

# 2 arguments
@test map(foo, (), ()) === ()
@test map(foo, (1,), (1,)) === (2,)
@test map(foo, (1,2), (1,2)) === (2,4)
@test map(foo, (1,2,3,4), (1,2,3,4)) === (2,4,6,8)

# n arguments
@test map(foo, (), (), ()) === ()
@test map(foo, (), (1,2,3), (1,2,3)) === ()
@test map(foo, (1,), (1,), (1,)) === (3,)
@test map(foo, (1,2), (1,2), (1,2)) === (3,6)
@test map(foo, (1,2,3,4), (1,2,3,4), (1,2,3,4)) === (3,6,9,12)



## comparison ##
@test isequal((), ())
@test isequal((1,2,3), (1,2,3))
@test !isequal((1,2,3), (1,2,4))
@test !isequal((1,2,3), (1,2))

@test ==((), ())
@test ==((1,2,3), (1,2,3))
@test !==((1,2,3), (1,2,4))
@test !==((1,2,3), (1,2))

@test isless((1,2), (1,3))
@test isless((1,), (1,2))
@test !isless((1,2), (1,2))
@test !isless((2,1), (1,2))



## functions ##
@test isempty(())
@test !isempty((1,))

@test reverse(()) === ()
@test reverse((1,2,3)) === (3,2,1)




## specialized reduction ##
@test sum((1,2,3)) === 6

@test prod(()) === 1
@test prod((1,2,3)) === 6

@test all(()) === true
@test all((true, true)) === true
@test all((true, false)) === false
@test all((false, false)) === false

@test any(()) === false
@test any((true, true)) === true
@test any((true, false)) === true
@test any((false, false)) === false

@test @inferred(ntuple(Base.Abs2Fun(), Val{2})) == (1, 4)
@test @inferred(ntuple(Base.Abs2Fun(), Val{6})) == (1, 4, 9, 16, 25, 36)

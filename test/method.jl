using Test

@testset "Normal generated functions" begin
    @generated g1(x) = :(x)
    @test g1(1) == 1
    @test g1("hi") == "hi"
end

# Invalidating Generated Functions
# TODO: For some reason this doesn't work inside a testset right now (See below)
@generated foo() = bar()
bar() = 2
# We can still call foo() even though bar() was defined after! Hooray! :)
@test foo() == 2
# Now we can change bar(), and foo() is also updated! Woohoo! :D
bar() = 3
@test foo() == 3


@testset "invalidating generated functions in a testset" begin
    @generated foo() = bar()
    bar() = 2

    # TODO: It seems like this doesn't work becuase of the @testset. Is that expected?
    # Would this work for regular functions? I think it's broken...
    @test foo() == 2
    bar() = 3
    @test_broken foo() == 3
end


# Functions that take arguments
@generated f(x) = f2(x) + f2(x)
f2(x::Type) = sizeof(x)
@test f(1) == 16
f2(x::Type) = sizeof(x)÷2
@test f(1) == 8


# Method at bottom of call-stack accepts ::Type, not ::Any
# The simple case -- bar(::Any):
@generated foo(x) = bar(x)
bar(x) = 2
@test foo(1) == 2
bar(x) = 3
@test foo(1) == 3
# This also works, with t(::Type{Int})
@generated f_type(x) = t(x)
t(::Type{Int}) = 2
@test f_type(1) == 2
t(::Type{Int}) = 3
@test f_type(1) == 3
# Yet for some reason this does not work:
# Somehow having t1(T) call typemax prevents forming a backedge from t1 to the generator.
@generated f_type2(x) = t1(x)
t1(T) = typemax(T)
@test f_type2(1) == typemax(Int)
t1(T) = 3
@test_broken f_type2(1) == 3


## Functions with type params
#@generated f(x::T) where T<:Number = biggest(T)
#biggest(::Type{T}) where T = typemax(T)
#f(10)
## It also works for newly defined types
#struct MyNum <: Number x::Int end
#Base.typemax(::Type{MyNum}) = MyNum(typemax(Int))
#f(MyNum(10))
## And still allows users to interactively change their mind about these definitions
#Base.typemax(::Type{MyNum}) = MyNum(100)
#biggest(::Type{MyNum}) = MyNum(100)
#f(MyNum(10))
#

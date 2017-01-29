# This file is a part of Julia. License is MIT: http://julialang.org/license

# simple keyword args case
kwf1(ones; tens=0, hundreds=0) = ones + 10*tens + 100*hundreds
@test kwf1(2) == 2
@test kwf1(2, hundreds=6) == 602
@test kwf1(2, tens=6) == 62
@test kwf1(1, hundreds=2, tens=7) == 271
@test kwf1(3, tens=7, hundreds=2) == 273

@test_throws MethodError kwf1()             # no method, too few args
@test_throws MethodError kwf1(1, z=0)       # unsupported keyword
@test_throws MethodError kwf1(1, 2)         # no method, too many positional args

# keyword args plus varargs
kwf2(x, rest...; y=1) = (x, y, rest)
@test isequal(kwf2(0), (0, 1, ()))
@test isequal(kwf2(0,1,2), (0, 1, (1,2)))
@test isequal(kwf2(0,1,2,y=88), (0, 88, (1,2)))
@test isequal(kwf2(0,y=88,1,2), (0, 88, (1,2)))
@test_throws MethodError kwf2(0, z=1)
@test_throws MethodError kwf2(y=1)

# Test for #13919
test13919(x::Vararg{Int}; key=100) = (x, key)
@test test13919(1, 1)[1] === (1, 1)
@test test13919(1, 1)[2] === 100
@test test13919(1, 1, key=10)[1] === (1, 1)
@test test13919(1, 1, key=10)[2] === 10

# keyword arg with declared type
kwf3(x; y::Float64 = 1.0) = x + y
@test kwf3(2) == 3.0
@test kwf3(2, y=3.0) == 5.0
@test_throws TypeError kwf3(2, y=3)  # wrong type keyword

# function with only keyword args
kwf4(;a=1,b=2) = (a,b)
@test isequal(kwf4(), (1,2))
@test isequal(kwf4(b=10), (1,10))

# in-order evaluation of keyword args
kwf9(;read=true,write=!read) = (read,write)
@test kwf9() === (true,false)
@test kwf9(read=false) === (false,true)
@test kwf9(write=true) === (true,true)

# rest keywords
kwdelegator(ones;kw...) = kwf1(ones;kw...)
@test kwdelegator(4,hundreds=8) == 804

# optional positional args
opaf1(a,b=1,c=2,d=3) = (a,b,c,d)
@test isequal(opaf1(0), (0,1,2,3))
@test isequal(opaf1(0,2), (0,2,2,3))
@test isequal(opaf1(0,2,4), (0,2,4,3))
@test isequal(opaf1(0,2,4,6), (0,2,4,6))
@test_throws MethodError opaf1()
@test_throws MethodError opaf1(0,1,2,3,4)

# optional positional plus varargs
opaf2(a=1,rest...) = (a,rest)
@test isequal(opaf2(), (1,()))
@test isequal(opaf2(2), (2,()))
@test isequal(opaf2(2,3), (2,(3,)))

# optional positional plus keyword args
opkwf1(a=0,b=1;k=2) = (a,b,k)
@test isequal(opkwf1(), (0,1,2))
@test isequal(opkwf1(10), (10,1,2))
@test isequal(opkwf1(10,20), (10,20,2))
@test_throws MethodError opkwf1(10,20,30)
@test isequal(opkwf1(10,20,k=8), (10,20,8))
@test isequal(opkwf1(11;k=8),    (11, 1,8))
@test isequal(opkwf1(k=8),       ( 0, 1,8))

# dictionaries as keywords
@test kwf1(4; Dict(:hundreds=>9, :tens=>5)...) == 954

# with inner function
let
    function kwf_maker()
        f(;k=0) = 2k+1
    end
    kwf5 = kwf_maker()
    @test kwf5() == 1
    @test kwf5(k=2) == 5
    @test_throws MethodError kwf5(1)
end

# with every feature!
extravagant_args(x,y=0,rest...;color="blue",kw...) =
    (x,y,rest,color,kwf1(6;tens=8,kw...))

@test isequal(extravagant_args(1), (1,0,(),"blue",86))
@test isequal(extravagant_args(1;hundreds=7), (1,0,(),"blue",786))
@test isequal(extravagant_args(1,2,3;Dict(:color=>"red", :hundreds=>3)...),
              (1,2,(3,),"red",386))

# passing empty kw container to function with no kwargs
@test sin(1.0) == sin(1.0; Dict()...)
# issue #18845
@test (@eval sin(1.0; $([]...))) == sin(1.0)
f18845() = 2
@test f18845(;) == 2
@test f18845(; []...) == 2
@test (@eval f18845(; $([]...))) == 2

# passing junk kw container
@test_throws BoundsError extravagant_args(1; Any[[]]...)

# keyword args with static parmeters
kwf6{T}(x; k::T=1) = T
@test kwf6(1) === Int
@test kwf6(1;k=2.5) === Float64

kwf7{T}(x::T; k::T=1) = T
@test kwf7(2) === Int
@test kwf7(1.5;k=2.5) === Float64
@test_throws MethodError kwf7(1.5)
@test_throws TypeError kwf7(1.5;k=2)

# try to confuse it with quoted symbol
kwf8{T}(x::MIME{:T};k::T=0) = 0
@test kwf8(MIME{:T}()) === 0

# issue #4538
macro TEST4538()
    quote
        function $(esc(:test4538))(x=1)
            return x
        end
    end
end
@TEST4538
@test test4538() == 1
@test test4538(2) == 2

macro TEST4538_2()
    quote
        function $(esc(:test4538_2))(;x=1)
            return x
        end
    end
end
@TEST4538_2
@test test4538_2() == 1
@test_throws MethodError test4538_2(2)
@test test4538_2(x=2) == 2

# that, but in a module
module Foo4538
macro TEST()
    quote
        function $(esc(:test4538_foo_2))(;x=1)
            return x
        end
    end
end
end
@Foo4538.TEST()
@test test4538_foo_2() == 1
@test test4538_foo_2(x=2) == 2

f4538_3(;x=1) = x
macro TEST4538_3()
    quote
        x = 2
        f4538_3(x=3)
    end
end
@test (@TEST4538_3) == 3

# issue #4801
type T4801{X}
    T4801{X}(;k=0) where X = new()
end
@test isa(T4801{Any}(k=0), T4801{Any})

# issue #4974
function Foo4974(f; kwargs...)
    (f(), kwargs)
end

function test4974(;kwargs...)
    Foo4974(;kwargs...) do
        2
    end
end

@test test4974(a=1) == (2, [(:a, 1)])

# issue #7704, computed keywords
@test kwf1(1; (:tens, 2)) == 21
let
    p = (:hundreds, 3)
    q = (:tens, 1)
    @test kwf1(0; p, q) == 310
    @test kwf1(0; q, hundreds=4) == 410
end

# with anonymous functions, issue #2773
let f = (x;a=1,b=2)->(x, a, b)
    @test f(0) === (0, 1, 2)
    @test f(1,a=10,b=20) === (1,10,20)
    @test f(0,b=88) === (0, 1, 88)
    @test_throws MethodError f(0,z=1)
end
@test ((a=2)->10a)(3) == 30
@test ((a=2)->10a)() == 20
@test ((a=1,b=2)->(a,b))() == (1,2)
@test ((a=1,b=2)->(a,b))(5) == (5,2)
@test ((a=1,b=2)->(a,b))(5,6) == (5,6)

# issue #9948
f9948, getx9948 = let
    x = 3
    h(;x=x) = x
    getx() = x
    h, getx
end
@test_throws UndefVarError f9948()
@test getx9948() == 3
@test f9948(x=5) == 5
@test_throws UndefVarError f9948()
@test getx9948() == 3

# issue #17785 - handle all sources of kwargs left-to-right
g17785(; a=1, b=2) = (a, b)
let opts = (:a=>3, :b=>4)
    @test g17785(; a=5, opts...) == (3, 4)
    @test g17785(; opts..., a=5) == (5, 4)
    @test g17785(; opts..., a=5, b=6) == (5, 6)
    @test g17785(; b=0, opts..., a=5) == (5, 4)
end

# pr #18396, kwargs before Base is defined
@eval Core.Inference begin
    f18396(;kwargs...) = g18396(;kwargs...)
    g18396(;x=1,y=2) = x+y
end
@test Core.Inference.f18396() == 3

# issue #7045, `invoke` with keyword args
f7045(x::Float64; y=true) = y ? 1 : invoke(f7045,Tuple{Real},x,y=y)
f7045(x::Real; y=true) = y ? 2 : 3
@test f7045(1) === 2
@test f7045(1.0) === 1
@test f7045(1, y=false) === 3
@test f7045(1.0, y=false) === 3

# This file is a part of Julia. License is MIT: https://julialang.org/license

kwf1(ones; tens=0, hundreds=0) = ones + 10*tens + 100*hundreds
@testset "simple keyword args case" begin
    @test kwf1(2) == 2
    @test kwf1(2, hundreds=6) == 602
    @test kwf1(2, tens=6) == 62
    @test kwf1(1, hundreds=2, tens=7) == 271
    @test kwf1(3, tens=7, hundreds=2) == 273

    @test_throws MethodError kwf1()             # no method, too few args
    @test_throws MethodError kwf1(1, z=0)       # unsupported keyword
    @test_throws MethodError kwf1(1, 2)         # no method, too many positional args
end
kwf2(x, rest...; y=1) = (x, y, rest)
@testset "keyword args plus varargs" begin
    @test isequal(kwf2(0), (0, 1, ()))
    @test isequal(kwf2(0,1,2), (0, 1, (1,2)))
    @test isequal(kwf2(0,1,2,y=88), (0, 88, (1,2)))
    @test isequal(kwf2(0,y=88,1,2), (0, 88, (1,2)))
    @test_throws MethodError kwf2(0, z=1)
    @test_throws MethodError kwf2(y=1)
end
@testset "Issue #13919" begin
    test13919(x::Vararg{Int}; key=100) = (x, key)
    @test test13919(1, 1)[1] === (1, 1)
    @test test13919(1, 1)[2] === 100
    @test test13919(1, 1, key=10)[1] === (1, 1)
    @test test13919(1, 1, key=10)[2] === 10
end
@testset "keyword arg with declared type" begin
    kwf3(x; y::Float64 = 1.0) = x + y
    @test kwf3(2) == 3.0
    @test kwf3(2, y=3.0) == 5.0
    @test_throws TypeError kwf3(2, y=3)  # wrong type keyword
end
@testset "function with only keyword args" begin
    kwf4(;a=1,b=2) = (a,b)
    @test isequal(kwf4(), (1,2))
    @test isequal(kwf4(b=10), (1,10))
end
@testset "in-order evaluation of keyword args" begin
    kwf9(;read=true,write=!read) = (read,write)
    @test kwf9() === (true,false)
    @test kwf9(read=false) === (false,true)
    @test kwf9(write=true) === (true,true)
end
# rest keywords
kwdelegator(ones;kw...) = kwf1(ones;kw...)
@test kwdelegator(4,hundreds=8) == 804

@testset "optional positional args" begin
    opaf1(a,b=1,c=2,d=3) = (a,b,c,d)
    @test isequal(opaf1(0), (0,1,2,3))
    @test isequal(opaf1(0,2), (0,2,2,3))
    @test isequal(opaf1(0,2,4), (0,2,4,3))
    @test isequal(opaf1(0,2,4,6), (0,2,4,6))
    @test_throws MethodError opaf1()
    @test_throws MethodError opaf1(0,1,2,3,4)

    @testset "with varargs" begin
        opaf2(a=1,rest...) = (a,rest)
        @test isequal(opaf2(), (1,()))
        @test isequal(opaf2(2), (2,()))
        @test isequal(opaf2(2,3), (2,(3,)))
    end
    @testset "with keyword args" begin
        opkwf1(a=0,b=1;k=2) = (a,b,k)
        @test isequal(opkwf1(), (0,1,2))
        @test isequal(opkwf1(10), (10,1,2))
        @test isequal(opkwf1(10,20), (10,20,2))
        @test_throws MethodError opkwf1(10,20,30)
        @test isequal(opkwf1(10,20,k=8), (10,20,8))
        @test isequal(opkwf1(11;k=8),    (11, 1,8))
        @test isequal(opkwf1(k=8),       ( 0, 1,8))
    end
end
# dictionaries as keywords
@test kwf1(4; Dict(:hundreds=>9, :tens=>5)...) == 954

@testset "with inner function" begin
    function kwf_maker()
        f(;k=0) = 2k+1
    end
    kwf5 = kwf_maker()
    @test kwf5() == 1
    @test kwf5(k=2) == 5
    @test_throws MethodError kwf5(1)
end

extravagant_args(x,y=0,rest...;color="blue",kw...) = (x,y,rest,color,kwf1(6;tens=8,kw...))
@testset "with every feature!" begin
    @test isequal(extravagant_args(1), (1,0,(),"blue",86))
    @test isequal(extravagant_args(1;hundreds=7), (1,0,(),"blue",786))
    @test isequal(extravagant_args(1,2,3;Dict(:color=>"red", :hundreds=>3)...),
                  (1,2,(3,),"red",386))
    # passing junk kw container
    @test_throws BoundsError extravagant_args(1; Any[[]]...)
end
# passing empty kw container to function with no kwargs
@test sin(1.0) == sin(1.0; Dict()...)
f18845() = 2
@testset "issue #18845" begin
    @test (@eval sin(1.0; $([]...))) == sin(1.0)
    @test f18845(;) == 2
    @test f18845(; []...) == 2
    @test (@eval f18845(; $([]...))) == 2
end

@testset "keyword args with static parameters" begin
    kwf6(x; k::T=1) where {T} = T
    @test kwf6(1) === Int
    @test kwf6(1;k=2.5) === Float64

    kwf7(x::T; k::T=1) where {T} = T
    @test kwf7(2) === Int
    @test kwf7(1.5;k=2.5) === Float64
    @test_throws MethodError kwf7(1.5)
    @test_throws TypeError kwf7(1.5; k=2)
end
# try to confuse it with quoted symbol
kwf8(x::MIME{:T};k::T=0) where {T} = 0
@test kwf8(MIME{:T}()) === 0

macro TEST4538()
    quote
        function $(esc(:test4538))(x=1)
            return x
        end
    end
end

macro TEST4538_2()
    quote
        function $(esc(:test4538_2))(;x=1)
            return x
        end
    end
end

module Foo4538
macro TEST()
    quote
        function $(esc(:test4538_foo_2))(;x=1)
            return x
        end
    end
end
end

f4538_3(;x=1) = x
macro TEST4538_3()
    quote
        x = 2
        f4538_3(x=3)
    end
end
@testset "issue #4538" begin
    @TEST4538
    @test test4538() == 1
    @test test4538(2) == 2
    @TEST4538_2
    @test test4538_2() == 1
    @test_throws MethodError test4538_2(2)
    @test test4538_2(x=2) == 2

    # that, but in a module
    @Foo4538.TEST()
    @test test4538_foo_2() == 1
    @test test4538_foo_2(x=2) == 2

    @test (@TEST4538_3) == 3
end
# issue #4801
mutable struct T4801{X}
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

@test test4974(a=1) == (2, pairs((a=1,)))

@testset "issue #7704, computed keywords" begin
    @test kwf1(1; :tens => 2) == 21
    let p = (:hundreds, 3),
        q = (:tens, 1)
        @test kwf1(0; p[1]=>p[2], q[1]=>q[2]) == 310
        @test kwf1(0; q[1]=>q[2], hundreds=4) == 410
    end
end
@testset "with anonymous functions, issue #2773" begin
    f = (x;a=1,b=2)->(x, a, b)
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

@testset "issue #9948" begin
    f9948, getx9948 = let x
        x = 3
        h(;x=x) = x
        getx() = x
        h, getx
    end
    @test f9948() == 3
    @test getx9948() == 3
    @test f9948(x=5) == 5
    @test f9948() == 3
    @test getx9948() == 3
end
@testset "issue #17785 - handle all sources of kwargs left-to-right" begin
    g17785(; a=1, b=2) = (a, b)
    let opts = (:a=>3, :b=>4)
        @test g17785(; a=5, opts...) == (3, 4)
        @test g17785(; opts..., a=5) == (5, 4)
        @test g17785(; opts..., a=5, b=6) == (5, 6)
        @test g17785(; b=0, opts..., a=5) == (5, 4)
    end
end
# pr #18396, kwargs before Base is defined
@eval Core.Compiler begin
    f18396(;kwargs...) = g18396(;kwargs...)
    g18396(;x=1,y=2) = x+y
end
@test Core.Compiler.f18396() == 3
@testset "issue #7045, `invoke` with keyword args" begin
    f7045(x::Float64; y=true) = y ? 1 : invoke(f7045,Tuple{Real},x,y=y)
    f7045(x::Real; y=true) = y ? 2 : 3
    @test f7045(1) === 2
    @test f7045(1.0) === 1
    @test f7045(1, y=false) === 3
    @test f7045(1.0, y=false) === 3
end

struct T20804{T}
    y::T
end
(f::T20804)(;x=10) = f.y + x
@testset "issue #20804" begin
    x = T20804(4)
    @test x() == 14
    @test x(x=8) == 12
end
@testset "issue #21147" begin
    function f21147(f::Tuple{A}; kwargs...) where {B,A<:Tuple{B}}
        return B
    end
    @test f21147(((1,),)) === Int
    @test f21147(((1,),), k = 2) === Int
    function g21147(f::Tuple{A}, k = 2) where {B,A<:Tuple{B}}
        return B
    end
    @test g21147(((1,),)) === Int
    @test g21147(((1,),), 2) === Int
end
@testset "issue #21510" begin
    f21510(; Base.@nospecialize a = 2) = a
    @test f21510(a=:b) == :b
    @test f21510() == 2
end
@testset "issue #21518" begin
    a = 0
    f21518(;kw=nothing) = kw
    g21518() = (a+=1; f21518)
    g21518()(kw=1)
    @test a == 1
    g21518()(; :kw=>1)
    @test a == 2
end

@testset "issue #17240 - evaluate default expressions in nested scopes" begin
    a = 10
    f17240(;a=a-1, b=2a) = (a, b)
    @test f17240() == (9, 18)
    @test f17240(a=2) == (2, 4)
    @test f17240(b=3) == (9, 3)
    @test f17240(a=2, b=1) == (2, 1)
end

@testset "issue #9535 - evaluate all arguments left-to-right" begin
    counter = 0
    function get_next()
        counter += 1
        return counter
    end
    f(args...; kws...) = (args, values(kws))
    @test f(get_next(), a=get_next(), get_next(),
            b=get_next(), get_next(),
            [get_next(), get_next()]...; c=get_next(),
            (d = get_next(), f = get_next())...) ==
                ((1, 3, 5, 6, 7),
                 (a = 2, b = 4, c = 8, d = 9, f = 10))
end

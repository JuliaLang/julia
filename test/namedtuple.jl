# This file is a part of Julia. License is MIT: https://julialang.org/license

@test_throws TypeError NamedTuple{1,Tuple{}}
@test_throws TypeError NamedTuple{(),1}
@test_throws TypeError NamedTuple{(:a,1),Tuple{Int}}
@test_throws ErrorException NamedTuple{(:a,:b),Tuple{Int}}
@test_throws ErrorException NamedTuple{(:a,:b),Tuple{Int,Vararg{Int}}}
@test_throws ErrorException NamedTuple{(:a,),Union{Tuple{Int},Tuple{String}}}
@test_throws ErrorException NamedTuple{(:a,:a),Tuple{Int,Int}}
@test_throws ErrorException NamedTuple{(:a,:a)}((1,2))
@test_throws ErrorException NamedTuple{(:a, :b, :a), NTuple{3, Int}}((1, 2, 3))
@test_throws ArgumentError NamedTuple{(:a, :b, :c), NTuple{3, Int}}((1, 2))

@test fieldcount(NamedTuple{(:a,:b,:c)}) == 3
@test fieldcount(NamedTuple{<:Any,Tuple{Int,Int}}) == 2
@test_throws ArgumentError fieldcount(NamedTuple)
@test_throws ArgumentError fieldcount(NamedTuple{<:Any,<:Tuple{Int,Vararg{Int}}})

@test (a=1,).a == 1
@test (a=2,)[1] == 2
@test (a=3,)[:a] == 3
@test (x=4, y=5, z=6).y == 5
@test (x=4, y=5, z=6).z == 6
@test_throws ErrorException (x=4, y=5, z=6).a
@test_throws BoundsError (a=2,)[0]
@test_throws BoundsError (a=2,)[2]

@test length(NamedTuple()) == 0
@test length((a=1,)) == 1
@test length((a=1, b=0)) == 2

@test firstindex((a=1, b=0)) == 1
@test firstindex((a=1,)) == 1
@test firstindex(NamedTuple()) == 1
@test lastindex((a=1, b=0)) == 2
@test lastindex((a=1,)) == 1
@test lastindex(NamedTuple()) == 0

@test isempty(NamedTuple())
@test !isempty((a=1,))
@test empty((a=1,)) === NamedTuple()
@test isempty(empty((a=1,)))

@test (a=1,b=2) === (a=1,b=2)
@test (a=1,b=2) !== (b=1,a=2)

@test (a=1,b=2) == (a=1,b=2)
@test (a=1,b=2) != (b=1,a=2)
@test NamedTuple() === NamedTuple()
@test NamedTuple() != (a=1,)
@test !isequal(NamedTuple(), (a=1,))

@test string((a=1,)) == "(a = 1,)"
@test string((name="", day=:today)) == "(name = \"\", day = :today)"
@test string(NamedTuple()) == "NamedTuple()"

@test hash((a = 1, b = "hello")) == hash(NamedTuple{(:a,:b),Tuple{Int,String}}((1, "hello")))
@test hash((a = 1, b = "hello")) != hash(NamedTuple{(:a,:c),Tuple{Int,String}}((1, "hello")))
@test hash((a = 1, b = "hello")) != hash(NamedTuple{(:a,:b),Tuple{Int,String}}((1, "helo")))

@test NamedTuple{(:a,:b),Tuple{Int8,Int16}}((1,2)) === (a=Int8(1), b=Int16(2))
@test convert(NamedTuple{(:a,:b),Tuple{Int8,Int16}}, (a=3,b=4)) === (a=Int8(3), b=Int16(4))
let NT = NamedTuple{(:a,:b),Tuple{Int8,Int16}}, nt = (x=3,y=4)
    @test_throws MethodError convert(NT, nt)
end

@test NamedTuple{(:a,:c)}((b=1,z=2,c=3,aa=4,a=5)) === (a=5, c=3)
@test NamedTuple{(:a,)}(NamedTuple{(:b, :a), Tuple{Int, Union{Int,Nothing}}}((1, 2))) ===
    NamedTuple{(:a,), Tuple{Union{Int,Nothing}}}((2,))

@test eltype((a=[1,2], b=[3,4])) === Vector{Int}

@test Tuple((a=[1,2], b=[3,4])) == ([1,2], [3,4])
@test Tuple(NamedTuple()) === ()
@test Tuple((x=4, y=5, z=6)) == (4,5,6)
@test collect((x=4, y=5, z=6)) == [4,5,6]
@test Tuple((a=1, b=2, c=3)) == (1, 2, 3)

@test isless((a=1,b=2), (a=1,b=3))
@test_broken isless((a=1,), (a=1,b=2))
@test !isless((a=1,b=2), (a=1,b=2))
@test !isless((a=2,b=1), (a=1,b=2))
@test_throws MethodError isless((a=1,), (x=2,))

@test map(-, (x=1, y=2)) == (x=-1, y=-2)
@test map(+, (x=1, y=2), (x=10, y=20)) == (x=11, y=22)
@test_throws ArgumentError map(+, (x=1, y=2), (y=10, x=20))
@test map(string, (x=1, y=2)) == (x="1", y="2")
@test map(round, (x=UInt, y=Int), (x=3.1, y=2//3)) == (x=UInt(3), y=1)

@test merge((a=1, b=2), (a=10,)) == (a=10, b=2)
@test merge((a=1, b=2), (a=10, z=20)) == (a=10, b=2, z=20)
@test merge((a=1, b=2), (z=20,)) == (a=1, b=2, z=20)
@test merge(NamedTuple(), (a=2, b=1)) == (a=2, b=1)
@test merge((a=2, b=1), NamedTuple()) == (a=2, b=1)
@test merge(NamedTuple(), NamedTuple()) == NamedTuple()
# `merge` should preserve element types
let nt = merge(NamedTuple{(:a,:b),Tuple{Int32,Union{Int32,Nothing}}}((1,Int32(2))),
               NamedTuple{(:a,:c),Tuple{Union{Int8,Nothing},Float64}}((nothing,1.0)))
    @test typeof(nt) == NamedTuple{(:a,:b,:c),Tuple{Union{Int8,Nothing},Union{Int32,Nothing},Float64}}
    @test repr(nt) == "NamedTuple{(:a, :b, :c),Tuple{Union{Nothing, Int8},Union{Nothing, Int32},Float64}}((nothing, 2, 1.0))"
end

@test merge(NamedTuple(), [:a=>1, :b=>2, :c=>3, :a=>4, :c=>5]) == (a=4, b=2, c=5)
@test merge((c=0, z=1), [:a=>1, :b=>2, :c=>3, :a=>4, :c=>5]) == (c=5, z=1, a=4, b=2)

@test keys((a=1, b=2, c=3)) == (:a, :b, :c)
@test keys(NamedTuple()) == ()
@test keys((a=1,)) == (:a,)
@test values((a=1, b=2, c=3)) == (1, 2, 3)
@test values(NamedTuple()) == ()
@test values((a=1,)) == (1,)
@test haskey((a=1, b=2, c=3), :a)
@test !haskey(NamedTuple(), :a)
@test !haskey((a=1,), :b)
@test get((a=1, b=2, c=3), :a, 0) == 1
@test get(NamedTuple(), :a, 0) == 0
@test get((a=1,), :b, 0) == 0
@test get(()->0, (a=1, b=2, c=3), :a) == 1
@test get(()->0, NamedTuple(), :a) == 0
@test get(()->0, (a=1,), :b) == 0
@test Base.tail((a = 1, b = 2.0, c = 'x')) ≡ (b = 2.0, c = 'x')
@test Base.tail((a = 1, )) ≡ NamedTuple()
@test_throws ArgumentError Base.tail(NamedTuple())

# syntax errors

@test Meta.lower(Main, Meta.parse("(a=1, 0)")) == Expr(:error, "invalid named tuple element \"0\"")
@test Meta.lower(Main, Meta.parse("(a=1, f(x))")) == Expr(:error, "invalid named tuple element \"f(x)\"")
@test Meta.lower(Main, Meta.parse("(a=1,a=2)")) == Expr(:error, "field name \"a\" repeated in named tuple")
@test Meta.lower(Main, Meta.parse("(a=1,b=0,a=2)")) == Expr(:error, "field name \"a\" repeated in named tuple")
@test Meta.lower(Main, Meta.parse("(c=1,a=1,b=0,a=2)")) == Expr(:error, "field name \"a\" repeated in named tuple")

@test Meta.lower(Main, Meta.parse("(; f(x))")) == Expr(:error, "invalid named tuple element \"f(x)\"")
@test Meta.lower(Main, Meta.parse("(;1=0)")) == Expr(:error, "invalid named tuple field name \"1\"")

@test eval(Expr(:tuple, Expr(:parameters))) === NamedTuple()
@test Meta.lower(Main, Meta.parse("(1,;2)")) == Expr(:error, "unexpected semicolon in tuple")

# splatting

let d = [:a=>1, :b=>2, :c=>3]   # use an array to preserve order
    @test (d..., a=10) == (a=10, b=2, c=3)
    @test (a=0, b=0, z=1, d..., x=4, y=5) == (a=1, b=2, z=1, c=3, x=4, y=5)
    @test (a=0, (b=2,a=1)..., c=3) == (a=1, b=2, c=3)

    t = (x=1, y=20)
    @test (;d...) == (a=1, b=2, c=3)
    @test (;d..., :z=>20) == (a=1, b=2, c=3, z=20)
    @test (;a=10, d..., :c=>30) == (a=1, b=2, c=30)
    y = (w=30, z=40)
    @test (;t..., y...) == (x=1, y=20, w=30, z=40)
    @test (;t..., y=0, y...) == (x=1, y=0, w=30, z=40)
end

# inference tests

namedtuple_get_a(x) = x.a
@test Base.return_types(namedtuple_get_a, (NamedTuple,)) == Any[Any]
@test Base.return_types(namedtuple_get_a, (typeof((b=1,a="")),)) == Any[String]

namedtuple_fieldtype_a(x) = fieldtype(typeof(x), :a)
@test Base.return_types(namedtuple_fieldtype_a, (NamedTuple,)) == Any[Type]
@test Base.return_types(namedtuple_fieldtype_a, (typeof((b=1,a="")),)) == Any[Type{String}]
namedtuple_fieldtype__(x, y) = fieldtype(typeof(x), y)
@test Base.return_types(namedtuple_fieldtype__, (typeof((b=1,a="")),Symbol))[1] >: Union{Type{Int}, Type{String}}

namedtuple_nfields(x) = nfields(x) === 0 ? 1 : ""
@test Union{Int,String} <: Base.return_types(namedtuple_nfields, (NamedTuple,))[1]

function nt_from_abstractly_typed_array()
    a = NamedTuple[(a=3,b=5)]
    (getfield(a[1],1), getfield(a[1],2))
end
@test nt_from_abstractly_typed_array() === (3,5)

let T = NamedTuple{(:a, :b), Tuple{Int64, Union{Float64, Nothing}}}, nt = T((1, nothing))
    @test nt == (a=1, b=nothing)
    @test typeof(nt) == T
    @test convert(T, (a=1, b=nothing)) == nt
    @test typeof(convert(T, (a=1, b=nothing))) === T
end

function abstr_nt_22194()
    a = NamedTuple[(a=1,), (b=2,)]
    return (a[1].a, a[2].b)
end
@test abstr_nt_22194() == (1, 2)
@test Base.return_types(abstr_nt_22194, ()) == Any[Tuple{Any,Any}]
function abstr_nt_22194_2()
    a = NamedTuple[(a=1,), (b=2,)]
    return a[1].b
end
@test_throws ErrorException abstr_nt_22194_2()
@test Base.return_types(abstr_nt_22194_2, ()) == Any[Any]

mutable struct HasAbstractNamedTuples
    x::NamedTuple{(:a,:b)}
end

function abstr_nt_22194_3()
    s = HasAbstractNamedTuples((a="",b=8))
    @test s.x.a === ""
    @test s.x.b === 8
    s.x = (a=1,b=:b)
    @test s.x.a === 1
    @test s.x.b === :b
    @test isdefined(s.x, :a)
    @test isdefined(s.x, :b)
    @test !isdefined(s.x, :c)
    @test nfields(s) == 1
    @test isdefined(s, :x)
    @test fieldtype(typeof(s), 1) == fieldtype(typeof(s), :x) == NamedTuple{(:a,:b)}
    @test fieldtype(typeof(s.x), :a) === Int
    @test fieldtype(typeof(s.x), :b) === Symbol
    return s.x.b
end
abstr_nt_22194_3()
@test Base.return_types(abstr_nt_22194_3, ()) == Any[Any]

@test Base.structdiff((a=1, b=2), (b=3,)) == (a=1,)
@test Base.structdiff((a=1, b=2, z=20), (b=3,)) == (a=1, z=20)
@test Base.structdiff((a=1, b=2, z=20), (b=3, q=20, z=1)) == (a=1,)
@test Base.structdiff((a=1, b=2, z=20), (b=3, q=20, z=1, a=0)) == NamedTuple()
@test Base.structdiff((a=1, b=2, z=20), NamedTuple{(:b,)}) == (a=1, z=20)
@test typeof(Base.structdiff(NamedTuple{(:a, :b), Tuple{Int32, Union{Int32, Nothing}}}((1, Int32(2))),
                             (a=0,))) === NamedTuple{(:b,), Tuple{Union{Int32, Nothing}}}

@test findall(isequal(1), (a=1, b=2)) == [:a]
@test findall(isequal(1), (a=1, b=1)) == [:a, :b]
@test isempty(findall(isequal(1), NamedTuple()))
@test isempty(findall(isequal(1), (a=2, b=3)))
@test findfirst(isequal(1), (a=1, b=2)) == :a
@test findlast(isequal(1), (a=1, b=2)) == :a
@test findfirst(isequal(1), (a=1, b=1)) == :a
@test findlast(isequal(1), (a=1, b=1)) == :b
@test findfirst(isequal(1), ()) === nothing
@test findlast(isequal(1), ()) === nothing
@test findfirst(isequal(1), (a=2, b=3)) === nothing
@test findlast(isequal(1), (a=2, b=3)) === nothing

# Test map with Nothing and Missing
for T in (Nothing, Missing)
    x = [(a=1, b=T()), (a=1, b=2)]
    y = map(v -> (a=v.a, b=v.b), [(a=1, b=T()), (a=1, b=2)])
    @test y isa Vector{NamedTuple{(:a,:b), T} where T<:Tuple}
    @test isequal(x, y)
end
y = map(v -> (a=v.a, b=v.a + v.b), [(a=1, b=missing), (a=1, b=2)])
@test y isa Vector{NamedTuple{(:a,:b), T} where T<:Tuple}
@test isequal(y, [(a=1, b=missing), (a=1, b=3)])

# issue #27187
@test reduce(merge,[(a = 1, b = 2), (c = 3, d = 4)]) == (a = 1, b = 2, c = 3, d = 4)
@test typeintersect(NamedTuple{()}, NamedTuple{names, Tuple{Int,Int}} where names) == Union{}

# Iterator constructor
@test NamedTuple{(:a, :b), Tuple{Int, Float64}}(Any[1.0, 2]) === (a=1, b=2.0)
@test NamedTuple{(:a, :b)}(Any[1.0, 2]) === (a=1.0, b=2)

# Left-associative merge, issue #29215
@test merge((a=1, b=2), (b=3, c=4), (c=5,)) === (a=1, b=3, c=5)
@test merge((a=1, b=2), (b=3, c=(d=1,)), (c=(d=2,),)) === (a=1, b=3, c=(d=2,))
@test merge((a=1, b=2)) === (a=1, b=2)

# issue #33270
let n = NamedTuple{(:T,), Tuple{Type{Float64}}}((Float64,))
    @test n isa NamedTuple{(:T,), Tuple{Type{Float64}}}
    @test n.T === Float64
end

# setindex
let nt0 = NamedTuple(), nt1 = (a=33,), nt2 = (a=0, b=:v)
    @test Base.setindex(nt0, 33, :a) == nt1
    @test Base.setindex(Base.setindex(nt1, 0, :a), :v, :b) == nt2
    @test Base.setindex(nt1, "value", :a) == (a="value",)
    @test Base.setindex(nt1, "value", :a) isa NamedTuple{(:a,),<:Tuple{AbstractString}}
end

# @NamedTuple
@testset "@NamedTuple" begin
    @test @NamedTuple{a::Int, b::String} === NamedTuple{(:a, :b),Tuple{Int,String}} ===
        @NamedTuple begin
            a::Int
            b::String
        end
    @test @NamedTuple{a::Int, b} === NamedTuple{(:a, :b),Tuple{Int,Any}}
    @test_throws LoadError include_string(Main, "@NamedTuple{a::Int, b, 3}")
    @test_throws LoadError include_string(Main, "@NamedTuple(a::Int, b)")
end

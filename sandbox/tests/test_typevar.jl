# Tests for Base_show(io, ::TypeVar) display
include("../show_type.jl")

using Test

@testset "TypeVar display" begin
    @test sprint(Base_show, TypeVar(:V, Signed, Real)) == "Signed<:V<:Real"
end

@testset "UnionAll display" begin
    @test Base_repr(UnionAll) == "UnionAll"
end

let x = TypeVar(:_), y = TypeVar(:_)
    @test Base_repr(UnionAll(x, UnionAll(y, Pair{x,y}))) == "Pair"
    @test Base_repr(UnionAll(y, UnionAll(x, Pair{x,y}))) == "Pair{_2, _1} where {_1, _2}"
    @test Base_repr(UnionAll(x, UnionAll(y, Pair{UnionAll(x,Ref{x}),y}))) == "Pair{Ref}"
    @test Base_repr(UnionAll(y, UnionAll(x, Pair{UnionAll(y,Ref{x}),y}))) == "Pair{Ref{_2}, _1} where {_1, _2}"
end

let x, y, z
    x = TypeVar(:a)
    y = TypeVar(:a)
    z = TypeVar(:a)
    @test Base_repr(UnionAll(z, UnionAll(x, UnionAll(y, Tuple{x,y,z})))) == "Tuple{a1, a2, a} where {a, a1, a2}"
    @test Base_repr(UnionAll(z, UnionAll(x, UnionAll(y, Tuple{z,y,x})))) == "Tuple{a, a2, a1} where {a, a1, a2}"
end

let x = TypeVar(:_, Number), y = TypeVar(:_, Number)
    @test Base_repr(UnionAll(x, UnionAll(y, Pair{x,y}))) == "Pair{_1, _2} where {_1<:Number, _2<:Number}"
    @test Base_repr(UnionAll(y, UnionAll(x, Pair{x,y}))) == "Pair{_2, _1} where {_1<:Number, _2<:Number}"
    @test Base_repr(UnionAll(x, UnionAll(y, Pair{UnionAll(x,Ref{x}),y}))) == "Pair{Ref{_1} where _1<:Number, _1} where _1<:Number"
    @test Base_repr(UnionAll(y, UnionAll(x, Pair{UnionAll(y,Ref{x}),y}))) == "Pair{Ref{_2}, _1} where {_1<:Number, _2<:Number}"
end

is_juliarepr(x) = eval(Meta.parse(Base_repr(x))) == x
@testset "unionall types" begin
    X = TypeVar(gensym())
    Y = TypeVar(gensym(), Ref, Ref)
    x, y, z = TypeVar(:a), TypeVar(:a), TypeVar(:a)
    struct TestTVUpper_sandbox{A<:Integer} end

    # named typevars
    @test is_juliarepr(Ref{A} where A)
    @test is_juliarepr(Ref{A} where A>:Ref)
    @test is_juliarepr(Ref{A} where A<:Ref)
    @test is_juliarepr(Ref{A} where Ref<:A<:Ref)
    @test is_juliarepr(TestTVUpper_sandbox{<:Real})
    @test is_juliarepr(TestTVUpper_sandbox{<:Integer})
    @test is_juliarepr(TestTVUpper_sandbox{<:Signed})

    # typearg order
    @test is_juliarepr(UnionAll(X, Pair{X,<:Any}))
    @test is_juliarepr(UnionAll(X, Pair{<:Any,X}))

    # duplicates
    @test is_juliarepr(UnionAll(X, Pair{X,X}))

    # nesting
    @test is_juliarepr(UnionAll(X, Ref{Ref{X}}))
    @test is_juliarepr(Union{T, Int} where T)
    @test is_juliarepr(Pair{A, <:A} where A)

    # renumbered typevars with same names
    @test is_juliarepr(UnionAll(z, UnionAll(x, UnionAll(y, Tuple{x,y,z}))))

    # shortened typevar printing
    @test Base_repr(Ref{<:Any}) == "Ref"
    @test Base_repr(Pair{1, <:Any}) == "Pair{1}"
    @test Base_repr(Ref{<:Number}) == "Ref{<:Number}"
    @test Base_repr(Pair{1, <:Number}) == "Pair{1, <:Number}"
    @test Base_repr(Ref{<:Ref}) == "Ref{<:Ref}"
    @test Base_repr(Ref{>:Ref}) == "Ref{>:Ref}"
    @test Base_repr(Pair{<:Any, 1}) == "Pair{<:Any, 1}"
    yname = sprint(Base_show_unquoted, Y.name)
    @test Base_repr(UnionAll(Y, Ref{Y})) == "Ref{$yname} where Ref<:$yname<:Ref"
    @test endswith(Base_repr(TestTVUpper_sandbox{<:Real}), "TestTVUpper_sandbox{<:Real}")
    @test endswith(Base_repr(TestTVUpper_sandbox), "TestTVUpper_sandbox")
    @test endswith(Base_repr(TestTVUpper_sandbox{<:Signed}), "TestTVUpper_sandbox{<:Signed}")

    # exception for tuples
    @test is_juliarepr(Tuple)
    @test is_juliarepr(Tuple{})
    @test is_juliarepr(Tuple{<:Any})
end

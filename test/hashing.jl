# This file is a part of Julia. License is MIT: https://julialang.org/license

using Random, LinearAlgebra, SparseArrays
isdefined(Main, :OffsetArrays) || @eval Main include("testhelpers/OffsetArrays.jl")
using .Main.OffsetArrays

types = Any[
    Bool,
    Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Float32, Float64,
    Rational{Int8}, Rational{UInt8}, Rational{Int16}, Rational{UInt16},
    Rational{Int32}, Rational{UInt32}, Rational{Int64}, Rational{UInt64}
]
vals = vcat(
    typemin(Int64),
    -Int64(maxintfloat(Float64)) .+ Int64[-4:1;],
    typemin(Int32),
    -Integer(maxintfloat(Float32)) .+ (-4:1),
    -2:2,
    Integer(maxintfloat(Float32)) .+ (-1:4),
    typemax(Int32),
    Int64(maxintfloat(Float64)) .+ Int64[-1:4;],
    typemax(Int64),
)

function coerce(T::Type, x)
    if T<:Rational
        convert(T, coerce(typeof(numerator(zero(T))), x))
    elseif !(T<:Integer)
        convert(T, x)
    else
        x % T
    end
end

for T = types[2:end],
    x = vals,
    a = coerce(T, x)
    @test hash(a,zero(UInt)) == invoke(hash, Tuple{Real, UInt}, a, zero(UInt))
    @test hash(a,one(UInt)) == invoke(hash, Tuple{Real, UInt}, a, one(UInt))
end

for T = types,
    S = types,
    x = vals,
    a = coerce(T, x),
    b = coerce(S, x)
    #println("$(typeof(a)) $a")
    #println("$(typeof(b)) $b")
    @test isequal(a,b) == (hash(a)==hash(b))
    # for y=vals
    #     println("T=$T; S=$S; x=$x; y=$y")
    #     c = convert(T,x//y)
    #     d = convert(S,x//y)
    #     @test !isequal(a,b) || hash(a)==hash(b)
    # end
end

# issue #8619
@test hash(nextfloat(2.0^63)) == hash(UInt64(nextfloat(2.0^63)))
@test hash(prevfloat(2.0^64)) == hash(UInt64(prevfloat(2.0^64)))

# issue #9264
@test hash(1//6,zero(UInt)) == invoke(hash, Tuple{Real, UInt}, 1//6, zero(UInt))
@test hash(1//6) == hash(big(1)//big(6))
@test hash(1//6) == hash(0x01//0x06)

# hashing collections (e.g. issue #6870)
vals = Any[
    [1,2,3,4], [1 3;2 4], Any[1,2,3,4], [1,3,2,4],
    [1.0, 2.0, 3.0, 4.0], BigInt[1, 2, 3, 4],
    [1,0], [true,false], BitArray([true,false]),
    # Irrationals
    Any[1, pi], [1, pi], [pi, pi], Any[pi, pi],
    # Overflow with Int8
    Any[Int8(127), Int8(-128), -383], 127:-255:-383,
    # Loss of precision with Float64
    Any[-Int64(2)^53-1, 0.0, Int64(2)^53+1], [-Int64(2)^53-1, 0, Int64(2)^53+1],
        (-Int64(2)^53-1):Int64(2)^53+1:(Int64(2)^53+1),
    # Some combinations of elements support -, others do not
    [1, 2, "a"], [1, "a", 2], [1, 2, "a", 2], [1, 'a', 2],
    Set([1,2,3,4]),
    Set([1:10;]),                # these lead to different key orders
    Set([7,9,4,10,2,3,5,8,6,1]), #
    Dict(42 => 101, 77 => 93), Dict{Any,Any}(42 => 101, 77 => 93),
    (1,2,3,4), (1.0,2.0,3.0,4.0), (1,3,2,4),
    ("a","b"), (SubString("a",1,1), SubString("b",1,1)),
    # issue #6900
    Dict(x => x for x in 1:10),
    Dict(7=>7,9=>9,4=>4,10=>10,2=>2,3=>3,8=>8,5=>5,6=>6,1=>1),
    [], [1], [2], [1, 1], [1, 2], [1, 3], [2, 2], [1, 2, 2], [1, 3, 3],
    zeros(2, 2), spzeros(2, 2), Matrix(1.0I, 2, 2), sparse(1.0I, 2, 2),
    sparse(fill(1., 2, 2)), fill(1., 2, 2), sparse([0 0; 1 0]), [0 0; 1 0],
    [-0. 0; -0. 0.], SparseMatrixCSC(2, 2, [1, 3, 3], [1, 2], [-0., -0.]),
    # issue #16364
    1:4, 1:1:4, 1:-1:0, 1.0:4.0, 1.0:1.0:4.0, range(1, stop=4, length=4),
    # issue #35597, when `LinearIndices` does not begin at 1
    Base.IdentityUnitRange(2:4),
    OffsetArray(1:4, -2),
    OffsetArray([1 3; 2 4], -2, 2),
    OffsetArray(1:4, 0),
    OffsetArray([1 3; 2 4], 0, 0),
    'a':'e', ['a', 'b', 'c', 'd', 'e'],
    # check that hash is still consistent with heterogeneous arrays for which - is defined
    # for some pairs and not others
    ["a", "b", 1, 2], ["a", 1, 2], ["a", "b", 2, 2], ["a", "a", 1, 2], ["a", "b", 2, 3]
]

for a in vals, b in vals
    @test isequal(a,b) == (hash(a)==hash(b))
end

for a in vals
    a isa AbstractArray || continue
    if keys(a) == keys(Array(a))
        @test hash(a) == hash(Array(a)) == hash(Array{Any}(a))
    else
        @test hash(a) == hash(OffsetArray(Array(a), (first.(axes(a)).-1)...)) == hash(OffsetArray(Array{Any}(a), (first.(axes(a)).-1)...))
    end
end

vals = Any[
    Int[], Float64[],
    [0], [1], [2],
    # test vectors starting with ranges
    [1, 2], [1, 2, 3], [1, 2, 3, 4], [1, 2, 3, 4, 5], [1, 2, 3, 4, 5, 6],
    [2, 1], [3, 2, 1], [4, 3, 2, 1], [5, 4, 3, 2, 1], [5, 4, 3, 2, 1, 0, -1],
    # test vectors starting with ranges which trigger overflow with Int8
    [124, 125, 126, 127], [124, 125, 126, 127, -128], [-128, 127, -128],
    # test vectors including ranges
    [2, 1, 2, 3], [2, 3, 2, 1], [2, 1, 2, 3, 2], [2, 3, 2, 1, 2],
    # test various sparsity patterns
    [0, 0], [0, 0, 0], [0, 1], [1, 0],
    [0, 0, 1], [0, 1, 0], [1, 0, 0], [0, 1, 2],
    [0 0; 0 0], [1 0; 0 0], [0 1; 0 0], [0 0; 1 0], [0 0; 0 1],
    [5 1; 0 0], [1 1; 0 1], [0 2; 3 0], [0 2; 4 6], [4 0; 0 1],
    [0 0 0; 0 0 0], [1 0 0; 0 0 1], [0 0 2; 3 0 0], [0 0 7; 6 1 2],
    [4 0 0; 3 0 1], [0 2 4; 6 0 0],
    # various stored zeros patterns
    sparse([1], [1], [0]), sparse([1], [1], [-0.0]),
    sparse([1, 2], [1, 1], [-0.0, 0.0]), sparse([1, 2], [1, 1], [0.0, -0.0]),
    sparse([1, 2], [1, 1], [-0.0, 0.0], 3, 1), sparse([1, 2], [1, 1], [0.0, -0.0], 3, 1),
    sparse([1, 3], [1, 1], [-0.0, 0.0], 3, 1), sparse([1, 3], [1, 1], [0.0, -0.0], 3, 1),
    sparse([1, 2, 3], [1, 1, 1], [-1, 0, 1], 3, 1), sparse([1, 2, 3], [1, 1, 1], [-1.0, -0.0, 1.0], 3, 1),
    sparse([1, 3], [1, 1], [-1, 0], 3, 1), sparse([1, 2], [1, 1], [-1, 0], 3, 1)
]

for a in vals
    b = Array(a)
    @test hash(convert(Array{Any}, a)) == hash(b)
    @test hash(convert(Array{supertype(eltype(a))}, a)) == hash(b)
    @test hash(convert(Array{Float64}, a)) == hash(b)
    @test hash(sparse(a)) == hash(b)
    if !any(x -> isequal(x, -0.0), a)
        @test hash(convert(Array{Int}, a)) == hash(b)
        if all(x -> typemin(Int8) <= x <= typemax(Int8), a)
            @test hash(convert(Array{Int8}, a)) == hash(b)
        end
    end
end

# Test that overflow does not give inconsistent hashes with heterogeneous arrays
@test hash(Any[Int8(1), Int8(2), 255]) == hash([1, 2, 255])
@test hash(Any[Int8(127), Int8(-128), 129, 130]) ==
    hash([127, -128, 129, 130]) != hash([127,  128, 129, 130])

# Test hashing sparse matrix with type which does not support -
struct CustomHashReal
    x::Float64
end
Base.hash(x::CustomHashReal, h::UInt) = hash(x.x, h)
Base.:(==)(x::CustomHashReal, y::Number) = x.x == y
Base.:(==)(x::Number, y::CustomHashReal) = x == y.x
Base.zero(::Type{CustomHashReal}) = CustomHashReal(0.0)
Base.zero(x::CustomHashReal) = zero(CustomHashReal)

let a = sparse([CustomHashReal(0), CustomHashReal(3), CustomHashReal(3)])
    @test hash(a) == hash(Array(a))
end

vals = Any[
    0.0:0.1:0.3, 0.3:-0.1:0.0,
    0:-1:1, 0.0:-1.0:1.0, 0.0:1.1:10.0, -4:10,
    'a':'e', 'b':'a',
    range(1, stop=1, length=1), range(0.3, stop=1.0, length=3),  range(1, stop=1.1, length=20)
]

for a in vals
    @test hash(Array(a)) == hash(a)
end

@test hash(SubString("--hello--",3,7)) == hash("hello")
@test hash(:(X.x)) == hash(:(X.x))
@test hash(:(X.x)) != hash(:(X.y))

@test hash([1,2]) == hash(view([1,2,3,4],1:2))

let a = QuoteNode(1), b = QuoteNode(1.0)
    @test (hash(a)==hash(b)) == (a==b)
end

let a = Expr(:block, Core.TypedSlot(1, Any)),
    b = Expr(:block, Core.TypedSlot(1, Any)),
    c = Expr(:block, Core.TypedSlot(3, Any))
    @test a == b && hash(a) == hash(b)
    @test a != c && hash(a) != hash(c)
    @test b != c && hash(b) != hash(c)
end

@test hash(Dict(),hash(Set())) != hash(Set(),hash(Dict()))

# issue 15659
for prec in [3, 11, 15, 16, 31, 32, 33, 63, 64, 65, 254, 255, 256, 257, 258, 1023, 1024, 1025],
    v in Any[-0.0, 0, 1, -1, 1//10, 2//10, 3//10, 1//2, pi]
    setprecision(prec) do
        x = convert(BigFloat, v)
        @test precision(x) == prec
        num, pow, den = Base.decompose(x)
        y = num*big(2.0)^pow/den
        @test precision(y) == prec
        @test isequal(x, y)
    end
end

# issue #20744
@test hash(:c, hash(:b, hash(:a))) != hash(:a, hash(:b, hash(:c)))

# issue #5849, objectid of types
@test Vector === (Array{T,1} where T)
@test (Pair{A,B} where A where B) !== (Pair{A,B} where B where A)
let vals_expr = :(Any[Vector, (Array{T,1} where T), 1, 2, Union{Int, String}, Union{String, Int},
                      (Union{String, T} where T), Ref{Ref{T} where T}, (Ref{Ref{T}} where T),
                      (Vector{T} where T<:Real), (Vector{T} where T<:Integer),
                      (Vector{T} where T>:Integer),
                      (Pair{A,B} where A where B), (Pair{A,B} where B where A)])
    vals_a = eval(vals_expr)
    vals_b = eval(vals_expr)
    for (i, a) in enumerate(vals_a), (j, b) in enumerate(vals_b)
        @test i != j || (a === b)
        @test (a === b) == (objectid(a) == objectid(b))
    end
end

# issue #26038
let p1 = Ptr{Int8}(1), p2 = Ptr{Int32}(1), p3 = Ptr{Int8}(2)
    @test p1 == p2
    @test !isequal(p1, p2)
    @test p1 != p3
    @test hash(p1) != hash(p2)
    @test hash(p1) != hash(p3)
    @test hash(p1) == hash(Ptr{Int8}(1))

    @test p1 < p3
    @test !(p1 < p2)
    @test isless(p1, p3)
    @test_throws MethodError isless(p1, p2)
end

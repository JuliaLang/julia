# This file is a part of Julia. License is MIT: http://julialang.org/license

types = Any[
    Bool,
    Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Float32, Float64,
    Rational{Int8}, Rational{UInt8}, Rational{Int16}, Rational{UInt16},
    Rational{Int32}, Rational{UInt32}, Rational{Int64}, Rational{UInt64}
]
vals = vcat(
    typemin(Int64),
    -Int64(maxintfloat(Float64))+Int64[-4:1;],
    typemin(Int32),
    -Integer(maxintfloat(Float32))+(-4:1),
    -2:2,
    Integer(maxintfloat(Float32))+(-1:4),
    typemax(Int32),
    Int64(maxintfloat(Float64))+Int64[-1:4;],
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

for T=types[2:end], x=vals
    a = coerce(T,x)
    @test hash(a,zero(UInt)) == invoke(hash, Tuple{Real, UInt}, a, zero(UInt))
end

for T=types, S=types, x=vals
    a = coerce(T,x)
    b = coerce(S,x)
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
    [1,0], [true,false], BitArray([true,false]),
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
    zeros(2, 2), spzeros(2, 2), eye(2, 2), speye(2, 2),
    sparse(ones(2, 2)), ones(2, 2), sparse([0 0; 1 0]), [0 0; 1 0],
    [-0. 0; -0. 0.], SparseMatrixCSC(2, 2, [1, 3, 3], [1, 2], [-0., -0.])
]

for a in vals, b in vals
    @test isequal(a,b) == (hash(a)==hash(b))
end

@test hash(SubString("--hello--",3,7)) == hash("hello")
@test hash(:(X.x)) == hash(:(X.x))
@test hash(:(X.x)) != hash(:(X.y))

@test hash([1,2]) == hash(view([1,2,3,4],1:2))

# test explicit zeros in SparseMatrixCSC
x = sprand(10, 10, 0.5)
x[1] = 1
x.nzval[1] = 0
@test hash(x) == hash(Array(x))

let a = QuoteNode(1), b = QuoteNode(1.0)
    @test (hash(a)==hash(b)) == (a==b)
end

let a = Expr(:block, TypedSlot(1, Any)),
    b = Expr(:block, TypedSlot(1, Any)),
    c = Expr(:block, TypedSlot(3, Any))
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

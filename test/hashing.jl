types = Any[
    Bool, Char,
    Int8, Uint8, Int16, Uint16, Int32, Uint32, Int64, Uint64, Float32, Float64,
    Rational{Int8}, Rational{Uint8}, Rational{Int16}, Rational{Uint16},
    Rational{Int32}, Rational{Uint32}, Rational{Int64}, Rational{Uint64}
]
vals = [
    typemin(Int64),
    -int64(maxintfloat(Float64))+Int64[-4:1],
    typemin(Int32),
    -integer(maxintfloat(Float32))+(-4:1),
    -2:2,
    integer(maxintfloat(Float32))+(-1:4),
    typemax(Int32),
    int64(maxintfloat(Float64))+Int64[-1:4],
    typemax(Int64),
]

function coerce(T::Type, x)
    if T<:Rational
        return convert(T, coerce(typeof(num(zero(T))), x))
    end
    if !(T<:Integer) || T===Bool
        convert(T, x)
    elseif sizeof(T) < sizeof(x)
        x % T
    elseif sizeof(T) == sizeof(x)
        reinterpret(T, x)
    else
        convert(T, x)
    end
end

for T=types, S=types, x=vals
    a = coerce(T,x)
    b = coerce(S,x)
    if (isa(a,Char) && !is_valid_char(a)) || (isa(b,Char) && !is_valid_char(b))
        continue
    end
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
@test hash(nextfloat(2.0^63)) == hash(uint64(nextfloat(2.0^63)))
@test hash(prevfloat(2.0^64)) == hash(uint64(prevfloat(2.0^64)))

# hashing collections (e.g. issue #6870)
vals = Any[
    [1,2,3,4], [1 3;2 4], Any[1,2,3,4], [1,3,2,4],
    [1,0], [true,false], bitpack([true,false]),
    Set([1,2,3,4]),
    Set([1:10]),                 # these lead to different key orders
    Set([7,9,4,10,2,3,5,8,6,1]), #
    Dict(42 => 101, 77 => 93), Dict{Any,Any}(42 => 101, 77 => 93),
    (1,2,3,4), (1.0,2.0,3.0,4.0), (1,3,2,4),
    ("a","b"), (SubString("a",1,1), SubString("b",1,1)),
    # issue #6900
    [x => x for x in 1:10],
    Dict(7=>7,9=>9,4=>4,10=>10,2=>2,3=>3,8=>8,5=>5,6=>6,1=>1)
]

for a in vals, b in vals
    @test isequal(a,b) == (hash(a)==hash(b))
end

@test hash(RopeString("1","2")) == hash("12")
@test hash(SubString("--hello--",3,7)) == hash("hello")
@test hash(:(X.x)) == hash(:(X.x))
@test hash(:(X.x)) != hash(:(X.y))

@test hash([1,2]) == hash(sub([1,2,3,4],1:2))

# This file is a part of Julia. License is MIT: http://julialang.org/license

# Pair
p = Pair(10,20)
@test p == (10=>20)
@test isequal(p,10=>20)
@test start(p) == 1
@test next(p, 1) == (10,2)
@test !done(p, 1)
@test !done(p,2)
@test done(p,3)
@test !done(p,0)
@test endof(p) == length(p) == 2
@test Base.indexed_next(p, 1, (1,2)) == (10,2)
@test Base.indexed_next(p, 2, (1,2)) == (20,3)
@test (1=>2) < (2=>3)
@test (2=>2) < (2=>3)
@test !((2=>3) < (2=>3))
@test (2=>3) < (4=>3)
@test (1=>100) < (4=>1)
@test p[1] == 10
@test p[2] == 20
@test_throws BoundsError p[3]
@test_throws BoundsError p[false]
@test p[true] == 10
@test p[2.0] == 20
@test p[0x01] == 10
@test_throws InexactError p[2.3]
@test first(p) == 10
@test last(p) == 20
@test eltype(p) == Int
@test eltype(4 => 5.6) == Union{Int,Float64}

# Dict
h = Dict()
for i=1:10000
    h[i] = i+1
end
for i=1:10000
    @test (h[i] == i+1)
end
for i=1:2:10000
    delete!(h, i)
end
for i=1:2:10000
    h[i] = i+1
end
for i=1:10000
    @test (h[i] == i+1)
end
for i=1:10000
    delete!(h, i)
end
@test isempty(h)
h[77] = 100
@test h[77] == 100
for i=1:10000
    h[i] = i+1
end
for i=1:2:10000
    delete!(h, i)
end
for i=10001:20000
    h[i] = i+1
end
for i=2:2:10000
    @test h[i] == i+1
end
for i=10000:20000
    @test h[i] == i+1
end
h = Dict{Any,Any}("a" => 3)
@test h["a"] == 3
h["a","b"] = 4
@test h["a","b"] == h[("a","b")] == 4
h["a","b","c"] = 4
@test h["a","b","c"] == h[("a","b","c")] == 4

# Test eltype, keytype and valtype
@test eltype(h) == Pair{Any,Any}
@test keytype(h) == Any
@test valtype(h) == Any

let td = Dict{AbstractString,Float64}()
    @test eltype(td) == Pair{AbstractString,Float64}
    @test keytype(td) == AbstractString
    @test valtype(td) == Float64
    @test keytype(Dict{AbstractString,Float64}) === AbstractString
    @test valtype(Dict{AbstractString,Float64}) === Float64
end

let z = Dict()
    get_KeyError = false
    try
        z["a"]
    catch _e123_
        get_KeyError = isa(_e123_,KeyError)
    end
    @test get_KeyError
end

_d = Dict("a"=>0)
@test isa([k for k in filter(x->length(x)==1, collect(keys(_d)))], Vector{String})

let d = Dict(((1, 2), (3, 4)))
    @test d[1] === 2
    @test d[3] === 4
    d2 = Dict(1 => 2, 3 => 4)
    d3 = Dict((1 => 2, 3 => 4))
    @test d == d2 == d3
    @test typeof(d) == typeof(d2) == typeof(d3) == Dict{Int,Int}

    d = Dict(((1, 2), (3, "b")))
    @test d[1] === 2
    @test d[3] == "b"
    d2 = Dict(1 => 2, 3 => "b")
    d3 = Dict((1 => 2, 3 => "b"))
    @test d == d2 == d3
    @test typeof(d) == typeof(d2) == typeof(d3) == Dict{Int,Any}

    d = Dict(((1, 2), ("a", 4)))
    @test d[1] === 2
    @test d["a"] === 4
    d2 = Dict(1 => 2, "a" => 4)
    d3 = Dict((1 => 2, "a" => 4))
    @test d == d2 == d3
    @test typeof(d) == typeof(d2) == typeof(d3) == Dict{Any,Int}

    d = Dict(((1, 2), ("a", "b")))
    @test d[1] === 2
    @test d["a"] == "b"
    d2 = Dict(1 => 2, "a" => "b")
    d3 = Dict((1 => 2, "a" => "b"))
    @test d == d2 == d3
    @test typeof(d) == typeof(d2) == typeof(d3) == Dict{Any,Any}
end

@test_throws ArgumentError first(Dict())
@test first(Dict(:f=>2)) == (:f=>2)

# constructing Dicts from iterators
let d = @inferred Dict(i=>i for i=1:3)
    @test isa(d, Dict{Int,Int})
    @test d == Dict(1=>1, 2=>2, 3=>3)
end
let d = Dict(i==1 ? (1=>2) : (2.0=>3.0) for i=1:2)
    @test isa(d, Dict{Real,Real})
    @test d == Dict{Real,Real}(2.0=>3.0, 1=>2)
end

@test_throws KeyError Dict("a"=>2)[Base.secret_table_token]

# issue #1821
let d = Dict{String, Vector{Int}}()
    d["a"] = [1, 2]
    @test_throws MethodError d["b"] = 1
    @test isa(repr(d), AbstractString)  # check that printable without error
end

# issue #2344
let
    local bar
    bestkey(d, key) = key
    bestkey{K<:AbstractString,V}(d::Associative{K,V}, key) = string(key)
    bar(x) = bestkey(x, :y)
    @test bar(Dict(:x => [1,2,5])) == :y
    @test bar(Dict("x" => [1,2,5])) == "y"
end

# issue #1438
type I1438T
    id
end
import Base.hash
hash(x::I1438T, h::UInt) = hash(x.id, h)

let
    local seq, xs, s
    seq = [26,28,29,30,31,32,33,34,35,36,-32,-35,-34,-28,37,38,39,40,-30,
           -31,41,42,43,44,-33,-36,45,46,47,48,-37,-38,49,50,51,52,-46,-50,53]
    xs = [ I1438T(id) for id=1:53 ]
    s = Set()
    for id in seq
        if id > 0
            x = xs[id]
            push!(s, x)
            @test in(x, s)                 # check that x can be found
        else
            delete!(s, xs[-id])
        end
    end
end

@test  isequal(Dict(), Dict())
@test  isequal(Dict(1 => 1), Dict(1 => 1))
@test !isequal(Dict(1 => 1), Dict())
@test !isequal(Dict(1 => 1), Dict(1 => 2))
@test !isequal(Dict(1 => 1), Dict(2 => 1))

# Generate some data to populate dicts to be compared
data_in = [ (rand(1:1000), randstring(2)) for _ in 1:1001 ]

# Populate the first dict
d1 = Dict{Int, AbstractString}()
for (k,v) in data_in
    d1[k] = v
end
data_in = collect(d1)
# shuffle the data
for i in 1:length(data_in)
    j = rand(1:length(data_in))
    data_in[i], data_in[j] = data_in[j], data_in[i]
end
# Inserting data in different (shuffled) order should result in
# equivalent dict.
d2 = Dict{Int, AbstractString}()
for (k,v) in data_in
    d2[k] = v
end

@test  isequal(d1, d2)
d3 = copy(d2)
d4 = copy(d2)
# Removing an item gives different dict
delete!(d1, data_in[rand(1:length(data_in))][1])
@test !isequal(d1, d2)
# Changing a value gives different dict
d3[data_in[rand(1:length(data_in))][1]] = randstring(3)
!isequal(d1, d3)
# Adding a pair gives different dict
d4[1001] = randstring(3)
@test !isequal(d1, d4)

@test isequal(Dict(), sizehint!(Dict(),96))

# Here is what currently happens when dictionaries of different types
# are compared. This is not necessarily desirable. These tests are
# descriptive rather than proscriptive.
@test !isequal(Dict(1 => 2), Dict("dog" => "bone"))
@test isequal(Dict{Int,Int}(), Dict{AbstractString,AbstractString}())

# get! (get with default values assigned to the given location)
let f(x) = x^2, d = Dict(8=>19)
    @test get!(d, 8, 5) == 19
    @test get!(d, 19, 2) == 2

    @test get!(d, 42) do  # d is updated with f(2)
        f(2)
    end == 4

    @test get!(d, 42) do  # d is not updated
        f(200)
    end == 4

    @test get(d, 13) do   # d is not updated
        f(4)
    end == 16

    @test d == Dict(8=>19, 19=>2, 42=>4)
end

# show
for d in (Dict("\n" => "\n", "1" => "\n", "\n" => "2"),
          Dict(string(i) => i for i = 1:30),
          Dict(reshape(1:i^2,i,i) => reshape(1:i^2,i,i) for i = 1:24),
          Dict(String(Char['α':'α'+i;]) => String(Char['α':'α'+i;]) for i = (1:10)*10),
          Dict("key" => zeros(0, 0)))
    for cols in (12, 40, 80), rows in (2, 10, 24)
        # Ensure output is limited as requested
        s = IOBuffer()
        io = Base.IOContext(s, limit=true, displaysize=(rows, cols))
        Base.show(io, MIME("text/plain"), d)
        out = split(String(take!(s)),'\n')
        for line in out[2:end]
            @test strwidth(line) <= cols
        end
        @test length(out) <= rows

        for f in (keys, values)
            s = IOBuffer()
            io = Base.IOContext(s, limit=true, displaysize=(rows, cols))
            Base.show(io, MIME("text/plain"), f(d))
            out = split(String(take!(s)),'\n')
            for line in out[2:end]
                @test strwidth(line) <= cols
            end
            @test length(out) <= rows
        end
    end
    # Simply ensure these do not throw errors
    Base.show(IOBuffer(), d)
    @test !isempty(summary(d))
    @test !isempty(summary(keys(d)))
    @test !isempty(summary(values(d)))
end

# Issue #15739 - Compact REPL printouts of an `Associative` use brackets when appropriate
let d = Dict((1=>2) => (3=>45), (3=>10) => (10=>11))
    buf = IOBuffer()
    showcompact(buf, d)

    # Check explicitly for the expected strings, since the CPU bitness effects
    # dictionary ordering.
    result = String(buf)
    @test contains(result, "Dict")
    @test contains(result, "(1=>2)=>(3=>45)")
    @test contains(result, "(3=>10)=>(10=>11)")
end

# issue #9463
type Alpha end
Base.show(io::IO, ::Alpha) = print(io,"α")
let sbuff = IOBuffer(),
    io = Base.IOContext(sbuff, limit=true, displaysize=(10, 20))

    Base.show(io, MIME("text/plain"), Dict(Alpha()=>1))
    @test !contains(String(sbuff), "…")
    @test endswith(String(sbuff), "α => 1")
end

# issue #2540
let d = Dict{Any,Any}(Dict(x => 1 for x in ['a', 'b', 'c']))
    @test d == Dict('a'=>1, 'b'=>1, 'c'=> 1)
end

# issue #2629
let d = Dict{AbstractString,AbstractString}(Dict( a => "foo" for a in ["a","b","c"]))
    @test d == Dict("a"=>"foo","b"=>"foo","c"=>"foo")
end

# issue #5886
d5886 = Dict()
for k5886 in 1:11
   d5886[k5886] = 1
end
for k5886 in keys(d5886)
   # undefined ref if not fixed
   d5886[k5886] += 1
end

# issue #8877
let a = Dict("foo"  => 0.0, "bar" => 42.0),
        b = Dict("フー" => 17, "バー" => 4711)
    @test typeof(merge(a, b)) === Dict{String,Float64}
end

# issue 9295
let d = Dict()
    @test push!(d, 'a' => 1) === d
    @test d['a'] == 1
    @test push!(d, 'b' => 2, 'c' => 3) === d
    @test d['b'] == 2
    @test d['c'] == 3
    @test push!(d, 'd' => 4, 'e' => 5, 'f' => 6) === d
    @test d['d'] == 4
    @test d['e'] == 5
    @test d['f'] == 6
    @test length(d) == 6
end

# issue #10647
type T10647{T}; x::T; end
let a = ObjectIdDict()
    a[1] = a
    a[a] = 2
    a[3] = T10647(a)
    @test a == a
    show(IOBuffer(), a)
    Base.show(Base.IOContext(IOBuffer(), :limit => true), a)
    Base.show(IOBuffer(), a)
    Base.show(Base.IOContext(IOBuffer(), :limit => true), a)
end

let
    a = ObjectIdDict()
    a[1] = a
    a[a] = 2

    sa = similar(a)
    @test isempty(sa)
    @test isa(sa, ObjectIdDict)

    @test length(a) == 2
    @test 1 in keys(a)
    @test a in keys(a)
    @test a[1] === a
    @test a[a] === 2

    ca = copy(a)
    @test length(ca) == length(a)
    @test ca == a
    @test ca !== a # make sure they are different objects

    ca = empty!(ca)
    @test length(ca) == 0
    @test length(a) == 2
end

@test length(ObjectIdDict(1=>2, 1.0=>3)) == 2
@test length(Dict(1=>2, 1.0=>3)) == 1

let d = @inferred ObjectIdDict(i=>i for i=1:3)
    @test isa(d, ObjectIdDict)
    @test d == ObjectIdDict(1=>1, 2=>2, 3=>3)
end

let d = @inferred ObjectIdDict(Pair(1,1), Pair(2,2), Pair(3,3))
    @test isa(d, ObjectIdDict)
    @test d == ObjectIdDict(1=>1, 2=>2, 3=>3)
    @test eltype(d) == Pair{Any,Any}
end

# Issue #7944
let d = Dict{Int,Int}()
    get!(d, 0) do
        d[0] = 1
    end
    @test length(d) == 1
end

# iteration
d = Dict('a'=>1, 'b'=>1, 'c'=> 3)
@test [d[k] for k in keys(d)] == [d[k] for k in eachindex(d)] ==
      [v for (k, v) in d] == [d[x[1]] for (i, x) in enumerate(d)]

# generators, similar
d = Dict(:a=>"a")
@test @inferred(map(identity, d)) == d
@test @inferred(map(p->p.first=>p.second[1], d)) == Dict(:a=>'a')
@test_throws ArgumentError map(p->p.second, d)

# Issue 12451
@test_throws ArgumentError Dict(0)
@test_throws ArgumentError Dict([1])
@test_throws ArgumentError Dict([(1,2),0])

# test Dict constructor's argument checking (for an iterable of pairs or tuples)
# make sure other errors can propagate when the nature of the iterator is not the problem
@test_throws InexactError Dict(convert(Int,1.5) for i=1:1)
@test_throws InexactError WeakKeyDict(convert(Int,1.5) for i=1:1)

# ImmutableDict
import Base.ImmutableDict
let d = ImmutableDict{String, String}(),
    k1 = "key1",
    k2 = "key2",
    v1 = "value1",
    v2 = "value2",
    d1 = ImmutableDict(d, k1 => v1),
    d2 = ImmutableDict(d1, k2 => v2),
    d3 = ImmutableDict(d2, k1 => v2),
    d4 = ImmutableDict(d3, k2 => v1),
    dnan = ImmutableDict{String, Float64}(k2, NaN),
    dnum = ImmutableDict(dnan, k2 => 1)

    @test isempty(collect(d))
    @test !isempty(collect(d1))
    @test isempty(d)
    @test !isempty(d1)
    @test length(d) == 0
    @test length(d1) == 1
    @test length(d2) == 2
    @test length(d3) == 3
    @test length(d4) == 4
    @test !(k1 in keys(d))
    @test k1 in keys(d1)
    @test k1 in keys(d2)
    @test k1 in keys(d3)
    @test k1 in keys(d4)

    @test !haskey(d, k1)
    @test haskey(d1, k1)
    @test haskey(d2, k1)
    @test haskey(d3, k1)
    @test haskey(d4, k1)
    @test !(k2 in keys(d1))
    @test k2 in keys(d2)
    @test !(k1 in values(d4))
    @test v1 in values(d4)
    @test collect(d1) == [Pair(k1, v1)]
    @test collect(d4) == reverse([Pair(k1, v1), Pair(k2, v2), Pair(k1, v2), Pair(k2, v1)])
    @test d1 == ImmutableDict(d, k1 => v1)
    @test !((k1 => v2) in d2)
    @test (k1 => v2) in d3
    @test (k1 => v1) in d4
    @test (k1 => v2) in d4
    @test !in(k2 => "value2", d4, ===)
    @test in(k2 => v2, d4, ===)
    @test in(k2 => NaN, dnan, isequal)
    @test in(k2 => NaN, dnan, ===)
    @test !in(k2 => NaN, dnan, ==)
    @test !in(k2 => 1, dnum, ===)
    @test in(k2 => 1.0, dnum, ===)
    @test !in(k2 => 1, dnum, <)
    @test in(k2 => 0, dnum, <)
    @test get(d1, "key1", :default) === v1
    @test get(d4, "key1", :default) === v2
    @test get(d4, "foo", :default) === :default
    @test get(d, k1, :default) === :default
    @test d1["key1"] === v1
    @test d4["key1"] === v2
    @test similar(d3) === d
    @test similar(d) === d

    @test_throws KeyError d[k1]
    @test_throws KeyError d1["key2"]
end

# filtering
let d = Dict(zip(1:1000,1:1000)), f = (k,v) -> iseven(k)
    @test filter(f, d) == filter!(f, copy(d)) ==
          invoke(filter!, Tuple{Function,Associative}, f, copy(d)) ==
          Dict(zip(2:2:1000, 2:2:1000))
end

# issue #15077
immutable MyString <: AbstractString
    str::String
end
import Base.==

const global hashoffset = [UInt(190)]

Base.hash(s::MyString) = hash(s.str) + hashoffset[]
Base.endof(s::MyString) = endof(s.str)
Base.next(s::MyString, v::Int) = next(s.str, v)
Base.isequal(a::MyString, b::MyString) = isequal(a.str, b.str)
==(a::MyString, b::MyString) = (a.str == b.str)

let badKeys = [
    "FINO_emv5.0","FINO_ema0.1","RATE_ema1.0","NIBPM_ema1.0",
    "SAO2_emv5.0","O2FLOW_ema5.0","preop_Neuro/Psych_","gender_",
    "FIO2_ema0.1","PEAK_ema5.0","preop_Reproductive_denies","O2FLOW_ema0.1",
    "preop_Endocrine_denies","preop_Respiratory_",
    "NIBPM_ema0.1","PROPOFOL_MCG/KG/MIN_decay5.0","NIBPD_ema1.0","NIBPS_ema5.0",
    "anesthesiaStartTime","NIBPS_ema1.0","RESPRATE_ema1.0","PEAK_ema0.1",
    "preop_GU_denies","preop_Cardiovascular_","PIP_ema5.0","preop_ENT_denies",
    "preop_Skin_denies","preop_Renal_denies","asaCode_IIIE","N2OFLOW_emv5.0",
    "NIBPD_emv5.0", # <--- here is the key that we later can't find
    "NIBPM_ema5.0","preop_Respiratory_complete","ETCO2_ema5.0",
    "RESPRATE_ema0.1","preop_Functional Status_<2","preop_Renal_symptoms",
    "ECGRATE_ema5.0","FIO2_emv5.0","RESPRATE_emv5.0","7wu3ty0a4fs","BVO",
    "4UrCWXUsaT"
]
    d = Dict{AbstractString,Int}()
    for i = 1:length(badKeys)
        d[badKeys[i]] = i
    end
    # Check all keys for missing values
    for i = 1:length(badKeys)
        @test d[badKeys[i]] == i
    end

    # Walk through all possible hash values (mod size of hash table)
    for offset = 0:1023
        d2 = Dict{MyString,Int}()
        hashoffset[] = offset
        for i = 1:length(badKeys)
            d2[MyString(badKeys[i])] = i
        end
        # Check all keys for missing values
        for i = 1:length(badKeys)
            @test d2[MyString(badKeys[i])] == i
        end
    end
end

immutable MyInt <: Integer
    val::UInt
end

Base.hash(v::MyInt) = v.val + hashoffset[]
Base.endof(v::MyInt) = endof(v.val)
Base.next(v::MyInt, i::Int) = next(v.val, i)
Base.isequal(a::MyInt, b::MyInt) = isequal(a.val, b.val)
==(a::MyInt, b::MyInt) = (a.val == b.val)

let badKeys = UInt16[0xb800,0xa501,0xcdff,0x6303,0xe40a,0xcf0e,0xf3df,0xae99,0x9913,0x741c,
                     0xd01f,0xc822,0x9723,0xb7a0,0xea25,0x7423,0x6029,0x202a,0x822b,0x492c,
                     0xd02c,0x862d,0x8f34,0xe529,0xf938,0x4f39,0xd03a,0x473b,0x1e3b,0x1d3a,
                     0xcc39,0x7339,0xcf40,0x8740,0x813d,0xe640,0xc443,0x6344,0x3744,0x2c3d,
                     0x8c48,0xdf49,0x5743]
    # Walk through all possible hash values (mod size of hash table)
    for offset = 0:1023
        d2 = Dict{MyInt, Int}()
        hashoffset[] = offset
        for i = 1:length(badKeys)
            d2[MyInt(badKeys[i])] = i
        end
        # Check all keys for missing values
        for i = 1:length(badKeys)
            @test d2[MyInt(badKeys[i])] == i
        end
    end
end

# #18213
Dict(1 => rand(2,3), 'c' => "asdf") # just make sure this does not trigger a deprecation

@testset "WeakKeyDict" begin
    A = [1]
    B = [2]
    C = [3]
    local x = 0
    local y = 0
    local z = 0
    finalizer(A, a->(x+=1))
    finalizer(B, b->(y+=1))
    finalizer(C, c->(z+=1))

    # construction
    wkd = WeakKeyDict()
    wkd[A] = 2
    wkd[B] = 3
    wkd[C] = 4
    dd = convert(Dict{Any,Any},wkd)
    @test WeakKeyDict(dd) == wkd
    @test isa(WeakKeyDict(dd), WeakKeyDict{Any,Any})
    @test WeakKeyDict(A=>2, B=>3, C=>4) == wkd
    @test isa(WeakKeyDict(A=>2, B=>3, C=>4), WeakKeyDict{Array{Int,1},Int})
    @test WeakKeyDict(a=>i+1 for (i,a) in enumerate([A,B,C]) ) == wkd
    @test WeakKeyDict([(A,2), (B,3), (C,4)]) == wkd


    @test length(wkd) == 3
    @test !isempty(wkd)
    res = pop!(wkd, C)
    @test res == 4
    @test C ∉ keys(wkd)
    @test 4 ∉ values(wkd)
    @test length(wkd) == 2
    @test !isempty(wkd)
    wkd = filter!( (k,v) -> k != B, wkd)
    @test B ∉ keys(wkd)
    @test 3 ∉ values(wkd)
    @test length(wkd) == 1
    @test !isempty(wkd)

    wkd = empty!(wkd)
    @test wkd == similar(wkd)
    @test typeof(wkd) == typeof(similar(wkd))
    @test length(wkd) == 0
    @test isempty(wkd)
    @test isa(wkd, WeakKeyDict)
end

@testset "issue #19995, hash of dicts" begin
    @test hash(Dict(Dict(1=>2) => 3, Dict(4=>5) => 6)) != hash(Dict(Dict(4=>5) => 3, Dict(1=>2) => 6))
    a = Dict(Dict(3 => 4, 2 => 3) => 2, Dict(1 => 2, 5 => 6) => 1)
    b = Dict(Dict(1 => 2, 2 => 3, 5 => 6) => 1, Dict(3 => 4) => 2)
    @test hash(a) != hash(b)
end

type Foo_15776
    x::Vector{Pair{Tuple{Function, Vararg{Int}}, Int}}
end
@testset "issue #15776, convert for pair" begin
    z = [Pair((+,1,5,7), 3), Pair((-,6,5,3,5,8), 1)]
    f = Foo_15776(z)
    @test f.x[1].first == (+, 1, 5, 7)
    @test f.x[1].second == 3
    @test f.x[2].first == (-, 6, 5, 3, 5, 8)
    @test f.x[2].second == 1
end

@testset "issue #18708 error type for dict constructor" begin
    @test_throws UndefVarError Dict(x => y for x in 1:10)
end

type Error19179 <: Exception
end

@testset "issue #19179 throwing error in dict constructor" begin
    @test_throws Error19179 Dict(i => throw(Error19179()) for i in 1:10)
end

# issue #18090
let
    d = Dict(i => i^2 for i in 1:10_000)
    z = zip(keys(d), values(d))
    for (pair, tupl) in zip(d, z)
        @test pair[1] == tupl[1] && pair[2] == tupl[2]
    end
end
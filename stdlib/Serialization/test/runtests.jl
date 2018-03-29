# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Random, Serialization

# Check that serializer hasn't gone out-of-frame
@test Serialization.sertag(Symbol) == 1
@test Serialization.sertag(()) == 68
@test Serialization.sertag(false) == 76

function create_serialization_stream(f::Function)
    s = IOBuffer()
    f(s)
    close(s)
end

# Tags
create_serialization_stream() do s
    Serialization.writetag(s, Serialization.sertag(Bool))
    @test take!(s)[end] == UInt8(Serialization.sertag(Bool))
end

create_serialization_stream() do s
    Serialization.write_as_tag(s, Serialization.sertag(Bool))
    @test take!(s)[end] == UInt8(Serialization.sertag(Bool))
end

create_serialization_stream() do s
    Serialization.write_as_tag(s, Serialization.sertag(Symbol))
    data = take!(s)
    @test data[end-1] == 0x00
    @test data[end] == UInt8(Serialization.sertag(Symbol))
end

# SubString

create_serialization_stream() do s
    ss = Any[SubString("12345", 2, 4),
             SubString(GenericString("12345"), 2, 4),
             SubString(s"12345", 2, 4),
             SubString(GenericString(s"12345"), 2, 4),]
    for x in ss
        serialize(s, x)
    end
    seek(s, 0)
    for x in ss
        y = deserialize(s)
        @test x == y
        @test typeof(x) == typeof(y)
    end
end

# Boolean & Empty & Nothing
create_serialization_stream() do s
    serialize(s, true)
    serialize(s, ())
    serialize(s, nothing)
    seek(s, 0)
    @test deserialize(s) === true
    @test deserialize(s) === ()
    @test deserialize(s) === nothing
end

# Ptr
create_serialization_stream() do s
    serialize(s, C_NULL)
    seekstart(s)
    @test deserialize(s) === C_NULL
end

# Integer
create_serialization_stream() do s
    serialize(s, 0x1)
    serialize(s, 32)
    serialize(s, 33)
    seek(s,0)
    @test deserialize(s) === 0x01
    @test deserialize(s) === 32
    @test deserialize(s) === 33
end

# Tuple
create_serialization_stream() do s
    tpl = (0x1,'a')
    serialize(s, tpl)

    len = 257
    lt = ntuple(i->0x1, len)
    serialize(s, lt)

    serialize(s, Tuple{})

    seek(s, 0)
    @test deserialize(s) === tpl
    @test deserialize(s) === lt
    @test deserialize(s) === Tuple{}
end

# Dict
create_serialization_stream() do s
    dct = Dict("A"=>1, "B"=>2)
    serialize(s, dct)
    seek(s, 0)
    @test deserialize(s) == dct
end

# Symbol
create_serialization_stream() do s
    gensym(len) = Symbol(repeat("A", len))

    smbl = gensym(1)
    serialize(s, smbl)

    smbl2 = gensym(10)
    serialize(s, smbl2)

    smbl3 = gensym(257)
    serialize(s, smbl3)

    seek(s, 0)
    @test deserialize(s) === smbl
    @test deserialize(s) === smbl2
    @test deserialize(s) === smbl3
end

# Module
create_serialization_stream() do s # user-defined module
    mod = b"SomeModule"
    modstring = String(mod)
    eval(Meta.parse("module $(modstring); end"))
    modtype = eval(Meta.parse("$(modstring)"))
    serialize(s, modtype)
    seek(s, 0)
    @test deserialize(s) === modtype
end

# DataType
create_serialization_stream() do s # standard types
    typ1 = UInt8
    serialize(s, typ1)
    typ2 = Bool
    serialize(s, typ2)

    seek(s, 0)
    @test deserialize(s) == typ1
    @test deserialize(s) == typ2
end

create_serialization_stream() do s # user-defined type
    usertype = "SerializeSomeType"
    eval(Meta.parse("abstract type $(usertype) end"))
    utype = eval(Meta.parse("$(usertype)"))
    serialize(s, utype)
    seek(s, 0)
    @test deserialize(s) === utype
end

create_serialization_stream() do s # user-defined type
    usertype = "SerializeSomeType1"
    eval(Meta.parse("mutable struct $(usertype); end"))
    utype = eval(Meta.parse("$(usertype)"))
    serialize(s, utype)
    seek(s, 0)
    @test deserialize(s) === utype
end

create_serialization_stream() do s # user-defined type
    usertype = "SerializeSomeType2"
    eval(Meta.parse("abstract type $(usertype){T} end"))
    utype = eval(Meta.parse("$(usertype)"))
    serialize(s, utype)
    seek(s, 0)
    @test deserialize(s) == utype
end

create_serialization_stream() do s # immutable struct with 1 field
    usertype = "SerializeSomeType3"
    eval(Meta.parse("struct $(usertype){T}; a::T; end"))
    utype = eval(Meta.parse("$(usertype)"))
    serialize(s, utype)
    seek(s, 0)
    @test deserialize(s) == utype
end

create_serialization_stream() do s # immutable struct with 2 field
    usertype = "SerializeSomeType4"
    eval(Meta.parse("struct $(usertype){T}; a::T; b::T; end"))
    utval = eval(Meta.parse("$(usertype)(1,2)"))
    serialize(s, utval)
    seek(s, 0)
    @test deserialize(s) === utval
end

create_serialization_stream() do s # immutable struct with 3 field
    usertype = "SerializeSomeType5"
    eval(Meta.parse("struct $(usertype){T}; a::T; b::T; c::T; end"))
    utval = eval(Meta.parse("$(usertype)(1,2,3)"))
    serialize(s, utval)
    seek(s, 0)
    @test deserialize(s) === utval
end

create_serialization_stream() do s # immutable struct with 4 field
    usertype = "SerializeSomeType6"
    eval(Meta.parse("struct $(usertype){T}; a::T; b::T; c::T; d::T; end"))
    utval = eval(Meta.parse("$(usertype)(1,2,3,4)"))
    serialize(s, utval)
    seek(s, 0)
    @test deserialize(s) === utval
end

# Expression
create_serialization_stream() do s
    expr = Meta.parse("a = 1")
    serialize(s, expr)

    expr2 = Meta.parse(repeat("a = 1;", 300))
    serialize(s, expr2)

    seek(s, 0)
    @test deserialize(s) == expr
    @test deserialize(s) == expr2
end

# Array
mutable struct TA1
    v::UInt8
end
create_serialization_stream() do s # small 1d array
    arr1 = fill(0x01, 10)    # small 1d array
    serialize(s, arr1)
    arr2 = fill(0x01, 3, 3)  # small 2d array
    serialize(s, arr2)
    arr3 = fill(0x0001, 50)  # large 1d array
    serialize(s, arr3)
    arr4 = reshape([true, false, false, false, true, false, false, false, true], 3, 3)
    serialize(s, arr4)       # boolean array

    arr5 = Vector{TA1}(undef, 3)
    arr5[2] = TA1(0x01)
    serialize(s, arr5)

    seek(s, 0)
    @test deserialize(s) == arr1
    @test deserialize(s) == arr2
    @test deserialize(s) == arr3
    @test deserialize(s) == arr4

    result = deserialize(s)
    @test !isassigned(result,1)
    @test !isassigned(result,3)
    @test result[2].v == arr5[2].v
end

# SubArray
create_serialization_stream() do s # slices
    slc1 = view(UInt8[1,1,1,1], 2:3)
    serialize(s, slc1)
    slc2 = view(repeat(UInt8[1,2,3,4], 1, 4), 1, 2:4)
    serialize(s, slc2)

    seek(s, 0)
    @test deserialize(s) == slc1
    @test deserialize(s) == slc2
end

# Objects that have a SubArray as a type in a type-parameter list
module ArrayWrappers

struct ArrayWrapper{T,N,A<:AbstractArray} <: AbstractArray{T,N}
    data::A
end
ArrayWrapper(data::AbstractArray{T,N}) where {T,N} = ArrayWrapper{T,N,typeof(data)}(data)
Base.size(A::ArrayWrapper) = size(A.data)
Base.size(A::ArrayWrapper, d) = size(A.data, d)
Base.getindex(A::ArrayWrapper, i::Real...) = getindex(A.data, i...)

end

let A = rand(3,4)
    for B in (view(A, :, 2:4), view(A, 2, 1:3))
        C = ArrayWrappers.ArrayWrapper(B)
        io = IOBuffer()
        serialize(io, C)
        seek(io, 0)
        Cd = deserialize(io)
        @test size(Cd) == size(C)
        @test Cd == B
    end
end

# Function
serialize_test_function() = 1
serialize_test_function2 = ()->1
create_serialization_stream() do s # Base generic function
    serialize(s, sin)
    serialize(s, typeof)
    serialize(s, serialize_test_function)
    serialize(s, serialize_test_function2)

    seek(s, 0)
    @test deserialize(s) === sin
    @test deserialize(s) === typeof
    @test deserialize(s)() === 1
    @test deserialize(s)() === 1
end

# Anonymous Functions
main_ex = quote
    using Serialization
    $create_serialization_stream() do s
        local g() = :magic_token_anon_fun_test
        serialize(s, g)
        serialize(s, g)

        seekstart(s)
        ds = Serializer(s)
        local g2 = deserialize(ds)
        $Test.@test g2 !== g
        $Test.@test g2() == :magic_token_anon_fun_test
        $Test.@test g2() == :magic_token_anon_fun_test
        $Test.@test deserialize(ds) === g2

        # issue #21793
        y = x -> (() -> x)
        seekstart(s)
        serialize(s, y)
        seekstart(s)
        y2 = deserialize(s)
        x2 = y2(2)
        $Test.@test x2() == 2
    end
end
# This needs to be run on `Main` since the serializer treats it differently.
eval(Main, main_ex)

# Task
create_serialization_stream() do s # user-defined type array
    f = () -> begin task_local_storage(:v, 2); return 1+1 end
    t = Task(f)
    Base._wait(schedule(t))
    serialize(s, t)
    seek(s, 0)
    r = deserialize(s)
    @test r.storage[:v] == 2
    @test r.state == :done
    @test r.exception === nothing
end

struct MyErrorTypeTest <: Exception end
create_serialization_stream() do s # user-defined type array
    t = Task(()->throw(MyErrorTypeTest()))
    @test_throws MyErrorTypeTest Base._wait(schedule(t))
    serialize(s, t)
    seek(s, 0)
    r = deserialize(s)
    @test r.state == :failed
    @test isa(t.exception, MyErrorTypeTest)
end

# corner case: undefined inside immutable struct
struct MyNullable{T}
    hasvalue::Bool
    value::T

    MyNullable{T}() where {T} = new(false)
end
create_serialization_stream() do s
    serialize(s, MyNullable{Any}())
    seekstart(s)
    n = deserialize(s)
    @test isa(n, MyNullable)
    @test !isdefined(n, :value)
end

# cycles
module CycleFoo
    echo(x)=x
end
create_serialization_stream() do s
    echo(x) = x
    afunc = (x)->x
    A = Any[1,2,3,abs,abs,afunc,afunc,echo,echo,CycleFoo.echo,CycleFoo.echo,4,5]
    A[3] = A
    serialize(s, A)
    seekstart(s)
    b = deserialize(s)
    @test b[3] === b
    @test b[1] == 1
    @test b[4] === abs
    @test b[5] === b[4]
    @test b[5](-1) == 1

    @test b[6] === b[7]
    @test b[6]("Hello") == "Hello"

    @test b[8] === b[9]
    @test b[8]("World") == "World"

    @test b[10] === b[11]
    @test b[10]("foobar") == "foobar"

    @test b[end] == 5
    @test length(b) == length(A)
    @test isa(b,Vector{Any})
end

# shared references
create_serialization_stream() do s
    A = [1,2]
    B = [A,A]
    serialize(s, B)
    seekstart(s)
    C = deserialize(s)
    @test C == B
    @test C[1] === C[2]
end

# Regex
create_serialization_stream() do s
    r1 = r"a?b.*"
    serialize(s, r1)
    seekstart(s)
    r2 = deserialize(s)
    @test r1 == r2
end

# issue #13452
module Test13452
using Test, Serialization

module Shell
export foo
foo(x) = error("Instances must implement foo")
end

module Instance1
using ..Shell
Shell.foo(x::Int) = "This is an Int"
end

using .Shell, .Instance1
io = IOBuffer()
serialize(io, foo)
str = String(take!(io))
@test !occursin("Instance1", str)
@test occursin("Shell", str)

end  # module Test13452

# issue #15163
mutable struct B15163{T}
    x::Array{T}
end
let b = IOBuffer()
    serialize(b,B15163([1]))
    seekstart(b)
    c = deserialize(b)
    @test isa(c,B15163) && c.x == [1]
end
# related issue #20066
let b = IOBuffer()
    serialize(b, Dict{Vector, Vector}())
    seekstart(b)
    c = deserialize(b)
    @test isa(c, Dict{Vector, Vector})
end

# issue #15849
let b = IOBuffer()
    vt = Tuple[(1,)]
    serialize(b, vt)
    seekstart(b)
    @test deserialize(b) == vt
end

# issue #1770
let a = ['T', 'e', 's', 't'],
    f = IOBuffer()

    # issue #1770
    serialize(f, a)
    seek(f, 0)
    @test deserialize(f) == a
    f = IOBuffer()
    serialize(f, a)
    seek(f, 0)
    @test deserialize(f) == a

    # issue #4414
    seek(f,0)
    serialize(f, :β)
    seek(f,0)
    @test deserialize(f) === :β
end

# issue #20324
struct T20324{T}
    x::T
end
let x = T20324[T20324(1) for i = 1:2]
    b = IOBuffer()
    serialize(b, x)
    seekstart(b)
    y = deserialize(b)
    @test isa(y,Vector{T20324})
    @test y == x
end

# serializer header
let io = IOBuffer()
    serialize(io, ())
    seekstart(io)
    b = read(io)
    @test b[1] == Serialization.HEADER_TAG
    @test b[2:3] == b"JL"
    @test b[4] == Serialization.ser_version
    @test (b[5] & 0x3) == (ENDIAN_BOM == 0x01020304)
    @test ((b[5] & 0xc)>>2) == (sizeof(Int) == 8)
    @test (b[5] & 0xf0) == 0
    @test all(b[6:8] .== 0)
end

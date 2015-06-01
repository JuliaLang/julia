# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Test

function create_serialization_stream(f::Function)
    s = IOBuffer()
    f(s)
    close(s)
end

# Tags
create_serialization_stream() do s
    Serializer.writetag(s, Serializer.sertag(Bool))
    @test takebuf_array(s)[end] == UInt8(Serializer.sertag(Bool))
end

create_serialization_stream() do s
    Serializer.write_as_tag(s, Serializer.sertag(Bool))
    @test takebuf_array(s)[end] == UInt8(Serializer.sertag(Bool))
end

create_serialization_stream() do s
    Serializer.write_as_tag(s, Serializer.sertag(Symbol))
    data = takebuf_array(s)
    @test data[end-1] == 0x00
    @test data[end] == UInt8(Serializer.sertag(Symbol))
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
    @test_throws ErrorException serialize(s, C_NULL)
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

# Symbol
create_serialization_stream() do s
    gensym(len) = symbol(repeat("A", len))

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
    modstring = bytestring(mod)
    eval(parse("module $(modstring); end"))
    modtype = eval(parse("$(modstring)"))
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
    eval(parse("abstract $(usertype)"))
    utype = eval(parse("$(usertype)"))
    serialize(s, utype)
    seek(s, 0)
    @test deserialize(s) === utype
end

create_serialization_stream() do s # user-defined type
    usertype = "SerializeSomeType1"
    eval(parse("type $(usertype); end"))
    utype = eval(parse("$(usertype)"))
    serialize(s, utype)
    seek(s, 0)
    @test deserialize(s) === utype
end

create_serialization_stream() do s # user-defined type
    usertype = "SerializeSomeType2"
    eval(parse("abstract $(usertype){T}"))
    utype = eval(parse("$(usertype)"))
    serialize(s, utype)
    seek(s, 0)
    @test deserialize(s) === utype
end

create_serialization_stream() do s # immutable type with 1 field
    usertype = "SerializeSomeType3"
    eval(parse("immutable $(usertype){T}; a::T; end"))
    utype = eval(parse("$(usertype)"))
    serialize(s, utype)
    seek(s, 0)
    @test deserialize(s) === utype
end

create_serialization_stream() do s # immutable type with 2 field
    usertype = "SerializeSomeType4"
    eval(parse("immutable $(usertype){T}; a::T; b::T; end"))
    utval = eval(parse("$(usertype)(1,2)"))
    serialize(s, utval)
    seek(s, 0)
    @test deserialize(s) === utval
end

create_serialization_stream() do s # immutable type with 3 field
    usertype = "SerializeSomeType5"
    eval(parse("immutable $(usertype){T}; a::T; b::T; c::T; end"))
    utval = eval(parse("$(usertype)(1,2,3)"))
    serialize(s, utval)
    seek(s, 0)
    @test deserialize(s) === utval
end

create_serialization_stream() do s # immutable type with 4 field
    usertype = "SerializeSomeType6"
    eval(parse("immutable $(usertype){T}; a::T; b::T; c::T; d::T; end"))
    utval = eval(parse("$(usertype)(1,2,3,4)"))
    serialize(s, utval)
    seek(s, 0)
    @test deserialize(s) === utval
end

# Expression
create_serialization_stream() do s
    expr = parse("a = 1")
    serialize(s, expr)

    expr2 = parse(repeat("a = 1;", 300))
    serialize(s, expr2)

    seek(s, 0)
    @test deserialize(s) == expr
    @test deserialize(s) == expr2
end

# Array
type TA1
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

    arr5 = Array(TA1, 3)
    arr5[2] = TA1(0x01)
    serialize(s, arr5)

    seek(s, 0)
    @test deserialize(s) == arr1
    @test deserialize(s) == arr2
    @test deserialize(s) == arr3
    @test deserialize(s) == arr4

    result = deserialize(s)
    @test !isdefined(result,1)
    @test !isdefined(result,3)
    @test result[2].v == arr5[2].v
end

# SubArray
create_serialization_stream() do s # slices
    slc1 = slice(ones(UInt8, 4), 2:3)
    serialize(s, slc1)
    slc2 = slice(ones(UInt8, 4, 4) .+ [0x00, 0x01, 0x02, 0x03], 1, 2:4)
    serialize(s, slc2)

    seek(s, 0)
    @test deserialize(s) == slc1
    @test deserialize(s) == slc2
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
    @test deserialize(s) == serialize_test_function
    @test deserialize(s).code.ast == Base.uncompressed_ast(serialize_test_function2.code)
end

# Task
create_serialization_stream() do s # user-defined type array
    f = () -> begin task_local_storage(:v, 2); return 1+1 end
    t = Task(f)
    yieldto(t)
    serialize(s, t)
    seek(s, 0)
    r = deserialize(s)
    @test r.code.code.ast == Base.uncompressed_ast(f.code)
    @test r.storage[:v] == 2
    @test r.state == :done
    @test r.exception == nothing
end

create_serialization_stream() do s # user-defined type array
    t = Task(()->error("Test"))
    @test_throws ErrorException yieldto(t)
    serialize(s, t)
    seek(s, 0)
    r = deserialize(s)
    @test r.state == :failed
    @test isa(t.exception, ErrorException)
end

# corner case: undefined inside immutable type
create_serialization_stream() do s
    serialize(s, Nullable{Any}())
    seekstart(s)
    n = deserialize(s)
    @test isa(n, Nullable)
    @test !isdefined(n, :value)
end

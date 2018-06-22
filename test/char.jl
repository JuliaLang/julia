# This file is a part of Julia. License is MIT: https://julialang.org/license

@testset "basic properties" begin

    @test typemin(Char) == Char(0)
    @test ndims(Char) == 0
    @test getindex('a', 1) == 'a'
    @test_throws BoundsError getindex('a', 2)
    # This is current behavior, but it seems questionable
    @test getindex('a', 1, 1, 1) == 'a'
    @test_throws BoundsError getindex('a', 1, 1, 2)

    @test 'b' + 1 == 'c'
    @test typeof('b' + 1) == Char
    @test 1 + 'b' == 'c'
    @test typeof(1 + 'b') == Char
    @test 'b' - 1 == 'a'
    @test typeof('b' - 1) == Char

    @test widen('a') === 'a'
    # just check this works
    @test_throws Base.CodePointError Base.code_point_err(UInt32(1))
end

@testset "ASCII conversion to/from Integer" begin
    numberchars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
    lowerchars = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']
    upperchars = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']
    plane1_playingcards = ['🂠', '🂡', '🂢', '🂣', '🂤', '🂥', '🂦', '🂧', '🂨', '🂩', '🂪', '🂫', '🂬', '🂭', '🂮']
    plane2_cjkpart1 = ['𠀀', '𠀁', '𠀂', '𠀃', '𠀄', '𠀅', '𠀆', '𠀇', '𠀈', '𠀉', '𠀊', '𠀋', '𠀌', '𠀍', '𠀎', '𠀏']

    testarrays = [numberchars; lowerchars; upperchars; plane1_playingcards; plane2_cjkpart1]

    #Integer(x::Char) = Int(x)
    #tests ASCII 48 - 57
    counter = 48
    for x in numberchars
        @test Integer(x) == counter
        counter += 1
    end

    #tests ASCII 65 - 90
    counter = 65
    for x in upperchars
        @test Integer(x) == counter
        counter += 1
    end

    #tests ASCII 97 - 122
    counter = 97
    for x in lowerchars
        @test Integer(x) == counter
        counter += 1
    end

    #tests Unicode plane 1: 127136 - 127150
    counter = 127136
    for x in plane1_playingcards
        @test Integer(x) == counter
        counter += 1
    end

    #tests Unicode plane 2: 131072 - 131087
    counter = 131072
    for x in plane2_cjkpart1
        @test Integer(x) == counter
        counter += 1
    end

    #convert(::Type{Char}, x::Float16) = char(convert(UInt32, x))
    #convert(::Type{Char}, x::Float32) = char(convert(UInt32, x))
    #convert(::Type{Char}, x::Float64) = char(convert(UInt32, x))
    for x = 1:9
        @test convert(Char, Float16(x)) == convert(Char, Float32(x)) == convert(Char, Float64(x)) == Char(x)
    end

    #size(c::Char) = ()
    for x in testarrays
        @test size(x) == ()
        @test_throws BoundsError size(x,0)
        @test size(x,1) == 1
    end

    #ndims(c::Char) = 0
    for x in testarrays
        @test ndims(x) == 0
    end

    #length(c::Char) = 1
    for x in testarrays
        @test length(x) == 1
    end

    #lastindex(c::Char) = 1
    for x in testarrays
        @test lastindex(x) == 1
    end

    #getindex(c::Char) = c
    for x in testarrays
        @test getindex(x) == x
    end

    #first(c::Char) = c
    for x in testarrays
        @test first(x) == x
    end

    #last(c::Char) = c
    for x in testarrays
        @test last(x) == x
    end

    #eltype(c::Char) = Char
    for x in testarrays
        @test eltype(x) == Char
    end

    #iterate(c::Char)
    for x in testarrays
        @test iterate(x)[1] == x
        @test iterate(x, iterate(x)[2]) == nothing
    end

    #isless(x::Char, y::Integer) = isless(UInt32(x), y)
    for x in upperchars
        @test isless(x, Char(91)) == true
    end

    for x in lowerchars
        @test isless(x, Char(123)) == true
    end

    for x in numberchars
        @test isless(x, Char(66)) == true
    end

    for x in plane1_playingcards
        @test isless(x, Char(127151)) == true
    end

    for x in plane2_cjkpart1
        @test isless(x, Char(131088)) == true
    end

    #isless(x::Integer, y::Char) = isless(x, UInt32(y))
    for x in upperchars
        @test isless(Char(64), x) == true
    end

    for x in lowerchars
        @test isless(Char(96), x) == true
    end

    for x in numberchars
        @test isless(Char(47), x) == true
    end

    for x in plane1_playingcards
        @test isless(Char(127135), x) == true
    end

    for x in plane2_cjkpart1
        @test isless(Char(131071), x) == true
    end

    @test !isequal('x', 120)
    @test convert(Signed, 'A') === Int32(65)
    @test convert(Unsigned, 'A') === UInt32(65)
end #end of let block

@testset "issue #14573" begin
    array = ['a', 'b', 'c'] + [1, 2, 3]
    @test array == ['b', 'd', 'f']
    @test eltype(array) == Char

    array = [1, 2, 3] + ['a', 'b', 'c']
    @test array == ['b', 'd', 'f']
    @test eltype(array) == Char

    array = ['a', 'b', 'c'] - [0, 1, 2]
    @test array == ['a', 'a', 'a']
    @test eltype(array) == Char
end

@testset "sprint, repr" begin
    @test sprint(show, "text/plain", '$') == "'\$': ASCII/Unicode U+0024 (category Sc: Symbol, currency)"
    @test repr('$') == "'\$'"
end

@testset "read incomplete character at end of stream or file" begin
    local file = tempname()
    local iob = IOBuffer([0xf0])
    local bytes(c::Char) = codeunits(string(c))
    @test bytes(read(iob, Char)) == [0xf0]
    @test eof(iob)
    try
        write(file, 0xf0)
        open(file) do io
            @test bytes(read(io, Char)) == [0xf0]
            @test eof(io)
        end
        let io = Base.Filesystem.open(file, Base.Filesystem.JL_O_RDONLY)
            @test bytes(read(io, Char)) == [0xf0]
            @test eof(io)
            close(io)
        end
    finally
        rm(file, force=true)
    end
end

@testset "overlong codes" begin
    function test_overlong(c::Char, n::Integer, rep::String)
        if isvalid(c)
            @test Int(c) == n
        else
            @test_throws Base.InvalidCharError UInt32(c)
        end
        @test sprint(show, c) == rep
        if Base.isoverlong(c)
            @test occursin(rep*": [overlong]", sprint(show, "text/plain", c))
        end
    end

    # TODO: use char syntax once #25072 is fixed
    test_overlong('\0', 0, "'\\0'")
    test_overlong("\xc0\x80"[1], 0, "'\\xc0\\x80'")
    test_overlong("\xe0\x80\x80"[1], 0, "'\\xe0\\x80\\x80'")
    test_overlong("\xf0\x80\x80\x80"[1], 0, "'\\xf0\\x80\\x80\\x80'")

    test_overlong('\x30', 0x30, "'0'")
    test_overlong("\xc0\xb0"[1], 0x30, "'\\xc0\\xb0'")
    test_overlong("\xe0\x80\xb0"[1], 0x30, "'\\xe0\\x80\\xb0'")
    test_overlong("\xf0\x80\x80\xb0"[1], 0x30, "'\\xf0\\x80\\x80\\xb0'")

    test_overlong('\u8430', 0x8430, "'萰'")
    test_overlong("\xf0\x88\x90\xb0"[1], 0x8430, "'\\xf0\\x88\\x90\\xb0'")
end

# create a new AbstractChar type to test the fallbacks
primitive type ASCIIChar <: AbstractChar 8 end
ASCIIChar(c::UInt8) = reinterpret(ASCIIChar, c)
ASCIIChar(c::UInt32) = ASCIIChar(UInt8(c))
Base.codepoint(c::ASCIIChar) = reinterpret(UInt8, c)

@testset "abstractchar" begin
    @test AbstractChar('x') === AbstractChar(UInt32('x')) === 'x'

    @test isascii(ASCIIChar('x'))
    @test ASCIIChar('x') < 'y'
    @test ASCIIChar('x') == 'x' === Char(ASCIIChar('x')) === convert(Char, ASCIIChar('x'))
    @test ASCIIChar('x')^3 == "xxx"
    @test repr(ASCIIChar('x')) == "'x'"
    @test string(ASCIIChar('x')) == "x"
    @test_throws MethodError write(IOBuffer(), ASCIIChar('x'))
    @test_throws MethodError read(IOBuffer('x'), ASCIIChar)
end

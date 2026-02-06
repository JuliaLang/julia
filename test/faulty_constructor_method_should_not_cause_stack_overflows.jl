# new types with invalid constructors
for (typ, sup) in (
    (:Char, :AbstractChar),
    (:String, :AbstractString),
    (:Int, :Integer),
    (:UInt32, :Integer),
)
    fau = Symbol("Faulty", typ)
    @eval struct $fau <: $sup end
    @eval function Base.$typ(x::$fau) x end
end

# valid new subtype of `AbstractString`
struct MyString <: AbstractString
    str::String
end
Base.lastindex(s::MyString) = lastindex(s.str)
Base.iterate(s::MyString) = iterate(s, 1)
Base.iterate(s::MyString, state) = iterate(s, Int(state)::Int)
Base.iterate(s::MyString, state::Integer) = iterate(s, Int(state)::Int)
Base.iterate(s::MyString, state::Int) = iterate(s.str, state)
Base.isequal(a::MyString, b::MyString) = isequal(a.str, b.str)
Base.:(==)(a::MyString, b::MyString) = (a.str == b.str)

using Test
using Unicode: Unicode

@testset "faulty constructor method for new type should not cause stack overflows" begin
    exc = Union{TypeError,MethodError}
    @testset let x = FaultyChar()
        @test_throws exc isless(x, x)
        @test_throws exc x == x
        @test_throws exc hash(x, UInt(3))
        @test_throws exc print(devnull, x)
        @test_throws exc hex2bytes!(Vector{UInt8}(undef, 1), (x, x))
    end
    @testset let x = FaultyString()
        @test_throws exc hash(x, UInt(3))
        @test_throws exc repeat(x, 3)
        @test_throws exc startswith(devnull, x)
        @test_throws exc relpath(x, x)
        @test_throws exc tryparse(Float32, x)
        @test_throws exc tryparse(Float64, x)
        for f in (
            Symbol, ascii, splitpath, isdirpath, splitdir, splitdrive, splitext, normpath, abspath, ispath,
            Base.Filesystem.uripath,
            Sys.which, Sys.isexecutable, Sys.isreadable, Sys.iswritable,
            Unicode.normalize,
        )
            @test_throws exc f(x)
        end
    end
    @testset let x = FaultyInt()
        @test_throws exc readbytes!(IOBuffer(), Vector{UInt8}(undef, 0), x)
        for s in ("", MyString(""))
            @test_throws exc iterate(s, x)
            @test_throws exc isvalid(s, x)
            @test_throws exc length(s, x, x)
            @test_throws exc thisind(s, x)
            @test_throws exc prevind(s, x)
            @test_throws exc prevind(s, x, x)
            @test_throws exc nextind(s, x)
            @test_throws exc nextind(s, x, x)
            @test_throws exc codeunit(s, x)
            @test_throws exc SubString(s, x, x)
        end
    end
    @testset let x = FaultyUInt32()
        @test_throws exc Char(x)
    end
end

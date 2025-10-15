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
        @test_throws exc length("", x, x)
        @test_throws exc thisind("", x)
        @test_throws exc prevind("", x)
        @test_throws exc prevind("", x, x)
        @test_throws exc nextind("", x)
        @test_throws exc nextind("", x, x)
        @test_throws exc codeunit("", x)
        @test_throws exc SubString("", x, x)
    end
    @testset let x = FaultyUInt32()
        @test_throws exc Char(x)
    end
end

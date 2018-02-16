# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Random
import Base64:
    Base64EncodePipe,
    base64encode,
    Base64DecodePipe,
    base64decode,
    stringmime

const inputText = "Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure."
const encodedMaxLine76 = """
TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlz
IHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2Yg
dGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGlu
dWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRo
ZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4="""

@testset "Examples" begin
    # Encode and decode
    fname = tempname()
    open(fname, "w") do f
        opipe = Base64EncodePipe(f)
        write(opipe,inputText)
        @test close(opipe) === nothing
    end

    open(fname, "r") do f
        ipipe = Base64DecodePipe(f)
        @test read(ipipe, String) == inputText
        @test close(ipipe) === nothing
    end
    rm(fname)

    # Byte-by-byte encode and decode.
    buf = IOBuffer()
    pipe = Base64EncodePipe(buf)
    for char in inputText
        write(pipe, UInt8(char))
    end
    close(pipe)
    pipe = Base64DecodePipe(IOBuffer(take!(buf)))
    decoded = UInt8[]
    while !eof(pipe)
        push!(decoded, read(pipe, UInt8))
    end
    @test String(decoded) == inputText

    # Encode to string and decode
    @test String(base64decode(base64encode(inputText))) == inputText

    # Decode with max line chars = 76 and padding
    ipipe = Base64DecodePipe(IOBuffer(encodedMaxLine76))
    @test read(ipipe, String) == inputText

    # Decode with max line chars = 76 and no padding
    #ipipe = Base64DecodePipe(IOBuffer(encodedMaxLine76[1:end-1]))
    #@test read(ipipe, String) == inputText

    # Decode with two padding characters ("==")
    ipipe = Base64DecodePipe(IOBuffer(string(encodedMaxLine76[1:end-2],"==")))
    @test read(ipipe, String) == inputText[1:end-1]

    # Test incorrect format
    ipipe = Base64DecodePipe(IOBuffer(encodedMaxLine76[1:end-3]))
    @test_throws ArgumentError read(ipipe, String)

    # issue #21314
    @test base64decode(chomp("test")) == base64decode("test")
end

@testset "Random data" begin
    mt = MersenneTwister(1234)
    for _ in 1:1000
        data = rand(mt, UInt8, rand(0:300))
        @test hash(base64decode(base64encode(data))) == hash(data)
    end
end

@testset "stringmime" begin
    @test stringmime("text/plain", [1 2;3 4]) == repr("text/plain", [1 2;3 4])
    @test stringmime("text/html", "raw html data") == "raw html data"
    @test stringmime("text/plain", "string") == "\"string\""
    @test stringmime("image/png", UInt8[2,3,4,7]) == "AgMEBw=="
    @test stringmime("text/plain", 3.141592653589793, context=:compact=>true) == "3.14159"
end

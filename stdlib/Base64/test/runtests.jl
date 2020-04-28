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

const longEncodedText = "bmFtZSA9ICJHZW5pZSIKdXVpZCA9ICJjNDNjNzM2ZS1hMmQxLTExZTgtMTYx\nZi1hZjk1MTE3ZmJkMWUiCmF1dGhvcnMgPSBbIkFkcmlhbiBTYWxjZWFudSA8\nZUBlc3NlbmNpYXJ5LmNvbT4iXQp2ZXJzaW9uID0gIjAuOS40IgoKW2RlcHNd\nCkFyZ1BhcnNlID0gImM3ZTQ2MGM2LTJmYjktNTNhOS04YzViLTE2ZjUzNTg1\nMWM2MyIKRGF0ZXMgPSAiYWRlMmNhNzAtMzg5MS01OTQ1LTk4ZmItZGMwOTk0\nMzJlMDZhIgpEaXN0cmlidXRlZCA9ICI4YmE4OWUyMC0yODVjLTViNmYtOTM1\nNy05NDcwMDUyMGVlMWIiCkd1bWJvID0gIjcwOGVjMzc1LWIzZDYtNWE1Ny1h\nN2NlLTgyNTdiZjk4NjU3YSIKSFRUUCA9ICJjZDNlYjAxNi0zNWZiLTUwOTQt\nOTI5Yi01NThhOTZmYWQ2ZjMiCkh0dHBDb21tb24gPSAiNzcxNzJjMWItMjAz\nZi01NGFjLWFhNTQtM2YxMTk4ZmU5ZjkwIgpKU09OID0gIjY4MmMwNmEwLWRl\nNmEtNTRhYi1hMTQyLWM4YjFjZjc5Y2RlNiIKTWFya2Rvd24gPSAiZDZmNDM3\nNmUtYWVmNS01MDVhLTk2YzEtOWMwMjczOTQ2MDdhIgpNYmVkVExTID0gIjcz\nOWJlNDI5LWJlYTgtNTE0MS05OTEzLWNjNzBlN2YzNzM2ZCIKTWlsbGJvYXJk\nID0gIjM5ZWMxNDQ3LWRmNDQtNWY0Yy1iZWFhLTg2NmYzMGI0ZDNiMiIKTWlu\naUxvZ2dpbmcgPSAiZjU5NDAyZWMtMDI2Mi01NzA3LWE1NjEtNzcwYWY5NGJj\nNWE2IgpOZXR0bGUgPSAiNDlkZWExZWUtZjZmYS01YWE2LTlhMTEtODgxNmNl\nZTdkNGI5IgpOdWxsYWJsZXMgPSAiNGQxZTFkNzctNjI1ZS01YjQwLTkxMTMt\nYTU2MGVjN2E4ZWNkIgpPcmRlcmVkQ29sbGVjdGlvbnMgPSAiYmFjNTU4ZTEt\nNWU3Mi01ZWJjLThmZWUtYWJlOGE0NjlmNTVkIgpQa2cgPSAiNDRjZmU5NWEt\nMWViMi01MmVhLWI2NzItZTJhZmRmNjliNzhmIgpSZWV4cG9ydCA9ICIxODlh\nMzg2Ny0zMDUwLTUyZGEtYTgzNi1lNjMwYmE5MGFiNjkiClJldmlzZSA9ICIy\nOTVhZjMwZi1lNGFkLTUzN2ItODk4My0wMDEyNmMyYTNhYmUiClNIQSA9ICJl\nYThlOTE5Yy0yNDNjLTUxYWYtODgyNS1hYWE2M2NkNzIxY2UiClNlcmlhbGl6\nYXRpb24gPSAiOWU4OGI0MmEtZjgyOS01YjBjLWJiZTktOWU5MjMxOTgxNjZi\nIgpTb2NrZXRzID0gIjY0NjJmZTBiLTI0ZGUtNTYzMS04Njk3LWRkOTQxZjkw\nZGVjYyIKVVJJUGFyc2VyID0gIjMwNTc4YjQ1LTlhZGMtNTk0Ni1iMjgzLTY0\nNWVjNDIwYWY2NyIKVW5pY29kZSA9ICI0ZWMwYTgzZS00OTNlLTUwZTItYjlh\nYy04ZjcyYWNmNWE4ZjUiCllBTUwgPSAiZGRiNmQ5MjgtMjg2OC01NzBmLWJk\nZGYtYWIzZjljZjk5ZWI2IgoKW2NvbXBhdF0KanVsaWEgPSAiMSIK"
const longDecodedText = "name = \"Genie\"\nuuid = \"c43c736e-a2d1-11e8-161f-af95117fbd1e\"\nauthors = [\"Adrian Salceanu <e@essenciary.com>\"]\nversion = \"0.9.4\"\n\n[deps]\nArgParse = \"c7e460c6-2fb9-53a9-8c5b-16f535851c63\"\nDates = \"ade2ca70-3891-5945-98fb-dc099432e06a\"\nDistributed = \"8ba89e20-285c-5b6f-9357-94700520ee1b\"\nGumbo = \"708ec375-b3d6-5a57-a7ce-8257bf98657a\"\nHTTP = \"cd3eb016-35fb-5094-929b-558a96fad6f3\"\nHttpCommon = \"77172c1b-203f-54ac-aa54-3f1198fe9f90\"\nJSON = \"682c06a0-de6a-54ab-a142-c8b1cf79cde6\"\nMarkdown = \"d6f4376e-aef5-505a-96c1-9c027394607a\"\nMbedTLS = \"739be429-bea8-5141-9913-cc70e7f3736d\"\nMillboard = \"39ec1447-df44-5f4c-beaa-866f30b4d3b2\"\nMiniLogging = \"f59402ec-0262-5707-a561-770af94bc5a6\"\nNettle = \"49dea1ee-f6fa-5aa6-9a11-8816cee7d4b9\"\nNullables = \"4d1e1d77-625e-5b40-9113-a560ec7a8ecd\"\nOrderedCollections = \"bac558e1-5e72-5ebc-8fee-abe8a469f55d\"\nPkg = \"44cfe95a-1eb2-52ea-b672-e2afdf69b78f\"\nReexport = \"189a3867-3050-52da-a836-e630ba90ab69\"\nRevise = \"295af30f-e4ad-537b-8983-00126c2a3abe\"\nSHA = \"ea8e919c-243c-51af-8825-aaa63cd721ce\"\nSerialization = \"9e88b42a-f829-5b0c-bbe9-9e923198166b\"\nSockets = \"6462fe0b-24de-5631-8697-dd941f90decc\"\nURIParser = \"30578b45-9adc-5946-b283-645ec420af67\"\nUnicode = \"4ec0a83e-493e-50e2-b9ac-8f72acf5a8f5\"\nYAML = \"ddb6d928-2868-570f-bddf-ab3f9cf99eb6\"\n\n[compat]\njulia = \"1\"\n"

@testset "Examples" begin
    # Encode and decode
    fname = tempname()
    open(fname, "w") do f
        opipe = Base64EncodePipe(f)
        write(opipe, inputText)
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
    ipipe = Base64DecodePipe(IOBuffer(string(encodedMaxLine76[1:end - 2], "==")))
    @test read(ipipe, String) == inputText[1:end - 1]

    # Test incorrect format
    ipipe = Base64DecodePipe(IOBuffer(encodedMaxLine76[1:end - 3]))
    @test_throws ArgumentError read(ipipe, String)

    # issue #21314
    @test base64decode(chomp("test")) == base64decode("test")

    # issue #32397
    @test String(base64decode(longEncodedText)) == longDecodedText;
end

@testset "Random data" begin
    mt = MersenneTwister(1234)
    for _ in 1:1000
        data = rand(mt, UInt8, rand(0:300))
        @test hash(base64decode(base64encode(data))) == hash(data)
    end
end

struct PNG end
Base.show(io::IO, ::MIME"image/png", ::PNG) = print(io, "PNG")

@testset "stringmime" begin
    @test stringmime("text/plain", [1 2;3 4]) == repr("text/plain", [1 2;3 4])
    @test stringmime("text/html", "raw html data") == "raw html data"
    @test stringmime("text/plain", "string") == "\"string\""
    @test stringmime("image/png", UInt8[2,3,4,7]) == "AgMEBw=="
    @test stringmime("text/plain", 3.141592653589793, context = :compact => true) == "3.14159"
    @test stringmime("image/png", PNG()) == stringmime(MIME("image/png"), PNG()) == "UE5H"
end

function splace(in::String, p = 0.3)
    spaces = ["\n"," "]
    len = length(in)
    len == 0 && return in
    rc::String = ""
    i = 1
    for (x, v) in enumerate(sort(randsubseq(collect(1:len), p)))
        rc = rc * in[i:v] * rand(spaces)^rand(Int.(1:10))
        i = v + 1
    end
    return rc * in[i:len] * rand(spaces)^rand(Int.(1:10))
end

@testset "lstrsplaced" begin
    for _ in 1:1000
        @test String(base64decode(splace(longEncodedText))) == longDecodedText
    end
end

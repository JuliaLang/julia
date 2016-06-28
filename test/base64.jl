# This file is a part of Julia. License is MIT: http://julialang.org/license
@testset "base64" begin
const inputText = "Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure."
const encodedMaxLine76 =
"""TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlz
IHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2Yg
dGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGlu
dWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRo
ZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4="""

# Encode and decode
fname = tempname()
open(fname, "w") do f
    opipe = Base64EncodePipe(f)
    write(opipe,inputText)
    @test close(opipe) === nothing
end

open(fname, "r") do f
    ipipe = Base64DecodePipe(f)
    @test readstring(ipipe) == inputText
    @test close(ipipe) === nothing
end
rm(fname)

# Encode to string and decode
@test String(base64decode(base64encode(inputText))) == inputText

# Decode with max line chars = 76 and padding
ipipe = Base64DecodePipe(IOBuffer(encodedMaxLine76))
@test readstring(ipipe) == inputText

# Decode with max line chars = 76 and no padding
ipipe = Base64DecodePipe(IOBuffer(encodedMaxLine76[1:end-1]))
@test readstring(ipipe) == inputText

# Decode with two padding characters ("==")
ipipe = Base64DecodePipe(IOBuffer(string(encodedMaxLine76[1:end-2],"==")))
@test readstring(ipipe) == inputText[1:end-1]

# Test incorrect format
ipipe = Base64DecodePipe(IOBuffer(encodedMaxLine76[1:end-3]))
@test_throws ArgumentError readstring(ipipe)

end

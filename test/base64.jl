
const inputText = "Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure."
const encodedMaxLine76 =
"""TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlz
IHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2Yg
dGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGlu
dWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRo
ZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4="""

# Encode and decode
fname = tempname()
f = open(fname, "w")
opipe = Base64EncodePipe(f)
write(opipe,inputText)
close(opipe)
close(f)
f = open(fname, "r")
ipipe = Base64DecodePipe(f)
@test  readall(ipipe) == inputText
close(ipipe)
close(f)
rm(fname)

# Encode to string and decode
@test base64decode(base64encode(inputText)) == inputText

# Decode with max line chars = 76 and padding
ipipe = Base64DecodePipe(IOBuffer(encodedMaxLine76))
@test readall(ipipe) == inputText

# Decode with max line chars = 76 and no padding
ipipe = Base64DecodePipe(IOBuffer(encodedMaxLine76[1:end-1]))
@test readall(ipipe) == inputText

# Decode with two padding characters ("==")
ipipe = Base64DecodePipe(IOBuffer(string(encodedMaxLine76[1:end-2],"==")))
@test readall(ipipe) == inputText[1:end-1]

# Test incorrect format
ipipe = Base64DecodePipe(IOBuffer(encodedMaxLine76[1:end-3]))
@test_throws ErrorException readall(ipipe)

# This file is a part of Julia. License is MIT: http://julialang.org/license

# bytes2hex and hex2bytes
hex_str = "d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592"
bin_val = hex2bytes(hex_str)

@test div(length(hex_str), 2) == length(bin_val)
@test hex_str == bytes2hex(bin_val)

bin_val = hex2bytes("07bf")
@test bin_val[1] == 7
@test bin_val[2] == 191
@test typeof(bin_val) == Array{UInt8, 1}
@test length(bin_val) == 2

# all valid hex chars
@test "0123456789abcdefabcdef" == bytes2hex(hex2bytes("0123456789abcdefABCDEF"))

# odd size
@test_throws ArgumentError hex2bytes("0123456789abcdefABCDEF0")

#non-hex characters
@test_throws ArgumentError hex2bytes("0123456789abcdefABCDEFGH")

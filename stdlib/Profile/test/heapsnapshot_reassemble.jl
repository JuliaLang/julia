using Test

@testset "_write_decimal_number" begin
    _digits_buf = zeros(UInt8, ndigits(typemax(UInt)))
    io = IOBuffer()

    test_write(d) = begin
        Profile.HeapSnapshot._write_decimal_number(io, d, _digits_buf)
        s = String(take!(io))
        seekstart(io)
        return s
    end
    @test test_write(0) == "0"
    @test test_write(99) == "99"

    @test test_write(UInt8(0)) == "0"
    @test test_write(UInt32(0)) == "0"
    @test test_write(Int32(0)) == "0"

    @test test_write(UInt8(99)) == "99"
    @test test_write(UInt32(99)) == "99"
    @test test_write(Int32(99)) == "99"

    # Sample among possible UInts we might print
    for x in typemin(UInt8):typemax(UInt8)
        @test test_write(x) == string(x)
    end
    for x in typemin(UInt):typemax(UInt)÷10001:typemax(UInt)
        @test test_write(x) == string(x)
    end
end

function test_print_str_escape_json(input::AbstractString, expected::AbstractString)
    output = IOBuffer()
    Profile.HeapSnapshot.print_str_escape_json(output, input)
    @test String(take!(output)) == expected
end

@testset "print_str_escape_json" begin
    # Test basic string escaping
    test_print_str_escape_json("\"hello\"", "\"\\\"hello\\\"\"")

    # Test escaping of control characters
    test_print_str_escape_json("\x01\x02\x03", "\"\\u0001\\u0002\\u0003\"")

    # Test escaping of other special characters
    test_print_str_escape_json("\b\f\n\r\t", "\"\\b\\f\\n\\r\\t\"")

    # Test handling of mixed characters
    test_print_str_escape_json("abc\ndef\"ghi", "\"abc\\ndef\\\"ghi\"")

    # Test handling of empty string
    test_print_str_escape_json("", "\"\"")
end

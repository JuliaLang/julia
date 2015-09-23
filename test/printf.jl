# This file is a part of Julia. License is MIT: http://julialang.org/license

# int
@test string(f"%d", typemax(Int64)) == "9223372036854775807"
for (fmt, val) in (("%i", "42"),
                   ("%u", "42"),
                   ("Test: %i", "Test: 42"),
                   ("%#x", "0x2a"),
                   ("%#o", "052"),
                   ("%X", "2A"),
                   ("%X", "2A"),
                   ("% i", " 42"),
                   ("%+i", "+42"),
                   ("%4i", "  42"),
                   ("%-4i", "42  "))
    @test string(FormatString(fmt), 42) == val
end

# float / BigFloat
for (fmt, val) in (("%7.2f", "   1.23"),
                   ("%-7.2f", "1.23   "),
                   ("%07.2f", "0001.23"),
                   ("%.0f", "1"),
                   ("%#.0f", "1."),
                   ("%.4e", "1.2345e+00")),
      num in (1.2345, big"1.2345")
    @test string(FormatString(fmt), num) == val
end

# Inf / NaN handling
@test string(f"%f", Inf) == "Inf"
@test string(f"%f", NaN) == "NaN"
@test string(f"%f", big"Inf") == "Inf"
@test string(f"%f", big"NaN") == "NaN"

# scientific notation
@test string(f"%.0e", 3e142) == "3e+142"
@test string(f"%#.0e", 3e142) == "3.e+142"
@test string(f"%.0e", big"3e142") == "3e+142"
@test string(f"%#.0e", big"3e142") == "3.e+142"

# %g
for (val, res) in ((12345678., "1.23457e+07"),
                   (1234567.8, "1.23457e+06"),
                   (123456.78, "123457"),
                   (12345.678, "12345.7"),
                   (12340000.0, "1.234e+07"))
    @test string(f"%.6g", val) == res
end
for (val, res) in ((big"12345678.", "1.23457e+07"),
                   (big"1234567.8", "1.23457e+06"),
                   (big"123456.78", "123457"),
                   (big"12345.678", "12345.7"))
    @test string(f"%.6g", val) == res
end
for (fmt, val) in (("%10.5g", "     123.4"),
                   ("%+10.5g", "    +123.4"),
                   ("% 10.5g","     123.4"),
                   ("%#10.5g", "    123.40"),
                   ("%-10.5g", "123.4     "),
                   ("%-+10.5g", "+123.4    "),
                   ("%010.5g", "00000123.4")),
      num in (123.4, big"123.4")
    @test string(FormatString(fmt), num) == val
end
@test string(f"%10.5g", -123.4 ) == "    -123.4"
@test string(f"%010.5g", -123.4 ) == "-0000123.4"
@test string(f"%.6g", 12340000.0 ) == "1.234e+07"
@test string(f"%#.6g", 12340000.0 ) == "1.23400e+07"
@test string(f"%10.5g", big"-123.4" ) == "    -123.4"
@test string(f"%010.5g", big"-123.4" ) == "-0000123.4"
@test string(f"%.6g", big"12340000.0" ) == "1.234e+07"
@test string(f"%#.6g", big"12340000.0") == "1.23400e+07"

# hex float
@test string(f"%a", 1.5) == "0x1.8p+0"
@test string(f"%#.0a", 1.5) == "0x2.p+0"
@test string(f"%+30a", 1/3) == "         +0x1.5555555555555p-2"

# chars
@test string(f"%c", 65) == "A"
@test string(f"%c", 'A') == "A"
@test string(f"%c", 248) == "ø"
@test string(f"%c", 'ø') == "ø"

# strings
@test string(f"%s", "test") == "test"
@test string(f"%s", "tést") == "tést"

# reasonably complex
@test string(f"Test: %s%c%C%c%#-.0f.", "t", 65, 66, 67, -42) == "Test: tABC-42.."

#test simple splatting
@test string(f"%d%d", [1 2]...) == "12"

# combo
@test string(f"%f %d %d %f", 1.0, [3 4]..., 5) == "1.000000 3 4 5.000000"

# multi
@test string(f"%s %f %9.5f %d %d %d %d%d%d%d", [1:6;]..., [7,8,9,10]...) == "1 2.000000   3.00000 4 5 6 78910"

# comprehension
@test string(f"%s %s %s %d %d %d %f %f %f", Any[10^x+y for x=1:3,y=1:3 ]...) == "11 101 1001 12 102 1002 13.000000 103.000000 1003.000000"

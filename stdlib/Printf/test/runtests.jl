# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Printf

# this macro tests for exceptions thrown at macro expansion
macro test_me(ty, ex)
    return quote
        @test_throws $(esc(ty)) try
            $(esc(ex))
        catch err
            @test err isa LoadError
            @test err.file === $(string(__source__.file))
            @test err.line === $(__source__.line)
            rethrow(err.error)
        end
    end
end

# printf
# int
@test (@sprintf "%d" typemax(Int64)) == "9223372036854775807"
@test (@sprintf "%a" typemax(Int64)) == "0x7.fffffffffffffffp+60"
@test (@sprintf "%A" typemax(Int64)) == "0X7.FFFFFFFFFFFFFFFP+60"

#printing an int value
for (fmt, val) in (("%i", "42"),
                   ("%u", "42"),
                   ("Test: %i", "Test: 42"),
                   ("%#x", "0x2a"),
                   ("%#o", "052"),
                   ("%x", "2a"),
                   ("%X", "2A"),
                   ("% i", " 42"),
                   ("%+i", "+42"),
                   ("%4i", "  42"),
                   ("%-4i", "42  "),
                   ("%a", "0x2.ap+4"),
                   ("%A", "0X2.AP+4"),
                   ("%20a","            0x2.ap+4"),
                   ("%-20a","0x2.ap+4            "),
                   ("%f", "42.000000"),
                   ("%g", "42")),
     num in (UInt16(42), UInt32(42), UInt64(42), UInt128(42),
              Int16(42), Int32(42), Int64(42), Int128(42), big"42")
            #big"42" causes stack overflow on %a ; gh #14409
    num isa BigInt && fmt in ["%a", "%#o", "%g"] && continue
    @test @eval(@sprintf($fmt, $num) == $val)
end

# pointers
if Sys.WORD_SIZE == 64
    @test (@sprintf "%20p" 0) == "  0x0000000000000000"
    @test (@sprintf "%-20p" 0) == "0x0000000000000000  "
elseif Sys.WORD_SIZE == 32
    @test (@sprintf "%20p" 0) == "          0x00000000"
    @test (@sprintf "%-20p" 0) == "0x00000000          "
else
    @test false
end

# float / BigFloat
for (fmt, val) in (("%7.2f", "   1.23"),
                   ("%-7.2f", "1.23   "),
                   ("%07.2f", "0001.23"),
                   ("%.0f", "1"),
                   ("%#.0f", "1."),
                   ("%.4e", "1.2345e+00"),
                   ("%.4E", "1.2345E+00"),
                   ("%.2a", "0x1.3cp+0"),
                   ("%.2A", "0X1.3CP+0")),
      num in (1.2345, big"1.2345")
    @test @eval(@sprintf($fmt, $num) == $val)
end

# numeric spacing and various flag tests
function _test_flags(val, vflag::AbstractString, fmt::AbstractString, res::AbstractString, prefix::AbstractString)
    vflag = string("%", vflag)
    space_fmt = string(length(res) + length(prefix) + 3, fmt)
    fsign = string((val < 0 ? "-" : "+"), prefix)
    nsign = string((val < 0 ? "-" : " "), prefix)
    osign = val < 0 ? string("-", prefix) : string(prefix, "0")
    esign = string(val < 0 ? "-" : "", prefix)
    esignend = val < 0 ? "" : " "

    for (flag::AbstractString, ans::AbstractString) in (
            ("", string("  ", nsign, res)),
            ("+", string("  ", fsign, res)),
            (" ", string("  ", nsign, res)),
            ("0", string(osign, "00", res)),
            ("-", string(esign, res, "  ", esignend)),
            ("0+", string(fsign, "00", res)),
            ("0 ", string(nsign, "00", res)),
            ("-+", string(fsign, res, "  ")),
            ("- ", string(nsign, res, "  ")),
        )
        fmt_string = string(vflag, flag, space_fmt)
        @test @eval(@sprintf($fmt_string, $val) == $ans)
    end
end

for i in (
        (42, "", "i", "42", ""),
        (42, "", "d", "42", ""),

        (42, "", "u", "42", ""),
        (42, "", "x", "2a", ""),
        (42, "", "X", "2A", ""),
        (42, "", "o", "52", ""),

        (42, "#", "x", "2a", "0x"),
        (42, "#", "X", "2A", "0X"),
        (42, "#", "o", "052", ""),

        (1.2345, "", ".2f", "1.23", ""),
        (1.2345, "", ".2e", "1.23e+00", ""),
        (1.2345, "", ".2E", "1.23E+00", ""),

        (1.2345, "#", ".0f", "1.", ""),
        (1.2345, "#", ".0e", "1.e+00", ""),
        (1.2345, "#", ".0E", "1.E+00", ""),

        (1.2345, "", ".2a", "1.3cp+0", "0x"),
        (1.2345, "", ".2A", "1.3CP+0", "0X"),
    )
    _test_flags(i...)
    _test_flags(-i[1], i[2:5]...)
end

# Inf / NaN handling
@test (@sprintf "%f" Inf) == "Inf"
@test (@sprintf "%f" NaN) == "NaN"
@test (@sprintf "%f" big"Inf") == "Inf"
@test (@sprintf "%f" big"NaN") == "NaN"

# scientific notation
@test (@sprintf "%.0e" 3e142) == "3e+142"
@test (@sprintf "%#.0e" 3e142) == "3.e+142"
@test (@sprintf "%.0e" big"3e142") == "3e+142"
@test (@sprintf "%#.0e" big"3e142") == "3.e+142"

@test (@sprintf "%.0e" big"3e1042") == "3e+1042"

@test (@sprintf "%e" 3e42) == "3.000000e+42"
@test (@sprintf "%E" 3e42) == "3.000000E+42"
@test (@sprintf "%e" 3e-42) == "3.000000e-42"
@test (@sprintf "%E" 3e-42) == "3.000000E-42"
@test (@sprintf "%a" 3e4) == "0x1.d4cp+14"
@test (@sprintf "%A" 3e4) == "0X1.D4CP+14"
@test (@sprintf "%.4a" 3e-4) == "0x1.3a93p-12"
@test (@sprintf "%.4A" 3e-4) == "0X1.3A93P-12"

# %g
for (val, res) in ((12345678., "1.23457e+07"),
                   (1234567.8, "1.23457e+06"),
                   (123456.78, "123457"),
                   (12345.678, "12345.7"),
                   (12340000.0, "1.234e+07"))
    @test (@sprintf("%.6g", val) == res)
end
for (val, res) in ((big"12345678.", "1.23457e+07"),
                   (big"1234567.8", "1.23457e+06"),
                   (big"123456.78", "123457"),
                   (big"12345.678", "12345.7"))
    @test (@sprintf("%.6g", val) == res)
end
for (fmt, val) in (("%10.5g", "     123.4"),
                   ("%+10.5g", "    +123.4"),
                   ("% 10.5g","     123.4"),
                   ("%#10.5g", "    123.40"),
                   ("%-10.5g", "123.4     "),
                   ("%-+10.5g", "+123.4    "),
                   ("%010.5g", "00000123.4")),
      num in (123.4, big"123.4")
    @test @eval(@sprintf($fmt, $num) == $val)
end
@test( @sprintf( "%10.5g", -123.4 ) == "    -123.4")
@test( @sprintf( "%010.5g", -123.4 ) == "-0000123.4")
@test( @sprintf( "%.6g", 12340000.0 ) == "1.234e+07")
@test( @sprintf( "%#.6g", 12340000.0 ) == "1.23400e+07")
@test( @sprintf( "%10.5g", big"-123.4" ) == "    -123.4")
@test( @sprintf( "%010.5g", big"-123.4" ) == "-0000123.4")
@test( @sprintf( "%.6g", big"12340000.0" ) == "1.234e+07")
@test( @sprintf( "%#.6g", big"12340000.0") == "1.23400e+07")

# %g regression gh #14331
@test( @sprintf( "%.5g", 42) == "42")
@test( @sprintf( "%#.2g", 42) == "42.")
@test( @sprintf( "%#.5g", 42) == "42.000")

# hex float
@test (@sprintf "%a" 1.5) == "0x1.8p+0"
@test (@sprintf "%a" 1.5f0) == "0x1.8p+0"
@test (@sprintf "%a" big"1.5") == "0x1.8p+0"
@test (@sprintf "%#.0a" 1.5) == "0x2.p+0"
@test (@sprintf "%+30a" 1/3) == "         +0x1.5555555555555p-2"

# chars
@test (@sprintf "%c" 65) == "A"
@test (@sprintf "%c" 'A') == "A"
@test (@sprintf "%3c" 'A') == "  A"
@test (@sprintf "%-3c" 'A') == "A  "
@test (@sprintf "%c" 248) == "ø"
@test (@sprintf "%c" 'ø') == "ø"

# escape %
@test (@sprintf "%%") == "%"
@test (@sprintf "%%s") == "%s"
@test_me ArgumentError("invalid printf format string: \"%\"") @macroexpand(@sprintf "%") #" (fixes syntax highlighting)

# argument count
@test_me ArgumentError("@sprintf: wrong number of arguments (0) should be (1)") @macroexpand(@sprintf "%s")
@test_me ArgumentError("@sprintf: wrong number of arguments (2) should be (1)") @macroexpand(@sprintf "%s" "1" "2")

# no interpolation
@test_me ArgumentError("@sprintf: format must be a plain static string (no interpolation or prefix)") @macroexpand(@sprintf "$n")

# type width specifier parsing (ignored)
@test (@sprintf "%llf" 1.2) == "1.200000"
@test (@sprintf "%Lf" 1.2) == "1.200000"
@test (@sprintf "%hhu" 1) == "1"
@test (@sprintf "%hu" 1) == "1"
@test (@sprintf "%lu" 1) == "1"
@test (@sprintf "%llu" 1) == "1"
@test (@sprintf "%Lu" 1) == "1"
@test (@sprintf "%zu" 1) == "1"
@test (@sprintf "%ju" 1) == "1"
@test (@sprintf "%tu" 1) == "1"

# strings
@test (@sprintf "%s" "test") == "test"
@test (@sprintf "%s" "tést") == "tést"

@test (@sprintf "%8s" "test") == "    test"
@test (@sprintf "%-8s" "test") == "test    "

@test (@sprintf "%s" "tést") == "tést"

@test (@sprintf "%s" :test) == "test"
@test (@sprintf "%#s" :test) == ":test"
@test (@sprintf "%#8s" :test) == "   :test"
@test (@sprintf "%#-8s" :test) == ":test   "

@test (@sprintf "%8.3s" "test") == "     tes"
@test (@sprintf "%#8.3s" "test") == "     \"te"
@test (@sprintf "%-8.3s" "test") == "tes     "
@test (@sprintf "%#-8.3s" "test") == "\"te     "
@test (@sprintf "%.3s" "test") == "tes"
@test (@sprintf "%#.3s" "test") == "\"te"
@test (@sprintf "%-.3s" "test") == "tes"
@test (@sprintf "%#-.3s" "test") == "\"te"

# reasonably complex
@test (@sprintf "Test: %s%c%C%c%#-.0f." "t" 65 66 67 -42) == "Test: tABC-42.."

#test simple splatting
@test (@sprintf "%d%d" [1 2]...) == "12"

# invalid format specifiers, not "diouxXDOUeEfFgGaAcCsSpn"
for c in "bBhHIjJkKlLmMNPqQrRtTvVwWyYzZ"
    fmt_str = string("%", c)
    @test_me ArgumentError("@sprintf: first argument must be a format string") @macroexpand(@sprintf $fmt_str 1)
end

# combo
@test (@sprintf "%f %d %d %f" 1.0 [3 4]... 5) == "1.000000 3 4 5.000000"

# multi
@test (@sprintf "%s %f %9.5f %d %d %d %d%d%d%d" [1:6;]... [7,8,9,10]...) == "1 2.000000   3.00000 4 5 6 78910"

# comprehension
@test (@sprintf "%s %s %s %d %d %d %f %f %f" Any[10^x+y for x=1:3,y=1:3 ]...) == "11 101 1001 12 102 1002 13.000000 103.000000 1003.000000"

# @printf
@test_me ArgumentError("@printf: called with no arguments") @macroexpand(@printf)
@test_me ArgumentError("@printf: first or second argument must be a format string") @macroexpand(@printf 1)

# Check bug with trailing nul printing BigFloat
@test (@sprintf("%.330f", BigFloat(1)))[end] != '\0'

# Check utf8 strings #23880
@test (@sprintf("X%d", 2)) == "X2"
@test (@sprintf("\u00d0%d", 2)) == "\u00d02"
@test (@sprintf("\u0f00%d", 2)) == "\u0f002"
@test (@sprintf("\U0001ffff%d", 2)) == "\U0001ffff2"
@test (@sprintf("%dX%d", 1, 2)) == "1X2"
@test (@sprintf("%d\u00d0%d", 1, 2)) == "1\u00d02"
@test (@sprintf("%d\u0f00%d", 1, 2)) == "1\u0f002"
@test (@sprintf("%d\U0001ffff%d", 1, 2)) == "1\U0001ffff2"
@test (@sprintf("%d\u2203%d\u0203", 1, 2)) == "1\u22032\u0203"
@test_me ArgumentError @macroexpand(@sprintf("%y%d", 1, 2))
@test_me ArgumentError @macroexpand(@sprintf("%\u00d0%d", 1, 2))
@test_me ArgumentError @macroexpand(@sprintf("%\u0f00%d", 1, 2))
@test_me ArgumentError @macroexpand(@sprintf("%\U0001ffff%d", 1, 2))

# test at macro execution time
@test_throws ArgumentError("@sprintf: wrong number of arguments (2) should be (3)") (@sprintf "%d%d%d" 1:2...)

# issue #29662
@test (@sprintf "%12.3e" pi*1e100) == "  3.142e+100"

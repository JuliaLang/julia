# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Printf

@testset "Printf" begin

@testset "%p" begin

    # pointers
    if Sys.WORD_SIZE == 64
        @test (Printf.@sprintf "%20p" 0) == "  0x0000000000000000"
        @test (Printf.@sprintf "%-20p" 0) == "0x0000000000000000  "
        @test (Printf.@sprintf "%20p" C_NULL) == "  0x0000000000000000"
        @test (@sprintf "%-20p" C_NULL) == "0x0000000000000000  "
    elseif Sys.WORD_SIZE == 32
        @test (Printf.@sprintf "%20p" 0) == "          0x00000000"
        @test (Printf.@sprintf "%-20p" 0) == "0x00000000          "
        @test (@sprintf "%20p" C_NULL) == "          0x00000000"
        @test (@sprintf "%-20p" C_NULL) == "0x00000000          "
    end

    #40318
    @test @sprintf("%p", 0xfffffffffffe0000) == "0xfffffffffffe0000"

end

@testset "%a" begin

    # hex float
    @test (Printf.@sprintf "%a" 0.0) == "0x0p+0"
    @test (Printf.@sprintf "%a" -0.0) == "-0x0p+0"
    @test (Printf.@sprintf "%.3a" 0.0) == "0x0.000p+0"
    @test (Printf.@sprintf "%a" 1.5) == "0x1.8p+0"
    @test (Printf.@sprintf "%a" 1.5f0) == "0x1.8p+0"
    @test (Printf.@sprintf "%a" big"1.5") == "0x1.8p+0"
    @test (Printf.@sprintf "%#.0a" 1.5) == "0x2.p+0"
    @test (Printf.@sprintf "%+30a" 1/3) == "         +0x1.5555555555555p-2"

    @test Printf.@sprintf("%a", 1.5) == "0x1.8p+0"
    @test Printf.@sprintf("%a", 3.14) == "0x1.91eb851eb851fp+1"
    @test Printf.@sprintf("%.0a", 3.14) == "0x2p+1"
    @test Printf.@sprintf("%.1a", 3.14) == "0x1.9p+1"
    @test Printf.@sprintf("%.2a", 3.14) == "0x1.92p+1"
    @test Printf.@sprintf("%#a", 3.14) == "0x1.91eb851eb851fp+1"
    @test Printf.@sprintf("%#.0a", 3.14) == "0x2.p+1"
    @test Printf.@sprintf("%#.1a", 3.14) == "0x1.9p+1"
    @test Printf.@sprintf("%#.2a", 3.14) == "0x1.92p+1"
    @test Printf.@sprintf("%.6a", 1.5) == "0x1.800000p+0"

end

@testset "%g" begin

    # %g
    for (val, res) in ((12345678., "1.23457e+07"),
                    (1234567.8, "1.23457e+06"),
                    (123456.78, "123457"),
                    (12345.678, "12345.7"),
                    (12340000.0, "1.234e+07"))
        @test (Printf.@sprintf("%.6g", val) == res)
    end
    for (val, res) in ((big"12345678.", "1.23457e+07"),
                    (big"1234567.8", "1.23457e+06"),
                    (big"123456.78", "123457"),
                    (big"12345.678", "12345.7"))
        @test (Printf.@sprintf("%.6g", val) == res)
    end
    for (fmt, val) in (("%10.5g", "     123.4"),
                    ("%+10.5g", "    +123.4"),
                    ("% 10.5g","     123.4"),
                    ("%#10.5g", "    123.40"),
                    ("%-10.5g", "123.4     "),
                    ("%-+10.5g", "+123.4    "),
                    ("%010.5g", "00000123.4")),
        num in (123.4, big"123.4")
        @test Printf.format(Printf.Format(fmt), num) == val
    end
    @test( Printf.@sprintf( "%10.5g", -123.4 ) == "    -123.4")
    @test( Printf.@sprintf( "%010.5g", -123.4 ) == "-0000123.4")
    @test( Printf.@sprintf( "%.6g", 12340000.0 ) == "1.234e+07")
    @test( Printf.@sprintf( "%#.6g", 12340000.0 ) == "1.23400e+07")
    @test( Printf.@sprintf( "%10.5g", big"-123.4" ) == "    -123.4")
    @test( Printf.@sprintf( "%010.5g", big"-123.4" ) == "-0000123.4")
    @test( Printf.@sprintf( "%.6g", big"12340000.0" ) == "1.234e+07")
    @test( Printf.@sprintf( "%#.6g", big"12340000.0") == "1.23400e+07")

    # %g regression gh #14331
    @test( Printf.@sprintf( "%.5g", 42) == "42")
    @test( Printf.@sprintf( "%#.2g", 42) == "42.")
    @test( Printf.@sprintf( "%#.5g", 42) == "42.000")

    @test Printf.@sprintf("%g", 0.00012) == "0.00012"
    @test Printf.@sprintf("%g", 0.000012) == "1.2e-05"
    @test Printf.@sprintf("%g", 123456.7) == "123457"
    @test Printf.@sprintf("%g", 1234567.8) == "1.23457e+06"

    # %g regression gh #41631
    for (val, res) in ((Inf, "Inf"),
                       (-Inf, "-Inf"),
                       (NaN, "NaN"),
                       (-NaN, "NaN"))
        @test Printf.@sprintf("%g", val) == res
        @test Printf.@sprintf("%G", val) == res
    end

    # zeros
    @test Printf.@sprintf("%.15g", 0) == "0"
    @test Printf.@sprintf("%#.15g", 0) == "0.00000000000000"

end

@testset "%f" begin

    # Inf / NaN handling
    @test (Printf.@sprintf "%f" Inf) == "Inf"
    @test (Printf.@sprintf "%+f" Inf) == "+Inf"
    @test (Printf.@sprintf "% f" Inf) == " Inf"
    @test (Printf.@sprintf "% #f" Inf) == " Inf"
    @test (Printf.@sprintf "%f" -Inf) == "-Inf"
    @test (Printf.@sprintf "%+f" -Inf) == "-Inf"
    @test (Printf.@sprintf "%f" NaN) == "NaN"
    @test (Printf.@sprintf "%+f" NaN) == "+NaN"
    @test (Printf.@sprintf "% f" NaN) == " NaN"
    @test (Printf.@sprintf "% #f" NaN) == " NaN"
    @test (Printf.@sprintf "%e" big"Inf") == "Inf"
    @test (Printf.@sprintf "%e" big"NaN") == "NaN"

    @test (Printf.@sprintf "%.0f" 3e142) == "29999999999999997463140672961703247153805615792184250659629251954072073858354858644285983761764971823910371920726635399393477049701891710124032"

    @test Printf.@sprintf("%f", 1.234) == "1.234000"
    @test Printf.@sprintf("%F", 1.234) == "1.234000"
    @test Printf.@sprintf("%+f", 1.234) == "+1.234000"
    @test Printf.@sprintf("% f", 1.234) == " 1.234000"
    @test Printf.@sprintf("%f", -1.234) == "-1.234000"
    @test Printf.@sprintf("%+f", -1.234) == "-1.234000"
    @test Printf.@sprintf("% f", -1.234) == "-1.234000"
    @test Printf.@sprintf("%#f", 1.234) == "1.234000"
    @test Printf.@sprintf("%.2f", 1.234) == "1.23"
    @test Printf.@sprintf("%.2f", 1.235) == "1.24"
    @test Printf.@sprintf("%.2f", 0.235) == "0.23"
    @test Printf.@sprintf("%4.1f", 1.234) == " 1.2"
    @test Printf.@sprintf("%8.1f", 1.234) == "     1.2"
    @test Printf.@sprintf("%+8.1f", 1.234) == "    +1.2"
    @test Printf.@sprintf("% 8.1f", 1.234) == "     1.2"
    @test Printf.@sprintf("% 7.1f", 1.234) == "    1.2"
    @test Printf.@sprintf("% 08.1f", 1.234) == " 00001.2"
    @test Printf.@sprintf("%08.1f", 1.234) == "000001.2"
    @test Printf.@sprintf("%-08.1f", 1.234) == "1.2     "
    @test Printf.@sprintf("%-8.1f", 1.234) == "1.2     "
    @test Printf.@sprintf("%08.1f", -1.234) == "-00001.2"
    @test Printf.@sprintf("%09.1f", -1.234) == "-000001.2"
    @test Printf.@sprintf("%09.1f", 1.234) == "0000001.2"
    @test Printf.@sprintf("%+09.1f", 1.234) == "+000001.2"
    @test Printf.@sprintf("% 09.1f", 1.234) == " 000001.2"
    @test Printf.@sprintf("%+ 09.1f", 1.234) == "+000001.2"
    @test Printf.@sprintf("%+ 09.1f", 1.234) == "+000001.2"
    @test Printf.@sprintf("%+ 09.0f", 1.234) == "+00000001"
    @test Printf.@sprintf("%+ #09.0f", 1.234) == "+0000001."

    #40303
    @test Printf.@sprintf("%+7.1f", 9.96) == "  +10.0"
    @test Printf.@sprintf("% 7.1f", 9.96) == "   10.0"
end

@testset "%e" begin

    # Inf / NaN handling
    @test (Printf.@sprintf "%e" Inf) == "Inf"
    @test (Printf.@sprintf "%+e" Inf) == "+Inf"
    @test (Printf.@sprintf "% e" Inf) == " Inf"
    @test (Printf.@sprintf "% #e" Inf) == " Inf"
    @test (Printf.@sprintf "%e" -Inf) == "-Inf"
    @test (Printf.@sprintf "%+e" -Inf) == "-Inf"
    @test (Printf.@sprintf "%e" NaN) == "NaN"
    @test (Printf.@sprintf "%+e" NaN) == "+NaN"
    @test (Printf.@sprintf "% e" NaN) == " NaN"
    @test (Printf.@sprintf "% #e" NaN) == " NaN"
    @test (Printf.@sprintf "%e" big"Inf") == "Inf"
    @test (Printf.@sprintf "%e" big"NaN") == "NaN"

    # scientific notation
    @test (Printf.@sprintf "%.0e" 3e142) == "3e+142"
    @test (Printf.@sprintf "%#.0e" 3e142) == "3.e+142"
    @test (Printf.@sprintf "%.0e" big"3e142") == "3e+142"
    @test (Printf.@sprintf "%#.0e" big"3e142") == "3.e+142"

    @test (Printf.@sprintf "%.0e" big"3e1042") == "3e+1042"

    @test (Printf.@sprintf "%e" 3e42) == "3.000000e+42"
    @test (Printf.@sprintf "%E" 3e42) == "3.000000E+42"
    @test (Printf.@sprintf "%e" 3e-42) == "3.000000e-42"
    @test (Printf.@sprintf "%E" 3e-42) == "3.000000E-42"

    @test Printf.@sprintf("%e", 1.234) == "1.234000e+00"
    @test Printf.@sprintf("%E", 1.234) == "1.234000E+00"
    @test Printf.@sprintf("%+e", 1.234) == "+1.234000e+00"
    @test Printf.@sprintf("% e", 1.234) == " 1.234000e+00"
    @test Printf.@sprintf("%e", -1.234) == "-1.234000e+00"
    @test Printf.@sprintf("%+e", -1.234) == "-1.234000e+00"
    @test Printf.@sprintf("% e", -1.234) == "-1.234000e+00"
    @test Printf.@sprintf("%#e", 1.234) == "1.234000e+00"
    @test Printf.@sprintf("%.2e", 1.234) == "1.23e+00"
    @test Printf.@sprintf("%.2e", 1.235) == "1.24e+00"
    @test Printf.@sprintf("%.2e", 0.235) == "2.35e-01"
    @test Printf.@sprintf("%4.1e", 1.234) == "1.2e+00"
    @test Printf.@sprintf("%8.1e", 1.234) == " 1.2e+00"
    @test Printf.@sprintf("%+8.1e", 1.234) == "+1.2e+00"
    @test Printf.@sprintf("% 8.1e", 1.234) == " 1.2e+00"
    @test Printf.@sprintf("% 7.1e", 1.234) == " 1.2e+00"
    @test Printf.@sprintf("% 08.1e", 1.234) == " 1.2e+00"
    @test Printf.@sprintf("%08.1e", 1.234) == "01.2e+00"
    @test Printf.@sprintf("%-08.1e", 1.234) == "1.2e+00 "
    @test Printf.@sprintf("%-8.1e", 1.234) == "1.2e+00 "
    @test Printf.@sprintf("%-8.1e", 1.234) == "1.2e+00 "
    @test Printf.@sprintf("%08.1e", -1.234) == "-1.2e+00"
    @test Printf.@sprintf("%09.1e", -1.234) == "-01.2e+00"
    @test Printf.@sprintf("%09.1e", 1.234) == "001.2e+00"
    @test Printf.@sprintf("%+09.1e", 1.234) == "+01.2e+00"
    @test Printf.@sprintf("% 09.1e", 1.234) == " 01.2e+00"
    @test Printf.@sprintf("%+ 09.1e", 1.234) == "+01.2e+00"
    @test Printf.@sprintf("%+ 09.1e", 1.234) == "+01.2e+00"
    @test Printf.@sprintf("%+ 09.0e", 1.234) == "+0001e+00"
    @test Printf.@sprintf("%+ #09.0e", 1.234) == "+001.e+00"

    #40303
    @test Printf.@sprintf("%+9.1e", 9.96) == " +1.0e+01"
    @test Printf.@sprintf("% 9.1e", 9.96) == "  1.0e+01"
end

@testset "strings" begin

    @test Printf.@sprintf("Hallo heimur") == "Hallo heimur"
    @test Printf.@sprintf("+%s+", "hello") == "+hello+"
    @test Printf.@sprintf("%.1s", "foo") == "f"
    @test Printf.@sprintf("%s", "%%%%") == "%%%%"
    @test Printf.@sprintf("%s", "Hallo heimur") == "Hallo heimur"
    @test Printf.@sprintf("%+s", "Hallo heimur") == "Hallo heimur"
    @test Printf.@sprintf("% s", "Hallo heimur") == "Hallo heimur"
    @test Printf.@sprintf("%+ s", "Hallo heimur") == "Hallo heimur"
    @test Printf.@sprintf("%1s", "Hallo heimur") == "Hallo heimur"
    @test Printf.@sprintf("%20s", "Hallo") == "               Hallo"
    @test Printf.@sprintf("%-20s", "Hallo") == "Hallo               "
    @test Printf.@sprintf("%0-20s", "Hallo") == "Hallo               "
    @test Printf.@sprintf("%.20s", "Hallo heimur") == "Hallo heimur"
    @test Printf.@sprintf("%20.5s", "Hallo heimur") == "               Hallo"
    @test Printf.@sprintf("%.0s", "Hallo heimur") == ""
    @test Printf.@sprintf("%20.0s", "Hallo heimur") == "                    "
    @test Printf.@sprintf("%.s", "Hallo heimur") == ""
    @test Printf.@sprintf("%20.s", "Hallo heimur") == "                    "
    @test (Printf.@sprintf "%s" "test") == "test"
    @test (Printf.@sprintf "%s" "tést") == "tést"
    @test Printf.@sprintf("ø%sø", "hey") == "øheyø"
    @test Printf.@sprintf("%4sø", "ø") == "   øø"
    @test Printf.@sprintf("%-4sø", "ø") == "ø   ø"

    @test (Printf.@sprintf "%8s" "test") == "    test"
    @test (Printf.@sprintf "%-8s" "test") == "test    "

    @test (Printf.@sprintf "%s" :test) == "test"
    @test (Printf.@sprintf "%#s" :test) == ":test"
    @test (Printf.@sprintf "%#8s" :test) == "   :test"
    @test (Printf.@sprintf "%#-8s" :test) == ":test   "

    @test (Printf.@sprintf "%8.3s" "test") == "     tes"
    @test (Printf.@sprintf "%#8.3s" "test") == "     \"te"
    @test (Printf.@sprintf "%-8.3s" "test") == "tes     "
    @test (Printf.@sprintf "%#-8.3s" "test") == "\"te     "
    @test (Printf.@sprintf "%.3s" "test") == "tes"
    @test (Printf.@sprintf "%#.3s" "test") == "\"te"
    @test (Printf.@sprintf "%-.3s" "test") == "tes"
    @test (Printf.@sprintf "%#-.3s" "test") == "\"te"

    # issue #41068
    @test Printf.@sprintf("%.2s", "föó") == "fö"
    @test Printf.@sprintf("%5s", "föó") == "  föó"
    @test Printf.@sprintf("%6s", "😍🍕") == "  😍🍕"
    @test Printf.@sprintf("%2c", '🍕') == "🍕"
    @test Printf.@sprintf("%3c", '🍕') == " 🍕"
end

@testset "chars" begin

    @test Printf.@sprintf("%c", 'a') == "a"
    @test Printf.@sprintf("%c",  32) == " "
    @test Printf.@sprintf("%c",  36) == "\$"
    @test Printf.@sprintf("%3c", 'a') == "  a"
    @test Printf.@sprintf( "%c", 'x') == "x"
    @test Printf.@sprintf("%+c", 'x') == "x"
    @test Printf.@sprintf("% c", 'x') == "x"
    @test Printf.@sprintf("%+ c", 'x') == "x"
    @test Printf.@sprintf("%1c", 'x') == "x"
    @test Printf.@sprintf("%20c"  , 'x') == "                   x"
    @test Printf.@sprintf("%-20c" , 'x') == "x                   "
    @test Printf.@sprintf("%-020c", 'x') == "x                   "
    @test Printf.@sprintf("%c", 65) == "A"
    @test Printf.@sprintf("%c", 'A') == "A"
    @test Printf.@sprintf("%3c", 'A') == "  A"
    @test Printf.@sprintf("%-3c", 'A') == "A  "
    @test Printf.@sprintf("%c", 248) == "ø"
    @test Printf.@sprintf("%c", 'ø') == "ø"
    @test Printf.@sprintf("%c", "ø") == "ø"
    @test Printf.@sprintf("%c", '𐀀') == "𐀀"

end

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
        fmtd = Printf.format(Printf.Format(fmt_string), val)
        @test fmtd == ans
    end
end

@testset "basics" begin

    @test Printf.@sprintf("%%") == "%"
    @test Printf.@sprintf("1%%") == "1%"
    @test Printf.@sprintf("%%1") == "%1"
    @test Printf.@sprintf("1%%2") == "1%2"
    @test Printf.@sprintf("1%%%d", 2) == "1%2"
    @test Printf.@sprintf("1%%2%%3") == "1%2%3"
    @test Printf.@sprintf("GAP[%%]") == "GAP[%]"
    @test Printf.@sprintf("hey there") == "hey there"
    @test_throws ArgumentError Printf.Format("")
    @test_throws ArgumentError Printf.Format("%+")
    @test_throws ArgumentError Printf.Format("%.")
    @test_throws ArgumentError Printf.Format("%.0")
    @test isempty(Printf.Format("%%").formats)
    @test Printf.@sprintf("%d%d", 1, 2) == "12"
    @test (Printf.@sprintf "%d%d" [1 2]...) == "12"
    @test (Printf.@sprintf("X%d", 2)) == "X2"
    @test (Printf.@sprintf("\u00d0%d", 2)) == "\u00d02"
    @test (Printf.@sprintf("\u0f00%d", 2)) == "\u0f002"
    @test (Printf.@sprintf("\U0001ffff%d", 2)) == "\U0001ffff2"
    @test (Printf.@sprintf("%dX%d", 1, 2)) == "1X2"
    @test (Printf.@sprintf("%d\u00d0%d", 1, 2)) == "1\u00d02"
    @test (Printf.@sprintf("%d\u0f00%d", 1, 2)) == "1\u0f002"
    @test (Printf.@sprintf("%d\U0001ffff%d", 1, 2)) == "1\U0001ffff2"
    @test (Printf.@sprintf("%d\u2203%d\u0203", 1, 2)) == "1\u22032\u0203"
    @test_throws ArgumentError Printf.Format("%y%d")
    @test_throws ArgumentError Printf.Format("%\u00d0%d")
    @test_throws ArgumentError Printf.Format("%\u0f00%d")
    @test_throws ArgumentError Printf.Format("%\U0001ffff%d")
    @test Printf.@sprintf("%10.5d", 4) == "     00004"
    @test (Printf.@sprintf "%d" typemax(Int64)) == "9223372036854775807"

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
        @test Printf.format(Printf.Format(fmt), num) == val
    end

    for (fmt, val) in (("%i", "42"),
                   ("%u", "42"),
                   ("Test: %i", "Test: 42"),
                   ("%#x", "0x2a"),
                   ("%x", "2a"),
                   ("%X", "2A"),
                   ("% i", " 42"),
                   ("%+i", "+42"),
                   ("%4i", "  42"),
                   ("%-4i", "42  "),
                   ("%f", "42.000000"),
                   ("%g", "42"),
                   ("%e", "4.200000e+01")),
        num in (UInt16(42), UInt32(42), UInt64(42), UInt128(42),
                Int16(42), Int32(42), Int64(42), Int128(42), big"42")
        @test Printf.format(Printf.Format(fmt), num) == val
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

    # reasonably complex
    @test (Printf.@sprintf "Test: %s%c%C%c%#-.0f." "t" 65 66 67 -42) == "Test: tABC-42.."

    # combo
    @test (Printf.@sprintf "%f %d %d %f" 1.0 [3 4]... 5) == "1.000000 3 4 5.000000"

    # multi
    @test (Printf.@sprintf "%s %f %9.5f %d %d %d %d%d%d%d" [1:6;]... [7,8,9,10]...) == "1 2.000000   3.00000 4 5 6 78910"

    # comprehension
    @test (Printf.@sprintf "%s %s %s %d %d %d %f %f %f" Any[10^x+y for x=1:3,y=1:3 ]...) == "11 101 1001 12 102 1002 13.000000 103.000000 1003.000000"

    # more than 16 formats/args
    @test (Printf.@sprintf "%s %s %s %d %d %d %f %f %f %s %s %s %d %d %d %f %f %f" Any[10*x+(x+1) for x=1:18 ]...) ==
        "12 23 34 45 56 67 78.000000 89.000000 100.000000 111 122 133 144 155 166 177.000000 188.000000 199.000000"

    # Check bug with trailing nul printing BigFloat
    @test (Printf.@sprintf("%.330f", BigFloat(1)))[end] != '\0'

    # Check bugs with truncated output printing BigFloat
    @test (Printf.@sprintf("%f", parse(BigFloat, "1e400"))) ==
           "10000000000000000000000000000000000000000000000000000000000000000000000000000025262527574416492004687051900140830217136998040684679611623086405387447100385714565637522507383770691831689647535911648520404034824470543643098638520633064715221151920028135130764414460468236314621044034960475540018328999334468948008954289495190631358190153259681118693204411689043999084305348398480210026863210192871358464.000000"

    # Check that does not attempt to output incredibly large amounts of digits
    @test_throws ErrorException Printf.@sprintf("%f", parse(BigFloat, "1e99999"))

    # Check bug with precision > length of string
    @test Printf.@sprintf("%4.2s", "a") == "   a"

    # issue #29662
    @test (Printf.@sprintf "%12.3e" pi*1e100) == "  3.142e+100"

    @test string(Printf.Format("%a").formats[1]) == "%a"
    @test string(Printf.Format("%a").formats[1]; modifier="R") == "%Ra"

    @test Printf.@sprintf("%d", 3.14) == "3"
    @test Printf.@sprintf("%2d", 3.14) == " 3"
    @test Printf.@sprintf("%2d", big(3.14)) == " 3"
    @test Printf.@sprintf("%s", 1) == "1"
    @test Printf.@sprintf("%f", 1) == "1.000000"
    @test Printf.@sprintf("%e", 1) == "1.000000e+00"
    @test Printf.@sprintf("%g", 1) == "1"

    # issue #39748
    @test Printf.@sprintf("%.16g", 194.4778127560983) == "194.4778127560983"
    @test Printf.@sprintf("%.17g", 194.4778127560983) == "194.4778127560983"
    @test Printf.@sprintf("%.18g", 194.4778127560983) == "194.477812756098302"
    @test Printf.@sprintf("%.1g", 1.7976931348623157e308) == "2e+308"
    @test Printf.@sprintf("%.2g", 1.7976931348623157e308) == "1.8e+308"
    @test Printf.@sprintf("%.3g", 1.7976931348623157e308) == "1.8e+308"

    # escaped '%'
    @test_throws ArgumentError @sprintf("%s%%%s", "a")
    @test @sprintf("%s%%%s", "a", "b") == "a%b"

    # print float as %d uses round(x)
    @test @sprintf("%d", 25.5) == "26"

    # 37539
    @test @sprintf(" %.1e\n", 0.999) == " 1.0e+00\n"
    @test @sprintf("   %.1f", 9.999) == "   10.0"

    # 37552
    @test @sprintf("%d", 1.0e100) == "10000000000000000159028911097599180468360808563945281389781327557747838772170381060813469985856815104"
    @test @sprintf("%d", 3//1) == "3"
    @test @sprintf("%d", Inf) == "Inf"
    @test @sprintf(" %d", NaN) == " NaN"
end

@testset "integers" begin

    @test Printf.@sprintf( "% d",  42) == " 42"
    @test Printf.@sprintf( "% d", -42) == "-42"
    @test Printf.@sprintf( "% 5d",  42) == "   42"
    @test Printf.@sprintf( "% 5d", -42) == "  -42"
    @test Printf.@sprintf( "% 15d",  42) == "             42"
    @test Printf.@sprintf( "% 15d", -42) == "            -42"
    @test Printf.@sprintf("%+d",  42) == "+42"
    @test Printf.@sprintf("%+d", -42) == "-42"
    @test Printf.@sprintf("%+5d",  42) == "  +42"
    @test Printf.@sprintf("%+5d", -42) == "  -42"
    @test Printf.@sprintf("%+15d",  42) == "            +42"
    @test Printf.@sprintf("%+15d", -42) == "            -42"
    @test Printf.@sprintf( "%0d",  42) == "42"
    @test Printf.@sprintf( "%0d", -42) == "-42"
    @test Printf.@sprintf( "%05d",  42) == "00042"
    @test Printf.@sprintf( "%05d", -42) == "-0042"
    @test Printf.@sprintf( "%015d",  42) == "000000000000042"
    @test Printf.@sprintf( "%015d", -42) == "-00000000000042"
    @test Printf.@sprintf("%-d",  42) == "42"
    @test Printf.@sprintf("%-d", -42) == "-42"
    @test Printf.@sprintf("%-5d",  42) == "42   "
    @test Printf.@sprintf("%-5d", -42) == "-42  "
    @test Printf.@sprintf("%-15d",  42) == "42             "
    @test Printf.@sprintf("%-15d", -42) == "-42            "
    @test Printf.@sprintf("%-0d",  42) == "42"
    @test Printf.@sprintf("%-0d", -42) == "-42"
    @test Printf.@sprintf("%-05d",  42) == "42   "
    @test Printf.@sprintf("%-05d", -42) == "-42  "
    @test Printf.@sprintf("%-015d",  42) == "42             "
    @test Printf.@sprintf("%-015d", -42) == "-42            "
    @test Printf.@sprintf( "%0-d",  42) == "42"
    @test Printf.@sprintf( "%0-d", -42) == "-42"
    @test Printf.@sprintf( "%0-5d",  42) == "42   "
    @test Printf.@sprintf( "%0-5d", -42) == "-42  "
    @test Printf.@sprintf( "%0-15d",  42) == "42             "
    @test Printf.@sprintf( "%0-15d", -42) == "-42            "
    @test_throws ArgumentError Printf.Format("%d %")

    @test Printf.@sprintf("%lld", 18446744065119617025) == "18446744065119617025"
    @test Printf.@sprintf("%+8lld", 100) == "    +100"
    @test Printf.@sprintf("%+.8lld", 100) == "+00000100"
    @test Printf.@sprintf("%+10.8lld", 100) == " +00000100"
    @test_throws ArgumentError Printf.Format("%_1lld")
    @test Printf.@sprintf("%-1.5lld", -100) == "-00100"
    @test Printf.@sprintf("%5lld", 100) == "  100"
    @test Printf.@sprintf("%5lld", -100) == " -100"
    @test Printf.@sprintf("%-5lld", 100) == "100  "
    @test Printf.@sprintf("%-5lld", -100) == "-100 "
    @test Printf.@sprintf("%-.5lld", 100) == "00100"
    @test Printf.@sprintf("%-.5lld", -100) == "-00100"
    @test Printf.@sprintf("%-8.5lld", 100) == "00100   "
    @test Printf.@sprintf("%-8.5lld", -100) == "-00100  "
    @test Printf.@sprintf("%05lld", 100) == "00100"
    @test Printf.@sprintf("%05lld", -100) == "-0100"
    @test Printf.@sprintf("% lld", 100) == " 100"
    @test Printf.@sprintf("% lld", -100) == "-100"
    @test Printf.@sprintf("% 5lld", 100) == "  100"
    @test Printf.@sprintf("% 5lld", -100) == " -100"
    @test Printf.@sprintf("% .5lld", 100) == " 00100"
    @test Printf.@sprintf("% .5lld", -100) == "-00100"
    @test Printf.@sprintf("% 8.5lld", 100) == "   00100"
    @test Printf.@sprintf("% 8.5lld", -100) == "  -00100"
    @test Printf.@sprintf("%.0lld", 0) == "0"
    @test Printf.@sprintf("%#+21.18llx", -100) == "-0x000000000000000064"
    @test Printf.@sprintf("%#.25llo", -100) == "-00000000000000000000000144"
    @test Printf.@sprintf("%#+24.20llo", -100) == "  -000000000000000000144"
    @test Printf.@sprintf("%#+18.21llX", -100) == "-0X000000000000000000064"
    @test Printf.@sprintf("%#+20.24llo", -100) == "-0000000000000000000000144"
    @test Printf.@sprintf("%#+25.22llu", -1) == "  -0000000000000000000001"
    @test Printf.@sprintf("%#+25.22llu", -1) == "  -0000000000000000000001"
    @test Printf.@sprintf("%#+30.25llu", -1) == "    -0000000000000000000000001"
    @test Printf.@sprintf("%+#25.22lld", -1) == "  -0000000000000000000001"
    @test Printf.@sprintf("%#-8.5llo", 100) == "000144  "
    @test Printf.@sprintf("%#-+ 08.5lld", 100) == "+00100  "
    @test Printf.@sprintf("%#-+ 08.5lld", 100) == "+00100  "
    @test Printf.@sprintf("%.40lld",  1) == "0000000000000000000000000000000000000001"
    @test Printf.@sprintf("% .40lld",  1) == " 0000000000000000000000000000000000000001"
    @test Printf.@sprintf("% .40d",  1) == " 0000000000000000000000000000000000000001"
    @test Printf.@sprintf("%lld",  18446744065119617025) == "18446744065119617025"

    @test Printf.@sprintf("+%d+",  10) == "+10+"
    @test Printf.@sprintf("%#012x",  1) == "0x0000000001"
    @test Printf.@sprintf("%#04.8x",  1) == "0x00000001"

    @test Printf.@sprintf("%#-08.2x",  1) == "0x01    "
    @test Printf.@sprintf("%#08o",  1) == "00000001"
    @test Printf.@sprintf("%d",  1024) == "1024"
    @test Printf.@sprintf("%d", -1024) == "-1024"
    @test Printf.@sprintf("%i",  1024) == "1024"
    @test Printf.@sprintf("%i", -1024) == "-1024"
    @test Printf.@sprintf("%u",  1024) == "1024"
    @test Printf.@sprintf("%u",  UInt(4294966272)) == "4294966272"
    @test Printf.@sprintf("%o",  511) == "777"
    @test Printf.@sprintf("%o",  UInt(4294966785)) == "37777777001"
    @test Printf.@sprintf("%x",  305441741) == "1234abcd"
    @test Printf.@sprintf("%x",  UInt(3989525555)) == "edcb5433"
    @test Printf.@sprintf("%X",  305441741) == "1234ABCD"
    @test Printf.@sprintf("%X",  UInt(3989525555)) == "EDCB5433"
    @test Printf.@sprintf("%+d",  1024) == "+1024"
    @test Printf.@sprintf("%+d", -1024) == "-1024"
    @test Printf.@sprintf("%+i",  1024) == "+1024"
    @test Printf.@sprintf("%+i", -1024) == "-1024"
    @test Printf.@sprintf("%+u",  1024) == "+1024"
    @test Printf.@sprintf("%+u",  UInt(4294966272)) == "+4294966272"
    @test Printf.@sprintf("%+o",  511) == "+777"
    @test Printf.@sprintf("%+o",  UInt(4294966785)) == "+37777777001"
    @test Printf.@sprintf("%+x",  305441741) == "+1234abcd"
    @test Printf.@sprintf("%+x",  UInt(3989525555)) == "+edcb5433"
    @test Printf.@sprintf("%+X",  305441741) == "+1234ABCD"
    @test Printf.@sprintf("%+X",  UInt(3989525555)) == "+EDCB5433"
    @test Printf.@sprintf("% d",  1024) == " 1024"
    @test Printf.@sprintf("% d", -1024) == "-1024"
    @test Printf.@sprintf("% i",  1024) == " 1024"
    @test Printf.@sprintf("% i", -1024) == "-1024"
    @test Printf.@sprintf("% u",  1024) == " 1024"
    @test Printf.@sprintf("% u",  UInt(4294966272)) == " 4294966272"
    @test Printf.@sprintf("% o",  511) == " 777"
    @test Printf.@sprintf("% o",  UInt(4294966785)) == " 37777777001"
    @test Printf.@sprintf("% x",  305441741) == " 1234abcd"
    @test Printf.@sprintf("% x",  UInt(3989525555)) == " edcb5433"
    @test Printf.@sprintf("% X",  305441741) == " 1234ABCD"
    @test Printf.@sprintf("% X",  UInt(3989525555)) == " EDCB5433"
    @test Printf.@sprintf("%+ d",  1024) == "+1024"
    @test Printf.@sprintf("%+ d", -1024) == "-1024"
    @test Printf.@sprintf("%+ i",  1024) == "+1024"
    @test Printf.@sprintf("%+ i", -1024) == "-1024"
    @test Printf.@sprintf("%+ u",  1024) == "+1024"
    @test Printf.@sprintf("%+ u",  UInt(4294966272)) == "+4294966272"
    @test Printf.@sprintf("%+ o",  511) == "+777"
    @test Printf.@sprintf("%+ o",  UInt(4294966785)) == "+37777777001"
    @test Printf.@sprintf("%+ x",  305441741) == "+1234abcd"
    @test Printf.@sprintf("%+ x",  UInt(3989525555)) == "+edcb5433"
    @test Printf.@sprintf("%+ X",  305441741) == "+1234ABCD"
    @test Printf.@sprintf("%+ X",  UInt(3989525555)) == "+EDCB5433"
    @test Printf.@sprintf("%#o",  511) == "0777"
    @test Printf.@sprintf("%#o",  UInt(4294966785)) == "037777777001"
    @test Printf.@sprintf("%#x",  305441741) == "0x1234abcd"
    @test Printf.@sprintf("%#x",  UInt(3989525555)) == "0xedcb5433"
    @test Printf.@sprintf("%#X",  305441741) == "0X1234ABCD"
    @test Printf.@sprintf("%#X",  UInt(3989525555)) == "0XEDCB5433"
    @test Printf.@sprintf("%#o",  UInt(0)) == "00"
    @test Printf.@sprintf("%#x",  UInt(0)) == "0x0"
    @test Printf.@sprintf("%#X",  UInt(0)) == "0X0"
    @test Printf.@sprintf("%1d",  1024) == "1024"
    @test Printf.@sprintf("%1d", -1024) == "-1024"
    @test Printf.@sprintf("%1i",  1024) == "1024"
    @test Printf.@sprintf("%1i", -1024) == "-1024"
    @test Printf.@sprintf("%1u",  1024) == "1024"
    @test Printf.@sprintf("%1u",  UInt(4294966272)) == "4294966272"
    @test Printf.@sprintf("%1o",  511) == "777"
    @test Printf.@sprintf("%1o",  UInt(4294966785)) == "37777777001"
    @test Printf.@sprintf("%1x",  305441741) == "1234abcd"
    @test Printf.@sprintf("%1x",  UInt(3989525555)) == "edcb5433"
    @test Printf.@sprintf("%1X",  305441741) == "1234ABCD"
    @test Printf.@sprintf("%1X",  UInt(3989525555)) == "EDCB5433"
    @test Printf.@sprintf("%20d",  1024) == "                1024"
    @test Printf.@sprintf("%20d", -1024) == "               -1024"
    @test Printf.@sprintf("%20i",  1024) == "                1024"
    @test Printf.@sprintf("%20i", -1024) == "               -1024"
    @test Printf.@sprintf("%20u",  1024) == "                1024"
    @test Printf.@sprintf("%20u",  UInt(4294966272)) == "          4294966272"
    @test Printf.@sprintf("%20o",  511) == "                 777"
    @test Printf.@sprintf("%20o",  UInt(4294966785)) == "         37777777001"
    @test Printf.@sprintf("%20x",  305441741) == "            1234abcd"
    @test Printf.@sprintf("%20x",  UInt(3989525555)) == "            edcb5433"
    @test Printf.@sprintf("%20X",  305441741) == "            1234ABCD"
    @test Printf.@sprintf("%20X",  UInt(3989525555)) == "            EDCB5433"
    @test Printf.@sprintf("%-20d",  1024) == "1024                "
    @test Printf.@sprintf("%-20d", -1024) == "-1024               "
    @test Printf.@sprintf("%-20i",  1024) == "1024                "
    @test Printf.@sprintf("%-20i", -1024) == "-1024               "
    @test Printf.@sprintf("%-20u",  1024) == "1024                "
    @test Printf.@sprintf("%-20u",  UInt(4294966272)) == "4294966272          "
    @test Printf.@sprintf("%-20o",  511) == "777                 "
    @test Printf.@sprintf("%-20o",  UInt(4294966785)) == "37777777001         "
    @test Printf.@sprintf("%-20x",  305441741) == "1234abcd            "
    @test Printf.@sprintf("%-20x",  UInt(3989525555)) == "edcb5433            "
    @test Printf.@sprintf("%-20X",  305441741) == "1234ABCD            "
    @test Printf.@sprintf("%-20X",  UInt(3989525555)) == "EDCB5433            "
    @test Printf.@sprintf("%020d",  1024) == "00000000000000001024"
    @test Printf.@sprintf("%020d", -1024) == "-0000000000000001024"
    @test Printf.@sprintf("%020i",  1024) == "00000000000000001024"
    @test Printf.@sprintf("%020i", -1024) == "-0000000000000001024"
    @test Printf.@sprintf("%020u",  1024) == "00000000000000001024"
    @test Printf.@sprintf("%020u",  UInt(4294966272)) == "00000000004294966272"
    @test Printf.@sprintf("%020o",  511) == "00000000000000000777"
    @test Printf.@sprintf("%020o",  UInt(4294966785)) == "00000000037777777001"
    @test Printf.@sprintf("%020x",  305441741) == "0000000000001234abcd"
    @test Printf.@sprintf("%020x",  UInt(3989525555)) == "000000000000edcb5433"
    @test Printf.@sprintf("%020X",  305441741) == "0000000000001234ABCD"
    @test Printf.@sprintf("%020X",  UInt(3989525555)) == "000000000000EDCB5433"
    @test Printf.@sprintf("%#20o",  511) == "                0777"
    @test Printf.@sprintf("%#20o",  UInt(4294966785)) == "        037777777001"
    @test Printf.@sprintf("%#20x",  305441741) == "          0x1234abcd"
    @test Printf.@sprintf("%#20x",  UInt(3989525555)) == "          0xedcb5433"
    @test Printf.@sprintf("%#20X",  305441741) == "          0X1234ABCD"
    @test Printf.@sprintf("%#20X",  UInt(3989525555)) == "          0XEDCB5433"
    @test Printf.@sprintf("%#020o",  511) == "00000000000000000777"
    @test Printf.@sprintf("%#020o",  UInt(4294966785)) == "00000000037777777001"
    @test Printf.@sprintf("%#020x",  305441741) == "0x00000000001234abcd"
    @test Printf.@sprintf("%#020x",  UInt(3989525555)) == "0x0000000000edcb5433"
    @test Printf.@sprintf("%#020X",  305441741) == "0X00000000001234ABCD"
    @test Printf.@sprintf("%#020X",  UInt(3989525555)) == "0X0000000000EDCB5433"
    @test Printf.@sprintf("%0-20d",  1024) == "1024                "
    @test Printf.@sprintf("%0-20d", -1024) == "-1024               "
    @test Printf.@sprintf("%0-20i",  1024) == "1024                "
    @test Printf.@sprintf("%0-20i", -1024) == "-1024               "
    @test Printf.@sprintf("%0-20u",  1024) == "1024                "
    @test Printf.@sprintf("%0-20u",  UInt(4294966272)) == "4294966272          "
    @test Printf.@sprintf("%-020o",  511) == "777                 "
    @test Printf.@sprintf("%-020o",  UInt(4294966785)) == "37777777001         "
    @test Printf.@sprintf("%-020x",  305441741) == "1234abcd            "
    @test Printf.@sprintf("%-020x",  UInt(3989525555)) == "edcb5433            "
    @test Printf.@sprintf("%-020X",  305441741) == "1234ABCD            "
    @test Printf.@sprintf("%-020X",  UInt(3989525555)) == "EDCB5433            "
    @test Printf.@sprintf("%.20d",  1024) == "00000000000000001024"
    @test Printf.@sprintf("%.20d", -1024) == "-00000000000000001024"
    @test Printf.@sprintf("%.20i",  1024) == "00000000000000001024"
    @test Printf.@sprintf("%.20i", -1024) == "-00000000000000001024"
    @test Printf.@sprintf("%.20u",  1024) == "00000000000000001024"
    @test Printf.@sprintf("%.20u",  UInt(4294966272)) == "00000000004294966272"
    @test Printf.@sprintf("%.20o",  511) == "00000000000000000777"
    @test Printf.@sprintf("%.20o",  UInt(4294966785)) == "00000000037777777001"
    @test Printf.@sprintf("%.20x",  305441741) == "0000000000001234abcd"
    @test Printf.@sprintf("%.20x",  UInt(3989525555)) == "000000000000edcb5433"
    @test Printf.@sprintf("%.20X",  305441741) == "0000000000001234ABCD"
    @test Printf.@sprintf("%.20X",  UInt(3989525555)) == "000000000000EDCB5433"
    @test Printf.@sprintf("%20.5d",  1024) == "               01024"
    @test Printf.@sprintf("%20.5d", -1024) == "              -01024"
    @test Printf.@sprintf("%20.5i",  1024) == "               01024"
    @test Printf.@sprintf("%20.5i", -1024) == "              -01024"
    @test Printf.@sprintf("%20.5u",  1024) == "               01024"
    @test Printf.@sprintf("%20.5u",  UInt(4294966272)) == "          4294966272"
    @test Printf.@sprintf("%20.5o",  511) == "               00777"
    @test Printf.@sprintf("%20.5o",  UInt(4294966785)) == "         37777777001"
    @test Printf.@sprintf("%20.5x",  305441741) == "            1234abcd"
    @test Printf.@sprintf("%20.10x",  UInt(3989525555)) == "          00edcb5433"
    @test Printf.@sprintf("%20.5X",  305441741) == "            1234ABCD"
    @test Printf.@sprintf("%20.10X",  UInt(3989525555)) == "          00EDCB5433"
    @test Printf.@sprintf("%020.5d",  1024) == "               01024"
    @test Printf.@sprintf("%020.5d", -1024) == "              -01024"
    @test Printf.@sprintf("%020.5i",  1024) == "               01024"
    @test Printf.@sprintf("%020.5i", -1024) == "              -01024"
    @test Printf.@sprintf("%020.5u",  1024) == "               01024"
    @test Printf.@sprintf("%020.5u",  UInt(4294966272)) == "          4294966272"
    @test Printf.@sprintf("%020.5o",  511) == "               00777"
    @test Printf.@sprintf("%020.5o",  UInt(4294966785)) == "         37777777001"
    @test Printf.@sprintf("%020.5x",  305441741) == "            1234abcd"
    @test Printf.@sprintf("%020.10x",  UInt(3989525555)) == "          00edcb5433"
    @test Printf.@sprintf("%020.5X",  305441741) == "            1234ABCD"
    @test Printf.@sprintf("%020.10X",  UInt(3989525555)) == "          00EDCB5433"
    @test Printf.@sprintf("%20.0d",  1024) == "                1024"
    @test Printf.@sprintf("%20.d", -1024) == "               -1024"
    @test Printf.@sprintf("%20.d",  0) == "                   0"
    @test Printf.@sprintf("%20.0i",  1024) == "                1024"
    @test Printf.@sprintf("%20.i", -1024) == "               -1024"
    @test Printf.@sprintf("%20.i",  0) == "                   0"
    @test Printf.@sprintf("%20.u",  1024) == "                1024"
    @test Printf.@sprintf("%20.0u",  UInt(4294966272)) == "          4294966272"
    @test Printf.@sprintf("%20.u",  UInt(0)) == "                   0"
    @test Printf.@sprintf("%20.o",  511) == "                 777"
    @test Printf.@sprintf("%20.0o",  UInt(4294966785)) == "         37777777001"
    @test Printf.@sprintf("%20.o",  UInt(0)) == "                   0"
    @test Printf.@sprintf("%20.x",  305441741) == "            1234abcd"
    @test Printf.@sprintf("%20.0x",  UInt(3989525555)) == "            edcb5433"
    @test Printf.@sprintf("%20.x",  UInt(0)) == "                   0"
    @test Printf.@sprintf("%20.X",  305441741) == "            1234ABCD"
    @test Printf.@sprintf("%20.0X",  UInt(3989525555)) == "            EDCB5433"
    @test Printf.@sprintf("%20.X",  UInt(0)) == "                   0"

end

@testset "%n" begin
    x = Ref{Int}()
    @test (Printf.@sprintf("%d4%n", 123, x); x[] == 4)
    @test (Printf.@sprintf("%s%n", "😉", x); x[] == 4)
    @test (Printf.@sprintf("%s%n", "1234", x); x[] == 4)
end

end # @testset "Printf"

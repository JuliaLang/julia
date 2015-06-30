# This file is a part of Julia. License is MIT: http://julialang.org/license

# printf
# int
@test (@sprintf "%d" typemax(Int64)) == "9223372036854775807"
@test (@sprintf "%i" 42) == "42"
@test (@sprintf "%u" 42) == "42"
@test (@sprintf "Test: %i" 42) == "Test: 42"
@test (@sprintf "%#x" 42) == "0x2a"
@test (@sprintf "%#o" 42) == "052"
@test (@sprintf "%X" 42) == "2A"
@test (@sprintf "%X" 42) == "2A"
@test (@sprintf "% i" 42) == " 42"
@test (@sprintf "%+i" 42) == "+42"
@test (@sprintf "%4i" 42) == "  42"
@test (@sprintf "%-4i" 42) == "42  "
# float
@test (@sprintf "%7.2f" 1.2345) == "   1.23"
@test (@sprintf "%-7.2f" 1.2345) == "1.23   "
@test (@sprintf "%07.2f" 1.2345) == "0001.23"
@test (@sprintf "%.0f" 1.2345) == "1"
@test (@sprintf "%#.0f" 1.2345) == "1."
# Inf / NaN handling
@test (@sprintf "%f" Inf) == "Inf"
@test (@sprintf "%f" NaN) == "NaN"
# scientific notation
@test (@sprintf "%.4e" 1.2345) == "1.2345e+00"
@test (@sprintf "%.0e" 3e142) == "3e+142"
@test (@sprintf "%#.0e" 3e142) == "3.e+142"
# %g
@test( @sprintf( "%.6g", 12345678. ) == "1.23457e+07" )
@test( @sprintf( "%.6g", 1234567.8 ) == "1.23457e+06" )
@test( @sprintf( "%.6g", 123456.78 ) == "123457" )
@test( @sprintf( "%.6g", 12345.678 ) == "12345.7" )
@test( @sprintf( "%10.5g", 123.4 ) == "     123.4")
@test( @sprintf( "%+10.5g", 123.4 ) == "    +123.4")
@test( @sprintf( "% 10.5g", 123.4 ) == "     123.4")
@test( @sprintf( "%#10.5g", 123.4 ) == "    123.40")
@test( @sprintf( "%-10.5g", 123.4 ) == "123.4     ")
@test( @sprintf( "%-+10.5g", 123.4 ) == "+123.4    ")
@test( @sprintf( "%10.5g", -123.4 ) == "    -123.4")
@test( @sprintf( "%010.5g", 123.4 ) == "00000123.4")
@test( @sprintf( "%010.5g", -123.4 ) == "-0000123.4")
@test( @sprintf( "%.6g", 12340000.0 ) == "1.234e+07")
@test( @sprintf( "%#.6g", 12340000.0 ) == "1.23400e+07")
# hex float
@test (@sprintf "%a" 1.5) == "0x1.8p+0"
@test (@sprintf "%#.0a" 1.5) == "0x2.p+0"
@test (@sprintf "%+30a" 1/3) == "         +0x1.5555555555555p-2"
# chars
@test (@sprintf "%c" 65) == "A"
@test (@sprintf "%c" 'A') == "A"
@test (@sprintf "%c" 248) == "ø"
@test (@sprintf "%c" 'ø') == "ø"
# strings
@test (@sprintf "%s" "test") == "test"
@test (@sprintf "%s" "tést") == "tést"
# reasonably complex
@test (@sprintf "Test: %s%c%C%c%#-.0f." "t" 65 66 67 -42) == "Test: tABC-42.."
#test simple splatting
@test (@sprintf "%d%d" [1 2]...) == "12"
# combo
@test (@sprintf "%f %d %d %f" 1.0 [3 4]... 5) == "1.000000 3 4 5.000000"
# multi
@test (@sprintf "%s %f %9.5f %d %d %d %d%d%d%d" [1:6;]... [7,8,9,10]...) == "1 2.000000   3.00000 4 5 6 78910"
# comprehension
@test (@sprintf "%s %s %s %d %d %d %f %f %f" Any[10^x+y for x=1:3,y=1:3 ]...) == "11 101 1001 12 102 1002 13.000000 103.000000 1003.000000"

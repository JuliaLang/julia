@test mktemp() do _, file
    skipchars(file, isspace)
    true
end

@test eof(skipchars(IOBuffer("    "), isspace))
@test eof(skipchars(IOBuffer("#    \n   "), isspace, linecomment='#'))

macro test_skipchars(str, expected_char, lnc=Char(0xffffffff))
    quote
        io = skipchars(IOBuffer($str), isspace, linecomment=$lnc)
        @test !eof(io) && read(io, Char) == $expected_char
    end
end

@test_skipchars "abc" 'a'
@test_skipchars "   bac" 'b'
@test_skipchars "  #cm \n   cab" 'c' '#'


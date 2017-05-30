# This file is a part of Julia. License is MIT: https://julialang.org/license

# Test skipchars for IOStreams
mktemp() do path, file
    function append_to_file(str)
        mark(file)
        print(file, str)
        flush(file)
        reset(file)
    end

    # test it doesn't error on eof
    @test eof(skipchars(file, isspace))

    # test it correctly skips
    append_to_file("    ")
    @test eof(skipchars(file, isspace))

    # test it correctly detects comment lines
    append_to_file("#    \n   ")
    @test eof(skipchars(file, isspace, linecomment='#'))

    # test it stops at the appropriate time
    append_to_file("   not a space")
    @test !eof(skipchars(file, isspace))
    @test read(file, Char) == 'n'

    # test it correctly ignores the contents of comment lines
    append_to_file("  #not a space \n   not a space")
    @test !eof(skipchars(file, isspace, linecomment='#'))
    @test read(file, Char) == 'n'

    # test it correctly handles unicode
    for (byte,char) in zip(1:4, ('@','߷','࿊','𐋺'))
        append_to_file("abcdef$char")
        @test Base.codelen(char) == byte
        @test !eof(skipchars(file, isalpha))
        @test read(file, Char) == char
    end
end


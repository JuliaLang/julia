# Test skipchars for IOStreams
mktemp() do path, file
    function append_to_file(str)
        mark(file)
        println(file, str)
        flush(file)
        reset(file)
    end

    # test it doesn't error on eof
    @test skipchars(file, isspace) == file

    # test if it correctly skips
    append_to_file("    ")
    @test eof(skipchars(file, isspace))

    # test it correctly detects comment lines
    append_to_file("#    \n   ")
    @test eof(skipchars(file, isspace, linecomment='#'))

    # test it stops at the appropriate time
    append_to_file("   not a space")
    @test skipchars(file, isspace) == file
    @test !eof(file) && read(file, Char) == 'n'

    # test it correctly ignores the contents of comment lines
    append_to_file("  #not a space \n   not a space")
    @test skipchars(file, isspace, linecomment='#') == file
    @test !eof(file) && read(file, Char) == 'n'
end


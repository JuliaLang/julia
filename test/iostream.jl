# This file is a part of Julia. License is MIT: https://julialang.org/license

@testset "skipchars for IOStream" begin
    mktemp() do path, file
        function append_to_file(str)
            mark(file)
            print(file, str)
            flush(file)
            reset(file)
        end
        # test it doesn't error on eof
        @test eof(skipchars(isspace, file))

        # test it correctly skips
        append_to_file("    ")
        @test eof(skipchars(isspace, file))

        # test it correctly detects comment lines
        append_to_file("#    \n   ")
        @test eof(skipchars(isspace, file, linecomment='#'))

        # test it stops at the appropriate time
        append_to_file("   not a space")
        @test !eof(skipchars(isspace, file))
        @test read(file, Char) == 'n'

        # test it correctly ignores the contents of comment lines
        append_to_file("  #not a space \n   not a space")
        @test !eof(skipchars(isspace, file, linecomment='#'))
        @test read(file, Char) == 'n'

        # test it correctly handles unicode
        for (byte,char) in zip(1:4, ('@','߷','࿊','𐋺'))
            append_to_file("abcdef$char")
            @test Base.codelen(char) == byte
            @test !eof(skipchars(isalpha, file))
            @test read(file, Char) == char
        end
    end
end

@testset "readbytes_some! via readbytes!" begin
    mktemp() do path, file
        function append_to_file(str)
            mark(file)
            print(file, str)
            flush(file)
            reset(file)
        end
        append_to_file("aaaaaaaaaaaaaaaaa")
        b = UInt8[0]
        readbytes!(file, b, all=false)
        @test String(b) == "a"
        # with resizing of b
        b = UInt8[]
        readbytes!(file, b, 2, all=false)
        @test String(b) == "aa"
        # readbytes_all with resizing
        b = UInt8[]
        readbytes!(file, b, 15)
        @test String(b) == "aaaaaaaaaaaaaa"
    end
end
@testset "issue #18755" begin
    mktemp() do path, io
        write(io, zeros(UInt8, 131073))
        @test position(io) == 131073
        write(io, zeros(UInt8, 131073))
        @test position(io) == 262146
    end
end

@test Base.open_flags(read=false, write=true, append=false) == (read=false, write=true, create=true, truncate=true, append=false)

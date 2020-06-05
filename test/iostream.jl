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
        for (byte, char) in zip(1:4, ('@','߷','࿊','𐋺'))
            append_to_file("abcdef$char")
            @test ncodeunits(char) == byte
            @test !eof(skipchars(isletter, file))
            @test read(file, Char) == char
        end
    end
end

@testset "skipuntil for IOStream" begin
    mktemp() do path, file
        function append_to_file(str)
            mark(file)
            print(file, str)
            flush(file)
            reset(file)
        end
        # test it doesn't error on eof
        @test eof(skipuntil(isspace, file))

        # test it correctly skips
        append_to_file("    ")
        @test eof(skipuntil(!isspace, file))

        # test it correctly detects comment lines
        append_to_file("#    \n   ")
        @test eof(skipuntil(!isspace, file, linecomment='#'))

        # test it stops at the appropriate time
        append_to_file("   not a space")
        @test !eof(skipuntil(!isspace, file))
        @test read(file, Char) == 'n'

        # test it correctly ignores the contents of comment lines
        append_to_file("  #not a space \n   not a space")
        @test !eof(skipuntil(!isspace, file, linecomment='#'))
        @test read(file, Char) == 'n'

        # test it correctly handles unicode
        for (byte, char) in zip(1:4, ('@','߷','࿊','𐋺'))
            append_to_file("abcdef$char")
            @test ncodeunits(char) == byte
            @test !eof(skipuntil(!isletter, file))
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

@testset "issue #27951" begin
    a = UInt8[1 3; 2 4]
    s = view(a, [1,2], :)
    mktemp() do path, io
        write(io, s)
        seek(io, 0)
        b = Vector{UInt8}(undef, 4)
        @test readbytes!(io, b) == 4
        @test b == 0x01:0x04
    end
end

@test Base.open_flags(read=false, write=true, append=false) == (read=false, write=true, create=true, truncate=true, append=false)

@testset "issue #30978" begin
    mktemp() do path, io
        x = rand(UInt8, 100)
        write(path, x)
        # Should not throw OutOfMemoryError
        y = open(f -> read(f, typemax(Int)), path)
        @test x == y

        # Should resize y to right length
        y = zeros(UInt8, 99)
        open(f -> readbytes!(f, y, 101, all=true), path)
        @test x == y
        y = zeros(UInt8, 99)
        open(f -> readbytes!(f, y, 101, all=false), path)
        @test x == y

        # Should never shrink y below original size
        y = zeros(UInt8, 101)
        open(f -> readbytes!(f, y, 102, all=true), path)
        @test y == [x; 0]
        y = zeros(UInt8, 101)
        open(f -> readbytes!(f, y, 102, all=false), path)
        @test y == [x; 0]
    end
end

@testset "peek(::IOStream)" begin
    mktemp() do _, file
        @test_throws EOFError peek(file)
        mark(file)
        write(file, "Lávate las manos")
        flush(file)
        reset(file)
        @test peek(file) == 0x4c
    end
end

@testset "issue #36004" begin
    f = tempname()
    open(f, "w") do io
        write(io, "test")
    end
    open(f, "r") do io
        @test length(readavailable(io)) > 0
    end
end

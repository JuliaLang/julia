# This file is a part of Julia. License is MIT: https://julialang.org/license

using Random

ioslength(io::IOBuffer) = (io.seekable ? io.size : bytesavailable(io))

bufcontents(io::Base.GenericIOBuffer) = unsafe_string(pointer(io.data), io.size)

@testset "Basic tests" begin
    @test_throws ArgumentError IOBuffer(;maxsize=-1)
    @test_throws ArgumentError IOBuffer([0x01]; maxsize=-1)

    # Test that sizehint actually will sizehint the vector,
    v = UInt8[]
    buf = IOBuffer(v; sizehint=64, write=true)
    @test length(v.ref.mem) >= 64


end

@testset "Basic reading" begin
    # Readavailable is equal to read
    buf = IOBuffer("abcdef")
    @test read(buf, UInt8) == UInt8('a')
    @test bytesavailable(buf) == 5
    @test readavailable(buf) == b"bcdef"

    # Reading less than all the bytes
    buf = IOBuffer(b"ABCDEFGHIJ")
    @test read(buf, 1) == b"A"
    @test read(buf, 3) == b"BCD"

    # Reading more bytes than available will not error
    @test read(buf, 100) == b"EFGHIJ"
end

@testset "Byte occursin GenericIOBuffer" begin
    buf = IOBuffer(@view(collect(0x1f:0x3d)[1:end]))
    @test occursin(0x1f, buf)
    @test occursin(0x3d, buf)
    @test occursin(0x2a, buf)

    @test !occursin(0xff, buf)
    @test !occursin(0x00, buf)

    v = Vector{UInt8}("bcdefg")
    pushfirst!(v, UInt8('a'))
    buf = IOBuffer(v)
    @test occursin(UInt8('a'), buf)
    read(buf, UInt8)
    @test !occursin(UInt8('a'), buf)
    @test !occursin(0x00, buf)

    buf = IOBuffer("abcdefg")
    @test occursin(UInt8('a'), buf)
end

@testset "Non-Memory backed IOBuffer" begin
    buf = IOBuffer(Test.GenericArray(collect(0x02:0x0d)), read=true)
    @test read(buf) == 0x02:0x0d

    buf = IOBuffer(Test.GenericArray(collect(0x02:0x0d)), read=true)
    @test read(buf, UInt8) == 0x02
    @test read(buf) == 0x03:0x0d

    v = view(collect(UInt8('a'):UInt8('z')), 4:10)
    buf = IOBuffer(v, read=true, write=true)
    @test read(buf, UInt8) == UInt8('d')
    @test read(buf) == UInt8('e'):UInt8('j')
    seekstart(buf)
    @test read(buf, UInt8) == UInt8('d')
    write(buf, UInt8('x'))
    write(buf, "ABC")
    seekstart(buf)
    @test read(buf) == b"dxABCij"
end

@testset "Copying" begin
    # Test offset is preserved when copying
    v = UInt8[]
    pushfirst!(v, UInt8('a'), UInt8('b'), UInt8('c'))
    buf = IOBuffer(v; write=true, read=true, append=true)
    write(buf, "def")
    read(buf, UInt16)
    buf2 = copy(buf)
    @test String(read(buf)) == "cdef"
    @test String(read(buf2)) == "cdef"

    # Test copying with non-Memory backed GenericIOBuffer
    buf = IOBuffer(Test.GenericArray(collect(0x02:0x0d)), read=true)
    @test_broken read(buf, UInt16)
    buf2 = copy(buf)
    @test isreadable(buf2)
    @test !iswritable(buf2)
    @test_broken read(buf2) == 0x04:0x0d
end

@testset "copyuntil" begin
    a = IOBuffer(b"abcdeajdgabdfg")
    b = IOBuffer(collect(b"xx"); write=true, read=true, append=true)
    copyuntil(b, a, UInt8('a'))
    @test read(b) == b"xx"
    seekstart(b)
    copyuntil(b, a, UInt8('a'); keep=true)
    @test read(b) == b"xxbcdea"
    seekstart(b)
    copyuntil(b, a, UInt('w'))
    @test read(b) == b"xxbcdeajdgabdfg"
end

@testset "copyline" begin
    a = IOBuffer(b"abcde\nabc\r\nabc\n\r\nac")
    b = IOBuffer()
    copyline(b, a)
    @test take!(copy(b)) == b"abcde"
    copyline(b, a)
    @test take!(copy(b)) == b"abcdeabc"
    copyline(b, a; keep=true)
    @test take!(copy(b)) == b"abcdeabcabc\n"
    copyline(b, a; keep=false)
    @test take!(copy(b)) == b"abcdeabcabc\n"
    copyline(b, a; keep=false)
    @test take!(copy(b)) == b"abcdeabcabc\nac"
end

@testset "take!" begin
    a = IOBuffer("abc")
    @test take!(a) == b"abc"

    v = UInt8[]
    pushfirst!(v, 0x0a)
    buf = IOBuffer(v; write=true, append=true)
    write(buf, "def")
    @test take!(buf) == b"\ndef"

    v = view(collect(b"abcdefghij"), 3:9)
    buf = IOBuffer(v; write=true, read=true)
    read(buf, UInt8)
    write(buf, "xxy")
    @test take!(buf) == b"cxxyghi"

    v = view(collect(b"abcdefghij"), 3:9)
    buf = IOBuffer(v; write=true, read=true)
end

@testset "maxsize is preserved" begin
    # After take!
    buf = IOBuffer(; maxsize=3)
    print(buf, "abcdef")
    @test take!(buf) == b"abc"
    print(buf, "abcdef")
    @test take!(buf) == b"abc"

    # After resizing
    buf = IOBuffer(;maxsize=128)
    write(buf, collect(0x00:0x10))
    write(buf, collect(0x11:0x30))
    write(buf, collect(0x31:0x98))
    write(buf, collect(0x99:0xff))
    seekstart(buf)
    @test read(buf) == 0x00:UInt8(127)
end

@testset "Read/write empty IOBuffer" begin
    io = IOBuffer()
    @test eof(io)
    @test_throws EOFError read(io, UInt8)
    @test write(io,"abc") === 3
    @test isreadable(io)
    @test iswritable(io)
    @test isopen(io)
    @test ioslength(io) == 3
    @test position(io) == 3
    @test eof(io)
    seek(io, 0)
    @test read(io, UInt8) == convert(UInt8, 'a')
    a = Vector{UInt8}(undef, 2)
    @test read!(io, a) == a
    @test a == UInt8['b','c']
    @test bufcontents(io) == "abc"
    seek(io, 1)
    truncate(io, 2)
    @test position(io) == 1
    @test !eof(io)
    seekend(io)
    @test position(io) == 2
    truncate(io, 0)
    @test position(io) == 0
    truncate(io, 10)
    @test position(io) == 0
    @test all(io.data .== 0)
    @test write(io, Int16[1, 2, 3, 4, 5, 6]) === 12
    seek(io, 2)
    truncate(io, 10)
    @test ioslength(io) == 10
    io.readable = false
    @test_throws ArgumentError read!(io, UInt8[0])
    truncate(io, 0)
    @test write(io,"boston\ncambridge\n") > 0
    @test String(take!(io)) == "boston\ncambridge\n"
    @test String(take!(io)) == ""
    @test write(io, ComplexF64(0)) === 16
    @test write(io, Rational{Int64}(1//2)) === 16
    @test closewrite(io) === nothing
    @test_throws ArgumentError write(io, UInt8[0])
    @test eof(io)
    @test close(io) === nothing
    @test_throws ArgumentError write(io, UInt8[0])
    @test_throws ArgumentError seek(io, 0)
end

@testset "Read/write readonly IOBuffer" begin
    io = IOBuffer("hamster\nguinea pig\nturtle")
    @test position(io) == 0
    @test readline(io) == "hamster"
    @test read(io, String) == "guinea pig\nturtle"
    @test_throws EOFError read(io,UInt8)
    seek(io,0)
    @test read(io,UInt8) == convert(UInt8, 'h')
    @test_throws ArgumentError truncate(io,0)
    @test_throws ArgumentError write(io,UInt8(0))
    @test_throws ArgumentError write(io,UInt8[0])
    @test String(take!(io)) == "hamster\nguinea pig\nturtle"
    @test String(take!(io)) == "hamster\nguinea pig\nturtle" #should be unchanged
    close(io)
end

@testset "PipeBuffer" begin
    io = PipeBuffer()
    @test_throws EOFError read(io,UInt8)
    @test write(io,"pancakes\nwaffles\nblueberries\n") > 0

    # PipeBuffer is append, so writing to it does not advance the position
    @test position(io) == 0
    @test readline(io) == "pancakes"
    @test readline(io) == "waffles"
    @test write(io,"whipped cream\n") > 0
    @test readline(io) == "blueberries"

    # Pipebuffers do not support seeking, and therefore do not support truncation.
    @test_throws ArgumentError seek(io,0)
    @test_throws ArgumentError truncate(io,0)

    @test readline(io) == "whipped cream"
    @test write(io,"pancakes\nwaffles\nblueberries\n") > 0
    @test readlines(io) == String["pancakes", "waffles", "blueberries"]
    write(io,"\n\r\n\n\r \n") > 0
    @test readlines(io, keep=true) == String["\n", "\r\n", "\n", "\r \n"]
    write(io,"\n\r\n\n\r \n") > 0
    @test readlines(io, keep=false) == String["", "", "", "\r "]
    @test write(io,"α\nβ\nγ\nδ") > 0
    @test readlines(io, keep=true) == String["α\n","β\n","γ\n","δ"]
    @test write(io,"α\nβ\nγ\nδ") > 0
    @test readlines(io, keep=false) == String["α", "β", "γ", "δ"]
    @test readlines(IOBuffer(""), keep=true) == []
    @test readlines(IOBuffer(""), keep=false) == []
    @test readlines(IOBuffer("first\nsecond"), keep=true) == String["first\n", "second"]
    @test readlines(IOBuffer("first\nsecond"), keep=false) == String["first", "second"]

    let fname = tempname()
        for dokeep in [true, false],
            endline in ["\n", "\r\n"],
            i in -5:5

            ref = ("1"^(2^17 - i)) * endline
            open(fname, "w") do io
                write(io, ref)
            end
            x = readlines(fname, keep = dokeep)
            if !dokeep
                ref = chomp(ref)
            end
            @test ref == x[1]
        end
        rm(fname)
    end

end

@testset "issue 5453" begin
    io = IOBuffer("abcdef")
    a = Vector{UInt8}(undef, 1024)
    @test_throws EOFError read!(io,a)
    @test eof(io)
end

@test isempty(readlines(IOBuffer(), keep=true))

@testset "issue #8193" begin
    io = IOBuffer("asdf")
    @test position(skip(io, -1)) == 0
    @test position(skip(io, 6)) == 4
    @test position(seek(io, -1)) == 0
    @test position(seek(io, 6)) == 4
end

@testset "issue #10658" begin
    io = IOBuffer("hello")
    @test position(skip(io, 4)) == 4
    @test position(skip(io, 10)) == 5
    @test position(skip(io, -2)) == 3
    @test position(skip(io, -3)) == 0
    @test position(skip(io, -3)) == 0
end

@testset "issue #53908" begin
    @testset "offset $first" for first in (false, true)
        b = collect(0x01:0x05)
        sizehint!(b, 100; first) # make offset non zero
        io = IOBuffer(b)
        @test position(skip(io, 4)) == 4
        @test position(skip(io, typemax(Int))) == 5
        @test position(skip(io, typemax(Int128))) == 5
        @test position(skip(io, typemax(Int32))) == 5
        @test position(skip(io, typemin(Int))) == 0
        @test position(skip(io, typemin(Int128))) == 0
        @test position(skip(io, typemin(Int32))) == 0
        @test position(skip(io, 4)) == 4
        @test position(skip(io, -2)) == 2
        @test position(skip(io, -2)) == 0
        @test position(seek(io, -2)) == 0
        @test position(seek(io, typemax(Int))) == 5
        @test position(seek(io, typemax(Int128))) == 5
        @test position(seek(io, typemax(Int32))) == 5
        @test position(seek(io, typemin(Int))) == 0
        @test position(seek(io, typemin(Int128))) == 0
        @test position(seek(io, typemin(Int32))) == 0
    end
end

@testset "pr #11554" begin
    io  = IOBuffer(SubString("***αhelloworldω***", 4, 16))
    io2 = IOBuffer(Vector{UInt8}(b"goodnightmoon"), read=true, write=true)

    @test read(io, Char) == 'α'
    @test_throws ArgumentError write(io,"!")
    @test_throws ArgumentError write(io,'β')
    a = Vector{UInt8}(undef, 10)
    @test read!(io, a) === a
    @test String(a) == "helloworld"
    @test read(io, Char) == 'ω'
    @test_throws EOFError read(io,UInt8)
    skip(io, -3)
    @test read(io, String) == "dω"
    @test bufcontents(io) == "αhelloworldω"
    @test_throws ArgumentError write(io,"!")
    @test take!(io) == b"αhelloworldω"
    seek(io, 2)
    seekend(io2)
    write(io2, io)
    @test read(io, String) == ""
    @test read(io2, String) == ""
    @test String(take!(io)) == "αhelloworldω"
    seek(io2, 0)
    truncate(io2, io2.size - 2)
    @test read(io2, String) == "goodnightmoonhelloworld"
    seek(io2, 0)
    write(io2, io2)
    @test read(io2, String) == ""
    @test bufcontents(io2) == "goodnightmoonhelloworld"
end

# issue #11917
# (previous tests triggered this sometimes, but this should trigger nearly all the time)
let io = IOBuffer(maxsize=0)
   write(io, fill(0x01, 1048577))
end

@testset "BufferStream" begin
    bstream = Base.BufferStream()
    @test isopen(bstream)
    @test isreadable(bstream)
    @test iswritable(bstream)
    @test bytesavailable(bstream) == 0
    @test sprint(show, bstream) == "BufferStream(bytes waiting=$(bytesavailable(bstream.buffer)), isopen=true)"
    a = rand(UInt8,10)
    write(bstream,a)
    @test !eof(bstream)
    @test flush(bstream) === nothing
    b = read(bstream,UInt8)
    @test a[1] == b
    b = read(bstream,UInt8)
    @test a[2] == b
    c = zeros(UInt8,8)
    @test bytesavailable(bstream) == 8
    @test !eof(bstream)
    @test Base.reseteof(bstream) === nothing # TODO: Actually test intended effect
    read!(bstream,c)
    @test c == a[3:10]
    @test closewrite(bstream) === nothing
    @test eof(bstream)
    @test bytesavailable(bstream) == 0
    @test close(bstream) === nothing
    flag = Ref{Bool}(false)
    event = Base.Event()
    bstream = Base.BufferStream()
    task = @async begin
        notify(event)
        read(bstream, 16)
        flag[] = true
    end
    wait(event)
    write(bstream, rand(UInt8, 16))
    wait(task)
    @test flag[] == true
end

@test flush(IOBuffer()) === nothing # should be a no-op

# pr #19461
let io = IOBuffer()
    @test Base.buffer_writes(io) === io
end

@testset "skipchars" begin
    io = IOBuffer("")
    @test eof(skipchars(isspace, io))

    io = IOBuffer("   ")
    @test eof(skipchars(isspace, io))

    io = IOBuffer("#    \n     ")
    @test eof(skipchars(isspace, io, linecomment='#'))

    io = IOBuffer("      text")
    skipchars(isspace, io)
    @test String(readavailable(io)) == "text"

    io = IOBuffer("   # comment \n    text")
    skipchars(isspace, io, linecomment='#')
    @test String(readavailable(io)) == "text"

    for char in ['@','߷','࿊','𐋺']
        io = IOBuffer("alphabeticalstuff$char")
        @test !eof(skipchars(isletter, io))
        @test read(io, Char) == char
    end
end

@testset "Test constructor with a generic type argument." begin
    io = IOBuffer(maxsize=Int16(10))
    @test io isa IOBuffer
    io = IOBuffer(maxsize=Int32(10))
    @test io isa IOBuffer
    io = IOBuffer(maxsize=Int64(10))
    @test io isa IOBuffer
end

@testset "# 25398 return value for write(::IO, ::IO)" begin
    ioa = IOBuffer()
    iob = IOBuffer("World")
    n = write(ioa, iob)
    @test String(take!(ioa)) == "World"
    @test n == 5
end

@testset "Compacting" begin
    # Compacting works
    buf = Base.GenericIOBuffer(UInt8[], true, true, false, true, 5)
    mark(buf)
    write(buf, "Hello")
    reset(buf)
    unmark(buf)
    read(buf, UInt8)
    read(buf, UInt8)
    write(buf, "a!")
    @test length(buf.data) == 5
    @test take!(buf) == b"lloa!"

    # Compacting does not do anything when mark == 0
    buf = Base.GenericIOBuffer(UInt8[], true, true, false, true, 5)
    mark(buf)
    write(buf, "Hello")
    reset(buf)
    mark(buf)
    read(buf, UInt8)
    read(buf, UInt8)
    @test write(buf, "a!") == 0
    @test take!(buf) == b"llo"
end

@testset "peek(::GenericIOBuffer)" begin
    io = Base.GenericIOBuffer(UInt8[], true, true, false, true, typemax(Int))
    write(io, "こんにちは")
    @test peek(io) == 0xe3
    @test peek(io, Char) == 'こ'
    @test peek(io, Int32) == -476872221
    close(io)
end

@testset "bytesavailable devnull" begin
    @test bytesavailable(devnull) == 0
end

@testset "#48188 read_sub for non Array AbstractArray" begin
    a = [0,0,0]
    v = @view a[1:2]
    io = IOBuffer()
    write(io,1)
    seek(io,0)
    @test Base.read_sub(io,v,1,1) == [1,0]
end

@testset "with offset" begin
    b = pushfirst!([0x02], 0x01)
    @test take!(IOBuffer(b)) == [0x01, 0x02]
end

@testset "#54636 reading from non-dense vectors" begin
    data = 0x00:0xFF
    io = IOBuffer(data)
    @test read(io) == data

    data = @view(collect(0x00:0x0f)[begin:2:end])
    io = IOBuffer(data)
    @test read(io) == data
end

@testset "Writing Char to full buffer" begin
    io = IOBuffer(;maxsize=1)
    write(io, 'a')
    @test write(io, 'a') == 0
end

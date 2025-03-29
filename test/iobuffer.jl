# This file is a part of Julia. License is MIT: https://julialang.org/license

using Random

ioslength(io::IOBuffer) = (io.seekable ? io.size : bytesavailable(io))

bufcontents(io::Base.GenericIOBuffer) = unsafe_string(pointer(io.data), io.size)

# Julia Base's internals uses the PipeBuffer, which is an unseekable IOBuffer.
# There are no public constructors to build such a buffer, but we need to test
# it anyway.
# I make a new method here such that if the implementation of Base.PipeBuffer
# changes, these tests will still work.
new_unseekable_buffer() = Base.GenericIOBuffer(Memory{UInt8}(), true, true, false, true, typemax(Int), false)

@testset "Basic tests" begin
    @test_throws ArgumentError IOBuffer(;maxsize=-1)
    @test_throws ArgumentError IOBuffer([0x01]; maxsize=-1)

    # Test that sizehint actually will sizehint the vector,
    v = UInt8[]
    buf = IOBuffer(v; sizehint=64, write=true)
    @test length(v.ref.mem) >= 64

    # Test that you can't make an IOBuffer with a maxsize
    # smaller than the size you actually give it
    @test_throws ArgumentError IOBuffer([0x01, 0x02]; maxsize=1)
    @test_throws ArgumentError IOBuffer(b"abcdefghij"; maxsize=8)
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

    # Passing truncate=false will still truncate an IOBuffer with no
    # initialized data
    @test isempty(read(IOBuffer(;sizehint=34, truncate=false)))
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
    @test read(buf, UInt16) == 0x0302
    buf2 = copy(buf)
    @test isreadable(buf2)
    @test !iswritable(buf2)
    @test read(buf2) == 0x04:0x0d

    # Test copying a non-seekable stream
    buf = new_unseekable_buffer()
    write(buf, "abcdef")
    read(buf, UInt16)
    mark(buf)
    read(buf, UInt16)
    buf2 = copy(buf)
    @test read(buf2) == b"ef"
    reset(buf2)
    @test read(buf2) == b"cdef"

    # Test copying seekable stream
    buf = IOBuffer()
    write(buf, "abcdef")
    seekstart(buf)
    read(buf)
    mark(buf)
    buf2 = copy(buf)
    @test reset(buf2) == 6
    seekstart(buf2)
    @test read(buf2) == b"abcdef"

    # Test copying a taken buffer
    buf = IOBuffer()
    write(buf, "abcdef")
    take!(buf)
    buf2 = copy(buf)
    @test eof(buf2)
    seekstart(buf2)
    @test eof(buf2)
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

    # Test a current bug in copyline
    a = Base.SecretBuffer("abcde\r\n")
    b = IOBuffer()
    write(b, "xxxxxxxxxx")
    seek(b, 2)
    copyline(b, a; keep=false)
    Base.shred!(a)
    @test take!(b) == b"xxabcdexxx"
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

    # Take on unseekable buffer does not return used bytes.
    buf = new_unseekable_buffer()
    write(buf, 0x61)
    write(buf, "bcd")
    @test read(buf, UInt8) == 0x61
    @test take!(buf) == b"bcd"

    # Compaction is reset after take!
    buf = Base.GenericIOBuffer(Memory{UInt8}(), true, true, false, true, 100, false)
    write(buf, rand(UInt8, 50))
    read(buf, 40)
    write(buf, rand(UInt8, 100))
    mark(buf)
    read(buf, 70)
    @test position(buf) == 110
    @test length(buf.data) <= 100
    v = take!(buf)
    write(buf, 0xf1)
    @test position(buf) == 0
    @test !ismarked(buf)
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

    # Edge case: When passing a Vector, does not error if the
    # underlying mem is larger than maxsize
    v = pushfirst!([0x01], 0x02)
    io = IOBuffer(v; maxsize=2)
    @test read(io) == b"\x02\x01"

    # Buffer will not write past maxsize, even if given a larger buffer
    # And also even if the data is taken and replaced
    v = sizehint!(UInt8[], 128)
    io = IOBuffer(v; write=true, read=true, maxsize=12)
    write(io, 0x01:0x0f)
    seekstart(io)
    @test read(io) == 0x01:0x0c
    @test write(io, 0x01) == 0
    @test write(io, "abc") == 0
    @test take!(io).ref.mem === v.ref.mem
    write(io, 0x01:0x0f)
    @test take!(io) == 0x01:0x0c
end

@testset "Write to self" begin
    buffer = IOBuffer()
    @test_throws ArgumentError write(buffer, buffer)

    # Write to another IOBuffer with limited size
    to = IOBuffer(;maxsize=4)
    from = IOBuffer(collect(b"abcdefghi"))
    write(to, from)
    @test String(take!(to)) == "abcd"
    @test eof(from)
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
    @test all(view(io.data, 1:10) .== 0)
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

@testset "Truncate" begin
    # Fails for non-writable and non-seekable
    @test_throws ArgumentError truncate(PipeBuffer(), 0)
    @test_throws ArgumentError truncate(IOBuffer(b"abcde"), 3)

    # Standard use
    buf = IOBuffer(collect(b"abcdef"); write=true, read=true)
    truncate(buf, 4)
    @test read(buf) == b"abcd"
    @test take!(buf) == b"abcd"

    # Mark is removed if beyond the size
    buf = IOBuffer()
    write(buf, "abcde")
    seek(buf, 4)
    mark(buf)
    truncate(buf, 4)
    @test !ismarked(buf)

    # Making it larger
    buf = IOBuffer(collect(b"abcdef"); write=true, read=true)
    seek(buf, 3)
    truncate(buf, 3)
    write(buf, 'X')
    mark(buf)
    truncate(buf, 5)
    @test ismarked(buf)
    @test reset(buf) == 4
    @test take!(buf) == b"abcX\0"

    # With offset
    v = pushfirst!(UInt8[0x62, 0x63, 0x64], 0x61)
    buf = IOBuffer(v; write=true, read=true)
    seekstart(buf)
    read(buf, UInt8)
    mark(buf)
    truncate(buf, 7)
    @test reset(buf) == 1
    @test take!(buf) == b"abcd\0\0\0"
end

@testset "Position of compactable buffer" begin
    # Set maxsize, because otherwise compaction it too hard to reason about,
    # and this test will be brittle
    io = Base.GenericIOBuffer(Memory{UInt8}(), true, true, false, true, 100, false)
    write(io, "abcd")
    read(io, UInt16)
    @test position(io) == 2
    write(io, "abcde"^80)
    @test position(io) == 2
    read(io, 60)
    @test position(io) == 62
    mark(io)
    # Trigger compaction
    write(io, rand(UInt8, 50))
    @test position(io) == 62
    v1 = read(io, 20)
    @test position(io) == 82
    @test reset(io) == 62
    @test position(io) == 62
    v2 = read(io, 20)
    @test v1 == v2
end

@testset "PipeBuffer" begin
    io = new_unseekable_buffer()
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
    buf = Base.GenericIOBuffer(UInt8[], true, true, false, true, 20, false)
    mark(buf)
    write(buf, "Hello"^5)
    reset(buf)
    unmark(buf)
    read(buf, UInt8)
    read(buf, UInt8)
    write(buf, "a!")
    @test length(buf.data) == 20
    @test String(take!(buf)) == "llo" * "Hello"^3 * "a!"

    # Compacting does not do anything when mark == 0
    buf = Base.GenericIOBuffer(UInt8[], true, true, false, true, 5, false)
    mark(buf)
    write(buf, "Hello")
    reset(buf)
    mark(buf)
    read(buf, UInt8)
    read(buf, UInt8)
    @test write(buf, "a!") == 0
    @test take!(buf) == b"llo"

    # Compacting without maxsize still works
    buf = new_unseekable_buffer()
    data = repeat(b"abcdefg", 100)
    write(buf, data)
    read(buf, 600)
    data_len = length(buf.data)
    write(buf, view(data, 1:500))
    @test length(buf.data) == data_len
end

@testset "peek(::GenericIOBuffer)" begin
    io = Base.GenericIOBuffer(UInt8[], true, true, false, true, typemax(Int), false)
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
    write(io,0)
    seek(io,0)
    @test read!(io, v) == [1, 0]
end

@testset "with offset" begin
    b = pushfirst!([0x02], 0x01)
    @test take!(IOBuffer(b)) == [0x01, 0x02]

    # Read-only buffer does not take control of underlying buffer
    v = pushfirst!([0x62, 0x63], 0x61)
    buf = IOBuffer(v; write=false)
    @test read(buf) == b"abc"
    @test v == b"abc" # v is unchanged

    # Truncate
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

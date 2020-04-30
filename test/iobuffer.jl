# This file is a part of Julia. License is MIT: https://julialang.org/license

using Random

ioslength(io::IOBuffer) = (io.seekable ? io.size : bytesavailable(io))

bufcontents(io::Base.GenericIOBuffer) = unsafe_string(pointer(io.data), io.size)

@testset "Read/write empty IOBuffer" begin
    io = IOBuffer()
    @test eof(io)
    @test_throws EOFError read(io,UInt8)
    @test write(io,"abc") === 3
    @test isreadable(io)
    @test iswritable(io)
    @test isopen(io)
    @test ioslength(io) == 3
    @test position(io) == 3
    @test eof(io)
    seek(io, 0)
    @test read(io,UInt8) == convert(UInt8, 'a')
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
    @test write(io,Int16[1,2,3,4,5,6]) === 12
    seek(io, 2)
    truncate(io, 10)
    @test ioslength(io) == 10
    io.readable = false
    @test_throws ArgumentError read!(io,UInt8[0])
    truncate(io, 0)
    @test write(io,"boston\ncambridge\n") > 0
    @test String(take!(io)) == "boston\ncambridge\n"
    @test String(take!(io)) == ""
    @test write(io, Complex{Float64}(0)) === 16
    @test write(io, Rational{Int64}(1//2)) === 16
    close(io)
    @test_throws ArgumentError write(io,UInt8[0])
    @test_throws ArgumentError seek(io,0)
    @test eof(io)
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
    @test_throws ArgumentError Base.compact(io) # not writeable
    close(io)
end

@testset "PipeBuffer" begin
    io = PipeBuffer()
    @test_throws EOFError read(io,UInt8)
    @test write(io,"pancakes\nwaffles\nblueberries\n") > 0
    @test position(io) == 0
    @test readline(io) == "pancakes"
    Base.compact(io)
    @test readline(io) == "waffles"
    @test write(io,"whipped cream\n") > 0
    @test readline(io) == "blueberries"
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

    Base.compact(io)
    @test position(io) == 0
    @test ioslength(io) == 0
    Base.ensureroom(io,50)
    @test position(io) == 0
    @test ioslength(io) == 0
    @test length(io.data) == 50
    Base.ensureroom(io,10)
    @test ioslength(io) == 0
    @test length(io.data) == 50
    io.maxsize = 75
    Base.ensureroom(io,100)
    @test ioslength(io) == 0
    @test length(io.data) == 75
    seekend(io)
    @test ioslength(io) == 0
    @test position(io) == 0
    write(io,zeros(UInt8,200))
    @test ioslength(io) == 75
    @test length(io.data) == 75
    write(io,1)
    @test ioslength(io) == 75
    @test length(io.data) == 75
    write(io,[1,2,3])
    @test ioslength(io) == 75
    @test length(io.data) == 75
    skip(io,1)
    @test write(io,UInt8(104)) === 1
    skip(io,3)
    @test write(io,b"apples") === 3
    skip(io,71)
    @test write(io,'y') === 1
    @test read(io, String) == "happy"
    @test eof(io)
    write(io,zeros(UInt8,73))
    write(io,'a')
    write(io,'b')
    write(io,'c')
    write(io,'d')
    write(io,'e')
    @test ioslength(io) == 75
    @test length(io.data) == 75
    @test position(io) == 0
    skip(io,72)
    @test String(take!(io)) == "\0ab"
    @test String(take!(io)) == ""

    # issues 4021
    print(io, true)
    close(io)
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
    @test sprint(show, bstream) == "BufferStream() bytes waiting:$(bytesavailable(bstream.buffer)), isopen:true"
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
    read!(bstream,c)
    @test c == a[3:10]
    @test close(bstream) === nothing
    @test eof(bstream)
    @test bytesavailable(bstream) == 0
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

@testset "Base.compact" begin
    a = Base.GenericIOBuffer(UInt8[], true, true, false, true, typemax(Int))
    mark(a) # mark at position 0
    write(a, "Hello!")
    @test Base.compact(a) == nothing # because pointer > mark
    close(a)
    b = Base.GenericIOBuffer(UInt8[], true, true, false, true, typemax(Int))
    write(b, "Hello!")
    read(b)
    mark(b) # mark at position 6
    write(b, "Goodbye!") # now pointer is > mark but mark is > 0
    Base.compact(b)
    @test readline(b) == "Goodbye!"
    close(b)
end

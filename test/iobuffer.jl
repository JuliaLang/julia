# This file is a part of Julia. License is MIT: http://julialang.org/license

ioslength(io::IOBuffer) = (io.seekable ? io.size : nb_available(io))

let io = IOBuffer()
@test eof(io)
@test_throws EOFError read(io,UInt8)
@test write(io,"abc") == 3
@test isreadable(io)
@test iswritable(io)
@test isopen(io)
@test ioslength(io) == 3
@test position(io) == 3
@test eof(io)
seek(io, 0)
@test read(io,UInt8) == convert(UInt8, 'a')
a = Array{UInt8}(2)
@test read!(io, a) == a
@test a == UInt8['b','c']
@test String(io) == "abc"
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
@test write(io,Int16[1,2,3,4,5,6]) == 12
seek(io, 2)
truncate(io, 10)
@test ioslength(io) == 10
io.readable = false
@test_throws ArgumentError read!(io,UInt8[0])
truncate(io, 0)
@test write(io,"boston\ncambridge\n") > 0
@test String(take!(io)) == "boston\ncambridge\n"
@test String(take!(io)) == ""
@test write(io, Complex{Float64}(0)) == 16
@test write(io, Rational{Int64}(1//2)) == 16
close(io)
@test_throws ArgumentError write(io,UInt8[0])
@test_throws ArgumentError seek(io,0)
@test eof(io)
end

let io = IOBuffer("hamster\nguinea pig\nturtle")
@test position(io) == 0
@test readline(io) == "hamster"
@test readstring(io) == "guinea pig\nturtle"
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

let io = PipeBuffer()
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
@test readlines(io, chomp=false) == String["\n", "\r\n", "\n", "\r \n"]
write(io,"\n\r\n\n\r \n") > 0
@test readlines(io, chomp=true) == String["", "", "", "\r "]
@test write(io,"α\nβ\nγ\nδ") > 0
@test readlines(io, chomp=false) == String["α\n","β\n","γ\n","δ"]
@test write(io,"α\nβ\nγ\nδ") > 0
@test readlines(io, chomp=true) == String["α", "β", "γ", "δ"]
@test readlines(IOBuffer(""), chomp=false) == []
@test readlines(IOBuffer(""), chomp=true) == []
@test readlines(IOBuffer("first\nsecond"), chomp=false) == String["first\n", "second"]
@test readlines(IOBuffer("first\nsecond"), chomp=true) == String["first", "second"]
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
@test write(io,UInt8(104)) == 1
skip(io,3)
@test write(io,b"apples") == 3
skip(io,71)
@test write(io,'y') == 1
@test readstring(io) == "happy"
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

# issue 5453
let io=IOBuffer("abcdef")
a = Array{UInt8}(1024)
@test_throws EOFError read!(io,a)
@test eof(io)
end

@test isempty(readlines(IOBuffer(), chomp=false))

# issue #8193
let io=IOBuffer("asdf")
    @test position(skip(io, -1)) == 0
    @test position(skip(io, 6)) == 4
    @test position(seek(io, -1)) == 0
    @test position(seek(io, 6)) == 4
end

# issue #10658
let io=IOBuffer("hello")
    @test position(skip(io, 4)) == 4
    @test position(skip(io, 10)) == 5
    @test position(skip(io, -2)) == 3
    @test position(skip(io, -3)) == 0
    @test position(skip(io, -3)) == 0
end

# pr #11554
let io=IOBuffer(SubString("***αhelloworldω***",4,16)), io2 = IOBuffer(b"goodnightmoon", true, true)
    @test read(io, Char) == 'α'
    @test_throws ArgumentError write(io,"!")
    @test_throws ArgumentError write(io,'β')
    a = Array{UInt8}(10)
    @test read!(io, a) === a
    @test String(a) == "helloworld"
    @test read(io, Char) == 'ω'
    @test_throws EOFError read(io,UInt8)
    skip(io, -3)
    @test readstring(io) == "dω"
    @test String(io) == "αhelloworldω"
    @test_throws ArgumentError write(io,"!")
    @test take!(io) == b"αhelloworldω"
    seek(io, 2)
    seekend(io2)
    write(io2, io)
    @test readstring(io) == ""
    @test readstring(io2) == ""
    @test String(take!(io)) == "αhelloworldω"
    seek(io2, 0)
    truncate(io2, io2.size - 2)
    @test readstring(io2) == "goodnightmoonhelloworld"
    seek(io2, 0)
    write(io2, io2)
    @test readstring(io2) == ""
    @test String(io2) == "goodnightmoonhelloworld"
end

# issue #11917
# (previous tests triggered this sometimes, but this should trigger nearly all the time)
let io = IOBuffer(0)
   write(io, ones(UInt8, 1048577))
end

let bstream = BufferStream()
    @test isopen(bstream)
    @test isreadable(bstream)
    @test iswritable(bstream)
    @test nb_available(bstream) == 0
    @test sprint(io -> show(io,bstream)) == "BufferStream() bytes waiting:$(nb_available(bstream.buffer)), isopen:true"
    a = rand(UInt8,10)
    write(bstream,a)
    @test !eof(bstream)
    @test flush(bstream) === nothing
    b = read(bstream,UInt8)
    @test a[1] == b
    b = read(bstream,UInt8)
    @test a[2] == b
    c = zeros(UInt8,8)
    @test nb_available(bstream) == 8
    @test !eof(bstream)
    read!(bstream,c)
    @test c == a[3:10]
    @test close(bstream) === nothing
    @test eof(bstream)
    @test nb_available(bstream) == 0
end

@test flush(IOBuffer()) === nothing # should be a no-op

# pr #19461
let io = IOBuffer()
    @test Base.buffer_writes(io) === io
end

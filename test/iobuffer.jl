ioslength(io::IOBuffer) = (io.seekable ? io.size : nb_available(io))

let io = IOBuffer()
@test eof(io)
@test_throws read(io,Uint8)
@test write(io,"abc") == 3
@test ioslength(io) == 3
@test position(io) == 3
@test eof(io)
seek(io, 0)
@test read(io,Uint8) == 'a'
a = Array(Uint8, 2)
@test read(io, a) == a
@test a == ['b','c']
@test bytestring(io) == "abc"
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
@test_throws read(io,Uint8[0])
truncate(io, 0)
@test write(io,"boston\ncambridge\n") > 0
@test takebuf_string(io) == "boston\ncambridge\n"
@test takebuf_string(io) == ""
close(io)
@test_throws write(io,Uint8[0])
@test_throws seek(io,0)
@test eof(io)
end

let io = IOBuffer("hamster\nguinea pig\nturtle")
@test position(io) == 0
@test readline(io) == "hamster\n"
@test readall(io) == "guinea pig\nturtle"
@test_throws read(io,Uint8)
seek(io,0)
@test read(io,Uint8) == 'h'
@test_throws truncate(io,0)
@test_throws write(io,uint8(0))
@test_throws write(io,Uint8[0])
@test takebuf_string(io) == "hamster\nguinea pig\nturtle"
@test takebuf_string(io) == "hamster\nguinea pig\nturtle" #should be unchanged
close(io)
end

let io = PipeBuffer()
@test_throws read(io,Uint8)
@test write(io,"pancakes\nwaffles\nblueberries\n") > 0
@test position(io) == 0
@test readline(io) == "pancakes\n"
Base.compact(io)
@test readline(io) == "waffles\n"
@test write(io,"whipped cream\n") > 0
@test readline(io) == "blueberries\n"
@test_throws seek(io,0)
@test_throws truncate(io,0)
@test readline(io) == "whipped cream\n"
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
write(io,zeros(Uint8,200))
@test ioslength(io) == 75
@test length(io.data) == 75
write(io,1)
@test ioslength(io) == 75
@test length(io.data) == 75
write(io,[1,2,3])
@test ioslength(io) == 75
@test length(io.data) == 75
skip(io,1)
@test write(io,uint8(104)) == 1
skip(io,3)
@test write(io,"apples".data) == 3
skip(io,71)
@test write(io,'y') == 1
@test readall(io) == "happy"
@test eof(io)
write(io,zeros(Uint8,73))
write(io,'a')
write(io,'b')
write(io,'c')
write(io,'d')
write(io,'e')
@test ioslength(io) == 75
@test length(io.data) == 75
@test position(io) == 0
skip(io,72)
@test takebuf_string(io) == "\0ab"
@test takebuf_string(io) == ""

# issues 4021
print(io, true)
close(io)
end

@test isempty(readlines(IOBuffer()))

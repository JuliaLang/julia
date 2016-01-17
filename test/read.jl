mktempdir() do dir

tasks = []

# Create test file...
filename = joinpath(dir, "file.txt")
text = "C1,C2\n1,2\na,b\n"
open(io-> write(io, text), filename, "w")

# List of IO producers...
l = Vector{Tuple{AbstractString,Function}}()


# File
io = ()->Base.Filesystem.open(filename, Base.Filesystem.JL_O_RDONLY)
s = io()
@test isa(s, IO)
@test isa(s, Base.Filesystem.File)
close(s)
push!(l, ("File", io))


# IOStream
io = ()->open(filename)
s = io()
@test isa(s, IO)
@test isa(s, IOStream)
close(s)
push!(l, ("IOStream", io))


# IOBuffer
io = ()->IOBuffer(text)
s = io()
@test isa(s, IO)
@test isa(s, IOBuffer)
close(s)
push!(l, ("IOBuffer", io))

@windows ? nothing : begin

# PipeEndpoint
socketname = joinpath(dir, "socket")
io = ()-> begin
    c = Base.Condition()
    tsk = @async begin
        con = listen(socketname)
        Base.notify(c)
        sock = accept(con)
        write(sock,text)
        close(con)
        close(sock)
    end
    push!(tasks, tsk)
    wait(c)
    connect(socketname)
end
s = io()
@test isa(s, IO)
@test isa(s, Base.PipeEndpoint)
close(s)
for tsk in tasks
    wait(tsk)
end
push!(l, ("PipeEndpoint", io))


# Pipe
io = () -> open(`echo -n $text`)[1]
s = io()
@test isa(s, IO)
@test isa(s, Pipe)
close(s)
push!(l, ("Pipe", io))

end

open_streams = []

for (name, f) in l

    io = ()->(s=f(); push!(open_streams, s); s)

    #println("$name readall...")
    @test readall(io()) == text
    @test readall(io()) == readall(filename)

    #println("$name read...")
    @test readbytes(io()) == Vector{UInt8}(text)
    @test readbytes(io()) == open(readbytes,filename)
    @test read(io(), UInt8) == read(IOBuffer(text), UInt8)
    @test read(io(), UInt8) == open(io->read(io, UInt8), filename)
    @test read(io(), Int) == read(IOBuffer(text), Int)
    @test read(io(), Int) == open(io->read(io,Int),filename)
    s1 = io()
    s2 = IOBuffer(text)
    @test read(s1, UInt32, 2) == read(s2, UInt32, 2)
    @test !eof(s1)
    @test read(s1, UInt8, 5) == read(s2, UInt8, 5)
    @test !eof(s1)
    @test read(s1, UInt8, 1) == read(s2, UInt8, 1)
    @test eof(s1)
    @test_throws EOFError read(s1, UInt8)
    @test eof(s1)
    close(s1)
    close(s2)

    #println("$name readuntil...")
    @test readuntil(io(), '\n') == open(io->readuntil(io,'\n'),filename)
    @test readuntil(io(), "\n") == open(io->readuntil(io,"\n"),filename)
    @test readuntil(io(), ',') == open(io->readuntil(io,','),filename)

    #println("$name eof...")
    n = length(text) - 1
    @test read!(io(), Vector{UInt8}(n)) ==
          read!(IOBuffer(text), Vector{UInt8}(n))
    @test (s = io(); read!(s, Vector{UInt8}(n)); !eof(s))
    n = length(text)
    @test read!(io(), Vector{UInt8}(n)) ==
          read!(IOBuffer(text), Vector{UInt8}(n))
    @test (s = io(); read!(s, Vector{UInt8}(n)); eof(s))
    n = length(text) + 1
    @test_throws EOFError read!(io(), Vector{UInt8}(n))
    @test_throws EOFError read!(io(), Vector{UInt8}(n))

    #println("$name read!...")
    for n = 1:length(text)
        @test read!(io(), Vector{UInt8}(n)) ==
              read!(IOBuffer(text), Vector{UInt8}(n))
        @test read!(io(), Vector{UInt8}(n)) ==
              open(io->read!(io, Vector{UInt8}(n)), filename)
    end
    @test_throws EOFError read!(io(), Vector{UInt8}(length(text)+1))

    #println("$name readline...")
    @test readline(io()) == readline(IOBuffer(text))
    @test readline(io()) == open(readline,filename)

    #println("$name readlines...")
    @test readlines(io()) == readlines(IOBuffer(text))
    @test readlines(io()) == open(readlines,filename)
    @test collect(eachline(io())) == collect(eachline(IOBuffer(text)))

    #println("$name countlines...")
    @test countlines(io()) == countlines(IOBuffer(text))

    #println("$name readcsv...")
    @test readcsv(io()) == readcsv(IOBuffer(text))

    if !(typeof(io()) in [Base.PipeEndpoint, Pipe])

        #println("$name position...")
        @test (s = io(); read!(s, Vector{UInt8}(4)); position(s))  == 4

        #println("$name seek...")
        for n = 0:length(text)-1
            @test readlines(seek(io(), n)) == readlines(seek(IOBuffer(text), n))
        end
        #println("$name skip...")
        for n = 0:length(text)-1
            @test readlines(seek(io(), n)) == readlines(seek(IOBuffer(text), n))
            @test readlines(skip(io(), n)) == readlines(skip(IOBuffer(text), n))
        end
        #println("$name seekend...")
        @test readall(seekend(io())) == ""
    end
end

for s in open_streams
    try close(s) end
end

for tsk in tasks
    wait(tsk)
end

end

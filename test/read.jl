# This file is a part of Julia. License is MIT: http://julialang.org/license

mktempdir() do dir

tasks = []

# Create test file...
filename = joinpath(dir, "file.txt")
text = "C1,C2\n1,2\na,b\n"

# List of IO producers...
l = Vector{Tuple{AbstractString,Function}}()


# File
io = (text) -> begin
    write(filename, text)
    Base.Filesystem.open(filename, Base.Filesystem.JL_O_RDONLY)
end
s = io(text)
@test isa(s, IO)
@test isa(s, Base.Filesystem.File)
close(s)
push!(l, ("File", io))


# IOStream
io = (text) -> begin
    write(filename, text)
    open(filename)
end
s = io(text)
@test isa(s, IO)
@test isa(s, IOStream)
close(s)
push!(l, ("IOStream", io))


# IOBuffer
io = (text)->IOBuffer(text)
s = io(text)
@test isa(s, IO)
@test isa(s, IOBuffer)
close(s)
push!(l, ("IOBuffer", io))


function run_test_server(srv, text)
    push!(tasks, @async begin
        try
            sock = accept(srv)
            try
                write(sock,text)
            catch e
                if typeof(e) != Base.UVError
                    rethrow(e)
                end
            finally
                close(sock)
            end
        finally
            close(srv)
        end
    end)
    yield()
end


# TCPSocket
io = (text) -> begin
    port, srv = listenany(rand(2000:4000))
    run_test_server(srv, text)
    connect(port)
end
s = io(text)
@test isa(s, IO)
@test isa(s, TCPSocket)
close(s)
push!(l, ("TCPSocket", io))


# PipeEndpoint
io = (text) -> begin
    a = "\\\\.\\pipe\\uv-test-$(randstring(6))"
    b = joinpath(dir, "socket-$(randstring(6))")
    socketname = @windows ? a : b
    srv = listen(socketname)
    run_test_server(srv, text)
    connect(socketname)
end
s = io(text)
@test isa(s, IO)
@test isa(s, Base.PipeEndpoint)
close(s)
push!(l, ("PipeEndpoint", io))


#FIXME See https://github.com/JuliaLang/julia/issues/14747
#      Reading from open(::Command) seems to deadlock on Linux/Travis
#=
@windows ? nothing : begin

# Windows type command not working?
# See "could not spawn `type 'C:\Users\appveyor\AppData\Local\Temp\1\jul3516.tmp\file.txt'`"
#https://ci.appveyor.com/project/StefanKarpinski/julia/build/1.0.12733/job/hpwjs4hmf03vs5ag#L1244

# Pipe
io = (text) -> begin
    write(filename, text)
    open(`$(@windows ? "type" : "cat") $filename`)[1]
#    Was open(`echo -n $text`)[1]
#    See https://github.com/JuliaLang/julia/issues/14747
end
s = io(text)
@test isa(s, IO)
@test isa(s, Pipe)
close(s)
push!(l, ("Pipe", io))

end
=#


open_streams = []
function cleanup()
    for s in open_streams
        try close(s) end
    end
    empty!(open_streams)
    for tsk in tasks
        wait(tsk)
    end
    empty!(tasks)
end


verbose = false


for (name, f) in l

    io = ()->(s=f(text); push!(open_streams, s); s)

    write(filename, text)

    verbose && println("$name read...")
    @test read(io(), UInt8) == read(IOBuffer(text), UInt8)
    @test read(io(), UInt8) == read(filename, UInt8)
    @test read(io(), Int) == read(IOBuffer(text), Int)
    @test read(io(), Int) == read(filename,Int)
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

    verbose && println("$name eof...")
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

    old_text = text
    cleanup()

    for text in [
        old_text,
        UTF8String(Char['A' + i % 52 for i in 1:(div(Base.SZ_UNBUFFERED_IO,2))]),
        UTF8String(Char['A' + i % 52 for i in 1:(    Base.SZ_UNBUFFERED_IO -1)]),
        UTF8String(Char['A' + i % 52 for i in 1:(    Base.SZ_UNBUFFERED_IO   )]),
        UTF8String(Char['A' + i % 52 for i in 1:(    Base.SZ_UNBUFFERED_IO +1)])
    ]

        write(filename, text)

        verbose && println("$name readstring...")
        @test readstring(io()) == text

        @test readstring(io()) == readstring(filename)


        verbose && println("$name read...")
        @test read(io()) == Vector{UInt8}(text)

        @test read(io()) == read(filename)

        cleanup()


        verbose && println("$name readbytes!...")
        l = length(text)
        for n = [1, 2, l-2, l-1, l, l+1, l+2]
            a1 = Vector{UInt8}(n);
            a2 = Vector{UInt8}(n)
            s1 = io()
            s2 = IOBuffer(text)
            n1 = readbytes!(s1, a1)
            n2 = readbytes!(s2, a2)
            @test n1 == n2
            @test length(a1) == length(a2)
            @test a1[1:n1] == a2[1:n2]
            @test n <= length(text) || eof(s1)
            @test n <= length(text) || eof(s2)

            cleanup()
        end

        verbose && println("$name read!...")
        l = length(text)
        for n = [1, 2, l-2, l-1, l]
            @test read!(io(), Vector{UInt8}(n)) ==
                  read!(IOBuffer(text), Vector{UInt8}(n))
            @test read!(io(), Vector{UInt8}(n)) ==
                  read!(filename, Vector{UInt8}(n))

            cleanup()
        end
        @test_throws EOFError read!(io(), Vector{UInt8}(length(text)+1))


        verbose && println("$name readuntil...")
        @test readuntil(io(), '\n') == readuntil(IOBuffer(text),'\n')
        @test readuntil(io(), '\n') == readuntil(filename,'\n')
        @test readuntil(io(), "\n") == readuntil(IOBuffer(text),"\n")
        @test readuntil(io(), "\n") == readuntil(filename,"\n")
        @test readuntil(io(), ',')  == readuntil(IOBuffer(text),',')
        @test readuntil(io(), ',')  == readuntil(filename,',')

        cleanup()

        verbose && println("$name readline...")
        @test readline(io()) == readline(IOBuffer(text))
        @test readline(io()) == readline(filename)

        verbose && println("$name readlines...")
        @test readlines(io()) == readlines(IOBuffer(text))
        @test readlines(io()) == readlines(filename)
        @test collect(eachline(io())) == collect(eachline(IOBuffer(text)))
        @test collect(eachline(io())) == collect(eachline(filename))

        cleanup()

        verbose && println("$name countlines...")
        @test countlines(io()) == countlines(IOBuffer(text))

        verbose && println("$name readcsv...")
        @test readcsv(io()) == readcsv(IOBuffer(text))
        @test readcsv(io()) == readcsv(filename)

        cleanup()
    end

    text = old_text
    write(filename, text)

    if !(typeof(io()) in [Base.PipeEndpoint, Pipe, TCPSocket])

        verbose && println("$name position...")
        @test (s = io(); read!(s, Vector{UInt8}(4)); position(s))  == 4

        verbose && println("$name seek...")
        for n = 0:length(text)-1
            @test readlines(seek(io(), n)) == readlines(seek(IOBuffer(text), n))
            cleanup()
        end
        verbose && println("$name skip...")
        for n = 0:length(text)-1
            @test readlines(seek(io(), n)) == readlines(seek(IOBuffer(text), n))
            @test readlines(skip(io(), n)) == readlines(skip(IOBuffer(text), n))
            cleanup()
        end
        verbose && println("$name seekend...")
        @test readstring(seekend(io())) == ""
    end


    verbose && println("$name write(::IOStream, ...)")
    to = open("$filename.to", "w")
    write(to, io())
    close(to)
    @test readstring("$filename.to") == text

    verbose && println("$name write(filename, ...)")
    write("$filename.to", io())
    @test readstring("$filename.to") == text

    verbose && println("$name write(::IOBuffer, ...)")
    to = IOBuffer(Vector{UInt8}(copy(text)), false, true)
    write(to, io())
    @test takebuf_string(to) == text

    cleanup()
end

end

# This file is a part of Julia. License is MIT: http://julialang.org/license

##################################
# Cross Plaform tests for spawn. #
##################################

# Assumes the following are available in the
#  - GNU coreutils (or equivalent)
#  - perl

#TODO:
# - Windows:
#   - Add a test whether coreutils are available and skip tests if not

valgrind_off = ccall(:jl_running_on_valgrind,Cint,()) == 0

yes = `perl -le 'while (1) {print STDOUT "y"}'`

#### Examples used in the manual ####

@test readall(`echo hello | sort`) == "hello | sort\n"
@test readall(pipeline(`echo hello`, `sort`)) == "hello\n"
@test length(spawn(pipeline(`echo hello`, `sort`)).processes) == 2

out = readall(`echo hello` & `echo world`)
@test search(out,"world") != (0,0)
@test search(out,"hello") != (0,0)
@test readall(pipeline(`echo hello` & `echo world`, `sort`)) == "hello\nworld\n"

@test (run(`printf "       \033[34m[stdio passthrough ok]\033[0m\n"`); true)

# Test for SIGPIPE being treated as normal termination (throws an error if broken)
@unix_only @test (run(pipeline(yes,`head`,DevNull)); true)

begin
    a = Base.Condition()
    @schedule begin
        p = spawn(pipeline(yes,DevNull))
        Base.notify(a,p)
        @test !success(p)
    end
    p = wait(a)
    kill(p)
end

if valgrind_off
    # If --trace-children=yes is passed to valgrind, valgrind will
    # exit here with an error code, and no UVError will be raised.
    @test_throws Base.UVError run(`foo_is_not_a_valid_command`)
end

if false
    prefixer(prefix, sleep) = `perl -nle '$|=1; print "'$prefix' ", $_; sleep '$sleep';'`
    @test success(pipeline(`perl -le '$|=1; for(0..2){ print; sleep 1 }'`,
                       prefixer("A",2) & prefixer("B",2)))
    @test success(pipeline(`perl -le '$|=1; for(0..2){ print; sleep 1 }'`,
                       prefixer("X",3) & prefixer("Y",3) & prefixer("Z",3),
                       prefixer("A",2) & prefixer("B",2)))
end

@test  success(`true`)
@test !success(`false`)
@test success(pipeline(`true`, `true`))
if false
    @test  success(ignorestatus(`false`))
    @test  success(pipeline(ignorestatus(`false`), `true`))
    @test !success(pipeline(ignorestatus(`false`), `false`))
    @test !success(ignorestatus(`false`) & `false`)
    @test  success(ignorestatus(pipeline(`false`, `false`)))
    @test  success(ignorestatus(`false` & `false`))
end

# STDIN Redirection
file = tempname()
run(pipeline(`echo hello world`, file))
@test readall(pipeline(file, `cat`)) == "hello world\n"
@test open(readall, pipeline(file, `cat`), "r") == "hello world\n"
rm(file)

# Stream Redirection
@unix_only begin
    r = Channel(1)
    @async begin
        port, server = listenany(2326)
        put!(r,port)
        client = accept(server)
        @test readall(pipeline(client, `cat`)) == "hello world\n"
        close(server)
    end
    @async begin
        sock = connect(fetch(r))
        run(pipeline(`echo hello world`, sock))
        close(sock)
    end
end

@test readall(setenv(`sh -c "echo \$TEST"`,["TEST=Hello World"])) == "Hello World\n"
@test readall(setenv(`sh -c "echo \$TEST"`,Dict("TEST"=>"Hello World"))) == "Hello World\n"
@test readall(setenv(`sh -c "echo \$TEST"`,"TEST"=>"Hello World")) == "Hello World\n"
@test (withenv("TEST"=>"Hello World") do
       readall(`sh -c "echo \$TEST"`); end) == "Hello World\n"
pathA = readchomp(setenv(`sh -c "pwd -P"`;dir=".."))
pathB = readchomp(setenv(`sh -c "cd .. && pwd -P"`))
@unix_only @test Base.samefile(pathA, pathB)
# on windows, sh returns posix-style paths that are not valid according to ispath
@windows_only @test pathA == pathB

# Here we test that if we close a stream with pending writes, we don't lose the writes.
str = ""
for i=1:1000
  str = "$str\n $(randstring(10))"
end
stdout, stdin, proc = readandwrite(`cat -`)
write(stdin, str)
close(stdin)
str2 = readall(stdout)
@test str2 == str

# This test hangs if the end of run walk across uv streams calls shutdown on a stream that is shutting down.
file = tempname()
open(pipeline(`cat -`, file), "w") do io
    write(io, str)
end
rm(file)

# issue #3373
# fixing up Conditions after interruptions
r = Channel(1)
t = @async begin
    try
        wait(r)
    end
    p = spawn(`sleep 1`); wait(p)
    @test p.exitcode == 0
end
yield()
schedule(t, InterruptException(), error=true)
yield()
put!(r,11)
yield()

# Test marking of AsyncStream

r = Channel(1)
@async begin
    port, server = listenany(2327)
    put!(r, port)
    client = accept(server)
    write(client, "Hello, world!\n")
    write(client, "Goodbye, world...\n")
    close(server)
end
sock = connect(fetch(r))
mark(sock)
@test ismarked(sock)
@test readline(sock) == "Hello, world!\n"
@test readline(sock) == "Goodbye, world...\n"
@test reset(sock) == 0
@test !ismarked(sock)
mark(sock)
@test ismarked(sock)
@test readline(sock) == "Hello, world!\n"
unmark(sock)
@test !ismarked(sock)
@test_throws ArgumentError reset(sock)
@test !unmark(sock)
@test readline(sock) == "Goodbye, world...\n"
#@test eof(sock) ## doesn't work...
close(sock)

# issue #4535
exename = Base.julia_cmd()
if valgrind_off
    # If --trace-children=yes is passed to valgrind, we will get a
    # valgrind banner here, not "Hello World\n".
    @test readall(pipeline(`$exename -f -e 'println(STDERR,"Hello World")'`, stderr=`cat`)) == "Hello World\n"
    out = Pipe()
    proc = spawn(pipeline(`$exename -f -e 'println(STDERR,"Hello World")'`, stderr = out))
    close(out.in)
    @test readall(out) == "Hello World\n"
    @test success(proc)
end

# issue #6310
@test readall(pipeline(`echo "2+2"`, `$exename -f`)) == "4\n"

# issue #5904
@test run(pipeline(ignorestatus(`false`), `true`)) === nothing


# issue #6010
# TODO: should create separate set of task tests
ducer = @async for i=1:100; produce(i); end
yield()
@test consume(ducer) == 1
@test consume(ducer) == 2

# redirect_*
OLD_STDOUT = STDOUT
fname = tempname()
f = open(fname,"w")
redirect_stdout(f)
println("Hello World")
redirect_stdout(OLD_STDOUT)
close(f)
@test "Hello World\n" == readall(fname)
@test is(OLD_STDOUT,STDOUT)
rm(fname)

# Test that redirecting an IOStream does not crash the process
fname = tempname()
cmd = """
# Overwrite libuv memory before freeing it, to make sure that a use after free
# triggers an assertion.
function thrash(handle::Ptr{Void})
    # Kill the memory, but write a nice low value in the libuv type field to
    # trigger the right code path
    ccall(:memset,Ptr{Void},(Ptr{Void},Cint,Csize_t),handle,0xee,3*sizeof(Ptr{Void}))
    unsafe_store!(convert(Ptr{Cint},handle+2*sizeof(Ptr{Void})),15)
    nothing
end
OLD_STDERR = STDERR
redirect_stderr(open("$(escape_string(fname))","w"))
# Usually this would be done by GC. Do it manually, to make the failure
# case more reliable.
oldhandle = OLD_STDERR.handle
OLD_STDERR.status = Base.StatusClosing
OLD_STDERR.handle = C_NULL
ccall(:uv_close,Void,(Ptr{Void},Ptr{Void}),oldhandle,cfunction(thrash,Void,(Ptr{Void},)))
sleep(1)
import Base.zzzInvalidIdentifier
"""
try
    in = open(`$exename -f`, "w")
    write(in,cmd)
    close(in)
    wait(in)
catch
    error("IOStream redirect failed. Child stderr was \n$(readall(fname))\n")
end
rm(fname)

# issue #10994: libuv can't handle strings containing NUL
let bad = "bad\0name"
    @test_throws ArgumentError run(`$bad`)
    @test_throws ArgumentError run(`echo $bad`)
    @test_throws ArgumentError run(setenv(`echo hello`, bad=>"good"))
    @test_throws ArgumentError run(setenv(`echo hello`, "good"=>bad))
end

# issue #8529
let fname = tempname()
    open(fname, "w") do f
        println(f, "test")
    end
    code = """
    for line in eachline(STDIN)
        run(pipeline(`echo asdf`,`cat`))
    end
    """
    @test success(pipeline(`cat $fname`, `$exename -e $code`))
    rm(fname)
end

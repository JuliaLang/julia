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
@test readall(pipe(`echo hello`, `sort`)) == "hello\n"
@test length(spawn(pipe(`echo hello`, `sort`)).processes) == 2

out = readall(`echo hello` & `echo world`)
@test search(out,"world") != (0,0)
@test search(out,"hello") != (0,0)
@test readall(pipe(`echo hello` & `echo world`, `sort`)) == "hello\nworld\n"

@test (run(`printf "       \033[34m[stdio passthrough ok]\033[0m\n"`); true)

# Test for SIGPIPE being treated as normal termination (throws an error if broken)
@unix_only @test (run(pipe(yes,`head`,DevNull)); true)

begin
    a = Base.Condition()
    @schedule begin
        p = spawn(pipe(yes,DevNull))
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
    @test success(pipe(`perl -le '$|=1; for(0..2){ print; sleep 1 }'`,
                       prefixer("A",2) & prefixer("B",2)))
    @test success(pipe(`perl -le '$|=1; for(0..2){ print; sleep 1 }'`,
                       prefixer("X",3) & prefixer("Y",3) & prefixer("Z",3),
                       prefixer("A",2) & prefixer("B",2)))
end

@test  success(`true`)
@test !success(`false`)
@test success(pipe(`true`, `true`))
if false
    @test  success(ignorestatus(`false`))
    @test  success(pipe(ignorestatus(`false`), `true`))
    @test !success(pipe(ignorestatus(`false`), `false`))
    @test !success(ignorestatus(`false`) & `false`)
    @test  success(ignorestatus(pipe(`false`, `false`)))
    @test  success(ignorestatus(`false` & `false`))
end

# STDIN Redirection
file = tempname()
run(pipe(`echo hello world`, file))
@test readall(pipe(file, `cat`)) == "hello world\n"
@test open(readall, pipe(file, `cat`), "r") == "hello world\n"
rm(file)

# Stream Redirection
@unix_only begin
    r = RemoteRef()
    @async begin
        port, server = listenany(2326)
        put!(r,port)
        client = accept(server)
        @test readall(pipe(client, `cat`)) == "hello world\n"
        close(server)
    end
    @async begin
        sock = connect(fetch(r))
        run(pipe(`echo hello world`, sock))
        close(sock)
    end
end

readall(setenv(`sh -c "echo \$TEST"`,["TEST=Hello World"])) == "Hello World\n"
readall(setenv(`sh -c "echo \$TEST"`,Dict("TEST"=>"Hello World"))) == "Hello World\n"
readall(setenv(`sh -c "pwd"`;dir="/")) == readall(setenv(`sh -c "cd / && pwd"`))

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
open(pipe(`cat -`, file), "w") do io
    write(io, str)
end
rm(file)

# issue #3373
# fixing up Conditions after interruptions
r = RemoteRef()
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

r = RemoteRef()
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
exename = joinpath(JULIA_HOME, Base.julia_exename())
if valgrind_off
    # If --trace-children=yes is passed to valgrind, we will get a
    # valgrind banner here, not "Hello World\n".
    @test readall(pipe(`$exename -f -e 'println(STDERR,"Hello World")'`, stderr=`cat`)) == "Hello World\n"
end

# issue #6310
@test readall(pipe(`echo "2+2"`, `$exename -f`)) == "4\n"

# issue #5904
@test run(pipe(ignorestatus(`false`), `true`)) === nothing


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

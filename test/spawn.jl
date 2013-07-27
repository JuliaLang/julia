##################################
# Cross Plaform tests for spawn. #
##################################

# Assumes the following are available in the
#  - GNU coreutils (or equivalent)
#  - perl

#TODO:
# - Windows:
#   - Add a test whether coreutils are available and skip tests if not

#### Examples used in the manual ####

@test readall(`echo hello | sort`) == "hello | sort\n"
@test readall(`echo hello` |> `sort`) == "hello\n"
@test length(spawn(`echo hello` |> `sort`).processes) == 2

out = readall(`echo hello` & `echo world`)
@test search(out,"world") != (0,0)
@test search(out,"hello") != (0,0)
@test readall((`echo hello` & `echo world`) |> `sort`)=="hello\nworld\n"

@test (run(`printf "       \033[34m[stdio passthrough ok]\033[0m\n"`); true)

# Test for SIGPIPE being treated as normal termination (throws an error if broken)
@test (run(`yes`|>`head`|>SpawnNullStream()); true)

a = Base.Condition()

@schedule begin
    p = spawn(`yes`|>SpawnNullStream())
    Base.notify(a,p)
    @test !Base.wait_success(p)
end
p = wait(a)
kill(p)

@test_throws run(`foo`)

if false
    prefixer(prefix, sleep) = `perl -nle '$|=1; print "'$prefix' ", $_; sleep '$sleep';'`
    @test success(`perl -le '$|=1; for(0..2){ print; sleep 1 }'` |>
                  prefixer("A",2) & prefixer("B",2))
    @test success(`perl -le '$|=1; for(0..2){ print; sleep 1 }'` |>
                  prefixer("X",3) & prefixer("Y",3) & prefixer("Z",3) |>
                  prefixer("A",2) & prefixer("B",2))
end

@test  success(`true`)
@test !success(`false`)
@test success(`true` |> `true`)
if false
    @test  success(ignorestatus(`false`))
    @test  success(ignorestatus(`false`) |> `true`)
    @test !success(ignorestatus(`false`) |> `false`)
    @test !success(ignorestatus(`false`) & `false`)
    @test  success(ignorestatus(`false` |> `false`))
    @test  success(ignorestatus(`false` & `false`))
end

# STDIN Redirection
file = tempname()
run(`echo hello world` |> file)
@test readall(file |> `cat`) == "hello world\n"

# Stream Redirection
@async begin
    server = listen(2326)
    client = accept(server)
    @test readall(client |> `cat`) == "hello world\n"
    close(server)
end
@async begin
    sock = connect(2326)
    run(`echo hello world` |> sock)
    close(sock)
end

readall(setenv(`sh -c 'echo \$TEST'`,["TEST=Hello World"])) == "Hello World\n"
readall(setenv(`sh -c 'echo \$TEST'`,["TEST"=>"Hello World"])) == "Hello World\n"

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
stdin, proc = writesto(`cat -` |> file)
write(stdin, str)
close(stdin)

# issue #3373
# fixing up Conditions after interruptions
r = RemoteRef()
t = @async begin
    try
        wait(r)
    end
    @test wait(spawn(`sleep 1`)) == 0
end
yield()
Base.interrupt_waiting_task(t, InterruptException())
yield()
put(r,11)
yield()

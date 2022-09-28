# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using REPL
using Random
import REPL.LineEdit
using Markdown

const BASE_TEST_PATH = joinpath(Sys.BINDIR, "..", "share", "julia", "test")
isdefined(Main, :FakePTYs) || @eval Main include(joinpath($(BASE_TEST_PATH), "testhelpers", "FakePTYs.jl"))
import .Main.FakePTYs: with_fake_pty

# For curmod_*
include(joinpath(BASE_TEST_PATH, "testenv.jl"))

include("FakeTerminals.jl")
import .FakeTerminals.FakeTerminal


function kill_timer(delay)
    # Give ourselves a generous timer here, just to prevent
    # this causing e.g. a CI hang when there's something unexpected in the output.
    # This is really messy and leaves the process in an undefined state.
    # the proper and correct way to do this in real code would be to destroy the
    # IO handles: `close(stdout_read); close(stdin_write)`
    test_task = current_task()
    function kill_test(t)
        # **DON'T COPY ME.**
        # The correct way to handle timeouts is to close the handle:
        # e.g. `close(stdout_read); close(stdin_write)`
        test_task.queue === nothing || Base.list_deletefirst!(test_task.queue, test_task)
        schedule(test_task, "hard kill repl test"; error=true)
        print(stderr, "WARNING: attempting hard kill of repl test after exceeding timeout\n")
    end
    return Timer(kill_test, delay)
end

# REPL tests
function fake_repl(@nospecialize(f); options::REPL.Options=REPL.Options(confirm_exit=false))
    # Use pipes so we can easily do blocking reads
    # In the future if we want we can add a test that the right object
    # gets displayed by intercepting the display
    input = Pipe()
    output = Pipe()
    err = Pipe()
    Base.link_pipe!(input, reader_supports_async=true, writer_supports_async=true)
    Base.link_pipe!(output, reader_supports_async=true, writer_supports_async=true)
    Base.link_pipe!(err, reader_supports_async=true, writer_supports_async=true)

    repl = REPL.LineEditREPL(FakeTerminal(input.out, output.in, err.in, options.hascolor), options.hascolor)
    repl.options = options

    hard_kill = kill_timer(900) # Your debugging session starts now. You have 15 minutes. Go.
    f(input.in, output.out, repl)
    t = @async begin
        close(input.in)
        close(output.in)
        close(err.in)
    end
    @test read(err.out, String) == ""
    #display(read(output.out, String))
    Base.wait(t)
    close(hard_kill)
    nothing
end

# Writing ^C to the repl will cause sigint, so let's not die on that
Base.exit_on_sigint(false)


# These are integration tests. If you want to unit test test e.g. completion, or
# exact LineEdit behavior, put them in the appropriate test files.
# Furthermore since we are emulating an entire terminal, there may be control characters
# in the mix. If verification needs to be done, keep it to the bare minimum. Basically
# this should make sure nothing crashes without depending on how exactly the control
# characters are being used.
fake_repl(options = REPL.Options(confirm_exit=false,hascolor=true)) do stdin_write, stdout_read, repl
    repl.specialdisplay = REPL.REPLDisplay(repl)
    repl.history_file = false

    repltask = @async begin
        REPL.run_repl(repl)
    end

    global inc = false
    global b = Condition()
    global c = Condition()
    let cmd = "\"Hello REPL\""
        write(stdin_write, "$(curmod_prefix)inc || wait($(curmod_prefix)b); r = $cmd; notify($(curmod_prefix)c); r\r")
    end
    let t = @async begin
            inc = true
            notify(b)
            wait(c)
        end
        while (d = readline(stdout_read)) != ""
            # first line [optional]: until 80th char of input
            # second line: until end of input
            # third line: "Hello REPL"
            # last line: blank
            # last+1 line: next prompt
        end
        wait(t)
    end

    write(stdin_write, '\x03')
    # Test cd feature in shell mode.
    origpwd = pwd()
    mktempdir() do tmpdir
        try
            samefile = Base.Filesystem.samefile
            tmpdir_pwd = cd(pwd, tmpdir)
            homedir_pwd = cd(pwd, homedir())
            
            write(stdin_write, ";") # consume ^C signal
            readuntil(stdout_read, "shell> ")
            cd(tmpdir)
            write(stdin_write, "cd")
            readuntil(stdin_read, "cd")
            @test samefile(".", tmpdir)
            @show stat(tmpdir)
            @show stat(".")
            @show stat(pwd())
            write(stdin_write, "\b")
        finally
            cd(origpwd)
        end
    end

    # Delete line (^U) and close REPL (^D)
    write(stdin_write, "\x15\x04")
    Base.wait(repltask)

    nothing
end

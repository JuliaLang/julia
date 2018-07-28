import REPL
include(joinpath(Sys.STDLIB, "REPL", "test", "FakeTerminals.jl"))
import .FakeTerminals.FakeTerminal

const CTRL_C = '\x03'
const UP_ARROW = "\e[A"
const DOWN_ARROW = "\e[B"

# TODO: Have a utility to generate this from a real REPL session?
precompile_script = """
2+2
print("")
@time 1+1
?reinterpret
;pwd
using Ra\t$CTRL_C
\\alpha\t$CTRL_C
\e[200~paste here ;)\e[201~"$CTRL_C
$UP_ARROW$DOWN_ARROW
123\b\b\b$CTRL_C
\b\b
f(x) = x03
f(1,2)
[][1]
cd("complet_path\t\t$CTRL_C
"""

function fake_repl(@nospecialize(f))
    input = Pipe()
    output = Pipe()
    err = Pipe()
    Base.link_pipe!(input,  reader_supports_async=true, writer_supports_async=true)
    Base.link_pipe!(output, reader_supports_async=true, writer_supports_async=true)
    Base.link_pipe!(err,    reader_supports_async=true, writer_supports_async=true)

    repl = REPL.LineEditREPL(FakeTerminal(input.out, output.in, err.in), true)

    f(input.in, output.out, repl)
    t = @async begin
        close(input.in)
        close(output.in)
        close(err.in)
    end
    #print(read(output.out, String))
    Base._wait(t)
    nothing
end

function run_repl()
    # Writing ^C to the repl will cause sigint, so let's not die on that
    ccall(:jl_exit_on_sigint, Cvoid, (Cint,), 0)

    fake_repl() do stdin_write, stdout_read, repl
        repl.specialdisplay = REPL.REPLDisplay(repl)
        repl.history_file = false

        repltask = @async begin
            REPL.run_repl(repl)
        end

        write(stdin_write, precompile_script)
        # Close REPL ^D
        write(stdin_write, '\x04')

        readavailable(stdout_read)

        Base._wait(repltask)
    end

    ccall(:jl_exit_on_sigint, Cvoid, (Cint,), 1)
end

run_repl()

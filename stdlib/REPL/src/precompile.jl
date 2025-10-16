# This file is a part of Julia. License is MIT: https://julialang.org/license

module Precompile

import ..REPL

# Ugly hack for our cache file to not have a dependency edge on the FakePTYs file.
Base._track_dependencies[] = false
try
    Base.include(@__MODULE__, joinpath(Sys.BINDIR, "..", "share", "julia", "test", "testhelpers", "FakePTYs.jl"))
    @Core.latestworld
    import .FakePTYs: open_fake_pty
finally
    Base._track_dependencies[] = true
end

function repl_workload()
    # these are intentionally triggered
    allowed_errors = [
        "BoundsError: attempt to access 0-element Vector{Any} at index [1]",
        "MethodError: no method matching f(::$Int, ::$Int)",
        "Padding of type", # reinterpret docstring has ERROR examples
    ]
    function check_errors(out)
        str = String(out)
        if occursin("ERROR:", str) && !any(occursin(e, str) for e in allowed_errors)
            @error "Unexpected error (Review REPL precompilation with debug_output on):\n$str"
            exit(1)
        end
    end
    ## Debugging options
    # View the code sent to the repl by setting this to `stdout`
    debug_output = devnull # or stdout

    CTRL_C = '\x03'
    CTRL_D = '\x04'
    CTRL_R = '\x12'
    UP_ARROW = "\e[A"
    DOWN_ARROW = "\e[B"

    # This is notified as soon as the first prompt appears
    repl_init_event = Base.Event()

    atreplinit() do repl
        # Main is closed so we can't evaluate in it, but atreplinit runs at
        # a time that repl.mistate === nothing so REPL.activate fails. So do
        # it async and wait for the first prompt to know its ready.
        t = @async begin
            wait(repl_init_event)
            REPL.activate(REPL.Precompile; interactive_utils=false)
        end
        Base.errormonitor(t)
    end

    repl_script = """
    2+2
    print("")
    printstyled("a", "b")
    display([1])
    display([1 2; 3 4])
    display("a string")
    foo(x) = 1
    @time @eval foo(1)
    ; pwd
    $CTRL_C
    $CTRL_R$CTRL_C#
    ? reinterpret
    using Ra\t$CTRL_C
    \\alpha\t$CTRL_C
    \e[200~paste here ;)\e[201~"$CTRL_C
    $UP_ARROW$DOWN_ARROW$CTRL_C
    123\b\b\b$CTRL_C
    \b\b$CTRL_C
    f(x) = x03
    f(1,2)
    [][1]
    Base.Iterators.minimum
    cd("complete_path\t\t$CTRL_C
    println("done")
    """

    JULIA_PROMPT = "julia> "
    PKG_PROMPT = "pkg> "
    SHELL_PROMPT = "shell> "
    HELP_PROMPT = "help?> "

    blackhole = Sys.isunix() ? "/dev/null" : "nul"

    withenv("JULIA_HISTORY" => blackhole,
            "JULIA_PROJECT" => nothing, # remove from environment
            "JULIA_LOAD_PATH" => "@stdlib",
            "JULIA_DEPOT_PATH" => Sys.iswindows() ? ";" : ":",
            "TERM" => "",
            "JULIA_FALLBACK_REPL" => "0" # Make sure REPL.jl is turned on
            ) do
        rawpts, ptm = open_fake_pty()
        pts = open(rawpts)::Base.TTY
        if Sys.iswindows()
            pts.ispty = false
        else
            # workaround libuv bug where it leaks pts
            Base._fd(pts) == rawpts || Base.close_stdio(rawpts)
        end
        # Prepare a background process to copy output from `ptm` until `pts` is closed
        output_copy = Base.BufferStream()
        tee = @async try
            while !eof(ptm)
                l = readavailable(ptm)
                write(debug_output, l)
                write(output_copy, l)
            end
            write(debug_output, "\n#### EOF ####\n")
        catch ex
            if !(ex isa Base.IOError && ex.code == Base.UV_EIO)
                rethrow() # ignore EIO on ptm after pts dies
            end
        finally
            close(output_copy)
            close(ptm)
        end
        Base.errormonitor(tee)
        orig_stdin = stdin
        orig_stdout = stdout
        orig_stderr = stderr
        repltask = @task try
            Base.run_std_repl(REPL, false, :yes, true)
        finally
            redirect_stdin(isopen(orig_stdin) ? orig_stdin : devnull)
            redirect_stdout(isopen(orig_stdout) ? orig_stdout : devnull)
            close(pts)
        end
        Base.errormonitor(repltask)
        try
            Base.REPL_MODULE_REF[] = REPL
            redirect_stdin(pts)
            redirect_stdout(pts)
            redirect_stderr(pts)
            try
                REPL.print_qualified_access_warning(Base.Iterators, Base, :minimum) # trigger the warning while stderr is suppressed
            finally
                redirect_stderr(isopen(orig_stderr) ? orig_stderr : devnull)
            end
            schedule(repltask)
            # wait for the definitive prompt before start writing to the TTY
            check_errors(readuntil(output_copy, JULIA_PROMPT))
            write(debug_output, "\n#### REPL STARTED ####\n")
            sleep(0.01)
            check_errors(readavailable(output_copy))
            # Input our script
            precompile_lines = split(repl_script::String, '\n'; keepempty=false)
            curr = 0
            for l in precompile_lines
                sleep(0.01) # try to let a bit of output accumulate before reading again
                curr += 1
                # consume any other output
                bytesavailable(output_copy) > 0 && check_errors(readavailable(output_copy))
                # push our input
                write(debug_output, "\n#### inputting statement: ####\n$(repr(l))\n####\n")
                # If the line ends with a CTRL_C, don't write an extra newline, which would
                # cause a second empty prompt. Our code below expects one new prompt per
                # input line and can race out of sync with the unexpected second line.
                endswith(l, CTRL_C) ? write(ptm, l) : write(ptm, l, "\n")
                check_errors(readuntil(output_copy, "\n"))
                # wait for the next prompt-like to appear
                check_errors(readuntil(output_copy, "\n"))
                strbuf = ""
                while !eof(output_copy)
                    strbuf *= String(readavailable(output_copy))
                    occursin(JULIA_PROMPT, strbuf) && break
                    occursin(PKG_PROMPT, strbuf) && break
                    occursin(SHELL_PROMPT, strbuf) && break
                    occursin(HELP_PROMPT, strbuf) && break
                    sleep(0.01) # try to let a bit of output accumulate before reading again
                end
                notify(repl_init_event)
                check_errors(strbuf)
            end
            write(debug_output, "\n#### COMPLETED - Closing REPL ####\n")
            write(ptm, "$CTRL_D")
            wait(repltask)
        finally
            redirect_stdin(isopen(orig_stdin) ? orig_stdin : devnull)
            redirect_stdout(isopen(orig_stdout) ? orig_stdout : devnull)
            close(pts)
        end
        wait(tee)
    end
    write(debug_output, "\n#### FINISHED ####\n")
    nothing
end

let
    if Base.generating_output() && Base.JLOptions().use_pkgimages != 0
        # Bare-bones PrecompileTools.jl
        # Do we need latestworld-if-toplevel here
        ccall(:jl_tag_newly_inferred_enable, Cvoid, ())
        try
            repl_workload()
            precompile(Tuple{typeof(Base.setindex!), Base.Dict{Any, Any}, Any, Int})
            precompile(Tuple{typeof(Base.delete!), Base.Set{Any}, String})
            precompile(Tuple{typeof(Base.:(==)), Char, String})
        finally
            ccall(:jl_tag_newly_inferred_disable, Cvoid, ())
        end
    end
end

end # Precompile

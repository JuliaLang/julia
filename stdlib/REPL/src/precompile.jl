# This file is a part of Julia. License is MIT: https://julialang.org/license

module Precompile

import ..REPL

# Ugly hack for our cache file to not have a dependency edge on the FakePTYs file.
Base._track_dependencies[] = false
try
    Base.include(@__MODULE__, joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia", "test", "testhelpers", "FakePTYs.jl"))
    @Core.latestworld
    import .FakePTYs: open_fake_pty
finally
    Base._track_dependencies[] = true
end

function repl_workload()
    # Capture debug output to show if something goes wrong
    debug_output = IOBuffer()

    # Errors that are intentionally triggered by the script
    allowed_errors = [
        "BoundsError: attempt to access 0-element Vector{Any} at index [1]",
        "MethodError: no method matching f(::$Int, ::$Int)",
        "Padding of type", # reinterpret docstring has ERROR examples
    ]

    function check_output()
        str = String(take!(copy(debug_output)))
        for line in eachline(IOBuffer(str))
            if occursin("ERROR:", line) && !any(e -> occursin(e, line), allowed_errors)
                println(stderr, """
                ========================================================================
                ERROR: Unexpected error during REPL precompilation
                ========================================================================
                Debug output:
                ------------------------------------------------------------------------
                """)
                println(stderr, str)
                println(stderr, "========================================================================")
                error("REPL precompilation encountered unexpected error: $line")
            end
        end
    end

    CTRL_C = '\x03'
    CTRL_D = '\x04'
    CTRL_R = '\x12'
    UP_ARROW = "\e[A"
    DOWN_ARROW = "\e[B"

    # Event that REPL notifies each time it's ready for input (autoreset so each wait blocks until next notify)
    prompt_ready = Base.Event(true)
    # Event to signal that REPL.activate has been called
    activate_done = Base.Event()

    atreplinit() do repl
        # Set the prompt_ready_event on the repl - run_frontend will copy it to mistate
        if repl isa REPL.LineEditREPL
            repl.prompt_ready_event = prompt_ready
        end
        # Start async task to wait for first prompt then activate the module
        t = @async begin
            wait(prompt_ready)
            REPL.activate(REPL.Precompile; interactive_utils=false)
            notify(activate_done)
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
    ;
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
    \x12?\x7f\e[A\e[B\t history\r
    println("done")
    """

    tmphistfile = tempname()
    write(tmphistfile, """
    # time: 2020-10-31 13:16:39 AWST
    # mode: julia
    \tcos
    # time: 2020-10-31 13:16:40 AWST
    # mode: julia
    \tsin
    # time: 2020-11-01 02:19:36 AWST
    # mode: help
    \t?
    """)

    withenv("JULIA_HISTORY" => tmphistfile,
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
        tee = @async try
            while !eof(ptm)
                l = readavailable(ptm)
                write(debug_output, l)
            end
        catch ex
            if !(ex isa Base.IOError && ex.code == Base.UV_EIO)
                rethrow() # ignore EIO on ptm after pts dies
            end
        finally
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
            # Wait for the first prompt, then for activate to complete
            wait(activate_done)
            # Send a newline to get the activated prompt
            write(ptm, "\n")
            # Wait for the new prompt to be ready
            wait(prompt_ready)

            # Input our script
            precompile_lines = split(repl_script::String, '\n'; keepempty=false)
            for l in precompile_lines
                # If the line ends with a CTRL_C, don't write an extra newline
                # CTRL_C cancels input but doesn't print a new prompt, so don't wait
                if endswith(l, CTRL_C)
                    write(ptm, l)
                    sleep(0.1)  # Brief pause to let CTRL_C be processed
                else
                    write(ptm, l, "\n")
                    # Wait for REPL to signal it's ready for next input
                    wait(prompt_ready)
                end
            end
            write(ptm, "$CTRL_D")
            wait(repltask)
        finally
            redirect_stdin(isopen(orig_stdin) ? orig_stdin : devnull)
            redirect_stdout(isopen(orig_stdout) ? orig_stdout : devnull)
            close(pts)
        end
        wait(tee)
    end
    # Check for any unexpected errors in the output
    check_output()
    rm(tmphistfile, force=true)
    nothing
end

let
    if Base.generating_output() && Base.JLOptions().use_pkgimages != 0
        # Bare-bones PrecompileTools.jl
        # Do we need latestworld-if-toplevel here
        ccall(:jl_tag_newly_inferred_enable, Cvoid, ())
        try
            repl_workload()
            precompile(Tuple{typeof(Base.setindex!), Base.Dict{Any, Any}, Any, Char})
            precompile(Tuple{typeof(Base.setindex!), Base.Dict{Any, Any}, Any, Int})
            precompile(Tuple{typeof(Base.delete!), Base.Set{Any}, String})
            precompile(Tuple{typeof(Base.:(==)), Char, String})
        finally
            ccall(:jl_tag_newly_inferred_disable, Cvoid, ())
        end
    end
end

end # Precompile

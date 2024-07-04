# This file is a part of Julia. License is MIT: https://julialang.org/license

module Precompile
# Can't use this during incremental: `@eval Module() begin``

import ..REPL
# Prepare this staging area with all the loaded packages available
for (_pkgid, _mod) in Base.loaded_modules
    if !(_pkgid.name in ("Main", "Core", "Base", "REPL"))
        eval(:(const $(Symbol(_mod)) = $_mod))
    end
end

# Ugly hack for our cache file to not have a dependency edge on the FakePTYs file.
Base._track_dependencies[] = false
try
    Base.include(@__MODULE__, joinpath(Sys.BINDIR, "..", "share", "julia", "test", "testhelpers", "FakePTYs.jl"))
    import .FakePTYs: open_fake_pty
finally
    Base._track_dependencies[] = true
end
using Base.Meta

import Markdown
import StyledStrings

## Debugging options
# Disable parallel precompiles generation by setting `false`
const PARALLEL_PRECOMPILATION = true

# View the code sent to the repl by setting this to `stdout`
const debug_output = devnull # or stdout

CTRL_C = '\x03'
CTRL_D = '\x04'
CTRL_R = '\x12'
UP_ARROW = "\e[A"
DOWN_ARROW = "\e[B"

repl_script = """
2+2
print("")
printstyled("a", "b")
display([1])
display([1 2; 3 4])
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
cd("complete_path\t\t$CTRL_C
"""

julia_exepath() = joinpath(Sys.BINDIR, Base.julia_exename())

const JULIA_PROMPT = "julia> "
const PKG_PROMPT = "pkg> "
const SHELL_PROMPT = "shell> "
const HELP_PROMPT = "help?> "

blackhole = Sys.isunix() ? "/dev/null" : "nul"
procenv = Dict{String,Any}(
        "JULIA_HISTORY" => blackhole,
        "JULIA_PROJECT" => nothing, # remove from environment
        "JULIA_LOAD_PATH" => "@stdlib",
        "JULIA_DEPOT_PATH" => Sys.iswindows() ? ";" : ":",
        "TERM" => "",
        "JULIA_FALLBACK_REPL" => "0") # Turn REPL.jl on in subprocess

generate_precompile_statements() = try
    # Extract the precompile statements from the precompile file
    statements_step = Channel{String}(Inf)

    step = @async mktemp() do precompile_file, precompile_file_h
        # Collect statements from running a REPL process and replaying our REPL script
        touch(precompile_file)
        pts, ptm = open_fake_pty()
        # we don't want existing REPL caches to be used so ignore them
        setup_cmd = """
        push!(Base.ignore_compiled_cache, Base.PkgId(Base.UUID("3fa0cd96-eef1-5676-8a61-b3b8758bbffb"), "REPL"))
        import REPL
        REPL.Terminals.is_precompiling[] = true
        """
        p = run(
                addenv(```$(julia_exepath()) -O0 --trace-compile=$precompile_file
                    --cpu-target=native --startup-file=no --compiled-modules=existing
                    --color=yes -i -e "$setup_cmd"```, procenv),
                pts, pts, pts; wait=false
            )
        Base.close_stdio(pts)
        # Prepare a background process to copy output from process until `pts` is closed
        output_copy = Base.BufferStream()
        tee = @async try
            while !eof(ptm)
                l = readavailable(ptm)
                write(debug_output, l)
                Sys.iswindows() && (sleep(0.1); yield(); yield()) # workaround hang - probably a libuv issue?
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
        repl_inputter = @async begin
            # wait for the definitive prompt before start writing to the TTY
            readuntil(output_copy, JULIA_PROMPT)
            sleep(0.1)
            readavailable(output_copy)
            # Input our script
            precompile_lines = split(repl_script::String, '\n'; keepempty=false)
            curr = 0
            for l in precompile_lines
                sleep(0.1)
                curr += 1
                # consume any other output
                bytesavailable(output_copy) > 0 && readavailable(output_copy)
                # push our input
                write(debug_output, "\n#### inputting statement: ####\n$(repr(l))\n####\n")
                # If the line ends with a CTRL_C, don't write an extra newline, which would
                # cause a second empty prompt. Our code below expects one new prompt per
                # input line and can race out of sync with the unexpected second line.
                endswith(l, CTRL_C) ? write(ptm, l) : write(ptm, l, "\n")
                readuntil(output_copy, "\n")
                # wait for the next prompt-like to appear
                readuntil(output_copy, "\n")
                strbuf = ""
                while !eof(output_copy)
                    strbuf *= String(readavailable(output_copy))
                    occursin(JULIA_PROMPT, strbuf) && break
                    occursin(PKG_PROMPT, strbuf) && break
                    occursin(SHELL_PROMPT, strbuf) && break
                    occursin(HELP_PROMPT, strbuf) && break
                    sleep(0.1)
                end
            end
            write(debug_output, "\n#### COMPLETED - Closing REPL ####\n")
            write(ptm, "$CTRL_D")
            wait(tee)
            success(p) || Base.pipeline_error(p)
            close(ptm)
            write(debug_output, "\n#### FINISHED ####\n")
        end
        Base.errormonitor(repl_inputter)

        n_step = 0
        precompile_copy = Base.BufferStream()
        buffer_reader = @async for statement in eachline(precompile_copy)
            push!(statements_step, statement)
            n_step += 1
        end

        open(precompile_file, "r") do io
            while true
                # We need to always call eof(io) for bytesavailable(io) to work
                eof(io) && istaskdone(repl_inputter) && eof(io) && break
                if bytesavailable(io) == 0
                    sleep(0.1)
                    continue
                end
                write(precompile_copy, readavailable(io))
            end
        end
        close(precompile_copy)
        wait(buffer_reader)
        return :ok
    end
    !PARALLEL_PRECOMPILATION && wait(step)
    bind(statements_step, step)

    # Make statements unique
    statements = Set{String}()
    # Execute the precompile statements
    for statement in statements_step
        # Main should be completely clean
        occursin("Main.", statement) && continue
        Base.in!(statement, statements) && continue
        try
            ps = Meta.parse(statement)
            if !isexpr(ps, :call)
                # these are typically comments
                @debug "skipping statement because it does not parse as an expression" statement
                delete!(statements, statement)
                continue
            end
            popfirst!(ps.args) # precompile(...)
            ps.head = :tuple
            # println(ps)
            ps = eval(ps)
            if !precompile(ps...)
                @warn "Failed to precompile expression" form=statement _module=nothing _file=nothing _line=0
            end
        catch ex
            # See #28808
            @warn "Failed to precompile expression" form=statement exception=ex _module=nothing _file=nothing _line=0
        end
    end

    fetch(step) == :ok || throw("Collecting precompiles failed: $(c.excp)")
    return nothing
finally
    GC.gc(true); GC.gc(false); # reduce memory footprint
end

generate_precompile_statements()

precompile(Tuple{typeof(getproperty), REPL.REPLBackend, Symbol})
precompile(Tuple{typeof(Base.take!), Base.Channel{Function}})
precompile(Tuple{typeof(Base.put!), Base.Channel{Function}, Function})
precompile(Tuple{typeof(Core.kwcall), NamedTuple{names, T} where T<:Tuple where names, typeof(REPL.LineEdit.complete_line), REPL.LineEdit.EmptyCompletionProvider, Any})

end # Precompile

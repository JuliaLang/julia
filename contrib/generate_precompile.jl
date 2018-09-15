# This file is a part of Julia. License is MIT: https://julialang.org/license

# Prevent this from being put into the Main namespace
let
M = Module()
@eval M begin
if !isdefined(Base, :uv_eventloop)
    Base.reinit_stdio()
end
Base.include(@__MODULE__, joinpath(Sys.BINDIR, "..", "share", "julia", "test", "testhelpers", "FakePTYs.jl"))
import .FakePTYs: with_fake_pty

CTRL_C = '\x03'
UP_ARROW = "\e[A"
DOWN_ARROW = "\e[B"

precompile_script = """
2+2
print("")
@time 1+1
; pwd
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
cd("complet_path\t\t$CTRL_C
"""

julia_cmd() = (julia = joinpath(Sys.BINDIR, Base.julia_exename()); `$julia`)
have_repl =  haskey(Base.loaded_modules,
                    Base.PkgId(Base.UUID("3fa0cd96-eef1-5676-8a61-b3b8758bbffb"), "REPL"))
Pkg = get(Base.loaded_modules,
          Base.PkgId(Base.UUID("44cfe95a-1eb2-52ea-b672-e2afdf69b78f"), "Pkg"),
          nothing)

if Pkg !== nothing
    precompile_script *= Pkg.precompile_script
end

function generate_precompile_statements()
    start_time = time()

    # Precompile a package
    mktempdir() do prec_path
        push!(DEPOT_PATH, prec_path)
        push!(LOAD_PATH, prec_path)
        pkgname = "__PackagePrecompilationStatementModule"
        mkpath(joinpath(prec_path, pkgname, "src"))
        write(joinpath(prec_path, pkgname, "src", "$pkgname.jl"),
              """
              module $pkgname
              end
              """)
        @eval using __PackagePrecompilationStatementModule
        empty!(LOAD_PATH)
        empty!(DEPOT_PATH)
    end

    # Create a staging area where all the loaded packages are available
    PrecompileStagingArea = Module()
    for (_pkgid, _mod) in Base.loaded_modules
        if !(_pkgid.name in ("Main", "Core", "Base"))
            eval(PrecompileStagingArea, :($(Symbol(_mod)) = $_mod))
        end
    end

    # TODO: Implement REPL replayer for Windows
    @static if !Sys.iswindows()
        print("Generating precompile statements...")
        sysimg = isempty(ARGS) ? joinpath(dirname(Sys.BINDIR), "lib", "julia", "sys.ji") : ARGS[1]

        mktemp() do precompile_file, _
            # Run a repl process and replay our script
            stdout_accumulator, stderr_accumulator = IOBuffer(), IOBuffer()
            with_fake_pty() do slave, master
                with_fake_pty() do slave_err, master_err
                    done = false
                    withenv("JULIA_HISTORY" => tempname(), "JULIA_PROJECT" => nothing,
                            "TERM" => "") do
                        p = run(`$(julia_cmd()) -O0 --trace-compile=$precompile_file --sysimage $sysimg
                                               --startup-file=no --color=yes`,
                                slave, slave, slave_err; wait=false)
                        readuntil(master, "julia>", keep=true)
                        for (tty, accumulator) in (master     => stdout_accumulator,
                                                   master_err => stderr_accumulator)
                            @async begin
                                while true
                                    done && break
                                    write(accumulator, readavailable(tty))
                                end
                            end
                        end
                        if have_repl
                            for l in split(precompile_script, '\n'; keepempty=false)
                                write(master, l, '\n')
                            end
                        end
                        write(master, "exit()\n")
                        wait(p)
                        done = true
                    end
                end
            end

            # Check what the REPL displayed
            # stdout_output = String(take!(stdout_accumulator))
            # println(stdout_output)

            # Extract the precompile statements from stderr
            statements = Set{String}()
            for statement in split(read(precompile_file, String), '\n')
                occursin("Main.", statement) && continue
                push!(statements, statement)
            end

            # Load the precompile statements
            statements_ordered = join(sort(collect(statements)), '\n')
            # println(statements_ordered)
            if have_repl
                # Seems like a reasonable number right now, adjust as needed
                @assert length(statements) > 700
            end

            Base.include_string(PrecompileStagingArea, statements_ordered)
            print(" $(length(statements)) generated in ")
            Base.time_print((time() - start_time) * 10^9)
            println()
        end
    end

    # Fall back to explicit list on Windows, might as well include them
    # for everyone though
    Base.include(PrecompileStagingArea, "precompile_explicit.jl")

    return
end

generate_precompile_statements()

end # @eval
end # let

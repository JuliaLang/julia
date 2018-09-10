# This file is a part of Julia. License is MIT: https://julialang.org/license

# Prevent this from being put into the Main namespace
let
M = Module()
@eval M begin
if !isdefined(Base, :uv_eventloop)
    Base.reinit_stdio()
end
Base.include(@__MODULE__, joinpath(Sys.BINDIR, "..", "share", "julia", "test", "testhelpers", "FakePTYs.jl"))
import .FakePTYs: open_fake_pty

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

push!(LOAD_PATH, Sys.STDLIB)
using Sockets
Sockets.__init__()
using Libdl
empty!(LOAD_PATH)

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

    print("Generating precompile statements...")
    sysimg = Base.unsafe_string(Base.JLOptions().image_file)
    mktemp() do precompile_file, _
        # Run a repl process and replay our script
        repl_output_buffer = IOBuffer()
        @static if Sys.iswindows()
            # Fake being cygwin
            pipename = """\\\\?\\pipe\\cygwin-$("0"^16)-pty10-abcdef"""
            server = listen(pipename)
            slave = connect(pipename)
            @assert ccall(:jl_ispty, Cint, (Ptr{Cvoid},), slave.handle) == 1
            master = accept(server)
        else
            slave, master = open_fake_pty()
        end
        done = false
        withenv("JULIA_HISTORY" => tempname(), "JULIA_PROJECT" => nothing,
                "TERM" => "") do
            if have_repl
                p = run(`$(julia_cmd()) -O0 --trace-compile=$precompile_file --sysimage $sysimg
                        --compile=all --startup-file=no --color=yes
                        -e 'import REPL; REPL.Terminals.is_precompiling[] = true'
                        -i`,
                        slave, slave, slave; wait=false)
                readuntil(master, "julia>", keep=true)
                t = @async begin
                    while true
                        sleep(0.5)
                        s = String(readavailable(master))
                        write(repl_output_buffer, s)
                        if occursin("__PRECOMPILE_END__", s)
                            break
                        end
                    end
                end
                if have_repl
                    for l in split(precompile_script, '\n'; keepempty=false)
                        write(master, l, '\n')
                    end
                end
                write(master, "print(\"__PRECOMPILE\", \"_END__\")", '\n')
                wait(t)

                # TODO Figure out why exit() on Windows doesn't exit the process
                if Sys.iswindows()
                    print(master, "ccall(:_exit, Cvoid, (Cint,), 0)\n")
                else
                    write(master, "exit()\n")
                    readuntil(master, "exit()\r\e[13C\r\n")
                    # @assert bytesavailable(master) == 0
                end
                wait(p)
            else
                # Is this even needed or is this already recorded just from starting this process?
                p = run(`$(julia_cmd()) -O0 --trace-compile=$precompile_file --sysimage $sysimg
                        --compile=all --startup-file=no
                        -e0`)
            end
        end
        close(master)

        # Check what the REPL displayed
        # repl_output = String(take!(repl_output_buffer))
        # println(repl_output)

        # Extract the precompile statements from stderr
        statements = Set{String}()
        for statement in split(read(precompile_file, String), '\n')
            occursin("Main.", statement) && continue
            push!(statements, statement)
        end

        if have_repl
            # Seems like a reasonable number right now, adjust as needed
            # comment out if debugging script
            @assert length(statements) > 700
        end

        # Create a staging area where all the loaded packages are available
        PrecompileStagingArea = Module()
        for (_pkgid, _mod) in Base.loaded_modules
            if !(_pkgid.name in ("Main", "Core", "Base"))
                eval(PrecompileStagingArea, :($(Symbol(_mod)) = $_mod))
            end
        end

        # Execute the collected precompile statements
        include_time = @elapsed for statement in sort(collect(statements))
            # println(statement)
            # Work around #28808
            occursin("\"YYYY-mm-dd\\THH:MM:SS\"", statement) && continue
            try
                Base.include_string(PrecompileStagingArea, statement)
            catch ex
                @error "Failed to precompile $statement"
                rethrow(ex)
            end
        end
        print(" $(length(statements)) generated in ")
        tot_time = time() - start_time
        Base.time_print(tot_time * 10^9)
        print(" (overhead "); Base.time_print((tot_time - include_time) * 10^9); println(")")
    end

    return
end

generate_precompile_statements()

end # @eval
end # let

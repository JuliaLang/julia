# This file is a part of Julia. License is MIT: https://julialang.org/license

if isempty(ARGS) || ARGS[1] !== "0"
Sys.__init_build()
# Prevent this from being put into the Main namespace
@eval Module() begin
if !isdefined(Base, :uv_eventloop)
    Base.reinit_stdio()
end
Base.include(@__MODULE__, joinpath(Sys.BINDIR, "..", "share", "julia", "test", "testhelpers", "FakePTYs.jl"))
import .FakePTYs: open_fake_pty

CTRL_C = '\x03'
UP_ARROW = "\e[A"
DOWN_ARROW = "\e[B"

hardcoded_precompile_statements = """
precompile(Tuple{typeof(Base.stale_cachefile), String, String})
precompile(Tuple{typeof(push!), Set{Module}, Module})
precompile(Tuple{typeof(push!), Set{Method}, Method})
precompile(Tuple{typeof(push!), Set{Base.PkgId}, Base.PkgId})
precompile(Tuple{typeof(setindex!), Dict{String,Base.PkgId}, Base.PkgId, String})
precompile(Tuple{typeof(get!), Type{Vector{Function}}, Dict{Base.PkgId,Vector{Function}}, Base.PkgId})
"""

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

julia_exepath() = joinpath(Sys.BINDIR, Base.julia_exename())

have_repl =  haskey(Base.loaded_modules,
                    Base.PkgId(Base.UUID("3fa0cd96-eef1-5676-8a61-b3b8758bbffb"), "REPL"))

Distributed = get(Base.loaded_modules,
          Base.PkgId(Base.UUID("8ba89e20-285c-5b6f-9357-94700520ee1b"), "Distributed"),
          nothing)
if Distributed !== nothing
    precompile_script *= """
    using Distributed
    addprocs(2)
    pmap(x->iseven(x) ? 1 : 0, 1:4)
    @distributed (+) for i = 1:100 Int(rand(Bool)) end
    """
end

Pkg = get(Base.loaded_modules,
          Base.PkgId(Base.UUID("44cfe95a-1eb2-52ea-b672-e2afdf69b78f"), "Pkg"),
          nothing)

if Pkg !== nothing
    precompile_script *= Pkg.precompile_script
end

function generate_precompile_statements()
    start_time = time_ns()
    debug_output = devnull # or stdout

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

    mktemp() do precompile_file, precompile_file_h
        # Run a repl process and replay our script
        pts, ptm = open_fake_pty()
        blackhole = Sys.isunix() ? "/dev/null" : "nul"
        if have_repl
            cmdargs = ```--color=yes
                      -e 'import REPL; REPL.Terminals.is_precompiling[] = true'
                      ```
        else
            cmdargs = `-e nothing`
        end
        p = withenv("JULIA_HISTORY" => blackhole,
                    "JULIA_PROJECT" => nothing, # remove from environment
                    "JULIA_LOAD_PATH" => Sys.iswindows() ? "@;@stdlib" : "@:@stdlib",
                    "TERM" => "") do
            sysimg = Base.unsafe_string(Base.JLOptions().image_file)
            run(```$(julia_exepath()) -O0 --trace-compile=$precompile_file --sysimage $sysimg
                   --cpu-target=native --startup-file=no --color=yes
                   -e 'import REPL; REPL.Terminals.is_precompiling[] = true'
                   -i $cmdargs```,
                   pts, pts, pts; wait=false)
        end
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
            close(output_copy)
            close(ptm)
        catch ex
            close(output_copy)
            close(ptm)
            if !(ex isa Base.IOError && ex.code == Base.UV_EIO)
                rethrow() # ignore EIO on ptm after pts dies
            end
        end
        # wait for the definitive prompt before start writing to the TTY
        readuntil(output_copy, "julia>")
        sleep(0.1)
        readavailable(output_copy)
        # Input our script
        if have_repl
            precompile_lines = split(precompile_script, '\n'; keepempty=false)
            curr = 0
            for l in precompile_lines
                sleep(0.1)
                curr += 1
                print("\rGenerating precompile statements... $curr/$(length(precompile_lines))")
                # consume any other output
                bytesavailable(output_copy) > 0 && readavailable(output_copy)
                # push our input
                write(debug_output, "\n#### inputting statement: ####\n$(repr(l))\n####\n")
                write(ptm, l, "\n")
                readuntil(output_copy, "\n")
                # wait for the next prompt-like to appear
                # NOTE: this is rather innaccurate because the Pkg REPL mode is a special flower
                readuntil(output_copy, "\n")
                readuntil(output_copy, "> ")
            end
            println()
        end
        write(ptm, "exit()\n")
        wait(tee)
        success(p) || Base.pipeline_error(p)
        close(ptm)
        write(debug_output, "\n#### FINISHED ####\n")

        # Extract the precompile statements from the precompile file
        statements = Set{String}()
        for statement in eachline(precompile_file_h)
            # Main should be completely clean
            occursin("Main.", statement) && continue
            push!(statements, statement)
        end

        for statement in split(hardcoded_precompile_statements, '\n')
            push!(statements, statement)
        end

        # Create a staging area where all the loaded packages are available
        PrecompileStagingArea = Module()
        for (_pkgid, _mod) in Base.loaded_modules
            if !(_pkgid.name in ("Main", "Core", "Base"))
                eval(PrecompileStagingArea, :(const $(Symbol(_mod)) = $_mod))
            end
        end

        # Execute the collected precompile statements
        n_succeeded = 0
        include_time = @elapsed for statement in sort(collect(statements))
            # println(statement)
            try
                Base.include_string(PrecompileStagingArea, statement)
                n_succeeded += 1
                print("\rExecuting precompile statements... $n_succeeded/$(length(statements))")
            catch
                # See #28808
                # @error "Failed to precompile $statement"
            end
        end
        println()
        if have_repl
            # Seems like a reasonable number right now, adjust as needed
            # comment out if debugging script
            @assert n_succeeded > 1500
        end

        tot_time = time_ns() - start_time
        include_time *= 1e9
        gen_time = tot_time - include_time
        println("Precompilation complete. Summary:")
        print("Total ─────── "); Base.time_print(tot_time); println()
        print("Generation ── "); Base.time_print(gen_time);     print(" "); show(IOContext(stdout, :compact=>true), gen_time / tot_time * 100); println("%")
        print("Execution ─── "); Base.time_print(include_time); print(" "); show(IOContext(stdout, :compact=>true), include_time / tot_time * 100); println("%")
    end

    return
end

generate_precompile_statements()

# As a last step in system image generation,
# remove some references to build time environment for a more reproducible build.
@eval Base PROGRAM_FILE = ""
@eval Sys begin
    BINDIR = ""
    STDLIB = ""
end
empty!(Base.ARGS)
empty!(Core.ARGS)

end # @eval
end

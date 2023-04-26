# This file is a part of Julia. License is MIT: https://julialang.org/license

if Threads.maxthreadid() != 1
    @warn "Running this file with multiple Julia threads may lead to a build error" Threads.maxthreadid()
end

if Base.isempty(Base.ARGS) || Base.ARGS[1] !== "0"
Sys.__init_build()
# Prevent this from being put into the Main namespace
@eval Module() begin
if !isdefined(Base, :uv_eventloop)
    Base.reinit_stdio()
end
Base.include(@__MODULE__, joinpath(Sys.BINDIR, "..", "share", "julia", "test", "testhelpers", "FakePTYs.jl"))
import .FakePTYs: open_fake_pty
using Base.Meta

## Debugging options
# Disable parallel precompiles generation by setting `false`
const PARALLEL_PRECOMPILATION = true

# View the code sent to the repl by setting this to `stdout`
const debug_output = devnull # or stdout

# Disable fancy printing
const fancyprint = (stdout isa Base.TTY) && Base.get_bool_env("CI", false) !== true
##

CTRL_C = '\x03'
CTRL_R = '\x12'
UP_ARROW = "\e[A"
DOWN_ARROW = "\e[B"

hardcoded_precompile_statements = """
# used by Revise.jl
precompile(Tuple{typeof(Base.parse_cache_header), String})
precompile(Base.read_dependency_src, (String, String))

# used by Requires.jl
precompile(Tuple{typeof(get!), Type{Vector{Function}}, Dict{Base.PkgId,Vector{Function}}, Base.PkgId})
precompile(Tuple{typeof(haskey), Dict{Base.PkgId,Vector{Function}}, Base.PkgId})
precompile(Tuple{typeof(delete!), Dict{Base.PkgId,Vector{Function}}, Base.PkgId})
precompile(Tuple{typeof(push!), Vector{Function}, Function})

# miscellaneous
precompile(Tuple{typeof(Base.require), Base.PkgId})
precompile(Tuple{typeof(Base.recursive_prefs_merge), Base.Dict{String, Any}})
precompile(Tuple{typeof(isassigned), Core.SimpleVector, Int})
precompile(Tuple{typeof(getindex), Core.SimpleVector, Int})
precompile(Tuple{typeof(Base.Experimental.register_error_hint), Any, Type})
precompile(Tuple{typeof(Base.display_error), Base.ExceptionStack})
precompile(Tuple{typeof(Base.close), Base.Channel, Exception})
precompile(Tuple{Core.kwftype(typeof(Type)), NamedTuple{(:sizehint,), Tuple{Int}}, Type{IOBuffer}})
precompile(Base.CoreLogging.current_logger_for_env, (Base.CoreLogging.LogLevel, String, Module))
precompile(Base.CoreLogging.current_logger_for_env, (Base.CoreLogging.LogLevel, Symbol, Module))
precompile(Base.CoreLogging.env_override_minlevel, (Symbol, Module))
precompile(Base.StackTraces.lookup, (Ptr{Nothing},))
"""

for T in (Float16, Float32, Float64), IO in (IOBuffer, IOContext{IOBuffer}, Base.TTY, IOContext{Base.TTY})
    global hardcoded_precompile_statements
    hardcoded_precompile_statements *= "precompile(Tuple{typeof(show), $IO, $T})\n"
end

repl_script = """
2+2
print("")
printstyled("a", "b")
display([1])
display([1 2; 3 4])
@time 1+1
; pwd
$CTRL_C
$CTRL_R$CTRL_C
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

precompile_script = """
# NOTE: these were moved to the end of Base.jl. TODO: move back here.
# # Used by Revise & its dependencies
# while true  # force inference
# delete!(push!(Set{Module}(), Base), Main)
# m = first(methods(+))
# delete!(push!(Set{Method}(), m), m)
# empty!(Set())
# push!(push!(Set{Union{GlobalRef,Symbol}}(), :two), GlobalRef(Base, :two))
# (setindex!(Dict{String,Base.PkgId}(), Base.PkgId(Base), "file.jl"))["file.jl"]
# (setindex!(Dict{Symbol,Vector{Int}}(), [1], :two))[:two]
# (setindex!(Dict{Base.PkgId,String}(), "file.jl", Base.PkgId(Base)))[Base.PkgId(Base)]
# (setindex!(Dict{Union{GlobalRef,Symbol}, Vector{Int}}(), [1], :two))[:two]
# (setindex!(IdDict{Type, Union{Missing, Vector{Tuple{LineNumberNode, Expr}}}}(), missing, Int))[Int]
# Dict{Symbol, Union{Nothing, Bool, Symbol}}(:one => false)[:one]
# Dict(Base => [:(1+1)])[Base]
# Dict(:one => [1])[:one]
# Dict("abc" => Set())["abc"]
# pushfirst!([], sum)
# get(Base.pkgorigins, Base.PkgId(Base), nothing)
# sort!([1,2,3])
# unique!([1,2,3])
# cumsum([1,2,3])
# append!(Int[], BitSet())
# isempty(BitSet())
# delete!(BitSet([1,2]), 3)
# deleteat!(Int32[1,2,3], [1,3])
# deleteat!(Any[1,2,3], [1,3])
# Core.svec(1, 2) == Core.svec(3, 4)
# # copy(Core.Compiler.retrieve_code_info(Core.Compiler.specialize_method(which(+, (Int, Int)), [Int, Int], Core.svec())))
# any(t->t[1].line > 1, [(LineNumberNode(2,:none),:(1+1))])
# break   # end force inference
# end
"""

julia_exepath() = joinpath(Sys.BINDIR, Base.julia_exename())

have_repl =  haskey(Base.loaded_modules,
                    Base.PkgId(Base.UUID("3fa0cd96-eef1-5676-8a61-b3b8758bbffb"), "REPL"))
if have_repl
    hardcoded_precompile_statements *= """
    precompile(Tuple{typeof(getproperty), REPL.REPLBackend, Symbol})
    """
end

Artifacts = get(Base.loaded_modules,
          Base.PkgId(Base.UUID("56f22d72-fd6d-98f1-02f0-08ddc0907c33"), "Artifacts"),
          nothing)
if Artifacts !== nothing
    precompile_script *= """
    using Artifacts, Base.BinaryPlatforms, Libdl
    artifacts_toml = abspath(joinpath(Sys.STDLIB, "Artifacts", "test", "Artifacts.toml"))
    artifact_hash("HelloWorldC", artifacts_toml)
    oldpwd = pwd(); cd(dirname(artifacts_toml))
    macroexpand(Main, :(@artifact_str("HelloWorldC")))
    cd(oldpwd)
    artifacts = Artifacts.load_artifacts_toml(artifacts_toml)
    platforms = [Artifacts.unpack_platform(e, "HelloWorldC", artifacts_toml) for e in artifacts["HelloWorldC"]]
    best_platform = select_platform(Dict(p => triplet(p) for p in platforms))
    dlopen("libjulia$(Base.isdebugbuild() ? "-debug" : "")", RTLD_LAZY | RTLD_DEEPBIND)
    """
end


Pkg = get(Base.loaded_modules,
          Base.PkgId(Base.UUID("44cfe95a-1eb2-52ea-b672-e2afdf69b78f"), "Pkg"),
          nothing)

if Pkg !== nothing
    # TODO: Split Pkg precompile script into REPL and script part
    repl_script = Pkg.precompile_script * repl_script # do larger workloads first for better parallelization
end

FileWatching = get(Base.loaded_modules,
          Base.PkgId(Base.UUID("7b1f6079-737a-58dc-b8bc-7a2ca5c1b5ee"), "FileWatching"),
          nothing)
if FileWatching !== nothing
    hardcoded_precompile_statements *= """
    precompile(Tuple{typeof(FileWatching.watch_file), String, Float64})
    precompile(Tuple{typeof(FileWatching.watch_file), String, Int})
    precompile(Tuple{typeof(FileWatching._uv_hook_close), FileWatching.FileMonitor})
    """
end

Libdl = get(Base.loaded_modules,
          Base.PkgId(Base.UUID("8f399da3-3557-5675-b5ff-fb832c97cbdb"), "Libdl"),
          nothing)
if Libdl !== nothing
    hardcoded_precompile_statements *= """
    precompile(Tuple{typeof(Libc.Libdl.dlopen), String})
    """
end


const JULIA_PROMPT = "julia> "
const PKG_PROMPT = "pkg> "
const SHELL_PROMPT = "shell> "
const HELP_PROMPT = "help?> "

# Printing the current state
let
    global print_state
    print_lk = ReentrantLock()
    status = Dict{String, String}(
        "step1" => "W",
        "step2" => "W",
        "repl" => "0/0",
        "step3" => "W",
        "clock" => "◐",
    )
    function print_status(key::String)
        txt = status[key]
        if startswith(txt, "W") # Waiting
            printstyled("? ", color=Base.warn_color()); print(txt[2:end])
        elseif startswith(txt, "R") # Running
            print(status["clock"], " ", txt[2:end])
        elseif startswith(txt, "F") # Finished
            printstyled("✓ ", color=:green); print(txt[2:end])
        else
            print(txt)
        end
    end
    function print_state(args::Pair{String,String}...)
        lock(print_lk) do
            isempty(args) || push!(status, args...)
            print("\r└ Collect (Basic: ")
            print_status("step1")
            print(", REPL ", status["repl"], ": ")
            print_status("step2")
            print(") => Execute ")
            print_status("step3")
        end
    end
end

ansi_enablecursor = "\e[?25h"
ansi_disablecursor = "\e[?25l"

generate_precompile_statements() = try # Make sure `ansi_enablecursor` is printed
    start_time = time_ns()
    sysimg = Base.unsafe_string(Base.JLOptions().image_file)

    # Extract the precompile statements from the precompile file
    statements_step1 = Channel{String}(Inf)
    statements_step2 = Channel{String}(Inf)

    # From hardcoded statements
    for statement in split(hardcoded_precompile_statements::String, '\n')
        push!(statements_step1, statement)
    end

    println("Collecting and executing precompile statements")
    fancyprint && print(ansi_disablecursor)
    print_state()
    clock = @async begin
        t = Timer(0; interval=1/10)
        anim_chars = ["◐","◓","◑","◒"]
        current = 1
        if fancyprint
            while isopen(statements_step2) || !isempty(statements_step2)
                print_state("clock" => anim_chars[current])
                wait(t)
                current = current == 4 ? 1 : current + 1
            end
        end
        close(t)
    end

    # Collect statements from running the script
    step1 = @async mktempdir() do prec_path
        print_state("step1" => "R")
        # Also precompile a package here
        pkgname = "__PackagePrecompilationStatementModule"
        mkpath(joinpath(prec_path, pkgname, "src"))
        path = joinpath(prec_path, pkgname, "src", "$pkgname.jl")
        write(path,
              """
              module $pkgname
              end
              """)
        tmp_prec = tempname(prec_path)
        tmp_proc = tempname(prec_path)
        s = """
            pushfirst!(DEPOT_PATH, $(repr(prec_path)));
            Base.PRECOMPILE_TRACE_COMPILE[] = $(repr(tmp_prec));
            Base.compilecache(Base.PkgId($(repr(pkgname))), $(repr(path)))
            $precompile_script
            """
        run(`$(julia_exepath()) -O0 --sysimage $sysimg --trace-compile=$tmp_proc --startup-file=no -Cnative -e $s`)
        n_step1 = 0
        for f in (tmp_prec, tmp_proc)
            isfile(f) || continue
            for statement in split(read(f, String), '\n')
                push!(statements_step1, statement)
                n_step1 += 1
            end
        end
        close(statements_step1)
        print_state("step1" => "F$n_step1")
        return :ok
    end
    !PARALLEL_PRECOMPILATION && wait(step1)

    step2 = @async mktemp() do precompile_file, precompile_file_h
        print_state("step2" => "R")
        # Collect statements from running a REPL process and replaying our REPL script
        touch(precompile_file)
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
                    "JULIA_PKG_PRECOMPILE_AUTO" => "0",
                    "TERM" => "") do
            run(```$(julia_exepath()) -O0 --trace-compile=$precompile_file --sysimage $sysimg
                   --cpu-target=native --startup-file=no -i $cmdargs```,
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
        catch ex
            if !(ex isa Base.IOError && ex.code == Base.UV_EIO)
                rethrow() # ignore EIO on ptm after pts dies
            end
        finally
            close(output_copy)
            close(ptm)
        end
        repl_inputter = @async begin
            # wait for the definitive prompt before start writing to the TTY
            readuntil(output_copy, JULIA_PROMPT)
            sleep(0.1)
            readavailable(output_copy)
            # Input our script
            if have_repl
                precompile_lines = split(repl_script::String, '\n'; keepempty=false)
                curr = 0
                for l in precompile_lines
                    sleep(0.1)
                    curr += 1
                    print_state("repl" => "$curr/$(length(precompile_lines))")
                    # consume any other output
                    bytesavailable(output_copy) > 0 && readavailable(output_copy)
                    # push our input
                    write(debug_output, "\n#### inputting statement: ####\n$(repr(l))\n####\n")
                    write(ptm, l, "\n")
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
            end
            write(ptm, "exit()\n")
            wait(tee)
            success(p) || Base.pipeline_error(p)
            close(ptm)
            write(debug_output, "\n#### FINISHED ####\n")
        end

        n_step2 = 0
        precompile_copy = Base.BufferStream()
        buffer_reader = @async for statement in eachline(precompile_copy)
            print_state("step2" => "R$n_step2")
            push!(statements_step2, statement)
            n_step2 += 1
        end

        open(precompile_file, "r") do io
            while true
                # We need to allways call eof(io) for bytesavailable(io) to work
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
        close(statements_step2)
        print_state("step2" => "F$n_step2")
        return :ok
    end
    !PARALLEL_PRECOMPILATION && wait(step2)

    # Create a staging area where all the loaded packages are available
    PrecompileStagingArea = Module()
    for (_pkgid, _mod) in Base.loaded_modules
        if !(_pkgid.name in ("Main", "Core", "Base"))
            eval(PrecompileStagingArea, :(const $(Symbol(_mod)) = $_mod))
        end
    end

    n_succeeded = 0
    # Make statements unique
    statements = Set{String}()
    # Execute the precompile statements
    for sts in [statements_step1, statements_step2], statement in sts
        # Main should be completely clean
        occursin("Main.", statement) && continue
        Base.in!(statement, statements) && continue
        # println(statement)
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
            ps = Core.eval(PrecompileStagingArea, ps)
            precompile(ps...)
            n_succeeded += 1
            failed = length(statements) - n_succeeded
            yield() # Make clock spinning
            print_state("step3" => string("R$n_succeeded", failed > 0 ? " ($failed failed)" : ""))
        catch ex
            # See #28808
            @warn "Failed to precompile expression" form=statement exception=ex _module=nothing _file=nothing _line=0
        end
    end
    wait(clock) # Stop asynchronous printing
    failed = length(statements) - n_succeeded
    print_state("step3" => string("F$n_succeeded", failed > 0 ? " ($failed failed)" : ""))
    println()
    if have_repl
        # Seems like a reasonable number right now, adjust as needed
        # comment out if debugging script
        n_succeeded > 1500 || @warn "Only $n_succeeded precompile statements"
    end

    fetch(step1) == :ok || throw("Step 1 of collecting precompiles failed.")
    fetch(step2) == :ok || throw("Step 2 of collecting precompiles failed.")

    tot_time = time_ns() - start_time
    println("Precompilation complete. Summary:")
    print("Total ─────── "); Base.time_print(tot_time);     println()
finally
    fancyprint && print(ansi_enablecursor)
    return
end

generate_precompile_statements()

# As a last step in system image generation,
# remove some references to build time environment for a more reproducible build.
Base.Filesystem.temp_cleanup_purge(force=true)
@eval Base PROGRAM_FILE = ""
@eval Sys begin
    BINDIR = ""
    STDLIB = ""
end
empty!(Base.ARGS)
empty!(Core.ARGS)

end # @eval
end # if

println("Outputting sysimage file...")
let pre_output_time = time_ns()
    # Print report after sysimage has been saved so all time spent can be captured
    Base.postoutput() do
        output_time = time_ns() - pre_output_time
        print("Output ────── "); Base.time_print(output_time); println()
    end
end

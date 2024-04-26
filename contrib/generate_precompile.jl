# This file is a part of Julia. License is MIT: https://julialang.org/license

# Prevent this from putting anything into the Main namespace
@eval Core.Module() begin

if Threads.maxthreadid() != 1
    @warn "Running this file with multiple Julia threads may lead to a build error" Threads.maxthreadid()
end

if Base.isempty(Base.ARGS) || Base.ARGS[1] !== "0"
Sys.__init_build()
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
precompile(Base.unsafe_string, (Ptr{UInt8},))
precompile(Base.unsafe_string, (Ptr{Int8},))

# loading.jl
precompile(Base.__require_prelocked, (Base.PkgId, Nothing))
precompile(Base._require, (Base.PkgId, Nothing))
precompile(Base.indexed_iterate, (Pair{Symbol, Union{Nothing, String}}, Int))
precompile(Base.indexed_iterate, (Pair{Symbol, Union{Nothing, String}}, Int, Int))

# Pkg loading
precompile(Tuple{typeof(Base.Filesystem.normpath), String, String, Vararg{String}})
precompile(Tuple{typeof(Base.append!), Array{String, 1}, Array{String, 1}})
precompile(Tuple{typeof(Base.join), Array{String, 1}, Char})
precompile(Tuple{typeof(Base.getindex), Base.Dict{Any, Any}, Char})
precompile(Tuple{typeof(Base.delete!), Base.Set{Any}, Char})
precompile(Tuple{typeof(Base.convert), Type{Base.Dict{String, Base.Dict{String, String}}}, Base.Dict{String, Any}})
precompile(Tuple{typeof(Base.convert), Type{Base.Dict{String, Array{String, 1}}}, Base.Dict{String, Any}})

# REPL
precompile(isequal, (String, String))
precompile(Base.check_open, (Base.TTY,))
precompile(Base.getproperty, (Base.TTY, Symbol))
precompile(write, (Base.TTY, String))
precompile(Tuple{typeof(Base.get), Base.TTY, Symbol, Bool})
precompile(Tuple{typeof(Base.hashindex), String, Int})
precompile(Tuple{typeof(Base.write), Base.GenericIOBuffer{Array{UInt8, 1}}, String})
precompile(Tuple{typeof(Base.indexed_iterate), Tuple{Nothing, Int}, Int})
precompile(Tuple{typeof(Base.indexed_iterate), Tuple{Nothing, Int}, Int, Int})
precompile(Tuple{typeof(Base._typeddict), Base.Dict{String, Any}, Base.Dict{String, Any}, Vararg{Base.Dict{String, Any}}})
precompile(Tuple{typeof(Base.promoteK), Type, Base.Dict{String, Any}, Base.Dict{String, Any}})
precompile(Tuple{typeof(Base.promoteK), Type, Base.Dict{String, Any}})
precompile(Tuple{typeof(Base.promoteV), Type, Base.Dict{String, Any}, Base.Dict{String, Any}})
precompile(Tuple{typeof(Base.eval_user_input), Base.PipeEndpoint, Any, Bool})
precompile(Tuple{typeof(Base.get), Base.PipeEndpoint, Symbol, Bool})

# used by Revise.jl
precompile(Tuple{typeof(Base.parse_cache_header), String})
precompile(Base.read_dependency_src, (String, String))

# used by Requires.jl
precompile(Tuple{typeof(get!), Type{Vector{Function}}, Dict{Base.PkgId,Vector{Function}}, Base.PkgId})
precompile(Tuple{typeof(haskey), Dict{Base.PkgId,Vector{Function}}, Base.PkgId})
precompile(Tuple{typeof(delete!), Dict{Base.PkgId,Vector{Function}}, Base.PkgId})
precompile(Tuple{typeof(push!), Vector{Function}, Function})

# preferences
precompile(Base.get_preferences, (Base.UUID,))
precompile(Base.record_compiletime_preference, (Base.UUID, String))

# miscellaneous
precompile(Tuple{typeof(Base.exit)})
precompile(Tuple{typeof(Base.require), Base.PkgId})
precompile(Tuple{typeof(Base.recursive_prefs_merge), Base.Dict{String, Any}})
precompile(Tuple{typeof(Base.recursive_prefs_merge), Base.Dict{String, Any}, Base.Dict{String, Any}, Vararg{Base.Dict{String, Any}}})
precompile(Tuple{typeof(Base.hashindex), Tuple{Base.PkgId, Nothing}, Int})
precompile(Tuple{typeof(Base.hashindex), Tuple{Base.PkgId, String}, Int})
precompile(Tuple{typeof(isassigned), Core.SimpleVector, Int})
precompile(Tuple{typeof(getindex), Core.SimpleVector, Int})
precompile(Tuple{typeof(Base.Experimental.register_error_hint), Any, Type})
precompile(Tuple{typeof(Base.display_error), Base.ExceptionStack})
precompile(Tuple{Core.kwftype(typeof(Type)), NamedTuple{(:sizehint,), Tuple{Int}}, Type{IOBuffer}})
precompile(Base.CoreLogging.current_logger_for_env, (Base.CoreLogging.LogLevel, String, Module))
precompile(Base.CoreLogging.current_logger_for_env, (Base.CoreLogging.LogLevel, Symbol, Module))
precompile(Base.CoreLogging.env_override_minlevel, (Symbol, Module))
precompile(Base.StackTraces.lookup, (Ptr{Nothing},))
precompile(Tuple{typeof(Base.run_module_init), Module, Int})

# precompilepkgs
precompile(Tuple{typeof(Base.get), Type{Array{String, 1}}, Base.Dict{String, Any}, String})
precompile(Tuple{typeof(Base.get), Type{Base.Dict{String, Any}}, Base.Dict{String, Any}, String})
precompile(Tuple{typeof(Base.haskey), Base.Dict{String, Any}, String})
precompile(Tuple{typeof(Base.indexed_iterate), Tuple{Base.TTY, Bool}, Int, Int})
precompile(Tuple{typeof(Base.indexed_iterate), Tuple{Base.TTY, Bool}, Int})
precompile(Tuple{typeof(Base.open), Base.CmdRedirect, String, Base.TTY})
precompile(Tuple{typeof(Base.Precompilation.precompilepkgs)})
precompile(Tuple{typeof(Base.Precompilation.printpkgstyle), Base.TTY, Symbol, String})
precompile(Tuple{typeof(Base.rawhandle), Base.TTY})
precompile(Tuple{typeof(Base.setindex!), Base.Dict{String, Array{String, 1}}, Array{String, 1}, String})
precompile(Tuple{typeof(Base.setindex!), GenericMemory{:not_atomic, Union{Base.Libc.RawFD, Base.SyncCloseFD, IO}, Core.AddrSpace{Core}(0x00)}, Base.TTY, Int})
precompile(Tuple{typeof(Base.setup_stdio), Base.TTY, Bool})
precompile(Tuple{typeof(Base.spawn_opts_inherit), Base.DevNull, Base.TTY, Base.TTY})
precompile(Tuple{typeof(Core.kwcall), NamedTuple{(:context,), Tuple{Base.TTY}}, typeof(Base.sprint), Function})
precompile(Tuple{Type{Base.UUID}, Base.UUID})
"""

for T in (Float16, Float32, Float64), IO in (IOBuffer, IOContext{IOBuffer}, Base.TTY, IOContext{Base.TTY})
    global hardcoded_precompile_statements
    hardcoded_precompile_statements *= "precompile(Tuple{typeof(show), $IO, $T})\n"
end

# Precompiles for Revise and other packages
precompile_script = """
for match = Base._methods(+, (Int, Int), -1, Base.get_world_counter())
    m = match.method
    delete!(push!(Set{Method}(), m), m)
    copy(Core.Compiler.retrieve_code_info(Core.Compiler.specialize_method(match), typemax(UInt)))

    empty!(Set())
    push!(push!(Set{Union{GlobalRef,Symbol}}(), :two), GlobalRef(Base, :two))
    (setindex!(Dict{String,Base.PkgId}(), Base.PkgId(Base), "file.jl"))["file.jl"]
    (setindex!(Dict{Symbol,Vector{Int}}(), [1], :two))[:two]
    (setindex!(Dict{Base.PkgId,String}(), "file.jl", Base.PkgId(Base)))[Base.PkgId(Base)]
    (setindex!(Dict{Union{GlobalRef,Symbol}, Vector{Int}}(), [1], :two))[:two]
    (setindex!(IdDict{Type, Union{Missing, Vector{Tuple{LineNumberNode, Expr}}}}(), missing, Int))[Int]
    Dict{Symbol, Union{Nothing, Bool, Symbol}}(:one => false)[:one]
    Dict(Base => [:(1+1)])[Base]
    Dict(:one => [1])[:one]
    Dict("abc" => Set())["abc"]
    pushfirst!([], sum)
    get(Base.pkgorigins, Base.PkgId(Base), nothing)
    sort!([1,2,3])
    unique!([1,2,3])
    cumsum([1,2,3])
    append!(Int[], BitSet())
    isempty(BitSet())
    delete!(BitSet([1,2]), 3)
    deleteat!(Int32[1,2,3], [1,3])
    deleteat!(Any[1,2,3], [1,3])
    Core.svec(1, 2) == Core.svec(3, 4)
    any(t->t[1].line > 1, [(LineNumberNode(2,:none), :(1+1))])

    # Code loading uses this
    sortperm(mtime.(readdir(".")), rev=true)
    # JLLWrappers uses these
    Dict{Base.UUID,Set{String}}()[Base.UUID("692b3bcd-3c85-4b1f-b108-f13ce0eb3210")] = Set{String}()
    get!(Set{String}, Dict{Base.UUID,Set{String}}(), Base.UUID("692b3bcd-3c85-4b1f-b108-f13ce0eb3210"))
    eachindex(IndexLinear(), Expr[])
    push!(Expr[], Expr(:return, false))
    vcat(String[], String[])
    k, v = (:hello => nothing)

    # Preferences uses these
    get(Dict{String,Any}(), "missing", nothing)
    delete!(Dict{String,Any}(), "missing")
    for (k, v) in Dict{String,Any}()
        println(k)
    end

    # interactive startup uses this
    write(IOBuffer(), "")

    break   # only actually need to do this once
end
"""

julia_exepath() = joinpath(Sys.BINDIR, Base.julia_exename())

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

# Printing the current state
let
    global print_state
    print_lk = ReentrantLock()
    status = Dict{String, String}(
        "step1" => "W",
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
            print(") => Execute ")
            print_status("step3")
        end
    end
end

ansi_enablecursor = "\e[?25h"
ansi_disablecursor = "\e[?25l"
blackhole = Sys.isunix() ? "/dev/null" : "nul"
procenv = Dict{String,Any}(
        "JULIA_HISTORY" => blackhole,
        "JULIA_LOAD_PATH" => "@$(Sys.iswindows() ? ";" : ":")@stdlib",
        "JULIA_DEPOT_PATH" => Sys.iswindows() ? ";" : ":",
        "TERM" => "",
        # "JULIA_DEBUG" => "precompilation",
        "JULIA_FALLBACK_REPL" => "true")

generate_precompile_statements() = try # Make sure `ansi_enablecursor` is printed
    start_time = time_ns()
    sysimg = Base.unsafe_string(Base.JLOptions().image_file)

    # Extract the precompile statements from the precompile file
    statements_step1 = Channel{String}(Inf)

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
            while isopen(statements_step1) || !isempty(statements_step1)
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
        pkguuid = "824efdaf-a0e9-431c-8ee7-3d356b2531c2"
        pkgpath = joinpath(prec_path, pkgname)
        mkpath(joinpath(pkgpath, "src"))
        write(joinpath(pkgpath, "src", "$pkgname.jl"),
            """
            module $pkgname
            println("Precompiling $pkgname")
            end
            """)
        write(joinpath(pkgpath, "Project.toml"),
            """
            name = "$pkgname"
            uuid = "$pkguuid"
            """)
        touch(joinpath(pkgpath, "Manifest.toml"))
        tmp_prec = tempname(prec_path)
        tmp_proc = tempname(prec_path)
        s = """
            pushfirst!(DEPOT_PATH, $(repr(joinpath(prec_path,"depot"))));
            Base.PRECOMPILE_TRACE_COMPILE[] = $(repr(tmp_prec));
            Base.Precompilation.precompilepkgs(;fancyprint=true);
            $precompile_script
            """
        p = run(pipeline(addenv(`$(julia_exepath()) -O0 --trace-compile=$tmp_proc --sysimage $sysimg
                --cpu-target=native --startup-file=no --color=yes --project=$(pkgpath)`, procenv),
                 stdin=IOBuffer(s), stderr=debug_output, stdout=debug_output))
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
    Base.errormonitor(step1)
    !PARALLEL_PRECOMPILATION && wait(step1)

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
    for sts in [statements_step1,], statement in sts
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
            if precompile(ps...)
                n_succeeded += 1
            else
                @warn "Failed to precompile expression" form=statement _module=nothing _file=nothing _line=0
            end
            failed = length(statements) - n_succeeded
            yield() # Make clock spinning
            print_state("step3" => string("R$n_succeeded", failed > 0 ? " ($failed failed)" : ""))
        catch ex
            # See #28808
            @warn "Failed to precompile expression" form=statement exception=(ex,catch_backtrace()) _module=nothing _file=nothing _line=0
        end
    end
    wait(clock) # Stop asynchronous printing
    failed = length(statements) - n_succeeded
    print_state("step3" => string("F$n_succeeded", failed > 0 ? " ($failed failed)" : ""))
    println()
    # Seems like a reasonable number right now, adjust as needed
    # comment out if debugging script
    n_succeeded > (have_repl ? 650 : 90) || @warn "Only $n_succeeded precompile statements"

    fetch(step1) == :ok || throw("Step 1 of collecting precompiles failed.")

    tot_time = time_ns() - start_time
    println("Precompilation complete. Summary:")
    print("Total ─────── "); Base.time_print(stdout, tot_time); println()
finally
    fancyprint && print(ansi_enablecursor)
    GC.gc(true); GC.gc(false); # reduce memory footprint
    return
end

generate_precompile_statements()

let stdout = Ref{IO}(stdout)
    Base.PROGRAM_FILE = ""
    Sys.BINDIR = ""
    Sys.STDLIB = ""
    empty!(Base.ARGS)
    empty!(Core.ARGS)
    empty!(Base.TOML_CACHE.d)
    Base.TOML.reinit!(Base.TOML_CACHE.p, "")

    println("Outputting sysimage file...")
    Base.stdout = Core.stdout
    Base.stderr = Core.stderr

    # Print report after sysimage has been saved so all time spent can be captured
    pre_output_time = time_ns()
    Base.postoutput() do
        output_time = time_ns() - pre_output_time
        let stdout = stdout[]
            print(stdout, "Output ────── "); Base.time_print(stdout, output_time); println(stdout)
        end
        stdout[] = Core.stdout
    end
end

end # if
end # @eval

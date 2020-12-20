# This file is a part of Julia. License is MIT: https://julialang.org/license

if isempty(ARGS) || ARGS[1] !== "0"
Sys.__init_build()
# Prevent this from being put into the Main namespace
@eval Module() begin
if !isdefined(Base, :uv_eventloop)
    Base.reinit_stdio()
end
Base.include(@__MODULE__, joinpath(Sys.BINDIR::String, "..", "share", "julia", "test", "testhelpers", "FakePTYs.jl"))
import .FakePTYs: open_fake_pty
using Base.Meta

CTRL_C = '\x03'
UP_ARROW = "\e[A"
DOWN_ARROW = "\e[B"

hardcoded_precompile_statements = """
# used by Revise.jl
@assert precompile(Tuple{typeof(Base.parse_cache_header), String})
@assert precompile(Base.read_dependency_src, (String, String))
@assert precompile(Base.CoreLogging.current_logger_for_env, (Base.CoreLogging.LogLevel, String, Module))

# used by Requires.jl
@assert precompile(Tuple{typeof(get!), Type{Vector{Function}}, Dict{Base.PkgId,Vector{Function}}, Base.PkgId})
@assert precompile(Tuple{typeof(haskey), Dict{Base.PkgId,Vector{Function}}, Base.PkgId})
@assert precompile(Tuple{typeof(delete!), Dict{Base.PkgId,Vector{Function}}, Base.PkgId})
@assert precompile(Tuple{typeof(push!), Vector{Function}, Function})
# miscellaneous
@assert precompile(Tuple{typeof(Base.require), Base.PkgId})
@assert precompile(Tuple{typeof(Base.recursive_prefs_merge), Base.Dict{String, Any}})
@assert precompile(Tuple{typeof(isassigned), Core.SimpleVector, Int})
@assert precompile(Tuple{typeof(getindex), Core.SimpleVector, Int})
@assert precompile(Tuple{typeof(Base.Experimental.register_error_hint), Any, Type})
"""

repl_script = """
2+2
print("")
display([1])
display([1 2; 3 4])
@time 1+1
; pwd
$CTRL_C
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

julia_exepath() = joinpath(Sys.BINDIR::String, Base.julia_exename())

have_repl =  haskey(Base.loaded_modules,
                    Base.PkgId(Base.UUID("3fa0cd96-eef1-5676-8a61-b3b8758bbffb"), "REPL"))
if have_repl
    hardcoded_precompile_statements *= """
    @assert precompile(Tuple{typeof(getproperty), REPL.REPLBackend, Symbol})
    """
end

Distributed = get(Base.loaded_modules,
          Base.PkgId(Base.UUID("8ba89e20-285c-5b6f-9357-94700520ee1b"), "Distributed"),
          nothing)
if Distributed !== nothing
    hardcoded_precompile_statements *= """
    @assert precompile(Tuple{typeof(Distributed.remotecall),Function,Int,Module,Vararg{Any, 100}})
    @assert precompile(Tuple{typeof(Distributed.procs)})
    @assert precompile(Tuple{typeof(Distributed.finalize_ref), Distributed.Future})
    """
# This is disabled because it doesn't give much benefit
# and the code in Distributed is poorly typed causing many invalidations
#=
    precompile_script *= """
    using Distributed
    addprocs(2)
    pmap(x->iseven(x) ? 1 : 0, 1:4)
    @distributed (+) for i = 1:100 Int(rand(Bool)) end
    """
=#
end


Artifacts = get(Base.loaded_modules,
          Base.PkgId(Base.UUID("56f22d72-fd6d-98f1-02f0-08ddc0907c33"), "Artifacts"),
          nothing)
if Artifacts !== nothing
    precompile_script *= """
    using Artifacts, Base.BinaryPlatforms, Libdl
    artifacts_toml = abspath(joinpath(Sys.STDLIB, "Artifacts", "test", "Artifacts.toml"))
    # cd(() -> (name = "c_simple"; @artifact_str(name)), dirname(artifacts_toml))
    artifacts = Artifacts.load_artifacts_toml(artifacts_toml)
    platforms = [Artifacts.unpack_platform(e, "c_simple", artifacts_toml) for e in artifacts["c_simple"]]
    best_platform = select_platform(Dict(p => triplet(p) for p in platforms))
    dlopen("libjulia$(ccall(:jl_is_debugbuild, Cint, ()) != 0 ? "-debug" : "")", RTLD_LAZY | RTLD_DEEPBIND)
    """
end


Pkg = get(Base.loaded_modules,
          Base.PkgId(Base.UUID("44cfe95a-1eb2-52ea-b672-e2afdf69b78f"), "Pkg"),
          nothing)

if Pkg !== nothing
    # TODO: Split Pkg precompile script into REPL and script part
    repl_script *= Pkg.precompile_script
end

FileWatching = get(Base.loaded_modules,
          Base.PkgId(Base.UUID("7b1f6079-737a-58dc-b8bc-7a2ca5c1b5ee"), "FileWatching"),
          nothing)
if FileWatching !== nothing
    hardcoded_precompile_statements *= """
    @assert precompile(Tuple{typeof(FileWatching.watch_file), String, Float64})
    @assert precompile(Tuple{typeof(FileWatching.watch_file), String, Int})
    @assert precompile(Tuple{typeof(FileWatching._uv_hook_close), FileWatching.FileMonitor})
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

Test = get(Base.loaded_modules,
          Base.PkgId(Base.UUID("8dfed614-e22c-5e08-85e1-65c5234f0b40"), "Test"),
          nothing)
if Test !== nothing
    hardcoded_precompile_statements *= """
    @assert precompile(Tuple{typeof(Test.do_test), Test.ExecutionResult, Any})
    @assert precompile(Tuple{typeof(Test.testset_beginend), Tuple{String, Expr}, Expr, LineNumberNode})
    @assert precompile(Tuple{typeof(Test.finish), Test.DefaultTestSet})
    @assert precompile(Tuple{typeof(Test.eval_test), Expr, Expr, LineNumberNode, Bool})
    """
end

function generate_precompile_statements()
    start_time = time_ns()
    debug_output = devnull # or stdout
    sysimg = Base.unsafe_string(Base.JLOptions().image_file)

    # Extract the precompile statements from the precompile file
    statements = Set{String}()

    # From hardcoded statements
    for statement in split(hardcoded_precompile_statements::String, '\n')
        push!(statements, statement)
    end

    # Collect statements from running the script
    mktempdir() do prec_path
        # Also precompile a package here
        pkgname = "__PackagePrecompilationStatementModule"
        mkpath(joinpath(prec_path, pkgname, "src"))
        path = joinpath(prec_path, pkgname, "src", "$pkgname.jl")
        write(path,
              """
              module $pkgname
              end
              """)
        tmp = tempname()
        s = """
            pushfirst!(DEPOT_PATH, $(repr(prec_path)));
            Base.PRECOMPILE_TRACE_COMPILE[] = $(repr(tmp));
            Base.compilecache(Base.PkgId($(repr(pkgname))), $(repr(path)))
            $precompile_script
            """
        run(`$(julia_exepath()) -O0 --sysimage $sysimg --startup-file=no -Cnative -e $s`)
        for statement in split(read(tmp, String), '\n')
            push!(statements, statement)
        end
    end

    mktemp() do precompile_file, precompile_file_h
        # Collect statements from running a REPL process and replaying our REPL script
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
            precompile_lines = split(repl_script::String, '\n'; keepempty=false)
            curr = 0
            for l in precompile_lines
                sleep(0.1)
                curr += 1
                print("\rGenerating REPL precompile statements... $curr/$(length(precompile_lines))")
                # consume any other output
                bytesavailable(output_copy) > 0 && readavailable(output_copy)
                # push our input
                write(debug_output, "\n#### inputting statement: ####\n$(repr(l))\n####\n")
                write(ptm, l, "\n")
                readuntil(output_copy, "\n")
                # wait for the next prompt-like to appear
                # NOTE: this is rather inaccurate because the Pkg REPL mode is a special flower
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

        for statement in split(read(precompile_file, String), '\n')
            # Main should be completely clean
            occursin("Main.", statement) && continue
            push!(statements, statement)
        end
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
        # The compiler has problem caching signatures with `Vararg{?, N}`. Replacing
        # N with a large number seems to work around it.
        ps = Meta.parse(statement)
        if isexpr(ps, :call)
            if isexpr(ps.args[end], :curly)
                l = ps.args[end]
                if length(l.args) == 2 && l.args[1] == :Vararg
                    push!(l.args, 100)
                end
            end
        end
        try
            # println(ps)
            Core.eval(PrecompileStagingArea, ps)
            n_succeeded += 1
            print("\rExecuting precompile statements... $n_succeeded/$(length(statements))")
        catch
            # See #28808
            @error "Failed to precompile $statement"
        end
    end
    println()
    if have_repl
        # Seems like a reasonable number right now, adjust as needed
        # comment out if debugging script
        @assert n_succeeded > 1200
    end

    tot_time = time_ns() - start_time
    include_time *= 1e9
    gen_time = tot_time - include_time
    println("Precompilation complete. Summary:")
    print("Total ─────── "); Base.time_print(tot_time); println()
    print("Generation ── "); Base.time_print(gen_time);     print(" "); show(IOContext(stdout, :compact=>true), gen_time / tot_time * 100); println("%")
    print("Execution ─── "); Base.time_print(include_time); print(" "); show(IOContext(stdout, :compact=>true), include_time / tot_time * 100); println("%")

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

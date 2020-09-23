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
# used by Revise.jl
@assert precompile(Tuple{typeof(Base.parse_cache_header), String})
@assert precompile(Tuple{typeof(pushfirst!), Vector{Any}, Function})
# used by Requires.jl
@assert precompile(Tuple{typeof(get!), Type{Vector{Function}}, Dict{Base.PkgId,Vector{Function}}, Base.PkgId})
@assert precompile(Tuple{typeof(haskey), Dict{Base.PkgId,Vector{Function}}, Base.PkgId})
@assert precompile(Tuple{typeof(delete!), Dict{Base.PkgId,Vector{Function}}, Base.PkgId})
@assert precompile(Tuple{typeof(push!), Vector{Function}, Function})
# miscellaneous
@assert precompile(Tuple{typeof(Base.require), Base.PkgId})
@assert precompile(Tuple{typeof(isassigned), Core.SimpleVector, Int})
@assert precompile(Tuple{typeof(Base.Experimental.register_error_hint), Any, Type})

# precompilation
precompile(Tuple{Type{REPL.Terminals.TTYTerminal}, String, Base.PipeEndpoint, Base.TTY, Base.TTY})
precompile(Tuple{typeof(Base.read), Base.PipeEndpoint, Type{String}})
precompile(Tuple{typeof(Base.Meta.parse), String})
precompile(Tuple{typeof(Base.MainInclude.eval), Expr})
precompile(Tuple{Type{Base.Pair{A, B} where B where A}, Base.PkgId, UInt64})
precompile(Tuple{Type{Base.PkgId}, String})
precompile(Tuple{typeof(Base.unique), Array{Any, 1}})
precompile(Tuple{typeof(Base.in), Tuple{Module, String, Float64}, Base.Set{Any}})
precompile(Tuple{typeof(Base.push!), Base.Set{Any}, Tuple{Module, String, Float64}})

# Loading a JLL package
precompile(Tuple{typeof(Base.show), Base.GenericIOBuffer{Array{UInt8, 1}}, UInt64})
precompile(Tuple{typeof(Base.julia_cmd)})
precompile(Tuple{Type{NamedTuple{(:stderr,), T} where T<:Tuple}, Tuple{Base.TTY}})
precompile(Tuple{getfield(Base, Symbol("#pipeline##kw")), NamedTuple{(:stderr,), Tuple{Base.TTY}}, typeof(Base.pipeline), Base.Cmd})
precompile(Tuple{typeof(Base.open), Base.CmdRedirect, String, Base.TTY})
precompile(Tuple{typeof(Base._spawn), Base.Cmd, Array{Any, 1}})
precompile(Tuple{typeof(Base.convert), Type{IO}, Base.PipeEndpoint})
precompile(Tuple{typeof(Base.getproperty), Base.Process, Symbol})
precompile(Tuple{typeof(Base.show), Base.GenericIOBuffer{Array{UInt8, 1}}, Array{String, 1}})
precompile(Tuple{typeof(Base.write), Base.PipeEndpoint, String})
precompile(Tuple{typeof(Base.success), Base.Process})
precompile(Tuple{getfield(Base, Symbol("#@eval")), LineNumberNode, Module, Any})
precompile(Tuple{getfield(Base, Symbol("#@static")), LineNumberNode, Module, Any})
precompile(Tuple{getfield(Base, Symbol("#@v_str")), LineNumberNode, Module, Any})
precompile(Tuple{typeof(Base.ident_cmp), Tuple{String, UInt64}, Tuple{String}})
precompile(Tuple{typeof(Base.getindex), Type{Union{Nothing, Expr}}, Expr, Expr})
precompile(Tuple{typeof(Base.iterate), Array{Union{Nothing, Expr}, 1}})
precompile(Tuple{typeof(Base.append!), Array{Any, 1}, Array{Any, 1}})
precompile(Tuple{typeof(Base.iterate), Array{Union{Nothing, Expr}, 1}, Int64})
precompile(Tuple{Type{Ref{String}}, String})
precompile(Tuple{Type{Base.Dict{Base.BinaryPlatforms.Platform, String}}})
precompile(Tuple{Type{NamedTuple{(:pkg_uuid,), T} where T<:Tuple}, Tuple{Base.UUID}})
precompile(Tuple{getfield(Artifacts, Symbol("#load_artifacts_toml##kw")), NamedTuple{(:pkg_uuid,), Tuple{Base.UUID}}, typeof(Artifacts.load_artifacts_toml), String})
precompile(Tuple{typeof(Base.Filesystem.readdir), String})
precompile(Tuple{typeof(Base.parse), Type{Base.BinaryPlatforms.Platform}, String})
precompile(Tuple{Type{NamedTuple{(:validate_strict, :libc, :call_abi, :libgfortran_version, :libstdcxx_version, :cxxstring_abi, :os_version), T} where T<:Tuple}, Tuple{Bool, String, Nothing, Nothing, Nothing, Nothing, Nothing}})
precompile(Tuple{typeof(Base.merge), NamedTuple{(:validate_strict, :libc, :call_abi, :libgfortran_version, :libstdcxx_version, :cxxstring_abi, :os_version), Tuple{Bool, String, Nothing, Nothing, Nothing, Nothing, Nothing}}, Array{Base.Pair{String, String}, 1}})
precompile(Tuple{typeof(Base.merge), NamedTuple{(:validate_strict, :libc, :call_abi, :libgfortran_version, :libstdcxx_version, :cxxstring_abi, :os_version), Tuple{Bool, String, Nothing, Nothing, Nothing, Nothing, Nothing}}, NamedTuple{(), Tuple{}}})
precompile(Tuple{getfield(Core, Symbol("#Type##kw")), NamedTuple{(:validate_strict, :libc, :call_abi, :libgfortran_version, :libstdcxx_version, :cxxstring_abi, :os_version), Tuple{Bool, String, Nothing, Nothing, Nothing, Nothing, Nothing}}, Type{Base.BinaryPlatforms.Platform}, String, String})
precompile(Tuple{Type{Base.Pair{A, B} where B where A}, Base.BinaryPlatforms.Platform, String})
precompile(Tuple{typeof(Base.indexed_iterate), Base.Pair{Base.BinaryPlatforms.Platform, String}, Int64})
precompile(Tuple{typeof(Base.indexed_iterate), Base.Pair{Base.BinaryPlatforms.Platform, String}, Int64, Int64})
precompile(Tuple{typeof(Base.empty), Base.Dict{Any, Any}, Type{Base.BinaryPlatforms.Platform}, Type{String}})
precompile(Tuple{typeof(Base.setindex!), Base.Dict{Base.BinaryPlatforms.Platform, String}, String, Base.BinaryPlatforms.Platform})
precompile(Tuple{Type{NamedTuple{(:validate_strict, :libc, :call_abi, :libgfortran_version, :libstdcxx_version, :cxxstring_abi, :os_version), T} where T<:Tuple}, Tuple{Bool, String, String, Nothing, Nothing, Nothing, Nothing}})
precompile(Tuple{typeof(Base.merge), NamedTuple{(:validate_strict, :libc, :call_abi, :libgfortran_version, :libstdcxx_version, :cxxstring_abi, :os_version), Tuple{Bool, String, String, Nothing, Nothing, Nothing, Nothing}}, Array{Base.Pair{String, String}, 1}})
precompile(Tuple{typeof(Base.merge), NamedTuple{(:validate_strict, :libc, :call_abi, :libgfortran_version, :libstdcxx_version, :cxxstring_abi, :os_version), Tuple{Bool, String, String, Nothing, Nothing, Nothing, Nothing}}, NamedTuple{(), Tuple{}}})
precompile(Tuple{getfield(Core, Symbol("#Type##kw")), NamedTuple{(:validate_strict, :libc, :call_abi, :libgfortran_version, :libstdcxx_version, :cxxstring_abi, :os_version), Tuple{Bool, String, String, Nothing, Nothing, Nothing, Nothing}}, Type{Base.BinaryPlatforms.Platform}, String, String})
precompile(Tuple{Type{NamedTuple{(:validate_strict, :libc, :call_abi, :libgfortran_version, :libstdcxx_version, :cxxstring_abi, :os_version), T} where T<:Tuple}, Tuple{Bool, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing}})
precompile(Tuple{typeof(Base.merge), NamedTuple{(:validate_strict, :libc, :call_abi, :libgfortran_version, :libstdcxx_version, :cxxstring_abi, :os_version), Tuple{Bool, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing}}, Array{Base.Pair{String, String}, 1}})
precompile(Tuple{typeof(Base.merge), NamedTuple{(:validate_strict, :libc, :call_abi, :libgfortran_version, :libstdcxx_version, :cxxstring_abi, :os_version), Tuple{Bool, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing}}, NamedTuple{(), Tuple{}}})
precompile(Tuple{getfield(Core, Symbol("#Type##kw")), NamedTuple{(:validate_strict, :libc, :call_abi, :libgfortran_version, :libstdcxx_version, :cxxstring_abi, :os_version), Tuple{Bool, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing}}, Type{Base.BinaryPlatforms.Platform}, String, String})
precompile(Tuple{Type{NamedTuple{(:validate_strict, :libc, :call_abi, :libgfortran_version, :libstdcxx_version, :cxxstring_abi, :os_version), T} where T<:Tuple}, Tuple{Bool, Nothing, Nothing, Nothing, Nothing, Nothing, Base.VersionNumber}})
precompile(Tuple{typeof(Base.merge), NamedTuple{(:validate_strict, :libc, :call_abi, :libgfortran_version, :libstdcxx_version, :cxxstring_abi, :os_version), Tuple{Bool, Nothing, Nothing, Nothing, Nothing, Nothing, Base.VersionNumber}}, Array{Base.Pair{String, String}, 1}})
precompile(Tuple{typeof(Base.merge), NamedTuple{(:validate_strict, :libc, :call_abi, :libgfortran_version, :libstdcxx_version, :cxxstring_abi, :os_version), Tuple{Bool, Nothing, Nothing, Nothing, Nothing, Nothing, Base.VersionNumber}}, NamedTuple{(), Tuple{}}})
precompile(Tuple{getfield(Core, Symbol("#Type##kw")), NamedTuple{(:validate_strict, :libc, :call_abi, :libgfortran_version, :libstdcxx_version, :cxxstring_abi, :os_version), Tuple{Bool, Nothing, Nothing, Nothing, Nothing, Nothing, Base.VersionNumber}}, Type{Base.BinaryPlatforms.Platform}, String, String})
precompile(Tuple{getfield(Base, Symbol("#@static")), LineNumberNode, Module, Any})
precompile(Tuple{typeof(Base.Filesystem.isdir), String})
precompile(Tuple{typeof(Base.isequal), Base.BinaryPlatforms.Platform, Base.BinaryPlatforms.Platform})
precompile(Tuple{typeof(Base.getindex), Type{Expr}})
precompile(Tuple{typeof(Base.isempty), Array{Symbol, 1}})
precompile(Tuple{typeof(Base.getindex), Type{Union{Nothing, Expr}}, Expr})
precompile(Tuple{typeof(Base.isequal), Tuple{Module, String, Float64}, Tuple{Module, String, Float64}})
precompile(Tuple{typeof(Base.join), Array{String, 1}, Char})
precompile(Tuple{typeof(Base.setindex!), Base.RefValue{String}, String})
precompile(Tuple{typeof(Base.invokelatest), Any})
precompile(Tuple{typeof(Base.append!), Array{String, 1}, Array{String, 1}})
precompile(Tuple{typeof(Base.push!), Array{Expr, 1}, Expr})
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
# Used by JuliaInterpreter
push!(Set{Module}(), Main)
push!(Set{Method}(), first(methods(collect)))
# Used by Revise
(setindex!(Dict{String,Base.PkgId}(), Base.PkgId(Base), "file.jl"))["file.jl"]
(setindex!(Dict{Base.PkgId,String}(), "file.jl", Base.PkgId(Base)))[Base.PkgId(Base)]
get(Base.pkgorigins, Base.PkgId(Base), nothing)
"""

julia_exepath() = joinpath(Sys.BINDIR, Base.julia_exename())

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
    precompile_script *= """
    using Distributed
    addprocs(2)
    pmap(x->iseven(x) ? 1 : 0, 1:4)
    @distributed (+) for i = 1:100 Int(rand(Bool)) end
    """
end

Artifacts = get(Base.loaded_modules,
          Base.PkgId(Base.UUID("56f22d72-fd6d-98f1-02f0-08ddc0907c33"), "Artifacts"),
          nothing)
if Artifacts !== nothing
    precompile_script *= """
    using Artifacts, Base.BinaryPlatforms, Libdl
    artifacts_toml = abspath(joinpath(Sys.STDLIB, "Artifacts", "test", "Artifacts.toml"))
    cd(() -> @artifact_str("c_simple"), dirname(artifacts_toml))
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
    precompile_script *= Pkg.precompile_script
end

FileWatching = get(Base.loaded_modules,
          Base.PkgId(Base.UUID("7b1f6079-737a-58dc-b8bc-7a2ca5c1b5ee"), "FileWatching"),
          nothing)
if FileWatching !== nothing
    hardcoded_precompile_statements *= """
    @assert precompile(Tuple{typeof(FileWatching.watch_file), String, Float64})
    @assert precompile(Tuple{typeof(FileWatching.watch_file), String, Int})
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
            @assert n_succeeded > 1200
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

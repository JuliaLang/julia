# known precompilation failures under JL
const INCOMPATIBLE_STDLIBS = String[]

const JULIA_EXECUTABLE = Base.unsafe_string(Base.JLOptions().julia_bin)

function run_quietly(cmd)
    mktemp() do _, output
        ok = success(pipeline(cmd; stdout=output, stderr=output))
        if !ok
            seekstart(output)
            write(stderr, read(output))
            error("command failed: $cmd")
        end
    end
    return nothing
end

stdlibs_to_test = filter(name -> !in(name, INCOMPATIBLE_STDLIBS), readdir(Sys.STDLIB))
push!(stdlibs_to_test, "Compiler")

configs = [
    # ``=>Base.CacheFlags(check_bounds=0, debug_level=2, opt_level=3),
    ``=>Base.CacheFlags(check_bounds=1, debug_level=2, opt_level=3),
]

compiler_path = joinpath(Sys.STDLIB, "..", "..", "Compiler")
setupproject_command = "using Pkg; Pkg.add($(stdlibs_to_test)); Pkg.develop(path=$(repr(compiler_path)))"
compilecache_command = "using Base: CacheFlags; Base.Precompilation.precompilepkgs($(stdlibs_to_test); configs=$(configs))"

# pre-compile stdlibs (into temporary depot)
mktempdir() do tmp_depot
    # first setup the project / environment
    env_dir = joinpath(tmp_depot, "environments", "v$(VERSION.major).$(VERSION.minor)")
    cmd = addenv(
        `$(JULIA_EXECUTABLE) --startup-file=no --project=$env_dir -e $setupproject_command`,
        ; inherit = true
    )
    run_quietly(cmd)

    # now actually perform the precompilation
    cmd = addenv(
        `$(JULIA_EXECUTABLE) --startup-file=no -e $compilecache_command`,
        "JULIA_LOAD_PATH" => "@stdlib$(Base.Linking.pathsep)$(env_dir)",
        "JULIA_CPU_TARGET" => "sysimage",
        "JULIA_USE_FLISP_LOWERING" => "0",
        "JULIA_USE_FALLBACK_REPL" => "0",
        "JULIA_DEPOT_PATH" => tmp_depot,
        ; inherit = true
    )
    run_quietly(cmd)
end

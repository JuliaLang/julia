import Libdl

# known test failures under JL
const INCOMPATIBLE_STDLIBS = String[

    # root-caused / minimized failures
    "Distributed",      # uses post-lowering `Expr(:const, ...)` form  (JuliaLang/JuliaLowering.jl#149)
    "InteractiveUtils", # highlighting test failure                    (JuliaLang/JuliaLowering.jl#155)
    "Logging",          # `@noinline` modifies stack trace             (JuliaLang/JuliaLowering.jl#159)
    "Mmap",             # static parameters + default args failure     (JuliaLang/JuliaLowering.jl#158)
    "Profile",          # unexpected kwarg syntax                      (JuliaLang/JuliaLowering.jl#151)
    "SparseArrays",     # broadcasted type regression                  (JuliaLang/JuliaLowering.jl#152)
    "Test",             # overloading `..`                             (JuliaLang/JuliaLowering.jl#154) 

    # undiagnosed failures
    "REPL",  # inference failures (4 items pre-compiled, but expected 0)
    "Pkg",   # box failures (TBD)

    # these take too long to run, even if they did pass
    "LinearAlgebra", # fails due to @inferred regressions
]

const JULIA_EXECUTABLE = Base.unsafe_string(Base.JLOptions().julia_bin)
const JULIA_CPU_TARGET = get(ENV, "JULIA_CPU_TARGET", Base.unsafe_string(Base.JLOptions().cpu_target))
const debug = get(ENV, "JULIA_BUILD_MODE", "release") == "debug" ? "-debug" : ""
const JL_sysimage = joinpath(dirname(unsafe_string(Base.JLOptions().image_file)), "sys-JL$(debug).$(Libdl.dlext)")

function compile_JL_sysimage(output_filepath)
    sysimage = unsafe_string(Base.JLOptions().image_file)
    output_sysimage = abspath(output_filepath)
    output_object = "$(splitext(output_sysimage)[1])-o.a"

    package_root = joinpath(Sys.STDLIB, "..", "..", "JuliaLowering")
    cmd = `$(JULIA_EXECUTABLE) -C "$(JULIA_CPU_TARGET)" --output-o $(output_object)
           --startup-file=no --warn-overwrite=yes --depwarn=error --sysimage $(sysimage)
           -e "Core.include(Base, $(repr(joinpath(package_root, "src", "JuliaLowering.jl"))))"`
    cmd = addenv(
        Base.Cmd(cmd; dir = joinpath(package_root, "src")),
        "JULIA_BINDIR" => unsafe_string(Base.JLOptions().julia_bindir),
        "JULIA_LOAD_PATH" => "@stdlib",
        "JULIA_PROJECT" => nothing,
        "JULIA_DEPOT_PATH" => ":",
        "JULIA_NUM_THREADS" => "1",
    )
    println("Compiling incremental sysimage with JuliaLowering...")
    success(run(cmd))

    cmd = Base.Linking.link_image_cmd(output_object, output_sysimage)
    success(run(cmd))

    return nothing
end

# ensure JL-inclusive sysimage is built / available
if "BUILDROOT" in keys(ENV)
    # Running via Makefile, use sysimage.mk with its built-in caching / file tracking
    run(`$(ENV["MAKE"]) -C $(ENV["BUILDROOT"]) -f sysimage.mk sysimg-JL-$(ENV["JULIA_BUILD_MODE"])`)
else
    # Standalone test run (CI), compile every time
    compile_JL_sysimage(JL_sysimage)
end
stdlibs = readdir(Sys.STDLIB)
stdlibs_to_test = filter(name -> !in(name, INCOMPATIBLE_STDLIBS), stdlibs)

configs = [
    ``=>Base.CacheFlags(check_bounds=0, debug_level=2, opt_level=3),
    ``=>Base.CacheFlags(check_bounds=1, debug_level=2, opt_level=3),
]
setupproject_command = "using Pkg; Pkg.add($(stdlibs))"
compilecache_command = "using Base: CacheFlags; Base.Precompilation.precompilepkgs($(stdlibs); configs=$(configs))"
testpkgs_command = "using Pkg; Pkg.test($(stdlibs_to_test))"

# pre-compile stdlibs (into temporary depot)
mktempdir() do tmp_depot
    tmp_depot = abspath("./JuliaLowering_depot")

    # first setup the project / environment
    env_dir = joinpath(tmp_depot, "environments", "v$(VERSION.major).$(VERSION.minor)")
    cmd = addenv(
        `$(JULIA_EXECUTABLE) --startup-file=no --project=$env_dir -e $setupproject_command`,
        ; inherit = true
    )
    success(run(cmd))

    # now actually perform the precompilation
    cmd = addenv(
        `$(JULIA_EXECUTABLE) --sysimage $(JL_sysimage) --startup-file=no -e $compilecache_command`,
        "JULIA_LOAD_PATH" => "@stdlib$(Base.Linking.pathsep)$(env_dir)",
        "JULIA_CPU_TARGET" => "sysimage",
        "JULIA_USE_FLISP_LOWERING" => "0",
        "JULIA_USE_FALLBACK_REPL" => "0",
        "JULIA_DEPOT_PATH" => tmp_depot,
        ; inherit = true
    )
    success(run(cmd))

    # and then run the stdlib tests
    cmd = addenv(
        `$(JULIA_EXECUTABLE) --sysimage $(JL_sysimage) --startup-file=no -e $testpkgs_command`,
        "JULIA_LOAD_PATH" => "@stdlib$(Base.Linking.pathsep)$(env_dir)",
        "JULIA_CPU_TARGET" => "sysimage",
        "JULIA_USE_FLISP_LOWERING" => "0",
        "JULIA_USE_FALLBACK_REPL" => "0",
        "JULIA_DEPOT_PATH" => tmp_depot,
        ; inherit = true
    )
    success(run(cmd))
end

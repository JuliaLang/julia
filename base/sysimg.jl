# This file is a part of Julia. License is MIT: https://julialang.org/license

# Can be loaded on top of either an existing system image built from
# `Base_compiler.jl` or standalone, in which case we will build it now.
let had_compiler = isdefined(Main, :Base)
if had_compiler; else
include("Base_compiler.jl")
end

Core.include(Base, "Base.jl")

had_compiler && ccall(:jl_init_restored_module, Cvoid, (Any,), Base)
end

# Set up Main module
using Base.MainInclude # ans, err, and sometimes Out

# These definitions calls Base._include rather than Base.include to get
# one-frame stacktraces for the common case of using include(fname) in Main.

"""
    include([mapexpr::Function,] path::AbstractString)

Evaluate the contents of the input source file in the global scope of the containing module.
Every module (except those defined with `baremodule`) has its own
definition of `include`, which evaluates the file in that module.
Returns the result of the last evaluated expression of the input file. During including,
a task-local include path is set to the directory containing the file. Nested calls to
`include` will search relative to that path. This function is typically used to load source
interactively, or to combine files in packages that are broken into multiple source files.
The argument `path` is normalized using [`normpath`](@ref) which will resolve
relative path tokens such as `..` and convert `/` to the appropriate path separator.

The optional first argument `mapexpr` can be used to transform the included code before
it is evaluated: for each parsed expression `expr` in `path`, the `include` function
actually evaluates `mapexpr(expr)`.  If it is omitted, `mapexpr` defaults to [`identity`](@ref).

Use [`Base.include`](@ref) to evaluate a file into another module.

!!! note
    Julia's syntax lowering recognizes an explicit call to a literal `include`
    at top-level and inserts an implicit `@Core.latestworld` to make any include'd
    definitions visible to subsequent code. Note however that this recognition
    is *syntactic*. I.e. assigning `const myinclude = include` may require
    and explicit `@Core.latestworld` call after `myinclude`.

!!! compat "Julia 1.5"
    Julia 1.5 is required for passing the `mapexpr` argument.
"""
const include = Base.IncludeInto(Main)

"""
    eval(expr)

Evaluate an expression in the global scope of the containing module.
Every `Module` (except those defined with `baremodule`) has its own 1-argument
definition of `eval`, which evaluates expressions in that module.
"""
const eval = Core.EvalInto(Main)

# Ensure this file is also tracked
pushfirst!(Base._included_files, (@__MODULE__, abspath(@__FILE__)))

# set up depot & load paths to be able to find stdlib packages
Base.init_depot_path()
Base.init_load_path()

if Base.is_primary_base_module
# load some stdlib packages but don't put their names in Main
let
    # Loading here does not call __init__(). This leads to uninitialized RNG
    # state which causes rand(::UnitRange{Int}) to hang. This is a workaround:
    task = current_task()
    task.rngState0 = 0x5156087469e170ab
    task.rngState1 = 0x7431eaead385992c
    task.rngState2 = 0x503e1d32781c2608
    task.rngState3 = 0x3a77f7189200c20b
    task.rngState4 = 0x5502376d099035ae

    # Stdlibs sorted in dependency, then alphabetical, order by contrib/print_sorted_stdlibs.jl
    # Run with the `--exclude-jlls` option to filter out all JLL packages
    if isdefined(Base.BuildSettings, :INCLUDE_STDLIBS)
        # e.g. INCLUDE_STDLIBS = "FileWatching,Libdl,Artifacts,SHA,Sockets,LinearAlgebra,Random"
        stdlibs = Symbol.(split(Base.BuildSettings.INCLUDE_STDLIBS, ","))
    else
        # TODO: this is included for compatibility with PackageCompiler, which looks for it.
        # This should eventually be removed so we only use `BuildSettings`.
        stdlibs = [
            # No dependencies
            :FileWatching, # used by loading.jl -- implicit assumption that init runs
            :Libdl, # Transitive through LinAlg
            :Artifacts, # Transitive through LinAlg
            :SHA, # transitive through Random
            :Sockets, # used by stream.jl

            # Transitive through LingAlg
            # OpenBLAS_jll
            # libblastrampoline_jll

            # 1-depth packages
            :LinearAlgebra, # Commits type-piracy and GEMM
            :Random, # Can't be removed due to rand being exported by Base
        ]
    end
    # PackageCompiler can filter out stdlibs so it can be empty
    maxlen = maximum(textwidth.(string.(stdlibs)); init=0)

    tot_time_stdlib = 0.0
    # use a temp module to avoid leaving the type of this closure in Main
    push!(empty!(LOAD_PATH), "@stdlib")
    m = Core.Module()
    GC.@preserve m begin
        print_time = @eval m (mod, t) -> (print(rpad(string(mod) * "  ", $maxlen + 3, "─"));
                                          Base.time_print(stdout, t * 10^9); println())
        print_time(Base, (Base.end_base_include - Base.start_base_include) * 10^(-9))

        Base._track_dependencies[] = true
        tot_time_stdlib = @elapsed for stdlib in stdlibs
            tt = @elapsed Base.require(Base, stdlib)
            print_time(stdlib, tt)
        end
        for dep in Base._require_dependencies
            mod, path, fsize, mtime = dep[1], dep[2], dep[3], dep[5]
            (fsize == 0 || mtime == 0.0) && continue
            push!(Base._included_files, (mod, path))
        end
        empty!(Base._require_dependencies)
        Base._track_dependencies[] = false

        print_time("Stdlibs total", tot_time_stdlib)
    end

    # Clear global state
    empty!(Core.ARGS)
    empty!(Base.ARGS)
    empty!(LOAD_PATH)
    Base.init_load_path() # want to be able to find external packages in userimg.jl

    ccall(:jl_clear_implicit_imports, Cvoid, (Any,), Main)

    tot_time_userimg = @elapsed (isfile("userimg.jl") && Base.include(Main, "userimg.jl"))

    tot_time_base = (Base.end_base_include - Base.start_base_include) * 10.0^(-9)
    tot_time = tot_time_base + tot_time_stdlib + tot_time_userimg

    println("Sysimage built. Summary:")
    print("Base ──────── "); Base.time_print(stdout, tot_time_base    * 10^9); print(" "); show(IOContext(stdout, :compact=>true), (tot_time_base    / tot_time) * 100); println("%")
    print("Stdlibs ───── "); Base.time_print(stdout, tot_time_stdlib  * 10^9); print(" "); show(IOContext(stdout, :compact=>true), (tot_time_stdlib  / tot_time) * 100); println("%")
    if isfile("userimg.jl")
    print("Userimg ───── "); Base.time_print(stdout, tot_time_userimg * 10^9); print(" "); show(IOContext(stdout, :compact=>true), (tot_time_userimg / tot_time) * 100); println("%")
    end
    print("Total ─────── "); Base.time_print(stdout, tot_time         * 10^9); println();

    empty!(LOAD_PATH)
    empty!(DEPOT_PATH)
end

empty!(Base.TOML_CACHE.d)
Base.TOML.reinit!(Base.TOML_CACHE.p, "")
@eval Base BUILDROOT = ""
@eval Sys begin
    BINDIR = ""
    STDLIB = ""
end
end

# This file is a part of Julia. License is MIT: https://julialang.org/license

## initdefs.jl - initialization and runtime management definitions

"""
    PROGRAM_FILE

A string containing the script name passed to Julia from the command line. Note that the
script name remains unchanged from within included files. Alternatively see
[`@__FILE__`](@ref).
"""
global PROGRAM_FILE = ""

"""
    ARGS

An array of the command line arguments passed to Julia, as strings.
"""
const ARGS = String[]

"""
    exit(code=0)

Quit the program with an exit code. The default exit code is zero, indicating that the
program completed successfully (see also [`quit`](@ref)). In an interactive session,
`exit()` can be called with the keyboard shorcut `^D`.

"""
exit(n) = ccall(:jl_exit, Cvoid, (Int32,), n)
exit() = exit(0)

"""
    quit()

Quit the program indicating successful completion. This function is equivalent to
`exit(0)` (see [`exit`](@ref)). In an interactive session, `quit()` can be called
with the keyboard shorcut `^D`.
"""
quit() = exit()

const roottask = current_task()

is_interactive = false

"""
    isinteractive() -> Bool

Determine whether Julia is running an interactive session.
"""
isinteractive() = (is_interactive::Bool)

"""
    LOAD_PATH

An array of paths as strings or custom loader objects for the `require`
function and `using` and `import` statements to consider when loading
code.
"""
const LOAD_PATH = String[]
const LOAD_CACHE_PATH = String[]

function init_load_path(BINDIR = Sys.BINDIR)
    vers = "v$(VERSION.major).$(VERSION.minor)"
    if haskey(ENV, "JULIA_LOAD_PATH")
        prepend!(LOAD_PATH, split(ENV["JULIA_LOAD_PATH"], @static Sys.iswindows() ? ';' : ':'))
    end
    push!(LOAD_PATH, abspath(BINDIR, "..", "local", "share", "julia", "site", vers))
    push!(LOAD_PATH, abspath(BINDIR, "..", "share", "julia", "site", vers))
    #push!(LOAD_CACHE_PATH, abspath(BINDIR, "..", "lib", "julia")) #TODO: add a builtin location?
end

function early_init()
    Sys._early_init()
    # make sure OpenBLAS does not set CPU affinity (#1070, #9639)
    ENV["OPENBLAS_MAIN_FREE"] = get(ENV, "OPENBLAS_MAIN_FREE",
                                    get(ENV, "GOTOBLAS_MAIN_FREE", "1"))
    if Sys.CPU_CORES > 8 && !("OPENBLAS_NUM_THREADS" in keys(ENV)) && !("OMP_NUM_THREADS" in keys(ENV))
        # Prevent openblas from starting too many threads, unless/until specifically requested
        ENV["OPENBLAS_NUM_THREADS"] = 8
    end
end

const atexit_hooks = []

"""
    atexit(f)

Register a zero-argument function `f()` to be called at process exit. `atexit()` hooks are
called in last in first out (LIFO) order and run before object finalizers.
"""
atexit(f::Function) = (pushfirst!(atexit_hooks, f); nothing)

function _atexit()
    for f in atexit_hooks
        try
            f()
        catch err
            show(STDERR, err)
            println(STDERR)
        end
    end
end

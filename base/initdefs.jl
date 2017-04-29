# This file is a part of Julia. License is MIT: http://julialang.org/license

## initdefs.jl - initialization and runtime management definitions

"""
    PROGRAM_FILE

A string containing the script name passed to Julia from the command line. Note that the
script name remains unchanged from within included files. Alternatively see
[`@__FILE__`](@ref).
"""
PROGRAM_FILE = ""

"""
    ARGS

An array of the command line arguments passed to Julia, as strings.
"""
const ARGS = String[]

exit(n) = ccall(:jl_exit, Void, (Int32,), n)
exit() = exit(0)
quit() = exit()

const roottask = current_task()

is_interactive = false
isinteractive() = (is_interactive::Bool)

"""
    LOAD_PATH

An array of paths as strings or custom loader objects for the `require`
function and `using` and `import` statements to consider when loading
code. To create a custom loader type, define the type and then add
appropriate methods to the `Base.load_hook` function with the following
signature:

    Base.load_hook(loader::Loader, name::String, found::Any)

The `loader` argument is the current value in `LOAD_PATH`, `name` is the
name of the module to load, and `found` is the path of any previously
found code to provide `name`. If no provider has been found earlier in
`LOAD_PATH` then the value of `found` will be `nothing`. Custom loader
functionality is experimental and may break or change in Julia 1.0.
"""
const LOAD_PATH = Any[]
const LOAD_CACHE_PATH = String[]

function init_load_path()
    vers = "v$(VERSION.major).$(VERSION.minor)"
    if haskey(ENV, "JULIA_LOAD_PATH")
        prepend!(LOAD_PATH, split(ENV["JULIA_LOAD_PATH"], @static is_windows() ? ';' : ':'))
    end
    push!(LOAD_PATH, abspath(JULIA_HOME, "..", "local", "share", "julia", "site", vers))
    push!(LOAD_PATH, abspath(JULIA_HOME, "..", "share", "julia", "site", vers))
    #push!(LOAD_CACHE_PATH, abspath(JULIA_HOME, "..", "lib", "julia")) #TODO: add a builtin location?
end

function early_init()
    global const JULIA_HOME = ccall(:jl_get_julia_home, Any, ())
    # make sure OpenBLAS does not set CPU affinity (#1070, #9639)
    ENV["OPENBLAS_MAIN_FREE"] = get(ENV, "OPENBLAS_MAIN_FREE",
                                    get(ENV, "GOTOBLAS_MAIN_FREE", "1"))
    if Sys.CPU_CORES > 8 && !("OPENBLAS_NUM_THREADS" in keys(ENV)) && !("OMP_NUM_THREADS" in keys(ENV))
        # Prevent openblas from starting too many threads, unless/until specifically requested
        ENV["OPENBLAS_NUM_THREADS"] = 8
    end
end

"""
    JULIA_HOME

A string containing the full path to the directory containing the `julia` executable.
"""
:JULIA_HOME

const atexit_hooks = []

atexit(f::Function) = (unshift!(atexit_hooks, f); nothing)

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

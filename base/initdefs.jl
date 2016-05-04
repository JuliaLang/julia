# This file is a part of Julia. License is MIT: http://julialang.org/license

## initdefs.jl - initialization and runtime management definitions

PROGRAM_FILE = ""
const ARGS = String[]

exit(n) = ccall(:jl_exit, Void, (Int32,), n)
exit() = exit(0)
quit() = exit()

const roottask = current_task()

is_interactive = false
isinteractive() = (is_interactive::Bool)

const LOAD_PATH = String[]
const LOAD_CACHE_PATH = String[]
function init_load_path()
    vers = "v$(VERSION.major).$(VERSION.minor)"
    if haskey(ENV,"JULIA_LOAD_PATH")
        prepend!(LOAD_PATH, split(ENV["JULIA_LOAD_PATH"], @windows? ';' : ':'))
    end
    push!(LOAD_PATH,abspath(JULIA_HOME,"..","local","share","julia","site",vers))
    push!(LOAD_PATH,abspath(JULIA_HOME,"..","share","julia","site",vers))
    push!(LOAD_CACHE_PATH,abspath(JULIA_HOME,"..","usr","lib","julia")) #TODO: fixme
end

# initialize the local proc network address / port
function init_bind_addr()
    opts = JLOptions()
    if opts.bindto != C_NULL
        bind_to = split(bytestring(opts.bindto), ":")
        bind_addr = string(parse(IPAddr, bind_to[1]))
        if length(bind_to) > 1
            bind_port = parse(Int,bind_to[2])
        else
            bind_port = 0
        end
    else
        bind_port = 0
        try
            bind_addr = string(getipaddr())
        catch
            # All networking is unavailable, initialize bind_addr to the loopback address
            # Will cause an exception to be raised only when used.
            bind_addr = "127.0.0.1"
        end
    end
    global LPROC
    LPROC.bind_addr = bind_addr
    LPROC.bind_port = UInt16(bind_port)
end

function early_init()
    global const JULIA_HOME = ccall(:jl_get_julia_home, Any, ())
    # make sure OpenBLAS does not set CPU affinity (#1070, #9639)
    ENV["OPENBLAS_MAIN_FREE"] = get(ENV, "OPENBLAS_MAIN_FREE",
                                    get(ENV, "GOTOBLAS_MAIN_FREE", "1"))
    if CPU_CORES > 8 && !("OPENBLAS_NUM_THREADS" in keys(ENV)) && !("OMP_NUM_THREADS" in keys(ENV))
        # Prevent openblas from starting too many threads, unless/until specifically requested
        ENV["OPENBLAS_NUM_THREADS"] = 8
    end
end

function init_parallel()
    start_gc_msgs_task()
    atexit(terminate_all_workers)

    init_bind_addr()

    # start in "head node" mode, if worker, will override later.
    global PGRP
    global LPROC
    LPROC.id = 1
    assert(isempty(PGRP.workers))
    register_worker(LPROC)
end

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

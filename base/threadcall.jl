# This file is a part of Julia. License is MIT: https://julialang.org/license

const max_ccall_threads = parse(Int, get(ENV, "UV_THREADPOOL_SIZE", "4"))
const threadcall_restrictor = Semaphore(max_ccall_threads)

"""
    @threadcall((cfunc, clib), rettype, (argtypes...), argvals...)

The `@threadcall` macro is called in the same way as [`ccall`](@ref) but does the work
in a different thread. This is useful when you want to call a blocking C
function without causing the current `julia` thread to become blocked. Concurrency
is limited by size of the libuv thread pool, which defaults to 4 threads but
can be increased by setting the `UV_THREADPOOL_SIZE` environment variable and
restarting the `julia` process.

Note that the called function should never call back into Julia.
"""
macro threadcall(f, rettype, argtypes, argvals...)
    # check for usage errors
    isa(argtypes,Expr) && argtypes.head === :tuple ||
        error("threadcall: argument types must be a tuple")
    length(argtypes.args) == length(argvals) ||
        error("threadcall: wrong number of arguments to C function")

    # hygiene escape arguments
    f = esc(f)
    rettype = esc(rettype)
    argtypes = map(esc, argtypes.args)
    argvals = map(esc, argvals)

    # bind the argument values and a result cell to local variables so the
    # wrapper closure below captures native Julia objects. Closing over them
    # keeps everything the worker thread needs rooted for the duration of the
    # call, without serializing into a raw buffer or any manual GC.@preserve.
    args = [Symbol("arg", i) for i in 1:length(argvals)]
    argbinds = [:($(args[i]) = $(argvals[i])) for i in 1:length(argvals)]

    return quote
        # use cglobal to look up the function on the calling thread
        cfptr = cglobal($f)
        $(argbinds...)
        result = Ref{$rettype}()
        # closure that performs the actual call on the worker thread and stores
        # the result into the captured cell
        wrapper = () -> (result[] = ccall(cfptr, $rettype, ($(argtypes...),), $(args...)); nothing)
        do_threadcall(wrapper, result)
    end
end

# call wrapper invoked on the libuv worker thread. `F` is the concrete type of
# the work closure, so `f()` is fully devirtualized; the closure is delivered
# by value through the `Ref{F}` cfunction argument in `do_threadcall`.
function threadcall_run(f::F) where F
    f()
    return
end

# called from the libuv event loop once the queued work has finished, to wake
# up the task waiting in `do_threadcall`
function threadcall_notify(ct::Task, ctx::RefValue{Any})
    schedule(ct)
    unpreserve_handle(ct)
    unpreserve_handle(ctx)
    return
end

function do_threadcall(wrapper::F, result::Ref{T}) where {F, T}
    # a plain (non-closure) call wrapper, specialized to `F` via the `Ref{F}`
    # argument type; the closure is handed to C as an opaque context pointer
    c_run = @cfunction(threadcall_run, Cvoid, (Ref{F},))
    # function pointer used to notify us when the work is done
    c_notify_fun = @cfunction(threadcall_notify, Cvoid, (Ref{Task}, Ref{RefValue{Any}},))

    # box the closure so wrapper has a stable address to preserve
    # pass as Ptr{Cvoid} to pass that stable address
    ctx = RefValue{Any}(wrapper)

    # wait for a worker thread to be available
    acquire(threadcall_restrictor)
    try
        ct = current_task()
        # keep the waiting task and the boxed closure alive until the worker thread
        # has finished and woken us back up: the task so the notifier can find it by
        # pointer, and the closure box so its captured values survive
        preserve_handle(ct)
        preserve_handle(ctx)
        # queue up the work to be done
        ccall(:jl_queue_work, Cvoid,
            (Ptr{Cvoid}, Ptr{Cvoid}, Ptr{Cvoid}, Any, Ref{RefValue{Any}}),
            c_run, ctx, c_notify_fun, ct, ctx)

        # wait for a result
        wait()
    finally
        release(threadcall_restrictor)
    end
    return result[]
end

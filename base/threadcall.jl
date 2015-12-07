const max_ccall_threads = parse(Int, get(ENV, "UV_THREADPOOL_SIZE", "4"))
const thread_notifiers = [Nullable{Condition}() for i in 1:max_ccall_threads]
const threadcall_restrictor = Semaphore(max_ccall_threads)

function notify_fun(idx)
    global thread_notifiers
    notify(get(thread_notifiers[idx]))
    return
end

function init_threadcall()
    global const c_notify_fun = cfunction(notify_fun, Void, (Cint,))
end

"""
    @threadcall((cfunc, clib), rettype, (argtypes...), argvals...)

The `@threadcall` macro is called in the same way as `ccall` but does the work
in a different thread. This is useful when you want to call a blocking C
function without causing the main `julia` thread to become blocked. Concurrency
is limited by size of the libuv thread pool, which defaults to 4 threads but
can be increased by setting the `UV_THREADPOOL_SIZE` environment variable and
restarting the `julia` process.
"""
macro threadcall(f, rettype, argtypes, argvals...)
    f = esc(f)
    rettype = esc(rettype)
    argvals = map(esc, argvals)
    isa(argtypes,Expr) && argtypes.head == :tuple ||
        error("syntax: threadcall argument types must be a tuple; try \"(T,)\"")
    length(argtypes.args) == length(argvals) ||
        error("threadcall: wrong number of arguments to C function")

    # construct non-allocating wrapper to call C function
    wrapper = :(function wrapper(args_ptr::Ptr{Void}, retval_ptr::Ptr{Void})
        p = args_ptr
    end)
    body = wrapper.args[2].args
    args = Symbol[]
    for (i,T) in enumerate(argtypes.args)
        arg = symbol("arg$i")
        push!(body, :($arg = unsafe_load(convert(Ptr{$T}, p))))
        push!(body, :(p += sizeof($T)))
        push!(args, arg)
    end
    push!(body, :(ret = ccall($f, $rettype, ($(argtypes.args...),), $(args...))))
    push!(body, :(unsafe_store!(convert(Ptr{$rettype}, retval_ptr), ret)))
    push!(body, :(return sizeof($rettype)))

    # generate code to fill an argument buffer
    filler = Any[]
    for (T, arg) in zip(argtypes.args, argvals)
        push!(filler, :(write(buf, convert($T, $arg))))
    end

    :(let
        # generate wrapper function
        $wrapper
        fun_ptr = cfunction(wrapper, Int, (Ptr{Void}, Ptr{Void}))

        # wait for a worker thread to be available
        acquire(threadcall_restrictor)
        idx = findfirst(isnull, thread_notifiers)
        thread_notifiers[idx] = Nullable{Condition}(Condition())

        # create and fill argument and return buffers
        buf = IOBuffer()
        $(filler...)
        args_arr = takebuf_array(buf)
        ret_arr = Array(UInt8, sizeof($rettype))

        # queue up the work to be done
        ccall(:jl_queue_work, Void,
            (Ptr{Void}, Ptr{UInt8}, Ptr{UInt8}, Ptr{Void}, Cint),
            fun_ptr, args_arr, ret_arr, c_notify_fun, idx)

        # wait for a result & return it
        wait(get(thread_notifiers[idx]))
        thread_notifiers[idx] = Nullable{Condition}()
        release(threadcall_restrictor)

        unsafe_load(convert(Ptr{$rettype}, pointer(ret_arr)))
    end)
end

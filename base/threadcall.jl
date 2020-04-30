# This file is a part of Julia. License is MIT: https://julialang.org/license

const max_ccall_threads = parse(Int, get(ENV, "UV_THREADPOOL_SIZE", "4"))
const thread_notifiers = Union{Base.Condition, Nothing}[nothing for i in 1:max_ccall_threads]
const threadcall_restrictor = Semaphore(max_ccall_threads)

"""
    @threadcall((cfunc, clib), rettype, (argtypes...), argvals...)

The `@threadcall` macro is called in the same way as [`ccall`](@ref) but does the work
in a different thread. This is useful when you want to call a blocking C
function without causing the main `julia` thread to become blocked. Concurrency
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

    # construct non-allocating wrapper to call C function
    wrapper = :(function (args_ptr::Ptr{Cvoid}, retval_ptr::Ptr{Cvoid})
        p = args_ptr
        # the rest of the body is created below
    end)
    body = wrapper.args[2].args
    args = Symbol[]
    for (i, T) in enumerate(argtypes)
        arg = Symbol("arg", i)
        push!(body, :($arg = unsafe_load(convert(Ptr{$T}, p))))
        push!(body, :(p += Core.sizeof($T)))
        push!(args, arg)
    end
    push!(body, :(ret = ccall($f, $rettype, ($(argtypes...),), $(args...))))
    push!(body, :(unsafe_store!(convert(Ptr{$rettype}, retval_ptr), ret)))
    push!(body, :(return Int(Core.sizeof($rettype))))

    # return code to generate wrapper function and send work request thread queue
    wrapper = Expr(Symbol("hygienic-scope"), wrapper, @__MODULE__)
    return :(let fun_ptr = @cfunction($wrapper, Int, (Ptr{Cvoid}, Ptr{Cvoid}))
        do_threadcall(fun_ptr, $rettype, Any[$(argtypes...)], Any[$(argvals...)])
    end)
end

function do_threadcall(fun_ptr::Ptr{Cvoid}, rettype::Type, argtypes::Vector, argvals::Vector)
    # generate function pointer
    c_notify_fun = @cfunction(
        function notify_fun(idx)
            global thread_notifiers
            notify(thread_notifiers[idx])
            return
        end, Cvoid, (Cint,))

    # cconvert, root and unsafe_convert arguments
    roots = Any[]
    args_size = isempty(argtypes) ? 0 : sum(Core.sizeof, argtypes)
    args_arr = Vector{UInt8}(undef, args_size)
    ptr = pointer(args_arr)
    for (T, x) in zip(argtypes, argvals)
        isbitstype(T) || throw(ArgumentError("threadcall requires isbits argument types"))
        y = cconvert(T, x)
        push!(roots, y)
        unsafe_store!(convert(Ptr{T}, ptr), unsafe_convert(T, y)::T)
        ptr += Core.sizeof(T)
    end

    # create return buffer
    ret_arr = Vector{UInt8}(undef, Core.sizeof(rettype))

    # wait for a worker thread to be available
    acquire(threadcall_restrictor)
    idx = findfirst(isequal(nothing), thread_notifiers)::Int
    thread_notifiers[idx] = Base.Condition()

    GC.@preserve args_arr ret_arr roots begin
        # queue up the work to be done
        ccall(:jl_queue_work, Cvoid,
            (Ptr{Cvoid}, Ptr{UInt8}, Ptr{UInt8}, Ptr{Cvoid}, Cint),
            fun_ptr, args_arr, ret_arr, c_notify_fun, idx)

        # wait for a result & return it
        wait(thread_notifiers[idx])
        thread_notifiers[idx] = nothing
        release(threadcall_restrictor)

        r = unsafe_load(convert(Ptr{rettype}, pointer(ret_arr)))
    end
    return r
end

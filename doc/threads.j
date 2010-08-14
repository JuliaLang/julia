# concurrency sketches

struct Future{T} <: Wraps{T}
    value::Ref{Nullable{T}}
    where::Thread
    ....
end

function unwrap{T}(x::Future{T})
    if (is(x.value,()))
        v = wait_for(x.where)
        x.value = v
    end
    return x.value
end

# where the Wraps type defines (perhaps implicitly)

(+)(x::Wraps, y::Wraps) = wrap(unwrap(x)+unwrap(y), x, y)

# OR there could be a version that makes a closure for lazy evaluation:

(+)(x::WrapsLazily, y::WrapsLazily) = wrap(()->(unwrap(x)+unwrap(y)), x, y)

# Then we can write

wrap(thunk, inputs::Future...) = Future((), spawn_thread(thunk))


# symmetric coroutine operations:
# coroutine(f[, stacksize]) - create a coroutine from a function
# v = yieldto(c, args...) - switch to a coroutine
# done() - top function of coroutine has exited

# implementing generator-like asymmetric coroutines:
generator_stack = ()  # implicit return points for yielding. thread-local.
next(g,v) = (generator_stack=(current_coroutine(), generator_stack);
             yieldto(g, v))
yield(v) = (caller=generator_stack[1];
            generator_stack=generator_stack[2];
            yieldto(caller, v))

function producer(x)
    ...
    yield(val)
    ...
end

function consumer()
    G = coroutine(producer)
    while !done(G)
        nxt = next(G, 0)
    end
end

# function preamble for dealing with limited stack space:
function anyfunc(args...)
    if (almost-out-of-stack)
        C = coroutine(anyfunc)
        # when C exits it will yield to us
        set_exit_coroutine(C, current_coroutine())
        # if somebody yields to us, yield to C instead
        forward_coroutine(current_coroutine(), C)
        return yieldto(C, args...)
    end
    ...
end

function spawn(thunk)
    c = coroutine(thunk)
    make_runnable(c)
    # return a thunk that forces the coroutine to finish.
    return ()->(while !done(c); yieldto(c); end; yieldto(c))
end

# pick and run 1 task until it yields
function schedule()
    t = pick_task(runQ)
    yieldto(t)
    if !done(t)
        make_runnable(t)
    end
end

scheduler = coroutine(()->while true; schedule(); end)

function read(stream)
    token = asynch_read(stream)
    make_waiting(current_coroutine(), token)
    yieldto(scheduler)
    return io_result(token)
end

# implementing try/catch exception handling

before
try
    try_block
catch
    catch_block
end
after

|
V

before
C = current_coroutine()
prev = current_exception_handler()
eh = coroutine(()->(current_exception_handler(prev);
                    catch_block;
                    yieldto(C)))
current_exception_handler(eh)
totry = coroutine(()->try_block)
yieldto(totry)
current_exception_handler(prev)
after

# can be optimized to:

before
C = current_coroutine()
eh = copy_coroutine(current_coroutine())
prev = current_exception_handler()
current_exception_handler(eh)
if (!setjmp(eh._ctxt))
  try_block
else
  current_exception_handler(prev)
  mark_done(eh)  # make sure nobody yields to this task again
  current_coroutine(C)  # make it look like C is running
  catch_block
end
current_exception_handler(prev)
after

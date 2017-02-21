# This file is a part of Julia. License is MIT: http://julialang.org/license

let nextidx = 0
    global nextproc
    function nextproc()
        p = -1
        if p == -1
            p = workers()[(nextidx % nworkers()) + 1]
            nextidx += 1
        end
        p
    end
end

spawnat(p, thunk) = sync_add(remotecall(thunk, p))

spawn_somewhere(thunk) = spawnat(nextproc(),thunk)

macro spawn(expr)
    thunk = esc(:(()->($expr)))
    :(spawn_somewhere($thunk))
end

macro spawnat(p, expr)
    thunk = esc(:(()->($expr)))
    :(spawnat($(esc(p)), $thunk))
end

"""
    @fetch

Equivalent to `fetch(@spawn expr)`.
See [`fetch`](@ref) and [`@spawn`](@ref).
"""
macro fetch(expr)
    thunk = esc(:(()->($expr)))
    :(remotecall_fetch($thunk, nextproc()))
end

"""
    @fetchfrom

Equivalent to `fetch(@spawnat p expr)`.
See [`fetch`](@ref) and [`@spawnat`](@ref).
"""
macro fetchfrom(p, expr)
    thunk = esc(:(()->($expr)))
    :(remotecall_fetch($thunk, $(esc(p))))
end

"""
    @everywhere expr

Execute an expression under `Main` everywhere. Equivalent to calling
`eval(Main, expr)` on all processes. Errors on any of the processes are collected into a
`CompositeException` and thrown. For example :

    @everywhere bar=1

will define `Main.bar` on all processes.

Unlike [`@spawn`](@ref) and [`@spawnat`](@ref),
`@everywhere` does not capture any local variables. Prefixing
`@everywhere` with [`@eval`](@ref) allows us to broadcast
local variables using interpolation :

    foo = 1
    @eval @everywhere bar=\$foo

The expression is evaluated under `Main` irrespective of where `@everywhere` is called from.
For example :

    module FooBar
        foo() = @everywhere bar()=myid()
    end
    FooBar.foo()

will result in `Main.bar` being defined on all processes and not `FooBar.bar`.
"""
macro everywhere(ex)
    quote
        sync_begin()
        for pid in workers()
            async_run_thunk(()->remotecall_fetch(eval_ew_expr, pid, $(Expr(:quote,ex))))
            yield() # ensure that the remotecall_fetch has been started
        end

        # execute locally last as we do not want local execution to block serialization
        # of the request to remote nodes.
        if nprocs() > 1
            async_run_thunk(()->eval_ew_expr($(Expr(:quote,ex))))
        end

        sync_end()
    end
end

eval_ew_expr(ex) = (eval(Main, ex); nothing)

# Statically split range [1,N] into equal sized chunks for np processors
function splitrange(N::Int, np::Int)
    each = div(N,np)
    extras = rem(N,np)
    nchunks = each > 0 ? np : extras
    chunks = Array{UnitRange{Int}}(nchunks)
    lo = 1
    for i in 1:nchunks
        hi = lo + each - 1
        if extras > 0
            hi += 1
            extras -= 1
        end
        chunks[i] = lo:hi
        lo = hi+1
    end
    return chunks
end

function preduce(reducer, f, R)
    N = length(R)
    chunks = splitrange(N, nworkers())
    all_w = workers()[1:length(chunks)]

    w_exec = Task[]
    for (idx,pid) in enumerate(all_w)
        t = Task(()->remotecall_fetch(f, pid, reducer, R, first(chunks[idx]), last(chunks[idx])))
        schedule(t)
        push!(w_exec, t)
    end
    reduce(reducer, [wait(t) for t in w_exec])
end

function pfor(f, R)
    [@spawn f(R, first(c), last(c)) for c in splitrange(length(R), nworkers())]
end

function make_preduce_body(var, body)
    quote
        function (reducer, R, lo::Int, hi::Int)
            $(esc(var)) = R[lo]
            ac = $(esc(body))
            if lo != hi
                for $(esc(var)) in R[(lo+1):hi]
                    ac = reducer(ac, $(esc(body)))
                end
            end
            ac
        end
    end
end

function make_pfor_body(var, body)
    quote
        function (R, lo::Int, hi::Int)
            for $(esc(var)) in R[lo:hi]
                $(esc(body))
            end
        end
    end
end

"""
    @parallel

A parallel for loop of the form :

    @parallel [reducer] for var = range
        body
    end

The specified range is partitioned and locally executed across all workers. In case an
optional reducer function is specified, `@parallel` performs local reductions on each worker
with a final reduction on the calling process.

Note that without a reducer function, `@parallel` executes asynchronously, i.e. it spawns
independent tasks on all available workers and returns immediately without waiting for
completion. To wait for completion, prefix the call with [`@sync`](@ref), like :

    @sync @parallel for var = range
        body
    end
"""
macro parallel(args...)
    na = length(args)
    if na==1
        loop = args[1]
    elseif na==2
        reducer = args[1]
        loop = args[2]
    else
        throw(ArgumentError("wrong number of arguments to @parallel"))
    end
    if !isa(loop,Expr) || loop.head !== :for
        error("malformed @parallel loop")
    end
    var = loop.args[1].args[1]
    r = loop.args[1].args[2]
    body = loop.args[2]
    if na==1
        thecall = :(pfor($(make_pfor_body(var, body)), $(esc(r))))
    else
        thecall = :(preduce($(esc(reducer)), $(make_preduce_body(var, body)), $(esc(r))))
    end
    thecall
end


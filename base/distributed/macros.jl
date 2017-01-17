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
function splitrange(N::Int, wlist::Array)
    np = length(wlist)
    each = div(N,np)
    extras = rem(N,np)
    nchunks = each > 0 ? np : extras
    chunks = Dict{Int, UnitRange{Int}}()
    lo = 1
    for i in 1:nchunks
        hi = lo + each - 1
        if extras > 0
            hi += 1
            extras -= 1
        end
        chunks[wlist[i]] = lo:hi
        lo = hi+1
    end
    return chunks
end

"""
    DistributedRef(initial)

Constructs a reducing accumulator designed to be used in conjunction with `@parallel`
for-loops. Takes a single argument specifying an initial value.

The body of the `@parallel` for-loop can refer to multiple `DistributedRef`s.
The 0-arg indexation syntax, `[]` is used to fetch from and assign to an accumulator in the loop body.
For example `acc[] = acc[] + i` will fetch the current, locally accumulated value in `acc`, add
the value of `i` and set the new accumulated value back in `acc`.

See [`@parallel`](@ref) for more details.

DistributedRefs can also be used independent of `@parallel` loops.

```julia
acc = DistributedRef(1)
@sync for p in workers()
    @spawnat p begin
        for i in 1:10
            acc[] += 1       # Local accumulation on each worker
        end
        push!(acc)           # Explicit push of local accumulation to driver node (typically node 1)
    end
end
reduce(+, acc)
```

Usage of DistributedRefs independent of a `@parallel` construct must observe the following:
- All remote tasks must be completed before calling `reduce` to retrieve the accumulated value. In the
  example above, this is achieved by [`@sync`](@ref).
- `push!(acc)` must be explictly called once on each worker. This pushes the locally accumulated value
  to the node driving the computation.

Note that the optional `initial` value is used on all workers. For example, if the reducing function is `+`,
`DistributedRef(25)` will add a total of `25*nworkers()` to the final result.
"""
mutable struct DistributedRef{T}
    initial::T
    value::T
    workers::Set{Int}       # Used on caller to detect arrival of all parts
    chnl::RemoteChannel
    hval::Int               # change hash value to ensure globals are serialized everytime

    DistributedRef{T}(initial, chnl) where T = new(initial, initial, Set{Int}(), chnl, 0)
end

DistributedRef{T}() where T  = DistributedRef{T}(zero(T))
DistributedRef{T}(initial::T) = DistributedRef{T}(initial, RemoteChannel(()->Channel{Tuple}(Inf)))

const dref_registry=Dict{RRID, Array{DistributedRef}}()

getindex(dref::DistributedRef) = dref.value
setindex!(dref::DistributedRef, v) = (dref.value = v)

hash(dref::DistributedRef, h::UInt) = hash(dref.hval, hash(dref.chnl, h))


"""
    push!(dref::DistributedRef)

Pushes the locally accumulated value to the calling node. Must be called once on each worker
when a DistributedRef is used independent of a [`@parallel`](@ref) construct.
"""
push!(dref::DistributedRef) = put_nowait!(dref.chnl, (myid(), dref.value))

function serialize(s::AbstractSerializer, dref::DistributedRef)
    serialize_type(s, typeof(dref))

    serialize(s, dref.initial)
    serialize(s, dref.chnl)
    rrid = get(task_local_storage(), :JULIA_PACC_TRACKER, ())
    serialize(s, rrid)

    destpid = worker_id_from_socket(s.io)
    push!(dref.workers, destpid)
    nothing
end

function deserialize(s::AbstractSerializer, t::Type{T}) where T <: DistributedRef
    initial = deserialize(s)
    chnl = deserialize(s)
    rrid = deserialize(s)

    dref = T(initial, chnl)

    global dref_registry
    rrid != () && push!(get!(dref_registry, rrid, []), dref)
    dref
end


"""
    reduce(op, dref::DistributedRef)

Performs a final reduction on the calling node of values accumulated by
a [`@parallel`](@ref) invocation and returns the reduced value.
"""
reduce(op, dref::DistributedRef) = reduce(op, dref.initial, dref)

function reduce{T}(op, v0, dref::DistributedRef{T})
    length(dref.workers) == 0 && return dref.value  # local execution, no workers present

    results = T[]
    while length(dref.workers) > 0
        (pid, v) = take!(dref.chnl)
        @assert pid in dref.workers
        delete!(dref.workers, pid)
        push!(results, v)
    end
    dref.hval += 1
    dref.value = reduce(op, v0, results)
    dref.value
end

"""
    clear!(dref::DistributedRef)

Clears a DistributedRef object enabling its reuse in a subsequent call.
"""
function clear!(dref::DistributedRef)
    dref.value = dref.initial
    dref.hval += 1
    dref
end

function preduce(reducer, f, R)
    w_exec = Task[]
    for (pid, r) in splitrange(length(R), workers())
        t = @schedule remotecall_fetch(f, pid, reducer, R, first(r), last(r))
        push!(w_exec, t)
    end
    reduce(reducer, [wait(t) for t in w_exec])
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

function pfor(f, R)
    lenR = length(R)
    chunks = splitrange(lenR, workers())
    rrid = RRID()
    task_local_storage(:JULIA_PACC_TRACKER, rrid)
    [remotecall(f, p, R, first(c), last(c), rrid) for (p,c) in chunks]
end

function make_pfor_body(var, body)
    quote
        function (R, lo::Int, hi::Int, rrid)
            global dref_registry
            dref_list = get(dref_registry, rrid, DistributedRef[])
            delete!(dref_registry, rrid)
            try
                for $(esc(var)) in R[lo:hi]
                    $(esc(body))
                end
            catch e
                for p2 in dref_list
                    put_nowait!(p2.chnl, (0, p2.initial))
                end
                rethrow(e)
            end
            for p2 in dref_list
                put_nowait!(p2.chnl, (myid(), p2.value))
            end
        end
    end
end

"""
    @parallel

A parallel for loop of the form :

    @parallel for var = range
        body
    end

The loop is executed in parallel across all workers, with each worker executing a subset
of the range. The call waits for completion of all iterations on all workers before returning.
Any updates to variables outside the loop body are not reflected on the calling node.
However, this is a common requirement and can be achieved in a couple of ways. One, the loop body
can update shared arrays, wherein the updates are visible on all nodes mapping the array. Second,
[`DistributedRef`](@ref) objects can be used to collect computed values efficiently.
The former can be used only on a single node (with multiple workers mapping the same shared segment), while
the latter can be used when a computation is distributed across nodes.

```jldoctest
julia> a = SharedArray{Float64}(4);

julia> c = 10;

julia> @parallel for i=1:4
           a[i] = i + c
       end

julia> a
4-element SharedArray{Float64,1}:
 11.0
 12.0
 13.0
 14.0
```

```jldoctest
julia> acc = DistributedRef(0);

julia> c = 100;

julia> @parallel for i in 1:10
           j = 2i + c
           acc[] += j
       end;

julia> reduce(+, acc)
1110
```
"""
macro parallel(args...)
    na = length(args)
    if na == 1
        loop = args[1]
    elseif na == 2
        depwarn("@parallel with a reducer is deprecated. Use DistributedRefs for reduction.", Symbol("@parallel"))
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
    if na == 1
        thecall = :(foreach(wait, pfor($(make_pfor_body(var, body)), $(esc(r)))))
    else
        thecall = :(preduce($(esc(reducer)), $(make_preduce_body(var, body)), $(esc(r))))
    end
    thecall
end

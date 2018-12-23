# This file is a part of Julia. License is MIT: https://julialang.org/license

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

spawnat(p, thunk) = remotecall(thunk, p)

spawn_somewhere(thunk) = spawnat(nextproc(),thunk)

"""
    @spawn

Create a closure around an expression and run it on an automatically-chosen process,
returning a [`Future`](@ref) to the result.

# Examples
```julia-repl
julia> addprocs(3);

julia> f = @spawn myid()
Future(2, 1, 5, nothing)

julia> fetch(f)
2

julia> f = @spawn myid()
Future(3, 1, 7, nothing)

julia> fetch(f)
3
```
"""
macro spawn(expr)
    thunk = esc(:(()->($expr)))
    var = esc(Base.sync_varname)
    quote
        local ref = spawn_somewhere($thunk)
        if $(Expr(:isdefined, var))
            push!($var, ref)
        end
        ref
    end
end

"""
    @spawnat

Create a closure around an expression and run the closure
asynchronously on process `p`. Return a [`Future`](@ref) to the result.
Accepts two arguments, `p` and an expression.

# Examples
```julia-repl
julia> addprocs(1);

julia> f = @spawnat 2 myid()
Future(2, 1, 3, nothing)

julia> fetch(f)
2
```
"""
macro spawnat(p, expr)
    thunk = esc(:(()->($expr)))
    var = esc(Base.sync_varname)
    quote
        local ref = spawnat($(esc(p)), $thunk)
        if $(Expr(:isdefined, var))
            push!($var, ref)
        end
        ref
    end
end

"""
    @fetch

Equivalent to `fetch(@spawn expr)`.
See [`fetch`](@ref) and [`@spawn`](@ref).

# Examples
```julia-repl
julia> addprocs(3);

julia> @fetch myid()
2

julia> @fetch myid()
3

julia> @fetch myid()
4

julia> @fetch myid()
2
```
"""
macro fetch(expr)
    thunk = esc(:(()->($expr)))
    :(remotecall_fetch($thunk, nextproc()))
end

"""
    @fetchfrom

Equivalent to `fetch(@spawnat p expr)`.
See [`fetch`](@ref) and [`@spawnat`](@ref).

# Examples
```julia-repl
julia> addprocs(3);

julia> @fetchfrom 2 myid()
2

julia> @fetchfrom 4 myid()
4
```
"""
macro fetchfrom(p, expr)
    thunk = esc(:(()->($expr)))
    :(remotecall_fetch($thunk, $(esc(p))))
end

# extract a list of modules to import from an expression
extract_imports!(imports, x) = imports
function extract_imports!(imports, ex::Expr)
    if Meta.isexpr(ex, (:import, :using))
        m = ex.args[1]
        if isa(m, Expr) && m.head === :(:)
            push!(imports, m.args[1].args[1])
        else
            for a in ex.args
                push!(imports, a.args[1])
            end
        end
    elseif Meta.isexpr(ex, :let)
        extract_imports!(imports, ex.args[2])
    elseif Meta.isexpr(ex, (:toplevel, :block))
        for i in eachindex(ex.args)
            extract_imports!(imports, ex.args[i])
        end
    end
    return imports
end
extract_imports(x) = extract_imports!(Symbol[], x)

"""
    @everywhere [procs()] expr

Execute an expression under `Main` on all `procs`.
Errors on any of the processes are collected into a
`CompositeException` and thrown. For example:

    @everywhere bar = 1

will define `Main.bar` on all processes.

Unlike [`@spawn`](@ref) and [`@spawnat`](@ref),
`@everywhere` does not capture any local variables.
Instead, local variables can be broadcast using interpolation:

    foo = 1
    @everywhere bar = \$foo

The optional argument `procs` allows specifying a subset of all
processes to have execute the expression.

Equivalent to calling `remotecall_eval(Main, procs, expr)`.
"""
macro everywhere(ex)
    procs = GlobalRef(@__MODULE__, :procs)
    return esc(:(@everywhere $procs() $ex))
end

macro everywhere(procs, ex)
    imps = [Expr(:import, m) for m in extract_imports(ex)]
    return quote
        $(isempty(imps) ? nothing : Expr(:toplevel, imps...)) # run imports locally first
        let ex = $(Expr(:quote, ex)), procs = $(esc(procs))
            remotecall_eval(Main, procs, ex)
        end
    end
end

"""
    remotecall_eval(m::Module, procs, expression)

Execute an expression under module `m` on the processes
specified in `procs`.
Errors on any of the processes are collected into a
`CompositeException` and thrown.

See also `@everywhere`.
"""
function remotecall_eval(m::Module, procs, ex)
    @sync begin
        run_locally = 0
        for pid in procs
            if pid == myid()
                run_locally += 1
            else
                @async remotecall_wait(Core.eval, pid, m, ex)
            end
        end
        yield() # ensure that the remotecall_fetch have had a chance to start

        # execute locally last as we do not want local execution to block serialization
        # of the request to remote nodes.
        for _ in 1:run_locally
            @async Core.eval(m, ex)
        end
    end
    nothing
end

# optimized version of remotecall_eval for a single pid
# and which also fetches the return value
function remotecall_eval(m::Module, pid::Int, ex)
    return remotecall_fetch(Core.eval, pid, m, ex)
end


# Statically split range [1,N] into equal sized chunks for np processors
function splitrange(N::Int, np::Int)
    each = div(N,np)
    extras = rem(N,np)
    nchunks = each > 0 ? np : extras
    chunks = Vector{UnitRange{Int}}(undef, nchunks)
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
    chunks = splitrange(Int(N), nworkers())
    all_w = workers()[1:length(chunks)]

    w_exec = Task[]
    for (idx,pid) in enumerate(all_w)
        t = Task(()->remotecall_fetch(f, pid, reducer, R, first(chunks[idx]), last(chunks[idx])))
        schedule(t)
        push!(w_exec, t)
    end
    reduce(reducer, [fetch(t) for t in w_exec])
end

function pfor(f, R)
    @async @sync for c in splitrange(length(R), nworkers())
        @spawn f(R, first(c), last(c))
    end
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
    @distributed

A distributed memory, parallel for loop of the form :

    @distributed [reducer] for var = range
        body
    end

The specified range is partitioned and locally executed across all workers. In case an
optional reducer function is specified, `@distributed` performs local reductions on each worker
with a final reduction on the calling process.

Note that without a reducer function, `@distributed` executes asynchronously, i.e. it spawns
independent tasks on all available workers and returns immediately without waiting for
completion. To wait for completion, prefix the call with [`@sync`](@ref), like :

    @sync @distributed for var = range
        body
    end
"""
macro distributed(args...)
    na = length(args)
    if na==1
        loop = args[1]
    elseif na==2
        reducer = args[1]
        loop = args[2]
    else
        throw(ArgumentError("wrong number of arguments to @distributed"))
    end
    if !isa(loop,Expr) || loop.head !== :for
        error("malformed @distributed loop")
    end
    var = loop.args[1].args[1]
    r = loop.args[1].args[2]
    body = loop.args[2]
    if na==1
        syncvar = esc(Base.sync_varname)
        return quote
            local ref = pfor($(make_pfor_body(var, body)), $(esc(r)))
            if $(Expr(:isdefined, syncvar))
                push!($syncvar, ref)
            end
            ref
        end
    else
        return :(preduce($(esc(reducer)), $(make_preduce_body(var, body)), $(esc(r))))
    end
end

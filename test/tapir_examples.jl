# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base.Experimental: Tapir

function fib(N)
    if N <= 1
        return N
    end
    Tapir.@sync begin
        Tapir.@spawn $x1 = fib(N - 1)
        $x2 = fib(N - 2)
    end
    return x1 + x2
end

@noinline fib_noinline_wrap(N) = fib(N)
@noinline fib1() = fib(1)
@noinline fib2() = fib(2)
@noinline fib3() = fib(3)
@noinline fib10() = fib(10)

module ReturnViaRef
using Base.Experimental: Tapir

@noinline produce() = P
P = 1

@noinline function store!(d, x)
    d[] = x
    return
end

function f()
    a = Ref{Any}()
    local b
    Tapir.@sync begin
        Tapir.@spawn begin
            store!(a, produce())
        end
        b = produce()
    end
    return (a[], b)
end

function g()
    a = Ref{Any}()
    local b
    Tapir.@sync begin
        Tapir.@spawn begin
            a[] = produce()
        end
        b = produce()
    end
    return (a[], b)
end
end # module ReturnViaRef


module DecayedPointers
using Base.Experimental: Tapir

mutable struct M2{T}
    x::Int
    y::T
end

@noinline function change!(m)
    m.y = 0
    return
end

function f()
    a = M2(1, M2(2, 3))
    b = M2(4, 5)
    Tapir.@sync begin
        Tapir.@spawn begin
            change!(a.y)
        end
        change!(b)
    end
    return (a, b)
end
end # module DecayedPointers


module SyncInLoop
using Base.Experimental: Tapir

@noinline consume(x) = (global G = x; nothing)

function body0()
    x = (Ref(1), Ref(2))
    Tapir.@sync begin
        Tapir.@spawn consume(x)
        consume(x)
    end
end

function loop0(n)
    for _ in 1:n
        body0()
    end
end
end # module SyncInLoop


module NestedAggregates
using Base.Experimental: Tapir

@noinline oneone() = [1]
@noinline twotwooneone() = ((oneone(), oneone()), (oneone(), oneone()))

function f()
    a = Ref{Any}()
    local b
    Tapir.@sync begin
        Tapir.@spawn begin
            a[] = twotwooneone()
        end
        b = twotwooneone()
    end
    return (a[], b)
end
end


module TaskOutputs
using Base.Experimental: Tapir

@noinline produce() = P::Int
P = 1

@noinline produce(x) = Base.inferencebarrier(x)::typeof(x)

""" A simple example with less macro tricks. """
function simple()
    Tapir.@output a b
    token = Tapir.@syncregion
    Tapir.@spawnin token begin
        a = produce()
    end
    b = produce()
    Tapir.@sync_end token
    a + b
end

call(f) = f()

function simple_closure_set_by_one(flag)
    Tapir.@output slot
    token = Tapir.@syncregion
    Tapir.@spawnin token begin
        call() do
            if flag
                slot = produce(111)
            end
        end
    end
    call() do
        if !flag
            slot = produce(222)
        end
    end
    Tapir.@sync_end token
    out = slot
    return out
end

function simple_conditional_task_output(x::Bool)
    Tapir.@output slot
    token = Tapir.@syncregion
    Tapir.@spawnin token if x
        slot = produce(111)
    end
    b = produce(222)
    Tapir.@sync_end token
    if @isdefined(slot)
        a = slot
    end
    return b + (x ? a : 0)
end

function f()
    v = 'a'
    Tapir.@sync begin
        Tapir.@spawn begin
            $a = (v, produce())
        end
        $b = produce()
    end
    return (a, b)
end

@noinline id(x) = rand() > 1 ? error("unreachable") : x

function set_distinct(bool::Bool)
    Tapir.@sync begin
        Tapir.@spawn if bool
            $x = id(1)
        else
            $y = id(2)
        end
        if bool
            $y = id(3)
        else
            $x = id(4)
        end
    end
    return x + y
end

function set_distinct_optimizable(bool::Bool)
    Tapir.@sync begin
        Tapir.@spawn if bool
            $x = 1
        else
            $y = 2
        end
        if bool
            $y = 3
        else
            $x = 4
        end
    end
    return x + y
end

function update_distinct(bool::Bool)
    x = 0
    y = 0
    Tapir.@sync begin
        Tapir.@spawn if bool
            $x = id($x + 1)
        else
            $y = id($y + 2)
        end
        if bool
            $y = id(y + 3)
        else
            $x = id(x + 4)
        end
    end
    return x + y
end

function local_update_after_store(n)
    Tapir.@sync begin
        Tapir.@spawn begin
            acc = 0
            for x in 1:n
                acc += x
            end
            $acc = acc
            acc *= 10  # should be ignored
        end
        produce()
    end
    return acc
end

function conditional_output(x)
    Tapir.@sync begin
        Tapir.@spawn begin
            a = produce()
            if x
                $a = a
            end
        end
        $b = produce()
    end
    return a + b
end

function independent_increments()
    a = 0
    b = 0
    Tapir.@output a b
    Tapir.@sync begin
        Tapir.@spawn a += produce(111)
        b += produce(222)
    end
    return a + b
end

function aggregate()
    Tapir.@output a b
    Tapir.@sync begin
        Tapir.@spawn a = produce((1, (2, (3, 4))))
        b = produce((x = (y = (z = (5, 6, 7),),), w = 8))
    end
    return a[2][2][2] + b.x.y.z[1]
end

function identity2(x, y)
    Tapir.@output a b
    Tapir.@sync begin
        Tapir.@spawn a = produce(x)
        b = produce(y)
    end
    return (a, b)
end

end # module TaskOutputs

module Racy
using Base.Experimental: Tapir

function update_distinct(bool::Bool)
    x = 0
    y = 0
    Tapir.@sync begin
        Tapir.@spawn if bool
            x += 1
        else
            y += 2
        end
        if bool
            y += 3
        else
            x += 4
        end
    end
    return x + y
end

function simple_race()
    a = 0
    Tapir.@sync begin
        Tapir.@spawn a += 1
        a += 2
    end
    a
end

end # module Racy

module SROA
using Base.Experimental: Tapir

mutable struct AB
    a::Int
    b::Int
end

@inline function sumto!(r, p, xs)
    for x in xs
        setproperty!(r, p, getproperty(r, p) + x)
    end
    return getproperty(r, p)
end

# Field access inside `@spawn` is not SROA'ed since it'd introduce a phi node in
# the continuation.
function demo_sroa_half()
    ab = AB(0, 0)
    Tapir.@sync begin
        Tapir.@spawn sumto!(ab, :a, 1:2:10)
        sumto!(ab, :b, 2:2:10)
    end
    return ab.a + ab.b
end

# All field access inside `@spawn` should be SROA'ed since there is no field
# access after sync.
function demo_sroa()
    ab = AB(0, 0)
    Tapir.@sync begin
        Tapir.@spawn $a = sumto!(ab, :a, 1:2:10)
        $b = sumto!(ab, :b, 2:2:10)
    end
    return a + b
end
end

module AdHocLoop
using Base.Experimental: Tapir
macro par(expr)
    body = expr.args[2]
    lhs = expr.args[1].args[1]
    range = expr.args[1].args[2]
    @gensym chunk
    quote
        $Tapir.@sync for $chunk in $Iterators.partition($(range), $Threads.nthreads())
            $Tapir.@spawn for $lhs in $chunk
                $body
            end
        end
    end |> esc
end

function tmap!(f, ys, xs)
    @par for i in eachindex(ys, xs)
        ys[i] = f(xs[i])
    end
    return ys
end

f() = tmap!(identity, zeros(10), 1:10)
end # module AdHocLoop

module NestedSpawns
using Base.Experimental: Tapir

@noinline inc(x) = Base.inferencebarrier(x + 1)::typeof(x)

function f()
    Tapir.@sync begin
        Tapir.@spawn begin
            x = inc(1)
            Tapir.@spawn begin
                $a = inc(x)
            end
            $b = x + 2
        end
        $c = inc(3)
    end
    return a + b + c
end

end # module NestedSpawns

function mapfold(f, op, xs; basesize = cld(length(xs), Threads.nthreads()))
    basesize = max(3, basesize)  # for length(left) + length(right) >= 4
    if length(xs) < basesize
        return mapfoldl(f, op, xs)
    end
    return _mapfold(f, op, xs, basesize)
end

function _mapfold(f, op, xs, basesize)
    if length(xs) <= basesize
        acc = @inbounds op(f(xs[begin]), f(xs[begin+1]))
        for i in eachindex(xs)[3:end]
            acc = op(acc, f(@inbounds xs[i]))
        end
        return acc
    else
        left = @inbounds @view xs[begin:(end-begin+1)÷2]
        right = @inbounds @view xs[(end-begin+1)÷2+1:end]
        Tapir.@sync begin
            Tapir.@spawn $z = _mapfold(f, op, right, basesize)
            $y = _mapfold(f, op, left, basesize)
        end
        return op(y, z)
    end
end

function append!!(a, b)
    ys::Vector = a isa Vector ? a : collect(a)
    if eltype(b) <: eltype(ys)
        zs = append!(ys, b)
    else
        zs = similar(ys, promote_type(eltype(ys), eltype(b)), (length(ys) + length(b)))
        copyto!(zs, 1, ys, 1, length(ys))
        zs[length(ys)+1:end] .= b
    end
    return zs
end

function tmap(f, xs; kw...)
    ys = mapfold(tuple ∘ f, append!!, xs; kw...)
    if ys isa Tuple
        return collect(ys)
    else
        return ys
    end
end

# This triggered a bug in inference:
tak(x, y, z) =
    if y < x
        a = Ref(0)
        b = Ref(0)
        local c
        Tapir.@sync begin
            Tapir.@spawn a[] = tak(x - 1, y, z)
            Tapir.@spawn b[] = tak(y - 1, z, x)
            c = tak(z - 1, x, y)
        end
        tak(a[], b[], c)
    else
        z
    end

module CapturedToken
using Base.Experimental: Tapir

@noinline produce(x) = Base.inferencebarrier(x)::typeof(x)

""" Immediately-invoked Function Expression (IIFE) is OK. """
function iife()
    Tapir.@output a b
    Tapir.@sync begin
        function closure()
            Tapir.@spawn a = produce(111)
        end
        closure()
        b = produce(222)
    end
    return a + b
end

""" Some trivial IIFE can be optimized out. """
function iife_optimizable()
    Tapir.@output a b
    Tapir.@sync begin
        function closure()
            Tapir.@spawn a = 111
        end
        closure()
        b = produce(222)
    end
    return a + b
end

"""
    escaped_spawn() -> closure

Return an "invalid" `closure` that throws as it captures the syncregion token.
"""
function escaped_spawn()
    local closure
    Tapir.@sync begin
        closure = function ()
            Tapir.@spawn produce(111)
        end
        produce(222)
    end
    return closure
end

"""
    invoke_escaped_spawn()::Union{}

Invoke a closure that captures syncregion after sync, which should be disallowed.
This tests that the copmiler can handle this even when the closure is inlined.
"""
function invoke_escaped_spawn()
    token = Tapir.@syncregion
    function closure()
        Tapir.@spawnin token produce(111)
    end
    produce(222)
    Tapir.@sync_end token
    closure()
    return
end

end # module CapturedToken

module OptimizableTasks
using Base.Experimental: Tapir

@noinline produce() = P::Int
P = 0

function trivial_detach(x, y)
    Tapir.@sync begin
        Tapir.@spawn $a = x + y
        $b = produce()
    end
    return a + b
end

function trivial_continuation(x, y)
    Tapir.@sync begin
        Tapir.@spawn $a = produce()
        $b = x + y
    end
    return a + b
end

function trivial_spawn_in_continuation()
    Tapir.@sync begin
        Tapir.@spawn produce()
        Tapir.@spawn :trivial
        :trivial
    end
end

function always_throw()
    Tapir.@sync begin
        Tapir.@spawn begin
            produce()
            throw(KeyError(0))
            produce()
        end
        produce()
    end
end
end # module OptimizableTasks

module NonOptimizableTasks
using Base.Experimental: Tapir

@noinline consume(x) = (global SINK = x; nothing)

function loop_in_spawn(n)
    Tapir.@sync begin
        Tapir.@spawn begin
            acc = 0
            for x in 1:n
                acc += x
            end
            $y = acc
        end
        consume(nothing)
    end
    return y
end

function loop_in_continuation(n)
    Tapir.@sync begin
        Tapir.@spawn consume(nothing)
        acc = 0
        for x in 1:n
            acc += x
        end
        $y = acc
    end
    return y
end

function spawn_in_loop()
    Tapir.@sync for i in 1:3
        Tapir.@spawn consume(i)
    end
end

function dontoptimize_dontoptimize()
    Tapir.@sync begin
        Tapir.@spawn Tapir.dontoptimize()
        Tapir.dontoptimize()
    end
    return
end

end # module NonOptimizableTasks

module TaskGroupOptimizations
using Base.Experimental: Tapir

@noinline produce(x) = Base.inferencebarrier(x)::typeof(x)

function two_root_spawns()
    Tapir.@sync begin
        Tapir.@spawn produce(111)
        Tapir.@spawn produce(222)
        produce(333)
    end
end

function nested_syncs()
    Tapir.@sync begin
        Tapir.@spawn produce(111)
        Tapir.@spawn produce(222)
        Tapir.@sync begin
            Tapir.@spawn produce(333)
            produce(444)
        end
        produce(4555)
    end
end

end # module TaskGroupOptimizations

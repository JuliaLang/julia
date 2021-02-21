using Base.Experimental: Tapir

function fib(N)
    if N <= 1
        return N
    end
    local x1, x2
    Tapir.@sync begin
        Tapir.@spawn x1 = fib(N - 1)
        x2 = fib(N - 2)
    end
    return x1 + x2
end

###
# Interesting corner cases and broken IR
###

##
# Parallel regions with errors are tricky
# #1  detach within %sr, #2, #3
# #2  ...
#     unreachable()
#     reattach within %sr, #3
# #3  sync within %sr
#
# Normally a unreachable get's turned into a ReturnNode(),
# but that breaks the CFG. So we need to detect that we are
# in a parallel region.
#
# Question:
#   - Can we elimante a parallel region that throws?
#     Probably if the sync is dead as well. We could always
#     use the serial projection and serially execute the region.

#=
function vecadd_err(out, A, B)
    @assert length(out) == length(A) == length(B)
    @inbounds begin
        @par for i in 1:length(out)
            out[i] = A[i] + B[i]
            error()
        end
    end
    return out
end

# This function is broken due to the PhiNode
@noinline function fib2(N)
    if N <= 1
        return N
    end
    token = @syncregion()
    x1 = 0
    @spawn token begin
        x1  = fib2(N-1)
    end
    x2 = fib2(N-2)
    @sync_end token
    return x1 + x2
end
=#


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

function f()
    v = 'a'
    local a, b
    Tapir.@sync begin
        Tapir.@spawn begin
            a = (v, produce())
        end
        b = produce()
    end
    return (a, b)
end

function set_distinct(bool::Bool)
    local x, y
    Tapir.@sync begin
        Tapir.@spawn if bool
            x = 1
        else
            y = 2
        end
        if bool
            y = 3
        else
            x = 4
        end
    end
    return x + y
end

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

end # module TaskOutputs


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
        local y, z
        Tapir.@sync begin
            Tapir.@spawn z = _mapfold(f, op, right, basesize)
            y = _mapfold(f, op, left, basesize)
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

"""
    Tapir

May-happen parallelism API.

This module provides two public API `Tapir.@sync` and `Tapir.@spawn` for
denoting tasks that _may_ run in parallel.
"""
module Tapir

#####
##### Julia-Tapir Runtime
#####

const TaskGroup = Channel{Any}

taskgroup() = Channel{Any}(Inf)::TaskGroup

function spawn!(tasks::TaskGroup, @nospecialize(f))
    t = Task(f)
    t.sticky = false
    schedule(t)
    push!(tasks, t)
    return nothing
end

# Can we make it more efficient using the may-happen parallelism property
# (i.e., the lack of concurrent synchronizations)?
const sync! = Base.sync_end

mutable struct UndefableRef{T}
    set::Bool
    x::T
    UndefableRef{T}() where {T} = new{T}(false)
end

@inline box(::Type{T}) where {T} = UndefableRef{T}()
@noinline noinline_box(::Type{T}) where {T} = __UNDEFINED__::UndefableRef{T}

function Base.setindex!(ref::UndefableRef, x)
    ref.x = x
    ref.set = true
    return ref
end

function Base.getindex(ref::UndefableRef)
    ref.set || not_set_error(ref)
    return ref.x
end

@noinline function not_set_error(::UndefableRef{T}) where {T}
    error("variable of type `$T` is not set")
end

#####
##### Julia-Tapir Frontend
#####

macro syncregion()
    Expr(:syncregion)
end

macro spawnin(token, expr)
    Expr(:spawn, esc(token), esc(Expr(:block, expr)))
end

macro sync_end(token)
    Expr(:sync, esc(token))
end

macro loopinfo(args...)
    Expr(:loopinfo, args...)
end

const tokenname = gensym(:token)

"""
    Tapir.@sync block
"""
macro sync(block)
    var = esc(tokenname)
    quote
        let $var = @syncregion()
            $(esc(block))
            @sync_end($var)
        end
    end
end

"""
    Tapir.@spawn expression
"""
macro spawn(expr)
    var = esc(tokenname)
    quote
        @spawnin $var $(esc(expr))
    end
end

end

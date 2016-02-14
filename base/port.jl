"""
A placeholder for a value that isn't known yet
"""
type Future{T}
    state::Symbol
    cond::Condition
    value::T
    error::Exception
    Future() = new(:pending, Condition())
end

"""
Fill in the `Future` with its final value. Any `Task`'s waiting
on this value will be able to continue
"""
function assign(f::Future, value::Any)
    if f.state == :pending
        f.state = :done
        f.value = value
    elseif f.state == :needed
        notify(f.cond, value)
    end
end

"""
If you were unable to compute the value of the Future then you should `error` it
"""
function Base.error(f::Future, error::Exception)
    if f.state == :pending
        f.state = :failed
        f.error = error
    elseif f.state == :needed
        notify(f.cond, error; error=true)
    end
end

"""
Await the `Future` if its `:pending`. Otherwise reproduce it's result
"""
function Base.wait(f::Future)
    f.state == :done && return f.value
    f.state == :failed && rethrow(f.error)
    f.state == :needed && return wait(f.cond)
    try
        f.state = :needed
        f.value = wait(f.cond)
        f.state = :done
        f.value
    catch e
        f.state = :failed
        f.error = e
        rethrow(e)
    end
end

abstract Stream
immutable StreamNode <: Stream
    head::Any
    tail::Any
end
immutable EndOfStream <: Stream end
const EOS = EndOfStream()

"""
`Port`s provide access to an eagerly generated infinite stream without
using infinite memory
"""
type Port
    cursor::Future{Stream}
    Port() = new(Future{Stream}())
end

Base.start(p::Port) = p.cursor
Base.next(::Port, f::Future) = (s = wait(f); (s.head, s.tail))
Base.done(::Port, f::Future) = wait(f) === EOS
Base.push!(p::Port, value::Any) = begin
    rest = Future{Stream}()
    assign(p.cursor, StreamNode(value, rest))
    p.cursor = rest
    p
end
Base.close(p::Port) = assign(p.cursor, EOS)
Base.isopen(p::Port) = p.cursor.state ≠ :done && p.cursor.state ≠ :failed

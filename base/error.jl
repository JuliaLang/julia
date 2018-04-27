# This file is a part of Julia. License is MIT: https://julialang.org/license

# pseudo-definitions to show how everything behaves
#
# throw(label, val) = # throw a value to a dynamically enclosing block
#
# function rethrow(val)
#     global current_exception = val
#     throw(current_handler(), current_exception)
# end
#
# rethrow() = rethrow(current_exception)
#
# function throw(val)
#     global catch_backtrace = backtrace()
#     rethrow(val)
# end

"""
    throw(e)

Throw an object as an exception.
"""
throw

## native julia error handling ##

"""
    error(message::AbstractString)

Raise an `ErrorException` with the given message.
"""
error(s::AbstractString) = throw(ErrorException(s))

"""
    error(msg...)

Raise an `ErrorException` with the given message.
"""
function error(s::Vararg{Any,N}) where {N}
    @_noinline_meta
    throw(ErrorException(Main.Base.string(s...)))
end

"""
    rethrow([e])

Throw an object without changing the current exception backtrace. The default argument is
the current exception (if called within a `catch` block).
"""
rethrow() = ccall(:jl_rethrow, Bottom, ())
rethrow(e) = ccall(:jl_rethrow_other, Bottom, (Any,), e)

struct InterpreterIP
    code::Union{CodeInfo,Core.MethodInstance,Nothing}
    stmt::Csize_t
end

# convert dual arrays (ips, interpreter_frames) to a single array of locations
function _reformat_bt(bt, bt2)
    ret = Vector{Union{InterpreterIP,Ptr{Cvoid}}}()
    i, j = 1, 1
    while i <= length(bt)
        ip = bt[i]::Ptr{Cvoid}
        if ip == Ptr{Cvoid}(-1%UInt)
            # The next one is really a CodeInfo
            push!(ret, InterpreterIP(
                bt2[j],
                bt[i+2]))
            j += 1
            i += 3
        else
            push!(ret, Ptr{Cvoid}(ip))
            i += 1
        end
    end
    ret
end

function backtrace end

"""
    catch_backtrace()

Get the backtrace of the current exception, for use within `catch` blocks.
"""
function catch_backtrace()
    bt = Ref{Any}(nothing)
    bt2 = Ref{Any}(nothing)
    ccall(:jl_get_backtrace, Cvoid, (Ref{Any}, Ref{Any}), bt, bt2)
    return _reformat_bt(bt[], bt2[])
end

## keyword arg lowering generates calls to this ##
function kwerr(kw, args::Vararg{Any,N}) where {N}
    @_noinline_meta
    throw(MethodError(typeof(args[1]).name.mt.kwsorter, (kw,args...)))
end

## system error handling ##
"""
    systemerror(sysfunc, iftrue)

Raises a `SystemError` for `errno` with the descriptive string `sysfunc` if `iftrue` is `true`
"""
systemerror(p, b::Bool; extrainfo=nothing) = b ? throw(Main.Base.SystemError(string(p), Libc.errno(), extrainfo)) : nothing

## assertion macro ##


"""
    @assert cond [text]

Throw an [`AssertionError`](@ref) if `cond` is `false`. Preferred syntax for writing assertions.
Message `text` is optionally displayed upon assertion failure.

!!! warning
    An assert might be disabled at various optimization levels.
    Assert should therefore only be used as a debugging tool
    and not used for authentication verification (e.g., verifying passwords),
    nor should side effects needed for the function to work correctly
    be used inside of asserts.

# Examples
```jldoctest
julia> @assert iseven(3) "3 is an odd number!"
ERROR: AssertionError: 3 is an odd number!

julia> @assert isodd(3) "What even are numbers?"
```
"""
macro assert(ex, msgs...)
    msg = isempty(msgs) ? ex : msgs[1]
    if isa(msg, AbstractString)
        msg = msg # pass-through
    elseif !isempty(msgs) && (isa(msg, Expr) || isa(msg, Symbol))
        # message is an expression needing evaluating
        msg = :(Main.Base.string($(esc(msg))))
    elseif isdefined(Main, :Base) && isdefined(Main.Base, :string) && applicable(Main.Base.string, msg)
        msg = Main.Base.string(msg)
    else
        # string() might not be defined during bootstrap
        msg = :(Main.Base.string($(Expr(:quote,msg))))
    end
    return :($(esc(ex)) ? $(nothing) : throw(AssertionError($msg)))
end

struct ExponentialBackOff
    n::Int
    first_delay::Float64
    max_delay::Float64
    factor::Float64
    jitter::Float64

    function ExponentialBackOff(n, first_delay, max_delay, factor, jitter)
        all(x->x>=0, (n, first_delay, max_delay, factor, jitter)) || error("all inputs must be non-negative")
        new(n, first_delay, max_delay, factor, jitter)
    end
end

"""
    ExponentialBackOff(; n=1, first_delay=0.05, max_delay=10.0, factor=5.0, jitter=0.1)

A [`Float64`](@ref) iterator of length `n` whose elements exponentially increase at a
rate in the interval `factor` * (1 ± `jitter`).  The first element is
`first_delay` and all elements are clamped to `max_delay`.
"""
ExponentialBackOff(; n=1, first_delay=0.05, max_delay=10.0, factor=5.0, jitter=0.1) =
    ExponentialBackOff(n, first_delay, max_delay, factor, jitter)
start(ebo::ExponentialBackOff) = (ebo.n, min(ebo.first_delay, ebo.max_delay))
function next(ebo::ExponentialBackOff, state)
    next_n = state[1]-1
    curr_delay = state[2]
    next_delay = min(ebo.max_delay, state[2] * ebo.factor * (1.0 - ebo.jitter + (rand(Float64) * 2.0 * ebo.jitter)))
    (curr_delay, (next_n, next_delay))
end
done(ebo::ExponentialBackOff, state) = state[1]<1
length(ebo::ExponentialBackOff) = ebo.n

"""
    retry(f::Function;  delays=ExponentialBackOff(), check=nothing) -> Function

Return an anonymous function that calls function `f`.  If an exception arises,
`f` is repeatedly called again, each time `check` returns `true`, after waiting the
number of seconds specified in `delays`.  `check` should input `delays`'s
current state and the `Exception`.

# Examples
```julia
retry(f, delays=fill(5.0, 3))
retry(f, delays=rand(5:10, 2))
retry(f, delays=Base.ExponentialBackOff(n=3, first_delay=5, max_delay=1000))
retry(http_get, check=(s,e)->e.status == "503")(url)
retry(read, check=(s,e)->isa(e, UVError))(io, 128; all=false)
```
"""
function retry(f::Function;  delays=ExponentialBackOff(), check=nothing)
    (args...; kwargs...) -> begin
        state = start(delays)
        while true
            try
                return f(args...; kwargs...)
            catch e
                done(delays, state) && rethrow(e)
                if check !== nothing
                    result = check(state, e)
                    state, retry_or_not = length(result) == 2 ? result : (state, result)
                    retry_or_not || rethrow(e)
                end
            end
            (delay, state) = next(delays, state)
            sleep(delay)
        end
    end
end

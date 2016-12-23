# This file is a part of Julia. License is MIT: http://julialang.org/license

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

## native julia error handling ##

error(s::AbstractString) = throw(ErrorException(s))
error(s...) = throw(ErrorException(Main.Base.string(s...)))

rethrow() = ccall(:jl_rethrow, Bottom, ())
rethrow(e) = ccall(:jl_rethrow_other, Bottom, (Any,), e)
backtrace() = ccall(:jl_backtrace_from_here, Array{Ptr{Void},1}, (Int32,), false)
catch_backtrace() = ccall(:jl_get_backtrace, Array{Ptr{Void},1}, ())

## keyword arg lowering generates calls to this ##
kwerr(kw, args...) = throw(MethodError(typeof(args[1]).name.mt.kwsorter, (kw,args...)))

## system error handling ##

systemerror(p, b::Bool; extrainfo=nothing) = b ? throw(Main.Base.SystemError(string(p), Libc.errno(), extrainfo)) : nothing

## assertion functions and macros ##

assert(x) = x ? nothing : throw(Main.Base.AssertionError())
macro assert(ex, msgs...)
    msg = isempty(msgs) ? ex : msgs[1]
    if isa(msg, AbstractString)
        msg = msg # pass-through
    elseif !isempty(msgs) && (isa(msg, Expr) || isa(msg, Symbol))
        # message is an expression needing evaluating
        msg = :(Main.Base.string($(esc(msg))))
    elseif isdefined(Main, :Base) && isdefined(Main.Base, :string)
        msg = Main.Base.string(msg)
    else
        # string() might not be defined during bootstrap
        msg = :(Main.Base.string($(Expr(:quote,msg))))
    end
    return :($(esc(ex)) ? $(nothing) : throw(Main.Base.AssertionError($msg)))
end

# NOTE: Please keep the constant values specified below in sync with the doc string
const DEFAULT_RETRY_N = 1
const DEFAULT_RETRY_ON = e->true
const DEFAULT_RETRY_MAX_DELAY = 10.0

"""
    retry(f, [retry_on]; n=1, max_delay=10.0) -> Function

Returns a lambda that retries function `f` up to `n` times in the
event of an exception. If `retry_on` is a `Type` then retry only
for exceptions of that type. If `retry_on` is a function
`test_error(::Exception) -> Bool` then retry only if it is true.

The first retry happens after a gap of 50 milliseconds or `max_delay`,
whichever is lower. Subsequently, the delays between retries are
exponentially increased with a random factor up to `max_delay`.

**Examples**
```julia
retry(http_get, e -> e.status == "503")(url)
retry(read, UVError)(io)
```
"""
function retry(f::Function, retry_on::Function=DEFAULT_RETRY_ON; n=DEFAULT_RETRY_N, max_delay=DEFAULT_RETRY_MAX_DELAY)
    (args...) -> begin
        delay = min(0.05, max_delay)
        for i = 1:n+1
            try
                return f(args...)
            catch e
                if i > n || try retry_on(e) end !== true
                    rethrow(e)
                end
            end
            delay = min(max_delay, delay)
            sleep(delay * (0.8 + (rand() * 0.2)))
            delay = delay * 5
        end
    end
end

retry(f::Function, t::Type; kw...) = retry(f, e->isa(e, t); kw...)

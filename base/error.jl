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
error(s...) = throw(ErrorException(string(s...)))

rethrow() = ccall(:jl_rethrow, Void, ())::Bottom
rethrow(e) = ccall(:jl_rethrow_other, Void, (Any,), e)::Bottom
backtrace() = ccall(:jl_backtrace_from_here, Array{Ptr{Void},1}, ())
catch_backtrace() = ccall(:jl_get_backtrace, Array{Ptr{Void},1}, ())

## keyword arg lowering generates calls to this ##
kwerr(kw) = error("unrecognized keyword argument \"", kw, "\"")

## system error handling ##

systemerror(p, b::Bool) = b ? throw(SystemError(string(p))) : nothing

## assertion functions and macros ##

assert(x) = x ? nothing : throw(AssertionError())
macro assert(ex, msgs...)
    msg = isempty(msgs) ? ex : msgs[1]
    if !isempty(msgs) && isa(msg, Expr)
        # message is an expression needing evaluating
        msg = :(string($(esc(msg))))
    elseif isdefined(Base, :string)
        msg = string(msg)
    else
        # string() might not be defined during bootstrap
        msg = :(string($(Expr(:quote,msg))))
    end
    :($(esc(ex)) ? $(nothing) : throw(AssertionError($msg)))
end

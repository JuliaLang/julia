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

error(s::AbstractString) = throw(Main.Base.ErrorException(s))
error(s...) = throw(Main.Base.ErrorException(Main.Base.string(s...)))

rethrow() = ccall(:jl_rethrow, Void, ())::Bottom
rethrow(e) = ccall(:jl_rethrow_other, Void, (Any,), e)::Bottom
backtrace() = ccall(:jl_backtrace_from_here, Array{Ptr{Void},1}, ())
catch_backtrace() = ccall(:jl_get_backtrace, Array{Ptr{Void},1}, ())

## keyword arg lowering generates calls to this ##
kwerr(kw) = error("unrecognized keyword argument \"", kw, "\"")

## system error handling ##

systemerror(p, b::Bool) = b ? throw(Main.Base.SystemError(string(p))) : nothing

## assertion functions and macros ##

assert(x) = x ? nothing : throw(Main.Base.AssertionError())
macro assert(ex, msgs...)
    msg = isempty(msgs) ? ex : msgs[1]
    if !isempty(msgs) && isa(msg, Expr)
        # message is an expression needing evaluating
        msg = :(Main.Base.string($(esc(msg))))
    elseif isdefined(Main, :Base) && isdefined(Main.Base, :string)
        msg = Main.Base.string(msg)
    else
        # string() might not be defined during bootstrap
        msg = :(Main.Base.string($(Expr(:quote,msg))))
    end
    :($(esc(ex)) ? $(nothing) : throw(Main.Base.AssertionError($msg)))
end

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

error(e::Exception) = throw(e)
error{E<:Exception}(::Type{E}) = throw(E())
error(s::String) = throw(ErrorException(s))
error(s...)      = throw(ErrorException(string(s...)))

macro unexpected()
    :(error("unexpected branch reached"))
end

rethrow() = ccall(:jl_rethrow, Void, ())::None
rethrow(e) = ccall(:jl_rethrow_other, Void, (Any,), e)::None
backtrace() = ccall(:jl_backtrace_from_here, Array{Ptr{Void},1}, ())
catch_backtrace() = ccall(:jl_get_backtrace, Array{Ptr{Void},1}, ())

## system error handling ##

errno() = ccall(:jl_errno, Int32, ())
strerror(e::Integer) = bytestring(ccall(:strerror, Ptr{Uint8}, (Int32,), e))
strerror() = strerror(errno())
systemerror(p, b::Bool) = b ? throw(SystemError(string(p))) : nothing

## assertion functions and macros ##

assert(x) = x ? nothing : error("assertion failed")
macro assert(ex,msg...)
    msg = isempty(msg) ? :(string($(Expr(:quote,ex)))) : esc(msg[1])
    :($(esc(ex)) ? nothing : error("assertion failed: ", $msg))
end

## printing with color ##

function with_output_color(f::Function, color::Symbol, io::IO, args...)
    have_color || return f(io, args...)
    print(io, get(text_colors, color, color_normal))
    try f(io, args...)
    finally
        print(io, color_normal)
    end
end

print_with_color(color::Symbol, io::IO, msg::String...) =
    with_output_color(print, color, io, msg...)
print_with_color(color::Symbol, msg::String...) =
    print_with_color(color, STDOUT, msg...)

# use colors to print messages and warnings in the REPL

function info(msg::String...; prefix="INFO: ")
    with_output_color(print, :blue, STDERR, prefix, chomp(string(msg...)))
    println(STDERR)
end
function warn(msg::String...; prefix="WARNING: ")
    with_output_color(print, :red,  STDERR, prefix, chomp(string(msg...)))
    println(STDERR)
end
warn(err::Exception; prefix="ERROR: ") =
    warn(sprint(io->showerror(io,err)), prefix=prefix)

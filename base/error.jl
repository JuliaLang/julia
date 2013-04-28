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

assert(x) = assert(x,'?')
assert(x,labl) = x ? nothing : error("assertion failed: ", labl)
macro assert(ex)
    :($(esc(ex)) ? nothing : error("assertion failed: ", $(string(ex))))
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
    print_with_color(color, OUTPUT_STREAM, msg...)

# use colors to print messages and warnings in the REPL

function info(msg::String...; depth=0)
    stack::Range1{Int} = 3 +
        if isa(depth,Int)
            depth:depth
        else
            depth
        end
    with_output_color(print, :blue, STDERR, "MESSAGE: ", msg...)
    with_output_color(show_backtrace, :blue, STDERR, backtrace(), stack)
    println(STDERR)
end
function warn(msg::String...; depth=0)
    stack::Range1{Int} = 3 +
        if isa(depth,Int)
            depth:depth
        else
            depth
        end
    with_output_color(print, :red,  STDERR, "WARNING: ", msg...)
    with_output_color(show_backtrace, :red, STDERR, backtrace(), stack)
    println(STDERR)
end

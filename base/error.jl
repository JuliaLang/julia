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
backtrace() = ccall(:jl_get_backtrace, Array{Any,1}, ())

## system error handling ##

errno() = ccall(:jl_errno, Int32, ())
strerror(e::Integer) = bytestring(ccall(:strerror, Ptr{Uint8}, (Int32,), e))
strerror() = strerror(errno())
system_error(p, b::Bool) = b ? throw(SystemError(string(p))) : nothing

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

info(msg::String...) = print_with_color(:blue, STDERR, "MESSAGE: ", msg..., "\n")
warn(msg::String...) = print_with_color(:red,  STDERR, "WARNING: ", msg..., "\n")

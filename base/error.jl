## native julia error handling ##

error(e::Exception) = throw(e)
error{E<:Exception}(::Type{E}) = throw(E())
error(s::String) = throw(ErrorException(s))
error(s...)      = throw(ErrorException(string(s...)))

macro unexpected()
    :(error("unexpected branch reached"))
end

## system error handling ##

errno() = ccall(:jl_errno, Int32, ())
strerror(e::Integer) = bytestring(ccall(:strerror, Ptr{Uint8}, (Int32,), e))
strerror() = strerror(errno())
system_error(p, b::Bool) = b ? throw(SystemError(string(p))) : nothing

## assertion functions and macros ##

assert(x) = assert(x,'?')
assert(x,labl) = x ? nothing : error("assertion failed: ", labl)

# @assert is for things that should never happen
macro assert(ex)
    :($(esc(ex)) ? nothing : error("assertion failed: ", $(string(ex))))
end
# @expect is for things that might happen
# if e.g. a function is called with the wrong arguments
macro expect(ex)
    :($(esc(ex)) ? nothing : error($(string("expected ", ex, " == true"))))
end

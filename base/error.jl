## native julia error handling ##

error(e::Exception) = throw(e)
error{E<:Exception}(::Type{E}) = throw(E())
error(s...) = throw(ErrorException(cstring(s...)))

macro unexpected()
    :(error("unexpected branch reached"))
end

## system error handling ##

errno() = ccall(:jl_errno, Int32, ())
strerror(e::Integer) = ccall(:jl_strerror, Any, (Int32,), e)::ByteString
strerror() = strerror(errno())
system_error(p, b::Bool) = b ? throw(SystemError(cstring(p))) : nothing

## assertion functions and macros ##

assert(x) = assert(x,'?')
assert(x::AbstractArray,labl) = (map(x->assert(x,labl), x); nothing)
assert(x,labl) = x ? nothing : error("assertion failed: ", labl)

macro assert(ex)
	:(assert(($ex), $string(ex)))
end

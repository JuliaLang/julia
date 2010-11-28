## native julia error handling ##

error(e::Exception) = throw(e)
error{E<:Exception}(::Type{E}) = throw(E())
error(s::ByteString) = throw(ErrorException(s))
error(s...) = error(print_to_string(print, s...))
assert(b::Bool) = b ? true : error("Assertion failed.")
assert(B::Tensor{Bool}) = assert(all(B))

## system error handling ##

errno() = ccall(dlsym(JuliaDLHandle,"jl_errno"), Int32, ())
strerror(e::Int) =
    ccall(dlsym(JuliaDLHandle,"jl_strerror"),
          Any, (Int32,), int32(e))::ByteString
strerror() = strerror(errno())
system_error(p::String, b::Bool) = b ? error(SystemError(p)) : true

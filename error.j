## native julia error handling ##

error(e::Exception) = throw(e)
error{E<:Exception}(::Type{E}) = throw(E())
error(s::ByteString) = throw(ErrorException(s))
error(s...) = error(print_to_string(print, s...))

## system error handling ##

errno() = ccall(dlsym(JuliaDLHandle,"jl_errno"), Int32, ())
strerror(e::Int) =
    ccall(dlsym(JuliaDLHandle,"jl_strerror"),
          Any, (Int32,), int32(e))::ByteString
strerror() = strerror(errno())
system_error(p::String, b::Bool) = b ? error(SystemError(p)) : true

## assertion functions and macros ##

assert_test(b::Bool) = b
assert_test(b::Tensor{Bool}) = all(b)
assert(x) = assert_test(x) ? true : error("Assertion failed.")

macro assert(ex)
    :(assert_test($ex) ? true :
      error("Assertion failed: ", $string(ex)))
end

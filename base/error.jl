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
assert(x,labl) = x ? nothing : throw(AssertionError(string(labl)))

# @assert is for things that should never happen
macro assert(e)
    :($(esc(e)) ? nothing : throw(AssertionError($(sprint(show_unquoted,e)))))
end

# @expect is for things that might happen
# if e.g. a function is called with the wrong arguments
# usage:
#                                         # x > 5 ==>
#    @expect x <= 5                       # error("expected: (x <= 5) == true")
#    @expect x <= 5 KeyError(x)           # error(KeyError(5))
#    @expect x <= 5 msg ArgumentError(msg) 
#    # fails ==> error(ArgumentError("expected: (x <= 5) == true"))
macro expect(args...)
    code_expect(args...)
end
code_expect(pred) =  (msym = gensym(); code_expect(pred, msym,     msym))
code_expect(pred, err)               = code_expect(pred, gensym(), err)
function code_expect(pred, msym::Symbol, err)
    msg = string("expected ", sprint(show_unquoted,pred), " == true")    
    esc(:( if !($pred)
        let $msym = $(expr(:quote, msg))
            error($err)
        end
    end))
end

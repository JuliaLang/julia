
"""
    FallbackTestSet

A simple fallback test set that throws immediately on a failure.
"""
struct FallbackTestSet <: AbstractTestSet end
fallback_testset = FallbackTestSet()

struct FallbackTestSetException <: Exception
    msg::AbstractString
end

function Base.showerror(io::IO, ex::FallbackTestSetException, bt; backtrace=true)
    print_with_color(Base.error_color(), io, ex.msg)
end

# Records nothing, and throws an error immediately whenever a Fail or
# Error occurs. Takes no action in the event of a Pass or Broken result
record(ts::FallbackTestSet, t::Union{Pass,Broken}) = t
function record(ts::FallbackTestSet, t::Union{Fail,Error})
    println(t)
    throw(FallbackTestSetException("There was an error during testing"))
end
# We don't need to do anything as we don't record anything
finish(ts::FallbackTestSet) = ts


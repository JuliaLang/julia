# This file is a part of Julia. License is MIT: https://julialang.org/license

# Declared exceptions runtime support

"""
    Except{T, E}

A wrapper type for values that may be either a successful result of type `T`
or an exception of type `E`. Similar to Rust's `Result<T, E>`.

Functions can declare their exception types using `Except` as a return type annotation:

```julia
function getindex(a::Vector{T}, i::Int)::Except{T, BoundsError}
    # ...
end
```

For multiple exception types, use `Union`:
```julia
function foo()::Except{Int, Union{KeyError, BoundsError}}
    # ...
end
```

See also: [`AnyExcept`](@ref), [`unwrap`](@ref), [`is_exception`](@ref)
"""
mutable struct Except{T, E}
    _value::T
    _exception::Union{E, Nothing}

    # Success constructor - value only
    function Except{T,E}(val) where {T, E}
        e = new{T,E}()
        e._value = val
        e._exception = nothing
        return e
    end

    # Internal constructor for exception case
    function Except{T,E}(::Nothing, exc::E) where {T, E}
        e = new{T,E}()
        e._exception = exc
        return e
    end
end

"""
    AnyExcept{E}

Convenience alias for `Except{Any, E}`. Use when you want to declare exception types
without constraining the return type.

```julia
function may_fail()::AnyExcept{IOError}
    # ...
end
```
"""
const AnyExcept{E} = Except{Any, E}

"""
    except_value(::Type{Except{T,E}}, val) where {T, E}

Create an `Except` containing a successful value.
"""
except_value(::Type{Except{T,E}}, val) where {T, E} = Except{T,E}(val)

"""
    except_exception(::Type{Except{T,E}}, exc::E) where {T, E}

Create an `Except` containing an exception.
"""
except_exception(::Type{Except{T,E}}, exc::E) where {T, E} = Except{T,E}(nothing, exc)

"""
    is_exception(e::Except) -> Bool

Return `true` if `e` contains an exception, `false` if it contains a value.
"""
is_exception(e::Except) = e._exception !== nothing

"""
    unwrap(e::Except)

Extract the value from an `Except`. If `e` contains an exception, throw it.

# Examples
```julia
e = Except{Int, BoundsError}(42)
unwrap(e)  # returns 42

e_err = except_exception(Except{Int, BoundsError}, BoundsError([1,2], 5))
unwrap(e_err)  # throws BoundsError
```
"""
function unwrap(e::Except)
    if is_exception(e)
        throw(e._exception)
    end
    return e._value
end

"""
    get_exception(e::Except)

Return the exception contained in `e`, or `nothing` if `e` contains a value.
"""
get_exception(e::Except) = e._exception

"""
    forward_or_unwrap(e::Except{T,E}) where {T,E}

For the postfix `?` operator: if `e` contains an exception, return a new `Except`
with that exception (for forwarding). Otherwise, return the unwrapped value.
"""
function forward_or_unwrap(e::Except{T,E}) where {T,E}
    if is_exception(e)
        return except_exception(Except{T,E}, e._exception)
    end
    return e._value
end

# For non-Except values, just return them (identity)
forward_or_unwrap(x) = x

"""
    ExceptRaw

Sentinel type used to dispatch to the raw (non-unwrapping) version of a function
that returns `Except`. Used internally by the `?` and `match?` operators.
"""
struct ExceptRaw end

"""
    except_raw

Singleton instance of `ExceptRaw` used to call the raw version of Except-returning functions.
"""
const except_raw = ExceptRaw()

# Convert a value to Except (wrap as success)
function convert(::Type{Except{T,E}}, val) where {T, E}
    return Except{T,E}(convert(T, val))
end

# Convert between Except types when the exception is compatible
function convert(::Type{Except{T,E}}, e::Except{T2,E2}) where {T, E, T2, E2}
    if is_exception(e)
        exc = e._exception
        if exc isa E
            return Except{T,E}(nothing, exc)
        else
            # Exception type mismatch - this shouldn't happen with proper usage
            throw(TypeError(:convert, "exception type", E, exc))
        end
    else
        return Except{T,E}(convert(T, e._value))
    end
end

function show(io::IO, e::Except{T,E}) where {T,E}
    if is_exception(e)
        print(io, "Except{", T, ", ", E, "}(exception: ")
        show(io, e._exception)
        print(io, ")")
    else
        print(io, "Except{", T, ", ", E, "}(")
        show(io, e._value)
        print(io, ")")
    end
end

# This file is a part of Julia. License is MIT: https://julialang.org/license

# Runtime support for declared exceptions.
#
# A function may declare the exceptions that are part of its API with the
# `except` keyword:
#
#     function findkey(d, k)::Int except KeyError
#         haskey(d, k) || throw(KeyError(k))?
#         return d[k]
#     end
#
# Declared exceptions do not unwind the stack. Lowering splits such a method
# (similar to keyword argument lowering) into an inner method returning an
# `Except{T,E}` value (carrying either the ordinary result or a declared
# exception), an outer method that unwraps it, throwing the exception for
# callers that do not participate, and an entry point on the `except_call`
# generic function. Callsites opt in to receiving declared exceptions with
# the postfix `?` operator (propagate everything the callee declares that
# fits the caller's own declaration) or the callsite `expr except E` filter
# (propagate only exceptions of type `E`).
# See the lowering in `src/julia-syntax.scm` for details.

"""
    Except{T, E}

The result of a function with a declared exception type
(`function f(x)::T except E`): a discriminated union carrying either an
ordinary result of type `T` or a declared exception of type `E`, traveling
as a value rather than unwinding the stack. Because both states are
represented inside the one wrapper type, the two channels can never be
confused, even for functions whose ordinary result is itself an `Except`
object.

Construct the ordinary state with `Except{T,E}(value)` (which converts
`value` to `T`, like a return type annotation) and the exceptional state
with `Except{T,E}(throw, exception)`. Use [`unwrap_except`](@ref) to
extract the result, throwing in the exceptional state; `case except` arms
of a `match` statement handle both states without throwing. The fields of
`Except` are internal.

!!! compat "Julia 1.14"
    Declared exceptions were added in Julia 1.14.
"""
struct Except{T, E}
    _val::Union{T, E}
    _iserr::Bool
    Except{T, E}(v) where {T, E} = new{T, E}(convert(T, v)::T, false)
    Except{T, E}(::typeof(throw), e::E) where {T, E} = new{T, E}(e, true)
end

# Construct an `Except` in the ordinary state. The exception parameter is
# `Union{}`: a value carries no exception, which lets inference prove the
# exceptional path dead when the state is known from the type.
except_value(v) = Except{typeof(v), Union{}}(v)

# Construct an `Except` in the exceptional state (no value).
except_error(e) = Except{Union{}, typeof(e)}(throw, e)

# The discriminant, total so that propagation sites can test arbitrary
# values: anything that is not an `Except` is an ordinary value.
except_iserr(@nospecialize x) = false
except_iserr(x::Except) = getfield(x, :_iserr)

# The ordinary result carried by an `Except` (identity on other values,
# for propagation sites applied to arbitrary expressions).
except_result(@nospecialize x) = x
except_result(x::Except{T}) where {T} = getfield(x, :_val)::T

# The declared exception carried by an `Except` in the exceptional state.
except_exception(x::Except{<:Any, E}) where {E} = getfield(x, :_val)::E

"""
    unwrap_except(x::Except)

Extract the result of an [`Except`](@ref): return the ordinary result, or
throw the declared exception it carries as an ordinary exception. This
implements the default behavior at the boundary between code using
declared exceptions and code that does not.
"""
unwrap_except(x::Except) = except_iserr(x) ? throw(except_exception(x)) : except_result(x)

"""
    except_call(f, args...; kwargs...) -> Except

Call `f`, returning its result as an [`Except`](@ref): declared exceptions
are carried in the exceptional state instead of being thrown. For
functions without declared exceptions this performs an ordinary call (any
exception is thrown as usual) and returns the result in the ordinary
state.

Methods of this function are added automatically for methods declared with
the `except` keyword; it is the declared-exception analog of `Core.kwcall`.
"""
except_call(f, args...; kwargs...) = except_value(f(args...; kwargs...))

# Exception-aware element collection for comprehensions containing `?`.
# `[f(a)? for a in arr]` is lowered to short-circuit on the first
# exceptional element and return it (to be propagated at the comprehension
# site in the enclosing function).
function except_collect(f, iter)
    out = Vector{Any}()
    for x in iter
        v = f(x)
        except_iserr(v) && return except_error(except_exception(v))
        push!(out, except_result(v))
    end
    isempty(out) && return except_value(Union{}[])
    # Narrow the element type based on the values collected
    return except_value(map(identity, out))
end

function except_collect_typed(::Type{T}, f, iter) where {T}
    out = Vector{T}()
    for x in iter
        v = f(x)
        except_iserr(v) && return except_error(except_exception(v))
        push!(out, except_result(v))
    end
    return except_value(out)
end

function show(io::IO, x::Except)
    show(io, typeof(x))
    print(io, '(')
    if except_iserr(x)
        print(io, "throw, ")
        show(io, except_exception(x))
    else
        show(io, except_result(x))
    end
    print(io, ')')
end

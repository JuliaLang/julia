# This file is a part of Julia. License is MIT: http://julialang.org/license

# Support for @polly

module Polly

export @polly

import Base: start, next, done

"""
Tells the compiler to apply the polyhedral optimizer Polly to a function.
"""
macro polly(func)
    (isa(func, Expr) && func.head == :function) || throw(ArgumentError("@polly can only be applied to functions"))
    canonicalize!(func)
    return esc(Base.pushmeta!(func, :polly))
end

# This range type only differs from `Base.UnitRange` in its representation of
# emptiness. An empty `Base.UnitRange` will always have `stop == start - 1`.
# For example, constructing a range `5:2` will actually result in `5:4`.
# `Polly.UnitRange` drops this requirement, i.e. `5:2` would be used as is,
# which allows for a simpler constructor. When iterating over a
# `Polly.UnitRange` loop bounds will therefore be more obvious to Polly than
# with a `Base.UnitRange`.
immutable UnitRange{T<:Real} <: AbstractUnitRange{T}
    start::T
    stop::T
    UnitRange(start, stop) = new(start, stop)
end
UnitRange{T<:Real}(start::T, stop::T) = Polly.UnitRange{T}(start, stop)

# This method was directly adopted from `Base.UnitRange`.
start{T}(r::Polly.UnitRange{T}) = oftype(r.start + one(T), r.start)

# This has to be different than for `Base.UnitRange` to reflect the different
# behavior of the `Polly.UnitRange` constructor.
done{T}(r::Polly.UnitRange{T}, i) = (i < oftype(i, r.start)) | (i > oftype(i, r.stop))

# `Base.StepRange` uses the same representation of emptiness as described above
# for `Base.UnitRange` with `stop == start - 1` but additionally, in the case of
# non-emptiness, its constructor will precompute the last value that is actually
# part of the range. For example, `5:2:8` would actually result in `5:2:7`. In
# `Polly.StepRange` we simplify construction by dropping these requirements,
# i.e. `5:2:8` would also be used as is. When iterating over a `Polly.StepRange`
# loop bounds will therefore be more obvious to Polly than with a
# `Base.StepRange`.
immutable StepRange{T,S} <: OrdinalRange{T,S}
    start::T
    step::S
    stop::T
    StepRange(start::T, step::S, stop::T) = new(start, step, stop)
end
StepRange{T,S}(start::T, step::S, stop::T) = Polly.StepRange{T,S}(start, step, stop)

# This method was directly adopted from `Base.StepRange`.
start(r::Polly.StepRange) = oftype(r.start + r.step, r.start)

# This method was directly adopted from `Base.StepRange`.
next{T}(r::Polly.StepRange{T}, i) = (convert(T,i), i + r.step)

# We have to use a simpler condition as for `Base.StepRange` in order to
# be able to derive the loop bounds in Polly. For now it also ignores
# wrap-arounds which could for example happen for `1:1:typemax(Int64)` which we
# consider a rare application.
done{T,S}(r::Polly.StepRange{T,S}, i) = (r.step > zero(r.step)) ? (i > oftype(i, r.stop)) :
                                                                  (i < oftype(i, r.stop))

# This was directly adopted from `Base.UnitRange` and `Base.StepRange` to avoid
# overflows for smaller `Integer` types.
let smallint = (Int === Int64 ?
                Union{Int8,UInt8,Int16,UInt16,Int32,UInt32} :
                Union{Int8,UInt8,Int16,UInt16})
    global start
    global next
    start{T<:smallint}(r::Polly.StepRange{T}) = convert(Int, r.start)
    next{T<:smallint}(r::Polly.StepRange{T}, i) = (i % T, i + r.step)
    start{T<:smallint}(r::Polly.UnitRange{T}) = convert(Int, r.start)
end

# Find assigments of the form `i = start:stop` and `i = start:step:stop` that
# occur in `for`-loop headers in `func` and replace them by
# `i = Polly.UnitRange(start,stop)` and `i = Polly.StepRange(start,step,stop)`.
function canonicalize!(func)
    worklist = [func]
    while !isempty(worklist)
        expr = pop!(worklist)
        if expr.head == :for
            loop_header = expr.args[1]
            canonicalize_ranges_in_loop_header!(loop_header)
            # The loop body might contain further loops that should be
            # canonicalized, so push it to the worklist for later examination.
            loop_body = expr.args[2]
            push!(worklist, loop_body)
        else
            # If `Expr` isn't a `for`-loop, it might contain nested expressions
            # which themselves contain `for`-loops.
            for arg in expr.args
                if isa(arg, Expr)
                    push!(worklist, arg)
                end
            end
        end
    end
end

# Find assigments of the form `i = start:stop` and `i = start:step:stop` in
# the given `loop_header` and replace them by `i = Polly.UnitRange(start,stop)`
# and `i = Polly.StepRange(start,step,stop)`.
function canonicalize_ranges_in_loop_header!(loop_header)
    if loop_header.head == :block
        # If `loop_header` is a `:block` expression we are dealing with a loop
        # of the form `for i1 = ..., i2 = ..., ...` which uses multiple
        # iteration variables.
        for assignment in loop_header.args
            canonicalize_range_in_assignment!(assignment)
        end
    else
        # If `loop_header` is not a `:block` expression we have just a simple
        # `for i = ...` with a single iteration variable.
        canonicalize_range_in_assignment!(loop_header)
    end
end

# If the given assignment has the form `i = start:stop` or `i = start:step:stop`
# then rewrite it to `i = Polly.UnitRange(start,stop)` or
# `i = Polly.StepRange(start,step,stop)`.
function canonicalize_range_in_assignment!(assignment)
    @assert(assignment.head == :(=))
    rhs = assignment.args[2]
    new_rhs = nothing

    if rhs.head == :(:)
        if length(rhs.args) == 2
            start = rhs.args[1]
            stop = rhs.args[2]
            new_rhs = :(Base.Polly.UnitRange($start,$stop))
        elseif length(rhs.args) == 3
            start = rhs.args[1]
            step = rhs.args[2]
            stop = rhs.args[3]
            new_rhs = :(Base.Polly.StepRange($start,$step,$stop))
        end
    end

    if new_rhs != nothing
        assignment.args[2] = new_rhs
    end
end

end # module Polly

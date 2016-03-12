# This file is a part of Julia. License is MIT: http://julialang.org/license


@noinline function precondition_error(msg)
    io = IOBuffer()
    StackTraces.show_spec_linfo(io, StackTraces.lookup(backtrace()[3]))
    throw(ArgumentError("$(takebuf_string(io)) requires $msg"))
end


"""
    @require precondition [message]

Throw `ArgumentError` if `precondition` is false.
"""
macro require(precondition, msg = string(precondition))
    esc(:(if ! $precondition Base.precondition_error($msg) end))
end
# FIXME
# Should this have a branch-prediction hint? (same for @assert?)
# http://llvm.org/docs/BranchWeightMetadata.html#built-in-expect-instructions

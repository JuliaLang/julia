module Pragma

export @unroll

##
# Uses the loopinfo expr node to attach LLVM loopinfo to loops
# the full list of supported metadata nodes is available at
# https://llvm.org/docs/LangRef.html#llvm-loop
# TODO:
#   - Figure out how to deal with compile-time constants in `@unroll(N, expr)`
#     so constants that come from `Val{N}` but are not parse time constant.
#   - Difference between `unroll_enable` and `unroll_full`
#   - ? Expose `unroll_disable`
#   - ? Expose `jam_disable`
##

module MD
    disable_nonforced() = (Symbol("llvm.loop.disable_nonforced"),)
    interleave(n) = (Symbol("llvm.loop.interleave.count"), convert(Int, n))
    vectorize_enable(flag) = (Symbol("llvm.loop.vectorize.enable"), convert(Bool, flag))
    vectorize_width(n) = (Symbol("llvm.loop.vectorize.width"), convert(Int, n))
    # ‘llvm.loop.vectorize.followup_vectorized’
    # ‘llvm.loop.vectorize.followup_epilogue’
    # ‘llvm.loop.vectorize.followup_all’
    unroll_count(n) = (Symbol("llvm.loop.unroll.count"), convert(Int, n))
    unroll_disable() = (Symbol("llvm.loop.unroll.disable"),)
    unroll_enable() = (Symbol("llvm.loop.unroll.enable"),)
    unroll_full() = (Symbol("llvm.loop.unroll.full"),)
    # ‘llvm.loop.unroll.followup’
    # ‘llvm.loop.unroll.followup_remainder’
    jam_count(n) = (Symbol("llvm.loop.unroll_and_jam.count"), convert(Int, n))
    jam_disable() = (Symbol("llvm.loop.unroll_and_jam.disable"),)
    jam_enable() = (Symbol("llvm.loop.unroll_and_jam.enable"),)
    # ‘llvm.loop.unroll_and_jam.followup_outer’
    # ‘llvm.loop.unroll_and_jam.followup_inner’
    # ‘llvm.loop.unroll_and_jam.followup_remainder_outer’
    # ‘llvm.loop.unroll_and_jam.followup_remainder_inner’
    # ‘llvm.loop.unroll_and_jam.followup_all’
    # ‘llvm.loop.licm_versioning.disable’
    # ‘llvm.loop.distribute.enable’
    # ‘llvm.loop.distribute.followup_coincident’
    # ‘llvm.loop.distribute.followup_sequential’
    # ‘llvm.loop.distribute.followup_fallback’
    # ‘llvm.loop.distribute.followup_all’
end

function loopinfo(name, expr, nodes...)
    if expr.head != :for
        error("Syntax error: pragma $name needs a for loop")
    end
    push!(expr.args[2].args, Expr(:loopinfo, nodes...))
    return expr
end

"""
   @unroll expr

Takes a for loop as `expr` and informs the LLVM unroller to fully unroll it, if
it is safe to do so and the loop count is known.
"""
macro unroll(expr)
    expr = loopinfo("@unroll", expr, MD.unroll_full())
    return esc(expr)
end

"""
    @unroll N expr

Takes a for loop as `expr` and informs the LLVM unroller to unroll it `N` times,
if it is safe to do so.
"""
macro unroll(N, expr)
    if !(N isa Integer)
        error("Syntax error: `@unroll N expr` needs a constant integer N")
    end
    expr = loopinfo("@unroll", expr, MD.unroll_count(N))
    return esc(expr)
end

macro jam(N, expr)
    if !(N isa Integer)
        error("Syntax error: `@jam N expr` needs a constant integer N")
    end
    expr = loopinfo("@jam", expr, MD.jam_count(N))
    return esc(expr)
end

macro jam(expr)
    expr = loopinfo("@jam", expr, MD.jam_enable())
    return esc(expr)
end

end #module

# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Like UnitRange{Int}, but can handle the `last` field being temporarily
< first (this can happen during compacting) with normalizing it
"""
struct StmtRange <: AbstractUnitRange{Int}
    start::Int
    stop::Int
end

first(r::StmtRange) = r.start
last(r::StmtRange) = r.stop
iterate(r::StmtRange, state=0) = (last(r) - first(r) < state) ? nothing : (first(r) + state, state + 1)

StmtRange(range::UnitRange{Int}) = StmtRange(first(range), last(range))

function getindex(v::StmtRange, i::Int)
    @inline
    #i isa Bool && throw(ArgumentError("invalid index: $i of type Bool"))
    val = v.start + (i - 1)
    @boundscheck let _in_unit_range = i > 0 && val <= v.stop && val >= v.start
        _in_unit_range || throw_boundserror(v, i)
    end
    val
end


struct BasicBlock
    stmts::StmtRange
    preds::Vector{Int}
    succs::Vector{Int}
end

function BasicBlock(stmts::StmtRange)
    return BasicBlock(stmts, Int[], Int[])
end

function BasicBlock(old_bb, stmts)
    return BasicBlock(stmts, old_bb.preds, old_bb.succs)
end

copy(bb::BasicBlock) = BasicBlock(bb.stmts, copy(bb.preds), copy(bb.succs))

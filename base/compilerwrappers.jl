# This file is a part of Julia. License is MIT: https://julialang.org/license

_qualifiedname(a, b::Symbol) = :($a.$b)
function _qualifiedname(a, b::Expr)
    @assert isexpr(b, :., 2)
    s = (b.args[2]::QuoteNode).value::Symbol
    return _qualifiedname(_qualifiedname(a, b.args[1]), s)
end

for T in [:BasicBlock, :CFG, :IRCode]
    @eval copy(x::Core.Compiler.$T) = Core.Compiler.copy(x)
end

for T in [:BasicBlock, :CFG]
    @eval ==(a::Core.Compiler.$T, b::Core.Compiler.$T) = Core.Compiler.:(==)(a, b)
end

for name in [
    :InstructionStream,
    :UseRefIterator,
    :DominatedBlocks,
    :SplitPositionIterator,
    :AbstractDict,
    :AbstractSet,
    :ValueIterator,
    :Generator,
    :(Iterators.Filter),
    :(Iterators.Flatten),
]
    T = _qualifiedname(:(Core.Compiler), name)
    @eval iterate(xs::$T) = Core.Compiler.iterate(xs)
    @eval iterate(xs::$T, state) = Core.Compiler.iterate(xs, state)
    @eval IteratorSize(::Type{<:$T}) = SizeUnknown()
end

for T in [
    :IRCode,
    :InstructionStream,
    :Instruction,
    :StmtRange,
    :UseRef,
    :UnitRange,
    :AbstractDict,
]
    @eval getindex(xs::Core.Compiler.$T, args...) = Core.Compiler.getindex(xs, args...)
    @eval setindex!(xs::Core.Compiler.$T, x, args...) =
        Core.Compiler.setindex!(xs, x, args...)
    @eval size(xs::Core.Compiler.$T) = Core.Compiler.size(xs)
    @eval length(xs::Core.Compiler.$T) = Core.Compiler.length(xs)
end

first(r::Core.Compiler.StmtRange) = Core.Compiler.first(r)
last(r::Core.Compiler.StmtRange) = Core.Compiler.last(r)

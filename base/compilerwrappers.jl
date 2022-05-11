# This file is a part of Julia. License is MIT: https://julialang.org/license

for T in [:BasicBlock, :CFG, :IRCode]
    @eval copy(x::Core.Compiler.$T) = Core.Compiler.copy(x)
end

for T in [:BasicBlock, :CFG]
    @eval ==(a::Core.Compiler.$T, b::Core.Compiler.$T) = Core.Compiler.:(==)(a, b)
end

for T in [:InstructionStream, :UseRefIterator, :DominatedBlocks]
    @eval iterate(xs::Core.Compiler.$T) = Core.Compiler.iterate(xs)
    @eval iterate(xs::Core.Compiler.$T, state) = Core.Compiler.iterate(xs, state)
    @eval IteratorSize(::Type{<:Core.Compiler.$T}) = SizeUnknown()
end

for T in [:IRCode, :InstructionStream, :Instruction, :StmtRange, :UseRef]
    @eval getindex(xs::Core.Compiler.$T, args...) = Core.Compiler.getindex(xs, args...)
    @eval setindex!(xs::Core.Compiler.$T, x, args...) =
        Core.Compiler.setindex!(xs, x, args...)
    @eval size(xs::Core.Compiler.$T) = Core.Compiler.size(xs)
    @eval length(xs::Core.Compiler.$T) = Core.Compiler.length(xs)
end

first(r::Core.Compiler.StmtRange) = Core.Compiler.first(r)
last(r::Core.Compiler.StmtRange) = Core.Compiler.last(r)

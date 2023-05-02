# This file is a part of Julia. License is MIT: https://julialang.org/license

module BinaryTreeMutable

# Adopted from
# https://benchmarksgame-team.pages.debian.net/benchmarksgame/description/binarytrees.html#binarytrees

using Base.Threads
using Printf

mutable struct Node
    l::Union{Nothing, Node}
    r::Union{Nothing, Node}
end

function make(n::Int)
    return n === 0 ? Node(nothing, nothing) : Node(make(n-1), make(n-1))
end

function check(node::Node)
    return  1 + (node.l === nothing ? 0 : check(node.l) + check(node.r))
end

function binary_trees(io, n::Int)
    @printf io "stretch tree of depth %jd\t check: %jd\n" n+1 check(make(n+1))

    long_tree = make(n)
    minDepth = 4
    resultSize = div((n - minDepth), 2) + 1
    results = Vector{String}(undef, resultSize)
    Threads.@threads for depth in minDepth:2:n
        c = 0
        niter = 1 << (n - depth + minDepth)
        for _ in 1:niter
            c += check(make(depth))
        end
        index = div((depth - minDepth),2) + 1
        results[index] = @sprintf "%jd\t trees of depth %jd\t check: %jd\n" niter depth c
    end

    for i in results
        write(io, i)
    end

    @printf io "long lived tree of depth %jd\t check: %jd\n" n check(long_tree)
end

end #module

using .BinaryTreeMutable

# Memory usage is 466MB
BinaryTreeMutable.binary_trees(devnull, 16)
GC.gc()

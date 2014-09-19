#
# The Computer Language Benchmarks Game
# binary-trees benchmark
# http://shootout.alioth.debian.org/u32/performance.php?test=binarytrees
#
# Ported from an OCaml version
#

abstract BTree

type Empty <: BTree
end

type Node <: BTree
    info
    left::BTree
    right::BTree
end

function make(val, d)
    if d == 0
        Node(val, Empty(), Empty())
    else
        nval = val * 2
        Node(val, make(nval-1, d-1), make(nval, d-1))
    end
end

check(t::Empty) = 0
check(t::Node) = t.info + check(t.left) - check(t.right)

function loop_depths(d, min_depth, max_depth)
    for i = 0:div(max_depth - d, 2)
        niter = 1 << (max_depth - d + min_depth)
        c = 0
        for j = 1:niter
            c += check(make(i, d)) + check(make(-i, d))
        end
#        @printf("%i\t trees of depth %i\t check: %i\n", 2*niter, d, c)
        d += 2
    end
end

function binary_trees(N::Int=10)
    const min_depth = 4
    const max_depth = N
    const stretch_depth = max_depth + 1

    # create and check stretch tree
    let c = check(make(0, stretch_depth))
#        @printf("stretch tree of depth %i\t check: %i\n", stretch_depth, c)
    end

    long_lived_tree = make(0, max_depth)

    loop_depths(min_depth, min_depth, max_depth)
#    @printf("long lived tree of depth %i\t check: %i\n", max_depth, check(long_lived_tree))
end


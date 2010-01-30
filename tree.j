struct EmptyTree
end

struct TreeNode[T]
    data:: T
    left:: Union(EmptyTree,TreeNode[T])
    right::Union(EmptyTree,TreeNode[T])
end

typealias Tree[T] Union(EmptyTree,TreeNode[T])

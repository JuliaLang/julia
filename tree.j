type EmptyTree
end

type TreeNode[`T]
    data:: T
    left:: Union[EmptyTree,TreeNode[T]]
    right::Union[EmptyTree,TreeNode[T]]
end

typealias Tree Union[EmptyTree,TreeNode[`T]]

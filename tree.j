type Node[`T]
    data::T
    left::Nullable[Node[T]]
    right::Nullable[Node[T]]
end

typealias Tree Nullable[Node[`T]]

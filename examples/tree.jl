abstract Tree{K,V}

type EmptyTree{K,V} <: Tree{K,V}
end

type TreeNode{K,V} <: Tree{K,V}
    key::  K
    data:: V
    left:: Tree{K,V}
    right::Tree{K,V}
end

type BTree{K,V}
    root:: Tree{K,V}

    BTree() = new(EmptyTree{K,V}())
end

has(t::EmptyTree, key) = false
has(t::BTree, key) = has(t.root, key)

function has(t::TreeNode, key)
    if t.key == key
        true
    elseif key < t.key
        has(t.left, key)
    else
        has(t.right, key)
    end
end

getindex(t::EmptyTree, k) = throw(KeyError(k))
getindex(t::BTree, k) = t.root[k]

function getindex(t::TreeNode, key)
    if t.key == key
        t.data
    elseif key < t.key
        t.left[key]
    else
        t.right[key]
    end
end

assign{K,V}(t::EmptyTree{K,V}, v, k) = TreeNode{K,V}(k, v, t, t)
assign(t::BTree, v, k) = (t.root = assign(t.root, v, k); t)

function assign(t::TreeNode, v, k)
    if t.key == k
        t.data = v
    elseif k < t.key
        t.left = assign(t.left, v, k)
    else
        t.right = assign(t.right, v, k)
    end
    t
end

del(t::EmptyTree, k) = throw(KeyError(k))
del(t::BTree, k) = (t.root = del(t.root, k); t)

function del(t::TreeNode, k)
    if t.key == k
        if isa(t.right,EmptyTree)
            t = t.left
        elseif isa(t.left,EmptyTree)
            t = t.right
        else
            r = t.right
            t = t.left
            insert(t, r)
        end
    elseif k < t.key
        t.left = del(t.left, k)
    else
        t.right = del(t.right, k)
    end
    t
end

insert(t::EmptyTree, r::TreeNode) = r

function insert(t::TreeNode, r::TreeNode)
    if r.key < t.key
        t.left = insert(t.left, r)
    else
        t.right = insert(t.right, r)
    end
    t
end

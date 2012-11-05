module Tries
using Base

import Base.assign,
       Base.get,
       Base.has,
       Base.keys

export Trie,
       assign,
       get,
       has,
       keys,
       keys_with_prefix,
       subtrie

type Trie{T}
    value::T
    children::Dict{Char,Trie{T}}
    is_key::Bool

    function Trie()
        self = new()
        self.children = (Char=>Trie{T})[]
        self.is_key = false
        self
    end
end

Trie() = Trie{Any}()

function assign{T}(t::Trie{T}, val::T, key::String)
    node = t
    for char in key
        if !has(node.children, char)
            node.children[char] = Trie{T}()
        end
        node = node.children[char]
    end
    node.is_key = true
    node.value = val
end

function subtrie(t::Trie, prefix::String)
    node = t
    for char in prefix
        if !has(node.children, char)
            return nothing
        else
            node = node.children[char]
        end
    end
    node
end

function has(t::Trie, key::String)
    node = subtrie(t, key)
    node != nothing && node.is_key
end

get(t::Trie, key::String) = get(t, key, nothing)
function get(t::Trie, key::String, notfound)
    node = subtrie(t, key)
    if node != nothing && node.is_key
        return node.value
    end
    notfound
end

function keys(t::Trie, prefix::String, found)
    if t.is_key
        push(found, prefix)
    end
    for (char,child) in t.children
        keys(child, strcat(prefix,char), found)
    end
end
keys(t::Trie, prefix::String) = (found=String[]; keys(t, prefix, found); found)
keys(t::Trie) = keys(t, "")

function keys_with_prefix(t::Trie, prefix::String)
    st = subtrie(t, prefix)
    st != nothing ? keys(st,prefix) : []
end

end # module

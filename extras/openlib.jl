
# TODO: collapse single-child chains
type Trie{T}
    value::T
    children::HashTable{Char,Trie{T}}
    is_key::Bool

    function Trie{T}()
        self = new()
        self.children = HashTable{Char,Trie{T}}()
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

type DLCache
    libnames::Trie{Bool}
    prefix::String
    suffix::String

    function DLCache()
        libnames = Trie{Bool}()
        prefix = "lib"
        suffix = ".so"
        for line in readlines(stdout(`/sbin/ldconfig -p`))
            m = match(r"^\s+lib(\S+)", line)
            if m != nothing
                name = m.captures[1]
                libnames[name] = true
            end
        end
        new(libnames, prefix, suffix)
    end
end

function lookup(cache::DLCache, name::String)
    re = Regex(I"^(?:$(cache.prefix))?(\S+)(?:$(cache.suffix))?\$")
    name = match(re,name).captures[1]
    name_with_suffix = strcat(name, cache.suffix)
    candidates = keys_with_prefix(cache.libnames, name_with_suffix)
    if length(candidates) > 0
        name_with_suffix = candidates[1]
    end
    strcat(cache.prefix, name_with_suffix)
end

const _jl_dlcache = DLCache()

function openlib(name::String)
    libname = lookup(_jl_dlcache, name)
println(libname)
    dlopen(libname)
end


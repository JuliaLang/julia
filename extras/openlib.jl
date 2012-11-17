
load("trie")

module Openlib
using Base
using Tries

export openlib

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

const UNAME = strip(readall(stdout(`uname`)))

if UNAME == "Linux"
    global const _jl_dlcache = DLCache()
else
    global const _jl_dlcache = nothing
end

function openlib(name::String)
    libname = _jl_dlcache != nothing ? lookup(_jl_dlcache, name) : name
    dlopen(libname)
end

end # module

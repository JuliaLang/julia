# This provides the possibility to associate metadata with (most)
# objects within Julia.  Probably the main purpose is to allow user
# documentation.
#
# See https://github.com/JuliaLang/julia/issues/3988
#
# (Note this has nothing to do with METADATA.jl which is used when
# adding new, external packages to Julia)

# Each metadata entry is just a dict Any=>Any:
typealias MetaData Dict{Any,Any}

# The Dict for _META: ideally this would be a typed, weak-key
# ObjectIdDict.  --> make one...

#typealias MetaDict WeakKeyDict{Any, MetaData} # Problem: does not work for immutables
#typealias MetaDict Dict{Any, MetaData} # Problem: if a mutable changes the hash changes
typealias MetaDict ObjectIdDict

_META = MetaDict() # this holds all the metadata

getmeta(obj) = _META[obj]
getmeta(obj, default) = get(_META, obj, default)

getmeta!(obj) = haskey(_META, obj) ? _META[obj] : (_META[obj] = MetaData())
getmeta!(obj, md::MetaData) = haskey(_META, obj) ? _META[obj] : (_META[obj] = md)

setmeta!(obj, md::MetaData) = _META[obj] = md

hasmeta(obj) = haskey(_META, obj)

# note: this only clears the central Base._META dict:
empty_META!() = (global _META = MetaDict(); nothing)
deletemeta!(obj) = (delete!(_META, obj); nothing)

# #####
# # some functions which may or may not be needed:

# For some immutables metadata is automatically associated again, for
# others like strings it is not:
function copywithmeta(obj)
    cop = copy(obj)
    if hasmeta(obj)
        setmeta!(cop, getmeta(obj))
    end
    return cop
end
# function deepcopywithmeta(obj)
#     error("not implemented")
# end

# function sizeofwithmeta(obj)
#     sizeof(obj) + sizeof(getmeta(obj))
# end



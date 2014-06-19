# This provides the possibility to associate metadata with (most)
# objects within Julia.  Probably the main purpose is to allow user
# documentation.  The interface follows that of Dict as much as
# possible.
#
# See https://github.com/JuliaLang/julia/issues/3988
#
# (Note this has nothing to do with METADATA.jl which is used when
# adding new, external packages to Julia)

# Each metadata entry is just a dict Any=>Any:
typealias MetaData Dict{Any,Any}

# The Dict for META:
typealias MetaDict WeakObjectIdDict{Any, MetaData}
#typealias MetaDict WeakKeyDict{Any, MetaData} # Problem: does not work for immutables
#typealias MetaDict Dict{Any, MetaData} # Problem: if a mutable changes the hash changes
#typealias MetaDict ObjectIdDict  # Problem: does not free references

META = MetaDict() # this holds all the metadata

## metadata interface:
# (If defining type which also holds metadata, implement the "#*"
# methods for it)

hasmeta(obj) = haskey(META, obj) #*
hasmeta(obj, key) = hasmeta(obj) && haskey(getmeta(obj), key)

getmeta(obj) = META[obj] #*
getmeta(obj, key) = getmeta(obj)[key]
function getmeta(obj, key, default)
    if hasmeta(obj)
        return get!(getmeta(obj), key, default)
    else
        default
    end
end

getmeta!(obj) = get!(META, obj, MetaData()) #*
function getmeta!(obj, key, default) 
    md = getmeta!(obj)
    return get!(md, key, default)
end

setmeta!(obj, md::MetaData) = ( META[obj] = md ) #*
function setmeta!(obj, key, value) 
    md = getmeta!(obj)
    md[key] = value
end

# This only clears the central Base.META dict, meta-storage in
# used-defined types will not be affected:
emptymeta!() = (global META = MetaDict(); nothing)

deletemeta!(obj) = (delete!(META, obj); nothing) #*
deletemeta!(obj, key) = (delete!(getmeta(obj), key); nothing) 

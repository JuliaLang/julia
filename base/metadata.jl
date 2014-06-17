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

# The Dict for _META: ideally this would be a typed, weak-key
# ObjectIdDict.  --> make one...

#typealias MetaDict WeakKeyDict{Any, MetaData} # Problem: does not work for immutables
#typealias MetaDict Dict{Any, MetaData} # Problem: if a mutable changes the hash changes
#typealias MetaDict ObjectIdDict
typealias MetaDict WeakObjectIdDict{Any, MetaData}

const secret_default = :__c782dbf1cf4d6a2e5e3865d7e95634f2e09b5903__

_META = MetaDict() # this holds all the metadata

getmeta(obj) = _META[obj]
getmeta(obj, key) = _META[obj][key]
function getmeta(obj, key, default)
    out = get(_META, obj, secret_default)
    out==secret_default ? default : out
end

getmeta!(obj) = get!(_META, obj, MetaData())
function getmeta!(obj, key, default) 
    md = getmeta!(obj)
    return get!(md, key, default)
end

setmeta!(obj, md::MetaData) = ( _META[obj] = md )
function setmeta!(obj, key, value) 
    md = getmeta!(obj)
    md[key] = value
end

hasmeta(obj) = haskey(_META, obj)
hasmeta(obj, key) = hasmeta(obj) && haskey(getmeta(obj), key)

# note: this only clears the central Base._META dict, meta-storage in
# used-defined types will not be affected:
emptymeta!() = (global _META = MetaDict(); nothing)
deletemeta!(obj) = (delete!(_META, obj); nothing)
deletemeta!(obj, key) = (delete!(_META[obj], key); nothing)

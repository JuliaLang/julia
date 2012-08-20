
module Named
	
import Base.*

export NamedIndex, SimpleIndex, NamedVector,
	length, isempty, names, copy, names!, replace_names!,
	replace_names, has, keys, push, del, ref,
	select, select_kv,
	set_group, set_groups, get_groups, isgroup,
	start, done, next, show
	

# an AbstractIndex is a thing that can be used to look up ordered things by name, but that
# will also accept a position or set of positions or range or other things and pass them
# through cleanly.
# an NamedIndex is the usual implementation, with ByteString names.
# a SimpleIndex only works if the things are integer indexes, which is weird.
abstract AbstractIndex

type NamedIndex <: AbstractIndex   # an OrderedDict would be nice here...
    lookup::Dict{ByteString,Indices}      # name => names array position
    names::Vector{ByteString}
end
NamedIndex{T<:ByteString}(x::Vector{T}) = NamedIndex(Dict{ByteString, Indices}(tuple(x...), tuple([1:length(x)]...)),
                                           make_unique(convert(Vector{ByteString}, x)))
NamedIndex() = NamedIndex(Dict{ByteString,Indices}(), ByteString[])
length(x::NamedIndex) = length(x.names)
names(x::NamedIndex) = copy(x.names)
copy(x::NamedIndex) = NamedIndex(copy(x.lookup), copy(x.names))

function names!(x::NamedIndex, nm::Vector)
    if length(nm) != length(x)
        error("lengths don't match.")
    end
    for i in 1:length(nm)
        del(x.lookup, x.names[i])
        x.lookup[nm[i]] = i
    end
    x.names = nm
end

# TODO: replace_names!(ni, 1, "one")
function replace_names!(x::NamedIndex, from::Vector, to::Vector)
    if length(from) != length(to)
        error("lengths of from and to don't match.")
    end
    for idx in 1:length(from)
        if has(x, from[idx]) && !has(x, to[idx])
            x.lookup[to[idx]] = x.lookup[from[idx]]
            x.names[x.lookup[from[idx]]] = to[idx]
            del(x.lookup, from[idx])
        end
    end
    x.names
end
replace_names!(x::NamedIndex, from, to) = replace_names!(x, [from], [to])
replace_names(x::NamedIndex, from, to) = replace_names!(copy(x), from, to)

has(x::NamedIndex, key) = has(x.lookup, key)
keys(x::NamedIndex) = names(x)
function push(x::NamedIndex, nm)
    x.lookup[nm] = length(x) + 1
    push(x.names, nm)
end
function del(x::NamedIndex, idx::Integer)
    # reset the lookup's beyond the deleted item
    for i in idx+1:length(x.names)
        x.lookup[x.names[i]] = i - 1
    end
    gr = get_groups(x)
    del(x.lookup, x.names[idx])
    del(x.names, idx)
    # fix groups:
    for (k,v) in gr
        newv = [[has(x, vv) ? vv : ASCIIString[] for vv in v]...]
        set_group(x, k, newv)
    end
end
function del(x::NamedIndex, nm)
    if !has(x.lookup, nm)
        return
    end
    idx = x.lookup[nm]
    del(x, idx)
end

ref{T<:ByteString}(x::NamedIndex, idx::Vector{T}) = [[x.lookup[i] for i in idx]...]
ref{T<:ByteString}(x::NamedIndex, idx::T) = x.lookup[idx]

# fall-throughs, when something other than the index type is passed
ref(x::AbstractIndex, idx::Int) = idx
ref(x::AbstractIndex, idx::Vector{Int}) = idx
ref(x::AbstractIndex, idx::Range{Int}) = [idx]
ref(x::AbstractIndex, idx::Range1{Int}) = [idx]
ref(x::AbstractIndex, idx::Vector{Bool}) = [1:length(x)][idx]
#ref(x::AbstractIndex, idx::AbstractDataVec{Bool}) = x[nareplace(idx, false)]
#ref(x::AbstractIndex, idx::AbstractDataVec{Int}) = x[nafilter(idx)]

type SimpleIndex <: AbstractIndex
    length::Integer
end
SimpleIndex() = SimpleIndex(0)
length(x::SimpleIndex) = x.length
names(x::SimpleIndex) = nothing

# Chris's idea of namespaces adapted by Harlan for column groups
function set_group(idx::NamedIndex, newgroup, names)
    if !has(idx, newgroup) || isa(idx.lookup[newgroup], Array)
        idx.lookup[newgroup] = [[idx.lookup[nm] for nm in names]...]
    end
end
function set_groups(idx::NamedIndex, gr::Dict)  # {ByteString,Vector{ByteString}}) breaks; duck type
    for (k,v) in gr
        if !has(idx, k) 
            idx.lookup[k] = [[idx.lookup[nm] for nm in v]...]
        end
    end
end
function get_groups(idx::NamedIndex)
    gr = Dict{ByteString,Vector{ByteString}}()
    for (k,v) in idx.lookup
        if isa(v,Array)
            gr[k] = idx.names[v]
        end
    end
    gr
end
function isgroup(idx::NamedIndex, name::ByteString)
  if has(idx, name)
    return isa(idx.lookup[name], Array)
  else
    return false
  end
end


# special pretty-printer for groups, which are just Dicts.
function pretty_show(io, gr::Dict{ByteString,Vector{ByteString}})
    allkeys = keys(gr)
    for k = allkeys
        print(io, "$(k): ")
        print(io, join(gr[k], ", "))
        if k != last(allkeys)
            print(io, "; ")
        end
    end
end


type NamedVector{V} <: Associative{ByteString,V}
    idx::NamedIndex
    arr::Vector{V}

    NamedVector() = new(NamedIndex(), Array(V,0))
end
NamedVector() = NamedVector{Any}()

function NamedVector{K<:ByteString,V}(a::Associative{K,V})
	ret = NamedVector{V}()
	for k in keys(a)
		ret[k] = a[k]
	end
	ret
end

# assignment by a string replaces or appends
function assign(id::NamedVector, v, key::ByteString)
    if has(id.idx, key)
        id.arr[id.idx[key]] = v
    else
        push(id.arr, v)
        push(id.idx, key)
    end
end
# assignment by an integer replaces or throws an error
assign(id::NamedVector, v, pos) = id.arr[pos] = v

ref(id::NamedVector, i) = id.arr[id.idx[i]]

function get{K}(id::NamedVector{K}, key, deflt)
    try
        id[key]
    catch e
        deflt
    end
end

names(nv::NamedVector) = names(nv.idx)
keys(nv::NamedVector) = names(nv)
select(nv::NamedVector, r) = nv[r]
select_kv(nv::NamedVector, r) = (names(nv)[r], nv[r])

has(id::NamedVector, key) = has(id.idx, key)
isempty(id::NamedVector) = isempty(id.arr)
length(id::NamedVector) = length(id.arr)

start(id::NamedVector) = 1
done(id::NamedVector, i) = i > length(id)
next(id::NamedVector, i) = (id[i], i+1)

function show(io, id::NamedVector)
    n = names(id.idx)
    for i = 1:min(length(id), 9)
        println(io, "$i, $(n[i]): $(id[i])")
    end
    if length(id) > 9
        println(io, "...")
    end
end

# this is handy -- should maybe go elsewhere?
function make_unique{S<:ByteString}(names::Vector{S})
    x = NamedIndex()
    names = copy(names)
    dups = Int[]
    for i in 1:length(names)
        if has(x, names[i])
            push(dups, i)
        else
            push(x, names[i])
        end
    end
    for i in dups
        nm = names[i]
        newnm = nm
        k = 1
        while true
            newnm = "$(nm)_$k"
            if !has(x, newnm)
                push(x, newnm)
                break
            end
            k += 1
        end
        names[i] = newnm
    end
    names
end

end #module

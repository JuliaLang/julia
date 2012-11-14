# When building data structures for use with interactive data manipuluation, 
# it is often handy to have data structures that can be efficiently indexed
# by position (like a vector) as well as readably indexed by name (like a
# hash table). The NamedIndex type here maps unique names to integer 
# indexes, looking up ByteString indexes in a Dict and passing other
# index types through. The NamedVector type implements a simple use case,
# with a backing vector of an arbitrary type. 

module Named
	
import Base.*

export NamedIndex, SimpleIndex, NamedVector,
	length, isempty, names, copy, names!, replace_names!,
	replace_names, has, keys, values, push, del, ref,
	select, select_kv,
	set_group, set_groups, get_group, get_groups, isgroup,
	start, done, next, show
	
# should this go elsewhere?
promote_rule(::Type{Union(UTF8String,ASCIIString)}, ::Type{ASCIIString} ) = Union(UTF8String,ASCIIString)

# an AbstractIndex is a thing that can be used to look up ordered things by name, but that
# will also accept a position or set of positions or range or other things and pass them
# through cleanly.
# an NamedIndex is the usual implementation, with ByteString names.
# a SimpleIndex only works if the things are integer indexes, which is weird.
abstract AbstractIndex

type NamedIndex <: AbstractIndex   # an OrderedDict would be nice here...
    lookup::Dict{ByteString,Indices}      # name => names array position
	groups::Dict  					  	# name => [other names]
    names::Vector{ByteString}
end

function NamedIndex{T<:ByteString}(x::Vector{T})
	NamedIndex(Dict{ByteString, Indices}(tuple(x...), tuple([1:length(x)]...)),
			   Dict(),
               make_unique(convert(Vector{ByteString}, x)))
end
NamedIndex() = NamedIndex(Dict{ByteString,Indices}(), Dict(), ByteString[])

length(x::NamedIndex) = length(x.names)
names(x::NamedIndex) = copy(x.names)
copy(x::NamedIndex) = NamedIndex(copy(x.lookup), copy(x.groups), copy(x.names))

# note: replacing names removes any groups no longer present
function names!(x::NamedIndex, nm::Vector)
    if length(nm) != length(x)
        error("lengths don't match.")
    end
    for i in 1:length(nm)
        del(x.lookup, x.names[i])
        x.lookup[nm[i]] = i
    end
    x.names = nm
	clean_groups!(x, nm)
	x.names
end

function clean_groups!(x::NamedIndex, nm::Vector)
	for gr_key in keys(x.groups)
		new_val = intersect(x.groups[gr_key], nm)
		if length(new_val) == 0
			del(x.groups, gr_key)
		else
			x.groups[gr_key] = new_val
		end
	end
end

function replace_names!(x::NamedIndex, indexes::Vector{Int}, new_names::Vector)
    if length(indexes) != length(new_names)
        error("lengths of indexes and new_names don't match.")
    end
	# it's tricky to figure out if we're ending up with dupes without trying it...
	# for now, copy twice to avoid clobbering the caller if we end up failing
	newx = copy(x)
    for i = 1:length(indexes)
		# remove the old lookup
		del(newx.lookup, newx.names[i])
		# add the new lookup
		newx.lookup[new_names[i]] = indexes[i]
		# replace the name
		newx.names[indexes[i]] = new_names[i]
    end
	# make sure we didn't screw anything up
	if length(keys(x.lookup)) != length(x.names)
		error("replacement failed -- ended up with duplicate names")
	end
	x.names = newx.names
	x.lookup = newx.lookup
	clean_groups!(x, x.names)
    x.names
end
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
	clean_groups!(x, x.names)
    x.names
end
replace_names!(x::NamedIndex, from, to) = replace_names!(x, [from], [to])
replace_names(x::NamedIndex, from, to) = replace_names!(copy(x), from, to)

# groups work with ref(), but not has(), keys(), etc
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
    del(x.lookup, x.names[idx])
    del(x.names, idx)
    # fix groups
	clean_groups!(x, x.names)
end
function del(x::NamedIndex, nm)
    if !has(x.lookup, nm)
        return
    end
    idx = x.lookup[nm]
    del(x, idx)
end

# ref should properly deal with the new groups
function ref{T<:ByteString}(x::NamedIndex, idx::Vector{T})
	# expand any groups, then do the comprehension
	idx2 = T[]
	for i in idx
		if isgroup(x, i)
			push(idx2, get_group(x, i))
		else
			push(idx2, i)
		end
	end
	[[x.lookup[i] for i in idx]...]
end
function ref{T<:ByteString}(x::NamedIndex, idx::T)
	if isgroup(x, idx)
		ref(x, get_group(x, idx))
	else
		x.lookup[idx]
	end
end

# if we get a symbol or a vector of symbols, convert to strings for ref
ref(x::NamedIndex, idx::Symbol) = x[string(idx)]
ref(x::NamedIndex, idx::Vector{Symbol}) = [[x.lookup[string(i)] for i in idx]...]

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

# Chris DuBois' idea: named
function set_group(idx::NamedIndex, newgroup, names)
	# confirm that the names exist
	for name in names
		if !has(idx, name)
			error("can't add group referring to non-existent name!")
		end
	end
	# add the group
	idx.groups[newgroup] = Set(names...)
end
function set_groups(idx::NamedIndex, gr::Dict)  # {ByteString,Vector{ByteString}}) breaks; duck type
    for (k,v) in gr
        set_group(idx, k, v)
    end
end
get_group(idx::NamedIndex, name) = elements(idx.groups[name]) # set -> array
get_groups(idx::NamedIndex) = idx.groups # returns a dict to sets, which may not be what you want
isgroup(idx::NamedIndex, name::ByteString) = has(idx.groups, name)

function show(io, idx::NamedIndex)
    println(io, "$(length(idx))-element NamedIndex")
    pretty_show(io, idx.groups)
    for i = 1:min(length(idx), 9)
        println(io, "$i = $(names(idx)[i])")
    end
    if length(idx) > 9
        println(io, "...")
    end
end

# special pretty-printer for groups, which are just Dicts.
function pretty_show(io, gr)
    allkeys = keys(gr)
    for k = allkeys
        print(io, "$(k): ")
        print(io, join(gr[k], ", "))
        if k == last(allkeys)
            println(io)
        else
            print(io, "; ")
        end
    end
end

############ NamedVector #############

type NamedVector{V} <: AbstractVector{V}
    idx::NamedIndex
    arr::Vector{V}
	#NamedVector() = new(NamedIndex(), Array(V,0))
end
NamedVector() = NamedVector(NamedIndex(), Array(Any,0))
NamedVector(T::Type) = NamedVector(NamedIndex(), Array(T,0))
#NamedVector(i::NamedIndex, a::Vector) = NamedVector(i,a)

function NamedVector{K<:ByteString,V}(a::Associative{K,V})
	ret = NamedVector(V)
	for k in keys(a)
		ret[k] = a[k]
	end
	ret
end

function NamedVector{K<:ByteString,V}(keys::Vector{K}, values::Vector{V})
	ret = NamedVector(V)
	for i = 1:length(keys)
		ret[keys[i]] = values[i]
	end
	ret
end

# similar's a little funny -- we make a similar array, but if the new vector's
# longer than what we started with, the new keys are weird
function similar(nv::NamedVector, element_type, dims)
    new_arr = similar(nv.arr, element_type, dims)
    if dims == size(nv)
        new_idx = copy(nv.idx)
    elseif dims < size(nv)
        new_idx = NamedIndex(names(nv.idx)[1:dims[1]])
    else
        new_names = vcat(names(nv.idx), fill("X", dims[1]-length(nv)))
        new_names = make_unique(new_names)
        new_idx = NamedIndex(new_names)
    end
    NamedVector(new_idx, new_arr)
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
assign(id::NamedVector, v, key::Symbol) = assign(id, v, string(key))
# assignment by an integer replaces or throws an error
assign(id::NamedVector, v, pos::Integer) = id.arr[pos] = v
assign(id::NamedVector, v, pos::Real) = assign(id, v, iround(pos))
assign(id::NamedVector, v, pos) = id.arr[pos] = v

ref{V,T<:Integer}(id::NamedVector{V}, ii::AbstractArray{T,1}) = id.arr[id.idx[ii]]
ref(id::NamedVector, i::Integer) = id.arr[id.idx[i]]
ref(id::NamedVector, i::Real) = ref(id, iround(i))
ref(id::NamedVector, i) = id.arr[id.idx[i]]
# ref gives the vector value; find gives the _position_ in the underlying NamedIndex
find(id::NamedVector, v) = has(id, v) ? id.idx[v] : 0

function get{K}(id::NamedVector{K}, key, deflt)
    try
        id[key]
    catch e
        deflt
    end
end

size(nv::NamedVector) = size(nv.arr)
names(nv::NamedVector) = names(nv.idx)
keys(nv::NamedVector) = names(nv)
values(nv::NamedVector) = nv.arr
select(nv::NamedVector, r::Int64) = nv[r]
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
    println(io, "$(length(id))-element $(eltype(id.arr)) NamedVector")
    pretty_show(io, id.idx.groups)
    for i = 1:min(length(id), 9)
        println(io, "$i, $(n[i]): $(id[i])")
    end
    if length(id) > 9
        println(io, "...")
    end
end
repl_show(io, id::NamedVector) = show(io, id::NamedVector)

# copying has to copy everything, because otherwise you can append an item,
# which can cause the vector to change length and break the original
copy(nv::NamedVector) = NamedVector(copy(nv.idx), copy(nv.arr))

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

set_group(v::NamedVector, newgroup, names) = set_group(v.idx, newgroup, names)
set_groups(v::NamedVector, gr) = set_groups(v.idx, gr)

get_group(v::NamedVector, name) = get_group(v.idx, name)
get_groups(v::NamedVector) = get_groups(v.idx)
isgroup(v::NamedVector, name::ByteString) = isgroup(v.idx, name)

end #module

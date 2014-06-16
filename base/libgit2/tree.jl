export GitTreeEntry, 
       entry_byname, entry_bypath,
       each_tree, each_blob,
       walk_trees, walk_blobs

type GitTreeEntry{T<:GitObject}
    name::String
    oid::Oid
    filemode::Cint
    owns::Bool
end

Oid(te::GitTreeEntry)  = te.oid
name(te::GitTreeEntry) = te.name

Base.isless(te1::GitTreeEntry, te2::GitTreeEntry) = isless(Oid(te1), Oid(te2))

Base.length(t::GitTree) = int(api.git_tree_entrycount(t.ptr))
Base.filemode(te::GitTreeEntry) = convert(Cint, te.filemode)

let 
    function tree_entry_name(ptr::Ptr{Void})
        nptr::Ptr{Cchar} = api.git_tree_entry_name(ptr)
        @assert nptr != C_NULL
        return bytestring(nptr)
    end

    function tree_entry_oid(ptr::Ptr{Void})
        idptr::Ptr{Uint8} = api.git_tree_entry_id(ptr)
        @assert idptr != C_NULL
        return Oid(idptr)
    end

    function tree_entry_type(ptr::Ptr{Void})
        t = api.git_tree_entry_type(ptr)
        t == api.OBJ_BLOB   && return GitBlob
        t == api.OBJ_COMMIT && return GitCommit
        t == api.OBJ_TAG    && return GitTag
        t == api.OBJ_TREE   && return GitTree
        error("unknown git_type $(t)")
    end

    tree_entry_filemode(ptr::Ptr{Void}) = api.git_tree_entry_filemode(ptr)

    #TODO: ownership of tree entry pointer 

    function GitTreeEntry(ptr::Ptr{Void}, owns::Bool=false)
        @assert ptr != C_NULL
        ty   = tree_entry_type(ptr)
        name = tree_entry_name(ptr)
        oid  = tree_entry_oid(ptr)
        fm   = tree_entry_filemode(ptr)
        return GitTreeEntry{ty}(name, oid, fm, owns)
    end
end

function entry_byname(t::GitTree, filename::String)
    @assert t.ptr != C_NULL
    bname = bytestring(filename)
    entry_ptr = api.git_tree_entry_byname(t.ptr, bname)
    entry_ptr == C_NULL && return nothing
    return GitTreeEntry(entry_ptr)
end

function entry_bypath(t::GitTree, path::String)
    @assert t.ptr != C_NULL
    bpath = bytestring(path)
    entry_ptr = Array(Ptr{Void}, 1)
    @check api.git_tree_entry_bypath(entry_ptr, t.ptr, bpath)
    return GitTreeEntry(entry_ptr[1], true)
end

function entry_byindex(t::GitTree, idx::Integer)
    @assert t.ptr != C_NULL
    entry_ptr = api.git_tree_entry_byindex(t.ptr, idx - 1)
    if entry_ptr == C_NULL
        return nothing
    else
        return GitTreeEntry(entry_ptr)
    end
end

function entry_byid(t::GitTree, id::Oid)
    @assert t.ptr != C_NULL
    entry_ptr = api.git_tree_entry_byid(t.ptr, id.oid)
    if entry_ptr == C_NULL
        return nothing
    else
        return GitTreeEntry(entry_ptr)
    end
end

Base.getindex(t::GitTree, entry::Integer) = entry_byindex(t, entry)
Base.getindex(t::GitTree, entry::String)  = entry_byname(t, entry)
Base.getindex(t::GitTree, entry::Oid)     = entry_byid(t, entry)

Base.start(t::GitTree) = begin
    @assert t.ptr != C_NULL
    te = entry_byindex(t, 1)
    return (1, te)
end

Base.done(t::GitTree, state) = state[1] > length(t)

Base.next(t::GitTree, state) = begin
    nidx = state[1] + 1
    (state[2], (nidx, entry_byindex(t, nidx)))
end

function each_blob(t::GitTree)
    @assert t.ptr != C_NULL
    @task for te in t
        isa(te, GitTreeEntry{GitBlob}) && produce(te)
    end
end

function each_tree(t::GitTree)
    @assert t.ptr != C_NULL
    @task for te in t
        isa(te, GitTreeEntry{GitTree}) && produce(te)
    end
end

function cb_treewalk(root::Ptr{Cchar}, entry::Ptr{Void}, data::Ptr{Void})
    try 
        produce(bytestring(root), GitTreeEntry(entry))
        return api.GIT_OK
    catch err
        return api.ERROR
    end
end

const c_cb_treewalk = cfunction(cb_treewalk, Cint, (Ptr{Cchar}, Ptr{Void}, Ptr{Void}))
 
function walk(t::GitTree, order=:postorder)
    @assert t.ptr != C_NULL
    local mode::Cint
    if order == :postorder
        mode = api.TREEWALK_POST
    elseif order == :preorder
        mode = api.TREEWALK_PRE
    else
        throw(ArgumentError("walk order can be :preorder or :postorder, got :$order"))
    end
    @task api.git_tree_walk(t.ptr, mode, c_cb_treewalk, C_NULL)  
end

function walk_blobs(t::GitTree, order=:postorder)
    @task for res in walk(t, order)
        isa(res[2], GitTreeEntry{GitBlob}) && produce(res)
    end
end

function walk_blobs(f::Function, t::GitTree, order=:postorder)
    for res in walk_blobs(t, order) 
        f(res)
    end
end

function walk_trees(t::GitTree, order=:postorder)
    @task for res in walk(t, order)
        isa(res[2], GitTreeEntry{GitTree}) && produce(res)
    end
end

function walk_trees(f::Function, t::GitTree, order=:postorder)
    for te in walk_trees(t, order)
        f(te)
    end
end

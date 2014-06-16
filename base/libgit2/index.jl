export GitIndex, IndexEntry, add_bypath!, write_tree!, write!, reload!, clear!,
       remove!, remove_dir!, add!, getentry, read_tree!, add_all!, update_all!,
       remove_all!, has_conflicts

type GitIndex
    ptr::Ptr{Void}

    function GitIndex(ptr::Ptr{Void})
        @assert ptr != C_NULL
        i = new(ptr)
        finalizer(i, free!)
        return i
    end
end

function GitIndex(path::String)
    index_ptr = Array(Ptr{Void}, 1)
    @check api.git_index_open(index_ptr, bytestring(path))
    return GitIndex(index_ptr[1])
end

free!(i::GitIndex) = begin
    if i.ptr != C_NULL
        api.git_index_free(i.ptr)
        i.ptr = C_NULL
    end
end

function has_conflicts(i::GitIndex)
    @assert i.ptr != C_NULL
    return bool(api.git_index_has_conflicts(i.ptr))
end

function clear!(i::GitIndex)
    @assert i.ptr != C_NULL
    api.git_index_clear(i.ptr)
    return i
end

function reload!(i::GitIndex)
    @assert i.ptr != C_NULL
    @check api.git_index_read(i.ptr, 0)
    return i
end

function write!(i::GitIndex)
    @assert i.ptr != C_NULL
    @check api.git_index_write(i.ptr)
    return i
end 

function remove!(i::GitIndex, path::String, stage::Integer=0)
    @assert i.ptr != C_NULL
    @check api.git_index_remove(i.ptr, bytestring(path), stage)
    return i
end

function remove_dir!(i::GitIndex, path::String, stage::Integer=0)
    @assert i.ptr != C_NULL
    @check api.git_index_remove_directory(i.ptr, bytestring(path), stage)
    return i
end

Base.length(i::GitIndex) = begin
    @assert i.ptr != C_NULL
    return int(api.git_index_entrycount(i.ptr))
end

Base.getindex(i::GitIndex, idx::Int) = begin
    @assert i.ptr != C_NULL
    @assert idx > 0
    entry_ptr = api.git_index_get_byindex(i.ptr, idx-1)
    if entry_ptr == C_NULL
        return nothing
    end
    return IndexEntry(entry_ptr)
end

Base.getindex(i::GitIndex, path::String) = begin
    @assert i.ptr != C_NULL
    entry_ptr = api.git_index_get_bypath(i.ptr, bytestring(path), 0)
    if entry_ptr == C_NULL
        return nothing
    end
    return IndexEntry(entry_ptr)
end

function write_tree!(i::GitIndex)
    @assert i.ptr != C_NULL
    id = Oid()
    @check api.git_index_write_tree(id.oid, i.ptr)
    return id
end

type IndexEntry
    path::String
    oid::Oid
    ctime::Float64
    mtime::Float64
    file_size::Int
    dev::Int
    ino::Int
    mode::Int
    uid::Int
    gid::Int
    valid::Bool
    stage::Int
end

Oid(entry::IndexEntry) = entry.oid

function api.GitIndexEntry(idx::IndexEntry)
    flags = uint16(0x0)
    flags &= ~api.IDXENTRY_STAGEMASK
    flags |= (uint16(idx.stage) << api.IDXENTRY_STAGESHIFT) & api.IDXENTRY_STAGEMASK
    flags &= ~api.IDXENTRY_VALID
    if idx.valid
        flags |= api.IDXENTRY_VALID
    end
    return api.GitIndexEntry(int64(idx.ctime), 
                             zero(Cuint), 
                             int64(idx.mtime), 
                             zero(Cuint), 
                             zero(Cuint), # padding
                             convert(Cuint, idx.dev),
                             convert(Cuint, idx.ino),
                             convert(Cuint, idx.mode),
                             convert(Cuint, idx.uid),
                             convert(Cuint, idx.gid),
                             int64(idx.file_size),
                             idx.oid.oid[1],
                             idx.oid.oid[2],
                             idx.oid.oid[3],
                             idx.oid.oid[4],
                             idx.oid.oid[5],
                             idx.oid.oid[6],
                             idx.oid.oid[7],
                             idx.oid.oid[8],
                             idx.oid.oid[9],
                             idx.oid.oid[10],
                             idx.oid.oid[11],
                             idx.oid.oid[12],
                             idx.oid.oid[13],
                             idx.oid.oid[14],
                             idx.oid.oid[15],
                             idx.oid.oid[16],
                             idx.oid.oid[17],
                             idx.oid.oid[18],
                             idx.oid.oid[19],
                             idx.oid.oid[20],
                             flags,
                             zero(Uint16),
                             convert(Ptr{Cchar}, pointer(idx.path)))
end

function IndexEntry(ptr::Ptr{api.GitIndexEntry})
    @assert ptr != C_NULL
    gentry = unsafe_load(ptr)
    path  = bytestring(gentry.path)
    ctime = gentry.ctime_seconds + (gentry.ctime_nanoseconds / 1e3)
    mtime = gentry.mtime_seconds + (gentry.mtime_nanoseconds / 1e3)
    dev   = int(gentry.dev)
    ino   = int(gentry.ino)
    mode  = int(gentry.mode)
    uid   = int(gentry.uid)
    gid   = int(gentry.gid)
    valid = bool(gentry.flags & api.IDXENTRY_VALID)
    stage = int((gentry.flags & api.IDXENTRY_STAGEMASK) >> api.IDXENTRY_STAGESHIFT)
    file_size = int(gentry.file_size)
    
    #TODO: refactor 
    arr = Array(Uint8, api.OID_RAWSZ)
    @get_oid_fieldnames(arr, gentry, oid)
    id = Oid(arr)

    return IndexEntry(path, id, ctime, mtime, file_size,
                      dev, ino, mode, uid, gid, valid, stage)
end

#TODO: add! is defined in Base
function Base.add!(idx::GitIndex, entry::IndexEntry)
    @assert idx.ptr != C_NULL
    gentry = api.GitIndexEntry(entry)
    @check ccall((:git_index_add, api.libgit2), Cint,
                 (Ptr{Void}, Ptr{api.GitIndexEntry}),
                 idx.ptr, &gentry)
    return idx
end

function Base.add!(idx::GitIndex, path::String)
    @assert idx.ptr != C_NULL
    bpath = bytestring(path)
    @check api.git_index_add_bypath(idx.ptr, bpath)
    return idx
end

Base.getindex(idx::GitIndex, path::String, stage::Int=0) = begin
    return getentry(idx, path, stage)
end

function getentry(idx::GitIndex, path::String, stage=0)
    @assert idx.ptr != C_NULL
    entry_ptr = api.git_index_get_bypath(idx.ptr, bytestring(path), stage)
    if entry_ptr == C_NULL
        return nothing
    end
    return IndexEntry(entry_ptr)
end

Base.(:(==))(e1::IndexEntry, e2::IndexEntry) = begin
    return e1.path  == e2.path &&
           e1.oid   == e2.oid &&
           e1.ctime == e2.ctime && 
           e1.mtime == e2.mtime &&
           e1.file_size == e2.file_size &&
           e1.dev  == e2.dev && 
           e1.ino  == e2.ino &&
           e1.mode == e2.mode &&
           e1.uid  == e2.uid &&
           e1.gid  == e2.gid &&
           e1.valid == e2.valid && 
           e1.stage == e2.stage
end
Base.isequal(e1::IndexEntry, e2::IndexEntry) = e1 == e2

Base.start(idx::GitIndex) = begin
    if length(idx) == 0
        return (0, nothing)
    end
    return (1, idx[1])
end

Base.done(idx::GitIndex, state) = begin
    i = state[1]
    return i == 0 || i > length(idx)
end

Base.next(idx::GitIndex, state) = begin
    i = state[1] + 1
    return (state[2], (i, idx[i]))
end

function read_tree!(idx::GitIndex, tree::GitTree)
    @assert idx.ptr != C_NULL && tree.ptr != C_NULL
    @check api.git_index_read_tree(idx.ptr, tree.ptr)
    return idx
end

function add_all!(idx::GitIndex, pspec::String;
                 force::Bool=false,
                 disable_pathsepc_match::Bool=false,
                 check_pathspec::Bool=false)
    return add_all!(idx, String[pspec],
                    force=force, 
                    disable_pathsepc_match=disable_pathsepc_match,
                    check_pathspec=check_pathspec)
end

function add_all!(idx::GitIndex, pspec::Array{String, 1};
                 force::Bool=false,
                 disable_pathsepc_match::Bool=false,
                 check_pathspec::Bool=false)
    @assert idx.ptr != C_NULL
    flags = api.INDEX_ADD_DEFAULT
    if force
        flags |= api.INDEX_ADD_FORCE
    end
    if disable_pathsepc_match
        flags |= api.INDEX_ADD_DISABLE_PATHSPEC_MATCH
    end
    if check_pathspec
        flags |= api.INDEX_ADD_CHECK_PATHSPEC
    end
    #TODO: memory leak?
    exception_ptr = Cint[0]
    gstr_arr  = api.GitStrArray(pspec)
    @check ccall((:git_index_add_all, api.libgit2), Cint,
                 (Ptr{Void}, Ptr{api.GitStrArray}, Cuint, Ptr{Void}, Ptr{Cint}),
                 idx.ptr, &gstr_arr, flags, C_NULL, exception_ptr)
    if exception_ptr[1] > 0
        #TODO: better exception handling
        error("exception thrown for add_all!: $(exception_ptr[1])")
    end
    return idx
end

function update_all!(idx::GitIndex, pspec::Array{String,1})
    @assert idx.ptr != C_NULL
    exception_ptr   = Cint[0]
    gstr_arr = api.GitStrArray(pspec)
    @check ccall((:git_index_update_all, api.libgit2), Cint,
                 (Ptr{Void}, Ptr{api.GitStrArray}, Ptr{Void}, Ptr{Cint}),
                 idx.ptr, &gstr_arr, C_NULL, exception_ptr)
    if exception_ptr[1] > 0
        #TODO: better exception handling
        error("exception thrown for update_all!: $(exception_ptr[1])")
    end
    return idx
end

update_all!(idx::GitIndex, pspec::String) = update_all!(idx, String[pspec])
update_all!(idx::GitIndex) = update_all!(idx, String[""])

function remove_all!(idx::GitIndex, pspec::Array{String, 1})
    @assert idx.ptr != C_NULL
    exception_ptr   = Cint[0]
    gstr_arr = api.GitStrArray(pspec)
    @check ccall((:git_index_remove_all, api.libgit2), Cint,
                 (Ptr{Void}, Ptr{api.GitStrArray}, Ptr{Void}, Ptr{Cint}),
                 idx.ptr, &gstr_arr, C_NULL, exception_ptr)
    if exception_ptr[1] > 0
        #TODO: better exception handling
        error("exception thrown for update_all!: $(exception_ptr[1])")
    end
    return idx
end

remove_all!(idx::GitIndex, pspec::String) = remove_all!(idx, String[pspec])

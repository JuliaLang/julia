
Base.(:(==))(o1::GitObject, o2::GitObject) = isequal(Oid(o1), Oid(o2))
Base.isequal(o1::GitObject, o2::GitObject) = isequal(Oid(o1), Oid(o2))
Base.isless(o1::GitObject, o2::GitObject)  = isless(Oid(o1), Oid(o2))

Base.hash(o::GitObject) = hash(hex(o))
Base.cmp(o1::GitObject, o2::GitObject) = cmp(Oid(o1), Oid(o2))

Oid(o::GitObject) = begin
    @assert o.ptr != C_NULL
    oid_ptr::Ptr{Uint8} = api.git_object_id(o.ptr)
    @assert oid_ptr != C_NULL
    return Oid(oid_ptr)
end

Base.hex(o::GitObject) = begin
    @assert o.ptr != C_NULL
    oid_ptr::Ptr{Uint8} = api.git_object_id(o.ptr)
    @assert oid_ptr != C_NULL
    hex_buff = Array(Uint8, api.OID_HEXSZ)
    @check api.git_oid_fmt(pointer(hex_buff), oid_ptr)
    return bytestring(hex_buff)
end

function raw(o::GitObject)
    repo_ptr = api.git_object_owner(o.ptr)
    oid_ptr  = api.git_object_id(o.ptr)
    odb_ptr  = Array(Ptr{Void}, 1)
    obj_ptr  = Array(Ptr{Void}, 1)
    @check api.git_repository_odb(odb_ptr, repo_ptr)
    err = api.git_odb_read(obj_ptr, odb_ptr[1], oid_ptr)
    api.git_odb_free(odb_ptr[1])
    if err < 0
        throw(GitError(err))
    end
    return OdbObject(obj_ptr[1])
end

function gitobj_from_ptr(ptr::Ptr{Void})
    @assert ptr != C_NULL
    obj_type = api.git_object_type(ptr) 
    T = gitobj_const_type(obj_type)
    return T(ptr)
end

function gitobj_const_type(obj_type::Integer)
    obj_type == api.OBJ_BLOB   && return GitBlob
    obj_type == api.OBJ_TREE   && return GitTree
    obj_type == api.OBJ_COMMIT && return GitCommit
    obj_type == api.OBJ_TAG    && return GitTag
    obj_type == api.OBJ_ANY    && return GitAny
    error("Unknown git type const: $obj_type")
end

module LibGit

type Ref
    ptr::Ptr{Void}
    function Ref(ptr::Ptr{Void})
        r = new(ptr)
        finalizer(r, r -> ccall((:git_reference_free, :libgit2), Void, (Ptr{Void},), r.ptr))
        return r
    end
end

type Obj
    ptr::Ptr{Void}
    function Obj(ptr::Ptr{Void})
        r = new(ptr)
        finalizer(r, r -> ccall((:git_object_free, :libgit2), Void, (Ptr{Void},), r.ptr))
        return r
    end
end

function repo(path::String)
    repo_ptr = Ptr{Void}[0]
    err = ccall((:git_repository_open, :libgit2), Cint,
                (Ptr{Ptr{Void}}, Ptr{Uint8}), repo_ptr, path)
    if err != 0
        if repo_ptr[1] != C_NULL
            ccall((:git_repository_free, :libgit2), Void, (Ptr{Void},), repo_ptr[1])
        end
        return C_NULL
    end
    return repo_ptr[1]
end

function close(repo_ptr::Ptr{Void})
    ccall((:git_repository_free, :libgit2), Void, (Ptr{Void},), repo_ptr)
end

function repo_head(r::Ptr{Void})
    head_ptr = Ptr{Void}[0]
    err = ccall((:git_repository_head, :libgit2), Cint,
                (Ptr{Ptr{Void}}, Ptr{Void}), head_ptr, r)
    (err != 0) && return nothing
    return Ref(head_ptr[1])
end

function ref_id(ref::Ref)
    ref == nothing && return ""
    oid_ptr = ccall((:git_reference_target, :libgit2), Ptr{UInt8}, (Ptr{Void},), ref.ptr)
    oid_ptr == C_NULL && return ""
    return bytes2hex(pointer_to_array(oid_ptr, 20))
end

function need_update(repo::Ptr{Void})
    ccall((:git_repository_is_bare, :libgit2), Cint, (Ptr{Void},), repo) != 1 && "git update-index -q --really-refresh"
end

function iscommit(id::String, repo::Ptr{Void})
    need_update(repo)

    oid = hex2bytes(id)
    cmt_ptr = Ptr{Void}[0]
    err = ccall((:git_commit_lookup, :libgit2), Cint,
                (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Uint8}), cmt_ptr, repo, oid)
    if err != 0
        return false
    else
        ccall((:git_commit_free, :libgit2), Void, (Ptr{Void},), cmt_ptr[1])
        return true
    end
end

function obj_id(ref::Obj)
    ref == nothing && return ""
    oid_ptr = ccall((:git_object_id, :libgit2), Ptr{UInt8}, (Ptr{Void},), ref.ptr)
    oid_ptr == C_NULL && return ""
    return bytes2hex(pointer_to_array(oid_ptr, 20))
end

function isdirty(repo::Ptr{Void})
    obj_ptr = Ptr{Void}[0]
    obj = "HEAD^{tree}"
    err = ccall((:git_revparse_single, :libgit2), Cint,
               (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Uint8}), obj_ptr, repo, obj)
    err != 0 && return true

    tree_oid = hex2bytes(obj_id(Obj(obj_ptr[1])))
    tree_ptr = Ptr{Void}[0]
    err = ccall((:git_tree_lookup, :libgit2), Cint,
               (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Uint8}), tree_ptr, repo, tree_oid)
    err != 0 && return true

    diff_ptr = Ptr{Void}[0]
    err = ccall((:git_diff_tree_to_workdir_with_index, :libgit2), Cint,
               (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Void}, Ptr{Void}, Ptr{Void}),
               diff_ptr, repo, tree_ptr[1], C_NULL, C_NULL)
    err != 0 && return true

    c = ccall((:git_diff_num_deltas, :libgit2), Cint, (Ptr{Void},), diff_ptr[1])
    c > 0 && return true

    return false
end

function isattached(repo::Ptr{Void})
    ccall((:git_repository_head_detached, :libgit2), Cint, (Ptr{Void},), repo) != 1
end

function merge_base(one::String, two::String, repo::Ptr{Void})
    oid1 = hex2bytes(one)
    oid2 = hex2bytes(two)
    moid = zeros(UInt8 ,20)
    err = ccall((:git_merge_base, :libgit2), Cint,
        (Ptr{UInt8}, Ptr{Void}, Ptr{UInt8}, Ptr{UInt8}), moid, repo, oid1, oid2)
    if err != 0
        return ""
    else
        bytes2hex(moid)
    end
end

function is_ancestor_of(a::AbstractString, b::AbstractString, repo::Ptr{Void})
    #A = readchomp(`rev-parse $a`, dir=dir)
    merge_base(a, b, prepo) == a
end

end
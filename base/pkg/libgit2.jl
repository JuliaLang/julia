module LibGit2

const GITHUB_REGEX =
    r"^(?:git@|git://|https://(?:[\w\.\+\-]+@)?)github.com[:/](([^/].+)/(.+?))(?:\.git)?$"i

function __init__()
    err = ccall((:git_libgit2_init, :libgit2), Cint, ())
    err > 0 || error("error initializing LibGit2 module")
    atexit() do
        gc()
        ccall((:git_libgit2_shutdown, :libgit2), Cint, ())
    end
end

include("libgit2/const.jl")
include("libgit2/error.jl")
include("libgit2/repository.jl")
include("libgit2/config.jl")
include("libgit2/clone.jl")

type GitRef
    ptr::Ptr{Void}
    function GitRef(ptr::Ptr{Void})
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

function head(repo::GitRepo)
    head_ptr = Ptr{Void}[0]
    err = ccall((:git_repository_head, :libgit2), Cint,
                (Ptr{Ptr{Void}}, Ptr{Void}), head_ptr, repo.ptr)

    (err != 0) && return nothing
    return GitRef(head_ptr[1])
end

function ref_id(ref::GitRef)
    ref == nothing && return ""

    typ = ccall((:git_reference_type, :libgit2), Cint, (Ptr{Void},), ref.ptr)
    if typ == 1
        oid_ptr = ccall((:git_reference_target, :libgit2), Ptr{UInt8}, (Ptr{Void},), ref.ptr)
        oid_ptr == C_NULL && return ""
        return bytes2hex(pointer_to_array(oid_ptr, 20))
    else
        oid_ptr = ccall((:git_reference_symbolic_target, :libgit2), Ptr{UInt8}, (Ptr{Void},), ref.ptr)
        oid_ptr == C_NULL && return ""
        return bytestring(oid_ptr)
    end
end

head_oid(repo::GitRepo) = head(repo) |> ref_id

function ref_name(ref::GitRef)
    ref == nothing && return ""

    name_ptr = ccall((:git_reference_shorthand, :libgit2), Ptr{UInt8}, (Ptr{Void},), ref.ptr)
    name_ptr == C_NULL && return ""
    return bytestring(name_ptr)
end

function need_update(repo::GitRepo)
    ccall((:git_repository_is_bare, :libgit2), Cint, (Ptr{Void},), repo.ptr) != 1 && "git update-index -q --really-refresh"
end

function iscommit(id::AbstractString, repo::GitRepo)
    need_update(repo)

    oid = hex2bytes(id)
    cmt_ptr = Ptr{Void}[0]
    err = ccall((:git_commit_lookup, :libgit2), Cint,
                (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Uint8}), cmt_ptr, repo.ptr, oid)
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

function isdirty(repo::GitRepo, paths::AbstractString="")
    tree_oid =revparse(repo, "HEAD^{tree}")
    tree_oid == nothing && return true

    tree_ptr = Ptr{Void}[0]
    err = ccall((:git_tree_lookup, :libgit2), Cint,
               (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Uint8}), tree_ptr, repo.ptr, tree_oid)
    err != 0 && return true

    diff_ptr = Ptr{Void}[0]
    err = ccall((:git_diff_tree_to_workdir_with_index, :libgit2), Cint,
               (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Void}, Ptr{Void}, Ptr{Void}),
               diff_ptr, repo.ptr, tree_ptr[1], C_NULL, C_NULL)
    err != 0 && return true

    if isempty(paths)
        c = ccall((:git_diff_num_deltas, :libgit2), Cint, (Ptr{Void},), diff_ptr[1])
        c > 0 && return true
    else
        # TODO look for specified path
        c = ccall((:git_diff_num_deltas, :libgit2), Cint, (Ptr{Void},), diff_ptr[1])
        c > 0 && return true
    end
    return false
end

function isattached(repo::GitRepo)
    ccall((:git_repository_head_detached, :libgit2), Cint, (Ptr{Void},), repo.ptr) != 1
end

function merge_base(one::AbstractString, two::AbstractString, repo::GitRepo)
    oid1 = hex2bytes(one)
    oid2 = hex2bytes(two)
    moid = zeros(UInt8 ,20)
    err = ccall((:git_merge_base, :libgit2), Cint,
        (Ptr{UInt8}, Ptr{Void}, Ptr{UInt8}, Ptr{UInt8}), moid, repo.ptr, oid1, oid2)
    if err != 0
        return ""
    else
        bytes2hex(moid)
    end
end

function revparse(repo::GitRepo, obj::AbstractString)
    obj_ptr = Ptr{Void}[0]
    err = ccall((:git_revparse_single, :libgit2), Cint,
               (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Uint8}), obj_ptr, repo.ptr, obj)
    err != 0 && return nothing
    return hex2bytes(obj_id(Obj(obj_ptr[1])))
end

function is_ancestor_of(a::AbstractString, b::AbstractString, repo::GitRepo)
    A = revparse(repo, a)
    merge_base(a, b, repo) == A
end

function set_remote_url(repo::GitRepo, url::AbstractString; remote::AbstractString="origin")
    cfg = GitConfig(repo)

    err = set!(AbstractString, cfg, "remote.$remote.url", url)
    err !=0 && return

    m = match(GITHUB_REGEX,url)
    m == nothing && return
    push = "git@github.com:$(m.captures[1]).git"
    if push != url
        err = set!(AbstractString, cfg, "remote.$remote.pushurl", push)
    end
end

function set_remote_url(path::AbstractString, url::AbstractString; remote::AbstractString="origin")
    repo = GitRepo(path)
    set_remote_url(repo, url, remote=remote)
    LibGit2.free!(prepo)
end

function mirror_callback(remote::Ptr{Ptr{Void}}, repo::Ptr{Void}, name::Ptr{UInt8}, url::Ptr{UInt8}, payload::Ptr{Void})
    # Create the remote with a mirroring url
    fetch_spec = "+refs/*:refs/*"
    err = ccall((:git_remote_create_with_fetchspec, :libgit2), Cint,
                (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{UInt8}, Ptr{UInt8}, Ptr{UInt8}),
                remote, repo, name, url, fetch_spec)
    err != 0 && return Cint(err)

    # And set the configuration option to true for the push command
    config = GitConfig(GitRepo(repo, false))
    name_str = bytestring(name)
    err = set!(config, "remote.$name_str.mirror", true)
    free!(config)
    err != 0 && return Cint(err)

    return Cint(0)
end
const mirror_cb = cfunction(mirror_callback, Cint, (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{UInt8}, Ptr{UInt8}, Ptr{Void}))

function fetch(repo::GitRepo, remote::AbstractString="origin")
    remote_ptr = [C_NULL]
    err = ccall((:git_remote_lookup, :libgit2), Cint,
                (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{UInt8}),
                remote_ptr, repo.ptr, remote)
    err != 0 && return GitError(err)

    err = ccall((:git_remote_fetch, :libgit2), Cint,
                (Ptr{Void}, Ptr{Void}, Ptr{UInt8}),
                remote_ptr[1], C_NULL, C_NULL)
    err != 0 && return GitError(err)
end

function normalize_url(url::AbstractString)
    m = match(GITHUB_REGEX,url)
    m == nothing ? url : "git://github.com/$(m.captures[1]).git"
end

end
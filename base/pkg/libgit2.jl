module LibGit2

export with_libgit2

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
include("libgit2/types.jl")
include("libgit2/error.jl")
include("libgit2/signature.jl")
include("libgit2/oid.jl")
include("libgit2/reference.jl")
include("libgit2/commit.jl")
include("libgit2/repository.jl")
include("libgit2/config.jl")
include("libgit2/walker.jl")
include("libgit2/remote.jl")

function need_update(repo::GitRepo)
    if !isbare(repo)
        "git update-index -q --really-refresh" #TODO: update-index
    end
end

function isattached(repo::GitRepo)
    ccall((:git_repository_head_detached, :libgit2), Cint, (Ptr{Void},), repo.ptr) != 1
end

function iscommit(id::AbstractString, repo::GitRepo)
    need_update(repo)
    res = true
    try
        get(GitCommit, repo, id)
    catch
        res = false
    end
    return res
end

function isdirty(repo::GitRepo, paths::AbstractString="")
    tree_oid = revparse(repo, "HEAD^{tree}")
    tree_oid == nothing && return true

    result = false
    tree = get(GitTree, repo, tree_oid)
    try
        diff_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
        @check ccall((:git_diff_tree_to_workdir_with_index, :libgit2), Cint,
                   (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Void}, Ptr{Void}, Ptr{Void}),
                   diff_ptr_ptr, repo.ptr, tree.ptr, C_NULL, C_NULL)
        diff = GitDiff(diff_ptr_ptr[])

        if isempty(paths)
            c = ccall((:git_diff_num_deltas, :libgit2), Cint, (Ptr{Void},), diff.ptr)
            result = c > 0
        else
            # TODO look for specified path
            c = ccall((:git_diff_num_deltas, :libgit2), Cint, (Ptr{Void},), diff.ptr)
            result = c > 0
        end
        finalize(diff)
    catch
        result = true
    finally
        finalize(tree)
    end
    return result
end

function merge_base(one::AbstractString, two::AbstractString, repo::GitRepo)
    oid1_ptr = Ref(Oid(one))
    oid2_ptr = Ref(Oid(two))
    moid_ptr = Ref(Oid())
    moid = try
        @check ccall((:git_merge_base, :libgit2), Cint,
                (Ptr{Oid}, Ptr{Void}, Ptr{Oid}, Ptr{Oid}),
                moid_ptr, repo.ptr, oid1_ptr, oid2_ptr)
        moid_ptr[]
    catch e
        warn("merge_base: ", e.msg)
        Oid()
    end
    return moid
end

function is_ancestor_of(a::AbstractString, b::AbstractString, repo::GitRepo)
    A = revparse(repo, a)
    merge_base(a, b, repo) == A
end

function set_remote_url(repo::GitRepo, url::AbstractString; remote::AbstractString="origin")
    with_libgit2(GitConfig, repo) do cfg
        set!(cfg, "remote.$remote.url", url)

        m = match(GITHUB_REGEX,url)
        if m != nothing
            push = "git@github.com:$(m.captures[1]).git"
            if push != url
                set!(cfg, "remote.$remote.pushurl", push)
            end
        end
    end
end

function set_remote_url(path::AbstractString, url::AbstractString; remote::AbstractString="origin")
    with_libgit2(GitRepo, path) do repo
        set_remote_url(repo, url, remote=remote)
    end
end

function mirror_callback(remote::Ptr{Ptr{Void}}, repo_ptr::Ptr{Void}, name::Ptr{UInt8}, url::Ptr{UInt8}, payload::Ptr{Void})
    # Create the remote with a mirroring url
    fetch_spec = "+refs/*:refs/*"
    err = ccall((:git_remote_create_with_fetchspec, :libgit2), Cint,
                (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{UInt8}, Ptr{UInt8}, Ptr{UInt8}),
                remote, repo_ptr, name, url, fetch_spec)
    err != 0 && return Cint(err)

    # And set the configuration option to true for the push command
    config = GitConfig(GitRepo(repo_ptr))
    name_str = bytestring(name)
    err= try set!(config, "remote.$name_str.mirror", true)
         catch -1
         finally finalize(config)
         end
    err != 0 && return Cint(err)
    return Cint(0)
end
const mirror_cb = cfunction(mirror_callback, Cint, (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{UInt8}, Ptr{UInt8}, Ptr{Void}))

function fetch(repo::GitRepo, remote::AbstractString="origin")
    rmt = get(GitRemote, repo, remote)

    with_libgit2(rmt, warn_on_exception=true) do rmt
        @check ccall((:git_remote_fetch, :libgit2), Cint,
                (Ptr{Void}, Ptr{Void}, Ptr{UInt8}),
                rmt.ptr, C_NULL, C_NULL)
    end
end

function clone(url::AbstractString, path::AbstractString;
               branch::AbstractString="",
               bare::Bool = false,
               remote_cb::Ptr{Void} = C_NULL)
    # start cloning
    clone_opts = CloneOptionsStruct()
    clone_opts.bare = Int32(bare)
    if !isempty(branch)
        clone_opts.checkout_branch = pointer(branch)
    end
    if remote_cb != C_NULL
        clone_opts.remote_cb = remote_cb
    end

    clone_opts_ref = Ref(clone_opts)
    repo_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_clone, :libgit2), Cint,
            (Ptr{Ptr{Void}}, Ptr{UInt8}, Ptr{UInt8}, Ref{CloneOptionsStruct}),
            repo_ptr_ptr, url, path, clone_opts_ref)
    return GitRepo(repo_ptr_ptr[])
end

function authors(repo::GitRepo)
    athrs = map(
        (oid,repo)->author(get(GitCommit, repo, oid))::Signature,
        repo) #, by = Pkg.LibGit2.GitConst.SORT_TIME)
    return athrs
end

function normalize_url(url::AbstractString)
    m = match(GITHUB_REGEX,url)
    m == nothing ? url : "git://github.com/$(m.captures[1]).git"
end

end
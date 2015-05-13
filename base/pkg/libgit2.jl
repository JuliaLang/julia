module LibGit2

export with_libgit2, with, with_warn
export GitRepo, GitConfig, GitIndex

const GITHUB_REGEX =
    r"^(?:git@|git://|https://(?:[\w\.\+\-]+@)?)github.com[:/](([^/].+)/(.+?))(?:\.git)?$"i

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
include("libgit2/strarray.jl")
include("libgit2/index.jl")

immutable State
    head::Oid
    index::Oid
    work::Oid
end

function normalize_url(url::AbstractString)
    m = match(GITHUB_REGEX,url)
    m == nothing ? url : "git://github.com/$(m.captures[1]).git"
end

function gitdir(d, repo::GitRepo)
    g = joinpath(d,".git")
    isdir(g) && return g
    path(repo)
end

"""Return HEAD Oid as string"""
function head(pkg::AbstractString)
    head_id =""
    repo = GitRepo(pkg)
    try
        head_id = string(head_oid(repo))
    catch err
        warn(err)
    finally
        finalize(repo)
    end
end

function need_update(repo::GitRepo)
    if !isbare(repo)
        # read updates index from filesystem
        # i.e. "git update-index -q --really-refresh"
        read!(repo, true)
    end
end

""" Checks if commit is in repository """
function iscommit(id::AbstractString, repo::GitRepo)
    res = true
    try
        get(GitCommit, repo, id)
    catch
        res = false
    end
    return res
end

""" git diff-index HEAD [-- <path>]"""
isdirty(repo::GitRepo, paths::AbstractString="") = isdiff(repo, GitConst.HEAD_FILE, paths)

""" git diff-index <treeish> [-- <path>]"""
function isdiff(repo::GitRepo, treeish::AbstractString, paths::AbstractString="")
    tree_oid = revparseid(repo, "$treeish^{tree}")
    iszero(tree_oid) && return true
    emptypathspec = isempty(paths)

    result = false
    tree = get(GitTree, repo, tree_oid)
    if !emptypathspec
        sa = StrArrayStruct(paths)
        diff_opts = DiffOptionsStruct(pathspec = sa)
    end
    try
        diff_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
        @check ccall((:git_diff_tree_to_workdir_with_index, :libgit2), Cint,
                   (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Void}, Ptr{DiffOptionsStruct}),
                   diff_ptr_ptr, repo.ptr, tree.ptr, emptypathspec ? C_NULL : Ref(diff_opts))
        diff = GitDiff(diff_ptr_ptr[])

        c = ccall((:git_diff_num_deltas, :libgit2), Cint, (Ptr{Void},), diff.ptr)
        result = c > 0
        finalize(diff)
    catch err
        warn(err)
        result = true
    finally
        !emptypathspec && finalize(sa)
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
    A = revparseid(repo, a)
    merge_base(a, b, repo) == A
end

function set_remote_url(repo::GitRepo, url::AbstractString; remote::AbstractString="origin")
    with(GitConfig, repo) do cfg
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
    with(GitRepo, path) do repo
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

function fetch(rmt::GitRemote)
    @check ccall((:git_remote_fetch, :libgit2), Cint,
            (Ptr{Void}, Ptr{Void}, Ptr{UInt8}),
            rmt.ptr, C_NULL, C_NULL)
end

function fetch(repo::GitRepo, remote::AbstractString="origin")
    rmt = get(GitRemote, repo, remote)
    try
        fetch(rmt)
    catch err
        warn("'fetch' thrown exception: $err")
    finally
        finalize(rmt)
    end
end

function fetch(repo::GitRepo, remote_path::AbstractString, refspecs::AbstractString)
    rmt = GitRemoteAnon(repo, remote_path, refspecs)
    try
        fetch(rmt)
    catch err
        warn("'fetch' thrown exception: $err")
    finally
        finalize(rmt)
    end
end

""" git branch """
function branch(repo::GitRepo)
    head_ref = head(repo)
    brnch = ""
    try
        brnch = branch(head_ref)
    finally
        finalize(head_ref)
    end
    return brnch
end

""" git branch -b <branch> [<start-point>] """
function branch!(repo::GitRepo, branch::AbstractString,
                 commit::AbstractString = "";
                 force::Bool=false)
    # if commit is empty get head commit oid
    commit_oid = if isempty(commit)
        head_ref = head(repo)
        commit_id = peel(head_ref, GitConst.OBJ_COMMIT)
    else
        Oid(commit)
    end
    iszero(commit_id) && return

    cmt =  get(GitCommit, repo, commit_oid)
    try
        create_branch(repo, cmt, branch,
            force=force,
            msg="pkg.libgit2.branch: moving to $branch")
        ref = create_branch(repo, cmt, branch, force=force)
        finalize(ref)
    finally
        finalize(cmt)
    end
end

""" git checkout [-f] --detach <commit> """
function checkout!(repo::GitRepo, commit::AbstractString = "";
                  force::Bool = true)
    # nothing to do
    isempty(commit) && return

    # grab head name
    head_name = GitConst.HEAD_FILE
    try
        head_ref = head(repo)
        head_name = shortname(head_ref)
        # if it is HEAD use short OID instead
        if head_name == GitConst.HEAD_FILE
            head_name = string(Oid(head_ref))
        end
        finalize(head_ref)
    catch err
        warn(err)
    end

    # search for commit or revparse branch to get a commit object
    obj = get(GitAnyObject, repo, Oid(commit))
    obj == nothing && return
    try
        peeled = peel(obj, GitConst.OBJ_COMMIT)
        peeled == nothing && return
        opts = force ? CheckoutOptionsStruct(checkout_strategy = GitConst.CHECKOUT_FORCE) :
                           CheckoutOptionsStruct()
        try
            # detach commit
            obj_oid = Oid(peeled)
            ref = create_reference(repo, obj_oid,
                force=force,
                msg="pkg.libgit2.checkout: moving from $head_name to $(string(obj_oid))")
            finalize(ref)

            # checkout branch or commit
            checkout_tree(repo, peeled, options = opts)
        finally
            finalize(peeled)
        end
    catch err
        rethrow(err)
        #warn("Checkout: $err")
    finally
        finalize(obj)
    end
end

function clone(url::AbstractString, path::AbstractString;
               branch::AbstractString="",
               bare::Bool = false,
               remote_cb::Ptr{Void} = C_NULL)
    # setup colne options
    clone_opts = CloneOptionsStruct(
                    bare = Int32(bare),
                    checkout_branch = isempty(branch) ? Ptr{UInt8}(C_NULL) : pointer(branch),
                    remote_cb = remote_cb
                )

    # start cloning
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

function snapshot(repo::GitRepo; dir="")
    head = Oid(repo, GitConst.HEAD_FILE)
    index = with(GitIndex, repo) do idx; write_tree!(idx) end
    work = try
        with(GitIndex, repo) do idx
            content = readdir(abspath(dir))
            if length(content) > 1
                files = [utf8(bytestring(c))::UTF8String for c in content]
                push!(files, utf8("."))

                add!(idx, files..., flags = GitConst.INDEX_ADD_CHECK_PATHSPEC)
                write!(idx)
            end
            write_tree!(idx)
        end
    finally
        # restore index
        with(GitIndex, repo) do idx
            read_tree!(idx, index)
            write!(idx)
        end
    end
    State(head, index, work)
end

function restore(s::State, repo::GitRepo; dir="")
    with(GitIndex, repo) do idx
        #TODO: reset -q --                  # unstage everything

        read_tree!(idx, s.work)             # move work tree to index
        write!(idx)

        #TODO: checkout-index -fa           # check the index out to work
        #TODO: clean -qdf                   # remove everything else

        read_tree!(idx, s.index)            # restore index
        write!(idx)

        #TODO: reset -q --soft $(s.head)    # restore head
    end
end

# TODO: restore required
function transact(f::Function, repo::GitRepo; dir="")
    #state = snapshot(repo, dir=dir)
    try f(repo) catch
        #restore(state, repo, dir=dir)
        rethrow()
    finally
        finalize(repo)
    end
end


function __init__()
    err = ccall((:git_libgit2_init, :libgit2), Cint, ())
    err > 0 || error("error initializing LibGit2 module")
    atexit() do
        gc()
        ccall((:git_libgit2_shutdown, :libgit2), Cint, ())
    end
end

end # module
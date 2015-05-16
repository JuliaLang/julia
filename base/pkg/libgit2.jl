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

""" git update-index """
function need_update(repo::GitRepo)
    if !isbare(repo)
        # read updates index from filesystem
        read!(repo, true)
    end
end

""" Checks if commit is in repository """
function iscommit(id::AbstractString, repo::GitRepo)
    res = true
    try
        c = get(GitCommit, repo, id)
        finalize(c)
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

""" git fetch <repository> [<refspec>]"""
function fetch(repo::GitRepo, remote::AbstractString="origin";
               refspecs::AbstractString="")
    rmt = if isempty(refspecs)
        get(GitRemote, repo, remote)
    else
        GitRemoteAnon(repo, remote, refspecs)
    end

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

""" git checkout [-b|-B] <branch> [<start-point>] [--track <remote>/<branch>] """
function branch!(repo::GitRepo, branch_name::AbstractString,
                 commit::AbstractString = ""; # start point
                 track::AbstractString  = "", # track remote branch
                 force::Bool=false,           # force branch creation
                 set_head::Bool=true)         # set as head reference on exit
    # try to lookup branch first
    branch_ref = force ? nothing : lookup_branch(repo, branch_name)
    if branch_ref == nothing
        # if commit is empty get head commit oid
        commit_id = if isempty(commit)
            head_ref = head(repo)
            try
                peel(head_ref, GitConst.OBJ_COMMIT)
            finally
                finalize(head_ref)
            end
        else
            Oid(commit)
        end
        iszero(commit_id) && return

        cmt =  get(GitCommit, repo, commit_id)
        try
            branch_ref = create_branch(repo, cmt, branch_name,
                force=force,
                msg="pkg.libgit2.branch: moving to $branch_name")
        finally
            finalize(cmt)
        end
    end
    try
        if !isempty(track) # setup tracking
            with(GitConfig, repo) do cfg
                set!(cfg, "branch.$branch_name.remote", GitConst.REMOTE_ORIGIN)
                set!(cfg, "branch.$branch_name.merge", name(branch_ref))
            end
        end

        # switch head to the branch
        set_head && head!(repo, branch_ref)
    finally
        finalize(branch_ref)
    end
    return
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

    # search for commit to get a commit object
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

            # checkout commit
            checkout_tree(repo, peeled, options = opts)
        finally
            finalize(peeled)
        end
    finally
        finalize(obj)
    end
end

""" git clone [-b <branch>] [--bare] <url> <dir> """
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

""" git reset [<committish>] [--] <pathspecs>... """
function reset!(repo::GitRepo, committish::AbstractString, pathspecs::AbstractString...)
    target_obj = isempty(committish) ? Nullable{GitAnyObject}() :
                                       Nullable(revparse(repo, committish))
    try
        reset!(repo, target_obj, pathspecs...)
    finally
        !isnull(target_obj) && finalize(Base.get(target_obj))
    end
end

""" git reset [--soft | --mixed | --hard] <commit> """
function reset!(repo::GitRepo, commit::Oid, mode::Cint = GitConst.GIT_RESET_MIXED)

    obj = get(GitAnyObject, repo, commit)
    obj == nothing && return
    try
        reset!(repo, obj, mode)
    finally
        finalize(obj)
    end
end

""" Returns all commit authors """
function authors(repo::GitRepo)
    athrs = map(
        (oid,repo)->author(get(GitCommit, repo, oid))::Signature,
        repo) #, by = Pkg.LibGit2.GitConst.SORT_TIME)
    return athrs
end

function snapshot(repo::GitRepo)
    head = Oid(repo, GitConst.HEAD_FILE)
    index = with(GitIndex, repo) do idx; write_tree!(idx) end
    work = try
        dir = splitdir(path(repo)[1:end-1])[1] # remove `/.git` part
        with(GitIndex, repo) do idx
            content = readdir(dir)
            if length(content) > 1
                files = [utf8(bytestring(c))::UTF8String for c in content]
                push!(files, utf8("."))

                add!(idx, files...)
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

function restore(s::State, repo::GitRepo)
    reset!(repo, GitConst.HEAD_FILE, "*")  # unstage everything
    with(GitIndex, repo) do idx
        read_tree!(idx, s.work)            # move work tree to index
        opts = CheckoutOptionsStruct(
                checkout_strategy = GitConst.CHECKOUT_FORCE |     # check the index out to work
                                    GitConst.CHECKOUT_REMOVE_UNTRACKED) # remove everything else
        checkout_index(repo, Nullable(idx), options = opts)

        read_tree!(idx, s.index)  # restore index
    end
    reset!(repo, s.head, GitConst.GIT_RESET_SOFT) # restore head
end

function transact(f::Function, repo::GitRepo)
    state = snapshot(repo)
    try f(repo) catch
        restore(state, repo)
        rethrow()
    finally
        finalize(repo)
    end
end


function __init__()
    err = ccall((:git_libgit2_init, :libgit2), Cint, ())
    err > 0 || error("error initializing LibGit2 module")
    atexit() do
        ccall((:git_libgit2_shutdown, :libgit2), Cint, ())
    end
end

end # module
# This file is a part of Julia. License is MIT: http://julialang.org/license

module LibGit2

import Base: merge!, cat, ==

export with, GitRepo, GitConfig

const GITHUB_REGEX =
    r"^(?:git@|git://|https://(?:[\w\.\+\-]+@)?)github.com[:/](([^/].+)/(.+?))(?:\.git)?$"i

include("utils.jl")
include("consts.jl")
include("types.jl")
include("error.jl")
include("signature.jl")
include("oid.jl")
include("reference.jl")
include("commit.jl")
include("repository.jl")
include("config.jl")
include("walker.jl")
include("remote.jl")
include("strarray.jl")
include("index.jl")
include("merge.jl")
include("tag.jl")
include("blob.jl")
include("diff.jl")
include("rebase.jl")
include("status.jl")
include("tree.jl")
include("callbacks.jl")

using .Error

immutable State
    head::Oid
    index::Oid
    work::Oid
end

"""Return HEAD Oid as string"""
function head(pkg::AbstractString)
    with(GitRepo, pkg) do repo
        string(head_oid(repo))
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
        if c === nothing
            res = false
        else
            finalize(c)
        end
    catch
        res = false
    end
    return res
end

""" git diff-index HEAD [-- <path>]"""
isdirty(repo::GitRepo, paths::AbstractString=""; cached::Bool=false) =
    isdiff(repo, Consts.HEAD_FILE, paths, cached=cached)

""" git diff-index <treeish> [-- <path>]"""
function isdiff(repo::GitRepo, treeish::AbstractString, paths::AbstractString=""; cached::Bool=false)
    tree_oid = revparseid(repo, "$treeish^{tree}")
    iszero(tree_oid) && return true
    result = false
    tree = get(GitTree, repo, tree_oid)
    try
        diff = diff_tree(repo, tree, paths)
        result = count(diff) > 0
        finalize(diff)
    catch err
        result = true
    finally
        finalize(tree)
    end
    return result
end

""" git diff --name-only --diff-filter=<filter> <branch1> <branch2> """
function diff_files(repo::GitRepo, branch1::AbstractString, branch2::AbstractString;
                    filter::Set{Cint}=Set([Consts.DELTA_ADDED, Consts.DELTA_MODIFIED, Consts.DELTA_DELETED]))
    b1_id = revparseid(repo, branch1*"^{tree}")
    b2_id = revparseid(repo, branch2*"^{tree}")
    tree1 = get(GitTree, repo, b1_id)
    tree2 = get(GitTree, repo, b2_id)
    files = AbstractString[]
    try
        diff = diff_tree(repo, tree1, tree2)
        for i in 1:count(diff)
            delta = diff[i]
            delta === nothing && break
            if delta.status in filter
                push!(files, unsafe_string(delta.new_file.path))
            end
        end
        finalize(diff)
    finally
        finalize(tree1)
        finalize(tree2)
    end
    return files
end

function is_ancestor_of(a::AbstractString, b::AbstractString, repo::GitRepo)
    A = revparseid(repo, a)
    merge_base(repo, a, b) == A
end

function set_remote_url(repo::GitRepo, url::AbstractString; remote::AbstractString="origin")
    with(GitConfig, repo) do cfg
        set!(cfg, "remote.$remote.url", url)

        m = match(GITHUB_REGEX,url)
        if m !== nothing
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

function make_payload{P<:AbstractCredentials}(payload::Nullable{P})
    Ref{Nullable{AbstractCredentials}}(payload)
end

""" git fetch [<url>|<repository>] [<refspecs>]"""
function fetch{T<:AbstractString, P<:AbstractCredentials}(repo::GitRepo;
                                  remote::AbstractString="origin",
                                  remoteurl::AbstractString="",
                                  refspecs::Vector{T}=AbstractString[],
                                  payload::Nullable{P}=Nullable{AbstractCredentials}())
    rmt = if isempty(remoteurl)
        get(GitRemote, repo, remote)
    else
        GitRemoteAnon(repo, remoteurl)
    end
    try
        payload = make_payload(payload)
        fo = FetchOptions(callbacks=RemoteCallbacks(credentials_cb(), payload))
        fetch(rmt, refspecs, msg="from $(url(rmt))", options = fo)
    finally
        finalize(rmt)
    end
end

""" git push [<url>|<repository>] [<refspecs>]"""
function push{T<:AbstractString, P<:AbstractCredentials}(repo::GitRepo;
              remote::AbstractString="origin",
              remoteurl::AbstractString="",
              refspecs::Vector{T}=AbstractString[],
              force::Bool=false,
              payload::Nullable{P}=Nullable{AbstractCredentials}())
    rmt = if isempty(remoteurl)
        get(GitRemote, repo, remote)
    else
        GitRemoteAnon(repo, remoteurl)
    end
    try
        payload = make_payload(payload)
        push_opts=PushOptions(callbacks=RemoteCallbacks(credentials_cb(), payload))
        push(rmt, refspecs, force=force, options=push_opts)
    finally
        finalize(rmt)
    end
end

""" git branch """
function branch(repo::GitRepo)
    head_ref = head(repo)
    try
        branch(head_ref)
    finally
        finalize(head_ref)
    end
end

""" git checkout [-b|-B] <branch> [<start-point>] [--track <remote>/<branch>] """
function branch!(repo::GitRepo, branch_name::AbstractString,
                 commit::AbstractString = ""; # start point
                 track::AbstractString  = "", # track remote branch
                 force::Bool=false,           # force branch creation
                 set_head::Bool=true)         # set as head reference on exit
    # try to lookup branch first
    branch_ref = force ? nothing : lookup_branch(repo, branch_name)
    if branch_ref === nothing
        branch_rmt_ref = isempty(track) ? nothing : lookup_branch(repo, "$track/$branch_name", true)
        # if commit is empty get head commit oid
        commit_id = if isempty(commit)
            if branch_rmt_ref === nothing
                with(head(repo)) do head_ref
                    with(peel(GitCommit, head_ref)) do hrc
                        Oid(hrc)
                    end
                end
            else
                tmpcmt = with(peel(GitCommit, branch_rmt_ref)) do hrc
                    Oid(hrc)
                end
                finalize(branch_rmt_ref)
                tmpcmt
            end
        else
            Oid(commit)
        end
        iszero(commit_id) && return
        cmt =  get(GitCommit, repo, commit_id)
        new_branch_ref = nothing
        try
            new_branch_ref = create_branch(repo, branch_name, cmt, force=force)
        finally
            finalize(cmt)
            new_branch_ref === nothing && throw(GitError(Error.Object, Error.ERROR, "cannot create branch `$branch_name` with `$commit_id`"))
            branch_ref = new_branch_ref
        end
    end
    try
        #TODO: what if branch tracks other then "origin" remote
        if !isempty(track) # setup tracking
            try
                with(GitConfig, repo) do cfg
                    set!(cfg, "branch.$branch_name.remote", Consts.REMOTE_ORIGIN)
                    set!(cfg, "branch.$branch_name.merge", name(branch_ref))
                end
            catch
                warn("Please provide remote tracking for branch '$branch_name' in '$(path(repo))'")
            end
        end

        if set_head
            # checkout selected branch
            with(peel(GitTree, branch_ref)) do btree
                checkout_tree(repo, btree)
            end

            # switch head to the branch
            head!(repo, branch_ref)
        end
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
    head_name = Consts.HEAD_FILE
    try
        with(head(repo)) do head_ref
            head_name = shortname(head_ref)
            # if it is HEAD use short OID instead
            if head_name == Consts.HEAD_FILE
                head_name = string(Oid(head_ref))
            end
        end
    end

    # search for commit to get a commit object
    obj = get(GitAnyObject, repo, Oid(commit))
    obj === nothing && return
    try
        peeled = peel(obj, Consts.OBJ_COMMIT)
        peeled === nothing && return
        opts = force ? CheckoutOptions(checkout_strategy = Consts.CHECKOUT_FORCE) :
                       CheckoutOptions()
        try
            # detach commit
            obj_oid = Oid(peeled)
            ref = GitReference(repo, obj_oid, force=force,
                msg="libgit2.checkout: moving from $head_name to $(string(obj_oid))")
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
function clone{P<:AbstractCredentials}(repo_url::AbstractString, repo_path::AbstractString;
               branch::AbstractString="",
               isbare::Bool = false,
               remote_cb::Ptr{Void} = C_NULL,
               payload::Nullable{P}=Nullable{AbstractCredentials}())
    # setup clone options
    lbranch = Base.cconvert(Cstring, branch)
    payload = make_payload(payload)
    fetch_opts=FetchOptions(callbacks = RemoteCallbacks(credentials_cb(), payload))
    clone_opts = CloneOptions(
                bare = Cint(isbare),
                checkout_branch = isempty(lbranch) ? Cstring(C_NULL) : Base.unsafe_convert(Cstring, lbranch),
                fetch_opts=fetch_opts,
                remote_cb = remote_cb
            )
    return clone(repo_url, repo_path, clone_opts)
end

""" git reset [<committish>] [--] <pathspecs>... """
function reset!(repo::GitRepo, committish::AbstractString, pathspecs::AbstractString...)
    obj = revparse(repo, !isempty(committish) ? committish : Consts.HEAD_FILE)
    # do not remove entries in the index matching the provided pathspecs with empty target commit tree
    obj === nothing && throw(GitError(Error.Object, Error.ERROR, "`$committish` not found"))
    try
        reset!(repo, Nullable(obj), pathspecs...)
    finally
        finalize(obj)
    end
end

""" git reset [--soft | --mixed | --hard] <commit> """
function reset!(repo::GitRepo, commit::Oid, mode::Cint = Consts.RESET_MIXED)
    obj = get(GitAnyObject, repo, commit)
    # object must exist for reset
    obj === nothing && throw(GitError(Error.Object, Error.ERROR, "Commit `$(string(commit))` object not found"))
    try
        reset!(repo, obj, mode)
    finally
        finalize(obj)
    end
end

""" git cat-file <commit> """
function cat{T<:GitObject}(repo::GitRepo, ::Type{T}, object::AbstractString)
    obj_id = revparseid(repo, object)
    iszero(obj_id) && return nothing

    obj = get(T, repo, obj_id)
    if isa(obj, GitBlob)
        return unsafe_string(convert(Ptr{UInt8}, content(obj)))
    else
        return nothing
    end
end

""" git rev-list --count <commit1> <commit2> """
function revcount(repo::GitRepo, fst::AbstractString, snd::AbstractString)
    fst_id = revparseid(repo, fst)
    snd_id = revparseid(repo, snd)
    base_id = merge_base(repo, string(fst_id), string(snd_id))
    fc = with(GitRevWalker(repo)) do walker
        count((i,r)->i!=base_id, walker, oid=fst_id, by=Consts.SORT_TOPOLOGICAL)
    end
    sc = with(GitRevWalker(repo)) do walker
        count((i,r)->i!=base_id, walker, oid=snd_id, by=Consts.SORT_TOPOLOGICAL)
    end
    return (fc-1, sc-1)
end

""" git merge [--ff-only] [<committish> | FETCH_HEAD] """
function merge!(repo::GitRepo;
                committish::AbstractString = "",
                branch::AbstractString = "",
                fastforward::Bool = false,
                merge_opts::MergeOptions = MergeOptions(),
                checkout_opts::CheckoutOptions = CheckoutOptions())
    # merge into head branch
    with(head(repo)) do head_ref
        upst_anns = if !isempty(committish) # merge committish into HEAD
            if committish == Consts.FETCH_HEAD # merge FETCH_HEAD
                fheads = fetchheads(repo)
                filter!(fh->fh.ismerge, fheads)
                if isempty(fheads)
                    throw(GitError(Error.Merge, Error.ERROR,
                                   "There is no fetch reference for this branch."))
                end
                map(fh->GitAnnotated(repo,fh), fheads)
            else # merge commitish
                [GitAnnotated(repo, committish)]
            end
        else
            if !isempty(branch) # merge provided branch into HEAD
                with(GitReference(repo, branch)) do brn_ref
                    [GitAnnotated(repo, brn_ref)]
                end
            else # try to get tracking remote branch for the head
                if !isattached(repo)
                    throw(GitError(Error.Merge, Error.ERROR,
                                   "Repository HEAD is detached. Remote tracking branch cannot be used."))
                end
                with(upstream(head_ref)) do tr_brn_ref
                    if tr_brn_ref === nothing
                        throw(GitError(Error.Merge, Error.ERROR,
                                       "There is no tracking information for the current branch."))
                    end
                    [GitAnnotated(repo, tr_brn_ref)]
                end
            end
        end

        try
            merge!(repo, upst_anns, fastforward,
                   merge_opts=merge_opts,
                   checkout_opts=checkout_opts)
        finally
            map(finalize, upst_anns)
        end
    end
end

""" git rebase --merge [--onto <newbase>] [<upstream>] """
function rebase!(repo::GitRepo, upstream::AbstractString="", newbase::AbstractString="")
    with(head(repo)) do head_ref
        head_ann = GitAnnotated(repo, head_ref)
        upst_ann  = if isempty(upstream)
            with(LibGit2.upstream(head_ref)) do brn_ref
                if brn_ref === nothing
                    throw(GitError(Error.Rebase, Error.ERROR,
                                   "There is no tracking information for the current branch."))
                end
                GitAnnotated(repo, brn_ref)
            end
        else
            GitAnnotated(repo, upstream)
        end
        try
            sig = default_signature(repo)
            try
                rbs = GitRebase(repo, head_ann, upst_ann)
                try
                    while (rbs_op = next(rbs)) !== nothing
                        commit(rbs, sig)
                    end
                    finish(rbs, sig)
                catch err
                    abort(rbs)
                finally
                    finalize(rbs)
                end
            finally
                #!isnull(onto_ann) && finalize(get(onto_ann))
                finalize(sig)
            end
        finally
            finalize(upst_ann)
            finalize(head_ann)
        end
    end
    return nothing
end


""" Returns all commit authors """
function authors(repo::GitRepo)
    return with(GitRevWalker(repo)) do walker
        map((oid,repo)->with(get(GitCommit, repo, oid)) do cmt
                            author(cmt)::Signature
                        end,
            walker) #, by = Consts.SORT_TIME)
    end
end

function snapshot(repo::GitRepo)
    head = Oid(repo, Consts.HEAD_FILE)
    index = with(GitIndex, repo) do idx; write_tree!(idx) end
    work = try
        with(GitIndex, repo) do idx
            if length(readdir(path(repo))) > 1
                add!(idx, ".")
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
    reset!(repo, Consts.HEAD_FILE, "*")  # unstage everything
    with(GitIndex, repo) do idx
        read_tree!(idx, s.work)            # move work tree to index
        opts = CheckoutOptions(
                checkout_strategy = Consts.CHECKOUT_FORCE |     # check the index out to work
                                    Consts.CHECKOUT_REMOVE_UNTRACKED) # remove everything else
        checkout_index(repo, Nullable(idx), options = opts)

        read_tree!(idx, s.index)  # restore index
    end
    reset!(repo, s.head, Consts.RESET_SOFT) # restore head
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

function set_ssl_cert_locations(cert_loc)
    cert_file = isfile(cert_loc) ? cert_loc : Cstring(C_NULL)
    cert_dir  = isdir(cert_loc) ? cert_loc : Cstring(C_NULL)
    cert_file == C_NULL && cert_dir == C_NULL && return
    # TODO FIX https://github.com/libgit2/libgit2/pull/3935#issuecomment-253910017
    #ccall((:git_libgit2_opts, :libgit2), Cint,
    #      (Cint, Cstring, Cstring),
    #      Cint(Consts.SET_SSL_CERT_LOCATIONS), cert_file, cert_dir)
    ENV["SSL_CERT_FILE"] = cert_file
    ENV["SSL_CERT_DIR"] = cert_dir
end

function __init__()
    # Look for OpenSSL env variable for CA bundle (linux only)
    # windows and macOS use the OS native security backends
    old_ssl_cert_dir = Base.get(ENV, "SSL_CERT_DIR", nothing)
    old_ssl_cert_file = Base.get(ENV, "SSL_CERT_FILE", nothing)
    @static if is_linux()
        cert_loc = if "SSL_CERT_DIR" in keys(ENV)
            ENV["SSL_CERT_DIR"]
        elseif "SSL_CERT_FILE" in keys(ENV)
            ENV["SSL_CERT_FILE"]
        else
            # If we have a bundled ca cert file, point libgit2 at that so SSL connections work.
            abspath(ccall(:jl_get_julia_home, Any, ()),Base.DATAROOTDIR,"julia","cert.pem")
        end
        set_ssl_cert_locations(cert_loc)
    end

    err = ccall((:git_libgit2_init, :libgit2), Cint, ())
    err > 0 || throw(ErrorException("error initializing LibGit2 module"))
    atexit() do
        ccall((:git_libgit2_shutdown, :libgit2), Cint, ())
    end

    @static if is_linux()
        if old_ssl_cert_dir != Base.get(ENV, "SSL_CERT_DIR", "")
            if old_ssl_cert_dir === nothing
                delete!(ENV, "SSL_CERT_DIR")
            else
                ENV["SSL_CERT_DIR"] = old_ssl_cert_dir
            end
        end
        if old_ssl_cert_file != Base.get(ENV, "SSL_CERT_FILE", "")
            if old_ssl_cert_file === nothing
                delete!(ENV, "SSL_CERT_FILE")
            else
                ENV["SSL_CERT_FILE"] = old_ssl_cert_file
            end
        end
    end
end


end # module

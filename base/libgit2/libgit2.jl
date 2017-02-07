# This file is a part of Julia. License is MIT: http://julialang.org/license

module LibGit2

import Base: merge!, cat, ==

export with, GitRepo, GitConfig

const GITHUB_REGEX =
    r"^(?:git@|git://|https://(?:[\w\.\+\-]+@)?)github.com[:/](([^/].+)/(.+?))(?:\.git)?$"i

const REFCOUNT = Threads.Atomic{UInt}()

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
    head::GitHash
    index::GitHash
    work::GitHash
end

"""
    head(pkg::AbstractString) -> String

Return current HEAD [`GitHash`](@ref) of
the `pkg` repo as a string.
"""
function head(pkg::AbstractString)
    with(GitRepo, pkg) do repo
        string(head_oid(repo))
    end
end

"""
    need_update(repo::GitRepo)

Equivalent to `git update-index`. Returns `true`
if `repo` needs updating.
"""
function need_update(repo::GitRepo)
    if !isbare(repo)
        # read updates index from filesystem
        read!(repo, true)
    end
end

"""
    iscommit(id::AbstractString, repo::GitRepo) -> Bool

Checks if commit `id` (which is a [`GitHash`](@ref) in string form)
is in the repository.
"""
function iscommit(id::AbstractString, repo::GitRepo)
    res = true
    try
        c = GitCommit(repo, id)
        if c === nothing
            res = false
        else
            close(c)
        end
    catch
        res = false
    end
    return res
end

"""
    LibGit2.isdirty(repo::GitRepo, pathspecs::AbstractString=""; cached::Bool=false) -> Bool

Checks if there have been any changes to tracked files in the working tree (if
`cached=false`) or the index (if `cached=true`).
`pathspecs` are the specifications for options for the diff.

Equivalent to `git diff-index HEAD [-- <pathspecs>]`.
"""
isdirty(repo::GitRepo, paths::AbstractString=""; cached::Bool=false) =
    isdiff(repo, Consts.HEAD_FILE, paths, cached=cached)

"""
    LibGit2.isdiff(repo::GitRepo, treeish::AbstractString, pathspecs::AbstractString=""; cached::Bool=false)

Checks if there are any differences between the tree specified by `treeish` and the
tracked files in the working tree (if `cached=false`) or the index (if `cached=true`).
`pathspecs` are the specifications for options for the diff.

Equivalent to `git diff-index <treeish> [-- <pathspecs>]`.
"""
function isdiff(repo::GitRepo, treeish::AbstractString, paths::AbstractString=""; cached::Bool=false)
    tree = GitTree(repo, "$treeish^{tree}")
    try
        diff = diff_tree(repo, tree, paths, cached=cached)
        result = count(diff) > 0
        close(diff)
    finally
        close(tree)
    end
    return result
end

"""
    diff_files(repo::GitRepo, branch1::AbstractString, branch2::AbstractString; kwarg...) -> Vector{AbstractString}

Show which files have changed in the git repository `repo` between branches `branch1`
and `branch2`.

The keyword argument is:
  * `filter::Set{Cint}=Set([Consts.DELTA_ADDED, Consts.DELTA_MODIFIED, Consts.DELTA_DELETED]))`,
    and it sets options for the diff. The default is to show files added, modified, or deleted.

Returns only the *names* of the files which have changed, *not* their contents.

Equivalent to `git diff --name-only --diff-filter=<filter> <branch1> <branch2>`.
"""
function diff_files(repo::GitRepo, branch1::AbstractString, branch2::AbstractString;
                    filter::Set{Cint}=Set([Consts.DELTA_ADDED, Consts.DELTA_MODIFIED, Consts.DELTA_DELETED]))
    b1_id = revparseid(repo, branch1*"^{tree}")
    b2_id = revparseid(repo, branch2*"^{tree}")
    tree1 = GitTree(repo, b1_id)
    tree2 = GitTree(repo, b2_id)
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
        close(diff)
    finally
        close(tree1)
        close(tree2)
    end
    return files
end

"""
    is_ancestor_of(a::AbstractString, b::AbstractString, repo::GitRepo) -> Bool

Returns `true` if `a`, a [`GitHash`](@ref) in string form, is an ancestor of
`b`, a [`GitHash`](@ref) in string form.
"""
function is_ancestor_of(a::AbstractString, b::AbstractString, repo::GitRepo)
    A = revparseid(repo, a)
    merge_base(repo, a, b) == A
end

"""
    set_remote_url(repo::GitRepo, url::AbstractString; remote::AbstractString="origin")

Set the `url` for `remote` for the git repository `repo`.
The default name of the remote is `"origin"`.
"""
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

"""
    set_remote_url(path::AbstractString, url::AbstractString; remote::AbstractString="origin")

Set the `url` for `remote` for the git repository located at `path`.
The default name of the remote is `"origin"`.
"""
function set_remote_url(path::AbstractString, url::AbstractString; remote::AbstractString="origin")
    with(GitRepo, path) do repo
        set_remote_url(repo, url, remote=remote)
    end
end

function make_payload{P<:AbstractCredentials}(payload::Nullable{P})
    Ref{Nullable{AbstractCredentials}}(payload)
end

"""
    fetch(repo::GitRepo; kwargs...)

Fetches updates from an upstream of the repository `repo`.

The keyword arguments are:
  * `remote::AbstractString="origin"`: which remote, specified by name,
    of `repo` to fetch from. If this is empty, the URL will be used to
    construct an anonymous remote.
  * `remoteurl::AbstractString=""`: the URL of `remote`. If not specified,
    will be assumed based on the given name of `remote`.
  * `refspecs=AbstractString[]`: determines properties of the fetch.
  * `payload=Nullable{AbstractCredentials}()`: provides credentials, if necessary,
    for instance if `remote` is a private repository.

Equivalent to `git fetch [<remoteurl>|<repo>] [<refspecs>]`.
"""
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
        close(rmt)
    end
end

"""
    push(repo::GitRepo; kwargs...)

Pushes updates to an upstream of `repo`.

The keyword arguments are:
  * `remote::AbstractString="origin"`: the name of the upstream remote to push to.
  * `remoteurl::AbstractString=""`: the URL of `remote`.
  * `refspecs=AbstractString[]`: determines properties of the push.
  * `force::Bool=false`: determines if the push will be a force push,
     overwriting the remote branch.
  * `payload=Nullable{AbstractCredentials}()`: provides credentials, if necessary,
    for instance if `remote` is a private repository.

Equivalent to `git push [<remoteurl>|<repo>] [<refspecs>]`.
"""
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
        close(rmt)
    end
end

"""
    branch(repo::GitRepo)

Equivalent to `git branch`.
Create a new branch from the current HEAD.
"""
function branch(repo::GitRepo)
    head_ref = head(repo)
    try
        branch(head_ref)
    finally
        close(head_ref)
    end
end

"""
    branch!(repo::GitRepo, branch_name::AbstractString, commit::AbstractString=""; kwargs...)

Checkout a new git branch in the `repo` repository. `commit` is the [`GitHash`](@ref),
in string form, which will be the start of the new branch.

The keyword arguments are:
  * `track::AbstractString=""`: the name of the
    remote branch this new branch should track, if any.
    If empty (the default), no remote branch
    will be tracked.
  * `force::Bool=false`: if `true`, branch creation will
    be forced.
  * `set_head::Bool=true`: if `true`, after the branch creation
    finishes the branch head will be set as the HEAD of `repo`.

Equivalent to `git checkout [-b|-B] <branch_name> [<commit>] [--track <track>]`.
"""
function branch!(repo::GitRepo, branch_name::AbstractString,
                 commit::AbstractString = ""; # start point
                 track::AbstractString  = "", # track remote branch
                 force::Bool=false,           # force branch creation
                 set_head::Bool=true)         # set as head reference on exit
    # try to lookup branch first
    branch_ref = force ? Nullable{GitReference}() : lookup_branch(repo, branch_name)
    if isnull(branch_ref)
        branch_rmt_ref = isempty(track) ? Nullable{GitReference}() : lookup_branch(repo, "$track/$branch_name", true)
        # if commit is empty get head commit oid
        commit_id = if isempty(commit)
            if isnull(branch_rmt_ref)
                with(head(repo)) do head_ref
                    with(peel(GitCommit, head_ref)) do hrc
                        GitHash(hrc)
                    end
                end
            else
                tmpcmt = with(peel(GitCommit, Base.get(branch_rmt_ref))) do hrc
                    GitHash(hrc)
                end
                close(Base.get(branch_rmt_ref))
                tmpcmt
            end
        else
            GitHash(commit)
        end
        iszero(commit_id) && return
        cmt =  GitCommit(repo, commit_id)
        new_branch_ref = nothing
        try
            new_branch_ref = Nullable(create_branch(repo, branch_name, cmt, force=force))
        finally
            close(cmt)
            isnull(new_branch_ref) && throw(GitError(Error.Object, Error.ERROR, "cannot create branch `$branch_name` with `$commit_id`"))
            branch_ref = new_branch_ref
        end
    end
    try
        #TODO: what if branch tracks other then "origin" remote
        if !isempty(track) # setup tracking
            try
                with(GitConfig, repo) do cfg
                    set!(cfg, "branch.$branch_name.remote", Consts.REMOTE_ORIGIN)
                    set!(cfg, "branch.$branch_name.merge", name(Base.get(branch_ref)))
                end
            catch
                warn("Please provide remote tracking for branch '$branch_name' in '$(path(repo))'")
            end
        end

        if set_head
            # checkout selected branch
            with(peel(GitTree, Base.get(branch_ref))) do btree
                checkout_tree(repo, btree)
            end

            # switch head to the branch
            head!(repo, Base.get(branch_ref))
        end
    finally
        close(Base.get(branch_ref))
    end
    return
end

"""
    checkout!(repo::GitRepo, commit::AbstractString=""; force::Bool=true)

Equivalent to `git checkout [-f] --detach <commit>`.
Checkout the git commit `commit` (a [`GitHash`](@ref) in string form)
in `repo`. If `force` is `true`, force the checkout and discard any
current changes. Note that this detaches the current HEAD.
"""
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
                head_name = string(GitHash(head_ref))
            end
        end
    end

    # search for commit to get a commit object
    obj = GitObject(repo, GitHash(commit))
    peeled = peel(GitCommit, obj)

    opts = force ? CheckoutOptions(checkout_strategy = Consts.CHECKOUT_FORCE) : CheckoutOptions()
    # detach commit
    obj_oid = GitHash(peeled)
    ref = GitReference(repo, obj_oid, force=force,
                       msg="libgit2.checkout: moving from $head_name to $(string(obj_oid))")

    # checkout commit
    checkout_tree(repo, peeled, options = opts)
end

"""
    clone(repo_url::AbstractString, repo_path::AbstractString; kwargs...)

Clone a remote repository located at `repo_url` to the local filesystem location `repo_path`.

The keyword arguments are:
  * `branch::AbstractString=""`: which branch of the remote to clone,
    if not the default repository branch (usually `master`).
  * `isbare::Bool=false`: if `true`, clone the remote as a bare repository,
    which will make `repo_path` itself the git directory instead of `repo_path/.git`.
    This means that a working tree cannot be checked out. Plays the role of the
    git CLI argument `--bare`.
  * `remote_cb::Ptr{Void}=C_NULL`: a callback which will be used to create the remote
    before it is cloned. If `C_NULL` (the default), no attempt will be made to create
    the remote - it will be assumed to already exist.
  * `payload::Nullable{P<:AbstractCredentials}=Nullable{AbstractCredentials}()`:
    provides credentials if necessary, for instance if the remote is a private
    repository.

Equivalent to `git clone [-b <branch>] [--bare] <repo_url> <repo_path>`.
"""
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
    obj = GitObject(repo, isempty(committish) ? Consts.HEAD_FILE : committish)
    # do not remove entries in the index matching the provided pathspecs with empty target commit tree
    reset!(repo, Nullable(obj), pathspecs...)
end

"""
    reset!(repo::GitRepo, id::GitHash, mode::Cint = Consts.RESET_MIXED)

Reset the repository `repo` to its state at `id`, using one of three modes
set by `mode`:
  1. `Consts.RESET_SOFT` - move HEAD to `id`.
  2. `Consts.RESET_MIXED` - default, move HEAD to `id` and reset the index to `id`.
  3. `Consts.RESET_HARD` - move HEAD to `id`, reset the index to `id`, and discard all working changes.

Equivalent to `git reset [--soft | --mixed | --hard] <id>`.
"""
reset!(repo::GitRepo, id::GitHash, mode::Cint = Consts.RESET_MIXED) =
    reset!(repo, GitObject(repo, id), mode)

""" git cat-file <commit> """
function cat(repo::GitRepo, spec)
    obj = GitObject(repo, spec)
    if isa(obj, GitBlob)
        content(obj)
    else
        nothing
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

"""
    merge!(repo::GitRepo; kwargs...) -> Bool

Perform a git merge on the repository `repo`, merging commits
with diverging history into the current branch. Returns `true`
if the merge succeeded, `false` if not.

The keyword arguments are:
  * `committish::AbstractString=""`: Merge the named commit(s) in `committish`.
  * `branch::AbstractString=""`: Merge the branch `branch` and all its commits
    since it diverged from the current branch.
  * `fastforward::Bool=false`: If `fastforward` is `true`, only merge if the
    merge is a fast-forward (the current branch head is an ancestor of the
    commits to be merged), otherwise refuse to merge and return `false`.
    This is equivalent to the git CLI option `--ff-only`.
  * `merge_opts::MergeOptions=MergeOptions()`: `merge_opts` specifies options
    for the merge, such as merge strategy in case of conflicts.
  * `checkout_opts::CheckoutOptions=CheckoutOptions()`: `checkout_opts` specifies
    options for the checkout step.

Equivalent to `git merge [--ff-only] [<committish> | <branch>]`.
"""
function merge!(repo::GitRepo;
                committish::AbstractString = "",
                branch::AbstractString = "",
                fastforward::Bool = false,
                merge_opts::MergeOptions = MergeOptions(),
                checkout_opts::CheckoutOptions = CheckoutOptions())
    # merge into head branch
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
            if isorphan(repo)
                # this isn't really a merge, but really moving HEAD
                # https://github.com/libgit2/libgit2/issues/2135#issuecomment-35997764
                # try to figure out remote tracking of orphan head

                m = with(GitReference(repo, Consts.HEAD_FILE)) do head_sym_ref
                    match(r"refs/heads/(.*)", fullname(head_sym_ref))
                end
                if m === nothing
                    throw(GitError(Error.Merge, Error.ERROR,
                                   "Unable to determine name of orphan branch."))
                end
                branchname = m.captures[1]
                remotename = with(GitConfig, repo) do cfg
                    LibGit2.get(String, cfg, "branch.$branchname.remote")
                end
                oid = with(GitReference(repo, "refs/remotes/$remotename/$branchname")) do ref
                    LibGit2.GitHash(ref)
                end
                with(GitCommit(repo, oid)) do cmt
                    LibGit2.create_branch(repo, branchname, cmt)
                end
                return true
            else
                with(head(repo)) do head_ref
                    tr_brn_ref = upstream(head_ref)
                    if isnull(tr_brn_ref)
                        throw(GitError(Error.Merge, Error.ERROR,
                                       "There is no tracking information for the current branch."))
                    end
                    try
                        [GitAnnotated(repo, Base.get(tr_brn_ref))]
                    finally
                        close(Base.get(tr_brn_ref))
                    end
                end
            end
        end
    end
    try
        merge!(repo, upst_anns, fastforward,
               merge_opts=merge_opts,
               checkout_opts=checkout_opts)
    finally
        map(close, upst_anns)
    end
end

"""
    LibGit2.rebase!(repo::GitRepo, upstream::AbstractString="", newbase::AbstractString="")

Attempt an automatic merge rebase of the current branch, from `upstream` if provided, or
otherwise from the upstream tracking branch.
`newbase` is the branch to rebase onto. By default this is `upstream`.

If any conflicts arise which cannot be automatically resolved, the rebase will abort,
leaving the repository and working tree in its original state, and the function will throw
a `GitError`. This is roughly equivalent to the following command line statement:

    git rebase --merge [<upstream>]
    if [ -d ".git/rebase-merge" ]; then
        git rebase --abort
    fi

"""
function rebase!(repo::GitRepo, upstream::AbstractString="", newbase::AbstractString="")
    with(head(repo)) do head_ref
        head_ann = GitAnnotated(repo, head_ref)
        upst_ann = if isempty(upstream)
            brn_ref = LibGit2.upstream(head_ref)
            if isnull(brn_ref)
                throw(GitError(Error.Rebase, Error.ERROR,
                               "There is no tracking information for the current branch."))
            end
            try
                GitAnnotated(repo, Base.get(brn_ref))
            finally
                close(brn_ref)
            end
        else
            GitAnnotated(repo, upstream)
        end
        onto_ann  = Nullable{GitAnnotated}(isempty(newbase) ? nothing : GitAnnotated(repo, newbase))
        try
            sig = default_signature(repo)
            try
                rbs = GitRebase(repo, head_ann, upst_ann, onto=onto_ann)
                try
                    while (rbs_op = next(rbs)) !== nothing
                        commit(rbs, sig)
                    end
                    finish(rbs, sig)
                catch err
                    abort(rbs)
                    rethrow(err)
                finally
                    close(rbs)
                end
            finally
                #!isnull(onto_ann) && close(get(onto_ann))
                close(sig)
            end
        finally
            if !isempty(newbase)
                close(Base.get(onto_ann))
            end
            close(upst_ann)
            close(head_ann)
        end
    end
    return head_oid(repo)
end


"""
    authors(repo::GitRepo) -> Vector{Signature}

Returns all authors of commits to the `repo` repository.
"""
function authors(repo::GitRepo)
    return with(GitRevWalker(repo)) do walker
        map((oid,repo)->with(GitCommit(repo, oid)) do cmt
                            author(cmt)::Signature
                        end,
            walker) #, by = Consts.SORT_TIME)
    end
end

"""
    snapshot(repo::GitRepo) -> State

Take a snapshot of the current state of the repository `repo`,
storing the current HEAD, index, and any uncommitted work.
The output `State` can be used later during a call to [`restore`](@ref)
to return the repository to the snapshotted state.
"""
function snapshot(repo::GitRepo)
    head = GitHash(repo, Consts.HEAD_FILE)
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

"""
    restore(s::State, repo::GitRepo)

Return a repository `repo` to a previous `State` `s`, for
example the HEAD of a branch before a merge attempt. `s`
can be generated using the [`snapshot`](@ref) function.
"""
function restore(s::State, repo::GitRepo)
    head = reset!(repo, Consts.HEAD_FILE, "*")  # unstage everything
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
        close(repo)
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
    REFCOUNT[] = 1

    atexit() do
        if Threads.atomic_sub!(REFCOUNT, UInt(1)) == 1
            # refcount zero, no objects to be finalized
            ccall((:git_libgit2_shutdown, :libgit2), Cint, ())
        end
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

# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Interface to [libgit2](https://libgit2.org/).
"""
module LibGit2

import Base: ==
using Base: something, notnothing
using Printf: @printf

export with, GitRepo, GitConfig

const GITHUB_REGEX =
    r"^(?:(?:ssh://)?git@|git://|https://(?:[\w\.\+\-]+@)?)github.com[:/](([^/].+)/(.+?))(?:\.git)?$"i

const REFCOUNT = Threads.Atomic{Int}(0)

function ensure_initialized end

include("error.jl")
include("utils.jl")
include("consts.jl")
include("types.jl")
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
include("blame.jl")
include("status.jl")
include("tree.jl")
include("gitcredential.jl")
include("callbacks.jl")

using .Error

struct State
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

Equivalent to `git update-index`. Return `true`
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

Check if commit `id` (which is a [`GitHash`](@ref) in string form)
is in the repository.

# Examples
```julia-repl
julia> repo = LibGit2.GitRepo(repo_path);

julia> LibGit2.add!(repo, test_file);

julia> commit_oid = LibGit2.commit(repo, "add test_file");

julia> LibGit2.iscommit(string(commit_oid), repo)
true
```
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

Check if there have been any changes to tracked files in the working tree (if
`cached=false`) or the index (if `cached=true`).
`pathspecs` are the specifications for options for the diff.

# Examples
```julia
repo = LibGit2.GitRepo(repo_path)
LibGit2.isdirty(repo) # should be false
open(joinpath(repo_path, new_file), "a") do f
    println(f, "here's my cool new file")
end
LibGit2.isdirty(repo) # now true
LibGit2.isdirty(repo, new_file) # now true
```

Equivalent to `git diff-index HEAD [-- <pathspecs>]`.
"""
isdirty(repo::GitRepo, paths::AbstractString=""; cached::Bool=false) =
    isdiff(repo, Consts.HEAD_FILE, paths, cached=cached)

"""
    LibGit2.isdiff(repo::GitRepo, treeish::AbstractString, pathspecs::AbstractString=""; cached::Bool=false)

Checks if there are any differences between the tree specified by `treeish` and the
tracked files in the working tree (if `cached=false`) or the index (if `cached=true`).
`pathspecs` are the specifications for options for the diff.

# Examples
```julia
repo = LibGit2.GitRepo(repo_path)
LibGit2.isdiff(repo, "HEAD") # should be false
open(joinpath(repo_path, new_file), "a") do f
    println(f, "here's my cool new file")
end
LibGit2.isdiff(repo, "HEAD") # now true
```

Equivalent to `git diff-index <treeish> [-- <pathspecs>]`.
"""
function isdiff(repo::GitRepo, treeish::AbstractString, paths::AbstractString=""; cached::Bool=false)
    tree = GitTree(repo, "$treeish^{tree}")
    try
        diff = diff_tree(repo, tree, paths, cached=cached)
        result = count(diff) > 0
        close(diff)
        return result
    finally
        close(tree)
    end
end

"""
    diff_files(repo::GitRepo, branch1::AbstractString, branch2::AbstractString; kwarg...) -> Vector{AbstractString}

Show which files have changed in the git repository `repo` between branches `branch1`
and `branch2`.

The keyword argument is:
  * `filter::Set{Consts.DELTA_STATUS}=Set([Consts.DELTA_ADDED, Consts.DELTA_MODIFIED, Consts.DELTA_DELETED]))`,
    and it sets options for the diff. The default is to show files added, modified, or deleted.

Return only the *names* of the files which have changed, *not* their contents.

# Examples
```julia
LibGit2.branch!(repo, "branch/a")
LibGit2.branch!(repo, "branch/b")
# add a file to repo
open(joinpath(LibGit2.path(repo),"file"),"w") do f
    write(f, "hello repo\n")
end
LibGit2.add!(repo, "file")
LibGit2.commit(repo, "add file")
# returns ["file"]
filt = Set([LibGit2.Consts.DELTA_ADDED])
files = LibGit2.diff_files(repo, "branch/a", "branch/b", filter=filt)
# returns [] because existing files weren't modified
filt = Set([LibGit2.Consts.DELTA_MODIFIED])
files = LibGit2.diff_files(repo, "branch/a", "branch/b", filter=filt)
```

Equivalent to `git diff --name-only --diff-filter=<filter> <branch1> <branch2>`.
"""
function diff_files(repo::GitRepo, branch1::AbstractString, branch2::AbstractString;
                    filter::Set{Consts.DELTA_STATUS}=Set([Consts.DELTA_ADDED, Consts.DELTA_MODIFIED, Consts.DELTA_DELETED]))
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
            if Consts.DELTA_STATUS(delta.status) in filter
                Base.push!(files, unsafe_string(delta.new_file.path))
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

Return `true` if `a`, a [`GitHash`](@ref) in string form, is an ancestor of
`b`, a [`GitHash`](@ref) in string form.

# Examples
```julia-repl
julia> repo = LibGit2.GitRepo(repo_path);

julia> LibGit2.add!(repo, test_file1);

julia> commit_oid1 = LibGit2.commit(repo, "commit1");

julia> LibGit2.add!(repo, test_file2);

julia> commit_oid2 = LibGit2.commit(repo, "commit2");

julia> LibGit2.is_ancestor_of(string(commit_oid1), string(commit_oid2), repo)
true
```
"""
function is_ancestor_of(a::AbstractString, b::AbstractString, repo::GitRepo)
    A = revparseid(repo, a)
    merge_base(repo, a, b) == A
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
  * `credentials=nothing`: provides credentials and/or settings when authenticating against
    a private `remote`.
  * `callbacks=Callbacks()`: user provided callbacks and payloads.

Equivalent to `git fetch [<remoteurl>|<repo>] [<refspecs>]`.
"""
function fetch(repo::GitRepo; remote::AbstractString="origin",
               remoteurl::AbstractString="",
               refspecs::Vector{<:AbstractString}=AbstractString[],
               credentials::Creds=nothing,
               callbacks::Callbacks=Callbacks())
    rmt = if isempty(remoteurl)
        get(GitRemote, repo, remote)
    else
        GitRemoteAnon(repo, remoteurl)
    end

    cred_payload = reset!(CredentialPayload(credentials), GitConfig(repo))
    if !haskey(callbacks, :credentials)
        callbacks[:credentials] = (credentials_cb(), cred_payload)
    elseif haskey(callbacks, :credentials) && credentials !== nothing
        throw(ArgumentError(string(
            "Unable to both use the provided `credentials` as a payload when the ",
            "`callbacks` also contain a credentials payload.")))
    end

    result = try
        remote_callbacks = RemoteCallbacks(callbacks)
        fo = FetchOptions(callbacks=remote_callbacks)
        fetch(rmt, refspecs, msg="from $(url(rmt))", options=fo)
    catch err
        if isa(err, GitError) && err.code === Error.EAUTH
            reject(cred_payload)
        else
            Base.shred!(cred_payload)
        end
        rethrow()
    finally
        close(rmt)
    end
    approve(cred_payload)
    return result
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
  * `credentials=nothing`: provides credentials and/or settings when authenticating against
     a private `remote`.
  * `callbacks=Callbacks()`: user provided callbacks and payloads.

Equivalent to `git push [<remoteurl>|<repo>] [<refspecs>]`.
"""
function push(repo::GitRepo; remote::AbstractString="origin",
              remoteurl::AbstractString="",
              refspecs::Vector{<:AbstractString}=AbstractString[],
              force::Bool=false,
              credentials::Creds=nothing,
              callbacks::Callbacks=Callbacks())
    rmt = if isempty(remoteurl)
        get(GitRemote, repo, remote)
    else
        GitRemoteAnon(repo, remoteurl)
    end

    cred_payload = reset!(CredentialPayload(credentials), GitConfig(repo))
    if !haskey(callbacks, :credentials)
        callbacks[:credentials] = (credentials_cb(), cred_payload)
    elseif haskey(callbacks, :credentials) && credentials !== nothing
        throw(ArgumentError(string(
            "Unable to both use the provided `credentials` as a payload when the ",
            "`callbacks` also contain a credentials payload.")))
    end

    result = try
        remote_callbacks = RemoteCallbacks(callbacks)
        push_opts = PushOptions(callbacks=remote_callbacks)
        push(rmt, refspecs, force=force, options=push_opts)
    catch err
        if isa(err, GitError) && err.code === Error.EAUTH
            reject(cred_payload)
        else
            Base.shred!(cred_payload)
        end
        rethrow()
    finally
        close(rmt)
    end
    approve(cred_payload)
    return result
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
If `commit` is an empty string, the current HEAD will be used.

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

# Examples
```julia
repo = LibGit2.GitRepo(repo_path)
LibGit2.branch!(repo, "new_branch", set_head=false)
```
"""
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
                        GitHash(hrc)
                    end
                end
            else
                tmpcmt = with(peel(GitCommit, branch_rmt_ref)) do hrc
                    GitHash(hrc)
                end
                close(branch_rmt_ref)
                tmpcmt
            end
        else
            GitHash(commit)
        end
        iszero(commit_id) && return
        cmt =  GitCommit(repo, commit_id)
        new_branch_ref = nothing
        try
            new_branch_ref = create_branch(repo, branch_name, cmt, force=force)
        finally
            close(cmt)
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
                @warn "Please provide remote tracking for branch '$branch_name' in '$(path(repo))'"
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
        close(branch_ref)
    end
    return
end

"""
    checkout!(repo::GitRepo, commit::AbstractString=""; force::Bool=true)

Equivalent to `git checkout [-f] --detach <commit>`.
Checkout the git commit `commit` (a [`GitHash`](@ref) in string form)
in `repo`. If `force` is `true`, force the checkout and discard any
current changes. Note that this detaches the current HEAD.

# Examples
```julia
repo = LibGit2.init(repo_path)
open(joinpath(LibGit2.path(repo), "file1"), "w") do f
    write(f, "111\n")
end
LibGit2.add!(repo, "file1")
commit_oid = LibGit2.commit(repo, "add file1")
open(joinpath(LibGit2.path(repo), "file1"), "w") do f
    write(f, "112\n")
end
# would fail without the force=true
# since there are modifications to the file
LibGit2.checkout!(repo, string(commit_oid), force=true)
```
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
    catch
    end

    # search for commit to get a commit object
    obj = GitObject(repo, GitHash(commit))
    peeled = peel(GitCommit, obj)
    obj_oid = GitHash(peeled)

    # checkout commit
    checkout_tree(repo, peeled, options = force ? CheckoutOptions(checkout_strategy = Consts.CHECKOUT_FORCE) : CheckoutOptions())

    GitReference(repo, obj_oid, force=force,
                 msg="libgit2.checkout: moving from $head_name to $(obj_oid))")

    return nothing
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
  * `remote_cb::Ptr{Cvoid}=C_NULL`: a callback which will be used to create the remote
    before it is cloned. If `C_NULL` (the default), no attempt will be made to create
    the remote - it will be assumed to already exist.
  * `credentials::Creds=nothing`: provides credentials and/or settings when authenticating
    against a private repository.
  * `callbacks::Callbacks=Callbacks()`: user provided callbacks and payloads.

Equivalent to `git clone [-b <branch>] [--bare] <repo_url> <repo_path>`.

# Examples
```julia
repo_url = "https://github.com/JuliaLang/Example.jl"
repo1 = LibGit2.clone(repo_url, "test_path")
repo2 = LibGit2.clone(repo_url, "test_path", isbare=true)
julia_url = "https://github.com/JuliaLang/julia"
julia_repo = LibGit2.clone(julia_url, "julia_path", branch="release-0.6")
```
"""
function clone(repo_url::AbstractString, repo_path::AbstractString;
               branch::AbstractString="",
               isbare::Bool = false,
               remote_cb::Ptr{Cvoid} = C_NULL,
               credentials::Creds=nothing,
               callbacks::Callbacks=Callbacks())
    cred_payload = reset!(CredentialPayload(credentials))
    if !haskey(callbacks, :credentials)
        callbacks[:credentials] = (credentials_cb(), cred_payload)
    elseif haskey(callbacks, :credentials) && credentials !== nothing
        throw(ArgumentError(string(
            "Unable to both use the provided `credentials` as a payload when the ",
            "`callbacks` also contain a credentials payload.")))
    end

    # setup clone options
    lbranch = Base.cconvert(Cstring, branch)
    GC.@preserve lbranch begin
        remote_callbacks = RemoteCallbacks(callbacks)
        fetch_opts = FetchOptions(callbacks=remote_callbacks)
        clone_opts = CloneOptions(
                    bare = Cint(isbare),
                    checkout_branch = isempty(lbranch) ? Cstring(C_NULL) : Base.unsafe_convert(Cstring, lbranch),
                    fetch_opts = fetch_opts,
                    remote_cb = remote_cb
                )
        repo = try
            clone(repo_url, repo_path, clone_opts)
        catch err
            if isa(err, GitError) && err.code === Error.EAUTH
                reject(cred_payload)
            else
                Base.shred!(cred_payload)
            end
            rethrow()
        end
    end
    approve(cred_payload)
    return repo
end

""" git reset [<committish>] [--] <pathspecs>... """
function reset!(repo::GitRepo, committish::AbstractString, pathspecs::AbstractString...)
    obj = GitObject(repo, isempty(committish) ? Consts.HEAD_FILE : committish)
    # do not remove entries in the index matching the provided pathspecs with empty target commit tree
    reset!(repo, obj, pathspecs...)
end

"""
    reset!(repo::GitRepo, id::GitHash, mode::Cint=Consts.RESET_MIXED)

Reset the repository `repo` to its state at `id`, using one of three modes
set by `mode`:
  1. `Consts.RESET_SOFT` - move HEAD to `id`.
  2. `Consts.RESET_MIXED` - default, move HEAD to `id` and reset the index to `id`.
  3. `Consts.RESET_HARD` - move HEAD to `id`, reset the index to `id`, and discard all working changes.

# Examples
```julia
# fetch changes
LibGit2.fetch(repo)
isfile(joinpath(repo_path, our_file)) # will be false

# fastforward merge the changes
LibGit2.merge!(repo, fastforward=true)

# because there was not any file locally, but there is
# a file remotely, we need to reset the branch
head_oid = LibGit2.head_oid(repo)
new_head = LibGit2.reset!(repo, head_oid, LibGit2.Consts.RESET_HARD)
```
In this example, the remote which is being fetched from *does* have
a file called `our_file` in its index, which is why we must reset.

Equivalent to `git reset [--soft | --mixed | --hard] <id>`.

# Examples
```julia
repo = LibGit2.GitRepo(repo_path)
head_oid = LibGit2.head_oid(repo)
open(joinpath(repo_path, "file1"), "w") do f
    write(f, "111\n")
end
LibGit2.add!(repo, "file1")
mode = LibGit2.Consts.RESET_HARD
# will discard the changes to file1
# and unstage it
new_head = LibGit2.reset!(repo, head_oid, mode)
```
"""
reset!(repo::GitRepo, id::GitHash, mode::Cint = Consts.RESET_MIXED) =
    reset!(repo, GitObject(repo, id), mode)

"""
    LibGit2.revcount(repo::GitRepo, commit1::AbstractString, commit2::AbstractString)

List the number of revisions between `commit1` and `commit2` (committish OIDs in string form).
Since `commit1` and `commit2` may be on different branches, `revcount` performs a "left-right"
revision list (and count), returning a tuple of `Int`s - the number of left and right
commits, respectively. A left (or right) commit refers to which side of a symmetric
difference in a tree the commit is reachable from.

Equivalent to `git rev-list --left-right --count <commit1> <commit2>`.

# Examples
```julia
repo = LibGit2.GitRepo(repo_path)
repo_file = open(joinpath(repo_path, test_file), "a")
println(repo_file, "hello world")
flush(repo_file)
LibGit2.add!(repo, test_file)
commit_oid1 = LibGit2.commit(repo, "commit 1")
println(repo_file, "hello world again")
flush(repo_file)
LibGit2.add!(repo, test_file)
commit_oid2 = LibGit2.commit(repo, "commit 2")
LibGit2.revcount(repo, string(commit_oid1), string(commit_oid2))
```

This will return `(-1, 0)`.
"""
function revcount(repo::GitRepo, commit1::AbstractString, commit2::AbstractString)
    commit1_id = revparseid(repo, commit1)
    commit2_id = revparseid(repo, commit2)
    base_id = merge_base(repo, string(commit1_id), string(commit2_id))
    fc = with(GitRevWalker(repo)) do walker
        count((i,r)->i!=base_id, walker, oid=commit1_id, by=Consts.SORT_TOPOLOGICAL)
    end
    sc = with(GitRevWalker(repo)) do walker
        count((i,r)->i!=base_id, walker, oid=commit2_id, by=Consts.SORT_TOPOLOGICAL)
    end
    return (fc-1, sc-1)
end

"""
    merge!(repo::GitRepo; kwargs...) -> Bool

Perform a git merge on the repository `repo`, merging commits
with diverging history into the current branch. Return `true`
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

!!! note
    If you specify a `branch`, this must be done in reference format, since
    the string will be turned into a `GitReference`. For example, if you
    wanted to merge branch `branch_a`, you would call
    `merge!(repo, branch="refs/heads/branch_a")`.
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
            Base.map(fh->GitAnnotated(repo,fh), fheads)
        else # merge committish
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
                    if tr_brn_ref === nothing
                        throw(GitError(Error.Merge, Error.ERROR,
                                       "There is no tracking information for the current branch."))
                    end
                    try
                        [GitAnnotated(repo, tr_brn_ref)]
                    finally
                        close(tr_brn_ref)
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
        Base.map(close, upst_anns)
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
            if brn_ref === nothing
                throw(GitError(Error.Rebase, Error.ERROR,
                               "There is no tracking information for the current branch."))
            end
            try
                GitAnnotated(repo, brn_ref)
            finally
                close(brn_ref)
            end
        else
            GitAnnotated(repo, upstream)
        end
        onto_ann = isempty(newbase) ? nothing : GitAnnotated(repo, newbase)
        try
            sig = default_signature(repo)
            try
                rbs = GitRebase(repo, head_ann, upst_ann, onto=onto_ann)
                try
                    for rbs_op in rbs
                        commit(rbs, sig)
                    end
                    finish(rbs, sig)
                catch
                    abort(rbs)
                    rethrow()
                finally
                    close(rbs)
                end
            finally
                #onto_ann !== nothing && close(onto_ann)
                close(sig)
            end
        finally
            if !isempty(newbase)
                close(onto_ann)
            end
            close(upst_ann)
            close(head_ann)
        end
    end
    return head_oid(repo)
end


"""
    authors(repo::GitRepo) -> Vector{Signature}

Return all authors of commits to the `repo` repository.

# Examples
```julia
repo = LibGit2.GitRepo(repo_path)
repo_file = open(joinpath(repo_path, test_file), "a")

println(repo_file, commit_msg)
flush(repo_file)
LibGit2.add!(repo, test_file)
sig = LibGit2.Signature("TEST", "TEST@TEST.COM", round(time(), 0), 0)
commit_oid1 = LibGit2.commit(repo, "commit1"; author=sig, committer=sig)
println(repo_file, randstring(10))
flush(repo_file)
LibGit2.add!(repo, test_file)
commit_oid2 = LibGit2.commit(repo, "commit2"; author=sig, committer=sig)

# will be a Vector of [sig, sig]
auths = LibGit2.authors(repo)
```
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
        checkout_index(repo, idx, options = opts)

        read_tree!(idx, s.index)  # restore index
    end
    reset!(repo, s.head, Consts.RESET_SOFT) # restore head
end

"""
    transact(f::Function, repo::GitRepo)

Apply function `f` to the git repository `repo`, taking a [`snapshot`](@ref) before
applying `f`. If an error occurs within `f`, `repo` will be returned to its snapshot
state using [`restore`](@ref). The error which occurred will be rethrown, but the
state of `repo` will not be corrupted.
"""
function transact(f::Function, repo::GitRepo)
    state = snapshot(repo)
    try f(repo) catch
        restore(state, repo)
        rethrow()
    finally
        close(repo)
    end
end

## lazy libgit2 initialization

function ensure_initialized()
    x = Threads.atomic_cas!(REFCOUNT, 0, 1)
    if x < 0
        negative_refcount_error(x)::Union{}
    end
    if x == 0
        initialize()
    end
    return nothing
end

@noinline function negative_refcount_error(x::Int)
    error("Negative LibGit2 REFCOUNT $x\nThis shouldn't happen, please file a bug report!")
end

@noinline function initialize()
    @check ccall((:git_libgit2_init, :libgit2), Cint, ())

    atexit() do
        # refcount zero, no objects to be finalized
        if Threads.atomic_sub!(REFCOUNT, 1) == 1
            ccall((:git_libgit2_shutdown, :libgit2), Cint, ())
        end
    end

    # Look for OpenSSL env variable for CA bundle (linux only)
    # windows and macOS use the OS native security backends
    @static if Sys.islinux()
        cert_loc = if "SSL_CERT_DIR" in keys(ENV)
            ENV["SSL_CERT_DIR"]
        elseif "SSL_CERT_FILE" in keys(ENV)
            ENV["SSL_CERT_FILE"]
        else
            # If we have a bundled ca cert file, point libgit2 at that so SSL connections work.
            abspath(ccall(:jl_get_julia_bindir, Any, ()), Base.DATAROOTDIR, "julia", "cert.pem")
        end
        set_ssl_cert_locations(cert_loc)
    end
end

function set_ssl_cert_locations(cert_loc)
    cert_file = isfile(cert_loc) ? cert_loc : Cstring(C_NULL)
    cert_dir  = isdir(cert_loc) ? cert_loc : Cstring(C_NULL)
    cert_file == C_NULL && cert_dir == C_NULL && return
    @check ccall((:git_libgit2_opts, :libgit2), Cint,
          (Cint, Cstring...),
          Cint(Consts.SET_SSL_CERT_LOCATIONS), cert_file, cert_dir)
end

end # module

# This file is a part of Julia. License is MIT: http://julialang.org/license

module LibGit2

import Base: merge!, cat

export with, GitRepo, GitConfig

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
include("libgit2/merge.jl")
include("libgit2/tag.jl")
include("libgit2/blob.jl")
include("libgit2/diff.jl")
include("libgit2/rebase.jl")
include("libgit2/repl.jl")
include("libgit2/utils.jl")

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
        finalize(c)
    catch
        res = false
    end
    return res
end

""" git diff-index HEAD [-- <path>]"""
isdirty(repo::GitRepo, paths::AbstractString=""; cached::Bool=false) = isdiff(repo, GitConst.HEAD_FILE, paths, cached=cached)

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
                    filter::Set{Cint}=Set([GitConst.DELTA_ADDED, GitConst.DELTA_MODIFIED, GitConst.DELTA_DELETED]))
    b1_id = revparseid(repo, branch1*"^{tree}")
    b2_id = revparseid(repo, branch2*"^{tree}")
    tree1 = get(GitTree, repo, b1_id)
    tree2 = get(GitTree, repo, b2_id)
    files = AbstractString[]
    try
        diff = diff_tree(repo, tree1, tree2)
        for i in 1:count(diff)
            delta = diff[i]
            delta == nothing && break
            if delta.status in filter
                push!(files, bytestring(delta.new_file.path))
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

function mirror_callback(remote::Ptr{Ptr{Void}}, repo_ptr::Ptr{Void}, name::Cstring, url::Cstring, payload::Ptr{Void})
    # Create the remote with a mirroring url
    fetch_spec = "+refs/*:refs/*"
    err = ccall((:git_remote_create_with_fetchspec, :libgit2), Cint,
                (Ptr{Ptr{Void}}, Ptr{Void}, Cstring, Cstring, Cstring),
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
const mirror_cb = cfunction(mirror_callback, Cint, (Ptr{Ptr{Void}}, Ptr{Void}, Cstring, Cstring, Ptr{Void}))

""" git fetch [<url>|<repository>] [<refspecs>]"""
function fetch{T<:AbstractString}(repo::GitRepo;
                                  remote::AbstractString="origin",
                                  remoteurl::AbstractString="",
                                  refspecs::Vector{T}=AbstractString[])
    rmt = if isempty(remoteurl)
        get(GitRemote, repo, remote)
    else
        GitRemoteAnon(repo, remoteurl)
    end
    try
        fetch(rmt, refspecs, msg="from $(url(rmt))")
    catch err
        warn("fetch: $err")
    finally
        finalize(rmt)
    end
end

""" git push [<url>|<repository>] [<refspecs>]"""
function push{T<:AbstractString}(repo::GitRepo;
              refspecs::Vector{T}=AbstractString[],
              remote::AbstractString="origin",
              remoteurl::AbstractString="",
              force::Bool=false)
    rmt = if isempty(remoteurl)
        get(GitRemote, repo, remote)
    else
        GitRemoteAnon(repo, remoteurl)
    end
    try
        push(rmt, refspecs, force=force, msg="to $(url(rmt))")
    catch err
        warn("push: $err")
    finally
        finalize(rmt)
    end
end

""" git branch """
function branch(repo::GitRepo)
    head_ref = head(repo)
    try
        branch(head_ref)
    catch
        ""
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
        if !isempty(track) # setup tracking #TODO: what is branch tracks other then "origin" remote
            try
                with(GitConfig, repo) do cfg
                    set!(cfg, "branch.$branch_name.remote", GitConst.REMOTE_ORIGIN)
                    set!(cfg, "branch.$branch_name.merge", name(branch_ref))
                end
            catch
                warn("Please provide remote tracking for branch '$branch_name' in '$(path(repo))'")
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
        with(head(repo)) do head_ref
            head_name = shortname(head_ref)
            # if it is HEAD use short OID instead
            if head_name == GitConst.HEAD_FILE
                head_name = string(Oid(head_ref))
            end
        end
    end

    # search for commit to get a commit object
    obj = get(GitAnyObject, repo, Oid(commit))
    obj == nothing && return
    try
        peeled = peel(obj, GitConst.OBJ_COMMIT)
        peeled == nothing && return
        opts = force ? CheckoutOptions(checkout_strategy = GitConst.CHECKOUT_FORCE) :
                       CheckoutOptions()
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
function clone(repo_url::AbstractString, repo_path::AbstractString;
               checkout_branch::AbstractString="",
               isbare::Bool = false,
               remote_cb::Ptr{Void} = C_NULL)
    # setup colne options
    clone_opts = CloneOptions(
                    bare = Int32(isbare),
                    checkout_branch = isempty(checkout_branch) ? Cstring_NULL :
                                      convert(Cstring, pointer(checkout_branch)),
                    remote_cb = remote_cb
                )
    return clone(repo_url, repo_path, clone_opts)
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
function reset!(repo::GitRepo, commit::Oid, mode::Cint = GitConst.RESET_MIXED)
    obj = get(GitAnyObject, repo, commit)
    obj == nothing && return
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
        return bytestring(convert(Ptr{UInt8}, content(obj)))
    else
        return nothing
    end
end

""" git rev-list --count <commit1> <commit2> """
function revcount(repo::GitRepo, fst::AbstractString, snd::AbstractString)
    fst_id = revparseid(repo, fst)
    snd_id = revparseid(repo, snd)
    base_id = merge_base(string(fst_id), string(snd_id), repo)
    fc = count((i,r)->i!=base_id, repo, oid=fst_id,
                by=Pkg.LibGit2.GitConst.SORT_TOPOLOGICAL)
    sc = count((i,r)->i!=base_id, repo, oid=snd_id,
                by=Pkg.LibGit2.GitConst.SORT_TOPOLOGICAL)
    return (fc-1, sc-1)
end

""" git merge [--ff-only] [<committish>] """
function merge!(repo::GitRepo, committish::AbstractString=""; fast_forward::Bool=false)
    # get head annotated upstream reference
    ret = with(head(repo)) do head_ref
        brn_ref = upstream(head_ref)
        upst_ann  = isempty(committish) ? GitAnnotated(repo, brn_ref) : GitAnnotated(repo, committish)
        try
            ma, mp = merge_analysis(repo, upst_ann)
            (ma & GitConst.MERGE_ANALYSIS_UP_TO_DATE == GitConst.MERGE_ANALYSIS_UP_TO_DATE) && return true
            if (ma & GitConst.MERGE_ANALYSIS_FASTFORWARD == GitConst.MERGE_ANALYSIS_FASTFORWARD)
                # do fastforward: checkout tree and update branch references
                # hur_oid = Oid(hur)
                # with(get(GitCommit, repo, hur_oid)) do cmt
                #     checkout_tree(repo, cmt)
                # end
                # target!(hr, hur_oid, msg="pkg.libgit2.megre!: fastforward $(name(hur)) into $(name(hr))")
                # head!(repo, hur, msg="--fastforward")

                brn_ref_oid = Oid(brn_ref)
                target!(head_ref, brn_ref_oid, msg="pkg.libgit2.megre!: fastforward $(name(brn_ref)) into $(name(head_ref))")
                reset!(repo, brn_ref_oid, GitConst.RESET_HARD)
            elseif (ma & GitConst.MERGE_ANALYSIS_NORMAL == GitConst.MERGE_ANALYSIS_NORMAL)
                fast_forward && return false # do not do merge
                merge!(repo, hua)
                cleanup(repo)
                info("Review and commit merged changes.")
            else
                warn("Unknown merge analysis result. Merging is not possible.")
                return false
            end
            return true
        finally
            finalize(upst_ann)
            finalize(brn_ref)
        end
    end
    return ret
end

""" git rebase --merge [--onto <newbase>] [<upstream>] """
function rebase!(repo::GitRepo, upstream::AbstractString="", newbase::AbstractString="")
    with(head(repo)) do head_ref
        head_ann = GitAnnotated(repo, head_ref)
        upst_ann  = if isempty(upstream)
            with(upstream(head_ref)) do brn_ref
                GitAnnotated(repo, brn_ref)
            end
        else
            GitAnnotated(repo, upstream)
        end
        try
            sig = default_signature(repo)
            try
                rbs = GitRebase(repo, head_ann, upst_ann, sig=Nullable(sig))
                try
                    while (rbs_op = next(rbs)) != nothing
                        commit(rbs, sig)
                    end
                    finish(rbs, sig)
                catch err
                    abort(rbs, sig)
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
end


""" Returns all commit authors """
function authors(repo::GitRepo)
    athrs = map(
        (oid,repo)->author(get(GitCommit, repo, oid))::Signature, #TODO: cleanup
        repo) #, by = GitConst.SORT_TIME)
    return athrs
end

function snapshot(repo::GitRepo)
    head = Oid(repo, GitConst.HEAD_FILE)
    index = with(GitIndex, repo) do idx; write_tree!(idx) end
    work = try
        with(GitIndex, repo) do idx
            content = readdir(path(repo))
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
        opts = CheckoutOptions(
                checkout_strategy = GitConst.CHECKOUT_FORCE |     # check the index out to work
                                    GitConst.CHECKOUT_REMOVE_UNTRACKED) # remove everything else
        checkout_index(repo, Nullable(idx), options = opts)

        read_tree!(idx, s.index)  # restore index
    end
    reset!(repo, s.head, GitConst.RESET_SOFT) # restore head
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
    err > 0 || throw(ErrorException("error initializing LibGit2 module"))
    atexit() do
        ccall((:git_libgit2_shutdown, :libgit2), Cint, ())
    end
end

end # module

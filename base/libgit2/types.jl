# This file is a part of Julia. License is MIT: http://julialang.org/license
import Base.@kwdef
import .Consts: GIT_SUBMODULE_IGNORE, GIT_MERGE_FILE_FAVOR, GIT_MERGE_FILE

const OID_RAWSZ = 20
const OID_HEXSZ = OID_RAWSZ * 2
const OID_MINPREFIXLEN = 4

abstract AbstractGitHash

"""
    GitHash

A git object identifier, based on the sha-1 hash. It is a $OID_RAWSZ byte string
($OID_HEXSZ hex digits) used to identify a `GitObject` in a repository.
"""
immutable GitHash <: AbstractGitHash
    val::NTuple{OID_RAWSZ, UInt8}
    GitHash(val::NTuple{OID_RAWSZ, UInt8}) = new(val)
end
GitHash() = GitHash(ntuple(i->zero(UInt8), OID_RAWSZ))

"""
    GitShortHash

This is a shortened form of `GitHash`, which can be used to identify a git object when it
is unique.

Internally it is stored as two fields: a full-size `GitHash` (`hash`) and a length
(`len`). Only the initial `len` hex digits of `hash` are used.
"""
immutable GitShortHash <: AbstractGitHash
    hash::GitHash   # underlying hash: unused digits are ignored
    len::Csize_t    # length in hex digits
end


"""
    LibGit2.TimeStruct

Time in a signature.
Matches the [`git_time`](https://libgit2.github.com/libgit2/#HEAD/type/git_time) struct.
"""
immutable TimeStruct
    time::Int64     # time in seconds from epoch
    offset::Cint    # timezone offset in minutes
end

"""
    LibGit2.SignatureStruct

An action signature (e.g. for committers, taggers, etc).
Matches the [`git_signature`](https://libgit2.github.com/libgit2/#HEAD/type/git_signature) struct.
"""
immutable SignatureStruct
    name::Ptr{UInt8}  # full name of the author
    email::Ptr{UInt8} # email of the author
    when::TimeStruct  # time when the action happened
end

"""
    LibGit2.StrArrayStruct

A LibGit2 representation of an array of strings.
Matches the [`git_strarray`](https://libgit2.github.com/libgit2/#HEAD/type/git_strarray) struct.

When fetching data from LibGit2, a typical usage would look like:
```julia
sa_ref = Ref(StrArrayStruct())
@check ccall(..., (Ptr{StrArrayStruct},), sa_ref)
res = convert(Vector{String}, sa_ref[])
free(sa_ref)
```
In particular, note that `LibGit2.free` should be called afterward on the `Ref` object.

Conversely, when passing a vector of strings to LibGit2, it is generally simplest to rely
on implicit conversion:
```julia
strs = String[...]
@check ccall(..., (Ptr{StrArrayStruct},), strs)
```
Note that no call to `free` is required as the data is allocated by Julia.
"""
immutable StrArrayStruct
   strings::Ptr{Cstring}
   count::Csize_t
end
StrArrayStruct() = StrArrayStruct(C_NULL, 0)

function free(sa_ref::Base.Ref{StrArrayStruct})
    ccall((:git_strarray_free, :libgit2), Void, (Ptr{StrArrayStruct},), sa_ref)
end

"""
    LibGit2.Buffer

A data buffer for exporting data from libgit2.
Matches the [`git_buf`](https://libgit2.github.com/libgit2/#HEAD/type/git_buf) struct.

When fetching data from LibGit2, a typical usage would look like:
```julia
buf_ref = Ref(Buffer())
@check ccall(..., (Ptr{Buffer},), buf_ref)
# operation on buf_ref
free(buf_ref)
```
In particular, note that `LibGit2.free` should be called afterward on the `Ref` object.
"""
immutable Buffer
    ptr::Ptr{Cchar}
    asize::Csize_t
    size::Csize_t
end
Buffer() = Buffer(C_NULL, 0, 0)

function free(buf_ref::Base.Ref{Buffer})
    ccall((:git_buf_free, :libgit2), Void, (Ptr{Buffer},), buf_ref)
end

"Abstract credentials payload"
abstract AbstractCredentials

"Checks if credentials were used"
checkused!(p::AbstractCredentials) = true
checkused!(p::Void) = false
"Resets credentials for another use"
reset!(p::AbstractCredentials, cnt::Int=3) = nothing

"""
    LibGit2.CheckoutOptions

Matches the [`git_checkout_options`](https://libgit2.github.com/libgit2/#HEAD/type/git_checkout_options) struct.
"""
@kwdef immutable CheckoutOptions
    version::Cuint = 1

    checkout_strategy::Cuint    = Consts.CHECKOUT_SAFE

    disable_filters::Cint
    dir_mode::Cuint
    file_mode::Cuint
    file_open_flags::Cint

    notify_flags::Cuint         = Consts.CHECKOUT_NOTIFY_NONE
    notify_cb::Ptr{Void}
    notify_payload::Ptr{Void}

    progress_cb::Ptr{Void}
    progress_payload::Ptr{Void}

    paths::StrArrayStruct

    baseline::Ptr{Void}
    baseline_index::Ptr{Void}

    target_directory::Cstring
    ancestor_label::Cstring
    our_label::Cstring
    their_label::Cstring

    perfdata_cb::Ptr{Void}
    perfdata_payload::Ptr{Void}
end

"""
    LibGit2.RemoteCallbacks

Callback settings.
Matches the [`git_remote_callbacks`](https://libgit2.github.com/libgit2/#HEAD/type/git_remote_callbacks) struct.
"""
@kwdef immutable RemoteCallbacks
    version::Cuint                    = 1
    sideband_progress::Ptr{Void}
    completion::Ptr{Void}
    credentials::Ptr{Void}
    certificate_check::Ptr{Void}
    transfer_progress::Ptr{Void}
    update_tips::Ptr{Void}
    pack_progress::Ptr{Void}
    push_transfer_progress::Ptr{Void}
    push_update_reference::Ptr{Void}
    push_negotiation::Ptr{Void}
    transport::Ptr{Void}
    payload::Ptr{Void}
end

function RemoteCallbacks(credentials::Ptr{Void}, payload::Ref{Nullable{AbstractCredentials}})
    RemoteCallbacks(credentials=credentials_cb(), payload=pointer_from_objref(payload))
end

"""
    LibGit2.ProxyOptions

Options for connecting through a proxy.

Matches the [`git_proxy_options`](https://libgit2.github.com/libgit2/#HEAD/type/git_proxy_options) struct.
"""
@kwdef immutable ProxyOptions
    version::Cuint             = 1
    proxytype::Cint
    url::Cstring
    credential_cb::Ptr{Void}
    certificate_cb::Ptr{Void}
    payload::Ptr{Void}
end


"""
    LibGit2.FetchOptions

Matches the [`git_fetch_options`](https://libgit2.github.com/libgit2/#HEAD/type/git_fetch_options) struct.
"""
@kwdef immutable FetchOptions
    version::Cuint                  = 1
    callbacks::RemoteCallbacks
    prune::Cint                     = Consts.FETCH_PRUNE_UNSPECIFIED
    update_fetchhead::Cint          = 1
    download_tags::Cint             = Consts.REMOTE_DOWNLOAD_TAGS_AUTO
    @static if LibGit2.VERSION >= v"0.25.0"
        proxy_opts::ProxyOptions
    end
    @static if LibGit2.VERSION >= v"0.24.0"
        custom_headers::StrArrayStruct
    end
end

"""
    LibGit2.CloneOptions

Matches the [`git_clone_options`](https://libgit2.github.com/libgit2/#HEAD/type/git_clone_options) struct.
"""
@kwdef immutable CloneOptions
    version::Cuint                      = 1
    checkout_opts::CheckoutOptions
    fetch_opts::FetchOptions
    bare::Cint
    localclone::Cint                    = Consts.CLONE_LOCAL_AUTO
    checkout_branch::Cstring
    repository_cb::Ptr{Void}
    repository_cb_payload::Ptr{Void}
    remote_cb::Ptr{Void}
    remote_cb_payload::Ptr{Void}
end

"""
    LibGit2.DiffOptionsStruct

Matches the [`git_diff_options`](https://libgit2.github.com/libgit2/#HEAD/type/git_diff_options) struct.
"""
@kwdef immutable DiffOptionsStruct
    version::Cuint                           = Consts.DIFF_OPTIONS_VERSION
    flags::UInt32                            = Consts.DIFF_NORMAL

    # options controlling which files are in the diff
    ignore_submodules::GIT_SUBMODULE_IGNORE  = Consts.SUBMODULE_IGNORE_UNSPECIFIED
    pathspec::StrArrayStruct
    notify_cb::Ptr{Void}
    @static if LibGit2.VERSION >= v"0.24.0"
        progress_cb::Ptr{Void}
    end
    payload::Ptr{Void}

    # options controlling how the diff text is generated
    context_lines::UInt32                    = UInt32(3)
    interhunk_lines::UInt32
    id_abbrev::UInt16                        = UInt16(7)
    max_size::Int64                          = Int64(512*1024*1024) #512Mb
    old_prefix::Cstring
    new_prefix::Cstring
end

"""
    LibGit2.DiffFile

Description of one side of a delta.
Matches the [`git_diff_file`](https://libgit2.github.com/libgit2/#HEAD/type/git_diff_file) struct.
"""
immutable DiffFile
    id::GitHash
    path::Cstring
    size::Int64
    flags::UInt32
    mode::UInt16
    @static if LibGit2.VERSION >= v"0.25.0"
        id_abbrev::UInt16
    end
end

"""
    LibGit2.DiffDelta

Description of changes to one entry.
Matches the [`git_diff_file`](https://libgit2.github.com/libgit2/#HEAD/type/git_diff_file) struct.
"""
immutable DiffDelta
    status::Cint
    flags::UInt32
    similarity::UInt16
    nfiles::UInt16
    old_file::DiffFile
    new_file::DiffFile
end

"""
    LibGit2.MergeOptions

Matches the [`git_merge_options`](https://libgit2.github.com/libgit2/#HEAD/type/git_merge_options) struct.
"""
@kwdef immutable MergeOptions
    version::Cuint                    = 1
    flags::Cint
    rename_threshold::Cuint           = 50
    target_limit::Cuint               = 200
    metric::Ptr{Void}
    @static if LibGit2.VERSION >= v"0.24.0"
        recursion_limit::Cuint
    end
    @static if LibGit2.VERSION >= v"0.25.0"
        default_driver::Cstring
    end
    file_favor::GIT_MERGE_FILE_FAVOR  = Consts.MERGE_FILE_FAVOR_NORMAL
    file_flags::GIT_MERGE_FILE        = Consts.MERGE_FILE_DEFAULT
end

"""
    LibGit2.PushOptions

Matches the [`git_push_options`](https://libgit2.github.com/libgit2/#HEAD/type/git_push_options) struct.
"""
@kwdef immutable PushOptions
    version::Cuint                     = 1
    parallelism::Cint                  = 1
    callbacks::RemoteCallbacks
    @static if LibGit2.VERSION >= v"0.25.0"
        proxy_opts::ProxyOptions
    end
    @static if LibGit2.VERSION >= v"0.24.0"
        custom_headers::StrArrayStruct
    end
end

"""
    LibGit2.IndexTime

Matches the [`git_index_time`](https://libgit2.github.com/libgit2/#HEAD/type/git_index_time) struct.
"""
immutable IndexTime
    seconds::Int64
    nanoseconds::Cuint
end

"""
    LibGit2.IndexEntry

In-memory representation of a file entry in the index.
Matches the [`git_index_entry`](https://libgit2.github.com/libgit2/#HEAD/type/git_index_entry) struct.
"""
immutable IndexEntry
    ctime::IndexTime
    mtime::IndexTime

    dev::UInt32
    ino::UInt32
    mode::UInt32
    uid::UInt32
    gid::UInt32
    file_size::Int64

    id::GitHash

    flags::UInt16
    flags_extended::UInt16

    path::Ptr{UInt8}
end
Base.show(io::IO, ie::IndexEntry) = print(io, "IndexEntry($(string(ie.id)))")

"""
    LibGit2.RebaseOptions

Matches the `git_rebase_options` struct.
"""
@kwdef immutable RebaseOptions
    version::Cuint                 = 1
    quiet::Cint                    = 1
    @static if LibGit2.VERSION >= v"0.24.0"
        inmemory::Cint
    end
    rewrite_notes_ref::Cstring
    @static if LibGit2.VERSION >= v"0.24.0"
        merge_opts::MergeOptions
    end
    checkout_opts::CheckoutOptions
end

"""
    LibGit2.RebaseOperation

Describes a single instruction/operation to be performed during the rebase.
Matches the [`git_rebase_operation`](https://libgit2.github.com/libgit2/#HEAD/type/git_rebase_operation_t) struct.
"""
immutable RebaseOperation
    optype::Cint
    id::GitHash
    exec::Cstring
end
Base.show(io::IO, rbo::RebaseOperation) = print(io, "RebaseOperation($(string(rbo.id)))")

"""
    LibGit2.StatusOptions

Options to control how `git_status_foreach_ext()` will issue callbacks.
Matches the [`git_status_opt_t`](https://libgit2.github.com/libgit2/#HEAD/type/git_status_opt_t) struct.
"""
@kwdef immutable StatusOptions
    version::Cuint           = 1
    show::Cint               = Consts.STATUS_SHOW_INDEX_AND_WORKDIR
    flags::Cuint             = Consts.STATUS_OPT_INCLUDE_UNTRACKED |
                               Consts.STATUS_OPT_RECURSE_UNTRACKED_DIRS |
                               Consts.STATUS_OPT_RENAMES_HEAD_TO_INDEX |
                               Consts.STATUS_OPT_SORT_CASE_SENSITIVELY
    pathspec::StrArrayStruct
end

"""
    LibGit2.StatusEntry

Providing the differences between the file as it exists in HEAD and the index, and
providing the differences between the index and the working directory.
Matches the `git_status_entry` struct.
"""
immutable StatusEntry
    status::Cuint
    head_to_index::Ptr{DiffDelta}
    index_to_workdir::Ptr{DiffDelta}
end

"""
    LibGit2.FetchHead

Contains the information about HEAD during a fetch, including the name and URL
of the branch fetched from, the oid of the HEAD, and whether the fetched HEAD
has been merged locally.
"""
immutable FetchHead
    name::String
    url::String
    oid::GitHash
    ismerge::Bool
end

# Abstract object types
abstract AbstractGitObject
Base.isempty(obj::AbstractGitObject) = (obj.ptr == C_NULL)

abstract GitObject <: AbstractGitObject

for (typ, reporef, sup, cname) in [
    (:GitRepo,       nothing,   :AbstractGitObject, :git_repository),
    (:GitTreeEntry,  nothing,   :AbstractGitObject, :git_tree_entry),
    (:GitConfig,     :Nullable, :AbstractGitObject, :git_config),
    (:GitIndex,      :Nullable, :AbstractGitObject, :git_index),
    (:GitRemote,     :GitRepo,  :AbstractGitObject, :git_remote),
    (:GitRevWalker,  :GitRepo,  :AbstractGitObject, :git_revwalk),
    (:GitReference,  :GitRepo,  :AbstractGitObject, :git_reference),
    (:GitDiff,       :GitRepo,  :AbstractGitObject, :git_diff),
    (:GitAnnotated,  :GitRepo,  :AbstractGitObject, :git_annotated_commit),
    (:GitRebase,     :GitRepo,  :AbstractGitObject, :git_rebase),
    (:GitStatus,     :GitRepo,  :AbstractGitObject, :git_status_list),
    (:GitBranchIter, :GitRepo,  :AbstractGitObject, :git_branch_iterator),
    (:GitUnknownObject,  :GitRepo,  :GitObject,         :git_object),
    (:GitCommit,     :GitRepo,  :GitObject,         :git_commit),
    (:GitBlob,       :GitRepo,  :GitObject,         :git_blob),
    (:GitTree,       :GitRepo,  :GitObject,         :git_tree),
    (:GitTag,        :GitRepo,  :GitObject,         :git_tag)]

    if reporef === nothing
        @eval type $typ <: $sup
            ptr::Ptr{Void}
            function $typ(ptr::Ptr{Void},fin=true)
                # fin=false should only be used when the pointer should not be free'd
                # e.g. from within callback functions which are passed a pointer
                @assert ptr != C_NULL
                obj = new(ptr)
                if fin
                    Threads.atomic_add!(REFCOUNT, UInt(1))
                    finalizer(obj, Base.close)
                end
                return obj
            end
        end
    elseif reporef == :Nullable
        @eval type $typ <: $sup
            nrepo::Nullable{GitRepo}
            ptr::Ptr{Void}
            function $typ(repo::GitRepo, ptr::Ptr{Void})
                @assert ptr != C_NULL
                obj = new(Nullable(repo), ptr)
                Threads.atomic_add!(REFCOUNT, UInt(1))
                finalizer(obj, Base.close)
                return obj
            end
            function $typ(ptr::Ptr{Void})
                @assert ptr != C_NULL
                obj = new(Nullable{GitRepo}(), ptr)
                Threads.atomic_add!(REFCOUNT, UInt(1))
                finalizer(obj, Base.close)
                return obj
            end
        end
    elseif reporef == :GitRepo
        @eval type $typ <: $sup
            repo::GitRepo
            ptr::Ptr{Void}
            function $typ(repo::GitRepo, ptr::Ptr{Void})
                @assert ptr != C_NULL
                obj = new(repo, ptr)
                Threads.atomic_add!(REFCOUNT, UInt(1))
                finalizer(obj, Base.close)
                return obj
            end
        end
    end
    @eval function Base.close(obj::$typ)
        if obj.ptr != C_NULL
            ccall(($(string(cname, :_free)), :libgit2), Void, (Ptr{Void},), obj.ptr)
            obj.ptr = C_NULL
            if Threads.atomic_sub!(REFCOUNT, UInt(1)) == 1
                # will the last finalizer please turn out the lights?
                ccall((:git_libgit2_shutdown, :libgit2), Cint, ())
            end
        end
    end
end

## Calling `GitObject(repo, ...)` will automatically resolve to the appropriate type.
function GitObject(repo::GitRepo, ptr::Ptr{Void})
    T = objtype(Consts.OBJECT(ptr))
    T(repo, ptr)
end

"""
    LibGit2.GitSignature

This is a Julia wrapper around a pointer to a
[`git_signature`](https://libgit2.github.com/libgit2/#HEAD/type/git_signature) object.
"""
type GitSignature <: AbstractGitObject
    ptr::Ptr{SignatureStruct}
    function GitSignature(ptr::Ptr{SignatureStruct})
        @assert ptr != C_NULL
        obj = new(ptr)
        finalizer(obj, Base.close)
        return obj
    end
end
function Base.close(obj::GitSignature)
    if obj.ptr != C_NULL
        ccall((:git_signature_free, :libgit2), Void, (Ptr{SignatureStruct},), obj.ptr)
        obj.ptr = C_NULL
    end
end

# Structure has the same layout as SignatureStruct
type Signature
    name::String
    email::String
    time::Int64
    time_offset::Cint
end

""" Resource management helper function
"""
function with(f::Function, obj)
    try
        f(obj)
    finally
        close(obj)
    end
end

with{T}(f::Function, ::Type{T}, args...) = with(f, T(args...))

function with_warn{T}(f::Function, ::Type{T}, args...)
    obj = T(args...)
    try
        with(f, obj)
    catch err
        warn("$(string(T)) thrown exception: $err")
    end
end

"""
    LibGit2.Const.OBJECT{T<:GitObject}(::Type{T})

The `OBJECT` enum value corresponding to type `T`.
"""
Consts.OBJECT(::Type{GitCommit})        = Consts.OBJ_COMMIT
Consts.OBJECT(::Type{GitTree})          = Consts.OBJ_TREE
Consts.OBJECT(::Type{GitBlob})          = Consts.OBJ_BLOB
Consts.OBJECT(::Type{GitTag})           = Consts.OBJ_TAG
Consts.OBJECT(::Type{GitUnknownObject}) = Consts.OBJ_ANY
Consts.OBJECT(::Type{GitObject})        = Consts.OBJ_ANY

Consts.OBJECT(ptr::Ptr{Void}) =
    ccall((:git_object_type, :libgit2), Consts.OBJECT, (Ptr{Void},), ptr)

"""
    objtype(obj_type::Consts.OBJECT)

Returns the type corresponding to the enum value.
"""
function objtype(obj_type::Consts.OBJECT)
    if obj_type == Consts.OBJ_COMMIT
        GitCommit
    elseif obj_type == Consts.OBJ_TREE
        GitTree
    elseif obj_type == Consts.OBJ_BLOB
        GitBlob
    elseif obj_type == Consts.OBJ_TAG
        GitTag
    elseif obj_type == Consts.OBJ_ANY #this name comes from the header
        GitUnknownObject
    else
        throw(GitError(Error.Object, Error.ENOTFOUND, "Object type $obj_type is not supported"))
    end
end

import Base.securezero!

"Credentials that support only `user` and `password` parameters"
type UserPasswordCredentials <: AbstractCredentials
    user::String
    pass::String
    prompt_if_incorrect::Bool    # Whether to allow interactive prompting if the credentials are incorrect
    count::Int                   # authentication failure protection count
    function UserPasswordCredentials(u::AbstractString,p::AbstractString,prompt_if_incorrect::Bool=false)
        c = new(u,p,prompt_if_incorrect,3)
        finalizer(c, securezero!)
        return c
    end
    UserPasswordCredentials(prompt_if_incorrect::Bool=false) = UserPasswordCredentials("","",prompt_if_incorrect)
end

function securezero!(cred::UserPasswordCredentials)
    securezero!(cred.user)
    securezero!(cred.pass)
    cred.count = 0
    return cred
end

"SSH credentials type"
type SSHCredentials <: AbstractCredentials
    user::String
    pass::String
    pubkey::String
    prvkey::String
    usesshagent::String  # used for ssh-agent authentication
    prompt_if_incorrect::Bool    # Whether to allow interactive prompting if the credentials are incorrect
    count::Int

    function SSHCredentials(u::AbstractString,p::AbstractString,prompt_if_incorrect::Bool=false)
        c = new(u,p,"","","Y",prompt_if_incorrect,3)
        finalizer(c, securezero!)
        return c
    end
    SSHCredentials(prompt_if_incorrect::Bool=false) = SSHCredentials("","",prompt_if_incorrect)
end
function securezero!(cred::SSHCredentials)
    securezero!(cred.user)
    securezero!(cred.pass)
    securezero!(cred.pubkey)
    securezero!(cred.prvkey)
    cred.count = 0
    return cred
end

"Credentials that support caching"
type CachedCredentials <: AbstractCredentials
    cred::Dict{String,AbstractCredentials}
    count::Int            # authentication failure protection count
    CachedCredentials() = new(Dict{String,AbstractCredentials}(),3)
end

"Checks if credentials were used or failed authentication, see `LibGit2.credentials_callback`"
function checkused!(p::Union{UserPasswordCredentials, SSHCredentials})
    p.count <= 0 && return true
    p.count -= 1
    return false
end
reset!(p::Union{UserPasswordCredentials, SSHCredentials}, cnt::Int=3) = (p.count = cnt; p)
reset!(p::CachedCredentials) = (foreach(reset!, values(p.cred)); p)

"Obtain the cached credentials for the given host+protocol (credid), or return and store the default if not found"
get_creds!(collection::CachedCredentials, credid, default) = get!(collection.cred, credid, default)
get_creds!(creds::AbstractCredentials, credid, default) = creds
get_creds!(creds::Void, credid, default) = default
function get_creds!(creds::Ref{Nullable{AbstractCredentials}}, credid, default)
    if isnull(creds[])
        creds[] = Nullable{AbstractCredentials}(default)
        return default
    else
        get_creds!(Base.get(creds[]), credid, default)
    end
end

function securezero!(p::CachedCredentials)
    foreach(securezero!, values(p.cred))
    return p
end

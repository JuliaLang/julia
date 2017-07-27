# This file is a part of Julia. License is MIT: https://julialang.org/license

import Base.@kwdef
import .Consts: GIT_SUBMODULE_IGNORE, GIT_MERGE_FILE_FAVOR, GIT_MERGE_FILE, GIT_CONFIG

const OID_RAWSZ = 20
const OID_HEXSZ = OID_RAWSZ * 2
const OID_MINPREFIXLEN = 4

abstract type AbstractGitHash end

"""
    GitHash

A git object identifier, based on the sha-1 hash. It is a $OID_RAWSZ byte string
($OID_HEXSZ hex digits) used to identify a `GitObject` in a repository.
"""
struct GitHash <: AbstractGitHash
    val::NTuple{OID_RAWSZ, UInt8}
    GitHash(val::NTuple{OID_RAWSZ, UInt8}) = new(val)
end
GitHash() = GitHash(ntuple(i->zero(UInt8), OID_RAWSZ))

"""
    GitShortHash(hash::GitHash, len::Integer)

A shortened git object identifier, which can be used to identify a git object when it is
unique, consisting of the initial `len` hexadecimal digits of `hash` (the remaining digits
are ignored).
"""
struct GitShortHash <: AbstractGitHash
    hash::GitHash   # underlying hash: unused digits are ignored
    len::Csize_t    # length in hex digits
end


"""
    LibGit2.TimeStruct

Time in a signature.
Matches the [`git_time`](https://libgit2.github.com/libgit2/#HEAD/type/git_time) struct.
"""
struct TimeStruct
    time::Int64     # time in seconds from epoch
    offset::Cint    # timezone offset in minutes
end

"""
    LibGit2.SignatureStruct

An action signature (e.g. for committers, taggers, etc).
Matches the [`git_signature`](https://libgit2.github.com/libgit2/#HEAD/type/git_signature) struct.
"""
struct SignatureStruct
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
struct StrArrayStruct
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
struct Buffer
    ptr::Ptr{Cchar}
    asize::Csize_t
    size::Csize_t
end
Buffer() = Buffer(C_NULL, 0, 0)

function free(buf_ref::Base.Ref{Buffer})
    ccall((:git_buf_free, :libgit2), Void, (Ptr{Buffer},), buf_ref)
end

"Abstract credentials payload"
abstract type AbstractCredentials end

"Checks if credentials were used"
checkused!(p::AbstractCredentials) = true
checkused!(p::Void) = false
"Resets credentials for another use"
reset!(p::AbstractCredentials, cnt::Int=3) = nothing

"""
    LibGit2.CheckoutOptions

Matches the [`git_checkout_options`](https://libgit2.github.com/libgit2/#HEAD/type/git_checkout_options) struct.
"""
@kwdef struct CheckoutOptions
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
@kwdef struct RemoteCallbacks
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

The fields represent:
  * `version`: version of the struct in use, in case this changes later. For now, always `1`.
  * `proxytype`: an `enum` for the type of proxy to use.
     Defined in [`git_proxy_t`](https://libgit2.github.com/libgit2/#HEAD/type/git_proxy_t).
     The corresponding Julia enum is `GIT_PROXY` and has values:
     - `PROXY_NONE`: do not attempt the connection through a proxy.
     - `PROXY_AUTO`: attempt to figure out the proxy configuration from the git configuration.
     - `PROXY_SPECIFIED`: connect using the URL given in the `url` field of this struct.
     Default is to auto-detect the proxy type.
  * `url`: the URL of the proxy.
  * `credential_cb`: a pointer to a callback function which will be called if the remote
    requires authentication to connect.
  * `certificate_cb`: a pointer to a callback function which will be called if certificate
    verification fails. This lets the user decide whether or not to keep connecting. If
    the function returns `1`, connecting will be allowed. If it returns `0`, the connection
    will not be allowed. A negative value can be used to return errors.
  * `payload`: the payload to be provided to the two callback functions.

# Examples
```julia-repl
julia> fo = LibGit2.FetchOptions();

julia> fo.proxy_opts = LibGit2.ProxyOptions(url=Cstring("https://my_proxy_url.com"))

julia> fetch(remote, "master", options=fo)
```
"""
@kwdef struct ProxyOptions
    version::Cuint               = 1
    proxytype::Consts.GIT_PROXY  = Consts.PROXY_AUTO
    url::Cstring
    credential_cb::Ptr{Void}
    certificate_cb::Ptr{Void}
    payload::Ptr{Void}
end


"""
    LibGit2.FetchOptions

Matches the [`git_fetch_options`](https://libgit2.github.com/libgit2/#HEAD/type/git_fetch_options) struct.
"""
@kwdef struct FetchOptions
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
@kwdef struct CloneOptions
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
@kwdef struct DiffOptionsStruct
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
    LibGit2.DescribeOptions

Matches the [`git_describe_options`](https://libgit2.github.com/libgit2/#HEAD/type/git_describe_options) struct.
"""
@kwdef struct DescribeOptions
    version::Cuint             = 1
    max_candidates_tags::Cuint = 10
    describe_strategy::Cuint   = Consts.DESCRIBE_DEFAULT

    pattern::Cstring
    only_follow_first_parent::Cint
    show_commit_oid_as_fallback::Cint
end

"""
    LibGit2.DescribeFormatOptions

Matches the [`git_describe_format_options`](https://libgit2.github.com/libgit2/#HEAD/type/git_describe_format_options) struct.

The fields represent:
  * `version`: version of the struct in use, in case this changes later. For now, always `1`.
  * `abbreviated_size`: lower bound on the size of the abbreviated `GitHash` to use, defaulting to `7`.
  * `always_use_long_format`: set to `1` to use the long format for strings even if a short format can be used.
  * `dirty_suffix`: if set, this will be appended to the end of the description string if the [`workdir`](@ref) is dirty.
"""
@kwdef struct DescribeFormatOptions
    version::Cuint          = 1
    abbreviated_size::Cuint = 7
    always_use_long_format::Cint
    dirty_suffix::Cstring
end

"""
    LibGit2.DiffFile

Description of one side of a delta.
Matches the [`git_diff_file`](https://libgit2.github.com/libgit2/#HEAD/type/git_diff_file) struct.

The fields represent:
  * `id`: the [`GitHash`](@ref) of the item in the diff. If the item is empty on this
     side of the diff (for instance, if the diff is of the removal of a file), this will
     be `GitHash(0)`.
  * `path`: a `NULL` terminated path to the item relative to the working directory of the repository.
  * `size`: the size of the item in bytes.
  * `flags`: a combination of the [`git_diff_flag_t`](https://libgit2.github.com/libgit2/#HEAD/type/git_diff_flag_t)
     flags. The `i`th bit of this integer sets the `i`th flag.
  * `mode`: the [`stat`](@ref) mode for the item.
  * `id_abbrev`: only present in LibGit2 versions newer than or equal to `0.25.0`.
     The length of the `id` field when converted using [`hex`](@ref). Usually equal to `OID_HEXSZ` ($OID_HEXSZ).
"""
struct DiffFile
    id::GitHash
    path::Cstring
    size::Int64
    flags::UInt32
    mode::UInt16
    @static if LibGit2.VERSION >= v"0.25.0"
        id_abbrev::UInt16
    end
end

function Base.show(io::IO, df::DiffFile)
    println(io, "DiffFile:")
    println(io, "Oid: $(df.id))")
    println(io, "Path: $(df.path)")
    println(io, "Size: $(df.size)")
end

"""
    LibGit2.DiffDelta

Description of changes to one entry.
Matches the [`git_diff_delta`](https://libgit2.github.com/libgit2/#HEAD/type/git_diff_delta) struct.

The fields represent:
  * `status`: One of `Consts.DELTA_STATUS`, indicating whether the file has been added/modified/deleted.
  * `flags`: Flags for the delta and the objects on each side. Determines whether to treat the file(s)
     as binary/text, whether they exist on each side of the diff, and whether the object ids are known
     to be correct.
  * `similarity`: Used to indicate if a file has been renamed or copied.
  * `nfiles`: The number of files in the delta (for instance, if the delta
     was run on a submodule commit id, it may contain more than one file).
  * `old_file`: A [`DiffFile`](@ref) containing information about the file(s) before the changes.
  * `new_file`: A [`DiffFile`](@ref) containing information about the file(s) after the changes.
"""
struct DiffDelta
    status::Cint
    flags::UInt32
    similarity::UInt16
    nfiles::UInt16
    old_file::DiffFile
    new_file::DiffFile
end

function Base.show(io::IO, dd::DiffDelta)
    println(io, "DiffDelta:")
    println(io, "Status: $(Consts.DELTA_STATUS(dd.status))")
    println(io, "Number of files: $(dd.nfiles)")
    println(io, "Old file:\n$(dd.old_file)")
    println(io, "New file:\n$(dd.new_file)")
end

"""
    LibGit2.MergeOptions

Matches the [`git_merge_options`](https://libgit2.github.com/libgit2/#HEAD/type/git_merge_options) struct.
"""
@kwdef struct MergeOptions
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
    LibGit2.BlameOptions

Matches the [`git_blame_options`](https://libgit2.github.com/libgit2/#HEAD/type/git_blame_options) struct.
"""
@kwdef struct BlameOptions
    version::Cuint                    = 1
    flags::UInt32                     = 0
    min_match_characters::UInt16      = 20
    newest_commit::GitHash
    oldest_commit::GitHash
    min_line::Csize_t                 = 1
    max_line::Csize_t                 = 0
end

"""
    LibGit2.PushOptions

Matches the [`git_push_options`](https://libgit2.github.com/libgit2/#HEAD/type/git_push_options) struct.

The fields represent:
  * `version`: version of the struct in use, in case this changes later. For now, always `1`.
  * `parallelism`: if a pack file must be created, this variable sets the number of worker
     threads which will be spawned by the packbuilder. If `0`, the packbuilder will auto-set
     the number of threads to use. The default is `1`.
  * `callbacks`: the callbacks (e.g. for authentication with the remote) to use for the push.
  * `proxy_opts`: only relevant if the LibGit2 version is greater than or equal to `0.25.0`.
     Sets options for using a proxy to communicate with a remote. See [`ProxyOptions`](@ref)
     for more information.
  * `custom_headers`: only relevant if the LibGit2 version is greater than or equal to `0.24.0`.
     Extra headers needed for the push operation.
"""
@kwdef struct PushOptions
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
    LibGit2.CherrypickOptions

Matches the [`git_cherrypick_options`](https://libgit2.github.com/libgit2/#HEAD/type/git_cherrypick_options) struct.
"""
@kwdef struct CherrypickOptions
    version::Cuint = 1
    mainline::Cuint = 0
    merge_opts::MergeOptions=MergeOptions()
    checkout_opts::CheckoutOptions=CheckoutOptions()
end


"""
    LibGit2.IndexTime

Matches the [`git_index_time`](https://libgit2.github.com/libgit2/#HEAD/type/git_index_time) struct.
"""
struct IndexTime
    seconds::Int64
    nanoseconds::Cuint
end

"""
    LibGit2.IndexEntry

In-memory representation of a file entry in the index.
Matches the [`git_index_entry`](https://libgit2.github.com/libgit2/#HEAD/type/git_index_entry) struct.
"""
struct IndexEntry
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
@kwdef struct RebaseOptions
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
struct RebaseOperation
    optype::Cint
    id::GitHash
    exec::Cstring
end
function Base.show(io::IO, rbo::RebaseOperation)
    println(io, "RebaseOperation($(string(rbo.id)))")
    println(io, "Operation type: $(Consts.GIT_REBASE_OPERATION(rbo.optype))")
end

"""
    LibGit2.StatusOptions

Options to control how `git_status_foreach_ext()` will issue callbacks.
Matches the [`git_status_opt_t`](https://libgit2.github.com/libgit2/#HEAD/type/git_status_opt_t) struct.
"""
@kwdef struct StatusOptions
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
struct StatusEntry
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
struct FetchHead
    name::String
    url::String
    oid::GitHash
    ismerge::Bool
end

function Base.show(io::IO, fh::FetchHead)
    println(io, "FetchHead:")
    println(io, "Name: $(fh.name)")
    println(io, "URL: $(fh.url)")
    print(io, "OID: ")
    show(io, fh.oid)
    println(io)
    println(io, "Merged: $(fh.ismerge)")
end

"""
    LibGit2.ConfigEntry

Matches the [`git_config_entry`](https://libgit2.github.com/libgit2/#HEAD/type/git_config_entry) struct.
"""
@kwdef struct ConfigEntry
    name::Cstring
    value::Cstring
    level::GIT_CONFIG = Consts.CONFIG_LEVEL_DEFAULT
    free::Ptr{Void}
    payload::Ptr{Void}
end

function Base.show(io::IO, ce::ConfigEntry)
    print(io, "ConfigEntry(\"", unsafe_string(ce.name), "\", \"", unsafe_string(ce.value), "\")")
end

# Abstract object types
abstract type AbstractGitObject end
Base.isempty(obj::AbstractGitObject) = (obj.ptr == C_NULL)

abstract type GitObject <: AbstractGitObject end

for (typ, owntyp, sup, cname) in [
    (:GitRepo,           nothing,               :AbstractGitObject, :git_repository),
    (:GitConfig,         :(Nullable{GitRepo}),  :AbstractGitObject, :git_config),
    (:GitIndex,          :(Nullable{GitRepo}),  :AbstractGitObject, :git_index),
    (:GitRemote,         :GitRepo,              :AbstractGitObject, :git_remote),
    (:GitRevWalker,      :GitRepo,              :AbstractGitObject, :git_revwalk),
    (:GitReference,      :GitRepo,              :AbstractGitObject, :git_reference),
    (:GitDescribeResult, :GitRepo,              :AbstractGitObject, :git_describe_result),
    (:GitDiff,           :GitRepo,              :AbstractGitObject, :git_diff),
    (:GitDiffStats,      :GitRepo,              :AbstractGitObject, :git_diff_stats),
    (:GitAnnotated,      :GitRepo,              :AbstractGitObject, :git_annotated_commit),
    (:GitRebase,         :GitRepo,              :AbstractGitObject, :git_rebase),
    (:GitBlame,          :GitRepo,              :AbstractGitObject, :git_blame),
    (:GitStatus,         :GitRepo,              :AbstractGitObject, :git_status_list),
    (:GitBranchIter,     :GitRepo,              :AbstractGitObject, :git_branch_iterator),
    (:GitConfigIter,     nothing,               :AbstractGitObject, :git_config_iterator),
    (:GitUnknownObject,  :GitRepo,              :GitObject,         :git_object),
    (:GitCommit,         :GitRepo,              :GitObject,         :git_commit),
    (:GitBlob,           :GitRepo,              :GitObject,         :git_blob),
    (:GitTree,           :GitRepo,              :GitObject,         :git_tree),
    (:GitTag,            :GitRepo,              :GitObject,         :git_tag),
    (:GitTreeEntry,      :GitTree,              :AbstractGitObject, :git_tree_entry),
    ]

    if owntyp === nothing
        @eval mutable struct $typ <: $sup
            ptr::Ptr{Void}
            function $typ(ptr::Ptr{Void}, fin::Bool=true)
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
    else
        @eval mutable struct $typ <: $sup
            owner::$owntyp
            ptr::Ptr{Void}
            function $typ(owner::$owntyp, ptr::Ptr{Void}, fin::Bool=true)
                @assert ptr != C_NULL
                obj = new(owner, ptr)
                if fin
                    Threads.atomic_add!(REFCOUNT, UInt(1))
                    finalizer(obj, Base.close)
                end
                return obj
            end
        end
        if isa(owntyp, Expr) && owntyp.args[1] == :Nullable
            @eval begin
                $typ(ptr::Ptr{Void}, fin::Bool=true) = $typ($owntyp(), ptr, fin)
                $typ(owner::$(owntyp.args[2]), ptr::Ptr{Void}, fin::Bool=true) =
                    $typ($owntyp(owner), ptr, fin)
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
mutable struct GitSignature <: AbstractGitObject
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
mutable struct Signature
    name::String
    email::String
    time::Int64
    time_offset::Cint
end

"""
    LibGit2.BlameHunk

Matches the [`git_blame_hunk`](https://libgit2.github.com/libgit2/#HEAD/type/git_blame_hunk) struct.
The fields represent:
    * `lines_in_hunk`: the number of lines in this hunk of the blame.
    * `final_commit_id`: the [`GitHash`](@ref) of the commit where this section was last changed.
    * `final_start_line_number`: the *one based* line number in the file where the
       hunk starts, in the *final* version of the file.
    * `final_signature`: the signature of the person who last modified this hunk. You will
       need to pass this to [`Signature`](@ref) to access its fields.
    * `orig_commit_id`: the [`GitHash`](@ref) of the commit where this hunk was first found.
    * `orig_path`: the path to the file where the hunk originated. This may be different
       than the current/final path, for instance if the file has been moved.
    * `orig_start_line_number`: the *one based* line number in the file where the
       hunk starts, in the *original* version of the file at `orig_path`.
    * `orig_signature`: the signature of the person who introduced this hunk. You will
       need to pass this to [`Signature`](@ref) to access its fields.
    * `boundary`: `'1'` if the original commit is a "boundary" commit (for instance, if it's
       equal to an oldest commit set in `options`).
"""
@kwdef struct BlameHunk
    lines_in_hunk::Csize_t

    final_commit_id::GitHash
    final_start_line_number::Csize_t
    final_signature::Ptr{SignatureStruct}

    orig_commit_id::GitHash
    orig_path::Cstring
    orig_start_line_number::Csize_t
    orig_signature::Ptr{SignatureStruct}

    boundary::Char
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

with(f::Function, ::Type{T}, args...) where {T} = with(f, T(args...))

function with_warn(f::Function, ::Type{T}, args...) where T
    obj = T(args...)
    try
        with(f, obj)
    catch err
        warn("$(string(T)) thrown exception: $err")
    end
end

"""
    LibGit2.Consts.OBJECT(::Type{T}) where T<:GitObject

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
mutable struct UserPasswordCredentials <: AbstractCredentials
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

function Base.:(==)(a::UserPasswordCredentials, b::UserPasswordCredentials)
    a.user == b.user && a.pass == b.pass
end

"SSH credentials type"
mutable struct SSHCredentials <: AbstractCredentials
    user::String
    pass::String
    prvkey::String
    pubkey::String
    usesshagent::String  # used for ssh-agent authentication
    prompt_if_incorrect::Bool    # Whether to allow interactive prompting if the credentials are incorrect
    count::Int
    function SSHCredentials(u::AbstractString,p::AbstractString,prvkey::AbstractString,pubkey::AbstractString,prompt_if_incorrect::Bool=false)
        c = new(u,p,prvkey,pubkey,"Y",prompt_if_incorrect,3)
        finalizer(c, securezero!)
        return c
    end
    SSHCredentials(u::AbstractString,p::AbstractString,prompt_if_incorrect::Bool=false) = SSHCredentials(u,p,"","",prompt_if_incorrect)
    SSHCredentials(prompt_if_incorrect::Bool=false) = SSHCredentials("","","","",prompt_if_incorrect)
end

function securezero!(cred::SSHCredentials)
    securezero!(cred.user)
    securezero!(cred.pass)
    securezero!(cred.prvkey)
    securezero!(cred.pubkey)
    cred.count = 0
    return cred
end

function Base.:(==)(a::SSHCredentials, b::SSHCredentials)
    a.user == b.user && a.pass == b.pass && a.prvkey == b.prvkey && a.pubkey == b.pubkey
end

"Credentials that support caching"
mutable struct CachedCredentials <: AbstractCredentials
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

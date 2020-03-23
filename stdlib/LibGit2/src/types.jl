# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base: something
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
GitHash(h::GitHash) = h

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
Matches the [`git_time`](https://libgit2.org/libgit2/#HEAD/type/git_time) struct.
"""
struct TimeStruct
    time::Int64     # time in seconds from epoch
    offset::Cint    # timezone offset in minutes
    @static if LibGit2.VERSION >= v"0.27.0"
        sign::Cchar
    end
end

"""
    LibGit2.SignatureStruct

An action signature (e.g. for committers, taggers, etc).
Matches the [`git_signature`](https://libgit2.org/libgit2/#HEAD/type/git_signature) struct.

The fields represent:
  * `name`: The full name of the committer or author of the commit.
  * `email`: The email at which the committer/author can be contacted.
  * `when`: a [`TimeStruct`](@ref) indicating when the commit was
     authored/committed into the repository.
"""
struct SignatureStruct
    name::Ptr{UInt8}  # full name of the author
    email::Ptr{UInt8} # email of the author
    when::TimeStruct  # time when the action happened
end

"""
    LibGit2.StrArrayStruct

A LibGit2 representation of an array of strings.
Matches the [`git_strarray`](https://libgit2.org/libgit2/#HEAD/type/git_strarray) struct.

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
    ensure_initialized()
    ccall((:git_strarray_free, :libgit2), Cvoid, (Ptr{StrArrayStruct},), sa_ref)
end

"""
    LibGit2.Buffer

A data buffer for exporting data from libgit2.
Matches the [`git_buf`](https://libgit2.org/libgit2/#HEAD/type/git_buf) struct.

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
    ensure_initialized()
    ccall((:git_buf_free, :libgit2), Cvoid, (Ptr{Buffer},), buf_ref)
end

"""
    LibGit2.CheckoutOptions

Matches the [`git_checkout_options`](https://libgit2.org/libgit2/#HEAD/type/git_checkout_options) struct.

The fields represent:
  * `version`: version of the struct in use, in case this changes later. For now, always `1`.
  * `checkout_strategy`: determine how to handle conflicts and whether to force the
     checkout/recreate missing files.
  * `disable_filters`: if nonzero, do not apply filters like CLRF (to convert file newlines between UNIX and DOS).
  * `dir_mode`: read/write/access mode for any directories involved in the checkout. Default is `0755`.
  * `file_mode`: read/write/access mode for any files involved in the checkout.
     Default is `0755` or `0644`, depending on the blob.
  * `file_open_flags`: bitflags used to open any files during the checkout.
  * `notify_flags`: Flags for what sort of conflicts the user should be notified about.
  * `notify_cb`: An optional callback function to notify the user if a checkout conflict occurs.
     If this function returns a non-zero value, the checkout will be cancelled.
  * `notify_payload`: Payload for the notify callback function.
  * `progress_cb`: An optional callback function to display checkout progress.
  * `progress_payload`: Payload for the progress callback.
  * `paths`: If not empty, describes which paths to search during the checkout.
     If empty, the checkout will occur over all files in the repository.
  * `baseline`: Expected content of the [`workdir`](@ref), captured in a (pointer to a)
     [`GitTree`](@ref). Defaults to the state of the tree at HEAD.
  * `baseline_index`: Expected content of the [`workdir`](@ref), captured in a (pointer to a)
     `GitIndex`. Defaults to the state of the index at HEAD.
  * `target_directory`: If not empty, checkout to this directory instead of the `workdir`.
  * `ancestor_label`: In case of conflicts, the name of the common ancestor side.
  * `our_label`: In case of conflicts, the name of "our" side.
  * `their_label`: In case of conflicts, the name of "their" side.
  * `perfdata_cb`: An optional callback function to display performance data.
  * `perfdata_payload`: Payload for the performance callback.
"""
@kwdef struct CheckoutOptions
    version::Cuint               = Cuint(1)

    checkout_strategy::Cuint     = Consts.CHECKOUT_SAFE

    disable_filters::Cint        = Cint(0)
    dir_mode::Cuint              = Cuint(0)
    file_mode::Cuint             = Cuint(0)
    file_open_flags::Cint        = Cint(0)

    notify_flags::Cuint          = Consts.CHECKOUT_NOTIFY_NONE
    notify_cb::Ptr{Cvoid}        = C_NULL
    notify_payload::Ptr{Cvoid}   = C_NULL

    progress_cb::Ptr{Cvoid}      = C_NULL
    progress_payload::Ptr{Cvoid} = C_NULL

    paths::StrArrayStruct        = StrArrayStruct()

    baseline::Ptr{Cvoid}         = C_NULL
    baseline_index::Ptr{Cvoid}   = C_NULL

    target_directory::Cstring    = Cstring(C_NULL)
    ancestor_label::Cstring      = Cstring(C_NULL)
    our_label::Cstring           = Cstring(C_NULL)
    their_label::Cstring         = Cstring(C_NULL)

    perfdata_cb::Ptr{Cvoid}      = C_NULL
    perfdata_payload::Ptr{Cvoid} = C_NULL
end

"""
    LibGit2.TransferProgress

Transfer progress information used by the `transfer_progress` remote callback.
Matches the [`git_transfer_progress`](https://libgit2.org/libgit2/#HEAD/type/git_transfer_progress) struct.
"""
@kwdef struct TransferProgress
    total_objects::Cuint    = Cuint(0)
    indexed_objects::Cuint  = Cuint(0)
    received_objects::Cuint = Cuint(0)
    local_objects::Cuint    = Cuint(0)
    total_deltas::Cuint     = Cuint(0)
    indexed_deltas::Cuint   = Cuint(0)
    received_bytes::Csize_t = Csize_t(0)
end

@kwdef struct RemoteCallbacksStruct
    version::Cuint                     = Cuint(1)
    sideband_progress::Ptr{Cvoid}      = C_NULL
    completion::Ptr{Cvoid}             = C_NULL
    credentials::Ptr{Cvoid}            = C_NULL
    certificate_check::Ptr{Cvoid}      = C_NULL
    transfer_progress::Ptr{Cvoid}      = C_NULL
    update_tips::Ptr{Cvoid}            = C_NULL
    pack_progress::Ptr{Cvoid}          = C_NULL
    push_transfer_progress::Ptr{Cvoid} = C_NULL
    push_update_reference::Ptr{Cvoid}  = C_NULL
    push_negotiation::Ptr{Cvoid}       = C_NULL
    transport::Ptr{Cvoid}              = C_NULL
    payload::Ptr{Cvoid}                = C_NULL
    @static if LibGit2.VERSION >= v"0.99.0"
        resolve_url::Ptr{Cvoid}        = C_NULL
    end
end

"""
    LibGit2.Callbacks

A dictionary which containing the callback name as the key and the value as a tuple of the
callback function and payload.

The `Callback` dictionary to construct `RemoteCallbacks` allows each callback to use a
distinct payload. Each callback, when called, will receive `Dict` which will hold the
callback's custom payload which can be accessed using the callback name.

# Examples
```julia
julia> c = LibGit2.Callbacks(:credentials => (LibGit2.credentials_cb(), LibGit2.CredentialPayload()));

julia> LibGit2.clone(url, callbacks=c);
```

See [`git_remote_callbacks`](https://libgit2.org/libgit2/#HEAD/type/git_remote_callbacks)
for details on supported callbacks.
"""
const Callbacks = Dict{Symbol, Tuple{Ptr{Cvoid}, Any}}

"""
    LibGit2.RemoteCallbacks

Callback settings.
Matches the [`git_remote_callbacks`](https://libgit2.org/libgit2/#HEAD/type/git_remote_callbacks) struct.
"""
struct RemoteCallbacks
    cb::RemoteCallbacksStruct
    gcroot::Ref{Any}

    function RemoteCallbacks(; version::Cuint=Cuint(1), payload=C_NULL, callbacks...)
        p = Ref{Any}(payload)
        pp = unsafe_load(Ptr{Ptr{Cvoid}}(Base.unsafe_convert(Ptr{Any}, p)))
        return new(RemoteCallbacksStruct(; version=version, payload=pp, callbacks...), p)
    end
end

function RemoteCallbacks(c::Callbacks)
    callbacks = Dict{Symbol, Ptr{Cvoid}}()
    payloads = Dict{Symbol, Any}()

    for (name, (callback, payload)) in c
        callbacks[name] = callback
        payloads[name] = payload
    end

    RemoteCallbacks(; payload=payloads, callbacks...)
end


"""
    LibGit2.ProxyOptions

Options for connecting through a proxy.

Matches the [`git_proxy_options`](https://libgit2.org/libgit2/#HEAD/type/git_proxy_options) struct.

The fields represent:
  * `version`: version of the struct in use, in case this changes later. For now, always `1`.
  * `proxytype`: an `enum` for the type of proxy to use.
     Defined in [`git_proxy_t`](https://libgit2.org/libgit2/#HEAD/type/git_proxy_t).
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
julia> fo = LibGit2.FetchOptions(
           proxy_opts = LibGit2.ProxyOptions(url = Cstring("https://my_proxy_url.com")))

julia> fetch(remote, "master", options=fo)
```
"""
@kwdef struct ProxyOptions
    version::Cuint               = Cuint(1)
    proxytype::Consts.GIT_PROXY  = Consts.PROXY_AUTO
    url::Cstring                 = Cstring(C_NULL)
    credential_cb::Ptr{Cvoid}    = C_NULL
    certificate_cb::Ptr{Cvoid}   = C_NULL
    payload::Ptr{Cvoid}          = C_NULL
end

@kwdef struct FetchOptionsStruct
    version::Cuint                     = Cuint(1)
    callbacks::RemoteCallbacksStruct   = RemoteCallbacksStruct()
    prune::Cint                        = Consts.FETCH_PRUNE_UNSPECIFIED
    update_fetchhead::Cint             = Cint(1)
    download_tags::Cint                = Consts.REMOTE_DOWNLOAD_TAGS_AUTO
    @static if LibGit2.VERSION >= v"0.25.0"
        proxy_opts::ProxyOptions       = ProxyOptions()
    end
    @static if LibGit2.VERSION >= v"0.24.0"
        custom_headers::StrArrayStruct = StrArrayStruct()
    end
end

"""
    LibGit2.FetchOptions

Matches the [`git_fetch_options`](https://libgit2.org/libgit2/#HEAD/type/git_fetch_options) struct.

The fields represent:
  * `version`: version of the struct in use, in case this changes later. For now, always `1`.
  * `callbacks`: remote callbacks to use during the fetch.
  * `prune`: whether to perform a prune after the fetch or not. The default is to
     use the setting from the `GitConfig`.
  * `update_fetchhead`: whether to update the [`FetchHead`](@ref) after the fetch.
     The default is to perform the update, which is the normal git behavior.
  * `download_tags`: whether to download tags present at the remote or not. The default
     is to request the tags for objects which are being downloaded anyway from the server.
  * `proxy_opts`: options for connecting to the remote through a proxy. See [`ProxyOptions`](@ref).
     Only present on libgit2 versions newer than or equal to 0.25.0.
  * `custom_headers`: any extra headers needed for the fetch. Only present on libgit2 versions
     newer than or equal to 0.24.0.
"""
struct FetchOptions
    opts::FetchOptionsStruct
    cb_gcroot::Ref{Any}
    function FetchOptions(; callbacks::RemoteCallbacks=RemoteCallbacks(), kwargs...)
        return new(FetchOptionsStruct(; kwargs..., callbacks=callbacks.cb), callbacks.gcroot)
    end
end


@kwdef struct CloneOptionsStruct
    version::Cuint                      = Cuint(1)
    checkout_opts::CheckoutOptions      = CheckoutOptions()
    fetch_opts::FetchOptionsStruct      = FetchOptionsStruct()
    bare::Cint                          = Cint(0)
    localclone::Cint                    = Consts.CLONE_LOCAL_AUTO
    checkout_branch::Cstring            = Cstring(C_NULL)
    repository_cb::Ptr{Cvoid}           = C_NULL
    repository_cb_payload::Ptr{Cvoid}   = C_NULL
    remote_cb::Ptr{Cvoid}               = C_NULL
    remote_cb_payload::Ptr{Cvoid}       = C_NULL
end

"""
    LibGit2.CloneOptions

Matches the [`git_clone_options`](https://libgit2.org/libgit2/#HEAD/type/git_clone_options) struct.

The fields represent:
  * `version`: version of the struct in use, in case this changes later. For now, always `1`.
  * `checkout_opts`: The options for performing the checkout of the remote as part of the clone.
  * `fetch_opts`: The options for performing the pre-checkout fetch of the remote as part of the clone.
  * `bare`: If `0`, clone the full remote repository. If non-zero, perform a bare clone, in which
     there is no local copy of the source files in the repository and the [`gitdir`](@ref) and [`workdir`](@ref)
     are the same.
  * `localclone`: Flag whether to clone a local object database or do a fetch. The default is to let git decide.
     It will not use the git-aware transport for a local clone, but will use it for URLs which begin with `file://`.
  * `checkout_branch`: The name of the branch to checkout. If an empty string, the default branch of the
     remote will be checked out.
  * `repository_cb`: An optional callback which will be used to create the *new* repository into which
     the clone is made.
  * `repository_cb_payload`: The payload for the repository callback.
  * `remote_cb`: An optional callback used to create the [`GitRemote`](@ref) before making the clone from it.
  * `remote_cb_payload`: The payload for the remote callback.
"""
struct CloneOptions
    opts::CloneOptionsStruct
    cb_gcroot::Ref{Any}
    function CloneOptions(; fetch_opts::FetchOptions=FetchOptions(), kwargs...)
        return new(CloneOptionsStruct(; kwargs..., fetch_opts=fetch_opts.opts), fetch_opts.cb_gcroot)
    end
end

"""
    LibGit2.DiffOptionsStruct

Matches the [`git_diff_options`](https://libgit2.org/libgit2/#HEAD/type/git_diff_options) struct.

The fields represent:
  * `version`: version of the struct in use, in case this changes later. For now, always `1`.
  * `flags`: flags controlling which files will appear in the diff. Defaults to `DIFF_NORMAL`.
  * `ignore_submodules`: whether to look at files in submodules or not. Defaults to
    `SUBMODULE_IGNORE_UNSPECIFIED`, which means the submodule's configuration will control
     whether it appears in the diff or not.
  * `pathspec`: path to files to include in the diff. Default is to use all files in the repository.
  * `notify_cb`: optional callback which will notify the user of changes to the diff as file deltas are
     added to it.
  * `progress_cb`: optional callback which will display diff progress. Only relevant on libgit2 versions
     at least as new as 0.24.0.
  * `payload`: the payload to pass to `notify_cb` and `progress_cb`.
  * `context_lines`: the number of *unchanged* lines used to define the edges of a hunk.
     This is also the number of lines which will be shown before/after a hunk to provide
     context. Default is 3.
  * `interhunk_lines`: the maximum number of *unchanged* lines *between* two separate
     hunks allowed before the hunks will be combined. Default is 0.
  * `id_abbrev`: sets the length of the abbreviated [`GitHash`](@ref) to print.
     Default is `7`.
  * `max_size`: the maximum file size of a blob. Above this size, it will be treated
     as a binary blob. The default is 512 MB.
  * `old_prefix`: the virtual file directory in which to place old files on one side
     of the diff. Default is `"a"`.
  * `new_prefix`: the virtual file directory in which to place new files on one side
     of the diff. Default is `"b"`.
"""
@kwdef struct DiffOptionsStruct
    version::Cuint                           = Consts.DIFF_OPTIONS_VERSION
    flags::UInt32                            = Consts.DIFF_NORMAL

    # options controlling which files are in the diff
    ignore_submodules::GIT_SUBMODULE_IGNORE  = Consts.SUBMODULE_IGNORE_UNSPECIFIED
    pathspec::StrArrayStruct                 = StrArrayStruct()
    notify_cb::Ptr{Cvoid}                    = C_NULL
    @static if LibGit2.VERSION >= v"0.24.0"
        progress_cb::Ptr{Cvoid}              = C_NULL
    end
    payload::Ptr{Cvoid}                      = C_NULL

    # options controlling how the diff text is generated
    context_lines::UInt32                    = UInt32(3)
    interhunk_lines::UInt32                  = UInt32(0)
    id_abbrev::UInt16                        = UInt16(7)
    max_size::Int64                          = Int64(512*1024*1024) #512Mb
    old_prefix::Cstring                      = Cstring(C_NULL)
    new_prefix::Cstring                      = Cstring(C_NULL)
end

"""
    LibGit2.DescribeOptions

Matches the [`git_describe_options`](https://libgit2.org/libgit2/#HEAD/type/git_describe_options) struct.

The fields represent:
  * `version`: version of the struct in use, in case this changes later. For now, always `1`.
  * `max_candidates_tags`: consider this many most recent tags in `refs/tags` to describe a commit.
     Defaults to 10 (so that the 10 most recent tags would be examined to see if they describe a commit).
  * `describe_strategy`: whether to consider all entries in `refs/tags` (equivalent to `git-describe --tags`)
     or all entries in `refs/` (equivalent to `git-describe --all`). The default is to only show annotated tags.
     If `Consts.DESCRIBE_TAGS` is passed, all tags, annotated or not, will be considered.
     If `Consts.DESCRIBE_ALL` is passed, any ref in `refs/` will be considered.
  * `pattern`: only consider tags which match `pattern`. Supports glob expansion.
  * `only_follow_first_parent`: when finding the distance from a matching reference to the described
     object, only consider the distance from the first parent.
  * `show_commit_oid_as_fallback`: if no matching reference can be found which describes a commit, show the
     commit's [`GitHash`](@ref) instead of throwing an error (the default behavior).
"""
@kwdef struct DescribeOptions
    version::Cuint                    = Cuint(1)
    max_candidates_tags::Cuint        = Cuint(10)
    describe_strategy::Cuint          = Consts.DESCRIBE_DEFAULT

    pattern::Cstring                  = Cstring(C_NULL)
    only_follow_first_parent::Cint    = Cint(0)
    show_commit_oid_as_fallback::Cint = Cint(0)
end

"""
    LibGit2.DescribeFormatOptions

Matches the [`git_describe_format_options`](https://libgit2.org/libgit2/#HEAD/type/git_describe_format_options) struct.

The fields represent:
  * `version`: version of the struct in use, in case this changes later. For now, always `1`.
  * `abbreviated_size`: lower bound on the size of the abbreviated `GitHash` to use, defaulting to `7`.
  * `always_use_long_format`: set to `1` to use the long format for strings even if a short format can be used.
  * `dirty_suffix`: if set, this will be appended to the end of the description string if the [`workdir`](@ref) is dirty.
"""
@kwdef struct DescribeFormatOptions
    version::Cuint               = Cuint(1)
    abbreviated_size::Cuint      = Cuint(7)
    always_use_long_format::Cint = Cint(0)
    dirty_suffix::Cstring        = Cstring(C_NULL)
end

"""
    LibGit2.DiffFile

Description of one side of a delta.
Matches the [`git_diff_file`](https://libgit2.org/libgit2/#HEAD/type/git_diff_file) struct.

The fields represent:
  * `id`: the [`GitHash`](@ref) of the item in the diff. If the item is empty on this
     side of the diff (for instance, if the diff is of the removal of a file), this will
     be `GitHash(0)`.
  * `path`: a `NULL` terminated path to the item relative to the working directory of the repository.
  * `size`: the size of the item in bytes.
  * `flags`: a combination of the [`git_diff_flag_t`](https://libgit2.org/libgit2/#HEAD/type/git_diff_flag_t)
     flags. The `i`th bit of this integer sets the `i`th flag.
  * `mode`: the [`stat`](@ref) mode for the item.
  * `id_abbrev`: only present in LibGit2 versions newer than or equal to `0.25.0`.
     The length of the `id` field when converted using [`string`](@ref). Usually equal to `OID_HEXSZ` ($OID_HEXSZ).
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
Matches the [`git_diff_delta`](https://libgit2.org/libgit2/#HEAD/type/git_diff_delta) struct.

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

Matches the [`git_merge_options`](https://libgit2.org/libgit2/#HEAD/type/git_merge_options) struct.

The fields represent:
  * `version`: version of the struct in use, in case this changes later. For now, always `1`.
  * `flags`: an `enum` for flags describing merge behavior.
     Defined in [`git_merge_flag_t`](https://github.com/libgit2/libgit2/blob/HEAD/include/git2/merge.h#L95).
     The corresponding Julia enum is `GIT_MERGE` and has values:
     - `MERGE_FIND_RENAMES`: detect if a file has been renamed between the common
       ancestor and the "ours" or "theirs" side of the merge. Allows merges where
       a file has been renamed.
     - `MERGE_FAIL_ON_CONFLICT`: exit immediately if a conflict is found rather
       than trying to resolve it.
     - `MERGE_SKIP_REUC`: do not write the REUC extension on the index resulting
       from the merge.
     - `MERGE_NO_RECURSIVE`: if the commits being merged have multiple merge bases,
       use the first one, rather than trying to recursively merge the bases.
  * `rename_threshold`: how similar two files must to consider one a rename of the other.
    This is an integer that sets the percentage similarity. The default is 50.
  * `target_limit`: the maximum number of files to compare with to look for renames.
    The default is 200.
  * `metric`: optional custom function to use to determine the similarity between two
    files for rename detection.
  * `recursion_limit`: the upper limit on the number of merges of common ancestors to
    perform to try to build a new virtual merge base for the merge. The default is no
    limit. This field is only present on libgit2 versions newer than 0.24.0.
  * `default_driver`: the merge driver to use if both sides have changed. This field
    is only present on libgit2 versions newer than 0.25.0.
  * `file_favor`: how to handle conflicting file contents for the `text` driver.
    - `MERGE_FILE_FAVOR_NORMAL`: if both sides of the merge have changes to a section,
       make a note of the conflict in the index which `git checkout` will use to create
       a merge file, which the user can then reference to resolve the conflicts. This is
       the default.
    - `MERGE_FILE_FAVOR_OURS`: if both sides of the merge have changes to a section,
       use the version in the "ours" side of the merge in the index.
    - `MERGE_FILE_FAVOR_THEIRS`: if both sides of the merge have changes to a section,
       use the version in the "theirs" side of the merge in the index.
    - `MERGE_FILE_FAVOR_UNION`: if both sides of the merge have changes to a section,
       include each unique line from both sides in the file which is put into the index.
  * `file_flags`: guidelines for merging files.
"""
@kwdef struct MergeOptions
    version::Cuint                    = Cuint(1)
    flags::Cint                       = Cint(0)
    rename_threshold::Cuint           = Cuint(50)
    target_limit::Cuint               = Cuint(200)
    metric::Ptr{Cvoid}                = C_NULL
    @static if LibGit2.VERSION >= v"0.24.0"
        recursion_limit::Cuint        = Cuint(0)
    end
    @static if LibGit2.VERSION >= v"0.25.0"
        default_driver::Cstring       = Cstring(C_NULL)
    end
    file_favor::GIT_MERGE_FILE_FAVOR  = Consts.MERGE_FILE_FAVOR_NORMAL
    file_flags::GIT_MERGE_FILE        = Consts.MERGE_FILE_DEFAULT
end

"""
    LibGit2.BlameOptions

Matches the [`git_blame_options`](https://libgit2.org/libgit2/#HEAD/type/git_blame_options) struct.

The fields represent:
  * `version`: version of the struct in use, in case this changes later. For now, always `1`.
  * `flags`: one of `Consts.BLAME_NORMAL` or `Consts.BLAME_FIRST_PARENT` (the other blame flags
     are not yet implemented by libgit2).
  * `min_match_characters`: the minimum number of *alphanumeric* characters which much change
    in a commit in order for the change to be associated with that commit. The default is 20.
    Only takes effect if one of the `Consts.BLAME_*_COPIES` flags are used, which libgit2 does
    not implement yet.
  * `newest_commit`: the [`GitHash`](@ref) of the newest commit from which to look at changes.
  * `oldest_commit`: the [`GitHash`](@ref) of the oldest commit from which to look at changes.
  * `min_line`: the first line of the file from which to starting blaming. The default is `1`.
  * `max_line`: the last line of the file to which to blame. The default is `0`, meaning the
    last line of the file.
"""
@kwdef struct BlameOptions
    version::Cuint                    = Cuint(1)
    flags::UInt32                     = UInt32(0)
    min_match_characters::UInt16      = UInt16(20)
    newest_commit::GitHash            = GitHash()
    oldest_commit::GitHash            = GitHash()
    min_line::Csize_t                 = Csize_t(1)
    max_line::Csize_t                 = Csize_t(0)
end

@kwdef struct PushOptionsStruct
    version::Cuint                     = Cuint(1)
    parallelism::Cint                  = Cint(1)
    callbacks::RemoteCallbacksStruct   = RemoteCallbacksStruct()
    @static if LibGit2.VERSION >= v"0.25.0"
        proxy_opts::ProxyOptions       = ProxyOptions()
    end
    @static if LibGit2.VERSION >= v"0.24.0"
        custom_headers::StrArrayStruct = StrArrayStruct()
    end
end

"""
    LibGit2.PushOptions

Matches the [`git_push_options`](https://libgit2.org/libgit2/#HEAD/type/git_push_options) struct.

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
struct PushOptions
    opts::PushOptionsStruct
    cb_gcroot::Ref{Any}
    function PushOptions(; callbacks::RemoteCallbacks=RemoteCallbacks(), kwargs...)
        return new(PushOptionsStruct(; kwargs..., callbacks=callbacks.cb), callbacks.gcroot)
    end
end

"""
    LibGit2.CherrypickOptions

Matches the [`git_cherrypick_options`](https://libgit2.org/libgit2/#HEAD/type/git_cherrypick_options) struct.

The fields represent:
  * `version`: version of the struct in use, in case this changes later. For now, always `1`.
  * `mainline`: if cherrypicking a merge commit, specifies the parent number (starting at `1`)
    which will allow cherrypick to apply the changes relative to that parent. Only relevant if
    cherrypicking a merge commit. Default is `0`.
  * `merge_opts`: options for merging the changes in. See [`MergeOptions`](@ref) for more information.
  * `checkout_opts`: options for the checkout of the commit being cherrypicked. See [`CheckoutOptions`](@ref)
     for more information.
"""
@kwdef struct CherrypickOptions
    version::Cuint = Cuint(1)
    mainline::Cuint = Cuint(0)
    merge_opts::MergeOptions = MergeOptions()
    checkout_opts::CheckoutOptions = CheckoutOptions()
end


"""
    LibGit2.IndexTime

Matches the [`git_index_time`](https://libgit2.org/libgit2/#HEAD/type/git_index_time) struct.
"""
struct IndexTime
    seconds::Int64
    nanoseconds::Cuint
end

"""
    LibGit2.IndexEntry

In-memory representation of a file entry in the index.
Matches the [`git_index_entry`](https://libgit2.org/libgit2/#HEAD/type/git_index_entry) struct.
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

The fields represent:
  * `version`: version of the struct in use, in case this changes later. For now, always `1`.
  * `quiet`: inform other git clients helping with/working on the rebase that the rebase
    should be done "quietly". Used for interoperability. The default is `1`.
  * `inmemory`: start an in-memory rebase. Callers working on the rebase can go through its
    steps and commit any changes, but cannot rewind HEAD or update the repository. The
    [`workdir`](@ref) will not be modified. Only present on libgit2 versions newer than or equal to 0.24.0.
  * `rewrite_notes_ref`: name of the reference to notes to use to rewrite the commit notes as
    the rebase is finished.
  * `merge_opts`: merge options controlling how the trees will be merged at each rebase step.
     Only present on libgit2 versions newer than or equal to 0.24.0.
  * `checkout_opts`: checkout options for writing files when initializing the rebase, stepping
    through it, and aborting it. See [`CheckoutOptions`](@ref) for more information.
"""
@kwdef struct RebaseOptions
    version::Cuint                 = Cuint(1)
    quiet::Cint                    = Cint(1)
    @static if LibGit2.VERSION >= v"0.24.0"
        inmemory::Cint             = Cint(0)
    end
    rewrite_notes_ref::Cstring     = Cstring(C_NULL)
    @static if LibGit2.VERSION >= v"0.24.0"
        merge_opts::MergeOptions   = MergeOptions()
    end
    checkout_opts::CheckoutOptions = CheckoutOptions()
end

"""
    LibGit2.RebaseOperation

Describes a single instruction/operation to be performed during the rebase.
Matches the [`git_rebase_operation`](https://libgit2.org/libgit2/#HEAD/type/git_rebase_operation_t) struct.

The fields represent:
  * `optype`: the type of rebase operation currently being performed. The options are:
      - `REBASE_OPERATION_PICK`: cherry-pick the commit in question.
      - `REBASE_OPERATION_REWORD`: cherry-pick the commit in question, but rewrite its
        message using the prompt.
      - `REBASE_OPERATION_EDIT`: cherry-pick the commit in question, but allow the user
        to edit the commit's contents and its message.
      - `REBASE_OPERATION_SQUASH`: squash the commit in question into the previous commit.
        The commit messages of the two commits will be merged.
      - `REBASE_OPERATION_FIXUP`: squash the commit in question into the previous commit.
        Only the commit message of the previous commit will be used.
      - `REBASE_OPERATION_EXEC`: do not cherry-pick a commit. Run a command and continue if
        the command exits successfully.
  * `id`: the [`GitHash`](@ref) of the commit being worked on during this rebase step.
  * `exec`: in case `REBASE_OPERATION_EXEC` is used, the command to run during this step
    (for instance, running the test suite after each commit).
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
Matches the [`git_status_opt_t`](https://libgit2.org/libgit2/#HEAD/type/git_status_opt_t) struct.

The fields represent:
  * `version`: version of the struct in use, in case this changes later. For now, always `1`.
  * `show`: a flag for which files to examine and in which order.
    The default is `Consts.STATUS_SHOW_INDEX_AND_WORKDIR`.
  * `flags`: flags for controlling any callbacks used in a status call.
  * `pathspec`: an array of paths to use for path-matching. The behavior of the path-matching
    will vary depending on the values of `show` and `flags`.
  * The `baseline` is the tree to be used for comparison to the working directory and
    index; defaults to HEAD.
"""
@kwdef struct StatusOptions
    version::Cuint           = Cuint(1)
    show::Cint               = Consts.STATUS_SHOW_INDEX_AND_WORKDIR
    flags::Cuint             = Consts.STATUS_OPT_INCLUDE_UNTRACKED |
                               Consts.STATUS_OPT_RECURSE_UNTRACKED_DIRS |
                               Consts.STATUS_OPT_RENAMES_HEAD_TO_INDEX |
                               Consts.STATUS_OPT_SORT_CASE_SENSITIVELY
    pathspec::StrArrayStruct = StrArrayStruct()
    @static if LibGit2.VERSION >= v"0.27.0"
        baseline::Ptr{Cvoid} = C_NULL
    end
end

"""
    LibGit2.StatusEntry

Providing the differences between the file as it exists in HEAD and the index, and
providing the differences between the index and the working directory.
Matches the `git_status_entry` struct.

The fields represent:
  * `status`: contains the status flags for the file, indicating if it is current,
    or has been changed in some way in the index or work tree.
  * `head_to_index`: a pointer to a [`DiffDelta`](@ref) which encapsulates the difference(s)
    between the file as it exists in HEAD and in the index.
  * `index_to_workdir`: a pointer to a `DiffDelta` which encapsulates the difference(s)
    between the file as it exists in the index and in the [`workdir`](@ref).
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

The fields represent:
  * `name`: The name in the local reference database of the fetch head, for example,
     `"refs/heads/master"`.
  * `url`: The URL of the fetch head.
  * `oid`: The [`GitHash`](@ref) of the tip of the fetch head.
  * `ismerge`: Boolean flag indicating whether the changes at the
     remote have been merged into the local copy yet or not. If `true`, the local
     copy is up to date with the remote fetch head.
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

Matches the [`git_config_entry`](https://libgit2.org/libgit2/#HEAD/type/git_config_entry) struct.
"""
@kwdef struct ConfigEntry
    name::Cstring       = Cstring(C_NULL)
    value::Cstring      = Cstring(C_NULL)
    level::GIT_CONFIG   = Consts.CONFIG_LEVEL_DEFAULT
    free::Ptr{Cvoid}    = C_NULL
    payload::Ptr{Cvoid} = C_NULL
end

function Base.show(io::IO, ce::ConfigEntry)
    print(io, "ConfigEntry(\"", unsafe_string(ce.name), "\", \"", unsafe_string(ce.value), "\")")
end

"""
    LibGit2.split_cfg_entry(ce::LibGit2.ConfigEntry) -> Tuple{String,String,String,String}

Break the `ConfigEntry` up to the following pieces: section, subsection, name, and value.

# Examples
Given the git configuration file containing:
```
[credential "https://example.com"]
    username = me
```

The `ConfigEntry` would look like the following:

```julia-repl
julia> entry
ConfigEntry("credential.https://example.com.username", "me")

julia> LibGit2.split_cfg_entry(entry)
("credential", "https://example.com", "username", "me")
```

Refer to the [git config syntax documentation](https://git-scm.com/docs/git-config#_syntax)
for more details.
"""
function split_cfg_entry(ce::ConfigEntry)
    key = unsafe_string(ce.name)

    # Determine the positions of the delimiters
    subsection_delim = something(findfirst(isequal('.'), key), 0)
    name_delim = something(findlast(isequal('.'), key), 0)

    section = SubString(key, 1, subsection_delim - 1)
    subsection = SubString(key, subsection_delim + 1, name_delim - 1)
    name = SubString(key, name_delim + 1)
    value = unsafe_string(ce.value)

    return (section, subsection, name, value)
end

# Abstract object types
abstract type AbstractGitObject end
Base.isempty(obj::AbstractGitObject) = (obj.ptr == C_NULL)

abstract type GitObject <: AbstractGitObject end

for (typ, owntyp, sup, cname) in [
    (:GitRepo,           nothing,                 :AbstractGitObject, :git_repository),
    (:GitConfig,         :(Union{GitRepo, Nothing}), :AbstractGitObject, :git_config),
    (:GitIndex,          :(Union{GitRepo, Nothing}), :AbstractGitObject, :git_index),
    (:GitRemote,         :GitRepo,                :AbstractGitObject, :git_remote),
    (:GitRevWalker,      :GitRepo,                :AbstractGitObject, :git_revwalk),
    (:GitReference,      :GitRepo,                :AbstractGitObject, :git_reference),
    (:GitDescribeResult, :GitRepo,                :AbstractGitObject, :git_describe_result),
    (:GitDiff,           :GitRepo,                :AbstractGitObject, :git_diff),
    (:GitDiffStats,      :GitRepo,                :AbstractGitObject, :git_diff_stats),
    (:GitAnnotated,      :GitRepo,                :AbstractGitObject, :git_annotated_commit),
    (:GitRebase,         :GitRepo,                :AbstractGitObject, :git_rebase),
    (:GitBlame,          :GitRepo,                :AbstractGitObject, :git_blame),
    (:GitStatus,         :GitRepo,                :AbstractGitObject, :git_status_list),
    (:GitBranchIter,     :GitRepo,                :AbstractGitObject, :git_branch_iterator),
    (:GitConfigIter,     nothing,                 :AbstractGitObject, :git_config_iterator),
    (:GitUnknownObject,  :GitRepo,                :GitObject,         :git_object),
    (:GitCommit,         :GitRepo,                :GitObject,         :git_commit),
    (:GitBlob,           :GitRepo,                :GitObject,         :git_blob),
    (:GitTree,           :GitRepo,                :GitObject,         :git_tree),
    (:GitTag,            :GitRepo,                :GitObject,         :git_tag),
    (:GitTreeEntry,      :GitTree,                :AbstractGitObject, :git_tree_entry),
    ]

    if owntyp === nothing
        @eval mutable struct $typ <: $sup
            ptr::Ptr{Cvoid}
            function $typ(ptr::Ptr{Cvoid}, fin::Bool=true)
                # fin=false should only be used when the pointer should not be free'd
                # e.g. from within callback functions which are passed a pointer
                @assert ptr != C_NULL
                obj = new(ptr)
                if fin
                    Threads.atomic_add!(REFCOUNT, 1)
                    finalizer(Base.close, obj)
                end
                return obj
            end
        end
    else
        @eval mutable struct $typ <: $sup
            owner::$owntyp
            ptr::Ptr{Cvoid}
            function $typ(owner::$owntyp, ptr::Ptr{Cvoid}, fin::Bool=true)
                @assert ptr != C_NULL
                obj = new(owner, ptr)
                if fin
                    Threads.atomic_add!(REFCOUNT, 1)
                    finalizer(Base.close, obj)
                end
                return obj
            end
        end
        if isa(owntyp, Expr) && owntyp.args[1] === :Union && owntyp.args[3] === :Nothing
            @eval begin
                $typ(ptr::Ptr{Cvoid}, fin::Bool=true) = $typ(nothing, ptr, fin)
            end
        end
    end
    @eval function Base.close(obj::$typ)
        if obj.ptr != C_NULL
            ensure_initialized()
            ccall(($(string(cname, :_free)), :libgit2), Cvoid, (Ptr{Cvoid},), obj.ptr)
            obj.ptr = C_NULL
            if Threads.atomic_sub!(REFCOUNT, 1) == 1
                # will the last finalizer please turn out the lights?
                ccall((:git_libgit2_shutdown, :libgit2), Cint, ())
            end
        end
    end
end

## Calling `GitObject(repo, ...)` will automatically resolve to the appropriate type.
function GitObject(repo::GitRepo, ptr::Ptr{Cvoid})
    T = objtype(Consts.OBJECT(ptr))
    T(repo, ptr)
end

"""
    LibGit2.GitSignature

This is a Julia wrapper around a pointer to a
[`git_signature`](https://libgit2.org/libgit2/#HEAD/type/git_signature) object.
"""
mutable struct GitSignature <: AbstractGitObject
    ptr::Ptr{SignatureStruct}
    function GitSignature(ptr::Ptr{SignatureStruct})
        @assert ptr != C_NULL
        obj = new(ptr)
        finalizer(Base.close, obj)
        return obj
    end
end
function Base.close(obj::GitSignature)
    if obj.ptr != C_NULL
        ensure_initialized()
        ccall((:git_signature_free, :libgit2), Cvoid, (Ptr{SignatureStruct},), obj.ptr)
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

Matches the [`git_blame_hunk`](https://libgit2.org/libgit2/#HEAD/type/git_blame_hunk) struct.
The fields represent:
    * `lines_in_hunk`: the number of lines in this hunk of the blame.
    * `final_commit_id`: the [`GitHash`](@ref) of the commit where this section was last changed.
    * `final_start_line_number`: the *one based* line number in the file where the
       hunk starts, in the *final* version of the file.
    * `final_signature`: the signature of the person who last modified this hunk. You will
       need to pass this to `Signature` to access its fields.
    * `orig_commit_id`: the [`GitHash`](@ref) of the commit where this hunk was first found.
    * `orig_path`: the path to the file where the hunk originated. This may be different
       than the current/final path, for instance if the file has been moved.
    * `orig_start_line_number`: the *one based* line number in the file where the
       hunk starts, in the *original* version of the file at `orig_path`.
    * `orig_signature`: the signature of the person who introduced this hunk. You will
       need to pass this to `Signature` to access its fields.
    * `boundary`: `'1'` if the original commit is a "boundary" commit (for instance, if it's
       equal to an oldest commit set in `options`).
"""
@kwdef struct BlameHunk
    lines_in_hunk::Csize_t                = Csize_t(0)

    final_commit_id::GitHash              = GitHash()
    final_start_line_number::Csize_t      = Csize_t(0)
    final_signature::Ptr{SignatureStruct} = Ptr{SignatureStruct}(C_NULL)

    orig_commit_id::GitHash               = GitHash()
    orig_path::Cstring                    = Cstring(C_NULL)
    orig_start_line_number::Csize_t       = Csize_t(0)
    orig_signature::Ptr{SignatureStruct}  = Ptr{SignatureStruct}(C_NULL)

    boundary::Char                        = '\0'
end

"""
    with(f::Function, obj)

Resource management helper function. Applies `f` to `obj`, making sure to call
`close` on `obj` after `f` successfully returns or throws an error. Ensures that
allocated git resources are finalized as soon as they are no longer needed.
"""
function with(f::Function, obj)
    try
        f(obj)
    finally
        close(obj)
    end
end

with(f::Function, ::Type{T}, args...) where {T} = with(f, T(args...))

"""
    with_warn(f::Function, ::Type{T}, args...)

Resource management helper function. Apply `f` to `args`, first constructing
an instance of type `T` from `args`. Makes sure to call `close` on the resulting
object after `f` successfully returns or throws an error. Ensures that
allocated git resources are finalized as soon as they are no longer needed. If an
error is thrown by `f`, a warning is shown containing the error.
"""
function with_warn(f::Function, ::Type{T}, args...) where T
    obj = T(args...)
    try
        with(f, obj)
    catch err
        @warn "$(string(T)) thrown exception:" exception=err
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

function Consts.OBJECT(ptr::Ptr{Cvoid})
    ensure_initialized()
    ccall((:git_object_type, :libgit2), Consts.OBJECT, (Ptr{Cvoid},), ptr)
end

"""
    objtype(obj_type::Consts.OBJECT)

Return the type corresponding to the enum value.
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

abstract type AbstractCredential end

"""
    isfilled(cred::AbstractCredential) -> Bool

Verifies that a credential is ready for use in authentication.
"""
isfilled(::AbstractCredential)

"Credential that support only `user` and `password` parameters"
mutable struct UserPasswordCredential <: AbstractCredential
    user::String
    pass::Base.SecretBuffer
    function UserPasswordCredential(user::AbstractString="", pass::Union{AbstractString, Base.SecretBuffer}="")
        new(user, pass)
    end
end

function Base.setproperty!(cred::UserPasswordCredential, name::Symbol, value)
    if name === :pass
        field = getfield(cred, name)
        Base.shred!(field)
    end
    setfield!(cred, name, convert(fieldtype(typeof(cred), name), value))
end

function Base.shred!(cred::UserPasswordCredential)
    cred.user = ""
    Base.shred!(cred.pass)
    return cred
end

function Base.:(==)(a::UserPasswordCredential, b::UserPasswordCredential)
    a.user == b.user && a.pass == b.pass
end

function isfilled(cred::UserPasswordCredential)
    !isempty(cred.user) && !isempty(cred.pass)
end

"SSH credential type"
mutable struct SSHCredential <: AbstractCredential
    user::String
    pass::Base.SecretBuffer
    # Paths to private keys
    prvkey::String
    pubkey::String
    function SSHCredential(user="", pass="",
                           prvkey="", pubkey="")
        new(user, pass, prvkey, pubkey)
    end
end

function Base.setproperty!(cred::SSHCredential, name::Symbol, value)
    if name === :pass
        field = getfield(cred, name)
        Base.shred!(field)
    end
    setfield!(cred, name, convert(fieldtype(typeof(cred), name), value))
end


function Base.shred!(cred::SSHCredential)
    cred.user = ""
    Base.shred!(cred.pass)
    cred.prvkey = ""
    cred.pubkey = ""
    return cred
end

function Base.:(==)(a::SSHCredential, b::SSHCredential)
    a.user == b.user && a.pass == b.pass && a.prvkey == b.prvkey && a.pubkey == b.pubkey
end

function isfilled(cred::SSHCredential)
    !isempty(cred.user) && isfile(cred.prvkey) && isfile(cred.pubkey) &&
    (!isempty(cred.pass) || !is_passphrase_required(cred.prvkey))
end

"Caches credential information for re-use"
struct CachedCredentials
    cred::Dict{String,AbstractCredential}
    CachedCredentials() = new(Dict{String,AbstractCredential}())
end

Base.haskey(cache::CachedCredentials, cred_id) = Base.haskey(cache.cred, cred_id)
Base.getindex(cache::CachedCredentials, cred_id) = Base.getindex(cache.cred, cred_id)
Base.get!(cache::CachedCredentials, cred_id, default) = Base.get!(cache.cred, cred_id, default)

function Base.shred!(p::CachedCredentials)
    foreach(Base.shred!, values(p.cred))
    return p
end

function approve(cache::CachedCredentials, cred::AbstractCredential, url::AbstractString)
    cred_id = credential_identifier(url)
    if haskey(cache.cred, cred_id)
        # Shred the cached credential we'll be overwriting if it isn't identical
        cred !== cache.cred[cred_id] && Base.shred!(cache.cred[cred_id])
    end
    cache.cred[cred_id] = cred
    nothing
end

function reject(cache::CachedCredentials, cred::AbstractCredential, url::AbstractString)
    cred_id = credential_identifier(url)
    if haskey(cache.cred, cred_id)
        # Shred the cached credential if it isn't the `cred` passed in
        cred !== cache.cred[cred_id] && Base.shred!(cache.cred[cred_id])
        delete!(cache.cred, cred_id)
    end
    nothing
end

"""
    LibGit2.CredentialPayload

Retains the state between multiple calls to the credential callback for the same URL.
A `CredentialPayload` instance is expected to be `reset!` whenever it will be used with a
different URL.
"""
mutable struct CredentialPayload
    explicit::Union{AbstractCredential, Nothing}
    cache::Union{CachedCredentials, Nothing}
    allow_ssh_agent::Bool    # Allow the use of the SSH agent to get credentials
    allow_git_helpers::Bool  # Allow the use of git credential helpers
    allow_prompt::Bool       # Allow prompting the user for credentials

    config::GitConfig

    # Ephemeral state fields
    credential::Union{AbstractCredential, Nothing}
    first_pass::Bool
    use_ssh_agent::Bool
    use_env::Bool
    use_git_helpers::Bool
    remaining_prompts::Int

    url::String
    scheme::String
    username::String
    host::String

    function CredentialPayload(
            credential::Union{AbstractCredential, Nothing}=nothing,
            cache::Union{CachedCredentials, Nothing}=nothing,
            config::GitConfig=GitConfig();
            allow_ssh_agent::Bool=true,
            allow_git_helpers::Bool=true,
            allow_prompt::Bool=true)

        payload = new(credential, cache, allow_ssh_agent, allow_git_helpers, allow_prompt, config)
        return reset!(payload)
    end
end

function CredentialPayload(credential::AbstractCredential; kwargs...)
    CredentialPayload(credential, nothing; kwargs...)
end

function CredentialPayload(cache::CachedCredentials; kwargs...)
    CredentialPayload(nothing, cache; kwargs...)
end

CredentialPayload(p::CredentialPayload) = p

function Base.shred!(p::CredentialPayload)
    # Note: Avoid shredding the `explicit` or `cache` fields as these are just references
    # and it is not our responsibility to shred them.
    p.credential !== nothing && Base.shred!(p.credential)
    p.credential = nothing
end

"""
    reset!(payload, [config]) -> CredentialPayload

Reset the `payload` state back to the initial values so that it can be used again within
the credential callback. If a `config` is provided the configuration will also be updated.
"""
function reset!(p::CredentialPayload, config::GitConfig=p.config)
    p.config = config
    p.credential = nothing
    p.first_pass = true
    p.use_ssh_agent = p.allow_ssh_agent
    p.use_env = true
    p.use_git_helpers = p.allow_git_helpers
    p.remaining_prompts = p.allow_prompt ? 3 : 0
    p.url = ""
    p.scheme = ""
    p.username = ""
    p.host = ""

    return p
end

"""
    approve(payload::CredentialPayload; shred::Bool=true) -> Nothing

Store the `payload` credential for re-use in a future authentication. Should only be called
when authentication was successful.

The `shred` keyword controls whether sensitive information in the payload credential field
should be destroyed. Should only be set to `false` during testing.
"""
function approve(p::CredentialPayload; shred::Bool=true)
    cred = p.credential
    cred === nothing && return  # No credential was used

    # Each `approve` call needs to avoid shredding the passed in credential as we need
    # the credential information intact for subsequent approve calls.
    if p.cache !== nothing
        approve(p.cache, cred, p.url)
        shred = false  # Avoid wiping `cred` as this would also wipe the cached copy
    end
    if p.allow_git_helpers
        approve(p.config, cred, p.url)
    end

    if shred
        Base.shred!(cred)
        p.credential = nothing
    end
    nothing
end

"""
    reject(payload::CredentialPayload; shred::Bool=true) -> Nothing

Discard the `payload` credential from begin re-used in future authentication. Should only be
called when authentication was unsuccessful.

The `shred` keyword controls whether sensitive information in the payload credential field
should be destroyed. Should only be set to `false` during testing.
"""
function reject(p::CredentialPayload; shred::Bool=true)
    cred = p.credential
    cred === nothing && return  # No credential was used

    # Note: each `reject` call needs to avoid shredding the passed in credential as we need
    # the credential information intact for subsequent reject calls.
    if p.cache !== nothing
        reject(p.cache, cred, p.url)
    end
    if p.allow_git_helpers
        reject(p.config, cred, p.url)
    end

    if shred
        Base.shred!(cred)
        p.credential = nothing
    end
    nothing
end

# Useful for functions which can handle various kinds of credentials
const Creds = Union{CredentialPayload, AbstractCredential, CachedCredentials, Nothing}

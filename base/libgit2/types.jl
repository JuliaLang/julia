# This file is a part of Julia. License is MIT: http://julialang.org/license

const Cstring_NULL = convert(Cstring, Ptr{UInt8}(C_NULL))

const OID_RAWSZ = 20
const OID_HEXSZ = OID_RAWSZ * 2
const OID_MINPREFIXLEN = 4

immutable Oid
    val::NTuple{OID_RAWSZ, UInt8}
    Oid(val::NTuple{OID_RAWSZ, UInt8}) = new(val)
end
Oid() = Oid(ntuple(i->zero(UInt8), OID_RAWSZ))

immutable TimeStruct
    time::Int64     # time in seconds from epoch
    offset::Cint    # timezone offset in minutes
end
TimeStruct() = TimeStruct(zero(Int64), zero(Cint))

immutable SignatureStruct
    name::Ptr{UInt8}  # full name of the author
    email::Ptr{UInt8} # email of the author
    when::TimeStruct  # time when the action happened
end
SignatureStruct() = SignatureStruct(Ptr{UInt8}(0),
                                    Ptr{UInt8}(0),
                                    TimeStruct())

immutable StrArrayStruct
   strings::Ptr{Cstring}
   count::Csize_t
end
StrArrayStruct() = StrArrayStruct(Ptr{Cstring}(C_NULL), zero(Csize_t))
function Base.finalize(sa::StrArrayStruct)
    sa_ptr = Ref(sa)
    ccall((:git_strarray_free, :libgit2), Void, (Ptr{StrArrayStruct},), sa_ptr)
    return sa_ptr[]
end

immutable Buffer
    ptr::Ptr{Cchar}
    asize::Csize_t
    size::Csize_t
end
Buffer() = Buffer(Ptr{Cchar}(C_NULL), zero(Csize_t), zero(Csize_t))
function Base.finalize(buf::Buffer)
    buf_ptr = Ref(buf)
    ccall((:git_buf_free, :libgit2), Void, (Ptr{Buffer},), buf_ptr)
    return buf_ptr[]
end

immutable CheckoutOptions
    version::Cuint

    checkout_strategy::Cuint

    disable_filters::Cint
    dir_mode::Cuint
    file_mode::Cuint
    file_open_flags::Cint

    notify_flags::Cuint
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
CheckoutOptions(; checkout_strategy::Cuint = GitConst.CHECKOUT_SAFE,
                  disable_filters::Cint = zero(Cint),
                  dir_mode::Cuint = Cuint(0), # Cuint(0o755),
                  file_mode::Cuint = Cuint(0), #Cuint(0o644),
                  file_open_flags::Cint = zero(Cint),
                  notify_flags::Cuint = GitConst.CHECKOUT_NOTIFY_NONE,
                  notify_cb::Ptr{Void} = Ptr{Void}(0),
                  notify_payload::Ptr{Void} = Ptr{Void}(0),
                  progress_cb::Ptr{Void} = Ptr{Void}(0),
                  progress_payload::Ptr{Void} = Ptr{Void}(0),
                  paths::StrArrayStruct = StrArrayStruct(),
                  baseline::Ptr{Void} = Ptr{Void}(0),
                  baseline_index::Ptr{Void} = Ptr{Void}(0),
                  target_directory::Cstring = Cstring_NULL,
                  ancestor_label::Cstring = Cstring_NULL,
                  our_label::Cstring = Cstring_NULL,
                  their_label::Cstring = Cstring_NULL,
                  perfdata_cb::Ptr{Void} = Ptr{Void}(0),
                  perfdata_payload::Ptr{Void} = Ptr{Void}(0)
)=CheckoutOptions(one(Cuint),
                  checkout_strategy,
                  disable_filters,
                  dir_mode,
                  file_mode,
                  file_open_flags,
                  notify_flags,
                  notify_cb,
                  notify_payload,
                  progress_cb,
                  progress_payload,
                  paths,
                  baseline,
                  baseline_index,
                  target_directory,
                  ancestor_label,
                  our_label,
                  their_label,
                  perfdata_cb,
                  perfdata_payload)

immutable RemoteCallbacks
    version::Cuint
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
RemoteCallbacks() = RemoteCallbacks(one(Cuint),
                                    Ptr{Void}(0),
                                    Ptr{Void}(0),
                                    Ptr{Void}(0),
                                    Ptr{Void}(0),
                                    Ptr{Void}(0),
                                    Ptr{Void}(0),
                                    Ptr{Void}(0),
                                    Ptr{Void}(0),
                                    Ptr{Void}(0),
                                    Ptr{Void}(0),
                                    Ptr{Void}(0),
                                    Ptr{Void}(0))

immutable FetchOptions
    version::Cuint
    callbacks::RemoteCallbacks
    prune::Cint
    update_fetchhead::Cint
    download_tags::Cint
end
FetchOptions(; callbacks::RemoteCallbacks = RemoteCallbacks(),
               prune::Cint = GitConst.FETCH_PRUNE_UNSPECIFIED,
               update_fetchhead::Cint = one(Cint),
               download_tags::Cint = GitConst.REMOTE_DOWNLOAD_TAGS_AUTO
) = FetchOptions(one(Cuint),
                 callbacks,
                 prune,
                 update_fetchhead,
                 download_tags)

immutable CloneOptions
    version::Cuint
    checkout_opts::CheckoutOptions
    fetch_opts::FetchOptions
    bare::Cint
    localclone::Cint
    checkout_branch::Cstring
    repository_cb::Ptr{Void}
    repository_cb_payload::Ptr{Void}
    remote_cb::Ptr{Void}
    remote_cb_payload::Ptr{Void}
end
CloneOptions(; checkout_opts::CheckoutOptions = CheckoutOptions(),
               fetch_opts::FetchOptions = FetchOptions(),
               bare::Cint = zero(Cint),
               localclone::Cint = GitConst.CLONE_LOCAL_AUTO,
               checkout_branch::Cstring = Cstring_NULL,
               repository_cb::Ptr{Void} = Ptr{Void}(0),
               repository_cb_payload::Ptr{Void} = Ptr{Void}(0),
               remote_cb::Ptr{Void} = Ptr{Void}(0),
               remote_cb_payload::Ptr{Void} = Ptr{Void}(0)
)=CloneOptions(one(Cuint),
               checkout_opts,
               fetch_opts,
               bare,
               localclone,
               checkout_branch,
               repository_cb,
               repository_cb_payload,
               remote_cb,
               remote_cb_payload)

# git diff option struct
immutable DiffOptionsStruct
    version::Cuint
    flags::UInt32

    # options controlling which files are in the diff
    ignore_submodules::Cint
    pathspec::StrArrayStruct
    notify_cb::Ptr{Void}
    notify_payload::Ptr{Void}

    # options controlling how the diff text is generated
    context_lines::UInt32
    interhunk_lines::UInt32
    id_abbrev::UInt16
    max_size::Coff_t
    old_prefix::Cstring
    new_prefix::Cstring
end
DiffOptionsStruct(; flags::UInt32 = GitConst.DIFF_NORMAL,
                    ignore_submodules::Cint = Cint(GitConst.SUBMODULE_IGNORE_UNSPECIFIED),
                    pathspec::StrArrayStruct = StrArrayStruct(),
                    notify_cb::Ptr{Void} = C_NULL,
                    notify_payload::Ptr{Void} = C_NULL,
                    context_lines::UInt32 = UInt32(3),
                    interhunk_lines::UInt32 = zero(UInt32),
                    id_abbrev::UInt16 = UInt16(7),
                    max_size::Coff_t = Coff_t(512*1024*1024), #zero(Coff_t), #512Mb
                    old_prefix::Cstring = Cstring_NULL,
                    new_prefix::Cstring = Cstring_NULL
)=DiffOptionsStruct(GitConst.DIFF_OPTIONS_VERSION,
                    flags,
                    ignore_submodules,
                    pathspec,
                    notify_cb,
                    notify_payload,
                    context_lines,
                    interhunk_lines,
                    id_abbrev,
                    max_size,
                    old_prefix,
                    new_prefix
                )

immutable DiffFile
    id::Oid
    path::Cstring
    size::Coff_t
    flags::Cuint
    mode::UInt16
end
DiffFile()=DiffFile(Oid(),Cstring_NULL,Coff_t(0),Cuint(0),UInt16(0))

immutable DiffDelta
    status::Cint
    flags::Cuint
    similarity::UInt16
    nfiles::UInt16
    old_file::DiffFile
    new_file::DiffFile
end
DiffDelta()=DiffDelta(Cint(0),Cuint(0),UInt16(0),UInt16(0),DiffFile(),DiffFile())

immutable MergeOptionsStruct
    version::Cuint
    flags::Cint
    rename_threshold::Cuint
    target_limit::Cuint
    metric::Ptr{Void}
    file_favor::Cint
end
MergeOptionsStruct(; flags::Cint = Cint(0),
                     rename_threshold::Cuint = Cuint(50),
                     target_limit::Cuint = Cuint(200),
                     metric::Ptr{Void} = Ptr{Void}(0),
                     file_favor::Cint = GitConst.MERGE_FILE_FAVOR_NORMAL
)=MergeOptionsStruct(one(Cuint),
                     flags,
                     rename_threshold,
                     target_limit,
                     metric,
                     file_favor
                    )

immutable PushOptionsStruct
    version::Cuint
    parallelism::Cint
end
PushOptionsStruct() = PushOptionsStruct(one(Cuint),one(Cuint))

immutable IndexTime
    seconds::Int64
    nanoseconds::Cuint
    IndexTime() = new(zero(Int64), zero(Cuint))
end

immutable IndexEntry
    ctime::IndexTime
    mtime::IndexTime

    dev::Cuint
    ino::Cuint
    mode::Cuint
    uid::Cuint
    gid::Cuint
    file_size::Coff_t

    id::Oid

    flags::UInt16
    flags_extended::UInt16

    path::Ptr{UInt8}
end
IndexEntry() = IndexEntry(IndexTime(),
                          IndexTime(),
                          Cuint(0),
                          Cuint(0),
                          Cuint(0),
                          Cuint(0),
                          Cuint(0),
                          Coff_t(0),
                          Oid(),
                          UInt16(0),
                          UInt16(0),
                          Ptr{UInt8}(0))
Base.show(io::IO, ie::IndexEntry) = print(io, "IndexEntry($(string(ie.id)))")

immutable RebaseOptions
    version::Cuint
    quiet::Cint
    rewrite_notes_ref::Cstring
    checkout_opts::CheckoutOptions
end
RebaseOptions(; quiet::Cint = Cint(1),
                rewrite_notes_ref::Cstring = Cstring_NULL,
                checkout_opts::CheckoutOptions = CheckoutOptions()
)=RebaseOptions(one(Cuint), quiet, rewrite_notes_ref, checkout_opts)

immutable RebaseOperation
    optype::Cint
    id::Oid
    exec::Cstring
end
RebaseOperation()=RebaseOperation(Cint(0), Oid(), Cstring_NULL)
Base.show(io::IO, rbo::RebaseOperation) = print(io, "RebaseOperation($(string(rbo.id)))")

immutable StatusOptions
    version::Cuint
    show::Cint
    flags::Cuint
    pathspec::StrArrayStruct
end
StatusOptions(; show::Cint = GitConst.STATUS_SHOW_INDEX_AND_WORKDIR,
                flags::Cuint = GitConst.STATUS_OPT_INCLUDE_UNTRACKED |
                               GitConst.STATUS_OPT_RECURSE_UNTRACKED_DIRS |
                               GitConst.STATUS_OPT_RENAMES_HEAD_TO_INDEX |
                               GitConst.STATUS_OPT_SORT_CASE_SENSITIVELY,
                pathspec::StrArrayStruct = StrArrayStruct()
)=StatusOptions(one(Cuint),
                show,
                flags,
                pathspec)

immutable StatusEntry
    status::Cuint
    head_to_index::Ptr{DiffDelta}
    index_to_workdir::Ptr{DiffDelta}
end
StatusEntry()=StatusEntry(Cuint(0), C_NULL, C_NULL)

# Abstract object types
abstract AbstractGitObject
Base.isempty(obj::AbstractGitObject) = (obj.ptr == C_NULL)

abstract GitObject <: AbstractGitObject
function Base.finalize(obj::GitObject)
    if obj.ptr != C_NULL
        ccall((:git_object_free, :libgit2), Void, (Ptr{Void},), obj.ptr)
        obj.ptr = C_NULL
    end
end

# Common types
for (typ, ref, sup, fnc) in (
            (:GitRemote,     :Void, :AbstractGitObject, :(:git_remote_free)),
            (:GitRevWalker,  :Void, :AbstractGitObject, :(:git_revwalk_free)),
            (:GitConfig,     :Void, :AbstractGitObject, :(:git_config_free)),
            (:GitReference,  :Void, :AbstractGitObject, :(:git_reference_free)),
            (:GitDiff,       :Void, :AbstractGitObject, :(:git_diff_free)),
            (:GitIndex,      :Void, :AbstractGitObject, :(:git_index_free)),
            (:GitRepo,       :Void, :AbstractGitObject, :(:git_repository_free)),
            (:GitAnnotated,  :Void, :AbstractGitObject, :(:git_annotated_commit_free)),
            (:GitRebase,     :Void, :AbstractGitObject, :(:git_rebase_free)),
            (:GitStatus,     :Void, :AbstractGitObject, :(:git_status_list_free)),
            (:GitSignature,  :SignatureStruct, :AbstractGitObject, :(:git_signature_free)),
            (:GitAnyObject,  :Void, :GitObject, nothing),
            (:GitCommit,     :Void, :GitObject, nothing),
            (:GitBlob,       :Void, :GitObject, nothing),
            (:GitTree,       :Void, :GitObject, nothing),
            (:GitTag,        :Void, :GitObject, nothing)
        )

    @eval type $typ <: $sup
        ptr::Ptr{$ref}
        function $typ(ptr::Ptr{$ref})
            @assert ptr != C_NULL
            obj = new(ptr)
            return obj
        end
    end

    if fnc != nothing
        @eval function Base.finalize(obj::$typ)
            if obj.ptr != C_NULL
                ccall(($fnc, :libgit2), Void, (Ptr{$ref},), obj.ptr)
                obj.ptr = C_NULL
            end
        end
    end

end

# Structure has the same layout as SignatureStruct
type Signature
    name::AbstractString
    email::AbstractString
    time::Int64
    time_offset::Int32
end

""" Resource management helper function
"""
function with(f::Function, obj)
    try
        f(obj)
    finally
        finalize(obj)
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


function getobjecttype{T<:GitObject}(::Type{T})
    return if T == GitCommit
        GitConst.OBJ_COMMIT
    elseif T == GitTree
        GitConst.OBJ_TREE
    elseif T == GitBlob
        GitConst.OBJ_BLOB
    elseif T == GitTag
        GitConst.OBJ_TAG
    elseif T == GitAnyObject
        GitConst.OBJ_ANY
    else
        throw(GitError(Error.Object, Error.ENOTFOUND, "Type $T is not supported"))
    end
end

function getobjecttype(obj_type::Cint)
    return if obj_type == GitConst.OBJ_COMMIT
        GitCommit
    elseif obj_type == GitConst.OBJ_TREE
        GitTree
    elseif obj_type == GitConst.OBJ_BLOB
        GitBlob
    elseif obj_type == GitConst.OBJ_TAG
        GitTag
    elseif obj_type == GitConst.OBJ_ANY
        GitAnyObject
    else
        throw(GitError(Error.Object, Error.ENOTFOUND, "Object type $obj_type is not supported"))
    end
end

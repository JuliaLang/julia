# This file is a part of Julia. License is MIT: http://julialang.org/license

"""
    @kwdef typedef

This is a helper macro that automatically defines a keyword-based constructor for the type declared in the expression `typedef`, which must be a `type` or `immutable` expression. The default argument is supplied by declaring fields of the form `field::Type = default`. If no default is provided then the default is:
 - null pointer for pointer types (`Ptr{T}`, `Cstring`, `Cwstring`)
 - zero for integer types
 - no-argument constructor calls (e.g. `T()`) for all other types
"""
macro kwdef(expr)
    typename = expr.args[2]
    defparams = Expr(:parameters)
    defcall = Expr(:call, typename)
    typeblk = expr.args[3]
    for i in eachindex(typeblk.args)
        ei = typeblk.args[i]
        if ei.head == :(=)
            dec = ei.args[1]
            def = ei.args[2]
            push!(defparams.args, Expr(:kw, dec, def))
            push!(defcall.args, dec.args[1])
            typeblk.args[i] = dec
        elseif ei.head == :(::)
            # no default value provided
            dec = ei
            T = ei.args[2]
            def = :($T <: Union{Ptr, Cstring, Cwstring} ? $T(C_NULL) : $T <: Integer ? zero($T) : $T())
            push!(defparams.args, Expr(:kw, dec, def))
            push!(defcall.args, dec.args[1])
        end
    end
    quote
        $expr
        $(esc(Expr(:call, typename, defparams))) = $(esc(defcall))
    end
end

import .Consts: GIT_SUBMODULE_IGNORE, GIT_MERGE_FILE_FAVOR, GIT_MERGE_FILE

const OID_RAWSZ = 20
const OID_HEXSZ = OID_RAWSZ * 2
const OID_MINPREFIXLEN = 4

immutable Oid
    val::NTuple{OID_RAWSZ, UInt8}
    Oid(val::NTuple{OID_RAWSZ, UInt8}) = new(val)
end
Oid() = Oid(ntuple(i->zero(UInt8), OID_RAWSZ))

@kwdef immutable TimeStruct
    time::Int64     # time in seconds from epoch
    offset::Cint    # timezone offset in minutes
end

@kwdef immutable SignatureStruct
    name::Ptr{UInt8}  # full name of the author
    email::Ptr{UInt8} # email of the author
    when::TimeStruct  # time when the action happened
end

@kwdef immutable StrArrayStruct
   strings::Ptr{Cstring}
   count::Csize_t
end
function Base.finalize(sa::StrArrayStruct)
    sa_ptr = Ref(sa)
    ccall((:git_strarray_free, :libgit2), Void, (Ptr{StrArrayStruct},), sa_ptr)
    return sa_ptr[]
end

@kwdef immutable Buffer
    ptr::Ptr{Cchar}
    asize::Csize_t
    size::Csize_t
end
function Base.finalize(buf::Buffer)
    buf_ptr = Ref(buf)
    ccall((:git_buf_free, :libgit2), Void, (Ptr{Buffer},), buf_ptr)
    return buf_ptr[]
end

"Abstract credentials payload"
abstract AbstractCredentials

"Checks if credentials were used"
checkused!(p::AbstractCredentials) = true
checkused!(p::Void) = false
"Resets credentials for another use"
reset!(p::AbstractCredentials, cnt::Int=3) = nothing

@kwdef immutable CheckoutOptions
    version::Cuint = one(Cuint)

    checkout_strategy::Cuint = Consts.CHECKOUT_SAFE

    disable_filters::Cint
    dir_mode::Cuint
    file_mode::Cuint
    file_open_flags::Cint

    notify_flags::Cuint = Consts.CHECKOUT_NOTIFY_NONE
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

@kwdef immutable RemoteCallbacks
    version::Cuint                   = one(Cuint)
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

if LibGit2.version() >= v"0.24.0"
    @kwdef immutable FetchOptions
        version::Cuint             = one(Cuint)
        callbacks::RemoteCallbacks
        prune::Cint                = Consts.FETCH_PRUNE_UNSPECIFIED
        update_fetchhead::Cint     = one(Cint)
        download_tags::Cint        = Consts.REMOTE_DOWNLOAD_TAGS_AUTO
        custom_headers::StrArrayStruct
    end
else
    @kwdef immutable FetchOptions
        version::Cuint             = one(Cuint)
        callbacks::RemoteCallbacks
        prune::Cint                = Consts.FETCH_PRUNE_UNSPECIFIED
        update_fetchhead::Cint     = one(Cint)
        download_tags::Cint        = Consts.REMOTE_DOWNLOAD_TAGS_AUTO
    end
end

@kwdef immutable CloneOptions
    version::Cuint                   = one(Cuint)
    checkout_opts::CheckoutOptions
    fetch_opts::FetchOptions
    bare::Cint
    localclone::Cint                 = Consts.CLONE_LOCAL_AUTO
    checkout_branch::Cstring
    repository_cb::Ptr{Void}
    repository_cb_payload::Ptr{Void}
    remote_cb::Ptr{Void}
    remote_cb_payload::Ptr{Void}
end

# git diff option struct
if LibGit2.version() >= v"0.24.0"
    @kwdef immutable DiffOptionsStruct
        version::Cuint                           = Consts.DIFF_OPTIONS_VERSION
        flags::UInt32                            = Consts.DIFF_NORMAL

        # options controlling which files are in the diff
        ignore_submodules::GIT_SUBMODULE_IGNORE  = Consts.SUBMODULE_IGNORE_UNSPECIFIED
        pathspec::StrArrayStruct
        notify_cb::Ptr{Void}
        progress_cb::Ptr{Void}
        payload::Ptr{Void}

        # options controlling how the diff text is generated
        context_lines::UInt32                    = UInt32(3)
        interhunk_lines::UInt32
        id_abbrev::UInt16                        = UInt16(7)
        max_size::Int64                          = Int64(512*1024*1024) #512Mb
        old_prefix::Cstring
        new_prefix::Cstring
    end
else
    @kwdef immutable DiffOptionsStruct
        version::Cuint                           = Consts.DIFF_OPTIONS_VERSION
        flags::UInt32                            = Consts.DIFF_NORMAL

        # options controlling which files are in the diff
        ignore_submodules::GIT_SUBMODULE_IGNORE  = Consts.SUBMODULE_IGNORE_UNSPECIFIED
        pathspec::StrArrayStruct
        notify_cb::Ptr{Void}
        payload::Ptr{Void}

        # options controlling how the diff text is generated
        context_lines::UInt32                    = UInt32(3)
        interhunk_lines::UInt32
        id_abbrev::UInt16                        = UInt16(7)
        max_size::Int64                          = Int64(512*1024*1024)
        old_prefix::Cstring
        new_prefix::Cstring
    end
end

@kwdef immutable DiffFile
    id::Oid
    path::Cstring
    size::Int64
    flags::UInt32
    mode::UInt16
end

@kwdef immutable DiffDelta
    status::Cint
    flags::UInt32
    similarity::UInt16
    nfiles::UInt16
    old_file::DiffFile
    new_file::DiffFile
end

# TODO: double check this when libgit2 v0.25.0 is released
if LibGit2.version() >= v"0.25.0"
    @kwdef immutable MergeOptions
        version::Cuint                    = one(Cuint)
        flags::Cint
        rename_threshold::Cuint           = Cuint(50)
        target_limit::Cuint               = Cuint(200)
        metric::Ptr{Void}
        recursion_limit::Cuint
        default_driver::Cstring
        file_favor::GIT_MERGE_FILE_FAVOR  = Consts.MERGE_FILE_FAVOR_NORMAL
        file_flags::GIT_MERGE_FILE        = Consts.MERGE_FILE_DEFAULT
    end
elseif LibGit2.version() >= v"0.24.0"
    @kwdef immutable MergeOptions
        version::Cuint                    = one(Cuint)
        flags::Cint
        rename_threshold::Cuint           = Cuint(50)
        target_limit::Cuint               = Cuint(200)
        metric::Ptr{Void}
        recursion_limit::Cuint
        file_favor::GIT_MERGE_FILE_FAVOR  = Consts.MERGE_FILE_FAVOR_NORMAL
        file_flags::GIT_MERGE_FILE        = Consts.MERGE_FILE_DEFAULT
    end
else
    @kwdef immutable MergeOptions
        version::Cuint                    = one(Cuint)
        flags::Cint
        rename_threshold::Cuint           = Cuint(50)
        target_limit::Cuint               = Cuint(200)
        metric::Ptr{Void}
        file_favor::GIT_MERGE_FILE_FAVOR  = Consts.MERGE_FILE_FAVOR_NORMAL
        file_flags::GIT_MERGE_FILE        = Consts.MERGE_FILE_DEFAULT
    end
end

if LibGit2.version() >= v"0.24.0"
    @kwdef immutable PushOptions
        version::Cuint                     = one(Cuint)
        parallelism::Cint                  = one(Cint)
        callbacks::RemoteCallbacks
        custom_headers::StrArrayStruct
    end
else
    @kwdef immutable PushOptions
        version::Cuint                     = one(Cuint)
        parallelism::Cint                  = one(Cint)
        callbacks::RemoteCallbacks
    end
end

@kwdef immutable IndexTime
    seconds::Int64
    nanoseconds::Cuint
end

@kwdef immutable IndexEntry
    ctime::IndexTime
    mtime::IndexTime

    dev::UInt32
    ino::UInt32
    mode::UInt32
    uid::UInt32
    gid::UInt32
    file_size::Int64

    id::Oid

    flags::UInt16
    flags_extended::UInt16

    path::Ptr{UInt8}
end
Base.show(io::IO, ie::IndexEntry) = print(io, "IndexEntry($(string(ie.id)))")


if LibGit2.version() >= v"0.24.0"
    @kwdef immutable RebaseOptions
        version::Cuint                 = one(Cuint)
        quiet::Cint                    = Cint(1)
        inmemory::Cint
        rewrite_notes_ref::Cstring
        merge_opts::MergeOptions
        checkout_opts::CheckoutOptions
    end
else
    @kwdef immutable RebaseOptions
        version::Cuint                 = one(Cuint)
        quiet::Cint                    = Cint(1)
        rewrite_notes_ref::Cstring
        checkout_opts::CheckoutOptions
    end
end

@kwdef immutable RebaseOperation
    optype::Cint
    id::Oid
    exec::Cstring
end
Base.show(io::IO, rbo::RebaseOperation) = print(io, "RebaseOperation($(string(rbo.id)))")

@kwdef immutable StatusOptions
    version::Cuint           = one(Cuint)
    show::Cint               = Consts.STATUS_SHOW_INDEX_AND_WORKDIR
    flags::Cuint             = Consts.STATUS_OPT_INCLUDE_UNTRACKED |
                               Consts.STATUS_OPT_RECURSE_UNTRACKED_DIRS |
                               Consts.STATUS_OPT_RENAMES_HEAD_TO_INDEX |
                               Consts.STATUS_OPT_SORT_CASE_SENSITIVELY
    pathspec::StrArrayStruct
end

@kwdef immutable StatusEntry
    status::Cuint
    head_to_index::Ptr{DiffDelta}
    index_to_workdir::Ptr{DiffDelta}
end

immutable FetchHead
    name::String
    url::String
    oid::Oid
    ismerge::Bool
end

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
            (:GitBranchIter, :Void, :AbstractGitObject, :(:git_branch_iterator_free)),
            (:GitTreeEntry,  :Void, :AbstractGitObject, :(:git_tree_entry_free)),
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

    if fnc !== nothing
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
        Consts.OBJ_COMMIT
    elseif T == GitTree
        Consts.OBJ_TREE
    elseif T == GitBlob
        Consts.OBJ_BLOB
    elseif T == GitTag
        Consts.OBJ_TAG
    elseif T == GitAnyObject
        Consts.OBJ_ANY
    else
        throw(GitError(Error.Object, Error.ENOTFOUND, "Type $T is not supported"))
    end
end

function getobjecttype(obj_type::Cint)
    return if obj_type == Consts.OBJ_COMMIT
        GitCommit
    elseif obj_type == Consts.OBJ_TREE
        GitTree
    elseif obj_type == Consts.OBJ_BLOB
        GitBlob
    elseif obj_type == Consts.OBJ_TAG
        GitTag
    elseif obj_type == Consts.OBJ_ANY
        GitAnyObject
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

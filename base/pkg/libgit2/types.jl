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
   strings::Ptr{Ptr{UInt8}}
   count::Csize_t
end
StrArrayStruct() = StrArrayStruct(Ptr{UInt8}(0), zero(Csize_t))

immutable CheckoutOptionsStruct
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
    target_directory::Ptr{UInt8}
    ancestor_label::Ptr{UInt8}
    our_label::Ptr{UInt8}
    their_label::Ptr{UInt8}
end
CheckoutOptionsStruct() = CheckoutOptionsStruct(one(Cuint),
                                                GitConst.CHECKOUT_SAFE_CREATE,
                                                zero(Cint),
                                                zero(Cuint), # Cuint(0o755), #
                                                zero(Cuint), # Cuint(0o755), #
                                                zero(Cint),
                                                GitConst.CHECKOUT_NOTIFY_NONE,
                                                Ptr{Void}(0), Ptr{Void}(0),
                                                Ptr{Void}(0), Ptr{Void}(0),
                                                StrArrayStruct(),
                                                Ptr{Void}(0),
                                                Ptr{UInt8}(0),
                                                Ptr{UInt8}(0),
                                                Ptr{UInt8}(0),
                                                Ptr{UInt8}(0))

immutable RemoteCallbacksStruct
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
    payload::Ptr{Void}
end
RemoteCallbacksStruct() = RemoteCallbacksStruct(one(Cuint),
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

type CloneOptionsStruct
    version::Cuint
    checkout_opts::CheckoutOptionsStruct
    remote_callbacks::RemoteCallbacksStruct
    bare::Cint
    localclone::Cint
    checkout_branch::Ptr{UInt8}
    signature::Ptr{Void}
    repository_cb::Ptr{Void}
    repository_cb_payload::Ptr{Void}
    remote_cb::Ptr{Void}
    remote_cb_payload::Ptr{Void}
end

CloneOptionsStruct() = CloneOptionsStruct(one(Cuint),
                                          CheckoutOptionsStruct(),
                                          RemoteCallbacksStruct(),
                                          zero(Cint),
                                          zero(Cint),
                                          Ptr{UInt8}(0),
                                          Ptr{Void}(0),
                                          Ptr{Void}(0), Ptr{Void}(0),
                                          Ptr{Void}(0), Ptr{Void}(0)
                                        )

type GitRepo
    ptr::Ptr{Void}

    function GitRepo(ptr::Ptr{Void}, own::Bool=true)
        @assert ptr != C_NULL
        r = new(ptr)
        own && finalizer(r, free!)
        return r
    end
end

function free!(r::GitRepo)
    if r.ptr != C_NULL
        ccall((:git_repository_free, :libgit2), Void, (Ptr{Void},), r.ptr)
        r.ptr = C_NULL
    end
end

abstract GitObject

function free!(o::GitObject)
    if o.ptr != C_NULL
        ccall((:git_object_free, :libgit2), Void, (Ptr{Void},), o.ptr)
        o.ptr = C_NULL
    end
end

type GitCommit <: GitObject
    ptr::Ptr{Void}

    function GitCommit(ptr::Ptr{Void})
        @assert ptr != C_NULL
        this = new(ptr)
        finalizer(this, free!)
        return this
    end
end

type GitTree <: GitObject
    ptr::Ptr{Void}

    function GitTree(ptr::Ptr{Void})
        @assert ptr != C_NULL
        this = new(ptr)
        finalizer(this, free!)
        return this
    end
end

type GitReference
    ptr::Ptr{Void}

    function GitReference(ptr::Ptr{Void})
        r = new(ptr)
        finalizer(r, free!)
        return r
    end
end

function free!(r::GitReference)
    if r.ptr != C_NULL
        ccall((:git_reference_free, :libgit2), Void, (Ptr{Void},), r.ptr)
        r.ptr = C_NULL
    end
end

type GitConfig
    ptr::Ptr{Void}

    function GitConfig(ptr::Ptr{Void})
        @assert ptr != C_NULL
        cfg = new(ptr)
        finalizer(cfg, free!)
        return cfg
    end
end

function free!(cfg::GitConfig)
    if cfg.ptr != C_NULL
        ccall((:git_config_free, :libgit2), Void, (Ptr{Void},), cfg.ptr)
        cfg.ptr = C_NULL
    end
end

type GitRevWalker
    ptr::Ptr{Void}

    function GitRevWalker(ptr::Ptr{Void})
        @assert ptr != C_NULL
        w = new(ptr)
        finalizer(w, free!)
        return w
    end
end

function free!(w::GitRevWalker)
    if w.ptr != C_NULL
        ccall((:git_revwalk_free, :libgit2), Void, (Ptr{Void},), w.ptr)
        w.ptr = C_NULL
    end
end

type Signature
    name::UTF8String
    email::UTF8String
    time::Int32
    time_offset::Int32
end

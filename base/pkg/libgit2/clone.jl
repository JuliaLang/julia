immutable StrArrayStruct
   strings::Ptr{Ptr{Uint8}}
   count::Csize_t
end
StrArrayStruct() = StrArrayStruct(Ptr{Uint8}(0), zero(Csize_t))

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
    target_directory::Ptr{Uint8}
    ancestor_label::Ptr{Uint8}
    our_label::Ptr{Uint8}
    their_label::Ptr{Uint8}
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
                                                Ptr{Uint8}(0),
                                                Ptr{Uint8}(0),
                                                Ptr{Uint8}(0),
                                                Ptr{Uint8}(0))

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
    checkout_branch::Ptr{Uint8}
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

function clone(url::AbstractString, path::AbstractString;
               branch::AbstractString="",
               bare::Bool = false,
               remote_cb::Ptr{Void} = C_NULL)
    # start cloning
    clone_opts = CloneOptionsStruct()
    clone_opts.bare = Int32(bare)
    if !isempty(branch)
        clone_opts.checkout_branch = pointer(branch)
    end
    if remote_cb != C_NULL
        clone_opts.remote_cb = remote_cb
    end

    clone_opts_ref = Ref(clone_opts)
    repo_ptr = Ptr{Void}[C_NULL]
    err = ccall((:git_clone, :libgit2), Cint,
            (Ptr{Ptr{Void}}, Ptr{UInt8}, Ptr{UInt8}, Ref{CloneOptionsStruct}),
            repo_ptr, url, path, clone_opts_ref)
    err != 0 && return nothing

    return GitRepo(repo_ptr[1])
end

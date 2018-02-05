# This file is a part of Julia. License is MIT: https://julialang.org/license

import LibGit2: AbstractCredential, UserPasswordCredential, SSHCredential,
    CachedCredentials, CredentialPayload, Payload
using Base: coalesce

const DEFAULT_PAYLOAD = CredentialPayload(allow_ssh_agent=false, allow_git_helpers=false)

"""
Emulates the LibGit2 credential loop to allows testing of the credential_callback function
without having to authenticate against a real server.
"""
function credential_loop(
        valid_credential::AbstractCredential,
        url::AbstractString,
        user::Union{AbstractString, Nothing},
        allowed_types::UInt32,
        payload::CredentialPayload;
        shred::Bool=true)
    cb = LibGit2.credentials_cb()
    libgitcred_ptr_ptr = Ref{Ptr{Cvoid}}(C_NULL)

    # Number of times credentials were authenticated against. With the real LibGit2
    # credential loop this would be how many times we sent credentials to the remote.
    num_authentications = 0

    # Emulate how LibGit2 uses the credential callback by repeatedly calling the function
    # until we find valid credentials or an exception is raised.
    err = Cint(0)
    while err == 0
        err = ccall(cb, Cint, (Ptr{Ptr{Cvoid}}, Cstring, Cstring, Cuint, Any),
                    libgitcred_ptr_ptr, url, coalesce(user, C_NULL),
                    allowed_types, payload)
        num_authentications += 1

        # Check if the callback provided us with valid credentials
        if payload.credential !== nothing && payload.credential == valid_credential
            LibGit2.approve(payload, shred=shred)
            break
        end

        if num_authentications > 50
            error("Credential callback seems to be caught in an infinite loop")
        end
    end

    # Note: LibGit2.GitError(0) will not work if an error message has been set.
    git_error = if err == 0
        LibGit2.GitError(LibGit2.Error.None, LibGit2.Error.GIT_OK, "No errors")
    else
        LibGit2.GitError(err)
    end

    # Reject and shred the credential when an authentication error occurs
    if git_error.code == LibGit2.Error.EAUTH
        LibGit2.reject(payload, shred=shred)
    end

    return git_error, num_authentications, payload
end

function credential_loop(
        valid_credential::UserPasswordCredential,
        url::AbstractString,
        user::Union{AbstractString, Nothing}=nothing,
        payload::CredentialPayload=DEFAULT_PAYLOAD;
        shred::Bool=true)
    credential_loop(valid_credential, url, user, 0x000001, payload, shred=shred)
end

function credential_loop(
        valid_credential::SSHCredential,
        url::AbstractString,
        user::Union{AbstractString, Nothing}=nothing,
        payload::CredentialPayload=DEFAULT_PAYLOAD;
        shred::Bool=true)
    credential_loop(valid_credential, url, user, 0x000046, payload, shred=shred)
end

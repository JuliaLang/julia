# This file is a part of Julia. License is MIT: https://julialang.org/license

import Base.LibGit2: AbstractCredentials, UserPasswordCredentials, SSHCredentials, CachedCredentials

"""
Emulates the LibGit2 credential loop to allows testing of the credential_callback function
without having to authenticate against a real server.
"""
function credential_loop(
        valid_credential::AbstractCredentials,
        url::AbstractString,
        user::AbstractString,
        allowed_types::UInt32,
        cache::CachedCredentials=CachedCredentials())
    cb = Base.LibGit2.credentials_cb()
    libgitcred_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    payload_ptr = Ref(Nullable{AbstractCredentials}(cache))

    # Number of times credentials were authenticated against. With the real LibGit2
    # credential loop this would be how many times we sent credentials to the remote.
    num_authentications = 0

    # Emulate how LibGit2 uses the credential callback by repeatedly calling the function
    # until we find valid credentials or an exception is raised.
    err = Cint(0)
    while err == 0
        err = ccall(cb, Cint, (Ptr{Ptr{Void}}, Cstring, Cstring, Cuint, Ptr{Void}),
            libgitcred_ptr_ptr, url, isempty(user) ? C_NULL : user, allowed_types, pointer_from_objref(payload_ptr))
        num_authentications += 1

        # Check if the callback provided us with valid credentials
        if length(cache.cred) == 1 && first(values(cache.cred)) == valid_credential
            break
        end

        if num_authentications > 50
            error("Credential callback seems to be caught in an infinite loop")
        end
    end

    return err, num_authentications
end

function credential_loop(
        valid_credential::UserPasswordCredentials,
        url::AbstractString,
        user::AbstractString="")
    credential_loop(valid_credential, url, user, 0x000001)
end

function credential_loop(
        valid_credential::SSHCredentials,
        url::AbstractString,
        user::AbstractString="";
        use_ssh_agent::Bool=false)
    cache = CachedCredentials()

    if !use_ssh_agent
        m = match(LibGit2.URL_REGEX, url)
        default_cred = LibGit2.reset!(SSHCredentials(true), -1)
        default_cred.usesshagent = "N"
        LibGit2.get_creds!(cache, "ssh://$(m[:host])", default_cred)
    end

    credential_loop(valid_credential, url, user, 0x000046, cache)
end

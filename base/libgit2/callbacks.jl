# This file is a part of Julia. License is MIT: http://julialang.org/license

"""Mirror callback function

Function sets `+refs/*:refs/*` refspecs and `mirror` flag for remote reference.
"""
function mirror_callback(remote::Ptr{Ptr{Void}}, repo_ptr::Ptr{Void},
                         name::Cstring, url::Cstring, payload::Ptr{Void})
    # Create the remote with a mirroring url
    fetch_spec = "+refs/*:refs/*"
    err = ccall((:git_remote_create_with_fetchspec, :libgit2), Cint,
                (Ptr{Ptr{Void}}, Ptr{Void}, Cstring, Cstring, Cstring),
                remote, repo_ptr, name, url, fetch_spec)
    err != 0 && return Cint(err)

    # And set the configuration option to true for the push command
    config = GitConfig(GitRepo(repo_ptr,false))
    name_str = unsafe_string(name)
    err= try set!(config, "remote.$name_str.mirror", true)
         catch -1
         finally close(config)
         end
    err != 0 && return Cint(err)
    return Cint(0)
end

function require_passphrase(private_key::AbstractString)
    !isfile(private_key) && return false

    # In encrypted private keys, the second line is "Proc-Type: 4,ENCRYPTED"
    return open(private_key) do f
        readline(f)
        readline(f) == "Proc-Type: 4,ENCRYPTED"
     end
end

function authenticate_ssh(libgit2credptr::Ptr{Ptr{Void}}, username_ptr, p::RemotePayload)
    c = Base.get(p.credential)::SSHCredential
    c.passphrase = ""
    state = p.state
    modified = false

    prompt_url = git_url(protocol=p.protocol, username=c.username, host=p.host)

    # first try ssh-agent if credentials support its usage
    if get!(state, :ssh_agent, 'Y') == 'Y'
        err = ccall((:git_cred_ssh_key_from_agent, :libgit2), Cint,
                     (Ptr{Ptr{Void}}, Cstring), libgit2credptr, username_ptr)
        if err == 0
            state[:ssh_agent] = 'U'  # used ssh-agent only one time
            return Cint(0)
        else
            state[:ssh_agent] = 'E'
        end
    end

    assert(!isempty(c.username))

    # All or nothing. Cache should only contain valid credentials
    if get!(state, :cache, 'Y') == 'Y' && !isnull(p.cache) && !isfilled(c)
        cred_id = "$(isempty(p.protocol) ? "ssh" : p.protocol)://$(p.host)"
        cached_cred = get_cred(unsafe_get(p.cache), cred_id, SSHCredential)

        c.private_key = cached_cred.private_key
        c.public_key = cached_cred.public_key
        c.passphrase = cached_cred.passphrase
        modified = true  # Valid once modified
    end

    if get!(state, :env, 'Y') == 'Y' && !isfilled(c)
        c.private_key = Base.get(ENV, "SSH_KEY_PATH", joinpath(homedir(), ".ssh", "id_rsa"))
        c.public_key = Base.get(ENV, "SSH_PUB_KEY_PATH", c.private_key * ".pub")
        c.passphrase = Base.get(ENV, "SSH_KEY_PASS", "")
        modified = true

        state[:env] = 'N'
    end

    # Fallback to default public key
    default_public_key = c.private_key * ".pub"
    if !modified && c.public_key != default_public_key
        c.public_key = default_public_key
        modified = true
    end

    # Since SSH authentication will raise an exception when credentials are incorrect we
    # will prompt the user for confirmation if we need to gather any additional info.
    prompt_for_keys = !modified || !isfile(c.private_key) || !isfile(c.public_key)
    while get!(state, :prompt, 'Y') == 'Y' && (!modified || !isfilled(c))
        if prompt_for_keys
            last_private_key = isfile(c.private_key) ? c.private_key : ""
            c.private_key = prompt(
                "Private key location for '$prompt_url'",
                default=last_private_key)

            # Update the public key to reflect the change to the private key
            if c.private_key != last_private_key
                c.public_key = c.private_key * ".pub"
            end

            # Avoid asking about the public key as typically this will just annoy users.
            if isfile(c.private_key) && !isfile(c.public_key)
                c.public_key = prompt("Public key location for '$prompt_url'")
            end
        else
            # Always prompt for private key on future iterations
            prompt_for_keys = true
        end

        if isempty(c.passphrase) && require_passphrase(c.private_key)
            @static if is_windows()
                res = Base.winprompt(
                    "Your SSH Key requires a password, please enter it now:",
                    "Passphrase required", c.private_key; prompt_username = false)
                isnull(res) && return Cint(Error.EAUTH)
                c.passphrase = unsafe_get(res)[2]
            else
                c.passphrase = prompt("Passphrase for $(c.private_key)", password=true)
            end
        end

        modified = true

        p.prompts_remaining -= 1
        if p.prompts_remaining <= 0
            state[:prompt] = 'N'
            ccall((:giterr_set_str, :libgit2), Void, (Cint, Cstring), Cint(Error.Callback), "Aborting, maximum number of user prompts reached.")
            return Cint(Error.EAUTH)
        end
    end

    # LibGit2 will only complain about the public key being missing if both the private key and
    # public key are missing.
    if !isfile(c.private_key)
        # Emulates the missing public key file error produced by LibGit2
        ccall((:giterr_set_str, :libgit2), Void,
            (Cint, Cstring), Cint(Error.SSH),
            "Failed to authenticate SSH session: Unable to open private key file")
        # ccall((:giterr_set_str, :libgit2), Void,
        #     (Cint, Cstring), Cint(Error.Callback),
        #     "Unable to open private key file")
        return Cint(Error.EAUTH)
    end

    # Note: Failure to authenticate with SSH credentials abend the callback loop.
    # Note: Passing in missing credential files will abend the callback loop.
    return ccall((:git_cred_ssh_key_new, :libgit2), Cint,
                 (Ptr{Ptr{Void}}, Cstring, Cstring, Cstring, Cstring),
                 libgit2credptr, c.username, c.public_key, c.private_key, c.passphrase)
end

function authenticate_userpass(libgit2credptr::Ptr{Ptr{Void}}, p::RemotePayload)

    c = Base.get(p.credential)::UserPasswordCredential
    c.password = ""
    state = p.state

    if get!(state, :cache, 'Y') == 'Y' && !isnull(p.cache) && !isfilled(c)
        cred_id = "$(isempty(p.protocol) ? "ssh" : p.protocol)://$(p.host)"
        cached_cred = get_cred(unsafe_get(p.cache), cred_id, UserPasswordCredential)

        c.username = cached_cred.username
        c.password = cached_cred.password

        state[:cache] == 'U'
    end

    if get!(state, :prompt, 'Y') == 'Y' && !isfilled(c)
        prompt_url = git_url(protocol=p.protocol, host=p.host)
        @static if is_windows()
            res = Base.winprompt(
                "Please enter your credentials for '$prompt_url'",
                "Credentials required",
                c.username;
                prompt_username=true)
            isnull(res) && return Cint(Error.EAUTH)
            c.username, c.password = unsafe_get(res)
        else
            c.username = prompt("Username for '$prompt_url'", default=c.username)

            if !isempty(c.username)
                prompt_url = git_url(protocol=p.protocol, host=p.host, username=c.username)
                c.password = prompt("Password for '$prompt_url'", password=true)
            end
        end

        p.prompts_remaining -= 1
        if p.prompts_remaining <= 0
            state[:prompt] = 'N'
            ccall((:giterr_set_str, :libgit2), Void, (Cint, Cstring), Cint(Error.Callback), "Aborting, maximum number of user prompts reached.")
            return Cint(Error.EAUTH)
        end
    end

    return ccall((:git_cred_userpass_plaintext_new, :libgit2), Cint,
                 (Ptr{Ptr{Void}}, Cstring, Cstring),
                 libgit2credptr, c.username, c.password)
end


"""Credentials callback function

Function provides different credential acquisition functionality w.r.t. a connection protocol.
If a payload is provided then `payload_ptr` should contain a `LibGit2.AbstractCredentials` object.

For `LibGit2.Consts.CREDTYPE_USERPASS_PLAINTEXT` type, if the payload contains fields:
`user` & `pass`, they are used to create authentication credentials.
Empty `user` name and `pass`word trigger an authentication error.

For `LibGit2.Consts.CREDTYPE_SSH_KEY` type, if the payload contains fields:
`user`, `prvkey`, `pubkey` & `pass`, they are used to create authentication credentials.
Empty `user` name triggers an authentication error.

Credentials are checked in the following order (if supported):
- ssh key pair (`ssh-agent` if specified in payload's `usesshagent` field)
- plain text

**Note**: Due to the specifics of the `libgit2` authentication procedure, when
authentication fails, this function is called again without any indication whether
authentication was successful or not. To avoid an infinite loop from repeatedly
using the same faulty credentials, the `checkused!` function can be called. This
function returns `true` if the credentials were used.
Using credentials triggers a user prompt for (re)entering required information.
`UserPasswordCredential` and `CachedCredentials` are implemented using a call
counting strategy that prevents repeated usage of faulty credentials.
"""
function credentials_callback(libgit2credptr::Ptr{Ptr{Void}}, url_ptr::Cstring,
                              username_ptr::Cstring,
                              allowed_types::Cuint, payload_ptr::Ptr{Void})
    err = Cint(0)
    handled = false

    # get credentials object from payload pointer
    @assert payload_ptr != C_NULL
    p = unsafe_pointer_to_objref(payload_ptr)[]

    # parse url for protocol and host
    if isempty(p.host)
        url = match(GIT_URL_REGEX, unsafe_string(url_ptr))

        p.protocol = url[:protocol] === nothing ? "" : url[:protocol]
        p.username = url[:user] === nothing ? "" : url[:user]
        p.host = url[:host]
        p.path = url[:path]
    end

    # use ssh key or ssh-agent
    if isset(allowed_types, Cuint(Consts.CREDTYPE_SSH_KEY))
        handled = true
        if isnull(p.credential) || !isa(Base.get(p.credential), SSHCredential)
            p.credential = Nullable(SSHCredential(p.username))
        end
        err = authenticate_ssh(libgit2credptr, username_ptr, p)
        err == 0 && return err
    end

    if isset(allowed_types, Cuint(Consts.CREDTYPE_USERPASS_PLAINTEXT))
        handled = true
        if isnull(p.credential) || !isa(Base.get(p.credential), UserPasswordCredential)
            p.credential = Nullable(UserPasswordCredential(p.username))
        end
        err = authenticate_userpass(libgit2credptr, p)
        err == 0 && return err
    end

    # Fail safe when we are unable to handle any of the allowed_types
    if !handled
        ccall((:giterr_set_str, :libgit2), Void, (Cint, Cstring), Cint(Error.Callback),
            @sprintf("Aborting credential callback. Unable authenticate using allowed types 0x%08x", allowed_types))
        err = Cint(Error.EAUTH)
    end

    return err
end

"""
    credentials_approve(payload::RemotePayload) -> Void

Saves the payload credentials for future requests to avoid interative authentication. Should
only be called if the credential callback resulted in a successful operation.
"""
function credentials_approve(p::RemotePayload)
    isnull(p.credential) && return  # No credentials were used

    c = Base.get(p.credential)
    cred_id = "$(isempty(p.protocol) ? "ssh" : p.protocol)://$(p.host)"

    if !isnull(p.cache)
        cache = Base.get(p.cache)
        approve(cache, cred_id, c)
    end
end

"""
    credentials_reject(payload::RemotePayload) -> Void

Removes the payload credentials if they were stored to avoid automatic re-use. Should only
be called if the credential callback resulted in a unsuccessful operation.
"""
function credentials_reject(p::RemotePayload)
    isnull(p.credential) && return  # No credentials were used

    c = Base.get(p.credential)
    cred_id = "$(isempty(p.protocol) ? "ssh" : p.protocol)://$(p.host)"

    if !isnull(p.cache)
        cache = Base.get(p.cache)
        reject(cache, cred_id, c)
    end
end

function fetchhead_foreach_callback(ref_name::Cstring, remote_url::Cstring,
                        oid::Ptr{GitHash}, is_merge::Cuint, payload::Ptr{Void})
    fhead_vec = unsafe_pointer_to_objref(payload)::Vector{FetchHead}
    push!(fhead_vec, FetchHead(unsafe_string(ref_name), unsafe_string(remote_url), unsafe_load(oid), is_merge == 1))
    return Cint(0)
end

"C function pointer for `mirror_callback`"
mirror_cb() = cfunction(mirror_callback, Cint, (Ptr{Ptr{Void}}, Ptr{Void}, Cstring, Cstring, Ptr{Void}))
"C function pointer for `credentials_callback`"
credentials_cb() = cfunction(credentials_callback, Cint, (Ptr{Ptr{Void}}, Cstring, Cstring, Cuint, Ptr{Void}))
"C function pointer for `fetchhead_foreach_callback`"
fetchhead_foreach_cb() = cfunction(fetchhead_foreach_callback, Cint, (Cstring, Cstring, Ptr{GitHash}, Cuint, Ptr{Void}))


function credential_loop(
    valid_credential::AbstractCredential, url::AbstractString, user::AbstractString, allowed_types::UInt32,
    cache::Nullable{CachedCredentials}=Nullable{CachedCredentials}(),
)
    cb = credentials_cb()
    libgitcred_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    payload_ptr = Ref(RemotePayload(cache))
    payload = payload_ptr[]

    # Emulate how LibGit2 uses the credential callback by repeatedly calling the function
    # until we find valid credentials or an exception is raised.
    num_iterations = 1
    err = Cint(0)
    while err == 0
        err = ccall(cb, Cint, (Ptr{Ptr{Void}}, Cstring, Cstring, Cuint, Ptr{Void}),
            libgitcred_ptr_ptr, url, isempty(user) ? C_NULL : user, allowed_types, pointer_from_objref(payload_ptr))

        # Check if the callback provided us with valid credentials
        if !isnull(payload.credential) && Base.get(payload.credential) == valid_credential
            break
        end

        num_iterations += 1
        if num_iterations > 20
            error("Credential callback seems to be caught in an infinite loop")
        end
    end

    return err
end

function credential_loop(
    valid_credential::UserPasswordCredential;
    url="https://github.com/test/package.jl", user="", cache=Nullable{CachedCredentials}())
    credential_loop(valid_credential, url, user, 0x000001, cache)
end

function credential_loop(
    valid_credential::SSHCredential;
    url="git@github.com/test/package.jl", user="git", cache=Nullable{CachedCredentials}())
    credential_loop(valid_credential, url, user, 0x000046, cache)
end

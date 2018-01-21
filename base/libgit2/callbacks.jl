# This file is a part of Julia. License is MIT: https://julialang.org/license

"""Mirror callback function

Function sets `+refs/*:refs/*` refspecs and `mirror` flag for remote reference.
"""
function mirror_callback(remote::Ptr{Ptr{Cvoid}}, repo_ptr::Ptr{Cvoid},
                         name::Cstring, url::Cstring, payload::Ptr{Cvoid})
    # Create the remote with a mirroring url
    fetch_spec = "+refs/*:refs/*"
    err = ccall((:git_remote_create_with_fetchspec, :libgit2), Cint,
                (Ptr{Ptr{Cvoid}}, Ptr{Cvoid}, Cstring, Cstring, Cstring),
                remote, repo_ptr, name, url, fetch_spec)
    err != 0 && return Cint(err)

    # And set the configuration option to true for the push command
    config = GitConfig(GitRepo(repo_ptr,false))
    name_str = unsafe_string(name)
    err= try set!(config, "remote.$name_str.mirror", true)
         catch; -1
         finally close(config)
         end
    err != 0 && return Cint(err)
    return Cint(0)
end

"""
    LibGit2.is_passphrase_required(private_key) -> Bool

Return `true` if the `private_key` file requires a passphrase, `false` otherwise.
"""
function is_passphrase_required(private_key::AbstractString)
    !isfile(private_key) && return false

    # In encrypted private keys, the second line is "Proc-Type: 4,ENCRYPTED"
    return open(private_key) do f
        readline(f)
        readline(f) == "Proc-Type: 4,ENCRYPTED"
    end
end

function user_abort()
    # Note: Potentially it could be better to just throw a Julia error.
    ccall((:giterr_set_str, :libgit2), Cvoid,
          (Cint, Cstring), Cint(Error.Callback),
          "Aborting, user cancelled credential request.")
    return Cint(Error.EUSER)
end

function prompt_limit()
    ccall((:giterr_set_str, :libgit2), Cvoid,
          (Cint, Cstring), Cint(Error.Callback),
          "Aborting, maximum number of prompts reached.")
    return Cint(Error.EAUTH)
end

function exhausted_abort()
    ccall((:giterr_set_str, :libgit2), Cvoid,
          (Cint, Cstring), Cint(Error.Callback),
          "All authentication methods have failed.")
    return Cint(Error.EAUTH)
end

function authenticate_ssh(libgit2credptr::Ptr{Ptr{Cvoid}}, p::CredentialPayload, username_ptr)
    cred = p.credential::SSHCredential
    revised = false

    # Use a filled credential as-is on the first pass. Reset password on sucessive calls.
    if p.first_pass && isfilled(cred)
        revised = true
    elseif !p.first_pass
        cred.pass = ""
    end

    # first try ssh-agent if credentials support its usage
    if p.use_ssh_agent && username_ptr != Cstring(C_NULL) && (!revised || !isfilled(cred))
        err = ccall((:git_cred_ssh_key_from_agent, :libgit2), Cint,
                    (Ptr{Ptr{Cvoid}}, Cstring), libgit2credptr, username_ptr)

        p.use_ssh_agent = false  # use ssh-agent only one time
        err == 0 && return Cint(0)
    end

    if p.use_env && (!revised || !isfilled(cred))
        if isempty(cred.user) && username_ptr != Cstring(C_NULL)
            cred.user = unsafe_string(username_ptr)
        end

        cred.prvkey = Base.get(ENV, "SSH_KEY_PATH") do
            default = joinpath(homedir(), ".ssh", "id_rsa")
            if isempty(cred.prvkey) && isfile(default)
                default
            else
                cred.prvkey
            end
        end

        cred.pubkey = Base.get(ENV, "SSH_PUB_KEY_PATH") do
            default = cred.prvkey * ".pub"
            if isempty(cred.pubkey) && isfile(default)
                default
            else
                cred.pubkey
            end
        end

        cred.pass = Base.get(ENV, "SSH_KEY_PASS", cred.pass)

        revised = true
        p.use_env = false
    end

    if p.remaining_prompts > 0 && (!revised || !isfilled(cred))
        if isempty(cred.user) || username_ptr == Cstring(C_NULL)
            url = git_url(scheme=p.scheme, host=p.host)
            response = Base.prompt("Username for '$url'", default=cred.user)
            response === nothing && return user_abort()
            cred.user = response
        end

        url = git_url(scheme=p.scheme, host=p.host, username=cred.user)

        # For SSH we need a private key location
        last_private_key = cred.prvkey
        if !isfile(cred.prvkey) || !revised || !haskey(ENV, "SSH_KEY_PATH")
            response = Base.prompt("Private key location for '$url'", default=cred.prvkey)
            response === nothing && return user_abort()
            cred.prvkey = expanduser(response)

            # Only update the public key if the private key changed
            if cred.prvkey != last_private_key
                cred.pubkey = cred.prvkey * ".pub"
            end
        end

        # For SSH we need a public key location. Avoid asking about the public key as
        # typically this will just annoy users.
        stale = !p.first_pass && cred.prvkey == last_private_key && cred.pubkey != cred.prvkey * ".pub"
        if isfile(cred.prvkey) && (stale || !isfile(cred.pubkey))
            response = Base.prompt("Public key location for '$url'", default=cred.pubkey)
            response === nothing && return user_abort()
            cred.pubkey = expanduser(response)
        end

        # Ask for a passphrase when the private key exists and requires a passphrase
        if isempty(cred.pass) && is_passphrase_required(cred.prvkey)
            if Sys.iswindows()
                response = Base.winprompt(
                    "Your SSH Key requires a password, please enter it now:",
                    "Passphrase required", cred.prvkey; prompt_username=false)
                response === nothing && return user_abort()
                cred.pass = response[2]
            else
                response = Base.prompt("Passphrase for $(cred.prvkey)", password=true)
                response === nothing && return user_abort()
                cred.pass = response
                isempty(cred.pass) && return user_abort()  # Ambiguous if EOF or newline
            end
        end

        revised = true

        p.remaining_prompts -= 1
        p.remaining_prompts <= 0 && return prompt_limit()
    end

    if !revised
        return exhausted_abort()
    end

    return ccall((:git_cred_ssh_key_new, :libgit2), Cint,
                 (Ptr{Ptr{Cvoid}}, Cstring, Cstring, Cstring, Cstring),
                 libgit2credptr, cred.user, cred.pubkey, cred.prvkey, cred.pass)
end

function authenticate_userpass(libgit2credptr::Ptr{Ptr{Cvoid}}, p::CredentialPayload)
    cred = p.credential::UserPasswordCredential
    revised = false

    # Use a filled credential as-is on the first pass. Reset password on sucessive calls.
    if p.first_pass && isfilled(cred)
        revised = true
    elseif !p.first_pass
        cred.pass = ""
    end

    if p.use_git_helpers && (!revised || !isfilled(cred))
        git_cred = GitCredential(p.config, p.url)

        # Use `deepcopy` to ensure zeroing the `git_cred` doesn't also zero the `cred`s copy
        cred.user = deepcopy(coalesce(git_cred.username, ""))
        cred.pass = deepcopy(coalesce(git_cred.password, ""))
        securezero!(git_cred)
        revised = true

        p.use_git_helpers = false
    end

    if p.remaining_prompts > 0 && (!revised || !isfilled(cred))
        url = git_url(scheme=p.scheme, host=p.host)
        username = isempty(cred.user) ? p.username : cred.user
        if Sys.iswindows()
            response = Base.winprompt(
                "Please enter your credentials for '$url'", "Credentials required",
                username; prompt_username=true)
            response === nothing && return user_abort()
            cred.user, cred.pass = response
        else
            response = Base.prompt("Username for '$url'", default=username)
            response === nothing && return user_abort()
            cred.user = response

            url = git_url(scheme=p.scheme, host=p.host, username=cred.user)
            response = Base.prompt("Password for '$url'", password=true)
            response === nothing && return user_abort()
            cred.pass = response
            isempty(cred.pass) && return user_abort()  # Ambiguous if EOF or newline
        end

        revised = true

        p.remaining_prompts -= 1
        p.remaining_prompts <= 0 && return prompt_limit()
    end

    if !revised
        return exhausted_abort()
    end

    return ccall((:git_cred_userpass_plaintext_new, :libgit2), Cint,
                 (Ptr{Ptr{Cvoid}}, Cstring, Cstring),
                 libgit2credptr, cred.user, cred.pass)
end


"""
    credential_callback(...) -> Cint

A LibGit2 credential callback function which provides different credential acquisition
functionality w.r.t. a connection protocol. The `payload_ptr` is required to contain a
`LibGit2.CredentialPayload` object which will keep track of state and settings.

The `allowed_types` contains a bitmask of `LibGit2.Consts.GIT_CREDTYPE` values specifying
which authentication methods should be attempted.

Credential authentication is done in the following order (if supported):
- SSH agent
- SSH private/public key pair
- Username/password plain text

If a user is presented with a credential prompt they can abort the prompt by typing `^D`
(pressing the control key together with the `d` key).

**Note**: Due to the specifics of the `libgit2` authentication procedure, when
authentication fails, this function is called again without any indication whether
authentication was successful or not. To avoid an infinite loop from repeatedly
using the same faulty credentials, we will keep track of state using the payload.

For addition details see the LibGit2 guide on
[authenticating against a server](https://libgit2.github.com/docs/guides/authentication/).
"""
function credentials_callback(libgit2credptr::Ptr{Ptr{Cvoid}}, url_ptr::Cstring,
                              username_ptr::Cstring,
                              allowed_types::Cuint, payload_ptr::Ptr{Cvoid})
    err = Cint(0)

    # get `CredentialPayload` object from payload pointer
    @assert payload_ptr != C_NULL
    p = unsafe_pointer_to_objref(payload_ptr)::CredentialPayload

    # Parse URL only during the first call to this function. Future calls will use the
    # information cached inside the payload.
    if isempty(p.url)
        p.url = unsafe_string(url_ptr)
        m = match(URL_REGEX, p.url)

        p.scheme = coalesce(m[:scheme], "")
        p.username = coalesce(m[:user], "")
        p.host = m[:host]

        # When an explicit credential is supplied we will make sure to use the given
        # credential during the first callback by modifying the allowed types. The
        # modification only is in effect for the first callback since `allowed_types` cannot
        # be mutated.
        if p.explicit !== nothing
            cred = p.explicit

            # Copy explicit credentials to avoid mutating approved credentials.
            p.credential = deepcopy(cred)

            if isa(cred, SSHCredential)
                allowed_types &= Cuint(Consts.CREDTYPE_SSH_KEY)
            elseif isa(cred, UserPasswordCredential)
                allowed_types &= Cuint(Consts.CREDTYPE_USERPASS_PLAINTEXT)
            else
                allowed_types &= Cuint(0)  # Unhandled credential type
            end
        elseif p.cache !== nothing
            cred_id = credential_identifier(p.scheme, p.host)

            # Perform a deepcopy as we do not want to mutate approved cached credentials
            if haskey(p.cache, cred_id)
                p.credential = deepcopy(p.cache[cred_id])
            end
        end

        p.first_pass = true
    else
        p.first_pass = false
    end

    # use ssh key or ssh-agent
    if isset(allowed_types, Cuint(Consts.CREDTYPE_SSH_KEY))
        if p.credential === nothing || !isa(p.credential, SSHCredential)
            p.credential = SSHCredential(p.username)
        end
        err = authenticate_ssh(libgit2credptr, p, username_ptr)
        err == 0 && return err
    end

    if isset(allowed_types, Cuint(Consts.CREDTYPE_USERPASS_PLAINTEXT))
        if p.credential === nothing || !isa(p.credential, UserPasswordCredential)
            p.credential = UserPasswordCredential(p.username)
        end
        err = authenticate_userpass(libgit2credptr, p)
        err == 0 && return err
    end

    # No authentication method we support succeeded. The most likely cause is
    # that explicit credentials were passed in, but said credentials are incompatible
    # with the requested authentication method.
    if err == 0
        if p.explicit !== nothing
            ccall((:giterr_set_str, :libgit2), Cvoid, (Cint, Cstring), Cint(Error.Callback),
                  "The explicitly provided credential is incompatible with the requested " *
                  "authentication methods.")
        end
        err = Cint(Error.EAUTH)
    end
    return err
end

function fetchhead_foreach_callback(ref_name::Cstring, remote_url::Cstring,
                        oid_ptr::Ptr{GitHash}, is_merge::Cuint, payload::Ptr{Cvoid})
    fhead_vec = unsafe_pointer_to_objref(payload)::Vector{FetchHead}
    Base.push!(fhead_vec, FetchHead(unsafe_string(ref_name), unsafe_string(remote_url),
        unsafe_load(oid_ptr), is_merge == 1))
    return Cint(0)
end

"C function pointer for `mirror_callback`"
mirror_cb() = cfunction(mirror_callback, Cint, Tuple{Ptr{Ptr{Cvoid}}, Ptr{Cvoid}, Cstring, Cstring, Ptr{Cvoid}})
"C function pointer for `credentials_callback`"
credentials_cb() = cfunction(credentials_callback, Cint, Tuple{Ptr{Ptr{Cvoid}}, Cstring, Cstring, Cuint, Ptr{Cvoid}})
"C function pointer for `fetchhead_foreach_callback`"
fetchhead_foreach_cb() = cfunction(fetchhead_foreach_callback, Cint, Tuple{Cstring, Cstring, Ptr{GitHash}, Cuint, Ptr{Cvoid}})

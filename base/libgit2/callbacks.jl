# This file is a part of Julia. License is MIT: https://julialang.org/license

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
    ccall((:giterr_set_str, :libgit2), Void,
          (Cint, Cstring),
          Cint(Error.Callback), "Aborting, user cancelled credential request.")

    return Cint(Error.EAUTH)
end

function authenticate_ssh(libgit2credptr::Ptr{Ptr{Void}}, p::CredentialPayload,
        username_ptr, schema, host)
    creds = Base.get(p.credential)::SSHCredentials
    isusedcreds = checkused!(creds)

    # Note: The same SSHCredentials can be used to authenticate separate requests using the
    # same credential cache. e.g. using Pkg.update when there are two private packages.
    errcls, errmsg = Error.last_error()
    if errcls != Error.None
        # Check if we used ssh-agent
        if creds.usesshagent == "U"
            creds.usesshagent = "E" # reported ssh-agent error, disables ssh agent use for the future
        end
    end

    # first try ssh-agent if credentials support its usage
    if creds.usesshagent == "Y" || creds.usesshagent == "U"
        err = ccall((:git_cred_ssh_key_from_agent, :libgit2), Cint,
                     (Ptr{Ptr{Void}}, Cstring), libgit2credptr, username_ptr)
        creds.usesshagent = "U" # used ssh-agent only one time
        err == 0 && return Cint(0)
    end

    if creds.prompt_if_incorrect
        # if username is not provided or empty, then prompt for it
        username = username_ptr != Cstring(C_NULL) ? unsafe_string(username_ptr) : ""
        if isempty(username)
            uname = creds.user # check if credentials were already used
            prompt_url = git_url(scheme=schema, host=host)
            if !isusedcreds
                username = uname
            else
                response = Base.prompt("Username for '$prompt_url'", default=uname)
                isnull(response) && return user_abort()
                username = unsafe_get(response)
            end
        end

        prompt_url = git_url(scheme=schema, host=host, username=username)

        # For SSH we need a private key location
        privatekey = if haskey(ENV,"SSH_KEY_PATH")
            ENV["SSH_KEY_PATH"]
        else
            keydefpath = creds.prvkey # check if credentials were already used
            if isempty(keydefpath) || isusedcreds
                defaultkeydefpath = joinpath(homedir(),".ssh","id_rsa")
                if isempty(keydefpath) && isfile(defaultkeydefpath)
                    keydefpath = defaultkeydefpath
                else
                    response = Base.prompt("Private key location for '$prompt_url'",
                        default=keydefpath)
                    isnull(response) && return user_abort()
                    keydefpath = unsafe_get(response)
                end
            end
            keydefpath
        end

        isfile(privatekey) || warn("Private key not found")

        # If the private key changed, invalidate the cached public key
        (privatekey != creds.prvkey) &&
            (creds.pubkey = "")

        # For SSH we need a public key location, look for environment vars SSH_* as well
        publickey = if haskey(ENV,"SSH_PUB_KEY_PATH")
            ENV["SSH_PUB_KEY_PATH"]
        else
            keydefpath = creds.pubkey # check if credentials were already used
            if isempty(keydefpath) || isusedcreds
                if isempty(keydefpath)
                    keydefpath = privatekey*".pub"
                end
                if !isfile(keydefpath)
                    response = Base.prompt("Public key location for '$prompt_url'",
                        default=keydefpath)
                    isnull(response) && return user_abort()
                    keydefpath = unsafe_get(response)
                end
            end
            keydefpath
        end

        passphrase = if haskey(ENV,"SSH_KEY_PASS")
            ENV["SSH_KEY_PASS"]
        else
            passdef = creds.pass # check if credentials were already used
            if (isempty(passdef) || isusedcreds) && is_passphrase_required(privatekey)
                if Sys.iswindows()
                    response = Base.winprompt(
                        "Your SSH Key requires a password, please enter it now:",
                        "Passphrase required", privatekey; prompt_username = false)
                    isnull(response) && return user_abort()
                    passdef = unsafe_get(response)[2]
                else
                    response = Base.prompt("Passphrase for $privatekey", password=true)
                    isnull(response) && return user_abort()
                    passdef = unsafe_get(response)
                    isempty(passdef) && return user_abort()  # Ambiguous if EOF or newline
                end
            end
            passdef
        end
        ((creds.user != username) || (creds.pass != passphrase) ||
            (creds.prvkey != privatekey) || (creds.pubkey != publickey)) && reset!(creds)

        creds.user = username # save credentials
        creds.prvkey = privatekey # save credentials
        creds.pubkey = publickey # save credentials
        creds.pass = passphrase
    else
        isusedcreds && return Cint(Error.EAUTH)
    end

    return ccall((:git_cred_ssh_key_new, :libgit2), Cint,
                 (Ptr{Ptr{Void}}, Cstring, Cstring, Cstring, Cstring),
                 libgit2credptr, creds.user, creds.pubkey, creds.prvkey, creds.pass)
end

function authenticate_userpass(libgit2credptr::Ptr{Ptr{Void}}, p::CredentialPayload,
        schema, host, urlusername)
    creds = Base.get(p.credential)::UserPasswordCredentials
    isusedcreds = checkused!(creds)

    if creds.prompt_if_incorrect
        username = creds.user
        userpass = creds.pass
        prompt_url = git_url(scheme=schema, host=host)
        if Sys.iswindows()
            if isempty(username) || isempty(userpass) || isusedcreds
                response = Base.winprompt("Please enter your credentials for '$prompt_url'", "Credentials required",
                    isempty(username) ? urlusername : username; prompt_username = true)
                isnull(response) && return user_abort()
                username, userpass = unsafe_get(response)
            end
        elseif isusedcreds
            response = Base.prompt("Username for '$prompt_url'",
                default=isempty(username) ? urlusername : username)
            isnull(response) && return user_abort()
            username = unsafe_get(response)

            prompt_url = git_url(scheme=schema, host=host, username=username)
            response = Base.prompt("Password for '$prompt_url'", password=true)
            isnull(response) && return user_abort()
            userpass = unsafe_get(response)
            isempty(userpass) && return user_abort()  # Ambiguous if EOF or newline
        end

        ((creds.user != username) || (creds.pass != userpass)) && reset!(creds)
        creds.user = username # save credentials
        creds.pass = userpass # save credentials
    else
        isusedcreds && return Cint(Error.EAUTH)
    end

    return ccall((:git_cred_userpass_plaintext_new, :libgit2), Cint,
                 (Ptr{Ptr{Void}}, Cstring, Cstring),
                 libgit2credptr, creds.user, creds.pass)
end


"""Credentials callback function

Function provides different credential acquisition functionality w.r.t. a connection protocol.
If a payload is provided then `payload_ptr` should contain a `LibGit2.CredentialPayload` object.

For `LibGit2.Consts.CREDTYPE_USERPASS_PLAINTEXT` type, if the payload contains fields:
`user` & `pass`, they are used to create authentication credentials.

For `LibGit2.Consts.CREDTYPE_SSH_KEY` type, if the payload contains fields:
`user`, `prvkey`, `pubkey` & `pass`, they are used to create authentication credentials.

Typing `^D` (control key together with the `d` key) will abort the credential prompt.

Credentials are checked in the following order (if supported):
- ssh key pair (`ssh-agent` if specified in payload's `usesshagent` field)
- plain text

**Note**: Due to the specifics of the `libgit2` authentication procedure, when
authentication fails, this function is called again without any indication whether
authentication was successful or not. To avoid an infinite loop from repeatedly
using the same faulty credentials, the `checkused!` function can be called. This
function returns `true` if the credentials were used.
Using credentials triggers a user prompt for (re)entering required information.
`UserPasswordCredentials` and `CachedCredentials` are implemented using a call
counting strategy that prevents repeated usage of faulty credentials.
"""
function credentials_callback(libgit2credptr::Ptr{Ptr{Void}}, url_ptr::Cstring,
                              username_ptr::Cstring,
                              allowed_types::Cuint, payload_ptr::Ptr{Void})
    err = Cint(0)
    url = unsafe_string(url_ptr)

    # get `CredentialPayload` object from payload pointer
    @assert payload_ptr != C_NULL
    p = unsafe_pointer_to_objref(payload_ptr)[]::CredentialPayload

    # parse url for schema and host
    urlparts = match(URL_REGEX, url)
    schema = urlparts[:scheme] === nothing ? "" : urlparts[:scheme]
    urlusername = urlparts[:user] === nothing ? "" : urlparts[:user]
    host = urlparts[:host]

    if !isnull(p.credential)
        creds = unsafe_get(p.credential)
        explicit = true
    else
        creds = Base.get(p.cache, nothing)
        explicit = false
    end

    # use ssh key or ssh-agent
    if isset(allowed_types, Cuint(Consts.CREDTYPE_SSH_KEY))
        sshcreds = get_creds!(creds, "ssh://$host", reset!(SSHCredentials(true), -1))
        if isa(sshcreds, SSHCredentials)
            p.credential = Nullable(sshcreds)
            err = authenticate_ssh(libgit2credptr, p, username_ptr, schema, host)
            err == 0 && return err
        end
    end

    if isset(allowed_types, Cuint(Consts.CREDTYPE_USERPASS_PLAINTEXT))
        defaultcreds = reset!(UserPasswordCredentials(true), -1)
        credid = "$(isempty(schema) ? "ssh" : schema)://$host"
        upcreds = get_creds!(creds, credid, defaultcreds)
        # If there were stored SSH credentials, but we ended up here that must
        # mean that something went wrong. Replace the SSH credentials by user/pass
        # credentials
        if !isa(upcreds, UserPasswordCredentials)
            upcreds = defaultcreds
            isa(creds, CachedCredentials) && (creds.creds[credid] = upcreds)
        end
        p.credential = Nullable(upcreds)
        return authenticate_userpass(libgit2credptr, p, schema, host, urlusername)
    end

    # No authentication method we support succeeded. The most likely cause is
    # that explicit credentials were passed in, but said credentials are incompatible
    # with the remote host.
    if err == 0
        if explicit
            warn("The explicitly provided credentials were incompatible with " *
                 "the server's supported authentication methods")
        end
        err = Cint(Error.EAUTH)
    end
    return err
end

function fetchhead_foreach_callback(ref_name::Cstring, remote_url::Cstring,
                        oid_ptr::Ptr{GitHash}, is_merge::Cuint, payload::Ptr{Void})
    fhead_vec = unsafe_pointer_to_objref(payload)::Vector{FetchHead}
    push!(fhead_vec, FetchHead(unsafe_string(ref_name), unsafe_string(remote_url),
        unsafe_load(oid_ptr), is_merge == 1))
    return Cint(0)
end

"C function pointer for `mirror_callback`"
mirror_cb() = cfunction(mirror_callback, Cint, Tuple{Ptr{Ptr{Void}}, Ptr{Void}, Cstring, Cstring, Ptr{Void}})
"C function pointer for `credentials_callback`"
credentials_cb() = cfunction(credentials_callback, Cint, Tuple{Ptr{Ptr{Void}}, Cstring, Cstring, Cuint, Ptr{Void}})
"C function pointer for `fetchhead_foreach_callback`"
fetchhead_foreach_cb() = cfunction(fetchhead_foreach_callback, Cint, Tuple{Cstring, Cstring, Ptr{GitHash}, Cuint, Ptr{Void}})

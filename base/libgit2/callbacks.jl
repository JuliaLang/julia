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
    config = GitConfig(GitRepo(repo_ptr))
    name_str = unsafe_string(name)
    err= try set!(config, "remote.$name_str.mirror", true)
         catch -1
         finally finalize(config)
         end
    err != 0 && return Cint(err)
    return Cint(0)
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
`UserPasswordCredentials` and `CachedCredentials` are implemented using a call
counting strategy that prevents repeated usage of faulty credentials.
"""
function credentials_callback(cred::Ptr{Ptr{Void}}, url_ptr::Cstring,
                              username_ptr::Cstring,
                              allowed_types::Cuint, payload_ptr::Ptr{Void})
    err = 0
    url = unsafe_string(url_ptr)

    # parse url for schema and host
    urlparts = match(urlmatcher, url)
    schema = urlparts.captures[1]
    host = urlparts.captures[5]
    schema = schema === nothing ? "" : schema*"://"

    # get credentials object from payload pointer
    creds = nothing
    if payload_ptr != C_NULL
        tmpobj = unsafe_pointer_to_objref(payload_ptr)
        if isa(tmpobj, AbstractCredentials)
            creds = tmpobj
        end
    end
    isusedcreds = checkused!(creds)

    # use ssh key or ssh-agent
    if isset(allowed_types, Cuint(Consts.CREDTYPE_SSH_KEY))
        creds == nothing && (creds = SSHCredentials())
        credid = "ssh://$host"

        # first try ssh-agent if credentials support its usage
        if creds[:usesshagent, credid] === nothing || creds[:usesshagent, credid] == "Y"
            err = ccall((:git_cred_ssh_key_from_agent, :libgit2), Cint,
                         (Ptr{Ptr{Void}}, Cstring), cred, username_ptr)
            creds[:usesshagent, credid] = "U" # used ssh-agent only one time
            err == 0 && return Cint(0)
        end

        errcls, errmsg = Error.last_error()
        if errcls != Error.None
            # Check if we used ssh-agent
            if creds[:usesshagent, credid] == "U"
                println("ERROR: $errmsg ssh-agent")
                creds[:usesshagent, credid] = "E" # reported ssh-agent error
            else
                println("ERROR: $errmsg")
            end
            flush(STDOUT)
        end

        # if username is not provided, then prompt for it
        username = if username_ptr == Cstring(C_NULL)
            uname = creds[:user, credid] # check if credentials were already used
            uname !== nothing && !isusedcreds ? uname : prompt("Username for '$schema$host'")
        else
            unsafe_string(username_ptr)
        end
        creds[:user, credid] = username # save credentials

        # For SSH we need a private key location
        privatekey = if haskey(ENV,"SSH_KEY_PATH")
            ENV["SSH_KEY_PATH"]
        else
            keydefpath = creds[:prvkey, credid] # check if credentials were already used
            if keydefpath !== nothing && !isusedcreds
                keydefpath # use cached value
            else
                keydefpath = if keydefpath === nothing
                    joinpath(homedir(),".ssh","id_rsa")
                end
                prompt("Private key location for '$schema$username@$host'", default=keydefpath)
            end
        end
        creds[:prvkey, credid] = privatekey # save credentials

        # For SSH we need a public key location, look for environment vars SSH_* as well
        publickey = if haskey(ENV,"SSH_PUB_KEY_PATH")
            ENV["SSH_PUB_KEY_PATH"]
        else
            keydefpath = creds[:pubkey, credid] # check if credentials were already used
            if keydefpath !== nothing && !isusedcreds
                keydefpath # use cached value
            else
                keydefpath = if keydefpath === nothing
                    privatekey*".pub"
                end
                if isfile(keydefpath)
                    keydefpath
                else
                    prompt("Public key location for '$schema$username@$host'", default=keydefpath)
                end
            end
        end
        creds[:pubkey, credid] = publickey # save credentials

        passphrase = if haskey(ENV,"SSH_KEY_PASS")
            ENV["SSH_KEY_PASS"]
        else
            passdef = creds[:pass, credid] # check if credentials were already used
            passdef !== nothing && !isusedcreds ? passdef : prompt("Passphrase for $privatekey", password=true)
        end
        creds[:pass, credid] = passphrase # save credentials

        isempty(username) && return Cint(Error.EAUTH)

        err = ccall((:git_cred_ssh_key_new, :libgit2), Cint,
                     (Ptr{Ptr{Void}}, Cstring, Cstring, Cstring, Cstring),
                     cred, username, publickey, privatekey, passphrase)
        err == 0 && return Cint(0)
    end

    if isset(allowed_types, Cuint(Consts.CREDTYPE_USERPASS_PLAINTEXT))
        creds == nothing && (creds = UserPasswordCredentials())
        credid = "$schema$host"
        username = creds[:user, credid]
        if username === nothing || isusedcreds
            username = prompt("Username for '$schema$host'")
            creds[:user, credid] = username # save credentials
        end

        userpass = creds[:pass, credid]
        if userpass === nothing || isusedcreds
            userpass = prompt("Password for '$schema$username@$host'", password=true)
            creds[:pass, credid] = userpass # save credentials
        end

        isempty(username) && isempty(userpass) && return Cint(Error.EAUTH)

        err = ccall((:git_cred_userpass_plaintext_new, :libgit2), Cint,
                     (Ptr{Ptr{Void}}, Cstring, Cstring),
                     cred, username, userpass)
        err == 0 && return Cint(0)
    end

    return Cint(err)
end

function fetchhead_foreach_callback(ref_name::Cstring, remote_url::Cstring,
                        oid::Ptr{Oid}, is_merge::Cuint, payload::Ptr{Void})
    fhead_vec = unsafe_pointer_to_objref(payload)::Vector{FetchHead}
    push!(fhead_vec, FetchHead(unsafe_string(ref_name), unsafe_string(remote_url), Oid(oid), is_merge == 1))
    return Cint(0)
end

"C function pointer for `mirror_callback`"
mirror_cb() = cfunction(mirror_callback, Cint, (Ptr{Ptr{Void}}, Ptr{Void}, Cstring, Cstring, Ptr{Void}))
"C function pointer for `credentials_callback`"
credentials_cb() = cfunction(credentials_callback, Cint, (Ptr{Ptr{Void}}, Cstring, Cstring, Cuint, Ptr{Void}))
"C function pointer for `fetchhead_foreach_callback`"
fetchhead_foreach_cb() = cfunction(fetchhead_foreach_callback, Cint, (Cstring, Cstring, Ptr{Oid}, Cuint, Ptr{Void}))

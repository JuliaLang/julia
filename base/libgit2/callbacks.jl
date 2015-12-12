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
    name_str = bytestring(name)
    err= try set!(config, "remote.$name_str.mirror", true)
         catch -1
         finally finalize(config)
         end
    err != 0 && return Cint(err)
    return Cint(0)
end

"""Credentials callback function

Function provides different credential acquisition functionality w.r.t. a connection protocol.
If payload is provided then `payload_ptr` should contain `LibGit2.AbstractPayload` object.

For `LibGit2.Consts.CREDTYPE_USERPASS_PLAINTEXT` type, if payload contains fields: `user` & `pass` they are used to create authentication credentials.
In addition, if payload has field `used` it can be set to `true` to indicate that payload was used and abort callback with error. This behavior is required to avoid repeated authentication calls with incorrect credentials.
"""
function credentials_callback(cred::Ptr{Ptr{Void}}, url_ptr::Cstring,
                              username_ptr::Cstring,
                              allowed_types::Cuint, payload_ptr::Ptr{Void})
    err = 1
    url = bytestring(url_ptr)

    if isset(allowed_types, Cuint(Consts.CREDTYPE_USERPASS_PLAINTEXT))
        username = userpass = ""
        if payload_ptr != C_NULL
            payload = unsafe_pointer_to_objref(payload_ptr)
            if isa(payload, AbstractPayload)
                isused(payload) && return Cint(-1)
                username = user(payload)
                userpass = password(payload)
                setused!(payload, true)
            end
        end
        if isempty(username)
            username = prompt("Username for '$url'")
        end
        if isempty(userpass)
            userpass = prompt("Password for '$username@$url'", password=true)
        end

        isempty(username) && isempty(userpass) && return Cint(-1)

        err = ccall((:git_cred_userpass_plaintext_new, :libgit2), Cint,
                     (Ptr{Ptr{Void}}, Cstring, Cstring),
                     cred, username, userpass)
        err == 0 && return Cint(0)
    elseif isset(allowed_types, Cuint(Consts.CREDTYPE_SSH_KEY)) && err > 0
        # use ssh-agent
        err = ccall((:git_cred_ssh_key_from_agent, :libgit2), Cint,
                     (Ptr{Ptr{Void}}, Cstring), cred, username_ptr)
        err == 0 && return Cint(0)
    elseif isset(allowed_types, Cuint(Consts.CREDTYPE_SSH_CUSTOM)) && err > 0
        # for SSH we need key info, look for environment vars SSH_* as well

        # if username is not provided, then prompt for it
        username = if username_ptr == Cstring_NULL
            prompt("Username for '$url'")
        else
            bytestring(username_ptr)
        end

        publickey = if "SSH_PUB_KEY" in keys(ENV)
            ENV["GITHUB_PUB_KEY"]
        else
            keydef = homedir()*"/.ssh/id_rsa.pub"
            prompt("Public key location", default=keydef)
        end

        privatekey = if "SSH_PRV_KEY" in keys(ENV)
            ENV["GITHUB_PRV_KEY"]
        else
            keydef = homedir()*"/.ssh/id_rsa"
            prompt("Private key location", default=keydef)
        end

        passphrase= get(ENV,"SSH_PRV_KEY_PASS","0") == "0" ? "" : prompt("Private key passphrase", password=true)

        err = ccall((:git_cred_ssh_key_new, :libgit2), Cint,
                     (Ptr{Ptr{Void}}, Cstring, Cstring, Cstring, Cstring),
                     cred, username, publickey, privatekey, passphrase)
        err == 0 && return Cint(0)
    end

    return Cint(err)
end

function fetchhead_foreach_callback(ref_name::Cstring, remote_url::Cstring,
                        oid::Ptr{Oid}, is_merge::Cuint, payload::Ptr{Void})
    fhead_vec = unsafe_pointer_to_objref(payload)::Vector{FetchHead}
    push!(fhead_vec, FetchHead(bytestring(ref_name), bytestring(remote_url), Oid(oid), is_merge == 1))
    return Cint(0)
end

"C function pointer for `mirror_callback`"
mirror_cb() = cfunction(mirror_callback, Cint, (Ptr{Ptr{Void}}, Ptr{Void}, Cstring, Cstring, Ptr{Void}))
"C function pointer for `credentials_callback`"
credentials_cb() = cfunction(credentials_callback, Cint, (Ptr{Ptr{Void}}, Cstring, Cstring, Cuint, Ptr{Void}))
"C function pointer for `fetchhead_foreach_callback`"
fetchhead_foreach_cb() = cfunction(fetchhead_foreach_callback, Cint, (Cstring, Cstring, Ptr{Oid}, Cuint, Ptr{Void}))

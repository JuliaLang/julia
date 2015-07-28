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

function credentials_callback(cred::Ptr{Ptr{Void}}, url::Cstring,
                              username_from_url::Cstring,
                              allowed_types::Cuint, payload::Ptr{Void})
    # Try ssh-agent first
    err = ccall((:git_cred_ssh_key_from_agent, :libgit2), Cint,
                 (Ptr{Ptr{Void}}, Cstring),
                 cred, username_from_url)
    err == 0 && return Cint(0)

    # TODO: deal with other auth types + WIN

    return Cint(1)
end

const mirror_cb = cfunction(mirror_callback, Cint, (Ptr{Ptr{Void}}, Ptr{Void}, Cstring, Cstring, Ptr{Void}))
const credentials_cb = cfunction(credentials_callback, Cint, (Ptr{Ptr{Void}}, Cstring, Cstring, Cuint, Ptr{Void}))

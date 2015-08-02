function prompt(msg::AbstractString; default::AbstractString="", password::Bool=false)
    msg = length(default) > 0 ? msg*" [$default]:" : msg*":"
    uinput = if password
        bytestring(ccall(:getpass, Cstring, (Cstring,), msg))
    else
        print(msg)
        chomp(readline(STDIN))
    end
    length(uinput) == 0 ? default : uinput
end

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

function credentials_callback(cred::Ptr{Ptr{Void}}, url_ptr::Cstring,
                              username_ptr::Cstring,
                              allowed_types::Cuint, payload::Ptr{Void})
    url = bytestring(url_ptr)

    # for HTTPS use keyboard-interactive prompt
    if startswith(url, "https")
        username = prompt("Username for '$url'")
        pass     = prompt("Password for '$url'", password=true)

        err = ccall((:git_cred_userpass_plaintext_new, :libgit2), Cint,
                     (Ptr{Ptr{Void}}, Cstring, Cstring),
                     cred, username, pass)
        err != 0 && return Cint(err)
    else
        # for SSH we need key info, look for environment vars GITHUB_* as well

        # if username is not provided, then prompt for it
        username = if username_ptr == Cstring_NULL
            prompt("Username for '$url'")
        else
            bytestring(username_ptr)
        end

        publickey = if "GITHUB_PUB_KEY" in keys(ENV)
            ENV["GITHUB_PUB_KEY"]
        else
            keydef = homedir()*"/.ssh/id_rsa.pub"
            prompt("Public key location", default=keydef)
        end

        privatekey = if "GITHUB_PRV_KEY" in keys(ENV)
            ENV["GITHUB_PRV_KEY"]
        else
            keydef = homedir()*"/.ssh/id_rsa"
            prompt("Private key location", default=keydef)
        end

        passphrase= if "GITHUB_PRV_KEY_PASS" in keys(ENV)
            ENV["GITHUB_PRV_KEY_PASS"]
        else
            prompt("Private key passphrase", password=true)
        end

        err = ccall((:git_cred_ssh_key_new, :libgit2), Cint,
                     (Ptr{Ptr{Void}}, Cstring, Cstring, Cstring, Cstring),
                     cred, username, publickey, privatekey, passphrase)
        err != 0 && return Cint(err)
    end

    return Cint(0)
end

function fetchhead_foreach_callback(ref_name::Cstring, remote_url::Cstring,
                        oid::Ptr{Oid}, is_merge::Cuint, payload::Ptr{Void})
    fhead_vec = unsafe_pointer_to_objref(payload)::Vector{FetchHead}
    push!(fhead_vec, FetchHead(bytestring(ref_name), bytestring(remote_url), Oid(oid), is_merge == 1))
    return Cint(0)
end

const mirror_cb = cfunction(mirror_callback, Cint, (Ptr{Ptr{Void}}, Ptr{Void}, Cstring, Cstring, Ptr{Void}))
const credentials_cb = cfunction(credentials_callback, Cint, (Ptr{Ptr{Void}}, Cstring, Cstring, Cuint, Ptr{Void}))
const fetchhead_foreach_cb = cfunction(fetchhead_foreach_callback, Cint, (Cstring, Cstring, Ptr{Oid}, Cuint, Ptr{Void}))
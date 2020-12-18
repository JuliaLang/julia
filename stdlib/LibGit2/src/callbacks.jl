# This file is a part of Julia. License is MIT: https://julialang.org/license

"""Mirror callback function

Function sets `+refs/*:refs/*` refspecs and `mirror` flag for remote reference.
"""
function mirror_callback(remote::Ptr{Ptr{Cvoid}}, repo_ptr::Ptr{Cvoid},
                         name::Cstring, url::Cstring, payload::Ptr{Cvoid})
    ensure_initialized()
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
    ensure_initialized()
    # Note: Potentially it could be better to just throw a Julia error.
    ccall((:giterr_set_str, :libgit2), Cvoid,
          (Cint, Cstring), Cint(Error.Callback),
          "Aborting, user cancelled credential request.")
    return Cint(Error.EUSER)
end

function prompt_limit()
    ensure_initialized()
    ccall((:giterr_set_str, :libgit2), Cvoid,
          (Cint, Cstring), Cint(Error.Callback),
          "Aborting, maximum number of prompts reached.")
    return Cint(Error.EAUTH)
end

function exhausted_abort()
    ensure_initialized()
    ccall((:giterr_set_str, :libgit2), Cvoid,
          (Cint, Cstring), Cint(Error.Callback),
          "All authentication methods have failed.")
    return Cint(Error.EAUTH)
end

function authenticate_ssh(libgit2credptr::Ptr{Ptr{Cvoid}}, p::CredentialPayload, username_ptr)
    ensure_initialized()
    cred = p.credential::SSHCredential
    revised = false

    # Use a filled credential as-is on the first pass. Reset password on successive calls.
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
                response = Base.getpass("Passphrase for $(cred.prvkey)")
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
    ensure_initialized()
    cred = p.credential::UserPasswordCredential
    revised = false

    # Use a filled credential as-is on the first pass. Reset password on successive calls.
    if p.first_pass && isfilled(cred)
        revised = true
    elseif !p.first_pass
        cred.pass = ""
    end

    if p.use_git_helpers && (!revised || !isfilled(cred))
        git_cred = GitCredential(p.config, p.url)

         # Use `deepcopy` to ensure shredding the `git_cred` does not shred the `cred`s copy
        cred.user = something(git_cred.username, "")
        cred.pass = deepcopy(something(git_cred.password, ""))
        Base.shred!(git_cred)
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
            response = Base.getpass("Password for '$url'")
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
[authenticating against a server](https://libgit2.org/docs/guides/authentication/).
"""
function credentials_callback(libgit2credptr::Ptr{Ptr{Cvoid}}, url_ptr::Cstring,
                              username_ptr::Cstring, allowed_types::Cuint,
                              p::CredentialPayload)
    err = Cint(0)

    # Parse URL only during the first call to this function. Future calls will use the
    # information cached inside the payload.
    if isempty(p.url)
        p.url = unsafe_string(url_ptr)
        m = match(URL_REGEX, p.url)

        p.scheme = something(m[:scheme], SubString(""))
        p.username = something(m[:user], SubString(""))
        p.host = m[:host]

        # When an explicit credential is supplied we will make sure to use the given
        # credential during the first callback by modifying the allowed types. The
        # modification only is in effect for the first callback since `allowed_types` cannot
        # be mutated.
        if p.explicit !== nothing
            cred = p.explicit

            # Copy explicit credentials to avoid mutating approved credentials.
            # invalidation fix from cred being non-inferrable
            p.credential = Base.invokelatest(deepcopy, cred)

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
                # invalidation fix from p.cache[cred_id] being non-inferrable
                p.credential = Base.invokelatest(deepcopy, p.cache[cred_id])
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
            ensure_initialized()
            ccall((:giterr_set_str, :libgit2), Cvoid, (Cint, Cstring), Cint(Error.Callback),
                  "The explicitly provided credential is incompatible with the requested " *
                  "authentication methods.")
        end
        err = Cint(Error.EAUTH)
    end
    return err
end

function credentials_callback(libgit2credptr::Ptr{Ptr{Cvoid}}, url_ptr::Cstring,
                              username_ptr::Cstring, allowed_types::Cuint,
                              payloads::Dict{Symbol, Any})
    p = payloads[:credentials]
    return credentials_callback(libgit2credptr, url_ptr, username_ptr, allowed_types, p)
end

function fetchhead_foreach_callback(ref_name::Cstring, remote_url::Cstring,
                                    oid_ptr::Ptr{GitHash}, is_merge::Cuint, payload::Any)
    fhead_vec = payload::Vector{FetchHead}
    Base.push!(fhead_vec, FetchHead(unsafe_string(ref_name), unsafe_string(remote_url),
        unsafe_load(oid_ptr), is_merge == 1))
    return Cint(0)
end

struct CertHostKey
    parent :: Cint
    mask   :: Cint
    md5    :: NTuple{16,UInt8}
    sha1   :: NTuple{20,UInt8}
    sha256 :: NTuple{32,UInt8}
end

struct KeyHashes
    sha1   :: Union{NTuple{20,UInt8}, Nothing}
    sha256 :: Union{NTuple{32,UInt8}, Nothing}
end

function KeyHashes(cert_p::Ptr{CertHostKey})
    cert = unsafe_load(cert_p)
    return KeyHashes(
        cert.mask & Consts.CERT_SSH_SHA1   != 0 ? cert.sha1   : nothing,
        cert.mask & Consts.CERT_SSH_SHA256 != 0 ? cert.sha256 : nothing,
    )
end

function verify_host_error(message::AbstractString)
    printstyled(stderr, "$message\n", color = :cyan, bold = true)
end

function certificate_callback(
    cert_p :: Ptr{CertHostKey},
    valid  :: Cint,
    host_p :: Ptr{Cchar},
    data_p :: Ptr{Cvoid},
)::Cint
    valid != 0 && return Consts.CERT_ACCEPT
    host = unsafe_string(host_p)
    cert_type = unsafe_load(convert(Ptr{Cint}, cert_p))
    transport = cert_type == Consts.CERT_TYPE_TLS ? "TLS" :
                cert_type == Consts.CERT_TYPE_SSH ? "SSH" : nothing
    if !NetworkOptions.verify_host(host, transport)
        # user has opted out of host verification
        return Consts.CERT_ACCEPT
    end
    if transport == "TLS"
        # TLS verification is done before the callback and indicated with the
        # incoming `valid` flag, so if we get here then host verification failed
        verify_host_error("TLS host verification: the identity of the server `$host` could not be verified. Someone could be trying to man-in-the-middle your connection. It is also possible that the correct server is using an invalid certificate or that your system's certificate authority root store is misconfigured.")
        return Consts.CERT_REJECT
    elseif transport == "SSH"
        # SSH verification has to be done here
        files = [joinpath(homedir(), ".ssh", "known_hosts")]
        check = ssh_knownhost_check(files, host, KeyHashes(cert_p))
        valid = false
        if check == Consts.SSH_HOST_KNOWN
            valid = true
        elseif check == Consts.SSH_HOST_UNKNOWN
            if Sys.which("ssh-keyscan") !== nothing
                msg = "Please run `ssh-keyscan $host >> $(files[1])` in order to add the server to your known hosts file and then try again."
            else
                msg = "Please connect once using `ssh $host` in order to add the server to your known hosts file and then try again. You may not be allowed to log in (wrong user and/or no login allowed), but ssh will prompt you to add a host key for the server which will allow libgit2 to verify the server."
            end
            verify_host_error("SSH host verification: the server `$host` is not a known host. $msg")
        elseif check == Consts.SSH_HOST_MISMATCH
            verify_host_error("SSH host verification: the identity of the server `$host` does not match its known hosts record. Someone could be trying to man-in-the-middle your connection. It is also possible that the server has changed its key, in which case you should check with the server administrator and if they confirm that the key has been changed, update your known hosts file.")
        elseif check == Consts.SSH_HOST_BAD_HASH
            verify_host_error("SSH host verification: no secure certificate hash available for `$host`, cannot verify server identity.")
        else
            @error("unexpected SSH known host check result", check)
        end
        return valid ? Consts.CERT_ACCEPT : Consts.CERT_REJECT
    end
    @error("unexpected transport encountered, refusing to validate", cert_type)
    return Consts.CERT_REJECT
end

## SSH known host checking
#
# We can't use libssh2_knownhost_check because libgit2, for no good reason,
# doesn't give us a host fingerprint that we can use for that and instead gives
# us multiple hashes of that fingerprint instead. Moreover, since a host can
# have multiple fingerprints in the known hosts file with different encryption
# types (gitlab.com does this, for example), we need to iterate through all the
# known hosts entries and manually check if any of them is a match.
#
# The fact that libgit2 won't give us a fingerprint also means that we cannot,
# even if we wanted to, prompt the user for whether to add the fingerprint to
# the known hosts file, since we don't have the fingerprint that should be
# added. The only option is to instruct the user how to add it themselves.
#
# Check logic: if a host appears in a known hosts file at all then one of the
# keys in that file must match or we declare a mismatch; if the host name
# doesn't appear in the file at all, however, we will continue searching files.
#
# This allows adding a host to the system known hosts file to fully override
# that host appearing in a bundled known hosts file. It is necessary to allow
# any of multiple entries in a single file to match, however, to allow for the
# possiblity that the file contains multiple fingerprints for the same host. If
# libgit2 gave us the fucking fingerprint then we could search for only an entry
# with the correct type, but we can't do that without the actual fingerprint.

struct KnownHost
    magic :: Cuint
    node  :: Ptr{Cvoid}
    name  :: Ptr{Cchar}
    key   :: Ptr{Cchar}
    type  :: Cint
end

function ssh_knownhost_check(
    files  :: AbstractVector{<:AbstractString},
    host   :: AbstractString,
    hashes :: KeyHashes,
)
    hashes.sha1 === hashes.sha256 === nothing &&
        return Consts.SSH_HOST_BAD_HASH
    session = @ccall "libssh2".libssh2_session_init_ex(
        C_NULL :: Ptr{Cvoid},
        C_NULL :: Ptr{Cvoid},
        C_NULL :: Ptr{Cvoid},
        C_NULL :: Ptr{Cvoid},
    ) :: Ptr{Cvoid}
    for file in files
        ispath(file) || continue
        hosts = @ccall "libssh2".libssh2_knownhost_init(
            session :: Ptr{Cvoid},
        ) :: Ptr{Cvoid}
        count = @ccall "libssh2".libssh2_knownhost_readfile(
            hosts :: Ptr{Cvoid},
            file  :: Cstring,
            1     :: Cint, # standard OpenSSH format
        ) :: Cint
        if count < 0
            @warn("Error parsing SSH known hosts file `$file`")
            @ccall "libssh2".libssh2_knownhost_free(hosts::Ptr{Cvoid})::Cvoid
            continue
        end
        name_match = false
        prev = Ptr{KnownHost}(0)
        store = Ref{Ptr{KnownHost}}()
        while true
            get = @ccall "libssh2".libssh2_knownhost_get(
                hosts :: Ptr{Cvoid},
                store :: Ptr{Ptr{KnownHost}},
                prev  :: Ptr{KnownHost},
            ) :: Cint
            get < 0 && @warn("Error searching SSH known hosts file `$file`")
            get == 0 || break # end of file or error
            # got a known hosts record for host, now check its key hash
            prev = store[]
            known_host = unsafe_load(prev)
            known_host.name == C_NULL && continue
            host == unsafe_string(known_host.name) || continue
            name_match = true # we've found some entry in this file
            key_match = true # all available hashes must match
            key = base64decode(unsafe_string(known_host.key))
            if hashes.sha1 !== nothing
                key_match &= sha1(key) == collect(hashes.sha1)
            end
            if hashes.sha256 !== nothing
                key_match &= sha256(key) == collect(hashes.sha256)
            end
            key_match || continue
            # name and key match found
            @ccall "libssh2".libssh2_knownhost_free(hosts::Ptr{Cvoid})::Cvoid
            @assert 0 == @ccall "libssh2".libssh2_session_free(session::Ptr{Cvoid})::Cint
            return Consts.SSH_HOST_KNOWN
        end
        @ccall "libssh2".libssh2_knownhost_free(hosts::Ptr{Cvoid})::Cvoid
        name_match || continue # no name match, search more files
        # name match but no key match => host mismatch
        @assert 0 == @ccall "libssh2".libssh2_session_free(session::Ptr{Cvoid})::Cint
        return Consts.SSH_HOST_MISMATCH
    end
    # name not found in any known hosts files
    @assert 0 == @ccall "libssh2".libssh2_session_free(session::Ptr{Cvoid})::Cint
    return Consts.SSH_HOST_UNKNOWN
end

"C function pointer for `mirror_callback`"
mirror_cb() = @cfunction(mirror_callback, Cint, (Ptr{Ptr{Cvoid}}, Ptr{Cvoid}, Cstring, Cstring, Ptr{Cvoid}))
"C function pointer for `credentials_callback`"
credentials_cb() = @cfunction(credentials_callback, Cint, (Ptr{Ptr{Cvoid}}, Cstring, Cstring, Cuint, Any))
"C function pointer for `fetchhead_foreach_callback`"
fetchhead_foreach_cb() = @cfunction(fetchhead_foreach_callback, Cint, (Cstring, Cstring, Ptr{GitHash}, Cuint, Any))
"C function pointer for `certificate_callback`"
certificate_cb() = @cfunction(certificate_callback, Cint, (Ptr{CertHostKey}, Cint, Ptr{Cchar}, Ptr{Cvoid}))

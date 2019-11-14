# This file is a part of Julia. License is MIT: https://julialang.org/license

const GIT_CRED_ATTRIBUTES = ("protocol", "host", "path", "username", "password", "url")

"""
    GitCredential

Git credential information used in communication with git credential helpers. The field are
named using the [input/output key specification](https://git-scm.com/docs/git-credential#IOFMT).
"""
mutable struct GitCredential
    protocol::Union{String, Nothing}
    host::Union{String, Nothing}
    path::Union{String, Nothing}
    username::Union{String, Nothing}
    password::Union{Base.SecretBuffer, Nothing}
    use_http_path::Bool

    function GitCredential(
            protocol::Union{AbstractString, Nothing}=nothing,
            host::Union{AbstractString, Nothing}=nothing,
            path::Union{AbstractString, Nothing}=nothing,
            username::Union{AbstractString, Nothing}=nothing,
            password::Union{AbstractString, Nothing}=nothing)
        new(protocol, host, path, username, password, true)
    end
end

function GitCredential(cfg::GitConfig, url::AbstractString)
    fill!(cfg, parse(GitCredential, url))
end

GitCredential(cred::UserPasswordCredential, url::AbstractString) = parse(GitCredential, url)

Base.:(==)(c1::GitCredential, c2::GitCredential) = (c1.protocol, c1.host, c1.path, c1.username, c1.password, c1.use_http_path) ==
                                                   (c2.protocol, c2.host, c2.path, c2.username, c2.password, c2.use_http_path)
Base.hash(cred::GitCredential, h::UInt) = hash(GitCredential, hash((cred.protocol, cred.host, cred.path, cred.username, cred.password, cred.use_http_path), h))

function Base.shred!(cred::GitCredential)
    cred.protocol = nothing
    cred.host = nothing
    cred.path = nothing
    cred.username = nothing
    cred.password !== nothing && Base.shred!(cred.password)
    cred.password = nothing
    return cred
end


"""
    ismatch(url, git_cred) -> Bool

Checks if the `git_cred` is valid for the given `url`.
"""
function ismatch(url::AbstractString, git_cred::GitCredential)
    isempty(url) && return true

    m = match(URL_REGEX, url)
    m === nothing && error("Unable to parse URL")

    # Note: missing URL groups match anything
    (m[:scheme] === nothing ? true : m[:scheme] == git_cred.protocol) &&
    (m[:host] === nothing ? true : m[:host] == git_cred.host) &&
    (m[:path] === nothing ? true : m[:path] == git_cred.path) &&
    (m[:user] === nothing ? true : m[:user] == git_cred.username)
end

function isfilled(cred::GitCredential)
    cred.username !== nothing && cred.password !== nothing
end

function Base.parse(::Type{GitCredential}, url::AbstractString)
    m = match(URL_REGEX, url)
    m === nothing && error("Unable to parse URL")
    return GitCredential(
        m[:scheme],
        m[:host],
        m[:path],
        m[:user],
        m[:password],
    )
end

function Base.copy!(a::GitCredential, b::GitCredential)
    Base.shred!(a)
    a.protocol = b.protocol
    a.host = b.host
    a.path = b.path
    a.username = b.username
    a.password = b.password === nothing ? nothing : copy(b.password)
    return a
end

function Base.write(io::IO, cred::GitCredential)
    cred.protocol !== nothing && write(io, "protocol=", cred.protocol, '\n')
    cred.host !== nothing && write(io, "host=", cred.host, '\n')
    cred.path !== nothing && cred.use_http_path && write(io, "path=", cred.path, '\n')
    cred.username !== nothing && write(io, "username=", cred.username, '\n')
    cred.password !== nothing && write(io, "password=", cred.password, '\n')
    nothing
end

function Base.read!(io::IO, cred::GitCredential)
    # https://git-scm.com/docs/git-credential#IOFMT
    while !eof(io)
        key = readuntil(io, '=')
        if key == "password"
            value = Base.SecretBuffer()
            while !eof(io) && (c = read(io, UInt8)) != UInt8('\n')
                write(value, c)
            end
            seekstart(value)
        else
            value = readuntil(io, '\n')
        end

        if key == "url"
            # Any components which are missing from the URL will be set to empty
            # https://git-scm.com/docs/git-credential#git-credential-codeurlcode
            Base.shred!(parse(GitCredential, value)) do urlcred
                copy!(cred, urlcred)
            end
        elseif key in GIT_CRED_ATTRIBUTES
            field = getproperty(cred, Symbol(key))
            field !== nothing && Symbol(key) === :password && Base.shred!(field)
            setproperty!(cred, Symbol(key), value)
        elseif !all(isspace, key)
            @warn "Unknown git credential attribute found: $(repr(key))"
        end
    end

    return cred
end

function fill!(cfg::GitConfig, cred::GitCredential)
    cred.use_http_path = use_http_path(cfg, cred)

    # When the username is missing default to using the username set in the configuration
    if cred.username === nothing
        cred.username = default_username(cfg, cred)
    end

    for helper in credential_helpers(cfg, cred)
        fill!(helper, cred)

        # "Once Git has acquired both a username and a password, no more helpers will be
        # tried." – https://git-scm.com/docs/gitcredentials#gitcredentials-helper
        !isfilled(cred) && break
    end

    return cred
end

struct GitCredentialHelper
    cmd::Cmd
end

function Base.parse(::Type{GitCredentialHelper}, helper::AbstractString)
    # The helper string can take on different behaviors depending on the value:
    # - "Code after `!` evaluated in shell" – https://git-scm.com/book/en/v2/Git-Tools-Credential-Storage
    # - "If the helper name is not an absolute path, then the string `git credential-` is
    #   prepended." – https://git-scm.com/docs/gitcredentials#gitcredentials-helper
    if startswith(helper, '!')
        cmd_str = helper[2:end]
    elseif isabspath(first(Base.shell_split(helper)))
        cmd_str = helper
    else
        cmd_str = "git credential-$helper"
    end

    GitCredentialHelper(`$(Base.shell_split(cmd_str))`)
end

function Base.:(==)(a::GitCredentialHelper, b::GitCredentialHelper)
    a.cmd == b.cmd
end

function run!(helper::GitCredentialHelper, operation::AbstractString, cred::GitCredential)
    cmd = `$(helper.cmd) $operation`
    p = open(cmd, "r+")

    # Provide the helper with the credential information we know
    write(p, cred)
    write(p, "\n")
    t = @async close(p.in)

    # Process the response from the helper
    Base.read!(p, cred)
    wait(p)

    return cred
end

function run(helper::GitCredentialHelper, operation::AbstractString, cred::GitCredential)
    run!(helper, operation, deepcopy(cred))
end

# The available actions between using `git credential` and helpers are slightly different.
# We will directly interact with the helpers as that way we can request credential
# information without a prompt (helper `get` vs. git credential `fill`).
# https://git-scm.com/book/en/v2/Git-Tools-Credential-Storage

fill!(helper::GitCredentialHelper, cred::GitCredential) = run!(helper, "get", cred)
approve(helper::GitCredentialHelper, cred::GitCredential) = run(helper, "store", cred)
reject(helper::GitCredentialHelper, cred::GitCredential) = run(helper, "erase", cred)

"""
    credential_helpers(config, git_cred) -> Vector{GitCredentialHelper}

Return all of the `GitCredentialHelper`s found within the provided `config` which are valid
for the specified `git_cred`.
"""
function credential_helpers(cfg::GitConfig, cred::GitCredential)
    helpers = GitCredentialHelper[]

    # https://git-scm.com/docs/gitcredentials#gitcredentials-helper
    for entry in GitConfigIter(cfg, r"credential.*\.helper")
        section, url, name, value = split_cfg_entry(entry)
        @assert name == "helper"

        # Only use configuration settings where the URL applies to the git credential
        ismatch(url, cred) || continue

        # An empty credential.helper resets the list to empty
        isempty(value) && empty!(helpers)

        # Due to a bug in libgit2 iteration we may read credential helpers out of order.
        # See: https://github.com/libgit2/libgit2/issues/4361
        #
        # Typically the ordering doesn't matter but does in this particular case. Disabling
        # credential helpers avoids potential issues with using the wrong credentials or
        # writing credentials to the wrong helper.
        if isempty(value)
            @warn """Resetting the helper list is currently unsupported:
                     ignoring all git credential helpers""" maxlog=1
            return GitCredentialHelper[]
        end

        Base.push!(helpers, parse(GitCredentialHelper, value))
    end

    return helpers
end

"""
    default_username(config, git_cred) -> Union{String, Nothing}

Return the default username, if any, provided by the `config` which is valid for the
specified `git_cred`.
"""
function default_username(cfg::GitConfig, cred::GitCredential)
    # https://git-scm.com/docs/gitcredentials#gitcredentials-username
    for entry in GitConfigIter(cfg, r"credential.*\.username")
        section, url, name, value = split_cfg_entry(entry)
        @assert name == "username"

        # Only use configuration settings where the URL applies to the git credential
        ismatch(url, cred) || continue
        return value
    end

    return nothing
end

function use_http_path(cfg::GitConfig, cred::GitCredential)
    seen_specific = false
    use_path = false  # Default is to ignore the path

    # https://git-scm.com/docs/gitcredentials#gitcredentials-useHttpPath
    #
    # Note: Ideally the regular expression should use "useHttpPath"
    # https://github.com/libgit2/libgit2/issues/4390
    for entry in GitConfigIter(cfg, r"credential.*\.usehttppath")
        section, url, name, value = split_cfg_entry(entry)

        # Ignore global configuration if we have already encountered more specific entry
        if ismatch(url, cred) && (!isempty(url) || !seen_specific)
            seen_specific = !isempty(url)
            use_path = value == "true"
        end
    end

    return use_path
end

approve(cfg::GitConfig, cred::AbstractCredential, url::AbstractString) = nothing
reject(cfg::GitConfig, cred::AbstractCredential, url::AbstractString) = nothing

function approve(cfg::GitConfig, cred::UserPasswordCredential, url::AbstractString)
    git_cred = GitCredential(cred, url)
    git_cred.use_http_path = use_http_path(cfg, git_cred)

    for helper in credential_helpers(cfg, git_cred)
        approve(helper, git_cred)
    end

    Base.shred!(git_cred)
    nothing
end

function reject(cfg::GitConfig, cred::UserPasswordCredential, url::AbstractString)
    git_cred = GitCredential(cred, url)
    git_cred.use_http_path = use_http_path(cfg, git_cred)

    for helper in credential_helpers(cfg, git_cred)
        reject(helper, git_cred)
    end

    Base.shred!(git_cred)
    nothing
end

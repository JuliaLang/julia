"""
    GitCredential

Git credential information used in communication with git credential helpers. The field are
named using the [input/output key specification](https://git-scm.com/docs/git-credential#IOFMT).
"""
mutable struct GitCredential
    protocol::Nullable{String}
    host::Nullable{String}
    path::Nullable{String}
    username::Nullable{String}
    password::Nullable{String}
    use_http_path::Bool

    function GitCredential(
            protocol::Nullable{<:AbstractString},
            host::Nullable{<:AbstractString},
            path::Nullable{<:AbstractString},
            username::Nullable{<:AbstractString},
            password::Nullable{<:AbstractString})
        c = new(protocol, host, path, username, password, true)
        finalizer(securezero!, c)
        return c
    end
end

function GitCredential(
        protocol::Union{AbstractString,Void}=nothing,
        host::Union{AbstractString,Void}=nothing,
        path::Union{AbstractString,Void}=nothing,
        username::Union{AbstractString,Void}=nothing,
        password::Union{AbstractString,Void}=nothing)
    GitCredential(
        Nullable{String}(protocol),
        Nullable{String}(host),
        Nullable{String}(path),
        Nullable{String}(username),
        Nullable{String}(password))
end

function GitCredential(cfg::GitConfig, url::AbstractString)
    fill!(cfg, parse(GitCredential, url))
end

function GitCredential(cred::UserPasswordCredentials, url::AbstractString)
    git_cred = parse(GitCredential, url)
    git_cred.username = Nullable{String}(cred.user)
    git_cred.password = Nullable{String}(cred.pass)
    return git_cred
end

function securezero!(cred::GitCredential)
    !isnull(cred.protocol) && securezero!(unsafe_get(cred.protocol))
    !isnull(cred.host) && securezero!(unsafe_get(cred.host))
    !isnull(cred.path) && securezero!(unsafe_get(cred.path))
    !isnull(cred.username) && securezero!(unsafe_get(cred.username))
    !isnull(cred.password) && securezero!(unsafe_get(cred.password))
    return cred
end

function Base.:(==)(a::GitCredential, b::GitCredential)
    isequal(a.protocol, b.protocol) &&
    isequal(a.host, b.host) &&
    isequal(a.path, b.path) &&
    isequal(a.username, b.username) &&
    isequal(a.password, b.password) &&
    a.use_http_path == b.use_http_path
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
    (m[:scheme] === nothing ? true : isequal(Nullable(m[:scheme]), git_cred.protocol)) &&
    (m[:host] === nothing ? true : isequal(Nullable(m[:host]), git_cred.host)) &&
    (m[:path] === nothing ? true : isequal(Nullable(m[:path]), git_cred.path)) &&
    (m[:user] === nothing ? true : isequal(Nullable(m[:user]), git_cred.username))
end

function isfilled(cred::GitCredential)
    !isnull(cred.username) && !isnull(cred.password)
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
    # Note: deepcopy calls avoid issues with securezero!
    a.protocol = deepcopy(b.protocol)
    a.host = deepcopy(b.host)
    a.path = deepcopy(b.path)
    a.username = deepcopy(b.username)
    a.password = deepcopy(b.password)
    return a
end

function Base.write(io::IO, cred::GitCredential)
    !isnull(cred.protocol) && println(io, "protocol=", unsafe_get(cred.protocol))
    !isnull(cred.host) && println(io, "host=", unsafe_get(cred.host))
    !isnull(cred.path) && cred.use_http_path && println(io, "path=", unsafe_get(cred.path))
    !isnull(cred.username) && println(io, "username=", unsafe_get(cred.username))
    !isnull(cred.password) && println(io, "password=", unsafe_get(cred.password))
    nothing
end

function Base.read!(io::IO, cred::GitCredential)
    # https://git-scm.com/docs/git-credential#IOFMT
    while !eof(io)
        key, value = split(readline(io), '=')

        if key == "url"
            # Any components which are missing from the URL will be set to empty
            # https://git-scm.com/docs/git-credential#git-credential-codeurlcode
            copy!(cred, parse(GitCredential, value))
        else
            setfield!(cred, Symbol(key), Nullable(String(value)))
        end
    end

    return cred
end

function fill!(cfg::GitConfig, cred::GitCredential)
    cred.use_http_path = use_http_path(cfg, cred)

    # When the username is missing default to using the username set in the configuration
    if isnull(cred.username)
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
    output, input, p = readandwrite(cmd)

    # Provide the helper with the credential information we know
    write(input, cred)
    write(input, "\n")
    t = @async close(input)

    # Process the response from the helper
    Base.read!(output, cred)
    close(output)
    wait(t)

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
        section, url, name, value = split(entry)
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
            Base.warn_once("Resetting the helper list is currently unsupported: " *
                 "ignoring all git credential helpers.")
            return GitCredentialHelper[]
        end

        push!(helpers, parse(GitCredentialHelper, value))
    end

    return helpers
end

"""
    default_username(config, git_cred) -> Nullable{String}

Return the default username, if any, provided by the `config` which is valid for the
specified `git_cred`.
"""
function default_username(cfg::GitConfig, cred::GitCredential)
    # https://git-scm.com/docs/gitcredentials#gitcredentials-username
    for entry in GitConfigIter(cfg, r"credential.*\.username")
        section, url, name, value = split(entry)
        @assert name == "username"

        # Only use configuration settings where the URL applies to the git credential
        ismatch(url, cred) || continue
        return Nullable{String}(value)
    end

    return Nullable{String}()
end

function use_http_path(cfg::GitConfig, cred::GitCredential)
    use_path = false  # Default is to ignore the path

    # https://git-scm.com/docs/gitcredentials#gitcredentials-useHttpPath
    #
    # Note: Ideally the regular expression should use "useHttpPath"
    # https://github.com/libgit2/libgit2/issues/4390
    for entry in GitConfigIter(cfg, r"credential.*\.usehttppath")
        section, url, name, value = split(entry)

        ismatch(url, cred) || continue
        use_path = value == "true"
    end

    return use_path
end

approve(cfg::GitConfig, cred::AbstractCredentials, url::AbstractString) = nothing
reject(cfg::GitConfig, cred::AbstractCredentials, url::AbstractString) = nothing

function approve(cfg::GitConfig, cred::UserPasswordCredentials, url::AbstractString)
    git_cred = GitCredential(cred, url)
    git_cred.use_http_path = use_http_path(cfg, git_cred)

    for helper in credential_helpers(cfg, git_cred)
        approve(helper, git_cred)
    end

    nothing
end

function reject(cfg::GitConfig, cred::UserPasswordCredentials, url::AbstractString)
    git_cred = GitCredential(cred, url)
    git_cred.use_http_path = use_http_path(cfg, git_cred)

    for helper in credential_helpers(cfg, git_cred)
        reject(helper, git_cred)
    end

    securezero!(git_cred)
    securezero!(cred)

    nothing
end

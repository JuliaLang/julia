function Base.parse(::Type{GitCredentialHelper}, helper::AbstractString)
    if startswith(helper, "!")
        cmd_str = helper[2:end]
    elseif isabspath(first(Base.shell_split(helper)))
        cmd_str = helper
    else
        cmd_str = "git credential-$helper"
    end

    GitCredentialHelper(`$(Base.shell_split(cmd_str)...)`)
end

function run!(helper::GitCredentialHelper, operation::AbstractString, cred::GitCredential)
    cmd = `$(helper.cmd) $operation`
    output, input, p = readandwrite(cmd)

    # Provide the helper with the credential information we know
    Base.write(input, cred)
    Base.write(input, "\n")
    close(input)

    # Process the response from the helper
    Base.read!(output, cred)
    close(output)

    return cred
end

function run(helper::GitCredentialHelper, operation::AbstractString, cred::GitCredential)
    updated_cred = deepcopy(cred)
    run!(helper, operation, updated_cred)
end

function Base.parse(::Type{GitCredential}, url::AbstractString)
    # TODO: It appears that the Git internals expect the contents to be URL encoded:
    # https://github.com/git/git/blob/24321375cda79f141be72d1a842e930df6f41725/credential.c#L324
    #
    # Match one of:
    # (1) proto://<host>/...
    # (2) proto://<user>@<host>/...
    # (3) proto://<user>:<pass>@<host>/...
    m = match(URL_REGEX, url)
    m === nothing && error("Unable to parse URL")
    host = m[:host] * (m[:port] != nothing ? ":$(m[:port])" : "")
    return GitCredential(
        m[:protocol],
        host,
        m[:path] === nothing ? "" : m[:path],
        m[:user] === nothing ? "" : m[:user],
        m[:password] === nothing ? "" : m[:password],
    )
end

function merge!(a::GitCredential, b::GitCredential)
    !isempty(b.protocol) && (a.protocol = b.protocol)
    !isempty(b.host) && (a.host = b.host)
    !isempty(b.path) && (a.path = b.path)
    !isempty(b.username) && (a.username = b.username)
    !isempty(b.password) && (a.password = b.password)
    return a
end

function Base.:(==)(a::GitCredential, b::GitCredential)
    return (
        a.protocol == b.protocol &&
        a.host == b.host &&
        a.path == b.path &&
        a.username == b.username &&
        a.password == b.password
    )
end

function Base.contains(haystack::GitCredential, needle::GitCredential)
    field_contains(h, n) = isempty(n) || (!isempty(h) && h == n)
    return (
        field_contains(haystack.protocol, needle.protocol) &&
        field_contains(haystack.host, needle.host) &&
        field_contains(haystack.path, needle.path) &&
        field_contains(haystack.username, needle.username)
    )
end

function Base.write(io::IO, cred::GitCredential)
    !isempty(cred.protocol) && println(io, "protocol=", cred.protocol)
    !isempty(cred.host) && println(io, "host=", cred.host)
    !isempty(cred.path) && println(io, "path=", cred.path)
    !isempty(cred.username) && println(io, "username=", cred.username)
    !isempty(cred.password) && println(io, "password=", cred.password)
    nothing
end

function Base.read!(io::IO, cred::GitCredential)
    # https://git-scm.com/docs/git-credential#IOFMT
    while !eof(io)
        key, value = split(readline(io), '=')

        if key == "protocol"
            cred.protocol = value
        elseif key == "host"
            cred.host = value
        elseif key == "path"
            cred.path = value
        elseif key == "username"
            cred.username = value
        elseif key == "password"
            cred.password = value
        elseif key == "url"
            merge!(cred, parse(GitCredential, value))
        end
    end

    return cred
end

Base.read(io::IO, ::Type{GitCredential}) = Base.read!(io, GitCredential())

"""
Git credential helpers that are relevant to the given credentials. If the credentials do not
contain a username one may be added at this step by the configuration.
"""
function helpers!(cfg::GitConfig, cred::GitCredential)
    # Note: Should be quoting user input but `\Q` and `\E` isn't supported by libgit2
    # ci = LibGit2.GitConfigIter(cfg, Regex("credential(\\.$protocol://$host)?\\.helper"))

    # Note: We will emulate the way Git reads the the configuration file which is from
    # top to bottom with no precedence on specificity.
    helpers = GitCredentialHelper[]
    for entry in GitConfigIter(cfg, r"credential.*")
        name, value = unsafe_string(entry.name), unsafe_string(entry.value)

        a, b = search(name, '.'), rsearch(name, '.')
        url = SubString(name, a + 1, b - 1)
        token = SubString(name, b + 1)

        if !isempty(url)
            !contains(cred, parse(GitCredential, url)) && continue
        end

        if token == "helper"
            push!(helpers, parse(GitCredentialHelper, value))
        elseif token == "username"
            if isempty(cred.username)
                cred.username = value
            end
        end
    end

    return helpers
end

function filled(cred::GitCredential)
    !isempty(cred.username) && !isempty(cred.password)
end

function fill!(helpers::AbstractArray{GitCredentialHelper}, cred::GitCredential)
    filled(cred) && return cred
    for helper in helpers
        run!(helper, "get", cred)
        filled(cred) && break
    end
    return cred
end

function fill(helpers::AbstractArray{GitCredentialHelper}, cred::GitCredential)
    new_cred = deepcopy(cred)
    fill!(helpers, new_cred)
end

function approve(helpers::AbstractArray{GitCredentialHelper}, cred::GitCredential)
    !filled(cred) && error("Credentials are not filled")
    for helper in helpers
        run(helper, "store", cred)
    end
end

function reject(helpers::AbstractArray{GitCredentialHelper}, cred::GitCredential)
    !filled(cred) && return

    for helper in helpers
        run(helper, "erase", cred)
    end

    Base.securezero!(cred.username)
    Base.securezero!(cred.password)
    cred.username = ""
    cred.password = ""
    nothing
end

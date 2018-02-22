# This file is a part of Julia. License is MIT: https://julialang.org/license

# Parse "GIT URLs" syntax (URLs and a scp-like syntax). For details see:
# https://git-scm.com/docs/git-clone#_git_urls_a_id_urls_a
const URL_REGEX = r"""
^(?:(?<scheme>ssh|git|https?)://)?+
(?:
    (?<user>.*?)
    (?:\:(?<password>.*?))?@
)?
(?<host>[A-Za-z0-9\-\.]+)
(?(<scheme>)
    # Only parse port when not using scp-like syntax
    (?:\:(?<port>\d+))?
    /?
    |
    :?
)
(?<path>
    # Require path to be preceeded by '/'. Alternatively, ':' when using scp-like syntax.
    (?<=(?(<scheme>)/|:))
    .*
)?
$
"""x

"""
    version() -> VersionNumber

Return the version of libgit2 in use, as a [`VersionNumber`](@ref man-version-number-literals).
"""
function version()
    major = Ref{Cint}(0)
    minor = Ref{Cint}(0)
    patch = Ref{Cint}(0)
    ccall((:git_libgit2_version, :libgit2), Cvoid,
          (Ref{Cint}, Ref{Cint}, Ref{Cint}), major, minor, patch)
    return VersionNumber(major[], minor[], patch[])
end
const VERSION = version()

"""
    isset(val::Integer, flag::Integer)

Test whether the bits of `val` indexed by `flag` are set (`1`) or unset (`0`).
"""
isset(val::Integer, flag::Integer) = (val & flag == flag)

"""
    reset(val::Integer, flag::Integer)

Unset the bits of `val` indexed by `flag`, returning them to `0`.
"""
reset(val::Integer, flag::Integer) = (val &= ~flag)

"""
    toggle(val::Integer, flag::Integer)

Flip the bits of `val` indexed by `flag`, so that if a bit is `0` it
will be `1` after the toggle, and vice-versa.
"""
toggle(val::Integer, flag::Integer) = (val |= flag)

"""
    features()

Return a list of git features the current version of libgit2 supports, such as
threading or using HTTPS or SSH.
"""
function features()
    feat = ccall((:git_libgit2_features, :libgit2), Cint, ())
    res = Consts.GIT_FEATURE[]
    for f in instances(Consts.GIT_FEATURE)
        isset(feat, Cuint(f)) && Base.push!(res, f)
    end
    return res
end

"""
    LibGit2.posixpath(path)

Standardise the path string `path` to use POSIX separators.
"""
function posixpath end
if Sys.iswindows()
    posixpath(path) = replace(path,'\\' => '/')
elseif Sys.isunix()
    posixpath(path) = path
end

"""
    LibGit2.git_url(; kwargs...) -> String

Create a string based upon the URL components provided. When the `scheme` keyword is not
provided the URL produced will use the alternative [scp-like syntax](https://git-scm.com/docs/git-clone#_git_urls_a_id_urls_a).

# Keywords

  * `scheme::AbstractString=""`: the URL scheme which identifies the protocol to be used.
    For HTTP use "http", SSH use "ssh", etc. When `scheme` is not provided the output format
    will be "ssh" but using the scp-like syntax.
  * `username::AbstractString=""`: the username to use in the output if provided.
  * `password::AbstractString=""`: the password to use in the output if provided.
  * `host::AbstractString=""`: the hostname to use in the output. A hostname is required to
    be specified.
  * `port::Union{AbstractString,Integer}=""`: the port number to use in the output if
    provided. Cannot be specified when using the scp-like syntax.
  * `path::AbstractString=""`: the path to use in the output if provided.

# Examples
```jldoctest
julia> LibGit2.git_url(username="git", host="github.com", path="JuliaLang/julia.git")
"git@github.com:JuliaLang/julia.git"

julia> LibGit2.git_url(scheme="https", host="github.com", path="/JuliaLang/julia.git")
"https://github.com/JuliaLang/julia.git"

julia> LibGit2.git_url(scheme="ssh", username="git", host="github.com", port=2222, path="JuliaLang/julia.git")
"ssh://git@github.com:2222/JuliaLang/julia.git"
```
"""
function git_url(;
        scheme::AbstractString="",
        username::AbstractString="",
        password::AbstractString="",
        host::AbstractString="",
        port::Union{AbstractString,Integer}="",
        path::AbstractString="")

    port_str = string(port)
    scp_syntax = isempty(scheme)

    isempty(host) && throw(ArgumentError("A host needs to be specified"))
    scp_syntax && !isempty(port_str) && throw(ArgumentError("Port cannot be specified when using scp-like syntax"))

    io = IOBuffer()
    !isempty(scheme) && print(io, scheme, "://")

    if !isempty(username) || !isempty(password)
        print(io, username)
        !isempty(password) && print(io, ':', password)
        print(io, '@')
    end

    print(io, host)
    !isempty(port_str) && print(io, ':', port_str)

    if !isempty(path)
        if scp_syntax
            print(io, ':')
        elseif !startswith(path, '/')
            print(io, '/')
        end
        print(io, path)
    end

    return String(take!(io))
end

function credential_identifier(scheme::AbstractString, host::AbstractString)
    string(isempty(scheme) ? "ssh" : scheme, "://", host)
end

function credential_identifier(url::AbstractString)
    m = match(URL_REGEX, url)
    scheme = coalesce(m[:scheme], "")
    host = m[:host]
    credential_identifier(scheme, host)
end

# This file is a part of Julia. License is MIT: http://julialang.org/license

# Parse GIT URLs and scp-like syntax.
# https://git-scm.com/docs/git-clone#_git_urls_a_id_urls_a
const GIT_URL_REGEX = r"""
^(?:(?<protocol>ssh|git|https?)://)?
(?:
    (?<user>.*?)
    (?:\:(?<password>.*?))?@
)?
(?<host>[A-Za-z0-9\-\.]+)
(?(<protocol>)
    (?:\:(?<port>\d+))?  # only parse port when not using SCP-like syntax
    |
    :?
)
(?<path>.*?)$
"""x

function version()
    major = Ref{Cint}(0)
    minor = Ref{Cint}(0)
    patch = Ref{Cint}(0)
    ccall((:git_libgit2_version, :libgit2), Void,
          (Ptr{Cint}, Ptr{Cint}, Ptr{Cint}), major, minor, patch)
    return VersionNumber(major[], minor[], patch[])
end
const VERSION = version()

isset(val::Integer, flag::Integer) = (val & flag == flag)
reset(val::Integer, flag::Integer) = (val &= ~flag)
toggle(val::Integer, flag::Integer) = (val |= flag)

function prompt(msg::AbstractString; default::AbstractString="", password::Bool=false)
    @static if is_windows()
        if password
            error("Command line prompt not supported for password entry on windows. Use winprompt instead")
        end
    end
    msg = !isempty(default) ? msg*" [$default]:" : msg*":"
    uinput = if password
        Base.getpass(msg)  # Automatically chomps. We cannot tell EOF from '\n'.
    else
        print(msg)
        readline(chomp=false)
    end
    if !password
        isempty(uinput) && return Nullable{String}()  # Encountered EOF
        uinput = chomp(uinput)
    end
    Nullable{String}(isempty(uinput) ? default : uinput)
end

function features()
    feat = ccall((:git_libgit2_features, :libgit2), Cint, ())
    res = Consts.GIT_FEATURE[]
    for f in instances(Consts.GIT_FEATURE)
        isset(feat, Cuint(f)) && push!(res, f)
    end
    return res
end

"""
    LibGit2.posixpath(path)

Standardise the path string `path` to use POSIX separators.
"""
function posixpath end
if is_windows()
    posixpath(path) = replace(path,'\\','/')
else is_unix()
    posixpath(path) = path
end

function git_url(;
    protocol::AbstractString="",
    username::AbstractString="",
    password::AbstractString="",
    host::AbstractString="",
    port::Union{AbstractString,Integer}="",
    path::AbstractString="",
)
    port_str = string(port)
    scp_syntax = isempty(protocol)

    isempty(host) && error("A host needs to be specified")
    scp_syntax && !isempty(port_str) && error("Port cannot be specified when using scp-like syntax")

    io = IOBuffer()
    if !isempty(protocol)
        print(io, protocol, "://")
    end

    if !isempty(username) || !isempty(password)
        print(io, username)
        !isempty(password) && print(io, ':', password)
        print(io, '@')
    end

    print(io, host)
    !isempty(port) && print(io, ':', port)
    print(io, scp_syntax && !isempty(path) ? ":" : "", path)

    return readstring(seekstart(io))
end

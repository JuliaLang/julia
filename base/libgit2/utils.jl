# This file is a part of Julia. License is MIT: http://julialang.org/license

const URL_REGEX = r"""
^(?:(?<scheme>https?|git|ssh)\:\/\/)?
(?:(?<user>.*?)(?:\:(?<password>.*?))?@)?
(?<host>[A-Za-z0-9\-\.]+)
(?:\:(?<port>\d+)?)?
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
    if is_windows() && password
        error("Command line prompt not supported for password entry on windows. Use winprompt instead")
    end
    msg = !isempty(default) ? msg*" [$default]:" : msg*":"
    uinput = if password
        Base.getpass(msg)
    else
        print(msg)
        readline()
    end
    isempty(uinput) ? default : uinput
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

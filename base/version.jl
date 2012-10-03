## semantic version numbers (http://semver.org)

type VersionNumber
    major::Int
    minor::Int
    patch::Int
    prerelease::Vector{Union(Int,ASCIIString)}
    build::Vector{Union(Int,ASCIIString)}

    function VersionNumber(major::Int, minor::Int, patch::Int, pre::Vector, bld::Vector)
        if major < 0 error("invalid major version: $major") end
        if minor < 0 error("invalid minor version: $minor") end
        if patch < 0 error("invalid patch version: $patch") end
        prerelease = Array(Union(Int,ASCIIString),length(pre))
        for i in 1:length(pre)
            ident = ascii(string(pre[i]))
            if isempty(ident) && !(length(pre)==1 && isempty(bld))
                error("invalid pre-release identifier: empty string")
            end
            if !ismatch(r"^(?:[0-9a-z-]*)?$"i, ident)
                error("invalid pre-release identifier: $ident")
            end
            if ismatch(r"^\d+$", ident)
                ident = parse_int(ident)
            end
            prerelease[i] = ident
        end
        build = Array(Union(Int,ASCIIString),length(bld))
        for i in 1:length(bld)
            ident = ascii(string(bld[i]))
            if !ismatch(r"^(?:[0-9a-z-]+)?$"i, ident)
                error("invalid build identifier: $ident")
            end
            if ismatch(r"^\d+$", ident)
                ident = parse_int(ident)
            end
            build[i] = ident
        end
        new(major, minor, patch, prerelease, build)
    end
end
VersionNumber(x::Integer, y::Integer, z::Integer) = VersionNumber(x, y, z, [], [])
VersionNumber(x::Integer, y::Integer)             = VersionNumber(x, y, 0, [], [])
VersionNumber(x::Integer)                         = VersionNumber(x, 0, 0, [], [])

function print(io::IO, v::VersionNumber)
    print(io, v.major)
    print(io, '.')
    print(io, v.minor)
    print(io, '.')
    print(io, v.patch)
    if !isempty(v.prerelease)
        print(io, '-')
        print_joined(io, v.prerelease,'.')
    end
    if !isempty(v.build)
        print(io, '+')
        print_joined(io, v.build,'.')
    end
end
show(io, v::VersionNumber) = print(io, "v\"", v, "\"")

convert(::Type{VersionNumber}, v::Integer) = VersionNumber(v)
convert(::Type{VersionNumber}, v::Tuple) = VersionNumber(v...)

const VERSION_REGEX = r"^
    v?                                      # prefix        (optional)
    (\d+)                                   # major         (required)
    (?:\.(\d+))?                            # minor         (optional)
    (?:\.(\d+))?                            # patch         (optional)
    (?:(-)|
    (?:-((?:[0-9a-z-]+\.)*[0-9a-z-]+))?     # pre-release   (optional)
    (?:\+((?:[0-9a-z-]+\.)*[0-9a-z-]+))?    # build         (optional)
    )
$"ix

function convert(::Type{VersionNumber}, v::String)
    m = match(VERSION_REGEX, v)
    if m == nothing error("invalid version string: $v") end
    major, minor, patch, minus, prerl, build = m.captures
    major = int(major)
    minor = minor != nothing ? int(minor) : 0
    patch = patch != nothing ? int(patch) : 0
    prerl = prerl != nothing ? split(prerl,'.') : minus != nothing ? [""] : []
    build = build != nothing ? split(build,'.') : []
    VersionNumber(major, minor, patch, prerl, build)
end

macro v_str(v); convert(VersionNumber, v); end

_jl_ident_cmp(a::Int, b::Int) = cmp(a,b)
_jl_ident_cmp(a::Int, b::ASCIIString) = isempty(b) ? +1 : -1
_jl_ident_cmp(a::ASCIIString, b::Int) = isempty(a) ? -1 : +1
_jl_ident_cmp(a::ASCIIString, b::ASCIIString) = cmp(a,b)

function _jl_ident_cmp(A::Vector{Union(Int,ASCIIString)},
                       B::Vector{Union(Int,ASCIIString)})
    i = start(A)
    j = start(B)
    while !done(A,i) && !done(B,i)
       a,i = next(A,i)
       b,j = next(B,j)
       c = _jl_ident_cmp(a,b)
       (c != 0) && return c
    end
    done(A,i) && !done(B,j) ? -1 :
    !done(A,i) && done(B,j) ? +1 : 0
end

function isequal(a::VersionNumber, b::VersionNumber)
    (a.major != b.major) && return false
    (a.minor != b.minor) && return false
    (a.patch != b.patch) && return false
    (_jl_ident_cmp(a.prerelease,b.prerelease) != 0) && return false
    (_jl_ident_cmp(a.build,b.build) != 0) && return false
    return true
end

function isless(a::VersionNumber, b::VersionNumber)
    (a.major < b.major) && return true
    (a.major > b.major) && return false
    (a.minor < b.minor) && return true
    (a.minor > b.minor) && return false
    (a.patch < b.patch) && return true
    (a.patch > b.patch) && return false
    (!isempty(a.prerelease) && isempty(b.prerelease)) && return true
    (isempty(a.prerelease) && !isempty(b.prerelease)) && return false
    c = _jl_ident_cmp(a.prerelease,b.prerelease)
    (c < 0) && return true
    (c > 0) && return false
    c = _jl_ident_cmp(a.build,b.build)
    (c < 0) && return true
    return false
end

## julia version info

const VERSION = convert(VersionNumber,readchomp("$JULIA_HOME/../../VERSION"))
try
    ver = string(VERSION)
    commit = readchomp(`git rev-parse HEAD`)
    tagged = try readchomp(`git rev-parse --verify --quiet v$ver`)
             catch "doesn't reference a commit"; end
    ctim = int(readall(`git log -1 --pretty=format:%ct`))
    if commit != tagged
        # 1250998746: ctime of first commit (Sat Aug 23 3:39:06 2009 UTC)
        push(VERSION.build, ctim - 1250998746)
        push(VERSION.build, "r$(commit[1:4])")
    end
    clean = success(`git diff --quiet HEAD`)
    if !clean; push(VERSION.build, "dirty"); end
    clean = clean ? "" : "*"
    isotime = strftime("%Y-%m-%d %H:%M:%S", ctim)
    global const _jl_commit_string = "Commit $(commit[1:10]) ($isotime)$clean"
    global const VERSION_COMMIT = commit[1:10]
catch
    global const _jl_commit_string = ""
    global const VERSION_COMMIT = ""
end

begin
const _jl_version_string = "Version $VERSION"
const _jl_banner_plain =
I"               _
   _       _ _(_)_     |  A fresh approach to technical computing
  (_)     | (_) (_)    |  Documentation: http://docs.julialang.org
   _ _   _| |_  __ _   |  Type \"help()\" to list help topics
  | | | | | | |/ _` |  |
  | | |_| | | | (_| |  |  $_jl_version_string
 _/ |\__'_|_|_|\__'_|  |  $_jl_commit_string
|__/                   |

"
local tx = "\033[0m\033[1m" # text
local jl = "\033[0m\033[1m" # julia
local d1 = "\033[34m" # first dot
local d2 = "\033[31m" # second dot
local d3 = "\033[32m" # third dot
local d4 = "\033[35m" # fourth dot
const _jl_banner_color =
"\033[1m               $(d3)_
   $(d1)_       $(jl)_$(tx) $(d2)_$(d3)(_)$(d4)_$(tx)     |  A fresh approach to technical computing
  $(d1)(_)$(jl)     | $(d2)(_)$(tx) $(d4)(_)$(tx)    |  Documentation: http://docs.julialang.org
   $(jl)_ _   _| |_  __ _$(tx)   |  Type \"help()\" to list help topics
  $(jl)| | | | | | |/ _` |$(tx)  |
  $(jl)| | |_| | | | (_| |$(tx)  |  $_jl_version_string
 $(jl)_/ |\\__'_|_|_|\\__'_|$(tx)  |  $_jl_commit_string
$(jl)|__/$(tx)                   |

\033[0m"
end # begin
